#!/usr/bin/python

from __future__ import print_function

import argparse
import functools
import json
import os
import re
import subprocess
from mako.template import Template

PROGRAM_NAME='swift-idl'
SOURCEKITTEN='sourceKitten'

# All classes and Enums in IDL.xcodeproj
# * Class and Enum must not have any REAL protocol; Those are ignored.
# * All methods are discarded in output
# * All comments are discarded in output
# * All generic Types except Array and Optional are not supposed.
# * Inherit class and enum are not supported

# Supported types
#     * Int, Float, Bool and String
#     * Enum
#     * NSDate and NSURL
#     * Optional and Array
#     * User declared class in IDL.xcodeproj

# Available JSON annotation
#   let foo: Int        // json:"-"
#   let fooBar: Int     // json:"foo-bar"
#   let fooBoo: Int     // json:",omitempty" (not supported currently)
#   let fooBee: Int     // json:",string"    (not supported currently)
#   case Unknown        // json:"-"          (not supported currently)

# Router
#   case foo            // router:"POST"
#   case bar(num: Int)  // router:",path/to/api"

def sourcekitten_doc(project, scheme):
    args = [SOURCEKITTEN, 'doc', '-project', project, '-scheme', scheme]
    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (out, err) = p.communicate()
    return json.loads(out)


def sourcekitten_syntax(filepath):
    def addContent(contents):
        def func(c):
            offset = c['offset']
            length = c['length']
            c['content'] = contents[offset : offset + length]
            return c
        return func

    def addLineNumber(contents):
        linenumMap = map(lambda x: 1 if x == '\n' else 0, contents) # FIXME: check CRLF or LF
        def func(c):
            offset = c['offset']
            c['lineNumber'] = sum(linenumMap[:offset], 1)
            return c
        return func

    def addPrevString(contents):
        def func(a, c):
            offset = c['offset']
            length = c['length']
            c['prevString'] = contents[a:offset]
            return offset + length
        return func

    def addNextString(contents):
        def func(a, c):
            if a:
                p = a['offset'] + a['length']
                offset = c['offset']
                a['nextString'] = contents[p:offset]
            return c
        return func

    def addAdditionalInfo(s):
        t = s[-1]
        with file(filepath) as f:
            contents = f.read()
            map(addContent(contents), s)
            map(addLineNumber(contents), s)
            reduce(addPrevString(contents), s, 0)
            reduce(addNextString(contents), s, None)

            p = t['offset'] + t['length']
            t['nextString'] = contents[p:-1]
            return s

    p = subprocess.Popen([SOURCEKITTEN, 'syntax', '--file', filepath], stdout=subprocess.PIPE)
    (stdoutdata, _) = p.communicate()
    tokens = addAdditionalInfo(json.loads(stdoutdata))
    return tokens


def tokenrange(tokens, offset, length):
    start = None
    end = len(tokens)
    for i in range(end):
        t = tokens[i]
        if t['offset'] < offset: continue
        if not start: start = i
        if t['offset'] >= offset + length:
            end = i
            break
    return range(start, end)


def indent(text, width=4):
    lines = [' ' * width + t for t in text.split('\n') if len(t.strip()) > 0]
    if width == 0: lines = [t if t.strip() != '//' else '' for t in lines]
    return '\n'.join(lines)

def parseAnnotation(comment_part):
    def parseOne(s):
        m = re.match(r'^(\w+):"([^"]+)"$', s)
        if m:
            g = m.groups()
            if len(g) == 2:
                return (g[0].lower(), map(unicode.strip, g[1].split(',')))
        return None

    splitted = re.findall(r'\b\w+:"[^"]+"', comment_part)
    return {q[0]:q[1] for q in map(parseOne, splitted) if q != None}


class JSONAnnotation:
    def __init__(self, var):
        anon_dic = var._annotations # FIXME: private access
        self.jsonOmitValue = False
        self.jsonLabel = var.name

        annons = anon_dic.get('json', [''])
        if annons[0] != '':
            if annons[0] == '-':
                self.jsonOmitValue = True
            else:
                self.jsonLabel = annons[0]

        for i in annons[1:]:
            if i == 'omitempty':
                pass
            elif i == 'string':
                pass
            else:
                raise RuntimeError('Unknown annotation: ' + i)


class RouterAnnotation:
    def __init__(self, case):
        anon_dic = case._annotations # FIXME: private access
        self._case = case
        self.method = 'GET'
        self.path = case._label # FIXME: private access
        self._responseType = None

        annons = anon_dic.get('router', [''])

        if len(annons) > 0:
            method = annons[0].upper()
            if method != '':
                assert(method in ['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE', 'TRACE', 'CONNECT', 'OPTIONS'])
                self.method = method

        if len(annons) > 1:
            path = annons[1]
            if path != '':
                self.path = path

        if len(annons) > 2:
            retType = annons[2]
            if retType != '':
                self._responseType = retType

    def paramSets(self):
        pathParams = re.findall(r'\(([^)]+)\)', self.path)
        caseParams = [i.name for i in self._case._assocVals if i.name != None] # FIXME: private access
        return (pathParams, caseParams)

    @property
    def responseType(self):
        return self._responseType or self._case._label + 'Response'

    @property
    def casePathString(self):
        pathParams, caseParams = self.paramSets()
        union = set(pathParams).intersection(set(caseParams))
        if len(union) == 0: return ''
        lets = [i if i in union else '_' for i in caseParams]
        return '(let (' + ', '.join(lets) + '))'

    @property
    def caseParams(self):
        pathParams, caseParams = self.paramSets()
        diff = set(caseParams).difference(set(pathParams))
        lets = [i if i in diff else None for i in caseParams]



def getIdlProtocolByName(protocols, name):
    for p in protocols:
        if p.name == name:
            return p.clazz
    return None

def getIdlProtocolsByNames(protocols, names):
    ret = []
    for n in names:
        clazz = getIdlProtocolByName(protocols, n)
        if clazz:
            ret.append(clazz())
    return ret

def getDefaultIdlProtocol(protocols, name = 'Default'):
    for p in protocols:
        if p.name == name:
            return getIdlProtocolsByNames(protocols, p.inheritedTypes)
    return []


class SwiftClass():
    def __init__(self, tokens, node):
        self._bodyOffset = node['key.bodyoffset']
        self._bodyLength = node['key.bodylength']
        self._name = node['key.name']
        self.isStruct = node['key.kind'] == 'source.lang.swift.decl.struct'

        def getAnnotations(tokens, node):
            ttr = tokenrange(tokens, node['key.offset'], node['key.length'])
            tts = [tokens[i] for i in ttr]
            lineNumber = tts[0]['lineNumber']

            for t in tts:
                if t['lineNumber'] == lineNumber and t['type'] == 'source.lang.swift.syntaxtype.comment':
                    content = t['content']
                    cs = content.split('//')
                    if len(cs) >= 2:
                        # the first comment area is only checked
                        return parseAnnotation(unicode(cs[1]))
            return {}

        def getVariables(a, n):
            if n.get('key.kind', None) == 'source.lang.swift.decl.var.instance':
                return a + [SwiftVariable(tokens, n)]
            return a

        self._variables = reduce(getVariables, node['key.substructure'], [])
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

        self._annotations = getAnnotations(tokens, node)

        self._innerClassesOrEnums = []
        subs = node.get('key.substructure', None)
        if subs:
            self._innerClassesOrEnums = visitSubstructure(getClassOrEnum, tokens, subs, [])

    def getDeclarationString(self, protocols, level=0):
        template = Template('''
public ${clazz.decltype} ${clazz.name}${inh} {
    % for v in clazz.variables:
    ${v.parsedDeclarationWithoutDefaultValue}
    % endfor

% for r in rs:
//
${r}
% endfor

% for s in sub:
//
${s}
% endfor
}
''')
        ps = getIdlProtocolsByNames(protocols, self._inheritedTypes)
        if len(ps) == 0:
            ps = getDefaultIdlProtocol(protocols, 'ClassDefault')

        typeInheritances = sum([e.protocolClass for e in ps], [])
        output = template.render(clazz=self,
                                 rs=[e.processClass(self) for e in ps],
                                 inh=': ' + ', '.join(typeInheritances) if len(typeInheritances) > 0 else '',
                                 sub=map(lambda e: e.getDeclarationString(protocols, level + 1), self._innerClassesOrEnums))
        return indent(output, level * 4)

    @property
    def static(self):
        return 'static' if self.isStruct else 'class'

    @property
    def decltype(self):
        return 'struct' if self.isStruct else 'class'

    @property
    def name(self):
        return self._name

    @property
    def variables(self):
        return self._variables


class SwiftVariable():
    def __init__(self, tokens, node):
        self._name              = node['key.name']
        self._typename          = node['key.typename']
        self._defaultValue      = None

        self._annotations = {}
        self._parsedTypename = parseTypename(self._typename)

        ## FIXME: parse for default value and annotations in comment
        ttr = tokenrange(tokens, node['key.offset'], node['key.length'])
        tts = [tokens[i] for i in ttr]
        ttt = [(i['content'], i['nextString']) for i in tts]
        # cut last element to '\n'
        last = ttt[-1]
        ln = last[1]
        idx = ln.find('\n')
        if idx >= 0:
            ln = ln[:idx]
        ttt[-1] = (last[0], ln)

        self._parsedDeclaration = unicode(''.join([i[0] + i[1] for i in ttt]))

        pos = min(ttr[-1] + 1, len(tokens) - 1)
        t = tokens[pos]
        lineNumber = tts[-1]['lineNumber']
        ln = t['lineNumber']

        while lineNumber == ln and pos + 1 < len(tokens):
            if t['type'] != 'source.lang.swift.syntaxtype.comment':
                break
            self._parsedDeclaration += t['content']
            pos += 1
            t = tokens[pos]
            ln = t['lineNumber']

        self._parseDeclaration()

    def _parseDeclaration(self):
        def parseDefaultValue(decl_part):
            ts = decl_part.split('=')
            if len(ts) == 2:
                self._defaultValue = ts[1].strip()

            if not self._defaultValue and self.isOptional:
                self._defaultValue = 'nil'

        cs = self._parsedDeclaration.split('//')
        if len(cs) >= 2:
            # the first comment area is only checked
            self._annotations = parseAnnotation(cs[1])

        parseDefaultValue(cs[0])

    @property
    def name(self):
        return self._name

    @property
    def defaultValue(self):
        return self._defaultValue

    @property
    def typename(self):
        return self._typename

    @property
    def baseTypename(self):
        return self._parsedTypename.baseTypename

    @property
    def isOptional(self):
        return self._typename[-1] == '?'

    @property
    def isArray(self):
        return self._typename[0] == '['

    @property
    def isArrayOfOptional(self):
        return len(self._typename) > 3 and self._typename[-2] == ']?'

    @property
    def parsedDeclarationWithoutDefaultValue(self):
        # FIXME: remove default value only
        s = self._parsedDeclaration.split('=')
        return 'public ' + s[0].strip()


class SwiftEnum():
    def __init__(self, tokens, node):
        def getCases(tokens, offset, length):
            cases = []
            for i in tokenrange(tokens, offset, length):
                t = tokens[i]
                if t['content'] == 'enum' and t['type'] == 'source.lang.swift.syntaxtype.keyword':
                    break # TODO: skip insteadof break
                if t['content'] == 'case' and t['type'] == 'source.lang.swift.syntaxtype.keyword':
                    cases.append(SwiftCase(tokens, i + 1, offset, length))
            return cases

        self._bodyOffset = node['key.bodyoffset']
        self._bodyLength = node['key.bodylength']

        self._name = node['key.name']
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

        self._cases = getCases(tokens, self._bodyOffset, self._bodyLength)

        self._innerClassesOrEnums = []
        subs = node.get('key.substructure', None)
        if subs:
            self._innerClassesOrEnums = visitSubstructure(getClassOrEnum, tokens, subs, [])

    def getDeclarationString(self, protocols, level = 0):
        template = Template('''
public enum ${enum.name}${inh} {
    % for c in enum._cases:
    case ${c}
    % endfor

% for r in rs:
//
${r}
% endfor

% for s in sub:
//
${s}
% endfor
}
''')
        ps = getIdlProtocolsByNames(protocols, self._inheritedTypes)
        if len(ps) == 0:
            ps = getDefaultIdlProtocol(protocols, 'EnumDefault')

        rawType = self.getRawType(protocols)

        typeInheritances = sum([e.protocolEnum for e in ps], [rawType] if rawType != None else [])
        output = template.render(enum=self,
                                 rs=filter(lambda e: e != None, map(lambda e: e.processEnum(self, rawType), ps)),
                                 inh=': ' + ', '.join(typeInheritances) if len(typeInheritances) > 0 else '',
                                 sub=map(lambda e: e.getDeclarationString(protocols, level + 1), self._innerClassesOrEnums))
        return indent(output, level * 4)

    @property
    def name(self):
        return self._name

    @property
    def inheritedTypes(self):
        return self._inheritedTypes

    @property
    def isRawType(self):
        if len(self._inheritedTypes) > 0:
            n = self._inheritedTypes[0]
            return n in ['String', 'Int', 'Float', 'Character'] # FIXME: naive guess
        return False

    def getRawType(self, protocols):
        if len(self._inheritedTypes) > 0:
            n = self._inheritedTypes[0]
            return n if self.isRawType else None


class SwiftCase():
    def __init__(self, tokens, pos, offset, length):
        self._label = None
        self._value = None
        self._assocVals = []
        self._annotations = {}

        paramPositon = 0
        t = tokens[pos]
        lineNumber = t['lineNumber']
        while t['offset'] < offset + length:
            if t['type'] == 'source.lang.swift.syntaxtype.keyword': break
            if t['type'] == 'source.lang.swift.syntaxtype.comment':
                ln = t['lineNumber']
                if ln == lineNumber:
                    content = t['content']
                    cs = content.split('//')
                    if len(cs) >= 2:
                        # the first comment area is only checked
                        self._annotations = parseAnnotation(unicode(cs[1]))
            else:
                q = lambda st, ch: len(st) if st.find(ch) == -1 else st.find(ch)

                tk = t['content']
                ps = t['prevString']
                ns = t['nextString']
                isType = ps.find(':') >= 0
                isValue = ps.find('=') >= 0
                # `sourcekitten syntax` doesn't contain any token of array and optional. So we have to guess roughly...
                isArray    = ps.find('[') > ps.find(':') >= 0
                isOptional = 0 <= ns.find('?') < min(q(ns, ','), q(ns, ')'))

                if self._label == None:
                    self._label = tk # first-element
                elif isValue:
                    self._value = tk
                elif isType:
                    tp = self._assocVals[-1]
                    self._assocVals[-1] = SwiftCaseAssocValue(tp._typename, tk, tp._positon, isArray, isOptional)
                else:
                    tp = SwiftCaseAssocValue(None, tk, paramPositon) # overridden when typename is taken
                    self._assocVals.append(tp)
                    paramPositon += 1

            pos += 1
            if pos >= len(tokens): break
            t = tokens[pos]

    @property
    def letString(self):
        if len(self._assocVals) == 0: return ''
        return '(let (' + ', '.join([i.varname for i in self._assocVals]) + '))'

    def __repr__(self):
        if self._value:
            return str(self._label) + ' = ' + str(self._value)
        else:
            p = ''
            if len(self._assocVals) > 0:
                p = '(' + ', '.join(map(str, self._assocVals)) + ')'
            return str(self._label) + p


class SwiftCaseAssocValue():
    def __init__(self, name, typename, positon, isArray = False, isOptional = False):
        self._name = name
        self._typename = typename
        self._positon = positon
        self._isArray = isArray
        self._isOptional = isOptional

    @property
    def name(self):
        return self._name

    @property
    def typename(self):
        tk = self._typename
        if self._isArray: tk = '[' + tk + ']'
        if self._isOptional: tk = tk + '?'
        return tk

    @property
    def varname(self):
        return self._name if self._name else 'v' + str(self._positon)

    @property
    def keyname(self):
        return self._name if self._name else str(self._positon)

    def __repr__(self):
        if self._name:
            return self._name + ': ' + self.typename
        else:
            return self.typename


class SwiftProtocol():
    def __init__(self, tokens, node):
        self._name = node['key.name']
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))
        self._clazz = None

    @property
    def clazz(self):
        return self._clazz

    @property
    def name(self):
        return self._name

    @property
    def inheritedTypes(self):
        return self._inheritedTypes

    def setClass(self, clazz):
        self._clazz = clazz


class SwiftTypename():
    def __init__(self, typename):
        self._typename = typename

    def __repr__(self):
        return self._typename

    @property
    def baseTypename(self):
        return self._typename

class SwiftArray():
    def __init__(self, targetClass):
        self._targetClass = targetClass

    def __repr__(self):
        return '[' + str(self._targetClass) + ']'

    @property
    def baseTypename(self):
        return self._targetClass.baseTypename

class SwiftOptional():
    def __init__(self, targetClass):
        self._targetClass = targetClass

    def __repr__(self):
        return str(self._targetClass) + '?'

    @property
    def baseTypename(self):
        return self._targetClass.baseTypename


def parseTypename(typename):
    if typename[-1] == '?':
        return SwiftOptional(parseTypename(typename[:-1]))
    elif typename[0] == '[' and typename[-1] == ']':
        return SwiftArray(parseTypename(typename[1:-1]))
    else:
        return SwiftTypename(typename)


class ClassInit():
    @property
    def protocolClass(self):
        return []

    def processClass(self, swiftClass):
        template = Template('''
<%
    p = ', '.join([v.name + ': ' + v.typename + (' = ' + v.defaultValue if v.defaultValue else '') for v in clazz.variables])
%>
public init(${p}) {
    % for v in clazz.variables:
    self.${v.name} = ${v.name}
    % endfor
}
''')
        return indent(template.render(clazz=swiftClass))


class JSONDecodable():
    @property
    def protocolClass(self):
        return ['JSONDecodable']

    @property
    def protocolEnum(self):
        return ['JSONDecodable']

    def processClass(self, swiftClass):
        template = Template('''
<% anon = annotations['json'] %>
public ${clazz.static} func parseJSON(data: AnyObject) throws -> ${clazz.name} {
    if !(data is NSDictionary) {
        throw JSONDecodeError.TypeMismatch(key: "${clazz.name}", type: "NSDictionary")
    }
    % for v in clazz.variables:
    <%
        an = anon(v)
        parse = 'parseJSONArrayForNullable' if v.isArrayOfOptional else 'parseJSONArray'
    %>
    //
    % if not an.jsonOmitValue:
    let ${v.name}: ${v.typename}
    if let v: AnyObject = data["${an.jsonLabel}"] {
        if v is NSNull {
        % if v.defaultValue:
            ${v.name} = ${v.defaultValue}
        % else:
            throw JSONDecodeError.NonNullablle(key: "${an.jsonLabel}")
        % endif
        } else {
            do {
            % if v.isArray:
                ${v.name} = try ${v.baseTypename}.${parse}(v)
            } catch JSONDecodeError.NonNullablle {
                throw JSONDecodeError.NonNullablle(key: "${an.jsonLabel}")
            % else:
                ${v.name} = try ${v.baseTypename}.parseJSON(v)
            % endif
            } catch JSONDecodeError.ValueTranslationFailed {
                throw JSONDecodeError.TypeMismatch(key: "${an.jsonLabel}", type: "${v.baseTypename}")
            }
        }
    } else {
    % if v.defaultValue:
        ${v.name} = ${v.defaultValue}
    % else:
        throw JSONDecodeError.MissingKey(key: "${an.jsonLabel}")
    % endif
    }
    % endif
    % endfor
    //
    <% jsonInits = ', '.join([v.name + ': ' + v.name for v in clazz.variables if not anon(v).jsonOmitValue]) %>
    return ${clazz.name}(${jsonInits})
}
''')
        return indent(template.render(clazz=swiftClass, annotations={'json': JSONAnnotation}))

    def processEnum(self, swiftEnum, rawType):
        template = Template('''
public static func parseJSON(data: AnyObject) throws -> ${enum.name} {
% if rawType:
    if let v = data as? ${enum.inheritedTypes[0]}, val = ${enum.name}(rawValue: v) {
        return val
    }
% else:
% for case in enum._cases:
    if let obj: AnyObject = data["${case._label}"] {
        % for v in case._assocVals:
        let ${v.name}: ${v.typename}
        if let vo = obj["${v.keyname}"], v = vo {
            do {
                ${v.name} = try ${v.typename}.parseJSON(v)
            } catch JSONDecodeError.ValueTranslationFailed {
                throw JSONDecodeError.TypeMismatch(key: "${v.keyname}", type: "${v.typename}")
            }
        } else {
            throw JSONDecodeError.MissingKey(key: "${v.keyname}")
        }
        % endfor
        //
        <%
            init = ', '.join([(v.name + ': ' + v.name) if v.name else v.name for v in case._assocVals])
        %>
        return .${case._label}(${init})
    }
% endfor
% endif
    throw JSONDecodeError.ValueTranslationFailed(type: "${enum.name}")
}
''')
        return indent(template.render(enum=swiftEnum, rawType=rawType, annotations={'json': JSONAnnotation}))


class EJDB():
    @property
    def protocolClass(self):
        return []

    def processClass(self, swiftClass):
        template = Template('''
<% anon = annotations['json'] %>
public ${clazz.static} func parseBSON(iter: BSONIterater) throws -> ${clazz.name} {
    % for v in clazz.variables:
    <%
        an = anon(v)
        parse = 'parseBSONArrayForNullable' if v.isArrayOfOptional else 'parseBSONArray'
    %>
    //
    % if not an.jsonOmitValue:
    let ${v.name}: ${v.typename}
    do {
        let type = itor.find("${an.jsonLabel}")

        if type == BSON_UNDEFINED {
        % if v.defaultValue:
            ${v.name} = ${v.defaultValue}
        % else:
            throw BSONDecodeError.MissingKey(key: "${an.jsonLabel}")
        % endif
        } else if type == BSON_NULL {
        % if v.defaultValue:
            ${v.name} = ${v.defaultValue}
        % else:
            throw JSONDecodeError.TypeMismatch(key: "${an.jsonLabel}", type: "${v.baseTypename}")
        % endif
        } else {
        % if v.isArray:
            ${v.name} = try ${v.baseTypename}.${parse}(itor)
        % else:
            ${v.name} = try ${v.baseTypename}.parseBSON(itor)
        % endif
        }
    }
    % endif
    % endfor
    //
    <% jsonInits = ', '.join([v.name + ': ' + v.name for v in clazz.variables if not anon(v).jsonOmitValue]) %>
    return ${clazz.name}(${jsonInits})
}
''')
        return indent(template.render(clazz=swiftClass, annotations={'json': JSONAnnotation}))


class JSONEncodable():
    @property
    def protocolClass(self):
        return ['JSONEncodable']

    @property
    def protocolEnum(self):
        return []

    def processClass(self, swiftClass):
        template = Template('''
<%  anon = annotations['json'] %>
public func toJSON() -> [String: AnyObject] {
    return [
    % for v in clazz.variables:
    <% an = anon(v) %>
    % if an.jsonOmitValue:
        <%doc>nohting</%doc>
    % elif v.isArray:
        <% z = '(' + v.name + ' ?? [])' if v.isOptional else v.name %>
        "${an.jsonLabel}": ${z}.map { $0.toJSON() }
    % elif v.isOptional:
        "${an.jsonLabel}": ${v.name}.map { $0.toJSON() } ?? NSNull()
    %else:
        "${an.jsonLabel}": ${v.name}.toJSON()
    % endif
    % endfor
    ]
}
''')
        return indent(template.render(clazz=swiftClass, annotations={'json': JSONAnnotation}))

    def processEnum(self, swiftEnum, rawType):
        template = Template('''
public func toJSON() -> ${enum.inheritedTypes[0]} {
    return rawValue
}
''')
        return indent(template.render(enum=swiftEnum, annotations={'json': JSONAnnotation}))

class ErrorType():
    @property
    def protocolClass(self):
        assert('ErrorType is not allowed for class')

    @property
    def protocolEnum(self):
        return ['ErrorType']

    def processEnum(self, swiftEnum, rawType):
        return None


class NSCoding():
    @property
    def protocolClass(self):
        return ['NSCoding']

    @property
    def protocolEnum(self):
        assert('NSCoding is not allowed for enum')

    def processClass(self, swiftClass):
        template = Template('''
required public init?(coder: NSCoder) {
    var failed = false

% for v in clazz.variables:
% if v.typename == 'Int':
    ${v.name} = coder.decodeIntegerForKey(${v.name})
% elif v.typename == 'Float':
    ${v.name} = coder.decodeFloatForKey(${v.name})
% else:
    if let ${v.name} = coder.decodeObjectForKey(${v.name}) as? ${v.typename} {
        self.${v.name} = ${v.name}
    } else {
        self.${v.name} = ${v.typename}()  // FIXME: set default value
        failed = true
    }
% endif
% endfor

    if failed {
        return // nil
    }
}
//
public func encodeWithCoder(coder: NSCoder) {
% for v in clazz.variables:
% if v.typename == 'Int':
    coder.encodeInteger(${v.name}, forKey: "${v.name}")
% elif v.typename == 'Float':
    coder.encodeFloat(${v.name}, forKey: "${v.name}")
% else:
    coder.encodeObject(${v.name}, forKey: "${v.name}")
% endif
% endfor
}
''')
        return indent(template.render(clazz=swiftClass))


class Printable():
    @property
    def protocolClass(self):
        return ['CustomStringConvertible']

    @property
    def protocolEnum(self):
        return ['CustomStringConvertible']

    def processClass(self, swiftClass):
        # FIXME: always public
        template = Template('''
<%
    p = ", ".join(["%s=\(%s)" % (v.name, v.name) for v in clazz.variables])
%>
public var description: String {
    return "${clazz.name}(${p})"
}
''')
        return indent(template.render(clazz=swiftClass))

    def processEnum(self, swiftEnum, rawType):
        if rawType != None:
            return '    public var description: String { return rawValue }'

        template = Template('''
public var description: String {
    switch self {
    % for case in enum._cases:
    <%
        av  = ['%s=\(%s)' % (v._name, v._name) if v._name else '\(%s)' % v.varname for v in case._assocVals]
        out = '(' + ', '.join(av) + ')' if len(av) else ''
    %>
    case .${case._label}${case.letString}: return "${case._label}${out}"
    % endfor
    }
}
''')
        return indent(template.render(enum=swiftEnum))


class EnumStaticInit():
    @property
    def protocolClass(self):
        return []

    @property
    def protocolEnum(self):
        return []

    def processClass(self, swiftClass):
        return None

    def processEnum(self, swiftEnum, rawType):
        if rawType != None:
            return None # FIXME
        else:
            template = Template('''
% for case in enum._cases:
<%
    ais = map(lambda x: '%s: %s = %s()' % (x._name, x.typename, x.typename) if x._name else 'arg%d: %s = %s()' % (x._positon, x.typename, x.typename), case._assocVals)
    cis = map(lambda x: '%s: %s' % (x._name, x._name) if x._name else 'arg%d' % x._positon, case._assocVals)
    params = ", ".join(ais)
    out    = '(' + ', '.join(cis) + ')' if len(cis) > 0 else ''
%>
public static func make${case._label}(${params}) -> ${enum.name} {
    return .${case._label}${out}
}
% endfor
''')
            return indent(template.render(enum=swiftEnum))


class URLRequestHelper():
    @property
    def protocolClass(self):
        return []

    @property
    def protocolEnum(self):
        return []

    def processClass(self, swiftClass):
        return None

    def processEnum(self, swiftEnum, rawType):
        if rawType != None:
            return None # FIXME
        else:
            template = Template('''
<% anon = annotations['router'] %>
public var method: String {
    switch self {
    % for case in enum._cases:
    <% an = anon(case) %>
    case .${case._label}: return "${an.method}"
    % endfor
    }
}
//
public var path: String {
    switch self {
    % for case in enum._cases:
    <% an = anon(case) %>
    case .${case._label}${an.casePathString}: return "${an.path}"
    % endfor
    }
}
//
public var params: [String: AnyObject] {
    switch self {
    % for case in enum._cases:
    <%
        def toJsonString(info):
             if info._isArray: return info._name + '.map { $0.toJSON() }'
             return info._name + '.toJSON()'

        an = anon(case)
        pathParams, caseParams = an.paramSets()
        diff = set(caseParams).difference(set(pathParams))

        lets = [i if i in diff else '_' for i in caseParams]
        letString = ('(let (' + ', '.join(lets) + '))') if len(diff) > 0 else ''

        dicx = [i for i in case._assocVals if i._name in diff]

        inits   = ['"%s": %s' % (i._name, toJsonString(i)) for i in dicx if not i._isOptional]
        initStr = ', '.join(inits) if len(inits) else ':'

        params = ['%s.map { p["%s"] = $0.toJSON() }' % (i._name, i._name) for i in dicx if i._isOptional]
    %>
    % if len(diff) > 0:
    case .${case._label}${letString}:
        % if len(params) > 0:
        var p: [String: AnyObject] = [${initStr}]
        % for p in params:
        ${p}
        % endfor
        return p
        % else:
        return [${initStr}]
        % endif
    % endif
    % endfor
    default: return [:]
    }
}
''')
            return indent(template.render(enum=swiftEnum, annotations={'router': RouterAnnotation}))


class APIKitHelper():
    @property
    def protocolEnum(self):
        return []

    def processEnum(self, swiftEnum, rawType):
        if rawType != None:
            return None # FIXME
        else:
            template = Template('''
<% anon = annotations['router'] %>
public enum api {
    % for case in enum._cases:
    <%
        an = anon(case)
        inits = map(lambda x: '%s: %s = %s()' % (x._name, x.typename, x.typename) if x._name else 'arg%d: %s = %s()' % (x._positon, x.typename, x.typename), case._assocVals)
        cs = map(lambda x: '%s: %s' % (x._name, x._name) if x._name else 'arg%d' % x._positon, case._assocVals)
        assocs = '(' + ', '.join(cs) + ')' if len(cs) else ''
    %>
    public class ${case._label}: APIKitRequest<${an.responseType}> {
        public init(${', '.join(inits)}) {
            super.init(route: .${case._label}${assocs})
        }
    }
    % endfor
}
//
public var method: String {
    switch self {
    % for case in enum._cases:
    <% an = anon(case) %>
    case .${case._label}: return "${an.method}"
    % endfor
    }
}
//
public var path: String {
    switch self {
    % for case in enum._cases:
    <% an = anon(case) %>
    case .${case._label}${an.casePathString}: return "${an.path}"
    % endfor
    }
}
//
public var params: [String: AnyObject] {
    switch self {
    % for case in enum._cases:
    <%
        def toJsonString(info):
             if info._isArray: return info._name + '.map { $0.toJSON() }'
             return info._name + '.toJSON()'

        an = anon(case)
        pathParams, caseParams = an.paramSets()
        diff = set(caseParams).difference(set(pathParams))

        lets = [i if i in diff else '_' for i in caseParams]
        letString = ('(let (' + ', '.join(lets) + '))') if len(diff) > 0 else ''

        dicx = [i for i in case._assocVals if i._name in diff]

        inits   = ['"%s": %s' % (i._name, toJsonString(i)) for i in dicx if not i._isOptional]
        initStr = ', '.join(inits) if len(inits) else ':'

        params = ['%s.map { p["%s"] = $0.toJSON() }' % (i._name, i._name) for i in dicx if i._isOptional]
    %>
    % if len(diff) > 0:
    case .${case._label}${letString}:
        % if len(params) > 0:
        var p: [String: AnyObject] = [${initStr}]
        % for p in params:
        ${p}
        % endfor
        return p
        % else:
        return [${initStr}]
        % endif
    % endif
    % endfor
    default: return [:]
    }
}
''')
            return indent(template.render(enum=swiftEnum, annotations={'router': RouterAnnotation}))


def processProject(func, parsed_doc):
    def visit(filepath, contents):
        sublist = contents.get('key.substructure', None)
        tokens = sourcekitten_syntax(filepath)

        tmp = []
        for i in sublist:
            tmp = func(tokens, tmp, i)
        return tmp

    assert(all([len(i) == 1 for i in parsed_doc]))
    return {dic.keys()[0]:visit(*dic.items()[0]) for dic in parsed_doc}


def visitSubstructure(func, tokens, sublist, initial):
    tmp = initial
    for i in sublist:
        tmp = func(tokens, tmp, i)
    return tmp


def getClassOrEnum(tokens, a, n):
    if n.get('key.kind', None) == 'source.lang.swift.decl.class' or n.get('key.kind', None) == 'source.lang.swift.decl.struct':
        return a + [SwiftClass(tokens, n)]
    elif n.get('key.kind', None) == 'source.lang.swift.decl.enum':
        return a + [SwiftEnum(tokens, n)]
    else:
        return a

def gatherIdlProtocol():
    def getProtocol(tokens, a, n):
        if n.get('key.kind', None) == 'source.lang.swift.decl.protocol':
            return a + [SwiftProtocol(tokens, n)]
        else:
            return a

    protocols = processProject(getProtocol, parsed)
    flatprotos = sum(protocols.values(), [])

    for p in flatprotos:
        c = globals().get(p.name, None)
        if c:
            p.setClass(c)

    return flatprotos


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=PROGRAM_NAME + ': Swift source generator from Swift')
    parser.add_argument('project', type=str, nargs='?', default='IDL.xcodeproj', help='project to parse')
    parser.add_argument('scheme', type=str, nargs='?', default='IDL', help='sceheme to parse')
    parser.add_argument('-o', '--output_dir', type=str, default='out', help='directory to output')
    parser.add_argument('-f', '--force', action='store_true', help='force to output')
    args = parser.parse_args()

    if not os.path.isdir(args.output_dir):
        print('output directory not found: ' + args.output_dir)
        exit(0)

    parsed = sourcekitten_doc(args.project, args.scheme)
    protocols = gatherIdlProtocol()

    classOrEnums = processProject(getClassOrEnum, parsed)

    for filepath, coe in classOrEnums.items():
        if len(coe) == 0: continue
        head, filename = os.path.split(filepath)

        outpath = os.path.join(args.output_dir, filename)
        exists = os.path.exists(outpath)
        if not args.force and exists:
            print('Error: output file is already exists: ' + outpath)
            exit(0)

        with file(outpath, 'w') as out:
            print(outpath + (' (overwritten)' if exists else ''))

            out.write('// This file was auto-generated from %s with %s.' % (filename, PROGRAM_NAME))
            out.write('\n\n')
            out.write('import Foundation')
            out.write('\n\n')
            map(lambda e: out.write(e.getDeclarationString(protocols) + '\n\n'), coe)
