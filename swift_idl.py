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


### Tokens

class SwiftToken():
    def __init__(self, line, offset, tokenType, content):
        self.line      = line
        self.offset    = offset
        self.content   = content
        self.tokenType = tokenType

    def __repr__(self):
        return self.content

    @property
    def isComment(self):
        return self.tokenType == 'source.lang.swift.syntaxtype.comment'

    @property
    def annotations(self):
        def parseAnnon(s):
            m = re.match(r'^(\w+):"([^"]+)"$', s)
            if m:
                g = m.groups()
                if len(g) == 2:
                    return (g[0].lower(), map(str.strip, g[1].split(',')))
            return None

        cs = self.content.split('//')
        if len(cs) >= 2:
            # the first comment area is only checked
            splitted = re.findall(r'\b\w+:"[^"]+"', cs[1])
            return {q[0]:q[1] for q in map(parseAnnon, splitted) if q != None}
        return {}


def getLinenumberFunction(source):
    linenum_map = map(lambda x: 1 if x == '\n' else 0, source) # FIXME: check CRLF or LF
    def func(offset):
        return sum(linenum_map[:offset], 1)
    return func

def getOmittedTokens(tokens, source):
    get_linenumber = getLinenumberFunction(source)

    omitted = []
    prev_offset = 0

    def append_if_not_empty(prev_offset, offset):
        s = source[prev_offset : offset]
        omitted.append(SwiftToken(get_linenumber(prev_offset), prev_offset, "omittedtoken", s))

    def process_token(tk):
        offset = tk['offset']
        append_if_not_empty(prev_offset, offset)
        return offset + tk['length']

    for i in tokens:
        prev_offset = process_token(i)
    append_if_not_empty(prev_offset, len(source))

    return omitted

def getSwiftTokens(tokens, source):
    get_linenumber = getLinenumberFunction(source)

    def conv(tk):
        offset = tk['offset']
        length = tk['length']
        tktype = tk['type']
        s = source[offset : offset + length]
        return SwiftToken(get_linenumber(offset), offset, tktype, s)

    ts = [conv(e) for e in tokens]
    os = getOmittedTokens(tokens, source)
    return sorted(ts + os, key=lambda e: e.offset)

def tokenrange(tokens, offset, length):
    start = None
    end = len(tokens)
    for i in range(end):
        t = tokens[i]
        if t.offset < offset: continue
        if not start: start = i
        if t.offset >= offset + length:
            end = i
            break
    return range(start, end)

### Swift Typename

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

### Swift structures base

# variable of tuple in case
class SwiftTupleVariable(object):
    def __init__(self, name, typename):
        self._name     = name     # maybe None
        self._typename = typename

    @property
    def name(self): return self._name

    @property
    def typename(self): return self._typename

    @property
    def isOptional(self): return self._typename[-1] == '?' # FIXME

    @property
    def isArray(self): return self._typename[0] == '[' # FIXME

    @property
    def isArrayOfOptional(self): return len(self._typename) > 3 and self._typename[-2] == ']?' # FIXME

    @property
    def baseTypename(self):
        return parseTypename(self._typename).baseTypename

# variable in struct or class
class SwiftVariable(SwiftTupleVariable):
    def __init__(self, name, typename, defaultValue, accessibility, annotations, parsedDeclaration):
        super(SwiftVariable, self).__init__(name, typename)
        self._defaultValue  = defaultValue
        self._accessibility = accessibility
        self._annotations   = annotations
        self._parsedDeclaration = parsedDeclaration

    @property
    def defaultValue(self): return self._defaultValue

    @property
    def accessibility(self): return self._accessibility

    @property
    def annotations(self): return self._annotations

    @property
    def parsedDeclarationWithoutDefaultValue(self):
        # FIXME: remove default value only
        s = self._parsedDeclaration.split('=')
        return 'public ' + s[0].strip()

# tuple in case or variables in struct or class
class SwiftVariableList(object):
    def __init__(self, name, variables, annotations):
        self._name        = name
        self._variables   = variables # SwiftVariable or SwiftTupleVariable
        self._annotations = annotations

    @property
    def name(self): return self._name

    @property
    def variables(self): return self._variables

    @property
    def annotations(self): return self._annotations

class SwiftCase(SwiftVariableList):
    pass

class SwiftEnum(object):
    def __init__(self, name, cases, inheritedTypes, annotations, substructure):
        self._name           = name
        self._cases          = cases
        self._inheritedTypes = inheritedTypes
        self._annotations    = annotations
        self._substructure   = substructure

    @property
    def name(self): return self._name

    @property
    def cases(self): return self._cases

    @property
    def annotations(self): return self._annotations

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
        ps = getIdlProtocols(protocols, self._inheritedTypes, 'EnumDefault')
        rawType = self.getRawType(protocols)
        typeInheritances = sum([e.protocolEnum for e in ps], [rawType] if rawType != None else [])

        output = template.render(enum=self,
                                 rs=filter(lambda e: e != None, map(lambda e: e.processEnum(self, rawType), ps)),
                                 inh=': ' + ', '.join(typeInheritances) if len(typeInheritances) > 0 else '',
                                 sub=map(lambda e: e.getDeclarationString(protocols, level + 1), self._innerClassesOrEnums))
        return indent(output, level * 4)

class SwiftClass(SwiftVariableList):
    def __init__(self, name, decltype, variables, inheritedTypes, annotations, substructure):
        super(SwiftClass, self).__init__(name, variables, annotations)
        self._decltype       = decltype
        self._inheritedTypes = inheritedTypes
        self._substructure   = substructure

    @property
    def name(self): return self._name

    @property
    def inheritedTypes(self): return self._inheritedTypes

    @property
    def decltype(self): return self._decltype

    @property
    def static(self): return 'static' if self._decltype == 'struct' else 'class'


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
        ps = getIdlProtocols(protocols, self._inheritedTypes, 'ClassDefault')
        typeInheritances = sum([e.protocolClass for e in ps], [])

        output = template.render(clazz=self,
                                 rs=[e.processClass(self) for e in ps],
                                 inh=': ' + ', '.join(typeInheritances) if len(typeInheritances) > 0 else '',
                                 sub=map(lambda e: e.getDeclarationString(protocols, level + 1), self._substructure))
        return indent(output, level * 4)


### Parsing functions

def visitProtocol(node, clazz):
    name = node['key.name']
    inheritedtypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))
    return SwiftProtocol(name, inheritedtypes, clazz)

def visitClass(node, tokens):
    def getVariables(a, n):
        if n.get('key.kind', None) == 'source.lang.swift.decl.var.instance':
            return a + [visitVariable(n, tokens)]
        return a

    name = node['key.name']
    decltype = 'struct' if node['key.kind'] == 'source.lang.swift.decl.struct' else 'class'

    variables = reduce(getVariables, node['key.substructure'], [])
    inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

    annotations = getAnnotations([tokens[i] for i in tokenrange(tokens, node['key.offset'], node['key.length'])])

    innerDecls = []
    subs = node.get('key.substructure', None)
    if subs:
        innerDecls = visitSubstructure(getDeclarations, tokens, subs, [])

    return SwiftClass(name, decltype, variables, inheritedTypes, annotations, innerDecls)

def visitEnum(node, tokens):
    # `sourcekitten doc` doesn't return `case` information. So we have to process.
    # LIMITATION:
    #   * You hove to declare `case`s of its enum first when you'll contain sub enums, due to parsing limitation.
    #   * You cannot include tuple in case.
    def getCases(tokens, offset, length):
        cases = []
        begin = None
        for i in tokenrange(tokens, offset, length):
            p = tokens[i - 1]
            t = tokens[i]
            if t.tokenType == 'source.lang.swift.syntaxtype.keyword':
                if t.content != 'case': break
                if begin != None:
                    cases.append(visitCase(getTokenForDecl(tokens, begin, p.offset - begin)))
                begin = t.offset

        if begin != None:
            cases.append(visitCase(getTokenForDecl(tokens, begin, t.offset - begin + 1)))

        return cases

    name = node['key.name']
    inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

    bodyOffset = node['key.bodyoffset']
    bodyLength = node['key.bodylength']
    cases = getCases(tokens, bodyOffset, bodyLength)

    innerDecls = []
    subs = node.get('key.substructure', None)
    if subs:
        innerDecls = visitSubstructure(getClassOrEnum, tokens, subs, [])

    annotations = {} # FIXME
    return SwiftEnum(name, cases, inheritedTypes, annotations, innerDecls)

def visitVariable(node, tokens):
    offset = node['key.offset']
    length = node['key.length']

    def getDefaultValue():
        tkrange = tokenrange(tokens, offset, length)
        var_tokens = [tokens[i] for i in tkrange]
        eqs = [e[0] for e in enumerate(var_tokens) if e[1].tokenType == 'omittedtoken' and e[1].content.strip() == '=']
        assert(len(eqs) <= 1)
        if len(eqs) == 1:
            p = eqs[0]
            if p + 1 < len(var_tokens):
                return var_tokens[p + 1].content
        return None

    name          = node['key.name']
    typename      = node['key.typename']
    defaultValue  = getDefaultValue()
    decl_tokens   = getTokenForDecl(tokens, offset, length)
    accessibility = None # FIXME
    parsedDecl    = ''.join([i.content for i in decl_tokens]).strip() # FIXME: unused
    annotations   = getAnnotations(decl_tokens)
    return SwiftVariable(name, typename, defaultValue, accessibility, annotations, parsedDecl)

def visitCase(tokens):
    assert(tokens[0].content == 'case' and tokens[0].tokenType == 'source.lang.swift.syntaxtype.keyword')
    assert(tokens[1].tokenType == 'omittedtoken')

    label = None
    assocVals = []
    tuple_pair = None
    for t in tokens[2:]:
        if t.tokenType == 'source.lang.swift.syntaxtype.identifier':
            if label == None:
                label = t.content
            else:
                if tuple_pair == None:
                    tuple_pair = (None, t.content)
                else:
                    tuple_pair = (tuple_pair[1], t.content)
        elif t.tokenType == 'omittedtoken':
            if t.content.strip() == ',':
                assocVals.append(SwiftTupleVariable(*tuple_pair))
                tuple_pair = None

    if tuple_pair != None:
        assocVals.append(SwiftTupleVariable(*tuple_pair))

    annotations = getAnnotations(tokens)
    return SwiftCase(label, assocVals, annotations)

def getTokenForDecl(tokens, offset, length):
    return [tokens[i] for i in getRangeForDecl(tokens, offset, length)]

def getRangeForDecl(tokens, offset, length):
    tkrange = tokenrange(tokens, offset, length)
    start   = tkrange[0]
    end     = tkrange[-1] + 1

    # include other elements of last line
    last_linenum = tokens[end - 1].line
    for pos in range(end, len(tokens)):
        if tokens[pos].line != last_linenum:
            return range(start, pos)

    return range(start, end)

def getAnnotations(tokens):
    for t in tokens:
        if t.isComment:
            annons = t.annotations
            if len(annons) > 0:
                return annons # FIXME: merge two or more annotation comments
    return {}

def processProject(func, structure):
    def getTokenList(filepath):
        with file(filepath) as f:
            source = f.read()
            syntax = sourcekittenSyntax(filepath)
            return getSwiftTokens(syntax, source)

    def visit(filepath, contents):
        sublist = contents.get('key.substructure', None)
        tokens = getTokenList(filepath)

        tmp_list = []
        for node in sublist:
            tmp_list = func(tmp_list, node, tokens)
        return tmp_list

    assert(all([len(i) == 1 for i in structure]))
    return {dic.keys()[0]:visit(*dic.items()[0]) for dic in structure}

def visitSubstructure(func, tokens, sublist, initial_list):
    tmp_list = initial_list
    for node in sublist:
        tmp_list = func(tmp_list, node, tokens)
    return tmp_list


### Annotation classes

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

    def paramSets(self):
        pathParams = re.findall(r'\(([^)]+)\)', self.path)
        caseParams = [i.name for i in self._case._assocVals if i.name != None] # FIXME: private access
        return (pathParams, caseParams)

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

### Protocols class and functions

class SwiftProtocol():
    def __init__(self, name, inheritedTypes, clazz):
        self._name           = name
        self._inheritedTypes = inheritedTypes
        self._clazz          = clazz

    @property
    def name(self): return self._name

    @property
    def inheritedTypes(self): return self._inheritedTypes

    @property
    def clazz(self): return self._clazz


def getIdlProtocols(protocols, typenames, default_protocols):
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

    ps = getIdlProtocolsByNames(protocols, typenames)
    if len(ps) == 0:
        ps = getDefaultIdlProtocol(protocols, default_protocols)
    return ps

### Render functions

def indent(text, width=4):
    lines = [' ' * width + t for t in text.split('\n') if len(t.strip()) > 0]
    if width == 0: lines = [t if t.strip() != '//' else '' for t in lines]
    return '\n'.join(lines)

### Render Class (IDL protocols)

class ClassInit():
    @property
    def protocolClass(self): return []

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
    def protocolClass(self): return ['JSONDecodable']

    @property
    def protocolEnum(self): return ['JSONDecodable']

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


class JSONEncodable():
    @property
    def protocolClass(self): return ['JSONEncodable']

    @property
    def protocolEnum(self): return []

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

class EJDB():
    @property
    def protocolClass(self): return []

    def processClass(self, swiftClass):
        template = Template('''
<% anon = annotations['json'] %>
public ${clazz.static} func parseBSON(iter: BSONIterater) throws -> ${clazz.name} {
    % for v in clazz.variables:
    <%
        an = anon(v)
        parse = 'parseBSONArrayForNullable' if v.isArrayOfOptional else 'parseBSONArray'
    %>
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
    //
    % endfor
    <% jsonInits = ', '.join([v.name + ': ' + v.name for v in clazz.variables if not anon(v).jsonOmitValue]) %>
    return ${clazz.name}(${jsonInits})
}
''')
        return indent(template.render(clazz=swiftClass, annotations={'json': JSONAnnotation}))

class ErrorType():
    @property
    def protocolEnum(self): return ['ErrorType']

    def processEnum(self, swiftEnum, rawType): return []


class NSCoding():
    @property
    def protocolClass(self): return ['NSCoding']

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
    def protocolClass(self): return ['CustomStringConvertible']

    @property
    def protocolEnum(self): return ['CustomStringConvertible']

    def processClass(self, swiftClass):
        # FIXME: always public
        template = Template('''
<%
    p = ", ".join(["%s=\(%s)" % (v.name, v.name) for v in clazz.variables])
%>
public var description: String {
    return "${clazz.name}(${p})
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
    def protocolEnum(self): return []

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
    def protocolEnum(self): return []

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



### command line pipe lines

def execSourcekitten(args):
    p = subprocess.Popen([SOURCEKITTEN] + args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (out, err) = p.communicate()
    return json.loads(out)

def sourcekittenDoc(project, scheme):
    return execSourcekitten(['doc', '-project', project, '-scheme', scheme])

def sourcekittenSyntax(filepath):
    return execSourcekitten(['syntax', '--file', filepath])

def parseArgs():
    parser = argparse.ArgumentParser(description=PROGRAM_NAME + ': Swift source generator from Swift')
    parser.add_argument('project', type=str, nargs='?', default='IDL.xcodeproj', help='project to parse')
    parser.add_argument('scheme', type=str, nargs='?', default='IDL', help='sceheme to parse')
    parser.add_argument('-s', '--sourcekitten', type=str, default='sourcekitten', help='path to sourcekitten')
    parser.add_argument('-o', '--output_dir', type=str, default='out', help='directory to output')
    parser.add_argument('-f', '--force', action='store_true', help='force to output')
    return parser.parse_args()

def checkOutputDir(output_dir):
    if not os.path.isdir(output_dir):
        print('output directory not found: ' + output_dir)
        exit(0)

def gatherIDLProtocol(structure):
    def getProtocolNode(ls, n, tokens):
        if n.get('key.kind', None) == 'source.lang.swift.decl.protocol':
            return ls + [n]
        else:
            return ls

    protocol_nodes = sum(processProject(getProtocolNode, structure).values(), [])

    protocols = []
    for node in protocol_nodes:
        name = node['key.name']
        idl_clazz = globals().get(name, None)
        if idl_clazz:
            protocols.append(visitProtocol(node, idl_clazz))
        else:
            print("Warning: protocol '%s' can not be processed." % name)

    return protocols

def getDeclarations(ls, n, tokens):
    if n.get('key.kind', None) == 'source.lang.swift.decl.class' or n.get('key.kind', None) == 'source.lang.swift.decl.struct':
        return ls + [visitClass(n, tokens)]
    elif n.get('key.kind', None) == 'source.lang.swift.decl.enum':
        return ls + [visitEnum(n, tokens)]
    else:
        return ls

def execute():
    args = parseArgs()
    checkOutputDir(args.output_dir)
    structure = sourcekittenDoc(args.project, args.scheme)
    protocols = gatherIDLProtocol(structure)

    decls_map = processProject(getDeclarations, structure)

    for filepath, decls in decls_map.items():
        if len(decls) == 0: continue
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
            map(lambda e: out.write(e.getDeclarationString(protocols) + '\n\n'), decls)


if __name__ == '__main__':
    execute()
