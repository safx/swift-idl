#!/usr/bin/python

from __future__ import print_function

import functools
import json
import os
import re
import subprocess


SOURCEKITTEN='sourceKitten'

# All classes and Enums in IDL.xcodeproj
# * Class and Enum must not have any protocol.
# * Enum must have a row-type.
# * Inner class is not supported
# * All methods are discarded in output
# * All comments are discarded in output
# * Generics is not supposed.

# Supported types
#     * Int, Float, Bool and String
#     * Enum of String and Int
#     * NSDate and NSURL
#     * Optional and Array
#     * User declared class in IDL.xcodeproj

# Available JSON annotation
#   let foo: Int        // json:"-"
#   let fooBar: Int     // json:"foo-bar"
#   let fooBoo: Int     // json:",omitempty" (not supported currently)
#   let fooBee: Int     // json:",string"    (not supported currently)
#   case Unknown        // json:"-"          (not supported currently)


def sourcekitten_doc():
    p = subprocess.Popen([SOURCEKITTEN, 'doc', '-project', 'IDL.xcodeproj'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
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
        with file(filepath) as f:
            contents = f.read()
            map(addContent(contents), s)
            map(addLineNumber(contents), s)
            reduce(addPrevString(contents), s, 0)
            reduce(addNextString(contents), s, None)
            return s

    p = subprocess.Popen([SOURCEKITTEN, 'syntax', '--file', filepath], stdout=subprocess.PIPE)
    (stdoutdata, _) = p.communicate()
    tokens = addAdditionalInfo(json.loads(stdoutdata))
    return tokens


def tokenrange(tokens, offset, length):
    start = None
    end = len(tokens)
    for i in range(len(tokens)):
        t = tokens[i]
        if t['offset'] < offset: continue
        if not start: start = i
        if t['offset'] >= offset + length:
            end = i
            break
    return range(start, end)


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


def getIdlProtocolByName(protocols, name):
    for p in protocols:
        if p.name == name:
            return p._clazz # FIXME: private
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
            return getIdlProtocolsByNames(protocols, p._inheritedTypes) # FIXME: private
    return []


class SwiftClass():
    def __init__(self, tokens, node):
        self._bodyOffset = node['key.bodyoffset']
        self._bodyLength = node['key.bodylength']
        self._name = node['key.name']
        #self._parsedDeclaration = node['key.parsed_declaration']

        def getVariables(a, n):
            if n.get('key.kind', None) == 'source.lang.swift.decl.var.instance':
                return a + [SwiftVariable(tokens, n)]
            return a

        self._variables = reduce(getVariables, node['key.substructure'], [])
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

    def getDeclarationString(self, protocols):
        ps = getIdlProtocolsByNames(protocols, self._inheritedTypes)
        if len(ps) == 0:
            ps = getDefaultIdlProtocol(protocols, 'ClassDefault')

        typeInheritances = [i.protocolClass for i in ps if i.protocolClass != None]

        # FIXME: always public
        ret = 'public class ' + self._name
        if len(typeInheritances) > 0:
            ret += ': ' + ', '.join(typeInheritances)
        ret += ' {\n'
        ret += '\n'.join(map(lambda x: '    ' + x.parsedDeclarationWithoutDefaultValue, self._variables))
        ret += '\n'
        if len(ps) > 0:
            ret += '\n'
            ret += '\n\n'.join(map(lambda e: e.processClass(self), ps))
            ret += '\n'
        ret += '}\n'
        return ret

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
        #self._parsedDeclaration = node['key.parsed_declaration']
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

        pos = ttr[-1] + 1
        t = tokens[pos]
        lineNumber = tts[-1]['lineNumber']
        ln = t['lineNumber']

        while lineNumber == ln:
            if t['type'] != 'source.lang.swift.syntaxtype.comment':
                break
            self._parsedDeclaration += t['content']
            pos += 1
            t = tokens[pos]
            ln = t['lineNumber']

        self._parseDeclaration()

        #print(self._name, self._defaultValue, self._jsonOmitValue, self.isOptional, self._parsedTypename)

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

        #self._filepath   = filename
        self._bodyOffset = node['key.bodyoffset']
        self._bodyLength = node['key.bodylength']

        self._name              = node['key.name']
        #self._parsedDeclaration = node['key.parsed_declaration']
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

        #tokens = sourcekitten_syntax(self._filepath) # TODO: cache
        self._cases = getCases(tokens, self._bodyOffset, self._bodyLength)

        self._innerClassesOrEnums = []
        subs = node.get('key.substructure', None)
        if subs:
            self._innerClassesOrEnums = visitSubstructure(getClassOrEnum, tokens, subs, [])

    def getDeclarationString(self, protocols):
        ps = getIdlProtocolsByNames(protocols, self._inheritedTypes)
        if len(ps) == 0:
            ps = getDefaultIdlProtocol(protocols, 'EnumDefault')

        rawType = self.getRawType(protocols)

        typeInheritances = ([rawType] if rawType != None else []) + [i.protocolEnum for i in ps if i.protocolEnum != None]
        processedProtocols = filter(lambda e: e != None, map(lambda e: e.processEnum(self, rawType), ps))

        ret = 'public enum ' + self._name
        if len(typeInheritances) > 0:
            ret += ': ' + ', '.join(typeInheritances)
        ret += ' {'

        if len(self._cases) > 0:
            ret += '\n'
            for c in self._cases:
                ret += '    case ' + str(c)
                ret += '\n'

        if len(processedProtocols) > 0:
            ret += '\n'
            ret += '\n\n'.join(processedProtocols)
            ret += '\n'

        if len(self._innerClassesOrEnums) > 0:
            sub = map(lambda e: e.getDeclarationString(protocols), self._innerClassesOrEnums)
            ret = reduce(lambda a, e: a + e, sub, ret + '\n')
        ret += '}\n'
        return ret

    @property
    def name(self):
        return self._name

    @property
    def isRawType(self):
        if len(self._inheritedTypes) > 0:
            n = self._inheritedTypes[0]
            return n in ['String', 'Int', 'Float', 'Character'] # FIXME: naive guess
        return False

    def getRawType(self, protocols):
        if len(self._inheritedTypes) > 0:
            n = self._inheritedTypes[0]
            #return None if getIdlProtocolByName(protocols, n) else n
            return n if self.isRawType else None


class SwiftCase():
    def __init__(self, tokens, pos, offset, length):
        self._label = None
        self._value = None
        self._assocVals = []
        self._annotations = {}

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
                tk = t['content']
                ps = t['prevString']
                isType = ps.find(':') >= 0
                isValue = ps.find('=') >= 0
                # `sourcekitten syntax` doesn't contain any token of array or optional. So we have to guess roughly...
                isArray = ps.find('[') > ps.find(':') >= 0

                if self._label == None:
                    self._label = tk # first-element
                elif isValue:
                    self._value = tk
                elif isType:
                    tp = self._assocVals[-1]
                    if isArray: tk = '[' + tk + ']'
                    self._assocVals[-1] = (tp[1], tk)
                else:
                    tp = (None, tk)
                    self._assocVals.append(tp)

            pos += 1
            if pos >= len(tokens): break
            t = tokens[pos]

    def __repr__(self):
        if self._value:
            return str(self._label) + ' = ' + str(self._value)
        else:
            if len(self._assocVals) == 0:
                return str(self._label)
            else:
                return str(self._label) + '(' + ', '.join(map(lambda e: e[0] + ': ' + e[1], self._assocVals)) + ')'




class SwiftProtocol():
    def __init__(self, tokens, node):
        self._name              = node['key.name']
        #self._parsedDeclaration = node['key.parsed_declaration']
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))
        self._clazz = None

    @property
    def name(self):
        return self._name

    def setClass(self, clazz):
        self._clazz = clazz

    def __repr__(self):
        return self._name + '(' + str(self._clazz) + ') :- ' + ', '.join(self._inheritedTypes)



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
        return None

    def processClass(self, swiftClass):
        def param(p):
            return p.name + ': ' + p.typename + (' = ' + p.defaultValue if p.defaultValue != None else '')
        def init(p):
            return ' ' * 8 + 'self.%s = %s' % (p.name, p.name)

        paramString = ', '.join(map(param, swiftClass.variables))
        initString = '\n'.join(map(init, swiftClass.variables))
        return '    public init(%s) {\n%s\n    }' % (paramString, initString)


class JSONDecodable():
    @property
    def protocolClass(self):
        return 'JSONDecodable'

    @property
    def protocolEnum(self):
        return None

    def processClass(self, swiftClass):
        def paramString(var):
            an = JSONAnnotation(var)
            if an.jsonOmitValue:
                return []

            ret = [
                'let {name}: {typename}',
                'if let v: AnyObject = data["{jsonlabel}"] {{',
                '    if v is NSNull {{',
                '        ' + ('{name} = {default}' if var.defaultValue else 'throw JSONDecodeError.NonNullablle(key: "{jsonlabel}")'),
                '    }} else {{',
                '        do {{',
            ]

            if var.isArray:
                ret += [
                    '            {name} = try {baseTypename}.%s(v)' % ('parseJSONArrayForNullable' if var.isArrayOfOptional else 'parseJSONArray',),
                    '        }} catch JSONDecodeError.NonNullablle {{',
                    '            throw JSONDecodeError.NonNullablle(key: "{jsonlabel}")',
                ]
            else:
                ret += [
                    '            {name} = try {baseTypename}.parseJSON(v)',
                ]

            ret += [
                '        }} catch JSONDecodeError.ValueTranslationFailed {{',
                '            throw JSONDecodeError.TypeMismatch(key: "{jsonlabel}", type: "{baseTypename}")',
                '        }}',
                '    }}',
                '}} else {{',
                '    ' + ('{name} = {default}' if var.defaultValue else 'throw JSONDecodeError.MissingKey(key: "{jsonlabel}")'),
                '}}',
                ''
            ]

            dic = {
                'jsonlabel'    : an.jsonLabel,
                'name'         : var.name,
                'typename'     : var.typename,
                'baseTypename' : var.baseTypename,
                'default'      : var.defaultValue
            }
            return map(lambda e: (' ' * 4) + e.format(**dic), ret)

        # FIXME: always public
        inits = ', '.join(map(lambda p: '%s: %s' % (p.name, p.name), filter(lambda x: not JSONAnnotation(x).jsonOmitValue, swiftClass.variables)))
        # check whetherr other key-value exists if needed
        lines = [ 'public class func parseJSON(data: AnyObject) throws -> %s {' % (swiftClass.name,) ]
        lines += sum(map(paramString, swiftClass.variables), [])
        lines += [ '    return %s(%s)'  % (swiftClass.name, inits), '}' ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

    def processEnum(self, swiftEnum, rawType):
        assert(rawType != None)
        # FIXME: always public
        lines = [
            'public static func parseJSON(data: AnyObject) throws -> %s {' % (swiftEnum.name),
            '    if let v = data as? %s, val = %s(rawValue: v) {' % (swiftEnum._inheritedTypes[0], swiftEnum.name), # FIXME: private access
            '        return val',
            '    }',
            '    throw JSONDecodeError.ValueTranslationFailed(type: "%s")' % (swiftEnum.name),
            '}'
        ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))


class JSONEncodable():
    @property
    def protocolClass(self):
        return 'JSONEncodable'

    @property
    def protocolEnum(self):
        return None

    def processClass(self, swiftClass):
        def paramString(var):
            an = JSONAnnotation(var)
            if an.jsonOmitValue:
                return []
            elif var.isArray:
                return ['"%s": %s.map { $0.toJSON() },' % (an.jsonLabel, '(' + var.name + ' ?? [])' if var.isOptional else var.name)]
            elif var.isOptional:
                return ['"%s": %s.map { $0.toJSON() } ?? NSNull(),' % (an.jsonLabel, var.name)]
            return ['"%s": %s.toJSON(),' % (an.jsonLabel, var.name)]

        # FIXME: always public
        lines = [
            'public func toJSON() -> [String: AnyObject] {',
            '    return [',
        ]
        lines += sum(map(lambda e: map(lambda x:'        ' + x, paramString(e)), swiftClass.variables), [])
        lines += [
            '    ]',
            '}'
        ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

    def processEnum(self, swiftEnum, rawType):
        assert(rawType != None)
        # FIXME: always public
        lines = [
            'public func toJSON() -> %s {' % (swiftEnum._inheritedTypes[0]), # FIXME: private access
            '    return rawValue',
            '}'
        ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))


class Printable():
    @property
    def protocolClass(self):
        return 'CustomStringConvertible'

    @property
    def protocolEnum(self):
        return 'CustomStringConvertible'

    def processClass(self, swiftClass):
        # FIXME: always public
        params = ', '.join(map(lambda x: '%s=\(%s)' % (x.name, x.name), swiftClass.variables))

        lines = [
            'public var description: String {',
            '    return "%s(%s)"'  % (swiftClass.name, params),
            '}'
        ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

    def processEnum(self, swiftEnum, rawType):
        if rawType != None:
            return '    public var description: String { return rawValue }'
        else:
            def getCaseString(case):
                assocVals = case._assocVals # FIXME
                ais = map(lambda x: '%s=\(v.%s)' % (x[0][0], x[1]) if x[0][0] else '\(v.%d)' % x[1], zip(assocVals, range(len(assocVals))))
                vo = '(' + ', '.join(ais) + ')' if len(ais) > 0 else ''
                return '    case .%s%s: return "%s%s"' % (case._label, '(let v)' if len(ais) > 0 else '', case._label, vo) # FIXME

            lines = [
                'public var description: String {',
                '    switch self {',
            ]
            lines += map(getCaseString, swiftEnum._cases)
            lines += [
                '    }',
                '}'
            ]
            return '\n'.join(map(lambda e: (' ' * 4) + e, lines))


class EnumStaticInit():
    @property
    def protocolClass(self):
        return None

    @property
    def protocolEnum(self):
        return None

    def processClass(self, swiftClass):
        return None

    def processEnum(self, swiftEnum, rawType):
        if rawType != None:
            return None # FIXME
        else:
            def getCaseString(case):
                assocVals = case._assocVals # FIXME
                # FIXME: check optional
                ais = map(lambda x: '%s: %s = %s()' % (x[0][0], x[0][1], x[0][1]) if x[0][0] else 'arg%d: %s = %s()' % (x[1], x[0][1], x[0][1]), zip(assocVals, range(len(assocVals))))
                cis = map(lambda x: '%s: %s' % (x[0][0], x[0][0]) if x[0][0] else 'arg%d' % (x[1],), zip(assocVals, range(len(assocVals))))

                ret = [
                    'public static func make%s(%s) -> %s {' % (case._label, ", ".join(ais), swiftEnum.name),
                    '    return .%s%s' % (case._label, '(' + ', '.join(cis) + ')' if len(cis) > 0 else ''),
                    '}'
                ]
                return ret

            lines = sum(map(getCaseString, swiftEnum._cases), [])
            return '\n'.join(map(lambda e: (' ' * 4) + e, lines))


class URLRequestHelper():
    @property
    def protocolClass(self):
        return None

    @property
    def protocolEnum(self):
        return None

    def processClass(self, swiftClass):
        return None

    def processEnum(self, swiftEnum, rawType):
        if rawType != None:
            return None # FIXME
        else:
            def getPath(case):
                # FIXME: check args
                anon_dic = case._annotations # FIXME: private access
                annons = anon_dic.get('router', [''])
                return annons[1] if len(annons) > 1 else case._label # FIXME: private access

            def getUsedParamNamesForPath(case):
                return re.findall(r'\(([^)]+)\)', getPath(case))

            def getMethodString():
                def getCaseMethodString(case):
                    anon_dic = case._annotations # FIXME: private access
                    annons = anon_dic.get('router', [''])
                    method = (annons[0] if annons[0] != '' else 'GET') if len(annons) >= 1 else 'GET'
                    return '    case .%s: return "%s"' %  (case._label, method)

                lines = [
                    'public var method: String {',
                    '    switch self {',
                ]
                lines += map(getCaseMethodString, swiftEnum._cases)
                lines += [
                    '    }',
                    '}',
                ]
                return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

            def getPathString():
                def getCasePathString(case):
                    path = getPath(case)

                    pathParams = set(getUsedParamNamesForPath(case))
                    caseParams = set([i[0] for i in case._assocVals if i[0] != None])
                    union = pathParams.intersection(caseParams)
                    lets = [i if i in union else '_' for i in caseParams]
                    letString = ('(let (' + ', '.join(lets) + '))') if len(union) > 0 else ''
                    return '    case .%s%s: return "%s"' %  (case._label, letString, path)

                lines = [
                    'public var path: String {',
                    '    switch self {',
                ]
                lines += map(getCasePathString, swiftEnum._cases)
                lines += [
                    '    }',
                    '}',
                ]
                return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

            def getParamsString():
                # FIXME
                def toJsonString(info):
                    nameSym, typeSym = info
                    if typeSym[0] == '[': return nameSym + '.map { $0.toJSON() }'
                    return nameSym + '.toJSON()'

                def getCaseParamsString(case):
                    pathParams = set(getUsedParamNamesForPath(case))
                    caseParams = set([i[0] for i in case._assocVals if i[0] != None])
                    diff = caseParams.difference(pathParams)

                    lets = [i if i in diff else '_' for i in caseParams]
                    letString = ('(let (' + ', '.join(lets) + '))') if len(diff) > 0 else ''

                    dicx = [i for i in case._assocVals if i[0] in diff]
                    dic = ', '.join(['"%s": %s' % (i[0], toJsonString(i)) for i in dicx])
                    dic = '[:]' if len(dic) == 0 else '[' + dic + ']'
                    return '    case .%s%s: return %s' %  (case._label, letString, dic)

                lines = [
                    'public var params: [String: AnyObject] {',
                    '    switch self {',
                ]
                lines += map(getCaseParamsString, swiftEnum._cases)
                lines += [
                    '    }',
                    '}',
                ]
                return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

            return getMethodString() + '\n' + getPathString() + '\n' + getParamsString()


def parseProject(func, parsed_doc):
    tmp = []

    for i in parsed_doc:
        for (filepath, v) in i.items():
            tokens = sourcekitten_syntax(filepath)
            subs = v.get('key.substructure', None)
            if subs:
                tmp = visitSubstructure(func, tokens, subs, tmp)
    return tmp

def visitSubstructure(func, tokens, sublist, initial):
    tmp = initial
    for i in sublist:
        tmp = func(tokens, tmp, i)
    return tmp


# FIXME: Inner or inherit classes are not supported
def getClassOrEnum(tokens, a, n):
    if n.get('key.kind', None) == 'source.lang.swift.decl.class':
        return a + [SwiftClass(tokens, n)]
    elif n.get('key.kind', None) == 'source.lang.swift.decl.enum':
        return a + [SwiftEnum(tokens, n)]
    else:
        return a

# FIXME: check Inner or inherit protocols and report error
def getherIdlProtocol(tokens, a, n):
    if n.get('key.kind', None) == 'source.lang.swift.decl.protocol':
        return a + [SwiftProtocol(tokens, n)]
    else:
        return a

def setIdlProtocolToClass(protocols):
    g = globals()
    for p in protocols:
        c = g.get(p.name, None)
        if c:
            p.setClass(c)



parsed = sourcekitten_doc()
classOrEnums = parseProject(getClassOrEnum, parsed)
protocols = parseProject(getherIdlProtocol, parsed)

###classOrEnums = visitSubstructure(getClassOrEnum, parsed, [])
###protocols = visitSubstructure(getherIdlProtocol, parsed, [])
setIdlProtocolToClass(protocols)


print('// This file was auto-generated by Swift-IDL.\n')
map(lambda e: print(e.getDeclarationString(protocols)), classOrEnums)
