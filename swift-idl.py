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

    def addAdditionalInfo(s):
        with file(filepath) as f:
            contents = f.read()
            map(addContent(contents), s)
            map(addLineNumber(contents), s)
            reduce(addPrevString(contents), s, 0)
            return s

    p = subprocess.Popen([SOURCEKITTEN, 'syntax', '--file', filepath], stdout=subprocess.PIPE)
    (stdoutdata, _) = p.communicate()
    tokens = addAdditionalInfo(json.loads(stdoutdata))
    return tokens


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


def getMacroProtocolByName(protocols, name):
    for p in protocols:
        if p.name == name:
            return p._clazz # FIXME: private
    return None

def getMacroProtocolsByNames(protocols, names):
    ret = []
    for n in names:
        clazz = getMacroProtocolByName(protocols, n)
        if clazz:
            ret.append(clazz())
    return ret

def getDefaultMacroProtocol(protocols, name = 'Default'):
    for p in protocols:
        if p.name == name:
            return getMacroProtocolsByNames(protocols, p._inheritedTypes) # FIXME: private
    return []


class SwiftClass():
    def __init__(self, node):
        self._filepath   = node['key.filepath']
        self._bodyOffset = node['key.bodyoffset']
        self._bodyLength = node['key.bodylength']
        self._name = node['key.name']
        self._parsedDeclaration = node['key.parsed_declaration']

        def getVariables(a, n):
            if n.get('key.kind', None) == 'source.lang.swift.decl.var.instance':
                return a + [SwiftVariable(n)]
            return a

        self._variables = reduce(getVariables, node['key.substructure'], [])
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

    def getDeclarationString(self, protocols):
        ps = getMacroProtocolsByNames(protocols, self._inheritedTypes)
        if len(ps) == 0:
            ps = getDefaultMacroProtocol(protocols, 'ClassDefault')

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
    def __init__(self, node):
        self._name              = node['key.name']
        self._typename          = node['key.typename']
        self._parsedDeclaration = node['key.parsed_declaration']
        self._defaultValue      = None

        self._annotations = {}
        self._parsedTypename = parseTypename(self._typename)
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
        return s[0].strip()


class SwiftEnum():
    def __init__(self, node):
        def getCasesForRawType(filepath, offset, length):
            tokens = sourcekitten_syntax(filepath) # FIXME: multiple loads
            cases = []
            for i in range(len(tokens)):
                t = tokens[i]
                if t['content'] == 'case' and t['type'] == 'source.lang.swift.syntaxtype.keyword' and offset <= t['offset'] < offset + length and i + 2 < len(tokens):
                    key = tokens[i+1]
                    val = tokens[i+2]
                    a = (key['content'], val['content'])
                    cases.append(a)
            return cases

        def getCasesForAssociatedValues(filepath, offset, length):
            tokens = sourcekitten_syntax(filepath) # FIXME: multiple loads
            cases = []
            i = 0
            while i < len(tokens):
                t = tokens[i]
                i += 1
                if t['content'] == 'case' and t['type'] == 'source.lang.swift.syntaxtype.keyword':
                    t = tokens[i]
                    caseLabel = None
                    assocVals = []
                    while i < len(tokens):
                        t = tokens[i]
                        if t['type'] == 'source.lang.swift.syntaxtype.keyword':
                            break
                        i += 1
                        tk = t['content']
                        ps = t['prevString']
                        isType = ps.find(':') >= 0
                        if caseLabel == None:
                            caseLabel = tk
                        elif isType:
                            tp = assocVals[-1]
                            assocVals[-1] = (tp[1], tk)
                        else:
                            tp = (None, tk)
                            assocVals.append(tp)

                    cases.append((caseLabel, assocVals))
            return cases

        self._filepath   = node['key.filepath']
        self._bodyOffset = node['key.bodyoffset']
        self._bodyLength = node['key.bodylength']

        self._name              = node['key.name']
        self._parsedDeclaration = node['key.parsed_declaration']
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

        if self.isRawType:
            self._cases = getCasesForRawType(self._filepath, self._bodyOffset, self._bodyLength)
        else:
            self._cases = getCasesForAssociatedValues(self._filepath, self._bodyOffset, self._bodyLength)

        with file(self._filepath) as f:
            self._contents = f.read()[self._bodyOffset : self._bodyOffset + self._bodyLength]

    def getDeclarationString(self, protocols):
        ps = getMacroProtocolsByNames(protocols, self._inheritedTypes)
        if len(ps) == 0:
            ps = getDefaultMacroProtocol(protocols, 'EnumDefault')

        rawType = self.getRawType(protocols)

        typeInheritances = ([rawType] if rawType != None else []) + [i.protocolEnum for i in ps if i.protocolEnum != None]
        processedProtocols = filter(lambda e: e != None, map(lambda e: e.processEnum(self, rawType), ps))

        ret = 'public enum ' + self._name
        if len(typeInheritances) > 0:
            ret += ': ' + ', '.join(typeInheritances)
        ret += ' {'
        ret += self._contents
        if len(processedProtocols) > 0:
            ret += '\n'
            ret += '\n\n'.join(processedProtocols)
            ret += '\n'
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
            #return None if getMacroProtocolByName(protocols, n) else n
            return n if self.isRawType else None


class SwiftProtocol():
    def __init__(self, node):
        self._name              = node['key.name']
        self._parsedDeclaration = node['key.parsed_declaration']
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
                '    if let _ = v as? NSNull {{',
                '        ' + ('{name} = {default}' if var.defaultValue else 'return (nil, "Null not allowed in \'{jsonlabel}\'")'),
            ]

            if var.isArray:
                ret += [
                    '    }} else if let array = v as? [AnyObject] {{',
                    '        var r: [{baseTypename}] = []',
                    '        r.reserveCapacity(count(array))',
                    '        for e in array {{',
                    '            if let _ = e as? NSNull {{',
                    '                ' + ('r.append(nil)' if var.isArrayOfOptional else 'return (nil, "Null not allowed in \'{jsonlabel}\'")'),
                    '            }}',
                    '',
                    '            let (casted, err) = {baseTypename}.parseJSON(e)',
                    '            if let c = casted {{',
                    '                r.append(c)',
                    '            }} else {{',
                    '                return (nil, err ?? "Type transformation failed in \'{jsonlabel}\'")',
                    '            }}',
                    '        }}',
                    '        {name} = r',
                    '    }} else {{',
                    '        return (nil, "Type transformation failed in \'{jsonlabel}\'")',
                ]
            else:
                ret += [
                    '    }} else {{',
                    '        let (casted, err) = {baseTypename}.parseJSON(v)',
                    '        if let c = casted {{',
                    '            {name} = c',
                    '        }} else {{',
                    '            return (nil, err ?? "Type transformation failed in \'{jsonlabel}\'")',
                    '        }}',
                ]

            ret += [
                '    }}',
                '}} else {{',
                '    ' + ('{name} = {default}' if var.defaultValue else 'return (nil, "Keyword not found: \'{jsonlabel}\'")'),
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
        lines = [ 'public class func parseJSON(data: AnyObject) -> (decoded: %s?, error: String?) {' % (swiftClass.name,) ]
        lines += sum(map(paramString, swiftClass.variables), [])
        lines += [ '    return (%s(%s), nil)'  % (swiftClass.name, inits), '}' ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

    def processEnum(self, swiftEnum, rawType):
        assert(rawType != None)
        # FIXME: always public
        lines = [
            'public static func parseJSON(data: AnyObject) -> (decoded: %s?, error: String?) {' % (swiftEnum.name),
            '    if let v = data as? %s {' % (swiftEnum._inheritedTypes[0]), # FIXME: private access
            '        return (%s(rawValue: v), nil)' % (swiftEnum.name),
            '    }',
            '    return (nil, "Type transformation failed in %s")' % (swiftEnum.name),
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
                return ['"%s": map(%s%s) { $0.toJSON() },' % (an.jsonLabel, var.name, ' ?? []' if var.isOptional else '')]
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
        return 'Printable'

    @property
    def protocolEnum(self):
        return 'Printable'

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
            def getCaseString(e):
                caseLabel, assocVals = e
                ais = map(lambda x: '%s=\(v.%s)' % (x[0][0], x[1]) if x[0][0] else '\(v.%d)' % x[1], zip(assocVals, range(len(assocVals))))
                vo = ", ".join(ais)
                return '    case .%s(let v): return "%s(%s)"' % (caseLabel, caseLabel, vo)

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
            def getCaseString(e):
                caseLabel, assocVals = e
                # FIXME: check optional
                ais = map(lambda x: '%s: %s = %s()' % (x[0][0], x[0][1], x[0][1]) if x[0][0] else 'arg%d: %s = %s()' % (x[1], x[0][1], x[0][1]), zip(assocVals, range(len(assocVals))))
                cis = map(lambda x: '%s: %s' % (x[0][0], x[0][0]) if x[0][0] else 'arg%d' % (x[1],), zip(assocVals, range(len(assocVals))))

                ret = [
                    'public static func make%s(%s) -> %s {' % (caseLabel, ", ".join(ais), swiftEnum.name),
                    '    return .%s(%s)' % (caseLabel, ", ".join(cis)),
                    '}'
                ]
                return ret

            lines = sum(map(getCaseString, swiftEnum._cases), [])
            return '\n'.join(map(lambda e: (' ' * 4) + e, lines))


def visitSubstructure(func, sublist, initial, level = 0):
    tmp = initial

    if level == 0: # Array
        for i in sublist:
            tmp = visitSubstructure(func, i, tmp, level + 1)
    elif level == 1: # Dict(File:Dict)
        for i in sublist:
            subs = sublist[i].get('key.substructure', None)
            if subs:
                tmp = visitSubstructure(func, subs, tmp, level + 1)
    else:
        for i in sublist:
            tmp = func(tmp, i)
            subs = i.get('key.substructure', None)
            if subs:
                tmp = visitSubstructure(func, subs, tmp, level + 1)
    return tmp


# FIXME: Inner or inherit classes are not supported
def getClassOrEnum(a, n):
    if n.get('key.kind', None) == 'source.lang.swift.decl.class':
        return a + [SwiftClass(n)]
    elif n.get('key.kind', None) == 'source.lang.swift.decl.enum':
        return a + [SwiftEnum(n)]
    else:
        return a

# FIXME: check Inner or inherit protocols and report error
def getherMacroProtocol(a, n):
    if n.get('key.kind', None) == 'source.lang.swift.decl.protocol':
        return a + [SwiftProtocol(n)]
    else:
        return a

def setMacroProtocolToClass(protocols):
    g = globals()
    for p in protocols:
        c = g.get(p.name, None)
        if c:
            p.setClass(c)



parsed = sourcekitten_doc()
classOrEnums = visitSubstructure(getClassOrEnum, parsed, [])
protocols = visitSubstructure(getherMacroProtocol, parsed, [])
setMacroProtocolToClass(protocols)


print('// This file was auto-generated by Swift-IDL.\n')
map(lambda e: print(e.getDeclarationString(protocols)), classOrEnums)
