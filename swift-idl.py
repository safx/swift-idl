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
    def getToken(f):
        contents = f.read()
        def func(c):
            offset = c['offset']
            length = c['length']
            content = contents[offset : offset + length]
            c['content'] = content
            return c
        return func

    def addToken(s):
        with file(filepath) as f:
            return map(getToken(f), s)

    p = subprocess.Popen([SOURCEKITTEN, 'syntax', '--file', filepath], stdout=subprocess.PIPE)
    (stdoutdata, _) = p.communicate()
    tokens = addToken(json.loads(stdoutdata))
    return tokens



class SwiftClass():
    def __init__(self, node):
        self.filepath   = node['key.filepath']
        self.bodyOffset = node['key.bodyoffset']
        self.bodyLength = node['key.bodylength']
        self._name = node['key.name']
        self._parsedDeclaration = node['key.parsed_declaration']

        def getVariables(a, n):
            if n.get('key.kind', None) == 'source.lang.swift.decl.var.instance':
                return a + [SwiftVariable(n)]
            return a

        self._variables = reduce(getVariables, node['key.substructure'], [])
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

    def _getClassDeclarationString(self):
        ps = [DefaultInit(), JSONDecodable(), JSONEncodable(), Printable()]

        inherited = ', '.join(self._inheritedTypes + [i.protocolClass for i in ps if i.protocolClass != None])
        variables = '\n'.join(map(lambda x: '    ' + x._parsedDeclarationWithoutDefaultValue, self._variables))

        # FIXME: always public
        ret = 'public class %s : %s {\n%s\n\n' % (self._name, inherited, variables)
        ret += '\n\n'.join(map(lambda e: e.processClass(self), ps))
        ret += '\n}\n'
        return ret

    def __repr__(self):
        return self._getClassDeclarationString()


class SwiftVariable():
    def __init__(self, node):
        self._name              = node['key.name']
        self._typename          = node['key.typename']
        self._parsedDeclaration = node['key.parsed_declaration']

        self._defaultValue      = None
        self._jsonOmitValue     = False
        self._jsonLabel         = self._name

        self._parseDeclaration()
        self._parsedTypename = parseTypename(self._typename)

        #print(self._name, self._defaultValue, self._jsonOmitValue, self.isOptional, self._parsedTypename)

    def _parseDeclaration(self):
        def parseAnnotation(comment_part):
            m = re.match('json:"([^"]+)"', comment_part.strip())
            if not m: return

            annons = m.group(1).split(',')
            if annons[0] != '':
                if annons[0] == '-':
                    self._jsonOmitValue = True
                else:
                    self._jsonLabel = annons[0]

            for i in annons[1:]:
                if i == 'omitempty':
                    pass
                elif i == 'string':
                    pass
                else:
                    raise RuntimeError('Unknown annotation: ' + i)

        def parseDefaultValue(decl_part):
            ts = decl_part.split('=')
            if len(ts) == 2:
                self._defaultValue = ts[1].strip()

            if not self._defaultValue and self.isOptional:
                self._defaultValue = 'nil'

        cs = self._parsedDeclaration.split('//')
        if len(cs) >= 2:
            # the first comment area is only checked
            parseAnnotation(cs[1])

        parseDefaultValue(cs[0])

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
    def _parsedDeclarationWithoutDefaultValue(self):
        # FIXME: remove default value only
        s = self._parsedDeclaration.split('=')
        return s[0].strip()

    @property
    def _descriptionString(self):
        return '%s=\(%s)' % (self._name, self._name)


class SwiftEnum():
    def __init__(self, node):
        def getCases(filepath, offset, length):
            tokens = sourcekitten_syntax(filepath)
            cases = []
            for i in range(len(tokens)):
                t = tokens[i]
                if t['content'] == 'case' and t['type'] == 'source.lang.swift.syntaxtype.keyword' and offset <= t['offset'] < offset + length  and i + 2 < len(tokens):
                    key = tokens[i+1]
                    val = tokens[i+2]
                    a = (key['content'], val['content'])
                    cases.append(a)
            return cases

        self.filepath   = node['key.filepath']
        self.bodyOffset = node['key.bodyoffset']
        self.bodyLength = node['key.bodylength']

        self._name              = node['key.name']
        self._parsedDeclaration = node['key.parsed_declaration']
        self._cases = getCases(self.filepath, self.bodyOffset, self.bodyLength)
        self._inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))
        if len(self._inheritedTypes) == 0:
            raise RuntimeError('Enum must have a row-type: ' + self._name)

        with file(self.filepath) as f:
            self._contents = f.read()[self.bodyOffset : self.bodyOffset + self.bodyLength]

    def _getClassDeclarationString(self):
        ps = [JSONDecodable(), JSONEncodable()]

        pss = [i.protocolEnum for i in ps if i.protocolEnum != None]
        inherited = ', '.join(pss)
        if len(pss) > 0:
            inherited = ', ' + inherited

        ret = self._parsedDeclaration + inherited + ' {' + self._contents + '\n'
        ret += '\n\n'.join(map(lambda e: e.processEnum(self), ps))
        ret += '\n}\n'
        return ret

    def __repr__(self):
        return self._getClassDeclarationString()


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


class DefaultInit():
    @property
    def protocolClass(self):
        return None

    def processClass(self, swiftClass):
        def param(p):
            return p._name + ': ' + p._typename + (' = ' + p._defaultValue if p._defaultValue != None else '')
        def init(p):
            return ' ' * 8 + 'self.%s = %s' % (p._name, p._name)

        paramString = ', '.join(map(param, swiftClass._variables))
        initString = '\n'.join(map(init, swiftClass._variables))
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
            #FIXME Array, User-type
            if var._jsonOmitValue:
                return []

            ret = [
                'let {name}: {typename}',
                'if let v: AnyObject = data["{jsonlabel}"] {{',
                '    if let _ = v as? NSNull {{',
                '        ' + ('{name} = {default}' if var._defaultValue else 'return (nil, "Null not allowed in \'{jsonlabel}\'")'),
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
                '    ' + ('{name} = {default}' if var._defaultValue else 'return (nil, "Keyword not found: \'{jsonlabel}\'")'),
                '}}',
                ''
            ]

            dic = {
                'jsonlabel'    : var._jsonLabel,
                'name'         : var._name,
                'typename'     : var._typename,
                'baseTypename' : var.baseTypename,
                'default'      : var._defaultValue
            }
            return map(lambda e: (' ' * 4) + e.format(**dic), ret)

        # FIXME: always public
        inits = ', '.join(map(lambda p: '%s: %s' % (p._name, p._name), filter(lambda x: not x._jsonOmitValue, swiftClass._variables)))
        # check whetherr other key-value exists if needed
        lines = [ 'public class func parseJSON(data: AnyObject) -> (decoded: %s?, error: String?) {' % (swiftClass._name,) ]
        lines += sum(map(paramString, swiftClass._variables), [])
        lines += [ '    return (%s(%s), nil)'  % (swiftClass._name, inits), '}' ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

    def processEnum(self, swiftEnum):
        # FIXME: always public
        lines = [
            'public static func parseJSON(data: AnyObject) -> (decoded: %s?, error: String?) {' % (swiftEnum._name),
            '    if let v = data as? %s {' % (swiftEnum._inheritedTypes[0]),
            '        return (%s(rawValue: v), nil)' % (swiftEnum._name),
            '    }',
            '    return (nil, "Type transformation failed in %s")' % (swiftEnum._name),
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
            if var._jsonOmitValue:
                return []
            elif var.isArray:
                return ['"%s": map(%s%s) { $0.toJSON() },' % (var._jsonLabel, var._name, ' ?? []' if var.isOptional else '')]
            elif var.isOptional:
                return ['"%s": %s.map { $0.toJSON() } ?? NSNull(),' % (var._jsonLabel, var._name)]
            return ['"%s": %s.toJSON(),' % (var._jsonLabel, var._name)]

        # FIXME: always public
        lines = [
            'public func toJSON() -> [String: AnyObject] {',
            '    return [',
        ]
        lines += sum(map(lambda e: map(lambda x:'        ' + x, paramString(e)), swiftClass._variables), [])
        lines += [
            '    ]',
            '}'
        ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

    def processEnum(self, swiftEnum):
        # FIXME: always public
        lines = [
            'public func toJSON() -> %s {' % (swiftEnum._inheritedTypes[0]),
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
        params = ', '.join(map(lambda x: x._descriptionString, swiftClass._variables))
        lines = [
            'public var description: String {',
            '    return "%s(%s)"'  % (swiftClass._name, params),
            '}'
        ]
        return '\n'.join(map(lambda e: (' ' * 4) + e, lines))



def getAliasesFromFiles(fileset):
    # FIXME: We treat as public and in global scope for all typealias because we don't check any accessibility.
    def getAliasesFromTokens(tokens):
        aliases = []
        for i in range(len(tokens)):
            t = tokens[i]
            if t['content'] == 'typealias' and t['type'] == 'source.lang.swift.syntaxtype.keyword' and i + 2 < len(tokens):
                key = tokens[i+1]
                val = tokens[i+2]
                a = (key['content'], val['content'])
                aliases.append(a)
        return aliases

    return sum([getAliasesFromTokens(sourcekitten_syntax(i)) for i in fileset], [])


def visitSubstructure(func, sublist, initial):
    tmp = initial
    for i in sublist:
        tmp = func(tmp, i)

        subs = i.get('key.substructure', None)
        if subs:
            tmp = visitSubstructure(func, subs, tmp)
    return tmp


def getFilepath(a, n):
    p = n.get('key.filepath', None)
    return a + [p] if p else a


# FIXME: Inner or inherit classes are not supported
def getClassOrEnum(a, n):
    if n.get('key.kind', None) == 'source.lang.swift.decl.class':
        return a + [SwiftClass(n)]
    elif n.get('key.kind', None) == 'source.lang.swift.decl.enum':
        return a + [SwiftEnum(n)]
    else:
        return a


parsed = sourcekitten_doc()

#files   = set(visitSubstructure(getFilepath, parsed, []))
#aliases = getAliasesFromFiles(files)
#print(aliases)

info = visitSubstructure(getClassOrEnum, parsed, [])

print('// This file was auto-generated by Swift-IDL.\n')
map(print, info)
