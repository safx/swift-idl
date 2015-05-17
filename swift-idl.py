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
#   let fooBoo: Int     // json:",omitempty"
#   let fooBee: Int     // json:",string"  (not supported currentry)
#   case Unknown        // json:"-"        (not supported currentry)


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
        def getDefaultInitString():
            params = ', '.join(map(lambda x: x._defaultInitParamString, self._variables))
            inits = '\n'.join(map(lambda x: x._defaultInitAssignString, self._variables))
            return '    public init(%s) {\n%s\n    }' % (params, inits)

        def getJsonParseExtensionString():
            # FIXME: always public
            inits = ', '.join(map(lambda x: x._defaultInitArgumentString, filter(lambda x: not x._jsonOmitValue, self._variables)))
            # check whetherr other key-value exists if needed
            lines = [ 'public class func parseJSON(data: [String: AnyObject]) -> (decoded: %s?, error: String?) {' % (self._name,) ] + sum(map(lambda x: x._jsonParseString, self._variables), []) + [ '    return (%s(%s), nil)'  % (self._name, inits), '}' ]
            return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

        def getDescriptionExtensionString():
            # FIXME: always public
            params = ', '.join(map(lambda x: x._descriptionString, self._variables))
            lines = [
                'public var description: String {',
                '    return "%s(%s)"'  % (self._name, params),
                '}'
            ]
            return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

        inherited = ', '.join(self._inheritedTypes + ['JSONDecodable', 'Printable']) # FIXME: omit if contains
        variables = '\n'.join(map(lambda x: '    %s' % (x,), self._variables))
        # FIXME: always public
        return 'public class %s : %s {\n%s\n\n%s\n\n%s\n\n%s\n}\n' % (self._name, inherited, variables, getDefaultInitString(), getJsonParseExtensionString(), getDescriptionExtensionString())

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
    def _defaultInitParamString(self):
        p = '%s: %s' % (self._name, self._typename)
        return p + (' = %s' % (self._defaultValue,) if self._defaultValue else '')

    @property
    def _defaultInitAssignString(self):
        return ' ' * 8 + 'self.%s = %s' % (self._name, self._name)

    @property
    def _defaultInitArgumentString(self):
        return '%s: %s' % (self._name, self._name)

    @property
    def _parsedDeclarationWithoutDefaultValue(self):
        # FIXME: remove default value only
        s = self._parsedDeclaration.split('=')
        return s[0].strip()

    @property
    def _descriptionString(self):
        return '%s=\(%s)' % (self._name, self._name)

    @property
    def _jsonParseString(self):
        #FIXME Array, User-type
        if self._jsonOmitValue:
            return []

        ret = [
            'let {name}: {typename}',
            'if let v: AnyObject = data["{jsonlabel}"] {{',
            '    if let _ = v as? NSNull {{',
            '        ' + ('{name} = {default}' if self._defaultValue else 'return (nil, "Null not allowed in \'{jsonlabel}\'")'),
        ]

        if self.isArray:
            ret += [
                '    }} else if let a = v as? [AnyObject] {{',
                '        var r: {typename} = []',
                '        r.reserveCapacity(count(a))',
                '        for elem in a {{',
                '            if let e = elem as? [String : AnyObject] {{',
                '                let (casted, err) = {baseTypename}.parseJSON(e)',
                '                if let c = casted {{',
                '                    r.append(c)',
                '                }} else {{',
                '                    return (nil, err ?? "Type transformation failed in \'{jsonlabel}\'")',
                '                }}',
                '            }} else {{',
                '                return (nil, "Object expected in \'{jsonlabel}\'")',
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
            '    ' + ('{name} = {default}' if self._defaultValue else 'return (nil, "Keyword not found: \'{jsonlabel}\'")'),
            '}}',
            ''
        ]

        dic = {
            'jsonlabel'    : self._jsonLabel,
            'name'         : self._name,
            'typename'     : self._typename,
            'baseTypename' : self.baseTypename,
            'default'      : self._defaultValue
        }
        return map(lambda e: (' ' * 4) + e.format(**dic), ret)

    def __repr__(self):
        return self._parsedDeclarationWithoutDefaultValue


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
        def getJsonParseExtensionString():
            # FIXME: always public
            lines = [
                'public static func parseJSON(data: AnyObject) -> (decoded: %s?, error: String?) {' % (self._name),
                '    if let v = data as? %s {' % (self._inheritedTypes[0]),
                '        return (%s(rawValue: v), nil)' % (self._name),
                '    }',
                '    return (nil, "Type transformation failed in %s")' % (self._name),
                '}'
            ]
            return '\n'.join(map(lambda e: (' ' * 4) + e, lines))

        return self._parsedDeclaration + ' {' + self._contents + '\n' + getJsonParseExtensionString() + '\n}\n'

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
        return self._targetClass

class SwiftOptional():
    def __init__(self, targetClass):
        self._targetClass = targetClass

    def __repr__(self):
        return str(self._targetClass) + '?'

    @property
    def baseTypename(self):
        return self._targetClass


def parseTypename(typename):
    if typename[-1] == '?':
        return SwiftOptional(parseTypename(typename[:-1]))
    elif typename[0] == '[' and typename[-1] == ']':
        return SwiftArray(parseTypename(typename[1:-1]))
    else:
        return SwiftTypename(typename)



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
