#!/usr/bin/python

from __future__ import print_function

import argparse
import functools
import itertools
import json
import os
import re
import subprocess
from mako.template import Template
from mako import exceptions

PROGRAM_NAME='swift-idl'
SOURCEKITTEN='sourceKitten'


### Tokens

class SwiftToken():
    def __init__(self, line, offset, tokenType, content):
        self.line      = line
        self.offset    = offset
        self.tokenType = tokenType
        self.content   = content

    def __repr__(self):
        return self.content + ' ' + self.tokenType

    @property
    def isComment(self):
        return self.tokenType == 'source.lang.swift.syntaxtype.comment'

    @property
    def annotations(self):
        cs = self.content.split('//')
        if len(cs) < 2: return {}
        # the first comment area is only checked
        return {g.group(1).lower():map(str.strip, g.group(2).split(','))
                for g in re.finditer(r'\b(\w+):"([^"]+)"', cs[1])
                if g.lastindex == 2}

def getSwiftTokens(tokens, source):
    linenum_map = map(lambda x: 1 if x == '\n' else 0, source) # FIXME: check CRLF or LF
    def getLinenumber(offset):
        return sum(linenum_map[:offset], 1)

    def getOmittedTokens(tokens, source):
        begins = [0] + [t['offset'] + t['length'] for t in tokens]
        ends   = [t['offset'] for t in tokens] + [len(source)]
        return [SwiftToken(getLinenumber(b), b, "omittedtoken", source[b:e])
                for b,e in zip(begins, ends)
                if b != e]

    def conv(tk):
        offset = tk['offset']
        length = tk['length']
        tktype = tk['type']
        return SwiftToken(getLinenumber(offset), offset, tktype, source[offset : offset + length])

    merged = map(conv, tokens) + getOmittedTokens(tokens, source)
    return sorted(merged, key=lambda e: e.offset)

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

class SwiftTypealias():
    def __init__(self, name, assignment):
        self._name       = name
        self._assignment = assignment

    @property
    def name(self): return self._name

    @property
    def assignment(self): return self._assignment

class SwiftTypename():
    def __init__(self, typename):
        self._typename = typename

    def __repr__(self):
        return self._typename

    @property
    def baseTypename(self): return self._typename

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

    def __repr__(self): return str(self._targetClass) + '?'

    @property
    def baseTypename(self): return self._targetClass.baseTypename

def parseTypename(typename):
    if typename[-1] == '?':
        return SwiftOptional(parseTypename(typename[:-1]))
    elif typename[0] == '[' and typename[-1] == ']':
        return SwiftArray(parseTypename(typename[1:-1]))
    else:
        return SwiftTypename(typename)

### Swift structures base

class SwiftVariableBase(object):
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

# variable of tuple in case
class SwiftTupleVariable(SwiftVariableBase):
    def __init__(self, name, typename, position):
        super(SwiftTupleVariable, self).__init__(name, typename)
        self._positon = position

    @property
    def positon(self): return self._positon

    @property
    def varname(self):
        return self._name if self._name else 'v' + str(self._positon)

    @property
    def keyname(self):
        return self._name if self._name else str(self._positon)

    @property
    def declaredString(self):
        if self._name:
            return self._name + ': ' + self.typename
        return self.typename

# variable in struct or class
class SwiftVariable(SwiftVariableBase):
    def __init__(self, name, typename, defaultValue, accessibility, annotations, parsedDeclaration):
        super(SwiftVariable, self).__init__(name, typename)
        self._defaultValue  = defaultValue
        self._accessibility = accessibility
        self._annotations   = annotations
        self._parsedDeclaration = parsedDeclaration

    @property
    def accessibility(self): return self._accessibility

    @property
    def annotations(self): return self._annotations

    @property
    def hasDefaultValue(self):
        return self.isOptional or self._defaultValue != None

    @property
    def defaultValue(self):
        if self._defaultValue:
            return self._defaultValue
        elif self.isOptional:
            return 'nil'
        return self.typename + '()'

    @property
    def parsedDeclarationWithoutDefaultValue(self):
        # FIXME: use original code: self._parsedDeclaration
        anon = ' '.join([k + ':"' + ','.join(v) + '"' for k,v in self._annotations.iteritems()]).strip()
        if len(anon) > 0: anon = ' // ' + anon
        return 'public let ' + self._name + ': ' + self._typename + anon

    def annotation(self, name):
        return getAnnotationMap()[name](self)

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

    def annotation(self, name):
        return getAnnotationMap()[name](self)

class SwiftCase(SwiftVariableList):
    def __init__(self, name, variables, annotations, rawValue):
        super(SwiftCase, self).__init__(name, variables, annotations)
        self._rawValue = rawValue

    @property
    def letString(self):
        if len(self.variables) == 0: return ''
        return '(let (' + ', '.join([i.varname for i in self.variables]) + '))'

    @property
    def declaredString(self):
        if self._rawValue != None:
            return str(self._name) + ' = ' + str(self._rawValue)
        else:
            p = ''
            if len(self._variables) > 0:
                p = '(' + ', '.join([e.declaredString for e in self._variables]) + ')'
            return str(self._name) + p


def getDeclarationString(clazzOrEnum, clazzOrEnumString, templateHeader, classes, protocols, isRootLevel):
    template = Template(templateHeader + '''
% for i in innerDecls:
//
${i}
% endfor

% for s in subDecls:
//
${s}
% endfor
}
% for i in outerDecls:
//
${i}
//
% endfor
''')
    def getattrif(me, name, defaultValue = None):
        if hasattr(me, name):
            return getattr(me, name)
        return defaultValue

    def callattrif(me, name, *p):
        if hasattr(me, name):
            return getattr(me, name)(*p)
        return None

    assert(clazzOrEnumString == 'Class' or clazzOrEnumString == 'Enum')

    ps = getIdlProtocols(protocols, clazzOrEnum.inheritedTypes, clazzOrEnumString + 'Default')
    for p in ps:
        callattrif(p, 'modify' + clazzOrEnumString, clazzOrEnum)

    subTemplateParams = {
        'classes': classes,
        'clazz' if clazzOrEnumString == 'Class' else 'enum': clazzOrEnum
    }

    isRawStyle = getattrif(clazzOrEnum, 'isRawStyle', False) # FIXME

    templates = [callattrif(p, clazzOrEnumString.lower() + 'Templates', isRawStyle) for p in ps]
    tupledTemplates = [e if type(e) == tuple else (e, None) for e in templates if e]
    innerTemplates, outerTemplates = zip(*tupledTemplates) if len(tupledTemplates) > 0 else ([], []) # unzip

    try:
        innerDecls = [indent(Template(e).render(**subTemplateParams)) for e in innerTemplates]
        outerDecls = [Template(e).render(**subTemplateParams) for e in outerTemplates if e]
    except:
        print(exceptions.html_error_template().render(css=False, full=False))
        exit(1)

    typeInheritances = sum([getattrif(p, 'protocol' + clazzOrEnumString, []) for p in ps], getNonIdlProtocols(protocols, clazzOrEnum.inheritedTypes))
    templateParams = {
        'clazz' if clazzOrEnumString == 'Class' else 'enum': clazzOrEnum,
        'innerDecls': innerDecls,
        'outerDecls': outerDecls,
        'inheritances': ': ' + ', '.join(typeInheritances) if len(typeInheritances) > 0 else '',
        'subDecls': map(lambda e: e.getDeclarationString(classes, protocols, False), clazzOrEnum.substructure)
    }
    output = template.render(**templateParams)
    return indent(output, isRootLevel)

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
    def inheritedTypes(self): return self._inheritedTypes

    @property
    def annotations(self): return self._annotations

    @property
    def substructure(self): return self._substructure

    @property
    def isEnum(self): return True

    @property
    def isRawStyle(self):
        if len(self._inheritedTypes) > 0:
            n = self._inheritedTypes[0]
            return n in ['String', 'Int', 'Float', 'Character'] # FIXME: naive guess
        return False

    #@property
    #def rawType(self):
    #    return self.isRawStyle if self._inheritedTypes[0] else None

    def annotation(self, name):
        return getAnnotationMap()[name](self)

    def getDeclarationString(self, classes, protocols, isRootLevel = True):
        header = '''
public enum ${enum.name}${inheritances} {
    % for c in enum.cases:
    case ${c.declaredString}
    % endfor
'''
        return getDeclarationString(self, 'Enum', header, classes, protocols, isRootLevel)


class SwiftClass(SwiftVariableList):
    def __init__(self, name, decltype, variables, inheritedTypes, typealiases, annotations, substructure):
        super(SwiftClass, self).__init__(name, variables, annotations)
        self._decltype       = decltype
        self._inheritedTypes = inheritedTypes
        self._typealiases    = typealiases
        self._substructure   = substructure

    @property
    def name(self): return self._name

    @property
    def inheritedTypes(self): return self._inheritedTypes

    @property
    def decltype(self): return self._decltype

    @property
    def typealiases(self): return self._typealiases

    @property
    def substructure(self): return self._substructure

    @property
    def isEnum(self): return False

    @property
    def static(self): return 'static' if self._decltype == 'struct' else 'class'

    def getDeclarationString(self, classes, protocols, isRootLevel = True):
        header = '''
public ${clazz.decltype} ${clazz.name}${inheritances} {
    % for a in clazz.typealiases:
    public typealias ${a.name} = ${a.assignment}
    % endfor
    % for v in clazz.variables:
    ${v.parsedDeclarationWithoutDefaultValue}
    % endfor
'''
        return getDeclarationString(self, 'Class', header, classes, protocols, isRootLevel)


### Parsing functions

def visitProtocol(node):
    name = node['key.name']
    clazz = globals()[name]
    inheritedtypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))
    return SwiftProtocol(name, inheritedtypes, clazz)

def visitClass(node, tokens):
    # `sourcekitten doc` doesn't return `typealias` information. So we have to process.
    #   * You must to declare typealias at the beginning of body in class.
    def getTypealiases():
        aliases = []
        def addTypealias(begin, end):
            if begin != None:
                aliases.append(visitTypealias([tokens[x] for x in range(begin, end)]))

        tkrange = tokenrange(tokens, node['key.bodyoffset'], node['key.bodylength'])
        begin = None
        for i in tkrange:
            t = tokens[i]
            if t.tokenType == 'source.lang.swift.syntaxtype.attribute.builtin':
                addTypealias(begin, i)
                begin = None
            if t.tokenType == 'source.lang.swift.syntaxtype.keyword':
                if t.content == 'typealias':
                    addTypealias(begin, i)
                    begin = i
                else:
                    addTypealias(begin, i)
                    break
            elif i == tkrange[-1]:
                addTypealias(begin, i + 1)

        return aliases

    def getVariables(a, n):
        if n.get('key.kind', None) == 'source.lang.swift.decl.var.instance':
            return a + [visitVariable(n, tokens)]
        return a

    name = node['key.name']
    decltype = 'struct' if node['key.kind'] == 'source.lang.swift.decl.struct' else 'class'

    variables = reduce(getVariables, node.get('key.substructure', []), [])
    inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

    annotations = getAnnotations([tokens[i] for i in tokenrange(tokens, node['key.bodyoffset'], node['key.bodylength'])])
    typealiases = getTypealiases()

    subs = node.get('key.substructure', [])
    innerDecls = visitSubstructure(getDeclarations, tokens, subs, [])

    return SwiftClass(name, decltype, variables, inheritedTypes, typealiases, annotations, innerDecls)

def visitEnum(node, tokens):
    name = node['key.name']
    inheritedTypes = map(lambda e: e['key.name'], node.get('key.inheritedtypes', []))

    subs = node.get('key.substructure', [])
    pred = lambda e: e['key.kind'] == 'source.lang.swift.decl.enumcase'
    cases = map(functools.partial(visitCase, tokens), filter(pred, subs))
    innerDecls = visitSubstructure(getDeclarations, tokens, itertools.ifilterfalse(pred, subs), [])

    annotations = {} # FIXME
    return SwiftEnum(name, cases, inheritedTypes, annotations, innerDecls)

def visitVariable(node, tokens):
    offset = node['key.offset']
    length = node['key.length']

    def getContent(t):
        if t.tokenType == 'omittedtoken':
            return t.content.split('\n')[0].split('}')[0]
        return t.content

    def getDefaultValue(var_tokens):
        eqs = [e[0] for e in enumerate(var_tokens) if e[1].tokenType == 'omittedtoken' and e[1].content.find('=') >= 0]
        assert(len(eqs) <= 1)
        if len(eqs) == 1:
            p = eqs[0]
            before = var_tokens[p].content.split('=')[1].split('\n')[0].split('}')[0].strip()
            assign_tokens = [i for i in var_tokens[p+1:] if i.tokenType != 'source.lang.swift.syntaxtype.comment']
            value = before + ''.join([getContent(e) for e in assign_tokens]).strip()
            return value if len(value) > 0 else None
        return None

    name          = node['key.name']
    typename      = node['key.typename']
    decl_tokens   = getTokenForDecl(tokens, offset, length)
    defaultValue  = getDefaultValue(decl_tokens)
    accessibility = None # FIXME
    parsedDecl    = ''.join([getContent(i) for i in decl_tokens]).strip() # FIXME: unused
    annotations   = getAnnotations(decl_tokens)
    return SwiftVariable(name, typename, defaultValue, accessibility, annotations, parsedDecl)

def visitCase(tokens, enumcase):
    # `sourcekitten doc` doesn't return `case` information. So we have to process.
    #   * You hove to declare `case`s at the beginning of body of enum when you'll contain sub enums, due to parsing limitation.
    #   * You cannot include tuple in case.
    offset = enumcase['key.offset']
    length = enumcase['key.length']
    caseTokens = getTokenForDecl(tokens, offset, length)

    assert(caseTokens[0].content == 'case' and caseTokens[0].tokenType == 'source.lang.swift.syntaxtype.keyword')
    assert(caseTokens[1].tokenType == 'omittedtoken')

    assocVals = []
    def addAssociateValue(valuePair, token):
        value, typename = tuple_pair
        if token.content.find('?') >= 0:
            typename += '?'
        if token.content.find(']') >= 0:
            typename = '[' + typename + ']'
        assocVals.append(SwiftTupleVariable(tuple_pair[0], typename, len(assocVals)))

    label = None
    value = None
    tuple_pair = None
    for t in caseTokens[2:]:
        if t.tokenType == 'source.lang.swift.syntaxtype.identifier':
            if label == None:
                label = t.content
            else:
                if tuple_pair == None:
                    tuple_pair = (None, t.content)
                else:
                    tuple_pair = (tuple_pair[1], t.content)
        elif t.tokenType == 'omittedtoken':
            # FIXME: This code would parse incorrectly in some cases.
            if t.content.find(',') >= 0 or t.content.find(')') >= 0:
                addAssociateValue(tuple_pair, t)
                tuple_pair = None
        elif t.isComment:
            pass
        else:
            assert(value == None)
            value = t.content # FIXME: check tokenType

    annotations = getAnnotations(caseTokens)
    return SwiftCase(label, assocVals, annotations, value)

def visitTypealias(tokens):
    assert(tokens[0].content == 'typealias' and tokens[0].tokenType == 'source.lang.swift.syntaxtype.keyword')
    assert(tokens[1].tokenType == 'omittedtoken')

    label = None
    typeident_base = ''
    before = ''
    for t in tokens[2:]:
        if t.tokenType == 'source.lang.swift.syntaxtype.identifier':
            assert(label == None)
            label = t.content
        elif t.tokenType == 'source.lang.swift.syntaxtype.typeidentifier':
            typeident_base += t.content
        elif t.tokenType == 'omittedtoken':
            # FIXME: This code would parse incorrectly in some cases.
            if typeident_base != '':
                typeident_base += re.sub(r'[\n{].+', '', t.content)
            elif label != None:
                before = re.sub(r'.+=\s+', '', t.content)

    typeident = before + typeident_base
    return SwiftTypealias(label, typeident)

def getTokenForDecl(tokens, offset, length):
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

    return [tokens[i] for i in getRangeForDecl(tokens, offset, length)]

def getAnnotations(tokens):
    annons = [t.annotations for t in tokens if t.isComment] + [{}]
    return annons[0]

def getTokenList(filepath):
    with file(filepath) as f:
        source = f.read()
        syntax = sourcekittenSyntax(filepath)
        return getSwiftTokens(syntax, source)

def processProject(func, structure):
    def visit(filepath, contents):
        sublist = contents.get('key.substructure', [])
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
        anon_dic = var.annotations
        self.isOmitValue = False
        self.jsonLabel = var.name

        annons = anon_dic.get('json', [''])

        if len(annons) == 0:
            return

        name = annons[0]
        if name != '':
            if name == '-':
                self.isOmitValue = True
            else:
                self.jsonLabel = name

        for i in annons[1:]:
            if i == 'omitempty':
                pass
            elif i == 'string':
                pass
            else:
                raise RuntimeError('Unknown annotation: ' + i)


class RouterAnnotation:
    def __init__(self, case_or_class):
        self._variables = case_or_class.variables
        self.method = 'GET'
        self.path = case_or_class.name

        annons = case_or_class.annotations.get('router', [''])

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
        params = [i.name for i in self._variables if i.name != None and i.annotations.get('router', [''])[0] != '-']
        return (pathParams, params)

    @property
    def casePathString(self):
        pathParams, params = self.paramSets()
        union = set(pathParams).intersection(set(params))
        if len(union) == 0: return ''
        lets = [i if i in union else '_' for i in params]
        return '(let (' + ', '.join(lets) + '))'

class WSAnnotation:
    def __init__(self, case_or_class):
        self._variables = case_or_class.variables
        self.name = case_or_class.name
        self.isOmitValue = False

        annons = case_or_class.annotations.get('ws', [''])

        if len(annons) > 0:
            name = annons[0]
            if name == '-':
                self.isOmitValue = True
            elif name != '':
                self.name = name

        if len(annons) > 1:
            self.typename = annons[1]

    @property
    def eventTypename(self):
        n = self.name[0].upper() + self.name[1:]
        return n + 'Event'

def getAnnotationMap():
    return {
        'json'  : JSONAnnotation,
        'router': RouterAnnotation,
        'ws'    : WSAnnotation
    }

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


def getIdlProtocolByName(protocols, name):
    for p in protocols:
        if p.name == name:
            return p.clazz
    return None

def getIdlProtocols(protocols, typenames, default_protocols):
    def getIdlProtocolsByNames(protocols, names):
        clazzes = [getIdlProtocolByName(protocols, n) for n in names]
        return [c() for c in clazzes if c]

    def getDefaultIdlProtocol(protocols, name = 'Default'):
        for p in protocols:
            if p.name == name:
                return getIdlProtocolsByNames(protocols, p.inheritedTypes)
        return []

    ps = getIdlProtocolsByNames(protocols, typenames)
    if len(ps) == 0:
        ps = getDefaultIdlProtocol(protocols, default_protocols)

    return ps

def getNonIdlProtocols(protocols, typenames):
    return [n for n in typenames if getIdlProtocolByName(protocols, n) == None]

### Render functions

def indent(text, isRootLevel=False):
    lines = [('' if isRootLevel else '\t') + t.replace(' ' * 4, '\t') for t in text.split('\n') if len(t.strip()) > 0]
    if isRootLevel: lines = [t if t.strip() != '//' else '' for t in lines]
    return '\n'.join(lines)

### Render Class (IDL protocols)

class ClassInit():
    def classTemplates(self, _):
        return '''
<%
    p = ', '.join([v.name + ': ' + v.typename + (' = ' + v.defaultValue if v.hasDefaultValue else '') for v in clazz.variables])
%>
public init(${p}) {
    % for v in clazz.variables:
    self.${v.name} = ${v.name}
    % endfor
}
'''


class Equatable():
    @property
    def protocolClass(self): return ['Equatable']

    @property
    def protocolEnum(self): return ['Equatable']

    def enumTemplates(self, isRawStyle):
        if isRawStyle:
            return None
        return '', '''
public func == (lhs: ${enum.name}, rhs: ${enum.name}) -> Bool {
    switch (lhs, rhs) {
    % for case in enum.cases:
    <%
        if len(case.variables) == 0:
            av  = 'true'
        elif len(case.variables) == 1:
            av  = 'l == r'
        else:
            av  = ' && '.join(['l.%s == r.%s' % (v.name, v.name) if v.name else 'l.%d == r.%d' % (v.positon, v.positon) for v in case.variables])
    %>
    % if len(case.variables) == 0:
    case (.${case.name}, .${case.name}): return ${av}
    % else:
    case let (.${case.name}(l), .${case.name}(r)): return ${av}
    % endif
    % endfor
    default: return false
    }
}
'''

    def classTemplates(self, _):
        return '', '''
<%
    p = ' &&\\n\t\t'.join(['lhs.' + v.name + ' == rhs.' + v.name for v in clazz.variables])
%>
public func == (lhs: ${clazz.name}, rhs: ${clazz.name}) -> Bool {
    return ${p}
}
'''

class Decodable():
    @property
    def protocolClass(self): return ['Decodable']

    @property
    def protocolEnum(self): return ['Decodable']

    def classTemplates(self, _):
        return '''
<%
    hasOmitValues   = any(map(lambda v: v.annotation('json').isOmitValue        , clazz.variables))
    hasRenameValues = any(map(lambda v: v.annotation('json').jsonLabel != v.name, clazz.variables))
%>
% if hasOmitValues or hasRenameValues:
private enum CodingKeys: String, CodingKey {
% for v in clazz.variables:
    <%
        an = v.annotation('json')
    %>
    % if not an.isOmitValue:
    % if v.name == an.jsonLabel:
    case ${v.name}
    % else:
    case ${v.name} = "${an.jsonLabel}"
    % endif
    % endif
% endfor
}
% endif
'''

class Lensy():
    def classTemplates(self, _):
        templateInner = '''
public struct Lenses {
    % for v in clazz.variables:
    public static let ${v.name} = Lens<${clazz.name}, ${v.typename}>(
        g: { $0.${v.name} },
        <%
        p = ', '.join([w.name + ': ' + ('newValue' if v.name == w.name else 'this.' + w.name) for w in clazz.variables])
        %>
        s: { (this, newValue) in ${clazz.name}(${p}) }
    )
    % endfor
}

public static var $: ${clazz.name}LensHelper<${clazz.name}> {
    return ${clazz.name}LensHelper<${clazz.name}>(lens: createIdentityLens())
}
'''
        templateOuter = '''
<%
   allLenses = [e.name for e in classes if 'Lensy' in e.inheritedTypes]
%>
public struct ${clazz.name}LensHelper<Whole>: LensHelperType {
    public assciatedtype Part = ${clazz.name}
    public let lens: Lens<Whole, Part>
    public init(lens: Lens<Whole, Part>) {
        self.init(lens: lens)
    }

    % for v in clazz.variables:
    <%
        helperType = (v.baseTypename + "LensHelper<Whole>") if v.baseTypename in allLenses else ("LensHelper<Whole, " + v.baseTypename + ">")
        if v.isArray:
            helperType = "ArrayLensHelper<Whole, %s, %s>" % (v.baseTypename, helperType)
        elif v.isOptional:
            helperType = "OptionalLensHelper<Whole, %s, %s>" % (v.baseTypename, helperType)
    %>
    public var ${v.name}: ${helperType} {
        return ${helperType}(parent: self, lens: ${clazz.name}.Lenses.${v.name})
    }
    % endfor
}
'''
        return templateInner, templateOuter


class ErrorType():
    @property
    def protocolEnum(self): return ['ErrorType']

    def enumTemplates(self, _): return None


class Printable():
    @property
    def protocolClass(self): return ['CustomStringConvertible']

    @property
    def protocolEnum(self): return ['CustomStringConvertible']

    def classTemplates(self, _):
        # FIXME: always public
        return '''
<%
    p = ", ".join(["%s=\(%s)" % (v.name, v.name) for v in clazz.variables])
%>
public var description: String {
    return "${clazz.name}(${p})"
}
'''

    def enumTemplates(self, isRawStyle):
        if isRawStyle: # FIXME
            return 'public var description: String { return rawValue }'

        return '''
public var description: String {
    switch self {
    % for case in enum.cases:
    <%
        av  = ['%s=\(%s)' % (v.name, v.name) if v.name else '\(%s)' % v.varname for v in case.variables]
        out = '(' + ', '.join(av) + ')' if len(av) else ''
    %>
    case .${case.name}${case.letString}: return "${case.name}${out}"
    % endfor
    }
}
'''


class EnumStaticInit():
    def enumTemplates(self, isRawStyle):
        if isRawStyle:
            return None
        return '''
% for case in enum.cases:
<%
    ais = map(lambda x: '%s: %s = %s()' % (x._name, x.typename, x.typename) if x._name else 'arg%d: %s = %s()' % (x._positon, x.typename, x.typename), case.variables)
    cis = map(lambda x: '%s: %s' % (x._name, x._name) if x._name else 'arg%d' % x._positon, case.variables)
    params = ", ".join(ais)
    out    = '(' + ', '.join(cis) + ')' if len(cis) > 0 else ''
%>
public static func make${case.name}(${params}) -> ${enum.name} {
    return .${case.name}${out}
}
% endfor
'''


class URLRequestHelper():
    def enumTemplates(self, isRawStyle):
        if isRawStyle:
            return None # FIXME
        else:
            return '''
public var method: String {
    switch self {
    % for case in enum.cases:
    <% an = case.annotation('router') %>
    case .${case.name}: return "${an.method}"
    % endfor
    }
}
//
public var path: String {
    switch self {
    % for case in enum.cases:
    <% an = case.annotation('router') %>
    case .${case.name}${an.casePathString}: return "${an.path}"
    % endfor
    }
}
//
public var params: [String: AnyObject] {
    switch self {
    % for case in enum.cases:
    <%
        def toJsonString(info):
             if info._isArray: return info._name + '.map { $0.toJSON() }'
             return info._name + '.toJSON()'

        an = case.annotation('router')
        pathParams, params = an.paramSets()
        diff = set(params).difference(set(pathParams))

        lets = [i if i in diff else '_' for i in params]
        letString = ('(let (' + ', '.join(lets) + '))') if len(diff) > 0 else ''

        dicx = [i for i in case.variables if i._name in diff]

        inits   = ['"%s": %s' % (i._name, toJsonString(i)) for i in dicx if not i._isOptional]
        initStr = ', '.join(inits) if len(inits) else ':'

        params = ['_ = %s.map { p["%s"] = $0.toJSON() }' % (i._name, i._name) for i in dicx if i._isOptional]
    %>
    % if len(diff) > 0:
    case .${case.name}${letString}:
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
'''


class APIKitHelper():
    def modifyClass(self, swiftClass):
        # add typealias Response
        ts = [e for e in swiftClass.typealiases if e.name == 'APIKitResponse']
        ar = None
        if len(ts) == 0:
            ar = SwiftTypealias('APIKitResponse', swiftClass.name + 'Response')
            swiftClass.typealiases.append(ar)
        else:
            ar = ts[0]

        rs = [e for e in swiftClass.typealiases if e.name == 'Response']
        if len(rs) == 0:
            swiftClass.typealiases.append(SwiftTypealias('Response', ar.assignment))

    def classTemplates(self, _):
        return '''
<%
 an = clazz.annotation('router')
%>
public var method: HTTPMethod {
    return .${an.method.lower()}
}
//
public var path: String {
    return "${an.path}"
}
//
    <%
        def makeAssignParamStatement(variable, classes, variable_name = '$0'):
            found = [e for e in classes if e.name == variable.baseTypename]
            if len(found) == 0:
                return variable_name + ' as AnyObject'
            else:
                assert(len(found) == 1)
                typeOrEnum = found[0]
                if typeOrEnum.isEnum:
                    if typeOrEnum.isRawStyle:
                        return variable_name + '.rawValue as AnyObject'
                    else:
                        return '/* FIXME: not raw enum */'
                else:
                    return '/* FIXME: not enum */'

        def toJsonString(variable, classes):
             if variable.isArray: return variable.name + '.map { ' + makeAssignParamStatement(variable, classes) + ' } as AnyObject'
             return makeAssignParamStatement(variable, classes, variable.name)

        pathParams, params = an.paramSets()
        diff = set(params).difference(set(pathParams))

        lets = [i if i in diff else '_' for i in params]
        letString = ('(let (' + ', '.join(lets) + '))') if len(diff) > 0 else ''

        dicx = [i for i in clazz.variables if i.name in diff]

        # FIXME: use annotation('route') insteadof annotation('json')
        inits   = ['"%s": %s' % (i.annotation('json').jsonLabel, toJsonString(i, classes)) for i in dicx if not i.isOptional]
        initStr = ', '.join(inits) if len(inits) else ':'

        params = ['_ = %s.map { p["%s"] = %s }' % (i.name, i.annotation('json').jsonLabel, makeAssignParamStatement(i, classes)) for i in dicx if i.isOptional]
    %>
% if len(params) > 0 or len(inits) > 0:
public var parameters: Any? {
    % if len(diff) > 0 and len(params) > 0:
    var p: [String: AnyObject] = [${initStr}]
    % for p in params:
    ${p}
    % endfor
    return p
    % else:
    return [${initStr}]
    % endif
}
% endif
'''


class WSHelper():
    def modifyEnum(self, swiftEnum):
        for c in swiftEnum.cases:
            anon = c.annotation('ws')
            if len(c.variables) == 0 and not anon.isOmitValue:
                c._variables = [SwiftTupleVariable(None, anon.eventTypename, 0)]

    def enumTemplates(self, _):
        return '''
fileprivate struct WSEventTypeChecker: Decodable {
    let type: String
}
//
fileprivate struct WSEvent<T: Decodable>: Decodable {
    let type: String
    let data: T
}
//
static func parse(data: Data) throws -> ${enum.name}? {
    let decoder = JSONDecoder()
    if #available(OSX 10.12, iOS 10.0, *) {
        decoder.dateDecodingStrategy = .iso8601
    } else {
        fatalError("Please use newer macOS")
    }
    if let eventType = try? decoder.decode(WSEventTypeChecker.self, from: data) {
        let type = eventType.type
        % for case in enum.cases:
        <% an = case.annotation('ws') %>
        % if not an.isOmitValue:
        <%
            name = case.variables[0].typename
        %>
        if type == "${an.name}" { let ev = try decoder.decode(WSEvent<${name}>.self, from: data); return .${case.name}(ev.data) }
        % endif
        % endfor
    }
    return nil // FIXME
}
'''


### command line pipe lines

def getXcodeVersion():
    p = subprocess.Popen(['xcodebuild', '-version'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    vs = out.split('\n')[0].split()
    if len(vs) > 0:
        return vs[1]
    return None

def getSchemes(project):
    p = subprocess.Popen(['xcodebuild', '-list', '-project', project], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    a = itertools.dropwhile(lambda e: e != '    Schemes:', out.split('\n'))
    return map(str.strip, itertools.islice(a, 1, None))

def execSourcekitten(args):
    p = subprocess.Popen([SOURCEKITTEN] + args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    return json.loads(out)

def sourcekittenDoc(project, scheme):
    return execSourcekitten(['doc', '--', '-project', project, '-scheme', scheme])

def sourcekittenSyntax(filepath):
    return execSourcekitten(['syntax', '--file', filepath])

def parseArgs():
    parser = argparse.ArgumentParser(description=PROGRAM_NAME + ': Swift source generator from Swift')
    parser.add_argument('project', type=str, nargs='?', default=None, help='project to parse')
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
    return [visitProtocol(node) for node in protocol_nodes if node['key.name'] in globals()]

def getDeclarations(ls, n, tokens):
    if n.get('key.kind', None) == 'source.lang.swift.decl.class' or n.get('key.kind', None) == 'source.lang.swift.decl.struct':
        return ls + [visitClass(n, tokens)]
    elif n.get('key.kind', None) == 'source.lang.swift.decl.enum':
        return ls + [visitEnum(n, tokens)]
    else:
        return ls

def getImports(filepath):
    r = []
    im = False
    for t in getTokenList(filepath):
        if t.tokenType == 'source.lang.swift.syntaxtype.keyword':
            im = t.content == 'import'
        elif t.tokenType == 'source.lang.swift.syntaxtype.identifier':
            if im:
                r.append(t.content.strip())
            im = False
    return r

def resolveProject(proj):
    if proj: return proj
    projs = [f for f in os.listdir('.') if os.path.splitext(f)[1] == '.xcodeproj']
    return sorted(projs)[0] if len(projs) > 0 else None

def execute():
    args = parseArgs()
    checkOutputDir(args.output_dir)
    project = resolveProject(args.project)
    if not project:
        print('Xcode project not found')
        return
    schemes = getSchemes(project)
    if not args.scheme in schemes:
        print('Scheme named "%s" is not found in project "%s"' % (args.scheme, project))
        map(print, ['Available schemes:'] + map(lambda e: '\t' + e, schemes))
        return

    structure = sourcekittenDoc(project, args.scheme)
    decls_map = processProject(getDeclarations, structure)

    classes = sum(decls_map.values(), [])
    protocols = gatherIDLProtocol(structure)

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
            #out.write('import Foundation')
            for i in getImports(filepath):
                out.write('import ' + i + '\n')
            out.write('\n\n')
            map(lambda e: out.write(e.getDeclarationString(classes, protocols) + '\n\n'), decls)


if __name__ == '__main__':
    assert(int(getXcodeVersion().split('.')[0]) >= 7)
    execute()
