#!/usr/bin/python

from __future__ import print_function

import argparse
import functools
import itertools
import json
import os
import subprocess
import sys
import re


ST_NONE   = 1
ST_BOOL   = 2
ST_INT    = 3
ST_STR    = 4
ST_FLOAT  = 5
ST_URL    = 6
ST_ANYOBJECT = 999

def mergeIntoSingleObject(guessedObjectArray):
    keys = reduce(lambda a, e: a.union(set(e.keys())), guessedObjectArray, set())
    merged = {}
    for obj in guessedObjectArray:
        for key in keys:
            value = obj.get(key, set([ST_NONE]))
            if type(value) == dict:
                merged[key] = value
            elif type(value) == list:
                merged[key] = mergeIntoSingleObject(value)
            elif key in merged:
                merged[key] = merged[key].union(value)
            else:
                merged[key] = value
    return [merged]

def guessTypenameForArray(json):
    arr = []
    for value in json:
        if type(value) == dict:
            arr.append(guessTypenameForDict(value))

    if all([type(i) == dict for i in arr]):
        return mergeIntoSingleObject(arr)

    assert(False)
    return arr # Unexpected return


def guessTypenameForDict(json):
    info = {}
    for key, value in json.iteritems():
        if type(value) == dict:
            info[key] = guessTypenameForDict(value)
        elif type(value) == list:
            info[key] = guessTypenameForArray(value)
        else:
            info[key] = set([guessTypename(value)])
    return info


def toCamelCase(name, firstLetterUpper=True):
    ts = re.split('[-_/]', name)
    if len(ts) > 1:
        ret = ''.join([e.capitalize() for e in ts])
    else:
        ret = name
    return (unicode.upper if firstLetterUpper else unicode.lower)(ret[0]) + ret[1:]


def guessTypename(v):
    if type(v) == type(None): return ST_NONE

    typemap = {
        bool: ST_BOOL,
        int:  ST_INT,
        str:  ST_STR,
        unicode: ST_STR,
        float: ST_FLOAT
    }

    typename = typemap.get(type(v), ST_NONE)
    if typename == ST_STR:
        if v.find('http://') == 0 or v.find('https://') == 0:
            if v.find('{') == -1 and v.find('{') == -1:
                return ST_URL

    return typename

def guessTypenameFromSet(ts):
    assert(len(ts) > 0)

    isOptional = ST_NONE in ts
    if isOptional:
        ts.remove(ST_NONE)
        if len(ts) == 0: return '<# AnyObject #>'

    typemap = {
        ST_BOOL: 'Bool',
        ST_INT: 'Int',
        ST_STR: 'String',
        ST_FLOAT: 'Float',
        ST_URL: 'NSURL',
        ST_ANYOBJECT: 'AnyObject'
    }

    if len(ts) == 1:
        s = list(ts)[0]
    else:
        if ST_STR in ts:
            s = ST_STR
        else:
            s = ST_ANYOBJECT

    ret = typemap.get(s, None)
    if ret == None: return '<# AnyObject #>'
    return ret + ('?' if isOptional else '')


def printClass(info, name, level = 0):
    subdicts = []

    print('\t' * level + 'struct %s: JSONDecodable {' % name)
    for key, value in info.iteritems():
        fieldName = toCamelCase(key, False)
        comment = '' if fieldName == key else '// json:"%s"' % key
        typename = toCamelCase(key)

        if type(value) == dict:
            subdicts.append((value, typename))
        elif type(value) == list:
            assert(len(value) == 1)
            subdicts.append((value[0], typename))
            typename = '[' + typename + ']'
        else:
            typename = guessTypenameFromSet(value)

        print('\t' * (level+1) + 'let %-20s: %-20s %s' % (fieldName, typename, comment))

    if len(subdicts) > 0:
        print()
        map(lambda e: printClass(e[0], e[1], level + 1), subdicts)
    print('\t' * level + '}')


def parseArgs():
    parser = argparse.ArgumentParser(description='Swift source generator from JSON')
    parser.add_argument('jsonfile', type=argparse.FileType('r'), nargs='?', help='json to parse', default=sys.stdin)
    parser.add_argument('-c', '--classname', type=str, default=None, help='class name')
    parser.add_argument('-p', '--parameter', type=str, default=None, help='annotation parameter')
    parser.add_argument('-a', '--apikit', action='store_true', help='APIKit')
    return parser.parse_args()


def resolveStructName(args):
    cname = args.classname
    if cname:
        return toCamelCase(cname)

    name = args.jsonfile.name
    if name == '<stdin>':
        return '<# ClassName #>'
    name, ext = os.path.splitext(os.path.basename(name))
    return toCamelCase(name)


def execute():
    args = parseArgs()
    obj = json.loads(args.jsonfile.read())
    args.jsonfile.close()

    info = guessTypenameForDict(obj)
    typename = resolveStructName(args)

    if args.apikit:
        param = args.parameter
        if not param:
            param = ',' + (args.classname or typename)
        print('struct %s: ClassInit, APIKitHelper, Request { // router:"%s"' % (typename, param))
        print('	typealias APIKitResponse = %sResponse' % (typename,))
        print('')
        printClass(info, typename + 'Response', 1)
        print('}')
    else:
        printClass(info, typename)

if __name__ == '__main__':
    execute()
