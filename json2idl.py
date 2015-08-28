#!/usr/bin/python

from __future__ import print_function

import argparse
import functools
import itertools
import json
import os
import re
import subprocess


ST_NONE   = 1
ST_BOOL   = 2
ST_INT    = 3
ST_STR    = 4
ST_FLOAT  = 5
ST_URL    = 6
ST_ANYOBJECT = 999

def mergeObject(arr):
    keys = reduce(lambda a, e: a.union(set(e.keys())), arr, set())
    merged = {}
    for obj in arr:
        for key in keys:
            value = obj.get(key, set([ST_NONE]))
            if type(value) == dict:
                merged[key] = value
            elif key in merged:
                merged[key] = merged[key].union(value)
            else:
                merged[key] = value
    return [merged]

def gatherInfoJSONArray(json):
    arr = []
    for value in json:
        if type(value) == dict:
            arr.append(gatherInfoJSONObject(value))

    if all([type(i) == dict for i in arr]):
        return mergeObject(arr)
    assert(False)
    return arr


def gatherInfoJSONObject(json):
    info = {}
    for key, value in json.iteritems():
        if type(value) == dict:
            info[key] = gatherInfoJSONObject(value)
        elif type(value) == list:
            info[key] = gatherInfoJSONArray(value)
        else:
            info[key] = set([guessTypename(value)])
    return info


def toCamelCase(name, convertFirstLetter=True):
    def c(s):
        if len(s) == 0: return s
        return s[0].upper() + s[1:]

    ts = re.split('[-_]', name)
    ret = ''.join([c(e) for e in ts])
    return ret if convertFirstLetter else name[0].lower() + ret[1:]



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
        if len(ts) == 0: return '<# Unknown #>'

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
    if ret == None: return '<# Unknown #>'
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
    parser.add_argument('jsonfile', type=str, nargs='?', help='json to parse')
    parser.add_argument('-c', '--classname', type=str, default=None, help='class name')
    return parser.parse_args()


def execute():
    args = parseArgs()
    filepath = args.jsonfile
    with file(filepath) as f:
        obj = json.loads(f.read())
        info = gatherInfoJSONObject(obj)
        name = args.classname
        if not name:
            name, ext = os.path.splitext(os.path.basename(filepath))
        printClass(info, toCamelCase(name))


if __name__ == '__main__':
    execute()
