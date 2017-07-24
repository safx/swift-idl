#!/usr/bin/python

from pyquery import PyQuery as pq
import json
import re
import sys
import json2idl

SITE_ROOT = 'https://developer.nulab-inc.com'
API_ROOT = SITE_ROOT + '/docs/typetalk/'
DOC_BASE_PATH = '/docs/typetalk/api/1/'

typenameMap = {
    'Number': 'Int',
    'Boolean': 'Bool'
}

defaultValueMap = {
    'Number': '0',
    'Boolean': 'false',
    'String': '""',
}


def printXCTAssert(value, path):
    path = path.replace('.embed.', '.embed!.')
    if len(path) > 5 and path[-5:] == '.type':
        path += '.rawValue'

    def p(v):
        s = str(v)
        if type(v) == str or type(v) == unicode:
            return '"%s"' % s.replace('"', '\\"').replace('\n', '\\n')
        elif type(v) == type(None):
            return 'nil'
        elif type(v) == bool:
            return s.lower()
        return s

    if type(value) == type(None):
        print('\t\t\t\tXCTAssertNil(%s)' % (path))
        return

    if path[-3:].lower() == 'url' and (value.find('http://') == 0 or value.find('https://') == 0):
        path += '.absoluteString'

    m = re.match(r'(\d{4}-\d{2}-\d{2})T(\d{2}:\d{2}:\d{2})Z', str(value))
    if m:
        path += '.description'
        value = m.group(1) + ' ' + m.group(2) + ' +0000'
    print('\t\t\t\tXCTAssertEqual(%s, %s)' % (path, p(value)))


def printTestForJSONDictionary(dic, prefix='r'):
    for k,v in dic.items():
        varName = ''.join([e if idx == 0 else e[0].upper() + e[1:] for idx, e in enumerate(k.split('_'))])
        pf = prefix + '.' + varName
        t = type(v)
        if t == dict:
            printTestForJSONDictionary(v, pf)
        elif t == list:
            printTestForJSONArray(v, pf)
        else:
            printXCTAssert(v, pf)

def printTestForJSONArray(arr, prefix='r.'):
    print('\t\t\t\tXCTAssertEqual(%s.count, %d)' % (prefix, len(arr)))
    for k,v in enumerate(arr):
        pf = prefix + '[' + str(k) + ']'
        t = type(v)
        if t == dict:
            printTestForJSONDictionary(v, pf)
        elif t == list:
            printTestForJSONArray(v, pf)
        else:
            printXCTAssert(v, pf)


class ParamInfo(object):
    def __init__(self, trElement):
        tds = [pq(trElement)('td').eq(j).text() for j in range(3)]
        ns = tds[0].split()
        self.name = ns[0]
        self.optional = len(ns) > 1 and ns[1] == '(Optional)'
        self.array = False
        self.typename = tds[1]
        self.description = tds[2]

        self.validVariableName = True
        name = self.name
        if name[-3:] == '[0]':
            self.array = True
            self.name = name[:-3]
        elif name.find('[0]') >= 0:
            self.validVariableName = False

    def getTypenameString(self):
        ret = typenameMap.get(self.typename, self.typename)
        if self.array:
            ret = '[%s] = []' % ret
        elif self.optional:
            ret += '? = nil'
        return ret

    def toParamString(self):
        return ('' if self.validVariableName else '//') + 'let %s: %s' % (self.name, self.getTypenameString())

    def __repr__(self):
        return '(%s, %s, %s, %s)' % (self.name, str(self.optional), self.typename, self.description)


class WebAPI(object):
    def __init__(self, hyphenName, method, path, scope, urlParams, formParams, queryParams, response):
        self.hyphenName = hyphenName
        self.method = method
        self.path = path
        self.scope = scope
        self.urlParams = urlParams
        self.formParams = formParams
        self.queryParams = queryParams
        self.response = response

    @property
    def className(self):
        def toCamelCase(name):
            return ''.join([e[0].upper() + e[1:] for e in name.split('-')])
        return toCamelCase(self.hyphenName)

    @property
    def params(self):
        return [e for e in self.urlParams + self.formParams + self.queryParams]

    def createCtorExprString(self):
        return self.className + '(' + ', '.join([e.name + ': ' + defaultValueMap[e.typename] for e in self.params if e.validVariableName and not e.optional and not e.array]) + ')'

    def printTestCase(self):
        if self.response != None:
            print('''
	func test%s() {
		createStub("%s")

		let expectation = self.expectation(description: "")
		TypetalkAPI.send(%s()) { result in
			switch result {
			case .success(let r):
''' % (self.className, self.hyphenName, self.className))
            root = json.loads(self.response)
            printTestForJSONDictionary(root)


	    print('''
				expectation.fulfill()
			case .failure(let error):
				XCTFail("\(error)")
			}
		}

		waitForExpectations(timeout: 3) { (error) in
			XCTAssertNil(error, "\(error)")
		}
	}
''')

    def getRequestClassString(self):
        paramsString = ''
        if len(self.params):
            maxLen = max([len(e.toParamString()) for e in self.params])
            formatString = '%-' + str(maxLen) + 's // %s\n'
            paramsString = ''.join(['\t' + formatString % (e.toParamString(), e.description) for e in self.params])

        return """class %s: ClassInit, APIKitHelper, TypetalkRequest { // router:"%s,%s"
%s}
    """ % (self.className, self.method, self.path, paramsString)

    def printResponseClass(self):
        if self.response != None:
            obj = json.loads(self.response)
            info = json2idl.guessTypenameForDict(obj)
            #print(info)
            json2idl.printClass(info, self.className + 'Response')

    def writeResponseJson(self):
        if self.response == None:
            return

        outputName = 'api_' + self.hyphenName + '.json'
        with file(outputName, 'w') as f:
            f.write(self.response)
            try:
                json.loads(self.response)
            except:
                print('WARNING:' + self.hyphenName + ' is not valid JSON.')


def getWebAPI(apiPagePath):
    getParam = lambda e: [ParamInfo(e) for e in e('table > tbody > tr')]
    def composeUrl(urlBase, urlParams):
        return urlBase + ''.join(['\\(' + e[1:] + ')' for e in urlParams])

    data = pq(url=SITE_ROOT + apiPagePath)

    method = None
    url = None
    scope = None
    urlParams = []
    queryParams = []
    formParams = []
    response = None

    hs = data('div.content__wrapper > h3')
    for q in range(len(hs)):
        e = hs.eq(q).next()
        f = hs.eq(q).text()
        if f == 'Method':
            method = e.text()
        elif f == 'URL':
            cs = e.contents()
            urlBase = cs[0]
            #print(cs, urlBase, e.text())
            #urlParams = [e.text for e in cs[1:]]
            #url = composeUrl(urlBase, urlParams)

            url = e.text()
        elif f == 'Scope':
            scope = e.text()
        elif f == 'URL parameters':
            urlParams = getParam(e)
        elif f == 'Query parameters':
            queryParams = getParam(e)
        elif f == 'Form parameters':
            formParams = getParam(e)
        elif f == 'Response Example':
            response = e.text() if e.text() != '' else '{}'
        else:
            print(f)
            raise 'Error'

    hyphenName = apiPagePath.split('/')[-1]
    path = url.replace('https://typetalk.in/api/v1/', '') # FIXME: typetalk
    return WebAPI(hyphenName, method, path, scope, urlParams, formParams, queryParams, response)



top = pq(url=API_ROOT)
rrr = 999
for i in top('a.sidebar__links'):
    path = i.get('href')
    if path.find(DOC_BASE_PATH) != 0: continue
    if path == '/docs/typetalk/api/1/streaming': continue # FIXME

    api = getWebAPI(path)
    #print("----- " + api.hyphenName)
    #print(api.getRequestClassString())
    api.printTestCase()
    #api.printResponseClass()
    #api.writeResponseJson()

    rrr -= 1
    if rrr < 0:
        sys.exit(0)  # DELETEME
