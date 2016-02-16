import unittest
import swift_idl as IDL

test_structure = {
    "key.kind" : "source.lang.swift.decl.struct",
    "key.offset" : 19,
    "key.nameoffset" : 26,
    "key.namelength" : 3,
    "key.inheritedtypes" : [
        {
            "key.name" : "JSONDecodable"
        }
    ],
    "key.bodylength" : 110,
    "key.accessibility" : "source.lang.swift.accessibility.internal",
    "key.substructure" : [
        {
            "key.kind" : "source.lang.swift.decl.var.instance",
            "key.offset" : 72,
            "key.attributes" : [
                {
                    "key.attribute" : "source.decl.attribute.__raw_doc_comment"
                }
            ],
            "key.nameoffset" : 76,
            "key.namelength" : 2,
            "key.length" : 15,
            "key.accessibility" : "source.lang.swift.accessibility.internal",
            "key.substructure" : [

            ],
            "key.typename" : "Int",
            "key.name" : "id"
        },
        {
            "key.kind" : "source.lang.swift.decl.var.instance",
            "key.offset" : 92,
            "key.nameoffset" : 96,
            "key.namelength" : 5,
            "key.length" : 17,
            "key.accessibility" : "source.lang.swift.accessibility.internal",
            "key.substructure" : [

            ],
            "key.typename" : "String",
            "key.name" : "query"
        },
        {
            "key.kind" : "source.lang.swift.decl.var.instance",
            "key.offset" : 126,
            "key.attributes" : [
                {
                    "key.attribute" : "source.decl.attribute.__raw_doc_comment"
                }
            ],
            "key.nameoffset" : 130,
            "key.namelength" : 1,
            "key.length" : 13,
            "key.accessibility" : "source.lang.swift.accessibility.internal",
            "key.substructure" : [

            ],
            "key.typename" : "String",
            "key.name" : "z"
        }
    ],
    "key.name" : "Foo",
    "key.length" : 138,
    "key.bodyoffset" : 46
}

test_syntax = [
  { "offset" : 0, "length" : 6, "type" : "source.lang.swift.syntaxtype.keyword" },
  { "offset" : 7, "length" : 10, "type" : "source.lang.swift.syntaxtype.identifier" },
  { "offset" : 19, "length" : 6, "type" : "source.lang.swift.syntaxtype.keyword" },
  { "offset" : 26, "length" : 3, "type" : "source.lang.swift.syntaxtype.identifier" },
  { "offset" : 31, "length" : 13, "type" : "source.lang.swift.syntaxtype.typeidentifier" },
  { "offset" : 47, "length" : 21, "type" : "source.lang.swift.syntaxtype.comment" },
  { "offset" : 72, "length" : 3, "type" : "source.lang.swift.syntaxtype.keyword" },
  { "offset" : 76, "length" : 2, "type" : "source.lang.swift.syntaxtype.identifier" },
  { "offset" : 80, "length" : 3, "type" : "source.lang.swift.syntaxtype.typeidentifier" },
  { "offset" : 86, "length" : 1, "type" : "source.lang.swift.syntaxtype.number" },
  { "offset" : 92, "length" : 3, "type" : "source.lang.swift.syntaxtype.keyword" },
  { "offset" : 96, "length" : 5, "type" : "source.lang.swift.syntaxtype.identifier" },
  { "offset" : 103, "length" : 6, "type" : "source.lang.swift.syntaxtype.typeidentifier" },
  { "offset" : 110, "length" : 12, "type" : "source.lang.swift.syntaxtype.comment" },
  { "offset" : 126, "length" : 3, "type" : "source.lang.swift.syntaxtype.keyword" },
  { "offset" : 130, "length" : 1, "type" : "source.lang.swift.syntaxtype.identifier" },
  { "offset" : 133, "length" : 6, "type" : "source.lang.swift.syntaxtype.typeidentifier" },
  { "offset" : 144, "length" : 12, "type" : "source.lang.swift.syntaxtype.comment" }
]

test_source = '''import Foundation

struct Foo: JSONDecodable { // sample:"foo,,bar"
    let id: Int = 3
    let query: String // json:"q"
    let z: String     // json:"-"
}
'''


class SampleStructTest(unittest.TestCase):
    def test_getSwiftTokens(self):
        tk = IDL.getSwiftTokens(test_syntax, test_source)
        self.assertEqual('import', tk[0].content)
        self.assertEqual(1, tk[0].line)
        self.assertEqual('source.lang.swift.syntaxtype.keyword', tk[0].tokenType)

        self.assertEqual('}\n', tk[-1].content)
        self.assertEqual(7, tk[-1].line)
        self.assertEqual('omittedtoken', tk[-1].tokenType)

if __name__ == '__main__':
    unittest.main()
