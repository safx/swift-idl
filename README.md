# Swift-IDL

Swift-IDL generates Swift source code from Swift.

Swift-IDL can generate functions or computed properties to add *peseudo* protocols as follows:

Available protocols is declared in IDLProtocols.swift.

* JSONEncodable
* JSONDecodable
* URLRequestHelper
* ClassInit
* Printable
* APIKitHelper
* EJDB
* EnumStaticInit (WIP, maybe dropped)
* NSCoding (WIP, maybe dropped)

## Requirements

* mako (`pip install mako`)
* SourceKitten (Use `brew install SourceKitten`)
* Xcode 7 (Set as default to use `xcode-select -s`)

## Up and Running

1. Create target, e.g., "IDL", which can omit some options in command line later, by choosing "Command Line Tool" to your project in Xcode.
1. Add scheme, e.g., "IDL". If you add target in Xcode, this step could be skipped.
1. Add Empty Swift file, which add target to "IDL"
1. `python swift-idl.py -o Source/gen YourProject.xcodeproj -f`
1. Add the output files to your project.
1. Add the additional Swift files from protocol directory, which depends on protocols you chosen. For example, add `JSONDecodable.swift` to your project, if you use `JSONDecodable`.

## Usage

`swift-idl.py --help` will show the usage text.

```sh
usage: swift-idl.py [-h] [-o OUTPUT_DIR] [-f] [project] [scheme]

swift-idl: Swift source generator from Swift

positional arguments:
  project               project to parse
  scheme                sceheme to parse

optional arguments:
  -h, --help            show this help message and exit
  -o OUTPUT_DIR, --output_dir OUTPUT_DIR
                        directory to output
  -f, --force           force to output
```


## IDL Protocols

### JSONDecodable

You can customize output by using annotatin 'json'.

### JSONEncodable

You can customize output by using annotatin 'json'.

### APIKitHelper

```swift
class CreateTalk: ClassInit, APIKitHelper, MyRequest { // router:"POST,topics/\(topicId)/talks"
    let topicId: Int
    let talkName: String
    let postIds: [Int] = []
}
```

You can add custom Request class e.g., MyRequest, for your customizing point.

### URLRequestHelper

```swift
enum Router: URLRequestHelper {
    case GetMessages(id: Int, count: Int?)      // router:",message/\(id)"
    case PostMessage(id: Int, message: String)  // router:"POST,message/\(id)"
}
```


## Annotations

We can customize output code to add annotations as formatted comment in member variables or cases.

```swift
public struct Blog: JSONDecodable {
    public let title      : String
    public let authorName : String       // JSON:"author-name"
    public let homepageURL: NSURL?       // JSON:"homepage-url"
    public let faviconURL : NSURL?       // JSON:"favicon-url"
    public let updated    : Bool = false // JSON:"-"
}
```

### [enum][class] `json`

`json` annotation is basically same as in Go-lang.

```swift
// json:"<Name>"
```

* Name: field name for JSON object. Variable name is used when name is omitted or empty string.
        For special case, if `Name` is `-`, this variable is ignored for encoding and decoding.

### [enum] `router`

```swift
// router:"<Method>,<Path>,<ResponseType>"
```

* `Method`: HTTP Method for the request like `GET` or `POST`. `GET` is used when `Method` is omitted or empty string.
    (default: `GET`)
* `Path`: path of the request URL. Case name or class name is used when `Path` is omitted or empty string.
* `Response`: response type of the request. Case name or class name appending `Response` is used when `Resonse` is omitted or empty string.
