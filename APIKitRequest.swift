//
//  APIKitRequest.swift
//  Swift-idl
//
//  Created by Safx Developer on 2015/05/12.
//  Copyright (c) 2015年 Safx Developers. All rights reserved.
//

import APIKit

public protocol APIKitRequestBase: Request {}

// You hove to uncomment or declare the following block.
/*
extension APIKitRequestBase {
    public var baseURL: NSURL { return NSURL(string: "https://www.example.com/api")! }
}
*/

public class APIKitRequest<T where T: JSONDecodable, T == T.DecodedType >: APIKitRequestBase {
    public typealias Response = T

    public let route: Router

    public init(route: Router) {
        self.route = route
    }

    public var method: HTTPMethod { return HTTPMethod(rawValue: route.method)! }
    public var path: String { return route.path }
    public var parameters: [String: AnyObject] { return route.params }

    public func responseFromObject(object: AnyObject, URLResponse: NSHTTPURLResponse) -> Response? {
        do {
            return try Response.parseJSON(object)
        } catch {
            print(error)
        }
        return nil
    }
}
