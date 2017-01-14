//
//  APIKitRequest.swift
//  Swift-idl
//
//  Created by Safx Developer on 2015/05/12.
//  Copyright (c) 2015年 Safx Developers. All rights reserved.
//

import Foundation
import APIKit

public protocol APIKitRequest: Request {
    associatedtype APIKitResponse
    associatedtype Response = APIKitResponse
    //typealias Response = Result<APIKitResponse>
}

/*
extension APIKitRequest {
    public var baseURL: NSURL { return NSURL(string: "https://api.example.com/")! }
}
*/

extension APIKitRequest where Self.Response: JSONDecodable, Self.Response == Self.Response.DecodedType {
    public func responseFromObject(_ object: AnyObject, URLResponse: HTTPURLResponse) throws -> Response {
        do {
            return try Response.parseJSON(object)
        } catch {
            print(error)
            throw ResponseError.UnexpectedObject(object)
        }
    }
}
