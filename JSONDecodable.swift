//
//  JSONDecodable.swift
//  Swift-idl
//
//  Created by Safx Developer on 2015/05/12.
//  Copyright (c) 2015年 Safx Developers. All rights reserved.
//

import Foundation

protocol JSONDecodable {
    typealias DecodedType = Self
    static func parseJSON(data: AnyObject) -> (decoded: DecodedType?, error: String?)
}

// MARK:

extension NSURL: JSONDecodable {
    static func parseJSON(data: AnyObject) -> (decoded: NSURL?, error: String?) {
        if let v = data as? String {
            return (NSURL(string: v), nil)
        }
        return (nil, "Type translate failed in NSURL")
    }
}

extension NSDate: JSONDecodable {
    static func parseJSON(data: AnyObject) -> (decoded: NSDate?, error: String?) {
        if let v = data as? String {
            let dateFormatter = NSDateFormatter()
            dateFormatter.dateFormat = "yyyy-MM-dd"
            if let newDate = dateFormatter.dateFromString(v) {
                return (newDate, nil)
            }
        }
        return (nil, "Type translate failed in NSDate")
    }
}

extension String: JSONDecodable {
    static func parseJSON(data: AnyObject) -> (decoded: String?, error: String?) {
        if let v = data as? String {
            return (v, nil)
        }
        return (nil, "Type translate failed in String")
    }
}

extension Float: JSONDecodable {
    static func parseJSON(data: AnyObject) -> (decoded: Float?, error: String?) {
        if let v = data as? NSNumber {
            return (v.floatValue, nil)
        }
        return (nil, "Type translate failed in Float")
    }
}

extension Int: JSONDecodable {
    static func parseJSON(data: AnyObject) -> (decoded: Int?, error: String?) {
        if let v = data as? NSNumber {
            return (v.integerValue, nil)
        }
        return (nil, "Type translate failed in Int")
    }
}

extension UInt: JSONDecodable {
    static func parseJSON(data: AnyObject) -> (decoded: UInt?, error: String?) {
        if let v = data as? NSNumber {
            return (UInt(v.unsignedIntegerValue), nil)
        }
        return (nil, "Type translate failed in UInt")
    }
}

extension Bool: JSONDecodable {
    static func parseJSON(data: AnyObject) -> (decoded: Bool?, error: String?) {
        if let v = data as? NSNumber {
            return (v.boolValue, nil)
        }
        return (nil, "Type translate failed in Bool")
    }
}
