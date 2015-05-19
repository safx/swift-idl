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
    static func parseJSON(data: [String: AnyObject]) -> (decoded: DecodedType?, error: String?)
}

// MARK:

extension NSURL {
    static func parseJSON(data: AnyObject) -> (NSURL?, String?) {
        if let v = data as? String {
            return (NSURL(string: v), nil)
        }
        return (nil, "Type translate failed in NSURL")
    }
}

extension NSDate {
    static func parseJSON(data: AnyObject) -> (NSDate?, String?) {
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

extension String {
    static func parseJSON(data: AnyObject) -> (String?, String?) {
        if let v = data as? String {
            return (v, nil)
        }
        return (nil, "Type translate failed in String")
    }
}

extension Float {
    static func parseJSON(data: AnyObject) -> (Float?, String?) {
        if let v = data as? NSNumber {
            return (v.floatValue, nil)
        }
        return (nil, "Type translate failed in Float")
    }
}

extension Int {
    static func parseJSON(data: AnyObject) -> (Int?, String?) {
        if let v = data as? NSNumber {
            return (v.integerValue, nil)
        }
        return (nil, "Type translate failed in Int")
    }
}

extension UInt {
    static func parseJSON(data: AnyObject) -> (UInt?, String?) {
        if let v = data as? NSNumber {
            return (UInt(v.unsignedIntegerValue), nil)
        }
        return (nil, "Type translate failed in UInt")
    }
}

extension Bool {
    static func parseJSON(data: AnyObject) -> (Bool?, String?) {
        if let v = data as? NSNumber {
            return (v.boolValue, nil)
        }
        return (nil, "Type translate failed in Bool")
    }
}
