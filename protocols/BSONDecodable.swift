//
//  BSONDecodable.swift
//  Swift-idl
//
//  Created by Safx Developer on 2015/05/12.
//  Copyright (c) 2015年 Safx Developers. All rights reserved.
//

import Foundation



public protocol BSONDecodable {
    typealias DecodedType = Self
    static func parseBSON(itor: BSONIterator) throws -> DecodedType
}

// MARK:

extension NSURL: BSONDecodable {
    public static func parseBSON(itor: BSONIterator) throws -> NSURL {
        if itor.type == BSON_STRING {
            if let val = NSURL(string: itor.stringValue) {
                return val
            }
        }
        throw JSONDecodeError.ValueTranslationFailed(type: "NSURL")
    }
}

extension NSDate: BSONDecodable {
    public static func parseBSON(itor: BSONIterator) throws -> NSDate {
        if itor.type == BSON_STRING {
            let dateFormatter = NSDateFormatter()
            dateFormatter.dateFormat = "yyyy-MM-dd"
            if let newDate = dateFormatter.dateFromString(itor.stringValue) {
                return newDate
            }
        }
        throw JSONDecodeError.ValueTranslationFailed(type: "NSURL")
    }
}

extension String: BSONDecodable {
    public static func parseBSON(itor: BSONIterator) throws -> String {
        if itor.type == BSON_STRING {
            return itor.stringValue
        }
        throw JSONDecodeError.ValueTranslationFailed(type: "String")
    }
}

extension Float: BSONDecodable {
    public static func parseBSON(itor: BSONIterator) throws -> Float {
        if itor.type == BSON_DOUBLE {
            return Float(itor.doubleValue) // FIXME
        }
        throw JSONDecodeError.ValueTranslationFailed(type: "Float")
    }
}

extension Int: BSONDecodable {
    public static func parseBSON(itor: BSONIterator) throws -> Int {
        if itor.type == BSON_DOUBLE {
            return itor.intValue
        }
        throw JSONDecodeError.ValueTranslationFailed(type: "Int")
    }
}

extension Bool: BSONDecodable {
    public static func parseBSON(itor: BSONIterator) throws -> Bool {
        if itor.type == BSON_BOOL {
            return itor.boolValue
        }
        throw JSONDecodeError.ValueTranslationFailed(type: "Bool")
    }
}
