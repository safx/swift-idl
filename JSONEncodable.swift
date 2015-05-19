//
//  JSONEncodable.swift
//  Swift-idl
//
//  Created by Safx Developer on 2015/05/12.
//  Copyright (c) 2015年 Safx Developers. All rights reserved.
//

import Foundation

protocol JSONEncodable {
    func toJSON() -> [String: AnyObject]
}

// MARK:

extension NSURL {
    func toJSON() -> String {
        return self.absoluteString!
    }
}

extension NSDate {
    func toJSON() -> String {
        let dateFormatter = NSDateFormatter()
        dateFormatter.dateFormat = "yyyy-MM-dd"
        return dateFormatter.stringFromDate(self)
    }
}

extension String {
    func toJSON() -> String {
        return self
    }
}

extension Float {
    func toJSON() -> NSNumber {
        return NSNumber(float: self)
    }
}

extension Int {
    func toJSON() -> NSNumber {
        return NSNumber(integer: self)
    }
}

extension UInt {
    func toJSON() -> NSNumber {
        return NSNumber(unsignedLong: self)
    }
}

extension Bool {
    func toJSON() -> NSNumber {
        return NSNumber(bool: self)
    }
}
