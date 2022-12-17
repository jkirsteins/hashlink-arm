//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 17/12/2022.
//

import XCTest

func XCTAssertEqualFloat(_ a: Float32, _ b: Float32, file: StaticString = #filePath,
                         line: UInt = #line) {
    let eq = abs(a - b) <= 0.01
    
    guard eq else {
        return XCTFail("\(a) != \(b)", file: file, line: line)
    }
}

func XCTAssertEqualDouble(_ a: Float64, _ b: Float64, file: StaticString = #filePath,
                          line: UInt = #line) {
    let eq = fabs(a - b) <= 0.01
    
    guard eq else {
        return XCTFail("\(a) != \(b)", file: file, line: line)
    }
}
