//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 16/11/2022.
//

import Foundation

protocol HLEnumConstructProvider {
    var nameProvider: any StringProvider { get }
    var params: UnsafePointer<UnsafePointer<HLType_CCompat>> { get }
    var size: Int32 { get }
    var hasptr: Bool { get }
    var offsets: UnsafePointer<Int32> { get }
}
