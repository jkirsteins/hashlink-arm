//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 16/11/2022.
//

import Foundation

protocol HLTypeEnumProvider {
    var nameProvider: any StringProvider { get }
    var constructsProvider: [any HLEnumConstructProvider] { get }
    var global_value: UnsafeRawPointer { get }
}
