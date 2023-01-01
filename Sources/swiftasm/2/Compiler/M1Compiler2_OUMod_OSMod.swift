//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 20/12/2022.
//

import Foundation

extension M1Compiler2 {
    func __omod_integer(
        dst: Reg,
        a: Reg,
        b: Reg,
        signed: Bool,
        regs: [any HLTypeKindProvider],
        mem: CpuOpBuffer)
    {
        assertInteger(reg: dst, from: regs)
        assertInteger(reg: a, from: regs)
        assertInteger(reg: b, from: regs)
        
        appendLoad(0, from: a, kinds: regs, mem: mem)
        appendSignMode(signed, reg: X.x0, from: a, kinds: regs, mem: mem)
        appendLoad(1, from: b, kinds: regs, mem: mem)
        appendSignMode(signed, reg: X.x1, from: b, kinds: regs, mem: mem)
        
        mem.append(
            signed ? M1Op.sdiv(X.x10, X.x0, X.x1) : M1Op.udiv(X.x10, X.x0, X.x1),
            M1Op.mul(X.x10, X.x10, X.x1),
            M1Op.subs(X.x0, X.x0, .reg64shift(X.x10, nil))
        )
        
        appendStore(0, into: dst, kinds: regs, mem: mem)
    }
}
