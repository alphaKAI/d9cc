module d9cc.opt;
import d9cc.genir, d9cc.parse;

// Optimization pass. In this pass, we promote all non-address-taken
// integer variables to register values. As a result, we may have more
// register values than the number of the physical registers, but
// that's fine. Regalloc will spill them out to memory.

// Rewrite
//
//  BPREL r1, <offset>
//  STORE r1, r2
//  LOAD  r3, r1
//
// to
//
//  NOP
//  r4 = r2
//  r3 = r4
static void opt(IR ir) {
  if (ir.op == IROpcode.BPREL) {
    Var var = ir.var;

    if (var.address_taken || var.ty.ty != CType.INT) {
      return;
    }

    if (var.promoted is null) {
      var.promoted = new Reg();
    }

    ir.op = IROpcode.NOP;
    ir.r0.promoted = var.promoted;
    return;
  }

  if (ir.op == IROpcode.LOAD) {
    if (ir.r2.promoted is null) {
      return;
    }
    ir.op = IROpcode.MOV;
    ir.r2 = ir.r2.promoted;
    return;
  }

  if (ir.op == IROpcode.STORE) {
    if (ir.r1.promoted is null) {
      return;
    }
    ir.op = IROpcode.MOV;
    ir.r0 = ir.r1.promoted;
    ir.r1 = null;
    return;
  }
}

void optimize(Program prog) {
  foreach (func; prog.funcs) {
    foreach (bb; func.bbs) {
      foreach (ir; bb.ir) {
        opt(ir);
      }
    }
  }
}
