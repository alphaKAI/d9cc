module d9cc.liveness;
import d9cc.genir, d9cc.parse, d9cc.util;
import std.algorithm;

// Liveness analysis.

// Fill bb.succ and bb.pred.
private void add_edges(BB bb) {
  if (bb.succ.length > 0) {
    return;
  }
  assert(bb.ir.length);

  IR ir = bb.ir[$ - 1];

  if (ir.bb1 !is null) {
    bb.succ ~= ir.bb1;
    ir.bb1.pred ~= bb;
    add_edges(ir.bb1);
  }

  if (ir.bb2 !is null) {
    bb.succ ~= ir.bb2;
    ir.bb2.pred ~= bb;
    add_edges(ir.bb2);
  }
}

// Initializes bb.def_regs.
private void set_def_regs(BB bb) {
  if (bb.param) {
    vec_union1(bb.def_regs, bb.param);
  }

  foreach (ir; bb.ir) {
    if (ir.r0) {
      vec_union1(bb.def_regs, ir.r0);
    }
  }
}

// Back-propagate r in the call flow graph.
private void propagate(BB bb, Reg r) {
  if (r is null || vec_contains(bb.def_regs, r)) {
    return;
  }

  if (!vec_union1(bb.in_regs, r)) {
    return;
  }

  foreach (pred; bb.pred) {
    if (vec_union1(pred.out_regs, r)) {
      propagate(pred, r);
    }
  }
}

// Initializes bb.in_regs and bb.out_regs.
private void visit(BB bb, IR ir) {
  propagate(bb, ir.r1);
  propagate(bb, ir.r2);
  propagate(bb, ir.bbarg);

  if (ir.op == IROpcode.CALL) {
    foreach (i; 0 .. ir.nargs) {
      propagate(bb, ir.args[i]);
    }
  }
}

void liveness(Program prog) {
  foreach (fn; prog.funcs) {
    add_edges(fn.bbs[0]);

    foreach (bb; fn.bbs) {
      set_def_regs(bb);

      foreach (ir; bb.ir) {
        visit(bb, ir);
      }
    }

    // Incoming registers of the entry BB correspond to
    // uninitialized variables in a program.
    // Add dummy definitions to make later analysis easy.
    BB ent = fn.bbs[0];
    foreach (r; ent.in_regs) {
      IR ir = new IR(IROpcode.MOV);
      ir.r0 = r;
      ir.imm = 0;
      ent.ir ~= ir;
      ent.def_regs ~= r;
    }
    ent.in_regs = [];
  }
}
