module d9cc.regalloc;
import d9cc.genir, d9cc.parse, d9cc.gen_x86, d9cc.util;

// Linear scan register allocator.
//
// Before this pass, it is assumed that we have infinite number of
// registers. This pass maps them to finite number of registers.
// Here is the algorithm:
//
// First, we find the definition and the last use for each register.
// A register is considered "live" in the range. At the definition of
// some register R, if all physical registers are already allocated,
// one of them (including R itself) needs to be spilled to the stack.
// As long as one register is spilled, the algorithm is logically
// correct. As a heuristic, we spill a register whose last use is
// furthest.
//
// We then insert load and store instructions for spilled registesr.
// The last register (num_regs-1'th register) is reserved for that
// purpose.

// Rewrite `A = B op C` to `A = B; A = A op C`.
private void three_to_two(BB bb) {
  IR[] v;

  foreach (ir; bb.ir) {

    if (ir.r0 is null || ir.r1 is null) {
      v ~= ir;
      continue;
    }

    assert(ir.r0 != ir.r1);

    IR ir2 = new IR(IROpcode.MOV);
    ir2.r0 = ir.r0;
    ir2.r2 = ir.r1;
    v ~= ir2;

    ir.r1 = ir.r0;
    v ~= ir;
  }
  bb.ir = v;
}

private void set_last_use(Reg r, int ic) {
  if (r && r.last_use < ic) {
    r.last_use = ic;
  }
}

private Reg[] collect_regs(Function fn) {
  Reg[] v;
  int ic = 1; // instruction counter

  foreach (bb; fn.bbs) {

    if (bb.param !is null) {
      bb.param.def = ic;
      v ~= bb.param;
    }

    foreach (ir; bb.ir) {

      if (ir.r0 !is null && !ir.r0.def) {
        ir.r0.def = ic;
        v ~= ir.r0;
      }

      set_last_use(ir.r1, ic);
      set_last_use(ir.r2, ic);
      set_last_use(ir.bbarg, ic);

      if (ir.op == IROpcode.CALL) {
        foreach (i; 0 .. ir.nargs) {
          set_last_use(ir.args[i], ic);
        }
      }
    }

    foreach (r; bb.out_regs) {
      set_last_use(r, ic);
    }
  }

  return v;
}

private int choose_to_spill(Reg[] used) {
  int k;
  foreach (int i; 1 .. num_regs) {
    if (used[k].last_use < used[i].last_use) {
      k = i;
    }
  }
  return k;
}

// Allocate registers.
private void scan(Reg[] regs) {
  Reg[num_regs] used;

  foreach (r; regs) {
    // Find an unused slot.
    bool found;
    foreach (int i; 0 .. num_regs) {
      if (used[i]!is null && r.def < used[i].last_use) {
        continue;
      }
      r.rn = i;
      used[i] = r;
      found = true;
      break;
    }

    if (found) {
      continue;
    }

    // Choose a register to spill and mark it as "spilled".
    used[num_regs - 1] = r;
    int k = choose_to_spill(used);

    r.rn = k;
    used[k].rn = num_regs - 1;
    used[k].spill = true;
    used[k] = r;
  }
}

private void spill_store(IR[] v, IR ir) {
  Reg r = ir.r0;
  if (r is null || !r.spill) {
    return;
  }

  IR ir2 = new IR(IROpcode.STORE_SPILL);
  ir2.r1 = r;
  ir2.var = r.var;
  v ~= ir2;
}

private void spill_load(IR[] v, IR ir, Reg r) {
  if (r is null || !r.spill) {
    return;
  }

  IR ir2 = new IR(IROpcode.LOAD_SPILL);
  ir2.r0 = r;
  ir2.var = r.var;
  v ~= ir2;
}

private void emit_spill_code(BB bb) {
  IR[] v;

  foreach (ir; bb.ir) {
    spill_load(v, ir, ir.r1);
    spill_load(v, ir, ir.r2);
    spill_load(v, ir, ir.bbarg);
    v ~= ir;
    spill_store(v, ir);
  }
  bb.ir = v;
}

void alloc_regs(Program prog) {
  foreach (fn; prog.funcs) {
    // Convert SSA to x86-ish two-address form.
    foreach (bb; fn.bbs) {
      three_to_two(bb);
    }

    // Allocate registers and decide which registers to spill.
    Reg[] regs = collect_regs(fn);
    scan(regs);

    // Reserve a stack area for spilled registers.
    foreach (r; regs) {
      if (!r.spill) {
        continue;
      }

      Var var = new Var;
      var.ty = ptr_to(int_ty());
      var.is_local = true;
      var.name = "spill";

      r.var = var;
      fn.lvars ~= var;
    }

    // Convert accesses to spilled registers to loads and stores.
    foreach (bb; fn.bbs) {
      emit_spill_code(bb);
    }
  }
}
