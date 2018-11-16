module d9cc.irdump;
import d9cc.genir, d9cc.parse;
import std.format, std.stdio;

private int regno(Reg r) {
  if (r is null) {
    return 0;
  }
  if (r.rn != -1) {
    return r.rn;
  }
  return r.vn;
}

private string tostr_call(IR ir) {
  string sb;
  sb ~= "r%d = %s(".format(regno(ir.r0), ir.name);

  foreach (i; 0 .. ir.nargs) {
    if (i != 0) {
      sb ~= ", ";
    }
    sb ~= "r%d".format(regno(ir.args[i]));
  }
  sb ~= ")";
  return sb;
}

private string tostr(IR ir) {
  int r0 = regno(ir.r0);
  int r1 = regno(ir.r1);
  int r2 = regno(ir.r2);

  switch (ir.op) {
  case IROpcode.ADD:
    return "r%d = r%d + r%d".format(r0, r1, r2);
  case IROpcode.CALL:
    return tostr_call(ir);
  case IROpcode.DIV:
    return "r%d = r%d / r%d".format(r0, r1, r2);
  case IROpcode.IMM:
    return "r%d = %d".format(r0, ir.imm);
  case IROpcode.JMP:
    if (ir.bbarg) {
      return "JMP .L%d (r%d)".format(ir.bb1.label, regno(ir.bbarg));
    }
    return "JMP .L%d".format(ir.bb1.label);
  case IROpcode.LABEL_ADDR:
    return "r%d = .L%d".format(r0, ir.label);
  case IROpcode.EQ:
    return "r%d = r%d == r%d".format(r0, r1, r2);
  case IROpcode.NE:
    return "r%d = r%d != r%d".format(r0, r1, r2);
  case IROpcode.LE:
    return "r%d = r%d <= r%d".format(r0, r1, r2);
  case IROpcode.LT:
    return "r%d = r%d < r%d".format(r0, r1, r2);
  case IROpcode.AND:
    return "r%d = r%d & r%d".format(r0, r1, r2);
  case IROpcode.OR:
    return "r%d = r%d | r%d".format(r0, r1, r2);
  case IROpcode.XOR:
    return "r%d = r%d ^ r%d".format(r0, r1, r2);
  case IROpcode.SHL:
    return "r%d = r%d << r%d".format(r0, r1, r2);
  case IROpcode.SHR:
    return "r%d = r%d >> r%d".format(r0, r1, r2);
  case IROpcode.LOAD:
    return "LOAD%d r%d, r%d".format(ir.size, r0, r2);
  case IROpcode.LOAD_SPILL:
    return "LOAD_SPILL r%d, %d".format(r0, ir.imm);
  case IROpcode.MOD:
    return "r%d = r%d %% r%d".format(r0, r1, r2);
  case IROpcode.MOV:
    return "r%d = r%d".format(r0, r2);
  case IROpcode.MUL:
    return "r%d = r%d * r%d".format(r0, r1, r2);
  case IROpcode.NOP:
    return "NOP";
  case IROpcode.RETURN:
    return "RET r%d".format(r2);
  case IROpcode.STORE:
    return "STORE%d r%d, r%d".format(ir.size, r1, r2);
  case IROpcode.STORE_ARG:
    return "STORE_ARG%d %d %s (%d)".format(ir.size,
        ir.imm, ir.var.name, ir.var.offset);
  case IROpcode.STORE_SPILL:
    return "STORE_SPILL r%d, %d".format(r1, ir.imm);
  case IROpcode.SUB:
    return "r%d = r%d - r%d".format(r0, r1, r2);
  case IROpcode.BPREL:
    return "BPREL r%d %s (%d)".format(r0, ir.var.name, ir.var.offset);
  case IROpcode.BR:
    return "BR r%d .L%d .L%d".format(r2, ir.bb1.label, ir.bb2.label);
  default:
    assert(0 && "unknown op");
  }
}

private void print_rel(string name, BB[] v) {
  if (v.length == 0) {
    return;
  }
  stderr.writef(" %s=", name);
  foreach (i, bb; v) {
    if (i > 0) {
      stderr.writef(",");
    }
    stderr.writef(".L%d", bb.label);
  }
}

private void print_regs(string name, Reg[] v) {
  if (v.length == 0) {
    return;
  }
  stderr.writef(" %s=", name);
  foreach (i, r; v) {
    if (i > 0) {
      stderr.writef(",");
    }
    stderr.writef("r%d", regno(r));
  }
}

private void print_bb(BB bb) {
  if (bb.param) {
    stderr.writef(".L%d(r%d)", bb.label, regno(bb.param));
  } else {
    stderr.writef(".L%d", bb.label);
  }

  print_rel("pred", bb.pred);
  print_rel("succ", bb.succ);
  print_regs("defs", bb.def_regs);
  print_regs("in", bb.in_regs);
  print_regs("out", bb.out_regs);
  stderr.writef("\n");
}

void dump_ir(Function[] irv) {
  foreach (fn; irv) {
    stderr.writef("%s:\n", fn.name);

    foreach (bb; fn.bbs) {
      print_bb(bb);

      foreach (ir; bb.ir) {
        stderr.writef("\t%s\n", tostr(ir));
      }
    }
  }
}
