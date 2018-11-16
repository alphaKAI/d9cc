module d9cc.gen_x86;
import d9cc.genir, d9cc.parse, d9cc.util;
import std.format, std.stdio;
import core.stdc.ctype : isgraph;

// This pass generates x86-64 assembly from IR.

enum regs = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
enum regs8 = ["r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
enum regs32 = ["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];

enum num_regs = regs.length;

enum argregs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
enum argregs8 = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
enum argregs32 = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];

private void emit(string str) {
  writeln("\t", str);
}

private void emit_cmp(string insn, IR ir) {
  int r0 = ir.r0.rn;
  int r1 = ir.r1.rn;
  int r2 = ir.r2.rn;

  emit("cmp %s, %s".format(regs[r1], regs[r2]));
  emit("%s %s".format(insn, regs8[r0]));
  emit("movzb %s, %s".format(regs[r0], regs8[r0]));
}

private string reg(int r, int size) {
  if (size == 1) {
    return regs8[r];
  }
  if (size == 4) {
    return regs32[r];
  }
  assert(size == 8);
  return regs[r];
}

private string argreg(int r, int size) {
  if (size == 1) {
    return argregs8[r];
  }
  if (size == 4) {
    return argregs32[r];
  }
  assert(size == 8);
  return argregs[r];
}

private void emit_ir(IR ir, string ret) {
  int r0 = ir.r0 ? ir.r0.rn : 0;
  int r1 = ir.r1 ? ir.r1.rn : 0;
  int r2 = ir.r2 ? ir.r2.rn : 0;

  switch (ir.op) {
  case IROpcode.IMM:
    emit("mov %s, %d".format(regs[r0], ir.imm));
    break;
  case IROpcode.BPREL:
    emit("lea %s, [rbp%d]".format(regs[r0], ir.var.offset));
    break;
  case IROpcode.MOV:
    emit("mov %s, %s".format(regs[r0], regs[r2]));
    break;
  case IROpcode.RETURN:
    emit("mov rax, %s".format(regs[r2]));
    emit("jmp %s".format(ret));
    break;
  case IROpcode.CALL:
    foreach (i; 0 .. ir.nargs) {
      emit("mov %s, %s".format(argregs[i], regs[ir.args[i].rn]));
    }

    emit("push r10");
    emit("push r11");
    emit("mov rax, 0");
    emit("call %s".format(ir.name));
    emit("pop r11");
    emit("pop r10");
    emit("mov %s, rax".format(regs[r0]));
    break;
  case IROpcode.LABEL_ADDR:
    emit("lea %s, %s".format(regs[r0], ir.name));
    break;
  case IROpcode.EQ:
    emit_cmp("sete", ir);
    break;
  case IROpcode.NE:
    emit_cmp("setne", ir);
    break;
  case IROpcode.LT:
    emit_cmp("setl", ir);
    break;
  case IROpcode.LE:
    emit_cmp("setle", ir);
    break;
  case IROpcode.AND:
    emit("and %s, %s".format(regs[r0], regs[r2]));
    break;
  case IROpcode.OR:
    emit("or %s, %s".format(regs[r0], regs[r2]));
    break;
  case IROpcode.XOR:
    emit("xor %s, %s".format(regs[r0], regs[r2]));
    break;
  case IROpcode.SHL:
    emit("mov cl, %s".format(regs8[r2]));
    emit("shl %s, cl".format(regs[r0]));
    break;
  case IROpcode.SHR:
    emit("mov cl, %s".format(regs8[r2]));
    emit("shr %s, cl".format(regs[r0]));
    break;
  case IROpcode.JMP:
    if (ir.bbarg) {
      emit("mov %s, %s".format(regs[ir.bb1.param.rn], regs[ir.bbarg.rn]));
    }
    emit("jmp .L%d".format(ir.bb1.label));
    break;
  case IROpcode.BR:
    emit("cmp %s, 0".format(regs[r2]));
    emit("jne .L%d".format(ir.bb1.label));
    emit("jmp .L%d".format(ir.bb2.label));
    break;
  case IROpcode.LOAD:
    emit("mov %s, [%s]".format(reg(r0, ir.size), regs[r2]));
    if (ir.size == 1) {
      emit("movzb %s, %s".format(regs[r0], regs8[r0]));
    }
    break;
  case IROpcode.LOAD_SPILL:
    emit("mov %s, [rbp%d]".format(regs[r0], ir.var.offset));
    break;
  case IROpcode.STORE:
    emit("mov [%s], %s".format(regs[r1], reg(r2, ir.size)));
    break;
  case IROpcode.STORE_ARG:
    emit("mov [rbp%d], %s".format(ir.var.offset, argreg(ir.imm, ir.size)));
    break;
  case IROpcode.STORE_SPILL:
    emit("mov [rbp%d], %s".format(ir.var.offset, regs[r1]));
    break;
  case IROpcode.ADD:
    emit("add %s, %s".format(regs[r0], regs[r2]));
    break;
  case IROpcode.SUB:
    emit("sub %s, %s".format(regs[r0], regs[r2]));
    break;
  case IROpcode.MUL:
    emit("mov rax, %s".format(regs[r2]));
    emit("imul %s".format(regs[r0]));
    emit("mov %s, rax".format(regs[r0]));
    break;
  case IROpcode.DIV:
    emit("mov rax, %s".format(regs[r0]));
    emit("cqo");
    emit("idiv %s".format(regs[r2]));
    emit("mov %s, rax".format(regs[r0]));
    break;
  case IROpcode.MOD:
    emit("mov rax, %s".format(regs[r0]));
    emit("cqo");
    emit("idiv %s".format(regs[r2]));
    emit("mov %s, rdx".format(regs[r0]));
    break;
  case IROpcode.NOP:
    break;
  default:
    assert(0 && "unknown operator");
  }
}

void emit_code(Function fn) {
  // Assign an offset from RBP to each local variable.
  int off;
  foreach (var; fn.lvars) {
    off += var.ty.size;
    off = roundup(off, var.ty._align);
    var.offset = -off;
  }

  // Emit assembly
  string ret = ".Lend%d".format(nlabel++);

  writeln(".text");
  writefln(".global %s", fn.name);
  writefln("%s:", fn.name);
  emit("push rbp");
  emit("mov rbp, rsp");
  emit("sub rsp, %d".format(roundup(off, 16)));
  emit("push r12");
  emit("push r13");
  emit("push r14");
  emit("push r15");

  foreach (bb; fn.bbs) {
    writefln(".L%d:", bb.label);
    foreach (ir; bb.ir) {
      emit_ir(ir, ret);
    }
  }

  writefln("%s:", ret);
  emit("pop r15");
  emit("pop r14");
  emit("pop r13");
  emit("pop r12");
  emit("mov rsp, rbp");
  emit("pop rbp");
  emit("ret");
}

private string backslash_escape(string s, int len) {
  static char[256] escaped = 0;
  escaped['\b'] = 'b';
  escaped['\f'] = 'f';
  escaped['\n'] = 'n';
  escaped['\r'] = 'r';
  escaped['\t'] = 't';
  escaped['\\'] = '\\';
  escaped['\''] = '\'';
  escaped['"'] = '"';

  string sb;
  foreach (ubyte c; s[0 .. len]) {
    char esc = escaped[c];
    if (esc != 0) {
      sb ~= '\\';
      sb ~= esc;
    } else if (isgraph(c) || c == ' ') {
      sb ~= c;
    } else {
      sb ~= format("\\%03o", c);
    }
  }
  return sb;
}

private void emit_data(Var var) {
  if (var.data !is null) {
    writeln(".data");
    writefln("%s:", var.name);
    emit(".ascii \"%s\"".format(backslash_escape(var.data, var.ty.size)));
    return;
  }

  writeln(".bss");
  writefln("%s:", var.name);
  emit(".zero %d".format(var.ty.size));
}

void gen_x86(Program prog) {
  writeln(".intel_syntax noprefix");

  foreach (gvar; prog.gvars) {
    emit_data(gvar);
  }

  foreach (func; prog.funcs) {
    emit_code(func);
  }
}
