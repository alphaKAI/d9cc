module d9cc.genir;
import d9cc.parse;
import std.format;

enum IROpcode {
  ADD = 1,
  SUB,
  MUL,
  DIV,
  IMM,
  BPREL,
  MOV,
  RETURN,
  CALL,
  LABEL_ADDR,
  EQ,
  NE,
  LE,
  LT,
  AND,
  OR,
  XOR,
  SHL,
  SHR,
  MOD,
  JMP,
  BR,
  LOAD,
  LOAD_SPILL,
  STORE,
  STORE_ARG,
  STORE_SPILL,
  NOP,
}

private Function fn;
private BB out_;
private int nreg = 1;

class Reg {
  int vn; // virtual register number
  int rn; // real register number

  // For optimizer
  Reg promoted;

  // For regalloc
  int def;
  int last_use;
  bool spill;
  Var var;

  this() {
    this.vn = nreg++;
    this.rn = -1;
  }
}

class IR {
  IROpcode op;

  Reg r0;
  Reg r1;
  Reg r2;

  int imm;
  int label;
  Var var;

  BB bb1;
  BB bb2;

  // Load/store size in bytes
  int size;

  // Function call
  string name;
  size_t nargs;
  Reg[6] args;

  // For liveness tracking
  //Vector *kill;

  // For SSA
  Reg bbarg;

  this(IROpcode op) {
    this.op = op;
    out_.ir ~= this;
  }
}

class BB {
  int label;
  IR[] ir;
  Reg param;

  // For liveness analysis
  BB[] succ;
  BB[] pred;
  Reg[] def_regs;
  Reg[] in_regs;
  Reg[] out_regs;

  this() {
    this.label = d9cc.parse.nlabel++;
    fn.bbs ~= this;
  }
}

private IR emit(IROpcode op, Reg r0, Reg r1, Reg r2) {
  IR ir = new IR(op);
  ir.r0 = r0;
  ir.r1 = r1;
  ir.r2 = r2;
  return ir;
}

private IR br(Reg r, BB then, BB els) {
  IR ir = new IR(IROpcode.BR);
  ir.r2 = r;
  ir.bb1 = then;
  ir.bb2 = els;
  return ir;
}

private IR jmp(BB bb) {
  IR ir = new IR(IROpcode.JMP);
  ir.bb1 = bb;
  return ir;
}

private IR jmp_arg(BB bb, Reg r) {
  IR ir = new IR(IROpcode.JMP);
  ir.bb1 = bb;
  ir.bbarg = r;
  return ir;
}

private Reg imm(int imm) {
  Reg r = new Reg;
  IR ir = new IR(IROpcode.IMM);
  ir.r0 = r;
  ir.imm = imm;
  return r;
}

private Reg gen_expr(Node node);

private void load(Node node, Reg dst, Reg src) {
  IR ir = emit(IROpcode.LOAD, dst, null, src);
  ir.size = node.ty.size;
}

// In C, all expressions that can be written on the left-hand side of
// the '=' operator must have an address in memory. In other words, if
// you can apply the '&' operator to take an address of some
// expression E, you can assign E to a new value.
//
// Other expressions, such as `1+2`, cannot be written on the lhs of
// '=', since they are just temporary values that don't have an address.
//
// The stuff that can be written on the lhs of '=' is called lvalue.
// Other values are called rvalue. An lvalue is essentially an address.
//
// When lvalues appear on the rvalue context, they are converted to
// rvalues by loading their values from their addresses. You can think
// '&' as an operator that suppresses such automatic lvalue-to-rvalue
// conversion.
//
// This function evaluates a given node as an lvalue.
private Reg gen_lval(Node node) {
  if (node.op == ND_DEREF) {
    return gen_expr(node.expr);
  }

  if (node.op == ND_DOT) {
    Reg r1 = new Reg;
    Reg r2 = gen_lval(node.expr);
    Reg r3 = imm(node.ty.offset);
    emit(IROpcode.ADD, r1, r2, r3);
    return r1;
  }

  assert(node.op == ND_VARREF);
  Var var = node.var;

  IR ir;
  if (var.is_local) {
    ir = new IR(IROpcode.BPREL);
    ir.r0 = new Reg;
    ir.var = var;
  } else {
    ir = new IR(IROpcode.LABEL_ADDR);
    ir.r0 = new Reg;
    ir.name = var.name;
  }
  return ir.r0;
}

private Reg gen_binop(IROpcode op, Node node) {
  Reg r1 = new Reg;
  Reg r2 = gen_expr(node.lhs);
  Reg r3 = gen_expr(node.rhs);
  emit(op, r1, r2, r3);
  return r1;
}

private void gen_stmt(Node node);

private Reg gen_expr(Node node) {
  switch (node.op) {
  case ND_NUM:
    return imm(node.val);
  case ND_EQ:
    return gen_binop(IROpcode.EQ, node);
  case ND_NE:
    return gen_binop(IROpcode.NE, node);
  case ND_LOGAND: {
      BB bb = new BB;
      BB set0 = new BB;
      BB set1 = new BB;
      BB last = new BB;

      br(gen_expr(node.lhs), bb, set0);

      out_ = bb;
      br(gen_expr(node.rhs), set1, set0);

      out_ = set0;
      jmp_arg(last, imm(0));

      out_ = set1;
      jmp_arg(last, imm(1));

      out_ = last;
      out_.param = new Reg;
      return out_.param;
    }
  case ND_LOGOR: {
      BB bb = new BB;
      BB set0 = new BB;
      BB set1 = new BB;
      BB last = new BB;

      Reg r1 = gen_expr(node.lhs);
      br(r1, set1, bb);

      out_ = bb;
      Reg r2 = gen_expr(node.rhs);
      br(r2, set1, set0);

      out_ = set0;
      jmp_arg(last, imm(0));

      out_ = set1;
      jmp_arg(last, imm(1));

      out_ = last;
      out_.param = new Reg;
      return out_.param;
    }
  case ND_VARREF:
  case ND_DOT: {
      Reg r = new Reg;
      load(node, r, gen_lval(node));
      return r;
    }
  case ND_CALL: {
      Reg[6] args;
      foreach (i, arg; node.args) {
        args[i] = gen_expr(arg);
      }

      IR ir = new IR(IROpcode.CALL);
      ir.r0 = new Reg;
      ir.name = node.name;
      ir.nargs = node.args.length;
      ir.args = args;
      return ir.r0;
    }
  case ND_ADDR:
    return gen_lval(node.expr);
  case ND_DEREF: {
      Reg r = new Reg;
      load(node, r, gen_expr(node.expr));
      return r;
    }
  case ND_CAST: {
      Reg r1 = gen_expr(node.expr);
      if (node.ty.ty != CType.BOOL) {
        return r1;
      }
      Reg r2 = new Reg;
      emit(IROpcode.NE, r2, r1, imm(0));
      return r2;
    }
  case ND_STMT_EXPR:
    foreach (stmt; node.stmts) {
      gen_stmt(stmt);
    }
    return gen_expr(node.expr);
  case '=': {
      Reg r1 = gen_expr(node.rhs);
      Reg r2 = gen_lval(node.lhs);

      IR ir = emit(IROpcode.STORE, null, r2, r1);
      ir.size = node.ty.size;
      return r1;
    }
  case '+':
    return gen_binop(IROpcode.ADD, node);
  case '-':
    return gen_binop(IROpcode.SUB, node);
  case '*':
    return gen_binop(IROpcode.MUL, node);
  case '/':
    return gen_binop(IROpcode.DIV, node);
  case '%':
    return gen_binop(IROpcode.MOD, node);
  case '<':
    return gen_binop(IROpcode.LT, node);
  case ND_LE:
    return gen_binop(IROpcode.LE, node);
  case '&':
    return gen_binop(IROpcode.AND, node);
  case '|':
    return gen_binop(IROpcode.OR, node);
  case '^':
    return gen_binop(IROpcode.XOR, node);
  case ND_SHL:
    return gen_binop(IROpcode.SHL, node);
  case ND_SHR:
    return gen_binop(IROpcode.SHR, node);
  case '~': {
      Reg r1 = new Reg;
      Reg r2 = gen_expr(node.expr);
      emit(IROpcode.XOR, r1, r2, imm(-1));
      return r1;
    }
  case ',':
    gen_expr(node.lhs);
    return gen_expr(node.rhs);
  case '?': {
      BB then = new BB;
      BB els = new BB;
      BB last = new BB;

      br(gen_expr(node.cond), then, els);

      out_ = then;
      jmp_arg(last, gen_expr(node.then));

      out_ = els;
      jmp_arg(last, gen_expr(node.els));

      out_ = last;
      out_.param = new Reg;
      return out_.param;
    }
  case '!': {
      Reg r1 = new Reg;
      Reg r2 = gen_expr(node.expr);
      emit(IROpcode.EQ, r1, r2, imm(0));
      return r1;
    }
  default:
    assert(0 && "unknown AST type");
  }
}

private void gen_stmt(Node node) {
  switch (node.op) {
  case ND_NULL:
    return;
  case ND_IF: {
      BB then = new BB;
      BB els = new BB;
      BB last = new BB;

      br(gen_expr(node.cond), then, els);

      out_ = then;
      gen_stmt(node.then);
      jmp(last);

      out_ = els;
      if (node.els)
        gen_stmt(node.els);
      jmp(last);

      out_ = last;
      return;
    }
  case ND_FOR: {
      BB cond = new BB;
      node.continue_ = new BB;
      BB body = new BB;
      node.break_ = new BB;

      if (node.init)
        gen_stmt(node.init);
      jmp(cond);

      out_ = cond;
      if (node.cond) {
        Reg r = gen_expr(node.cond);
        br(r, body, node.break_);
      } else {
        jmp(body);
      }

      out_ = body;
      gen_stmt(node.body_);
      jmp(node.continue_);

      out_ = node.continue_;
      if (node.inc)
        gen_expr(node.inc);
      jmp(cond);

      out_ = node.break_;
      return;
    }
  case ND_DO_WHILE: {
      node.continue_ = new BB;
      BB body_ = new BB;
      node.break_ = new BB;

      jmp(body_);

      out_ = body_;
      gen_stmt(node.body_);
      jmp(node.continue_);

      out_ = node.continue_;
      Reg r = gen_expr(node.cond);
      br(r, body_, node.break_);

      out_ = node.break_;
      return;
    }
  case ND_SWITCH: {
      node.break_ = new BB;
      node.continue_ = new BB;

      Reg r = gen_expr(node.cond);
      foreach (case_; node.cases) {
        case_.bb = new BB;

        BB next = new BB;
        Reg r2 = new Reg;
        emit(IROpcode.EQ, r2, r, imm(case_.val));
        br(r2, case_.bb, next);
        out_ = next;
      }
      jmp(node.break_);

      gen_stmt(node.body_);
      jmp(node.break_);

      out_ = node.break_;
      return;
    }
  case ND_CASE:
    jmp(node.bb);
    out_ = node.bb;
    gen_stmt(node.body_);
    break;
  case ND_BREAK:
    jmp(node.target.break_);
    out_ = new BB;
    break;
  case ND_CONTINUE:
    jmp(node.target.continue_);
    out_ = new BB;
    break;
  case ND_RETURN: {
      Reg r = gen_expr(node.expr);
      IR ir = new IR(IROpcode.RETURN);
      ir.r2 = r;
      out_ = new BB;
      return;
    }
  case ND_EXPR_STMT:
    gen_expr(node.expr);
    return;
  case ND_COMP_STMT:
    foreach (stmt; node.stmts) {
      gen_stmt(stmt);
    }
    return;
  default:
    throw new Error("unknown node: %d".format(node.op));
  }
}

private void gen_param(Var var, int i) {
  IR ir = new IR(IROpcode.STORE_ARG);
  ir.var = var;
  ir.imm = i;
  ir.size = var.ty.size;
  var.address_taken = true;
}

void gen_ir(Program prog) {
  foreach (func; prog.funcs) {
    fn = func;
    assert(fn.node.op == ND_FUNC);

    // Add an empty entry BB to make later analysis easy.
    out_ = new BB;
    BB bb = new BB;
    jmp(bb);
    out_ = bb;

    // Emit IR.
    Var[] params = fn.node.params;
    foreach (int i, param; params) {
      gen_param(param, i);
    }

    gen_stmt(fn.node.body_);

    // Make it always ends with a return to make later analysis easy.
    new IR(IROpcode.RETURN).r2 = imm(0);

    // Later passes shouldn't need the AST, so make it explicit.
    fn.node = null;
  }
}
