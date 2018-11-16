module d9cc.sema;
import d9cc.token, d9cc.parse, d9cc.util;
import std.format;

// Semantics analyzer. This pass plays a few important roles as shown
// below:
//
// - Add types to nodes. For example, a tree that represents "1+2" is
//   typed as INT because the result type of an addition of two
//   integers is integer.
//
// - Insert nodes to make array-to-pointer conversion explicit.
//   Recall that, in C, "array of T" is automatically converted to
//   "pointer to T" in most contexts.
//
// - Insert nodes for implicit cast so that they are explicitly
//   represented in AST.
//
// - Scales operands for pointer arithmetic. E.g. ptr+1 becomes ptr+4
//   for integer and becomes ptr+8 for pointer.
//
// - Reject bad assignments, such as `1=2+3`.

private Node maybe_decay(Node base, bool decay) {
  if (!decay || base.ty.ty != CType.ARY) {
    return base;
  }

  Node node = new Node(ND_ADDR);
  node.ty = ptr_to(base.ty.ary_of);
  node.expr = base;
  node.token = base.token;
  return node;
}

private void bad_node(Node node, string msg) {
  bad_token(node.token, msg);
}

private void check_lval(Node node) {
  int op = node.op;
  if (op != ND_VARREF && op != ND_DEREF && op != ND_DOT) {
    bad_node(node, "not an lvalue");
  }
}

private Node scale_ptr(int op, Node base, Type ty) {
  Node node = new Node(op);
  node.lhs = base;
  node.rhs = new_int_node(ty.ptr_to.size, base.token);
  node.token = base.token;
  return node;
}

private Node _cast(Node base, Type ty) {
  Node node = new Node(ND_CAST);
  node.ty = ty;
  node.expr = base;
  node.token = base.token;
  return node;
}

private void check_int(Node node) {
  CType t = node.ty.ty;
  if (t != CType.INT && t != CType.CHAR && t != CType.BOOL) {
    bad_node(node, "not an integer");
  }
}

private Node do_walk(Node node, bool decay);

private Node walk(Node node) {
  return do_walk(node, true);
}

private Node walk_nodecay(Node node) {
  return do_walk(node, false);
}

private Node do_walk(Node node, bool decay) {
  switch (node.op) {
  case ND_NUM:
  case ND_NULL:
  case ND_BREAK:
  case ND_CONTINUE:
    return node;
  case ND_VARREF:
    return maybe_decay(node, decay);
  case ND_IF:
    node.cond = walk(node.cond);
    node.then = walk(node.then);
    if (node.els !is null) {
      node.els = walk(node.els);
    }
    return node;
  case ND_FOR:
    if (node.init !is null) {
      node.init_ = walk(node.init);
    }
    if (node.cond !is null) {
      node.cond = walk(node.cond);
    }
    if (node.inc !is null) {
      node.inc = walk(node.inc);
    }
    node.body_ = walk(node.body_);
    return node;
  case ND_DO_WHILE:
  case ND_SWITCH:
    node.cond = walk(node.cond);
    node.body_ = walk(node.body_);
    return node;
  case ND_CASE:
    node.body_ = walk(node.body_);
    return node;
  case '+':
    node.lhs = walk(node.lhs);
    node.rhs = walk(node.rhs);

    if (node.rhs.ty.ty == CType.PTR) {
      Node n = node.lhs;
      node.lhs = node.rhs;
      node.rhs = n;
    }
    check_int(node.rhs);

    if (node.lhs.ty.ty == CType.PTR) {
      node.rhs = scale_ptr('*', node.rhs, node.lhs.ty);
      node.ty = node.lhs.ty;
    } else {
      node.ty = int_ty();
    }
    return node;
  case '-': {
      node.lhs = walk(node.lhs);
      node.rhs = walk(node.rhs);

      Type lty = node.lhs.ty;
      Type rty = node.rhs.ty;

      if (lty.ty == CType.PTR && rty.ty == CType.PTR) {
        if (!same_type(rty, lty)) {
          bad_node(node, "incompatible pointer");
        }
        node = scale_ptr('/', node, lty);
        node.ty = lty;
      } else {
        node.ty = int_ty();
      }
      return node;
    }
  case '=':
    node.lhs = walk_nodecay(node.lhs);
    check_lval(node.lhs);
    node.rhs = walk(node.rhs);
    if (node.lhs.ty.ty == CType.BOOL) {
      node.rhs = _cast(node.rhs, bool_ty());
    }
    node.ty = node.lhs.ty;
    return node;
  case ND_DOT: {
      node.expr = walk(node.expr);
      if (node.expr.ty.ty != CType.STRUCT) {
        bad_node(node, "struct expected before '.'");
      }

      Type ty = node.expr.ty;
      if (ty.members is null) {
        bad_node(node, "incomplete type");
      }

      node.ty = null;
      if (node.name in ty.members) {
        node.ty = ty.members[node.name];
      }
      if (node.ty is null) {
        bad_node(node, format("member missing: %s", node.name));
      }
      return maybe_decay(node, decay);
    }
  case '?':
    node.cond = walk(node.cond);
    node.then = walk(node.then);
    node.els = walk(node.els);
    node.ty = node.then.ty;
    return node;
  case '*':
  case '/':
  case '%':
  case '<':
  case '|':
  case '^':
  case '&':
  case ND_EQ:
  case ND_NE:
  case ND_LE:
  case ND_SHL:
  case ND_SHR:
  case ND_LOGAND:
  case ND_LOGOR:
    node.lhs = walk(node.lhs);
    node.rhs = walk(node.rhs);
    check_int(node.lhs);
    check_int(node.rhs);
    node.ty = int_ty();
    return node;
  case ',':
    node.lhs = walk(node.lhs);
    node.rhs = walk(node.rhs);
    node.ty = node.rhs.ty;
    return node;
  case '!':
  case '~':
    node.expr = walk(node.expr);
    check_int(node.expr);
    node.ty = int_ty();
    return node;
  case ND_ADDR:
    node.expr = walk(node.expr);
    check_lval(node.expr);
    node.ty = ptr_to(node.expr.ty);
    if (node.expr.op == ND_VARREF) {
      node.expr.var.address_taken = true;
    }
    return node;
  case ND_DEREF:
    node.expr = walk(node.expr);

    if (node.expr.ty.ty != CType.PTR) {
      bad_node(node, "operand must be a pointer");
    }

    if (node.expr.ty.ptr_to.ty == CType.VOID) {
      bad_node(node, "cannot dereference void pointer");
    }

    node.ty = node.expr.ty.ptr_to;
    return maybe_decay(node, decay);
  case ND_RETURN:
  case ND_EXPR_STMT:
    node.expr = walk(node.expr);
    return node;
  case ND_CALL:
    foreach (i, arg; node.args) {
      node.args[i] = walk(arg);
    }
    node.ty = node.ty.returning;
    return node;
  case ND_COMP_STMT: {
      foreach (i, stmt; node.stmts) {
        node.stmts[i] = walk(stmt);
      }
      return node;
    }
  case ND_STMT_EXPR: {
      foreach (i, stmt; node.stmts) {
        node.stmts[i] = walk(stmt);
      }
      node.expr = walk(node.expr);
      node.ty = node.expr.ty;
      return node;
    }
  default:
    assert(0 && "unknown node type");
  }
}

Type get_type(Node node) {
  return walk_nodecay(node).ty;
}

void sema(Program prog) {
  foreach (func; prog.funcs) {
    Node node = func.node;
    assert(node.op == ND_FUNC);
    node.body_ = walk(node.body_);
  }
}
