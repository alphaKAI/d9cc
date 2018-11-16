module d9cc.parse;
import d9cc.genir, d9cc.util, d9cc.token, d9cc.sema;
import core.stdc.ctype : isprint;
import std.format, std.conv;

// This is a recursive-descendent parser which constructs abstract
// syntax tree from input tokens.
//
// Variable names are resolved at this stage. We create a Var object
// when we see a variable definition and use it when we see a variable
// reference.
//
// Types are added to variables and literals. For other nodes, Sema
// will add type for them.
//
// Semantic checking is omitted from this parser to make the code in
// this file closely resemble the C's BNF. Invalid expressions, such
// as `1+2=3`, are accepted at this stage. Such errors are detected in
// a later pass.

enum {
  ND_NUM = 256, // Number literal
  ND_STRUCT, // Struct
  ND_DECL, // declaration
  ND_VARDEF, // Variable definition
  ND_VARREF, // Variable reference
  ND_CAST, // Cast
  ND_IF, // "if"
  ND_FOR, // "for"
  ND_DO_WHILE, // do ... while
  ND_SWITCH, // switch
  ND_CASE, // case
  ND_BREAK, // break
  ND_CONTINUE, // continue
  ND_ADDR, // address-of operator ("&")
  ND_DEREF, // pointer dereference ("*")
  ND_DOT, // Struct member access
  ND_EQ, // ==
  ND_NE, // !=
  ND_LE, // <=
  ND_LOGAND, // &&
  ND_LOGOR, // ||
  ND_SHL, // <<
  ND_SHR, // >>
  ND_MOD, // %
  ND_RETURN, // "return"
  ND_CALL, // Function call
  ND_FUNC, // Function definition
  ND_COMP_STMT, // Compound statement
  ND_EXPR_STMT, // Expression statement
  ND_STMT_EXPR, // Statement expression (GNU extn.)
  ND_NULL, // Null statement
}

enum CType {
  VOID = 1,
  BOOL,
  CHAR,
  INT,
  PTR,
  ARY,
  STRUCT,
  FUNC,
}

// Represents a variable.
class Var {
  Type ty;
  string name;
  bool is_local;

  // Local variables are compiled to offsets from RBP.
  int offset;

  // Global variables are compiled to labels with optional
  // initialized data.
  string data;

  // For optimization passes.
  bool address_taken;
  Reg promoted;
}

// AST node
class Node {
  int op; // Node type
  Type ty; // C type
  Node lhs; // left-hand side
  Node rhs; // right-hand side
  int val; // Number literal
  Node expr; // "return" or expresson stmt
  Node[] stmts; // Compound statement

  string name;

  // For ND_VARREF
  Var var;

  // "if" ( cond ) then "else" els
  // "for" ( init; cond; inc ) body
  // "while" ( cond ) body
  // "do" body "while" ( cond )
  // "switch" ( cond ) body
  // "case" val ":" body
  Node cond;
  Node then;
  Node els;
  Node init_;
  Node inc;
  Node body_;

  // For switch and case
  Node[] cases;
  BB bb;

  // For case, break and continue
  Node target;
  BB break_;
  BB continue_;

  // Function definition
  Var[] params;

  // Function call
  Node[] args;

  // For error reporting
  Token token;

  this() {
  }

  this(int op) {
    this.op = op;
  }
}

class Function {
  string name;
  Node node;
  Var[] lvars;
  BB[] bbs;
}

// Represents toplevel constructs.
class Program {
  Var[] gvars;
  Function[] funcs;
}

int nlabel = 1;

private class Env {
  Var[string] vars;
  Type[string] typedefs;
  Type[string] tags;
  Env prev;

  this(Env env = null) {
    this.prev = env;
  }
}

private Program prog;
private Var[] lvars;
private Node[] breaks;
private Node[] continues;
private Node[] switches;

private Token[] tokens;
private int pos;
private Env env;

private Node null_stmt;

static this() {
  null_stmt = new Node(ND_NULL);
}

private Var find_var(string name) {
  for (Env e = env; e !is null; e = e.prev) {
    Var var;
    if (name in e.vars) {
      var = e.vars[name];
    }
    if (var !is null) {
      return var;
    }
  }
  return null;
}

private Type find_typedef(string name) {
  for (Env e = env; e !is null; e = e.prev) {
    Type ty;
    if (name in e.typedefs) {
      ty = e.typedefs[name];
    }
    if (ty !is null) {
      return ty;
    }
  }
  return null;
}

private Type find_tag(string name) {
  for (Env e = env; e !is null; e = e.prev) {
    Type ty;
    if (name in e.tags) {
      ty = e.tags[name];
    }
    if (ty !is null) {
      return ty;
    }
  }
  return null;
}

private Var add_lvar(Type ty, string name) {
  Var var = new Var;
  var.ty = ty;
  var.is_local = true;
  var.name = name;
  env.vars[name] = var;
  lvars ~= var;
  return var;
}

private Var add_gvar(Type ty, string name, string data, bool is_extern) {
  Var var = new Var;
  var.ty = ty;
  var.is_local = false;
  var.name = name;
  var.data = data;
  env.vars[name] = var;
  if (!is_extern) {
    prog.gvars ~= var;
  }
  return var;
}

private void expect(int ty) {
  Token token = tokens[pos];
  if (token.ty == ty) {
    pos++;
    return;
  }

  if (isprint(ty)) {
    bad_token(token, format("%c expected", ty.to!char));
  }
  assert(ty == TK_WHILE);
  bad_token(token, "'while' expected");
}

private bool consume(int ty) {
  Token token = tokens[pos];
  if (token.ty != ty) {
    return false;
  }
  pos++;
  return true;
}

private bool is_typename() {
  Token token = tokens[pos];
  if (token.ty == TK_IDENT) {
    return find_typedef(token.name) !is null;
  }
  return token.ty == TK_INT || token.ty == TK_CHAR || token.ty == TK_VOID
    || token.ty == TK_STRUCT || token.ty == TK_TYPEOF || token.ty == TK_BOOL;
}

private void fix_struct_offsets(Type ty) {
  Type[] types = ty.members.values;

  int off = 0;
  foreach (type; types) {
    off = roundup(off, type._align);
    type.offset = off;
    off += type.size;

    if (ty._align < type._align) {
      ty._align = type._align;
    }
  }
  ty.size = roundup(off, ty._align);
}

private Type decl_specifiers() {
  Token t = tokens[pos++];

  if (t.ty == TK_IDENT) {
    Type ty = find_typedef(t.name);
    if (ty is null) {
      pos--;
    }
    return ty;
  }

  if (t.ty == TK_VOID) {
    return void_ty();
  }
  if (t.ty == TK_BOOL) {
    return bool_ty();
  }
  if (t.ty == TK_CHAR) {
    return char_ty();
  }
  if (t.ty == TK_INT) {
    return int_ty();
  }

  if (t.ty == TK_TYPEOF) {
    expect('(');
    Node node = expr();
    expect(')');
    return get_type(node);
  }

  if (t.ty == TK_STRUCT) {
    Token token = tokens[pos];
    Type ty = null;
    string tag = null;

    if (token.ty == TK_IDENT) {
      pos++;
      tag = token.name;
      ty = find_tag(tag);
    }

    if (ty is null) {
      ty = new Type(CType.STRUCT, 0);
    }

    if (consume('{')) {
      ty.members = typeof(ty.members).init;
      while (!consume('}')) {
        Node node = declaration_type();
        ty.members[node.name] = node.ty;
      }
      fix_struct_offsets(ty);
    }

    if (tag is null && ty.members is null) {
      bad_token(token, "bad struct definition");
    }
    if (tag !is null) {
      env.tags[tag] = ty;
    }
    return ty;
  }

  bad_token(t, "typename expected");
  return null;
}

private Node new_node(int op, Token t) {
  Node node = new Node(op);
  node.token = t;
  return node;
}

private Node new_binop(int op, Token t, Node lhs, Node rhs) {
  Node node = new_node(op, t);
  node.lhs = lhs;
  node.rhs = rhs;
  return node;
}

private Node new_expr(int op, Token t, Node expr) {
  Node node = new_node(op, t);
  node.expr = expr;
  return node;
}

private Node new_varref(Token t, Var var) {
  Node node = new_node(ND_VARREF, t);
  node.ty = var.ty;
  node.var = var;
  return node;
}

private Node new_deref(Token t, Var var) {
  return new_expr(ND_DEREF, t, new_varref(t, var));
}

Node new_int_node(int val, Token t) {
  Node node = new_node(ND_NUM, t);
  node.ty = int_ty();
  node.val = val;
  return node;
}

private string ident() {
  Token t = tokens[pos++];
  if (t.ty != TK_IDENT) {
    bad_token(t, "identifier expected");
  }
  return t.name;
}

private Node string_literal(Token t) {
  Type ty = ary_of(char_ty(), t.str.length.to!int);
  string name = format(".L.str%d", nlabel++);

  Node node = new_node(ND_VARREF, t);
  node.ty = ty;
  node.var = add_gvar(ty, name, t.str, false);
  return node;
}

private Node local_variable(Token t) {
  Var var = find_var(t.name);
  if (var is null) {
    bad_token(t, "undefined variable");
  }
  Node node = new_node(ND_VARREF, t);
  node.ty = var.ty;
  node.name = t.name;
  node.var = var;
  return node;
}

private Node function_call(Token t) {
  Var var = find_var(t.name);

  Node node = new_node(ND_CALL, t);
  node.name = t.name;
  node.args = [];

  if (var !is null && var.ty.ty == CType.FUNC) {
    node.ty = var.ty;
  } else {
    warn_token(t, "undefined function");
    node.ty = func_ty(int_ty());
  }

  while (!consume(')')) {
    if (node.args.length > 0) {
      expect(',');
    }
    node.args ~= assign();
  }
  return node;
}

private Node stmt_expr() {
  Token t = tokens[pos];
  Node[] v;

  env = new Env(env);
  do {
    v ~= stmt();
  }
  while (!consume('}'));
  expect(')');
  env = env.prev;

  Node last = vec_pop(v);
  if (last.op != ND_EXPR_STMT) {
    bad_token(last.token, "statement expression returning void");
  }

  Node node = new_node(ND_STMT_EXPR, t);
  node.stmts = v;
  node.expr = last.expr;
  return node;
}

private Node primary() {
  Token t = tokens[pos++];

  if (t.ty == '(') {
    if (consume('{')) {
      return stmt_expr();
    }
    Node node = expr();
    expect(')');
    return node;
  }

  if (t.ty == TK_NUM) {
    return new_int_node(t.val, t);
  }

  if (t.ty == TK_STR) {
    return string_literal(t);
  }
  if (t.ty == TK_IDENT) {
    if (consume('(')) {
      return function_call(t);
    }
    return local_variable(t);
  }

  bad_token(t, "primary expression expected");
  return null;
}

private Node new_stmt_expr(Token t, Node[] exprs) {
  Node last = vec_pop(exprs);

  Node[] v;
  foreach (expr; exprs) {
    v ~= new_expr(ND_EXPR_STMT, t, expr);
  }

  Node node = new_node(ND_STMT_EXPR, t);
  node.stmts = v;
  node.expr = last;
  return node;
}

// `x++` where x is of type T is compiled as
// `({ T *y = &x; T z = *y; *y = *y + 1; *z; })`.
private Node new_post_inc(Token t, Node e, int imm) {
  Node[] v;

  Var var1 = add_lvar(ptr_to(e.ty), "tmp");
  Var var2 = add_lvar(e.ty, "tmp");

  v ~= new_binop('=', t, new_varref(t, var1), new_expr(ND_ADDR, t, e));
  v ~= new_binop('=', t, new_varref(t, var2), new_deref(t, var1));
  v ~= new_binop('=', t, new_deref(t, var1), new_binop('+', t, new_deref(t,
      var1), new_int_node(imm, t)));
  v ~= new_varref(t, var2);
  return new_stmt_expr(t, v);
}

private Node postfix() {
  Node lhs = primary();

  for (;;) {
    Token t = tokens[pos];

    if (consume(TK_INC)) {
      lhs = new_post_inc(t, lhs, 1);
      continue;
    }

    if (consume(TK_DEC)) {
      lhs = new_post_inc(t, lhs, -1);
      continue;
    }

    if (consume('.')) {
      lhs = new_expr(ND_DOT, t, lhs);
      lhs.name = ident();
      continue;
    }

    if (consume(TK_ARROW)) {
      lhs = new_expr(ND_DOT, t, new_expr(ND_DEREF, t, lhs));
      lhs.name = ident();
      continue;
    }

    if (consume('[')) {
      Node node = new_binop('+', t, lhs, assign());
      lhs = new_expr(ND_DEREF, t, node);
      expect(']');
      continue;
    }
    return lhs;
  }
}

private Node unary() {
  Token t = tokens[pos];

  if (consume('-')) {
    return new_binop('-', t, new_int_node(0, t), unary());
  }
  if (consume('*')) {
    return new_expr(ND_DEREF, t, unary());
  }
  if (consume('&')) {
    return new_expr(ND_ADDR, t, unary());
  }
  if (consume('!')) {
    return new_expr('!', t, unary());
  }
  if (consume('~')) {
    return new_expr('~', t, unary());
  }
  if (consume(TK_SIZEOF)) {
    return new_int_node(get_type(unary()).size, t);
  }
  if (consume(TK_ALIGNOF)) {
    return new_int_node(get_type(unary())._align, t);
  }
  if (consume(TK_INC)) {
    return new_assign_eq('+', unary(), new_int_node(1, t));
  }
  if (consume(TK_DEC)) {
    return new_assign_eq('-', unary(), new_int_node(1, t));
  }
  return postfix();
}

private Node mul() {
  Node lhs = unary();
  for (;;) {
    Token t = tokens[pos];
    if (consume('*')) {
      lhs = new_binop('*', t, lhs, unary());
    } else if (consume('/')) {
      lhs = new_binop('/', t, lhs, unary());
    } else if (consume('%')) {
      lhs = new_binop('%', t, lhs, unary());
    } else {
      return lhs;
    }
  }
}

private Node add() {
  Node lhs = mul();
  for (;;) {
    Token t = tokens[pos];
    if (consume('+')) {
      lhs = new_binop('+', t, lhs, mul());
    } else if (consume('-')) {
      lhs = new_binop('-', t, lhs, mul());
    } else {
      return lhs;
    }
  }
}

private Node shift() {
  Node lhs = add();
  for (;;) {
    Token t = tokens[pos];
    if (consume(TK_SHL)) {
      lhs = new_binop(ND_SHL, t, lhs, add());
    } else if (consume(TK_SHR)) {
      lhs = new_binop(ND_SHR, t, lhs, add());
    } else {
      return lhs;
    }
  }
}

private Node relational() {
  Node lhs = shift();
  for (;;) {
    Token t = tokens[pos];
    if (consume('<')) {
      lhs = new_binop('<', t, lhs, shift());
    } else if (consume('>')) {
      lhs = new_binop('<', t, shift(), lhs);
    } else if (consume(TK_LE)) {
      lhs = new_binop(ND_LE, t, lhs, shift());
    } else if (consume(TK_GE)) {
      lhs = new_binop(ND_LE, t, shift(), lhs);
    } else {
      return lhs;
    }
  }
}

private Node equality() {
  Node lhs = relational();
  for (;;) {
    Token t = tokens[pos];
    if (consume(TK_EQ)) {
      lhs = new_binop(ND_EQ, t, lhs, relational());
    } else if (consume(TK_NE)) {
      lhs = new_binop(ND_NE, t, lhs, relational());
    } else {
      return lhs;
    }
  }
}

private Node bit_and() {
  Node lhs = equality();
  while (consume('&')) {
    Token t = tokens[pos];
    lhs = new_binop('&', t, lhs, equality());
  }
  return lhs;
}

private Node bit_xor() {
  Node lhs = bit_and();
  while (consume('^')) {
    Token t = tokens[pos];
    lhs = new_binop('^', t, lhs, bit_and());
  }
  return lhs;
}

private Node bit_or() {
  Node lhs = bit_xor();
  while (consume('|')) {
    Token t = tokens[pos];
    lhs = new_binop('|', t, lhs, bit_xor());
  }
  return lhs;
}

private Node logand() {
  Node lhs = bit_or();
  while (consume(TK_LOGAND)) {
    Token t = tokens[pos];
    lhs = new_binop(ND_LOGAND, t, lhs, bit_or());
  }
  return lhs;
}

private Node logor() {
  Node lhs = logand();
  while (consume(TK_LOGOR)) {
    Token t = tokens[pos];
    lhs = new_binop(ND_LOGOR, t, lhs, logand());
  }
  return lhs;
}

private Node conditional() {
  Node cond = logor();
  Token t = tokens[pos];
  if (!consume('?')) {
    return cond;
  }

  Node node = new_node('?', t);
  node.cond = cond;
  node.then = expr();
  expect(':');
  node.els = conditional();
  return node;
}

// `x op= y` where x is of type T is compiled as
// `({ T *z = &x; *z = *z op y; })`.
private Node new_assign_eq(int op, Node lhs, Node rhs) {
  Node[] v;
  Token t = lhs.token;

  // T *z = &x
  Var var = add_lvar(ptr_to(lhs.ty), "tmp");
  v ~= new_binop('=', t, new_varref(t, var), new_expr(ND_ADDR, t, lhs));

  // *z = *z op y
  v ~= new_binop('=', t, new_deref(t, var), new_binop(op, t, new_deref(t, var), rhs));
  return new_stmt_expr(t, v);
}

private Node assign() {
  Node lhs = conditional();
  Token t = tokens[pos];

  if (consume('=')) {
    return new_binop('=', t, lhs, assign());
  }
  if (consume(TK_MUL_EQ)) {
    return new_assign_eq('*', lhs, assign());
  }
  if (consume(TK_DIV_EQ)) {
    return new_assign_eq('/', lhs, assign());
  }
  if (consume(TK_MOD_EQ)) {
    return new_assign_eq('%', lhs, assign());
  }
  if (consume(TK_ADD_EQ)) {
    return new_assign_eq('+', lhs, assign());
  }
  if (consume(TK_SUB_EQ)) {
    return new_assign_eq('-', lhs, assign());
  }
  if (consume(TK_SHL_EQ)) {
    return new_assign_eq(ND_SHL, lhs, assign());
  }
  if (consume(TK_SHR_EQ)) {
    return new_assign_eq(ND_SHR, lhs, assign());
  }
  if (consume(TK_AND_EQ)) {
    return new_assign_eq(ND_LOGAND, lhs, assign());
  }
  if (consume(TK_XOR_EQ)) {
    return new_assign_eq('^', lhs, assign());
  }
  if (consume(TK_OR_EQ)) {
    return new_assign_eq('|', lhs, assign());
  }
  return lhs;
}

private Node expr() {
  Node lhs = assign();
  Token t = tokens[pos];
  if (!consume(',')) {
    return lhs;
  }
  return new_binop(',', t, lhs, expr());
}

private int const_expr() {
  Token t = tokens[pos];
  Node node = expr();
  if (node.op != ND_NUM) {
    bad_token(t, "constant expression expected");
  }
  return node.val;
}

private Type read_array(Type ty) {
  int[] v;

  while (consume('[')) {
    if (consume(']')) {
      v ~= -1;
      continue;
    }
    v ~= const_expr();
    expect(']');
  }

  foreach_reverse (len; v) {
    ty = ary_of(ty, len);
  }
  return ty;
}

private Node direct_decl(Type ty) {
  Token t = tokens[pos];
  Node node;
  Type placeholder = new Type(CType.VOID, 0); // dummy

  if (t.ty == TK_IDENT) {
    node = new_node(ND_VARDEF, t);
    node.ty = placeholder;
    node.name = ident();
  } else if (consume('(')) {
    node = declarator(placeholder);
    expect(')');
  } else {
    bad_token(t, "bad direct-declarator");
  }

  // Read the second half of type name (e.g. `[3][5]`).
  placeholder.copy_from(read_array(ty));

  // Read an initializer.
  if (consume('=')) {
    node.init_ = assign();
  }
  return node;
}

private Node declarator(Type ty) {
  while (consume('*')) {
    ty = ptr_to(ty);
  }
  return direct_decl(ty);
}

private Node declaration_type() {
  Type ty = decl_specifiers();
  Node node = declarator(ty);
  expect(';');
  return node;
}

private Node declaration() {
  Type ty = decl_specifiers();
  Node node = declarator(ty);
  expect(';');
  Var var = add_lvar(node.ty, node.name);

  if (node.init_ is null) {
    return null_stmt;
  }

  // Convert `T var = init` to `T var; var = init`.
  Token t = node.token;
  Node lhs = new_varref(t, var);
  Node rhs = node.init;
  node.init_ = null;

  Node expr = new_binop('=', t, lhs, rhs);
  return new_expr(ND_EXPR_STMT, t, expr);
}

private Var param_declaration() {
  Type ty = decl_specifiers();
  Node node = declarator(ty);
  ty = node.ty;
  if (ty.ty == CType.ARY) {
    ty = ptr_to(ty.ary_of);
  }
  return add_lvar(ty, node.name);
}

private Node expr_stmt() {
  Token t = tokens[pos];
  Node node = new_expr(ND_EXPR_STMT, t, expr());
  expect(';');
  return node;
}

private Node stmt() {
  Token t = tokens[pos++];

  switch (t.ty) {
  case TK_TYPEDEF: {
      Node node = declaration_type();
      assert(node.name !is null);
      env.typedefs[node.name] = node.ty;
      return null_stmt;
    }
  case TK_IF: {
      Node node = new_node(ND_IF, t);
      expect('(');
      node.cond = expr();
      expect(')');

      node.then = stmt();

      if (consume(TK_ELSE)) {
        node.els = stmt();
      }
      return node;
    }
  case TK_FOR: {
      Node node = new_node(ND_FOR, t);
      expect('(');
      env = new Env(env);
      breaks ~= node;
      continues ~= node;

      if (is_typename()) {
        node.init_ = declaration();
      } else if (!consume(';')) {
        node.init_ = expr_stmt();
      }

      if (!consume(';')) {
        node.cond = expr();
        expect(';');
      }

      if (!consume(')')) {
        node.inc = expr();
        expect(')');
      }

      node.body_ = stmt();

      vec_pop(breaks);
      vec_pop(continues);
      env = env.prev;
      return node;
    }
  case TK_WHILE: {
      Node node = new_node(ND_FOR, t);
      breaks ~= node;
      continues ~= node;

      expect('(');
      node.cond = expr();
      expect(')');
      node.body_ = stmt();

      vec_pop(breaks);
      vec_pop(continues);
      return node;
    }
  case TK_DO: {
      Node node = new_node(ND_DO_WHILE, t);
      breaks ~= node;
      continues ~= node;

      node.body_ = stmt();
      expect(TK_WHILE);
      expect('(');
      node.cond = expr();
      expect(')');
      expect(';');

      vec_pop(breaks);
      vec_pop(continues);
      return node;
    }
  case TK_SWITCH: {
      Node node = new_node(ND_SWITCH, t);
      node.cases = [];

      expect('(');
      node.cond = expr();
      expect(')');

      breaks ~= node;
      switches ~= node;
      node.body_ = stmt();
      vec_pop(breaks);
      vec_pop(switches);
      return node;
    }
  case TK_CASE: {
      if (switches.length == 0) {
        bad_token(t, "stray case");
      }
      Node node = new_node(ND_CASE, t);
      node.val = const_expr();
      expect(':');
      node.body_ = stmt();

      Node n = switches[$ - 1];
      n.cases ~= node;
      return node;
    }
  case TK_BREAK: {
      if (breaks.length == 0) {
        bad_token(t, "stray break");
      }
      Node node = new_node(ND_BREAK, t);
      node.target = breaks[$ - 1];
      return node;
    }
  case TK_CONTINUE: {
      if (continues.length == 0) {
        bad_token(t, "stray continue");
      }
      Node node = new_node(ND_CONTINUE, t);
      node.target = breaks[$ - 1];
      return node;
    }
  case TK_RETURN: {
      Node node = new_node(ND_RETURN, t);
      node.expr = expr();
      expect(';');
      return node;
    }
  case '{':
    return compound_stmt();
  case ';':
    return null_stmt;
  default:
    pos--;
    if (is_typename()) {
      return declaration();
    }
    return expr_stmt();
  }
}

private Node compound_stmt() {
  Token t = tokens[pos];
  Node node = new_node(ND_COMP_STMT, t);
  node.stmts = [];

  env = new Env(env);
  while (!consume('}')) {
    node.stmts ~= stmt();
  }
  env = env.prev;
  return node;
}

private void toplevel() {
  bool is_typedef = consume(TK_TYPEDEF);
  bool is_extern = consume(TK_EXTERN);

  Type ty = decl_specifiers();
  while (consume('*')) {
    ty = ptr_to(ty);
  }

  string name = ident();

  // Function
  if (consume('(')) {
    Var[] params;
    lvars = [];
    while (!consume(')')) {
      if (params.length > 0) {
        expect(',');
      }
      params ~= param_declaration();
    }

    Token t = tokens[pos];
    Node node = new_node(ND_DECL, t);

    breaks = [];
    continues = [];
    switches = [];

    node.name = name;
    node.params = params;

    node.ty = new Type(CType.FUNC, 0);
    node.ty.returning = ty;

    add_lvar(node.ty, name);

    if (consume(';')) {
      return;
    }

    node.op = ND_FUNC;
    t = tokens[pos];
    expect('{');
    if (is_typedef) {
      bad_token(t, "typedef has function definition");
    }
    node.body_ = compound_stmt();

    Function fn = new Function();
    fn.name = name;
    fn.node = node;
    fn.lvars = lvars;
    fn.bbs = [];
    prog.funcs ~= fn;
    return;
  }

  ty = read_array(ty);
  expect(';');

  if (is_typedef) {
    env.typedefs[name] = ty;
    return;
  }

  // Global variable
  add_gvar(ty, name, null, is_extern);
}

private bool is_eof() {
  Token t = tokens[pos];
  return t.ty == TK_EOF;
}

Program parse(Token[] tokens_) {
  tokens = tokens_;
  pos = 0;
  env = new Env;

  prog = new Program;
  prog.gvars = [];
  prog.funcs = [];

  while (!is_eof()) {
    toplevel();
  }
  return prog;
}
