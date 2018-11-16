module d9cc.preprocess;
import d9cc.token;

private class Env {
  Token[] input;
  Token[] output;
  int pos;
  Env prev;

  this(Env prev, Token[] input) {
    this.input = input;
    this.output = [];
    this.prev = prev;
  }
}

private Env env;

enum MacroType {
  OBJLIKE,
  FUNCLIKE,
}

private Macro[string] macros;

class Macro {
  MacroType ty;
  Token[] tokens;
  string[] params;

  this(MacroType ty, string name) {
    this.ty = ty;
    this.tokens = [];
    this.params = [];
    macros[name] = this;
  }
}

private void append(Token[] tokens) {
  foreach (token; tokens) {
    env.output ~= token;
  }
}

private void emit(Token token) {
  env.output ~= token;
}

private Token next() {
  assert(env.pos < env.input.length);
  return env.input[env.pos++];
}

private bool is_eof() {
  return env.pos == env.input.length;
}

private Token get(int ty, string msg) {
  Token token = next();
  if (token.ty != ty) {
    bad_token(token, msg);
  }
  return token;
}

private string ident(string msg) {
  Token token = get(TK_IDENT, msg);
  return token.name;
}

private Token peek() {
  return env.input[env.pos];
}

private bool consume(int ty) {
  if (peek().ty != ty) {
    return false;
  }
  env.pos++;
  return true;
}

private Token[] read_until_eol() {
  Token[] tokens;
  while (!is_eof()) {
    Token token = next();
    if (token.ty == '\n') {
      break;
    }
    tokens ~= token;
  }
  return tokens;
}

private Token new_int(Token tmpl, int val) {
  Token token = new Token;
  token.copy_from(tmpl);
  token.ty = TK_NUM;
  token.val = val;
  return token;
}

private Token new_string(Token tmpl, string str) {
  Token token = new Token;
  token.copy_from(tmpl);
  token.ty = TK_STR;
  token.str = str;
  return token;
}

private Token new_param(Token tmpl, int val) {
  Token token = new Token;
  token.copy_from(tmpl);
  token.ty = TK_PARAM;
  token.val = val;
  return token;
}

private bool is_ident(Token token, string s) {
  return token.ty == TK_IDENT && token.name == s;
}

// Replaces macro parameter tokens with TK_PARAM tokens.
private void replace_macro_params(Macro m) {
  Token[] tokens = m.tokens;
  string[] params = m.params;

  int[string] map;
  foreach (int i, name; params) {
    map[name] = i;
  }

  foreach (i, token; tokens) {
    if (token.ty != TK_IDENT) {
      continue;
    }
    int n = -1;
    if (token.name in map) {
      n = map[token.name];
    }
    if (n == -1) {
      continue;
    }
    tokens[i] = new_param(token, n);
  }
}

// Replaces '#' followed by a macro parameter with one token.
private void replace_hash_ident(Macro m) {
  Token[] tokens = m.tokens;
  Token[] v;

  int i;
  for (; i < tokens.length - 1; i++) {
    Token token1 = tokens[i];
    Token token2 = tokens[i + 1];

    if (token1.ty == '#' && token2.ty == TK_PARAM) {
      token2.stringize = true;
      v ~= token2;
      i++;
    } else {
      v ~= token1;
    }
  }

  if (i == tokens.length - 1) {
    v ~= tokens[i];
  }
  m.tokens = v;
}

private Token[] read_one_arg() {
  Token[] v;
  Token start = peek();
  int level;

  while (!is_eof()) {
    Token token = peek();
    if (level == 0) {
      if (token.ty == ')' || token.ty == ',') {
        return v;
      }
    }
    next();
    if (token.ty == '(') {
      level++;
    } else if (token.ty == ')') {
      level--;
    }
    v ~= token;
  }
  bad_token(start, "unclosed macro argument");
  return null;
}

private Token[] read_args() {
  Token[] v;

  if (consume(')')) {
    return v;
  }

  v ~= read_one_arg();

  while (!consume(')')) {
    get(',', "comma expected");
    v ~= read_one_arg();
  }

  return v;
}

private bool emit_special_macro(Token t) {
  if (is_ident(t, "__LINE__")) {
    emit(new_int(t, get_line_number(t)));
    return true;
  }
  return false;
}

private void apply_objlike(Macro m, Token start) {
  foreach (token; m.tokens) {
    if (emit_special_macro(token)) {
      continue;
    }
    emit(token);
  }
}

private void apply_funclike(Macro m, Token start) {
  get('(', "comma expected");

  Token[] args = read_args();
  if (m.params.length != args.length) {
    bad_token(start, "number of parameter does not match");
  }

  foreach (token; m.tokens) {
    if (emit_special_macro(token)) {
      continue;
    }

    if (token.ty == TK_PARAM) {
      if (token.stringize) {
        string s = stringize(args[token.val .. $]);
        emit(new_string(token, s));
      } else {
        append(args[token.val .. $]);
      }
      continue;
    }
    emit(token);
  }
}

private void apply(Macro m, Token start) {
  if (m.ty == MacroType.OBJLIKE) {
    apply_objlike(m, start);
  } else {
    apply_funclike(m, start);
  }
}

private void define_funclike(string name) {
  Macro m = new Macro(MacroType.FUNCLIKE, name);
  while (!consume(')')) {
    if (m.params.length > 0) {
      get(',', ", expected");
    }
    m.params ~= ident("parameter name expected");
  }

  m.tokens = read_until_eol();
  replace_macro_params(m);
  replace_hash_ident(m);
}

private void define_objlike(string name) {
  Macro m = new Macro(MacroType.OBJLIKE, name);
  m.tokens = read_until_eol();
}

private void define() {
  string name = ident("macro name expected");
  if (consume('(')) {
    return define_funclike(name);
  }
  return define_objlike(name);
}

private void include() {
  Token t = get(TK_STR, "string expected");
  string path = t.str;
  get('\n', "newline expected");
  append(tokenize(path, false));
}

Token[] preprocess(Token[] tokens) {
  if (macros is null) {
    macros = typeof(macros).init;
  }
  env = new Env(env, tokens);

  while (!is_eof()) {
    Token t = next();

    if (t.ty == TK_IDENT) {
      Macro m;
      if (t.name in macros) {
        m = macros[t.name];
      }
      if (m !is null) {
        apply(m, t);
      } else {
        emit(t);
      }
      continue;
    }

    if (t.ty != '#') {
      emit(t);
      continue;
    }

    t = get(TK_IDENT, "identifier expected");

    if (t.name == "define") {
      define();
    } else if (t.name == "include") {
      include();
    } else {
      bad_token(t, "unknown directive");
    }
  }

  Token[] v = env.output;
  env = env.prev;
  return v;
}
