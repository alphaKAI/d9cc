module d9cc.token;
import std.algorithm, std.string, std.ascii, std.stdio, std.conv, std.file;
import core.stdc.stdlib : exit, EXIT_FAILURE;
import d9cc.preprocess;

enum {
  TK_NUM = 256, // Number literal
  TK_STR, // String literal
  TK_IDENT, // Identifier
  TK_ARROW, // .
  TK_EXTERN, // "extern"
  TK_TYPEDEF, // "typedef"
  TK_INT, // "int"
  TK_CHAR, // "char"
  TK_VOID, // "void"
  TK_STRUCT, // "struct"
  TK_BOOL, // "_Bool"
  TK_IF, // "if"
  TK_ELSE, // "else"
  TK_FOR, // "for"
  TK_DO, // "do"
  TK_WHILE, // "while"
  TK_SWITCH, // "switch"
  TK_CASE, // "case"
  TK_BREAK, // "break"
  TK_CONTINUE, // "continue"
  TK_EQ, // ==
  TK_NE, // !=
  TK_LE, // <=
  TK_GE, // >=
  TK_LOGOR, // ||
  TK_LOGAND, // &&
  TK_SHL, // <<
  TK_SHR, // >>
  TK_INC, // ++
  TK_DEC, // --
  TK_MUL_EQ, // *=
  TK_DIV_EQ, // /=
  TK_MOD_EQ, // %=
  TK_ADD_EQ, // +=
  TK_SUB_EQ, // -=
  TK_SHL_EQ, // <<=
  TK_SHR_EQ, // >>=
  TK_AND_EQ, // &=
  TK_XOR_EQ, // ^=
  TK_OR_EQ, // |=
  TK_RETURN, // "return"
  TK_SIZEOF, // "sizeof"
  TK_ALIGNOF, // "_Alignof"
  TK_TYPEOF, // "typeof"
  TK_PARAM, // Function-like macro parameter
  TK_EOF, // End marker
}

// Token type
class Token {
  int ty; // Token type
  int val; // Number literal
  string name; // Identifier

  // String literal
  string str;

  // For preprocessor
  bool stringize;

  // For error reporting
  immutable(char)* buf;
  string path;
  immutable(char)* start;
  immutable(char)* end;

  void copy_from(Token that) {
    this.ty = that.ty;
    this.val = that.val;
    this.name = that.name;
    this.str = that.str;
    this.stringize = that.stringize;
    this.buf = that.buf;
    this.path = that.path;
    this.start = that.start;
    this.end = that.end;
  }
}

class TokenizeError : Error {
  this(string msg) {
    super(msg);
  }
}

private class Env {
  string path;
  immutable(char)* buf;
  Token[] tokens;
  Env prev;

  this(Env prev, string path, immutable(char)* buf) {
    this.path = path == "-" ? "(stdin)" : path;
    this.buf = buf;
    this.tokens = [];
    this.prev = prev;
  }
}

private Env env;
private alias KeywordMap = int[string];
private KeywordMap keywords;

private File open_file(string path) {
  if (path == "-") {
    return stdin;
  }

  return File(path, "r");
}

private string read_file(string path) {
  return readText(path);
}

immutable(char)* strchr(immutable(char)* str, char ch) {
  for (; str !is null && *str != '\0'; str++) {
    if (*str == ch) {
      break;
    }
  }

  if (*str == '\0') {
    return null;
  } else {
    return str;
  }
}

// Finds a line pointed by a given pointer from the input file
// to print it out.
private void print_line(immutable(char)* buf, string path, immutable(char)* pos) {
  immutable(char)* start = buf;
  int line;
  int col;

  for (immutable(char)* p = buf; p !is null && *p != '\0'; p++) {
    if (*p == '\n') {
      start = p + 1;
      line++;
      col = 0;
      continue;
    }

    if (p != pos) {
      col++;
      continue;
    }

    stderr.writef("error at %s:%d:%d\n\n", path, line + 1, col + 1);

    // Print out the line containing the error location.
    long linelen = strchr(p, '\n') - start;
    stderr.writef("%.*s\n", linelen, start.to!string);

    // Show tabs for tabs and spaces for other characters
    // so that the column matches.
    foreach (i; 0 .. col) {
      stderr.writef((start[i] == '\t') ? "\t" : " ");
    }

    stderr.writef("^\n\n");
    return;
  }
}

void warn_token(Token t, string msg) {
  if (t.start) {
    print_line(t.buf, t.path, t.start);
  }
  stderr.writef(msg);
  stderr.writef("\n");
}

void bad_token(Token t, string msg) {
  warn_token(t, msg);
  exit(EXIT_FAILURE);
}

private void bad_position(immutable(char)* p, string msg) {
  print_line(env.buf, env.path, p);
  throw new TokenizeError(msg);
}

int get_line_number(Token t) {
  int n;
  for (immutable(char)* p = t.buf; p < t.end; p++) {
    if (*p == '\n') {
      n++;
    }
  }
  return n;
}

// Returns true if Token t followed a space or a comment
// in an original source file.
private bool need_space(Token t) {
  immutable(char)* s = t.start;
  if (t.buf <= s - 1 && isWhite(s[-1])) {
    return true;
  }
  return t.buf <= s - 2 && startsWith((s - 2).to!string, "*/");
}

// For C preprocessor.
string stringize(Token[] tokens) {
  string sb;

  foreach (i, token; tokens) {
    if (token.ty == '\n') {
      continue;
    }
    if (i > 0 && need_space(token)) {
      sb ~= ' ';
    }

    assert(token.start && token.end);
    sb ~= token.start[0 .. token.end - token.start];
  }

  return sb;
}

// Atomic unit in the grammar is called "token".
// For example, `123`, `"abc"` and `while` are tokens.
// The tokenizer splits an input string into tokens.
// Spaces and comments are removed by the tokenizer.

private Token add(int ty, immutable(char)* start) {
  Token token = new Token();
  token.ty = ty;
  token.start = start;
  token.path = env.path;
  token.buf = env.buf;
  env.tokens ~= token;
  return token;
}

private struct Symbol {
  string name;
  int ty;
}

// dfmt off
Symbol[] symbols = [
  {"<<=", TK_SHL_EQ},
  {">>=", TK_SHR_EQ},
  {"!=", TK_NE},
  {"&&", TK_LOGAND},
  {"++", TK_INC},
  {"--", TK_DEC},
  {"->", TK_ARROW},
  {"<<", TK_SHL},
  {"<=", TK_LE},
  {"==", TK_EQ},
  {">=", TK_GE},
  {">>", TK_SHR},
  {"||", TK_LOGOR},
  {"*=", TK_MUL_EQ},
  {"/=", TK_DIV_EQ},
  {"%=", TK_MOD_EQ},
  {"+=", TK_ADD_EQ},
  {"-=", TK_SUB_EQ},
  {"&=", TK_AND_EQ},
  {"^=", TK_XOR_EQ},
  {"|=", TK_OR_EQ},
  {"\0", 0}];
// dfmt on

private KeywordMap keyword_map() {
  KeywordMap map;
  map["_Alignof"] = TK_ALIGNOF;
  map["_Bool"] = TK_BOOL;
  map["break"] = TK_BREAK;
  map["case"] = TK_CASE;
  map["char"] = TK_CHAR;
  map["continue"] = TK_CONTINUE;
  map["do"] = TK_DO;
  map["else"] = TK_ELSE;
  map["extern"] = TK_EXTERN;
  map["for"] = TK_FOR;
  map["if"] = TK_IF;
  map["int"] = TK_INT;
  map["return"] = TK_RETURN;
  map["sizeof"] = TK_SIZEOF;
  map["struct"] = TK_STRUCT;
  map["switch"] = TK_SWITCH;
  map["typedef"] = TK_TYPEDEF;
  map["typeof"] = TK_TYPEOF;
  map["void"] = TK_VOID;
  map["while"] = TK_WHILE;
  return map;
}

private immutable(char)* block_comment(immutable(char)* pos) {
  for (immutable(char)* p = pos + 2; *p; p++) {
    if (startsWith(p.to!string, "*/")) {
      return p + 2;
    }
  }
  bad_position(pos, "unclosed comment");
  return null;
}

private int isOctal(char c) {
  return '0' <= c && c <= '7';
}

private int hex(char c) {
  if ('0' <= c && c <= '9') {
    return c - '0';
  }
  if ('a' <= c && c <= 'f') {
    return c - 'a' + 10;
  }
  assert('A' <= c && c <= 'F');
  return c - 'A' + 10;
}

// Read a single character in a char or string literal.
private immutable(char)* c_char(int* res, immutable(char)* p) {
  // Nonescaped
  if (*p != '\\') {
    *res = *p;
    return p + 1;
  }
  p++;

  static char[256] escaped;
  escaped['a'] = '\a';
  escaped['b'] = '\b';
  escaped['f'] = '\f';
  escaped['n'] = '\n';
  escaped['r'] = '\r';
  escaped['t'] = '\t';
  escaped['v'] = '\v';
  escaped['e'] = '\033';
  escaped['E'] = '\033'; // Simple (e.g. `\n` or `\a`)
  int esc = escaped[cast(ubyte)*p];
  if (esc) {
    *res = esc;
    return p + 1;
  }

  // Hexadecimal
  if (*p == 'x') {
    *res = 0;
    p++;
    while (isHexDigit(*p))
      *res = *res * 16 + hex(*p++);
    return p;
  }

  // Octal
  if (isOctal(*p)) {
    int i = *p++ - '0';
    if (isOctal(*p))
      i = i * 8 + *p++ - '0';
    if (isOctal(*p))
      i = i * 8 + *p++ - '0';
    *res = i;
    return p;
  }

  *res = *p;
  return p + 1;
}

private immutable(char)* char_literal(immutable(char)* p) {
  Token t = add(TK_NUM, p++);
  p = c_char(&t.val, p);
  if (*p != '\'') {
    bad_token(t, "unclosed character literal");
  }
  t.end = p + 1;
  return p + 1;
}

private immutable(char)* string_literal(immutable(char)* p) {
  Token t = add(TK_STR, p++);
  string sb;

  while (*p != '"') {
    if (!*p) {
      bad_token(t, "unclosed string literal");
    }
    int c;
    p = c_char(&c, p);
    sb ~= c;
  }

  t.str = sb ~ '\0';
  t.end = p + 1;
  return p + 1;
}

private immutable(char)* ident(immutable(char)* p) {
  int len = 1;
  while (isAlpha(p[len]) || isDigit(p[len]) || p[len] == '_') {
    len++;
  }

  string name = p[0 .. len].to!string.dup;
  int ty = TK_IDENT;
  if (name in keywords) {
    ty = keywords[name];
  }

  Token t = add(ty, p);
  t.name = name;
  t.end = p + len;
  return p + len;
}

private immutable(char)* hexadecimal(immutable(char)* p) {
  Token t = add(TK_NUM, p);
  p += 2;

  if (!isHexDigit(*p)) {
    bad_token(t, "bad hexadecimal number");
  }

  while (isHexDigit(*p)) {
    t.val = t.val * 16 + hex(*p++);
  }
  t.end = p;
  return p;
}

private immutable(char)* octal(immutable(char)* p) {
  Token t = add(TK_NUM, p++);
  while (isOctal(*p)) {
    t.val = t.val * 8 + *p++ - '0';
  }
  t.end = p;
  return p;
}

private immutable(char)* decimal(immutable(char)* p) {
  Token t = add(TK_NUM, p);
  while (isDigit(*p)) {
    t.val = t.val * 10 + *p++ - '0';
  }
  t.end = p;
  return p;
}

private immutable(char)* number(immutable(char)* p) {
  if (startsWith(p.to!string, "0x") || startsWith(p.to!string, "0X")) {
    return hexadecimal(p);
  }
  if (*p == '0') {
    return octal(p);
  }
  return decimal(p);
}

private void scan() {
  immutable(char)* p = env.buf;

  loop: while (*p != '\0') {
    // New line (preprocessor-only token)
    if (*p == '\n') {
      Token t = add(*p, p);
      p++;
      t.end = p;
      continue;
    }

    // Whitespace
    if (isWhite(*p)) {
      p++;
      continue;
    }

    // Line comment
    if (startsWith(p.to!string, "//")) {
      while (*p && *p != '\n') {
        p++;
      }
      continue;
    }

    // Block comment
    if (startsWith(p.to!string, "/*")) {
      p = block_comment(p);
      continue;
    }

    // Character literal
    if (*p == '\'') {
      p = char_literal(p);
      continue;
    }

    // String literal
    if (*p == '"') {
      p = string_literal(p);
      continue;
    }

    // Multi-letter symbol
    foreach (symbol; symbols) {
      string name = symbol.name;

      if (!startsWith(p.to!string, name)) {
        continue;
      }

      Token t = add(symbol.ty, p);
      p += name.length;
      t.end = p;
      goto loop;
    }

    // Single-letter symbol
    if (strchr("+-*/;=(),{}<>[]&.!?:|^%~#", *p)) {
      Token t = add(*p, p);
      p++;
      t.end = p;
      continue;
    }

    // Keyword or identifier
    if (isAlpha(*p) || *p == '_') {
      p = ident(p);
      continue;
    }

    // Number
    if (isDigit(*p)) {
      p = number(p);
      continue;
    }

    bad_position(p, "cannot tokenize");
  }
}

private void replace_crlf(char* p) {
  for (char* q = p; *q;) {
    if (startsWith(q.to!string, "\r\n")) {
      q++;
    }
    *p++ = *q++;
  }
  *p = '\0';
}

// Concatenates continuation lines. We keep the total number of
// newline characters the same to keep the line counter sane.
private void remove_backslash_newline(char* p) {
  int cnt;

  for (char* q = p; *q;) {
    if (startsWith(q.to!string, "\\\n")) {
      cnt++;
      q += 2;
      continue;
    }
    if (*q == '\n') {
      foreach (_; 0 .. cnt + 1) {
        *p++ = '\n';
      }
      q++;
      cnt = 0;
      continue;
    }
    *p++ = *q++;
  }
  *p = '\0';
}

private Token[] strip_newline_tokens(Token[] tokens) {
  Token[] v;
  foreach (token; tokens) {
    if (token.ty != '\n') {
      v ~= token;
    }
  }
  return v;
}

private void append(Token x, Token y) {
  string sb;

  sb ~= x.str;
  sb ~= y.str;
  x.str = sb;
}

private Token[] join_string_literals(Token[] tokens) {
  Token[] ret;
  Token last;

  foreach (token; tokens) {
    if (last && last.ty == TK_STR && token.ty == TK_STR) {
      append(last, token);
      continue;
    }

    last = token;
    ret ~= token;
  }
  return ret;
}

Token[] tokenize(string path, bool add_eof) {
  if (keywords == null) {
    keywords = keyword_map();
  }

  string buf = read_file(path);
  char[] mut_buf = buf.to!(char[]);
  replace_crlf(&mut_buf[0]);
  remove_backslash_newline(&mut_buf[0]);
  buf = mut_buf.to!string;

  env = new Env(env, path, &buf[0]);
  scan();
  if (add_eof) {
    add(TK_EOF, null);
  }
  Token[] v = env.tokens;
  env = env.prev;

  v = preprocess(v);
  v = strip_newline_tokens(v);
  return join_string_literals(v);
}
