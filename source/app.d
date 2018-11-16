import std.stdio;
import d9cc.token, d9cc.parse, d9cc.genir, d9cc.sema, d9cc.irdump, d9cc.opt,
  d9cc.liveness, d9cc.regalloc, d9cc.gen_x86;
import core.stdc.stdlib : exit, EXIT_FAILURE;

void usage() {
  stderr.writeln("Usage: d9cc [-test] [-dump-ir1] [-dump-ir2] <file>");
  exit(EXIT_FAILURE);
}

void main(string[] args) {
  if (args.length == 1) {
    usage();
  }

  string path;
  bool dump_ir1;
  bool dump_ir2;

  if (args.length == 3 && args[1] == "-dump-ir1") {
    dump_ir1 = true;
    path = args[2];
  } else if (args.length == 3 && args[1] == "-dump-ir2") {
    dump_ir2 = true;
    path = args[2];
  } else {
    if (args.length != 2) {
      usage();
    }
    path = args[1];
  }

  // Tokenize and parse.
  Token[] tokens = tokenize(path, true);
  Program prog = parse(tokens);
  sema(prog);
  gen_ir(prog);

  if (dump_ir1) {
    dump_ir(prog.funcs);
  }

  optimize(prog);
  liveness(prog);
  alloc_regs(prog);

  if (dump_ir2) {
    dump_ir(prog.funcs);
  }

  gen_x86(prog);
}
