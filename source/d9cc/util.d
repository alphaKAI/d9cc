module d9cc.util;
import d9cc.parse;

class Type {
  CType ty;
  int size; // sizeof
  int _align; // alignof

  // Pointer
  Type ptr_to;

  // Array
  Type ary_of;
  size_t len;

  // Struct
  Type[string] members;
  int offset;

  // Function
  Type returning;

  this(CType ty, int size) {
    this.ty = ty;
    this.size = size;
    this._align = size;
  }

  void copy_from(Type that) {
    if (that is null) {
      return;
    }
    this.ty = that.ty;
    this.size = that.size;
    this._align = that._align;
    this.ptr_to = that.ptr_to;
    this.ary_of = that.ary_of;
    this.len = that.len;
    this.members = that.members;
    this.offset = that.offset;
    this.returning = that.returning;
  }
}

Type ptr_to(Type base) {
  Type ty = new Type(CType.PTR, 8);
  ty.ptr_to = base;
  return ty;
}

Type ary_of(Type base, int len) {
  Type ty = new Type(CType.ARY, base.size * len);
  ty._align = base._align;
  ty.ary_of = base;
  ty.len = len;
  return ty;
}

Type void_ty() {
  return new Type(CType.VOID, 0);
}

Type bool_ty() {
  return new Type(CType.BOOL, 1);
}

Type char_ty() {
  return new Type(CType.CHAR, 1);
}

Type int_ty() {
  return new Type(CType.INT, 4);
}

Type func_ty(Type returning) {
  Type ty = new Type(CType.FUNC, 0);
  ty.returning = returning;
  return ty;
}

bool same_type(Type x, Type y) {
  if (x.ty != y.ty) {
    return false;
  }

  switch (x.ty) {
  case CType.PTR:
    return same_type(x.ptr_to, y.ptr_to);
  case CType.ARY:
    return x.size == y.size && same_type(x.ary_of, y.ary_of);
  case CType.STRUCT:
  case CType.FUNC:
    return x == y;
  default:
    return true;
  }
}

int roundup(int x, int _align) {
  return (x + _align - 1) & ~(_align - 1);
}

T vec_pop(T)(T[] vec) {
  assert(vec.length > 0);
  T ret = vec[$ - 1];
  vec.length--;
  return ret;
}

bool vec_contains(T)(T[] vec, T elem) {
  foreach (e; vec) {
    if (e == elem) {
      return true;
    }
  }

  return false;
}

bool vec_union1(T)(T[] vec, T elem) {
  if (vec_contains(vec, elem)) {
    return false;
  }
  vec ~= elem;
  return true;
}
