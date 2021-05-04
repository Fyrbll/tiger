structure S = String

type key = string

exception AbsentKey

(* 1.1.b *)
datatype 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

val empty = LEAF

fun insert(LEAF,             key, value) = TREE(LEAF, key, value, LEAF)
  | insert(TREE(l, k, v, r), key, value) =
    case S.compare(key, k)
     of LESS    => TREE(insert(l, key, value), k, v, r)
      | EQUAL   => TREE(l, k, value, r)
      | GREATER => TREE(l, k, v, insert(r, key, value))

fun lookup (LEAF,             key) = raise AbsentKey
  | lookup (TREE(l, k, v, r), key) =
    case S.compare(key, k)
     of LESS    => lookup(l, key)
      | EQUAL   => v
      | GREATER => lookup(r, key)

(* 1.1.a *)
fun member (LEAF,             key) = false
  | member (TREE(l, k, v, r), key) =
    case S.compare(key, k)
     of LESS    => member(l, key)
      | EQUAL   => true
      | GREATER => member(r, key)

(* 1.1.c.a *)
(*
          t
         / \
        s   .
       /
      p
     /
    i
   /
  f
 /
b
*)

(* 1.1.c.b *)
(*
  a
 / \
.   b
     \
      c
       \
        d
         \
          e
           \
            f
             \
              g
               \
                h
                 \
                  i
*)

(* 1.1.d *)
(* 
Found the book but it's not too relevant to learning about compilers so I'll
get back to this exercise later.
*)
