(* camlp5r *)
(* mlsyntax.ml *)

value string_for_all_i f s =
  let slen = String.length s in
  let rec frec i =
    if i = slen then True
    else f i (String.get s i) && frec (i+1)
  in frec 0
;

value is_symbolchar = fun [
  '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' |
  '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' -> True
| _ -> False
]
;

value is_operator_gen startp startofs s =
  String.length s > 0 &&
  startp s &&
  string_for_all_i (fun i c ->
    i < startofs || is_symbolchar c) s
;

value is_PREFIXOP_start s = match String.get s 0 with [
  '!' | '~' | '?' -> True
| _ -> False
]
;
value is_PREFIXOP s = is_operator_gen is_PREFIXOP_start 1 s ;

value is_INFIXOP0_start s = match String.get s 0 with [
  '=' | '<' | '>' | '|' | '&' | '$' -> True
| _ -> False
]
;
value is_INFIXOP0 s = is_operator_gen is_INFIXOP0_start 1 s ;

value is_INFIXOP1_start s = match String.get s 0 with [
  '@' | '^' -> True
| _ -> False
]
;
value is_INFIXOP1 s = is_operator_gen is_INFIXOP1_start 1 s ;

value is_INFIXOP2_start s = match String.get s 0 with [
  '+' | '-' -> True
| _ -> False
]
;
value is_INFIXOP2 s = is_operator_gen is_INFIXOP2_start 1 s ;

value is_INFIXOP3_start s = match (String.get s 0,String.get s 1) with [
  ('*', c) when c <> '*'  -> True
| (('/' | '%'),_)   -> True
| _ -> False
]
;
value is_INFIXOP3 s = is_operator_gen is_INFIXOP3_start 1 s ;

value is_INFIXOP4_start s = 
String.length s >= 2 &&
match (String.get s 0,String.get s 1) with [
  ('*', '*')  -> True
| _ -> False
]
;
value is_INFIXOP4 s = is_operator_gen is_INFIXOP4_start 2 s ;

value is_HASHOP_start s = match String.get s 0 with [
  '#' -> True
| _ -> False
]
;
value is_HASHOP s = is_operator_gen is_HASHOP_start 1 s ;

value is_infix_operator s =
  is_INFIXOP0 s ||
  is_INFIXOP1 s ||
  is_INFIXOP2 s ||
  is_INFIXOP3 s ||
  is_INFIXOP4 s ||
  is_HASHOP s
;

value is_prefix_operator s = is_PREFIXOP s
;

value is_operator s =
  is_infix_operator s || is_prefix_operator s
;

