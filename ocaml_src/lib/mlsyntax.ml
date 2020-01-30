(* camlp5r *)
(* mlsyntax.ml *)

let string_for_all_i f s =
  let slen = String.length s in
  let rec frec i =
    if i = slen then true else f i (String.get s i) && frec (i + 1)
  in
  frec 0
;;

let is_symbolchar =
  function
    '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' |
    '>' | '?' | '@' | '^' | '|' | '~' ->
      true
  | _ -> false
;;

let is_operator_gen startp startofs s =
  startp s && string_for_all_i (fun i c -> i < startofs || is_symbolchar c) s
;;

let is_PREFIXOP_start s =
  match String.get s 0 with
    '!' | '~' | '?' -> true
  | _ -> false
;;
let is_PREFIXOP s = is_operator_gen is_PREFIXOP_start 1 s;;

let is_INFIXOP0_start s =
  match String.get s 0 with
    '=' | '<' | '>' | '|' | '&' | '$' -> true
  | _ -> false
;;
let is_INFIXOP0 s = is_operator_gen is_INFIXOP0_start 1 s;;

let is_INFIXOP1_start s =
  match String.get s 0 with
    '@' | '^' -> true
  | _ -> false
;;
let is_INFIXOP1 s = is_operator_gen is_INFIXOP1_start 1 s;;

let is_INFIXOP2_start s =
  match String.get s 0 with
    '+' | '-' -> true
  | _ -> false
;;
let is_INFIXOP2 s = is_operator_gen is_INFIXOP2_start 1 s;;

let is_INFIXOP3_start s =
  match String.get s 0, String.get s 1 with
    '*', c when c <> '*' -> true
  | ('/' | '%'), _ -> true
  | _ -> false
;;
let is_INFIXOP3 s = is_operator_gen is_INFIXOP3_start 1 s;;

let is_INFIXOP4_start s =
  match String.get s 0, String.get s 1 with
    '*', '*' -> true
  | _ -> false
;;
let is_INFIXOP4 s = is_operator_gen is_INFIXOP4_start 2 s;;

let is_HASHOP_start s =
  match String.get s 0 with
    '#' -> true
  | _ -> false
;;
let is_HASHOP s = is_operator_gen is_HASHOP_start 1 s;;
