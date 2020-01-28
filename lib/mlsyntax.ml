(* camlp5r *)
(* mlsyntax.ml *)

value string_for_all_i f s =
  let slen = String.length s in
  let rec frec i =
    if i = slen then True
    else f i (String.get s i) && frec (i+1)
  in frec 0
;

value is_prefix_symbol_start = fun [
    '!'
  | '~' | '?' -> True
  | _ -> False
]
;
value is_infix_symbol_start = fun [
    '=' | '<' | '>' | '|' | '&' | '$'
  | '@' | '^'
  | '+' | '-'
  | '*' | '/'  | '%' -> True
  | _ -> False
]
;

value is_symbolchar = fun [
  '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' |
  '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' -> True
| _ -> False
];

value is_operator = fun s ->
  let c0 = String.get s 0 in
  if c0 = '#' then
    string_for_all_i (fun i c ->
        i < 1 || is_symbolchar c) s
  else
    string_for_all_i (fun i c ->
        is_symbolchar c) s
;


value is_prefix_operator = fun s ->
  String.length s >= 2 &&
  string_for_all_i (fun i c ->
      i < 1 || is_symbolchar c
    ) s &&
  let c0 = String.get s 0 in
  let c1 = String.get s 0 in
  match (c0, c1) with [
    (('!' | '~' | '?'),_) -> True
  | _ -> False
  ]
;

value is_infix0_operator = fun s ->
  String.length s > 1 &&
  string_for_all_i (fun i c ->
      i < 1 || is_symbolchar c
    ) s &&
  let c0 = String.get s 0 in
  let c1 = String.get s 0 in
  match (c0, c1) with [
    (('=' | '<' | '>' | '|' | '&' | '$'),_) -> True
  | _ -> False
  ]
;

value is_infix1_operator = fun s ->
  String.length s > 1 &&
  string_for_all_i (fun i c ->
      i < 1 || is_symbolchar c
    ) s &&
  let c0 = String.get s 0 in
  let c1 = String.get s 0 in
  match (c0, c1) with [
    (('@' | '^'),_) -> True
  | _ -> False
  ]
;

value is_infix2_operator = fun s ->
  String.length s > 1 &&
  string_for_all_i (fun i c ->
      i < 1 || is_symbolchar c
    ) s &&
  let c0 = String.get s 0 in
  let c1 = String.get s 0 in
  match (c0, c1) with [
    (('+' | '-'),_) -> True
  | _ -> False
  ]
;

value is_infix3_operator = fun s ->
  String.length s > 1 &&
  string_for_all_i (fun i c ->
      i < 1 || is_symbolchar c
    ) s &&
  let c0 = String.get s 0 in
  let c1 = String.get s 0 in
  match (c0, c1) with [
    ('*',c2) when c2 <> '*' -> True
  | (('/' | '%'),_) -> True
  | _ -> False
  ]
;

value is_infix4_operator = fun s ->
  String.length s > 2 &&
  string_for_all_i (fun i c ->
      i < 2 || is_symbolchar c
    ) s &&
  let c0 = String.get s 0 in
  let c1 = String.get s 0 in
  match (c0, c1) with [
    ('*','*') when String.length s > 2 -> True
  | _ -> False
  ]
;

value user_defined_operator = fun s ->
  if String.length s < 2 ||
     not (string_for_all_i (fun i c ->
         i < 1 || is_symbolchar c
       ) s) then None
  else
    let c0 = String.get s 0 in
    let c1 = String.get s 0 in
    match (c0, c1) with [
      (('!' | '~' | '?'),_) -> Some("PREFIXOP", s)
    | (('=' | '<' | '>' | '|' | '&' | '$'),_) -> Some("INFIXOP0",s)
    | (('@' | '^'),_) -> Some("INFIXOP1", s)
    | (('+' | '-'),_) -> Some("INFIXOP2", s)
    | ('*','*') when String.length s > 2 -> Some("INFIXOP4",s)
    | ('*','*') -> None
    | (('*' | '/' | '%'),_) -> Some("INFIXOP3", s)
    | ('#',_) -> Some("HASHOP", s)
    | _ -> None
    ]
;
