(* camlp5r *)
(* pa_o.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";

open Asttools;
open Mlsyntax.Original;

value gram =
  Grammar.gcreate
    {Plexing.tok_func _ = failwith "no loaded parsing module";
     Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
     Plexing.tok_match = fun []; Plexing.tok_text _ = "";
     Plexing.tok_comm = None}
;

do {
  let odfa = Plexer.dollar_for_antiquotation.val in
  let osrs = Plexer.simplest_raw_strings.val in
  Plexer.dollar_for_antiquotation.val := False;
  Plexer.simplest_raw_strings.val := True;
  Plexer.utf8_lexing.val := True;
  Grammar.Unsafe.gram_reinit gram (Plexer.gmake ());
  Plexer.dollar_for_antiquotation.val := odfa;
  Plexer.simplest_raw_strings.val := osrs
};

value check_dot_uid_f strm =
  match Stream.npeek 5 strm with [
    [("",".") ; ("UIDENT",_) :: _] -> "OK"
  | [("",".") ; ("","$") ; ("LIDENT",("uid"|"_uid")) ; ("", ":") ; ("LIDENT", _) :: _] -> "OK"
  | _ -> raise Stream.Failure
  ]
;

value check_dot_uid =
  Grammar.Entry.of_parser gram "check_dot_uid"
    check_dot_uid_f
;

value argle1 : Grammar.Entry.e string = Grammar.Entry.create gram "argle1";
value argle2 : Grammar.Entry.e string = Grammar.Entry.create gram "argle2";
value sig_item : Grammar.Entry.e string = Grammar.Entry.create gram "sig_item";
value ext_opt = Grammar.Entry.create gram "ext_opt";
value alg_attributes_no_anti = Grammar.Entry.create gram "alg_attributes_no_anti";

EXTEND
  GLOBAL: sig_item argle1 argle2
  ext_opt
  alg_attributes_no_anti
  ;
  attribute_id:
  [ [ l = LIST1 [ i = LIDENT -> i | i = UIDENT -> i ] SEP "." -> String.concat "." l
    ] ]
  ;
  attribute_body:
  [ [
      id = attribute_id ->
      <:attribute_body< $_attrid:id$ $structure:[]$ >>
    ] ]
  ;
  item_attribute:
  [ [ "[@@" ; attr = attribute_body; "]" -> attr
    ] ]
  ;
  alg_attribute:
  [ [ "[@" ; attr = attribute_body; "]" -> attr
    ] ]
  ;
  item_attributes:
  [ [ l = LIST0 item_attribute -> l ]
  ]
  ;
  alg_attributes_no_anti:
  [ [ l = LIST0 alg_attribute -> l ]
  ]
  ;
  sig_item:
    [ "top"
      [ "open"; ext_opt; i = extended_longident ; item_attrs = item_attributes ->
          Printf.sprintf "open %s" i
      ] ]
  ;
  ext_opt: [ [ OPT [ "%" ; attribute_id ] ] ] ;

  (* Core types *)
  extended_longident:
    [ LEFTA
      [ me1 = SELF; "(" ; me2 = SELF ; ")" → Printf.sprintf "%s(%s)" me1 me2
      | me1 = SELF; check_dot_uid ; "."; i = UIDENT → Printf.sprintf "%s.%s" me1 i
      ]
    | "simple"
      [ i = UIDENT → i
      ] ]
  ;

END
;

if match Sys.getenv "HAS_ARGLE" with [
    exception Not_found -> failwith "must set HAS_ARGLE to use this test"
  | "true" -> True
  | "false" -> False
  | _ -> failwith "must set HAS_ARGLE to either true or false"
  ] then
EXTEND
  GLOBAL: argle1    argle2
    ;
  int_or_dot: [[ "A" -> "A" | "B" -> "B" ]] ;
  argle1:
    [ [ OPT [ "when" ]; "A" ->
          "A"
      | OPT [ "when" ]; "B"  ->
          "B"
      ] ]
  ;
  argle2:
    [ [ OPT [ "when" ]; e = int_or_dot ->
          e
      ] ]
  ;
END
else ()
;
value pa_sig_item s = s |> Stream.of_string |> Grammar.Entry.parse sig_item ;
value pa_argle1 s = s |> Stream.of_string |> Grammar.Entry.parse argle1 ;
value pa_argle2 s = s |> Stream.of_string |> Grammar.Entry.parse argle2 ;
