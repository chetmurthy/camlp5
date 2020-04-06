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

value argle1 : Grammar.Entry.e unit = Grammar.Entry.create gram "argle1";
value argle2 : Grammar.Entry.e unit = Grammar.Entry.create gram "argle2";
value sig_item : Grammar.Entry.e unit = Grammar.Entry.create gram "sig_item";

EXTEND
  GLOBAL: sig_item argle1 argle2
  ;
  sig_item:
    [ "top"
      [ [ "%" | ]; i = UIDENT -> ()
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
  int_or_dot: [[ "A" -> () | "B" -> () ]] ;
  argle1:
    [ [ [ "when" |  ]; "A" ->
          ()
      | [ "when" | ]; "B"  ->
          ()
      ] ]
  ;
  argle2:
    [ [ [ "when" | ]; int_or_dot ->
          ()
      ] ]
  ;
END
else ()
;
value pa_sig_item s = s |> Stream.of_string |> Grammar.Entry.parse sig_item ;
value pa_argle1 s = s |> Stream.of_string |> Grammar.Entry.parse argle1 ;
value pa_argle2 s = s |> Stream.of_string |> Grammar.Entry.parse argle2 ;
