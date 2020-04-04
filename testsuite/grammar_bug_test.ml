(* camlp5r *)
(* grammar_bug_test.ml *)
#load "pa_macro.cmo";

open OUnit2 ;
open OUnitTest ;

Pcaml.inter_phrases.val := Some (";;\n") ;

value pa_expr s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.expr ;
value sig_item s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.sig_item ;
value match_case s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.match_case ;
value argle1 s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.argle1 ;
value argle2 s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.argle2 ;

value tests = "grammar_bug" >::: [
    "simple" >:: (fun  [ _ ->
      ignore(pa_expr "1")
    ])
    ; "sig_item-open1" >:: (fun  [ _ ->
      ignore(sig_item "open A")
    ])
    ; "sig_item-open2" >:: (fun  [ _ ->
      ignore(sig_item "open A.B")
    ])
    ; "match-case-1" >:: (fun  [ _ ->
      ignore(match_case "1 -> 2")
    ])
    ; "argle1-1" >:: (fun [ _ ->
      ignore(argle1 "A")
                          ])
    ; "argle1-2" >:: (fun [ _ ->
      ignore(argle1 "B")
                          ])
    ; "argle2-1" >:: (fun [ _ ->
      ignore(argle2 "A")
                          ])
    ; "argle2-2" >:: (fun [ _ ->
      ignore(argle2 "B")
                          ])
    ]
 ;

value _ = run_test_tt_main tests ;
  
(*
;;; Local Variables: ***
;;; mode:tuareg ***
;;; End: ***

*)
