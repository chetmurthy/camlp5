(* camlp5r *)
(* grammar_bug_test.ml *)
#load "pa_macro.cmo";
#load "q_MLast.cmo";

open OUnit2 ;
open OUnitTest ;
open Testutil ;
open MLast ;
value loc = Ploc.dummy ;

value smart_exn_eq e1 e2 =
  let rec eqrec e1 e2 =
  match (e1, e2) with [
    (Ploc.Exc _ e1, Ploc.Exc _ e2) -> eqrec e1 e2
  | (Stream.Error msg1, Stream.Error msg2) -> msg1 = msg2
  | (Failure msg1, Failure msg2) -> msg1 = msg2
  | (Syntaxerr.Error (Other _), Syntaxerr.Error (Other _)) -> True
  | _ -> e1 = e2
  ]
  in eqrec e1 e2
;



Pcaml.inter_phrases.val := Some (";;\n") ;

value has_argle = ref False ;

value sig_item s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.sig_item ;
value argle1 s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.argle1 ;
value argle2 s = s |> Stream.of_string |> Grammar.Entry.parse Alt_pa_o.argle2 ;

value tests () = "grammar_bug" >::: [
    "sig_item-open1" >:: (fun  [ _ ->
      if has_argle.val then
      assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy 
                                              (Stdlib.Stream.Error "[ext_attributes] expected after 'open' (in [sig_item])")))
        (fun () -> ignore(sig_item "open A"))
      else
        ignore(sig_item "open A")
    ])
    ; "sig_item-open2" >:: (fun  [ _ ->
      if has_argle.val then
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy 
                                                (Stdlib.Stream.Error "[ext_attributes] expected after 'open' (in [sig_item])")))
          (fun () -> ignore(sig_item "open A.B"))
      else
        assert_equal ~{cmp=Reloc.eq_sig_item} <:sig_item< open A.B >> (sig_item "open A.B")
    ])
    ; "argle1-2" >:: (fun [ _ ->
      if has_argle.val then
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "illegal begin of argle1")))
          (fun () -> ignore(argle1 "B"))
      else
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "entry [argle1] is empty")))
          (fun () -> ignore(argle1 "B"))
                          ])
    ; "argle2-1" >:: (fun [ _ ->
      if has_argle.val then
        ignore(argle2 "A")
      else
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "entry [argle2] is empty")))
          (fun () -> ignore(argle2 "A"))
                          ])
    ; "argle2-2" >:: (fun [ _ ->
      if has_argle.val then
        ignore(argle2 "B")
      else
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "entry [argle2] is empty")))
          (fun () -> ignore(argle2 "B"))
                          ])
    ]
 ;

value _ = do {
  match Sys.getenv "HAS_ARGLE" with [
    exception Not_found -> failwith "must set HAS_ARGLE to use this test"
  | "true" -> has_argle.val := True
  | "false" -> has_argle.val := False
  | _ -> failwith "must set HAS_ARGLE to either true or false"
  ] ;
  run_test_tt_main (tests ()) ;
};
  
(*
;;; Local Variables: ***
;;; mode:tuareg ***
;;; End: ***

*)
