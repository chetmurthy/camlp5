(* camlp5r *)
(* grammar_bug_test.ml *)
#load "pa_macro.cmo";
#load "q_MLast.cmo";

open OUnit2 ;
open OUnitTest ;
open OUnitAssert ;
open MLast ;

value car = List.hd;
value cdr = List.tl;
value rec sep_last = fun [
    [] -> failwith "sep_last"
  | [hd] -> (hd,[])
  | [hd::tl] ->
      let (l,tl) = sep_last tl in (l,[hd::tl])
]
;
value invoked_with cmdna =
  let variant_names = [cmdna; cmdna^".byte"; cmdna^".native"; cmdna^".opt"] in

  let argv = Array.to_list Sys.argv in
  let path = Pcre.split ~{rex=(Pcre.regexp "/")} (car argv) in
  let (fname, _) = sep_last path in

  List.exists ((=) fname) variant_names
;

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

value assert_bool ?{printer} msg b =
  if not b then
    let msg0 = match printer with [ None -> "" | Some (f, arg) -> f arg ] in
    assert_failure (msg0^msg)
  else ()
;

value assert_raises_exn_pred ?{msg} ?{exnmsg} exnpred (f: unit -> 'a) =
  let pexn =
    Printexc.to_string
  in
  let get_error_string () =
    let str =
      Format.sprintf
        "expected exception %s, but no exception was raised."
        (match exnmsg with [ None -> "<no message provided>" | Some msg -> msg ])
    in
      match msg with [
          None ->
            assert_failure str

        | Some s ->
            assert_failure (s^"\n"^str) ]
  in
    match raises f with [
       None ->
          assert_failure (get_error_string ())

      | Some e ->
          let msg = match msg with [ None -> "" | Some s -> s ] in
          assert_bool ~{printer=(pexn,e)} msg (exnpred e) ]
;

open Alt_pa_o ;

value has_argle = ref False ;

value tests () = "grammar_bug" >::: [
    "sig_item-open1" >:: (fun  [ _ ->
      if has_argle.val then
      assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy 
                                              (Stdlib.Stream.Error "[ext_opt] expected after 'open' (in [sig_item])")))
        (fun () -> ignore(pa_sig_item "open A"))
      else
        ignore(pa_sig_item "open A")
    ])
    ; "argle1-2" >:: (fun [ _ ->
      if has_argle.val then
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "illegal begin of argle1")))
          (fun () -> ignore(pa_argle1 "B"))
      else
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "entry [argle1] is empty")))
          (fun () -> ignore(pa_argle1 "B"))
                          ])
    ; "argle2-1" >:: (fun [ _ ->
      if has_argle.val then
        assert_equal "A" (pa_argle2 "A")
      else
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "entry [argle2] is empty")))
          (fun () -> ignore(pa_argle2 "A"))
                          ])
    ; "argle2-2" >:: (fun [ _ ->
      if has_argle.val then
        assert_equal "B" (pa_argle2 "B")
      else
        assert_raises_exn_pred (smart_exn_eq (Ploc.Exc Ploc.dummy (Stdlib.Stream.Error "entry [argle2] is empty")))
          (fun () -> ignore(pa_argle2 "B"))
                          ])
    ]
 ;

value _ = 
if invoked_with "grammar_bug_test" then
do {
  match Sys.getenv "HAS_ARGLE" with [
    exception Not_found -> failwith "must set HAS_ARGLE to use this test"
  | "true" -> has_argle.val := True
  | "false" -> has_argle.val := False
  | _ -> failwith "must set HAS_ARGLE to either true or false"
  ] ;
  run_test_tt_main (tests ())
}
else ()
;
  
(*
;;; Local Variables: ***
;;; mode:tuareg ***
;;; End: ***

*)
