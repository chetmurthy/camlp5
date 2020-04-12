(* camlp5r *)
(* pa_passthru.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Asttools;
open MLast;

value prepend_longident li e =
  let rec prerec li e = match li with [
    <:longident:< $uid:uid$ >> -> <:expr< $uid:uid$ . $e$ >>
  | <:longident:< $longid:li$ . $uid:uid$ >> -> prerec li <:expr< $uid:uid$ . $e$ >>
  | _ -> assert False
  ] in
  prerec li e
;

value abstract_over l e =
  List.fold_right (fun p e -> let loc = loc_of_patt p in <:expr< fun $p$ -> $e$ >>) l e
;

value expr_applist e el =
  List.fold_left (fun e arg -> let loc = loc_of_expr arg in <:expr< $e$ $arg$ >>) e el
;

value arrows_list loc l ty =
  List.fold_right (fun argty ty -> <:ctyp< $argty$ -> $ty$ >>)
    l ty
;

value ctyp_wrap_attrs ty al =
  let loc = loc_of_ctyp ty in
  List.fold_left (fun ty attr -> <:ctyp< $ty$  [@ $_attribute:attr$ ] >>)
    ty al
;

value ctyp_applist e el =
  List.fold_left (fun e arg -> let loc = loc_of_ctyp arg in <:ctyp< $e$ $arg$ >>) e el
;

value ctyp_unapplist e =
  let rec unrec acc = fun [
    <:ctyp< $t$ $arg$ >> -> unrec [arg::acc] t
  | t -> (t,acc)
  ] in unrec [] e
;
