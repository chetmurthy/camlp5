(* camlp5r *)
(* camlp5_top.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_macro.cmo";
#load "q_MLast.cmo";

open Parsetree;
open Lexing;
open Versdep;
open Camlp5_top_funs;

Toploop.parse_toplevel_phrase.val := wrapped_toplevel_phrase ;

Pcaml.warning.val :=
  fun loc txt ->
    IFDEF OCAML_VERSION <= OCAML_2_00 THEN
      Toploop.print_warning (Ast2pt.mkloc loc) txt
    ELSIFDEF OCAML_VERSION <= OCAML_2_99 THEN
      Toploop.print_warning (Ast2pt.mkloc loc) (Warnings.Other txt)
    ELSE
      Toploop.print_warning (Ast2pt.mkloc loc) Format.err_formatter
        (IFDEF OCAML_VERSION <= OCAML_3_08_4 THEN Warnings.Other txt
         ELSIFDEF OCAML_VERSION < OCAML_4_02_0 THEN Warnings.Camlp4 txt
         ELSE Warnings.Preprocessor txt END)
    END;
