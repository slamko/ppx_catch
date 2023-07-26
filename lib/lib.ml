open Ppxlib

let ext_fun ~ctxt arr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let expr_desc = arr.pexp_desc in
  match expr_desc with
  | Pexp_let (Nonrecursive, bindings, expr) ->
     let eloc = expr.pexp_loc in

     let err_str_loc = Loc.make ~loc:eloc "err" in
     let catch_pat = Ast_builder.Default.(ppat_construct
                           ~loc
                           (Loc.make ~loc (lident "Failure"))
                           (Some
                              (ppat_var
                                 ~loc:eloc
                                 err_str_loc ))) in

     let catch_any_pat = Ast_builder.Default.ppat_any ~loc:eloc in

                           (* )) in *)
     let unknown_err_res = Ast_builder.Default.(pexp_construct
                     ~loc:eloc
                     (Loc.make ~loc:eloc (lident "Error"))
                     (Some (pexp_constant ~loc:eloc
                              (Pconst_string
                                 ("unknown error", eloc, None))))) in

     let err_res = Ast_builder.Default.(pexp_construct
                     ~loc:eloc
                     (Loc.make ~loc:eloc (lident "Error"))
                     (Some (pexp_ident
                              ~loc:eloc
                              (Loc.make ~loc:eloc (lident "err"))))) in
 

     let ok_expr =
       Ast_builder.Default.pexp_construct ~loc:expr.pexp_loc
         (Loc.make ~loc:eloc (lident "Ok"))
         (Some expr) in

     let new_expr = Ast_builder.Default.(pexp_try
                      ~loc:eloc
                      ok_expr
                      [case
                         ~lhs:catch_pat
                         ~guard:None
                         ~rhs:err_res
                    ]) in

     Ast_builder.Default.(pexp_let ~loc Nonrecursive bindings new_expr)
  | _ ->
     Location.raise_errorf ~loc "Extension applies only to let bindings."

let extracter () = Ast_pattern.(single_expr_payload __)

let exten = Extension.V3.declare
              "mat"
              Extension.Context.Expression
              (extracter ())
              ext_fun

let rule = Context_free.Rule.extension exten

let () = 
  Ppxlib.Driver.register_transformation ~rules:[rule] "mat";
  print_endline "Hello, World!";
  ()