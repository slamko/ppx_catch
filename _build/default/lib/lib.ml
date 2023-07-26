open Ppxlib

let ext_fun ~ctxt arr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let expr_desc = arr.pexp_desc in
  match expr_desc with
  | Pexp_let (recp, bindings, expr) ->
     let eloc = loc in

     let err_str_loc = Loc.make ~loc:eloc "err" in
     let catch_pat = Ast_builder.Default.(ppat_construct
                           ~loc
                           (Loc.make ~loc (lident "Failure"))
                           (Some
                              (ppat_var
                                 ~loc:eloc
                                 err_str_loc ))) in

     let catch_any_pat = Ast_builder.Default.ppat_any ~loc:eloc in

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

     Printf.printf "bnum %d\n" @@ List.length bindings ;
     let b = List.hd bindings in
     let new_expr = Ast_builder.Default.(pexp_try
                      ~loc:eloc
                      b.pvb_expr
                      [case
                         ~lhs:catch_pat
                         ~guard:None
                         ~rhs:err_res;
                       case
                         ~lhs:catch_any_pat
                         ~guard:None
                         ~rhs:unknown_err_res
                      ]
                    ) in

     let new_bind = {
         pvb_pat = b.pvb_pat;
         pvb_expr = new_expr;
         pvb_attributes = b.pvb_attributes;
         pvb_loc = b.pvb_loc;
       } in
     
     Ast_builder.Default.(pexp_let ~loc recp [new_bind] expr)
  | _ ->
     Location.raise_errorf ~loc "Extension applies only to let bindings."

let extracter () =
  Ast_pattern.(single_expr_payload __)
  (* Ast_pattern.(ptop_def __) *)

let exten = Extension.V3.declare
              "catch"
              Extension.Context.Expression
              (extracter ())
              ext_fun

let rule = Context_free.Rule.extension exten

let () = 
  Driver.register_transformation ~rules:[rule] "catch";
  ()
