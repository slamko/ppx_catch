open Ppxlib

let wrap_let_bind loc b =
     let err_str_loc = Loc.make ~loc "err" in
     let catch_pat = Ast_builder.Default.(ppat_construct
                           ~loc
                           (Loc.make ~loc (lident "Failure"))
                           (Some
                              (ppat_var
                                 ~loc
                                 err_str_loc ))) in

     let catch_any_pat = Ast_builder.Default.ppat_any ~loc in

     let unknown_err_res = Ast_builder.Default.(pexp_construct
                     ~loc
                     (Loc.make ~loc (lident "Error"))
                     (Some (pexp_constant ~loc
                              (Pconst_string
                                 ("unknown error", loc, None))))) in

     let err_res = Ast_builder.Default.(pexp_construct
                     ~loc
                     (Loc.make ~loc (lident "Error"))
                     (Some (pexp_ident
                              ~loc
                              (Loc.make ~loc (lident "err"))))) in
 

     let ok_expr =
       Ast_builder.Default.pexp_construct ~loc
         (Loc.make ~loc (lident "Ok"))
         (Some b.pvb_expr) in

     let new_expr = Ast_builder.Default.(pexp_try
                      ~loc
                      ok_expr
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

     {
         pvb_pat = b.pvb_pat;
         pvb_expr = new_expr;
         pvb_attributes = b.pvb_attributes;
         pvb_loc = b.pvb_loc;
     } 
 

let ext_fun ~ctxt arr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let expr_desc = arr.pexp_desc in
  match expr_desc with
  | Pexp_let (recp, bindings, expr) ->
     let eloc = loc in
     let new_bind = wrap_let_bind eloc @@ List.hd bindings in
     Ast_builder.Default.(pexp_let ~loc recp [new_bind] expr)
  | _ ->
     Location.raise_errorf ~loc "Extension applies only to let bindings."

let extracter () =
  Ast_pattern.(single_expr_payload __)
  (* Ast_pattern.(ptop_def __) *)

let top_ext ~ctxt arr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match arr with
  | PStr str ->
     let st_item = List.hd str in
     begin match st_item.pstr_desc with
     | Pstr_value (recp, bindings) ->
        let new_bind = wrap_let_bind loc @@ List.hd bindings in

        Printf.printf "Hello\n";
        {
          pstr_desc = Pstr_value (recp, [new_bind]);
          pstr_loc = st_item.pstr_loc;
        }
     | _ -> Location.raise_errorf
              ~loc "Extension applies only to let bindings." end
  | _ ->
     Location.raise_errorf ~loc "Extension applies only to let bindings."

let top = Extension.V3.declare
            "try"
            Extension.Context.Structure_item
            Ast_pattern.__
            top_ext


let exten = Extension.V3.declare
              "catch"
              Extension.Context.Expression
              (extracter ())
              ext_fun

let rule = Context_free.Rule.extension exten
let top_rule = Context_free.Rule.extension top

let () = 
  Driver.register_transformation ~rules:[rule] "catch";
  Driver.register_transformation ~rules:[top_rule] "try";
  ()
