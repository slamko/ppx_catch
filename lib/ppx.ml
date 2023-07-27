open Ppxlib

let catch_constr_build loc constr_name =
  Ast_builder.Default.(ppat_construct
                           ~loc
                           (Loc.make ~loc (lident constr_name))
                           (Some
                              (ppat_var ~loc
                                 (Loc.make ~loc "err" )
                                 ))) 

let wrap_expr loc pat_name expr =

  let failure_catch_pat = catch_constr_build loc "Failure" in
 
  let invarg_catch_pat = catch_constr_build loc "Invalid_argument" in

  let catch_any_pat = Ast_builder.Default.ppat_any ~loc in

  let unknown_err_res =
    Ast_builder.Default.(pexp_construct
                           ~loc
                           (Loc.make ~loc (lident "Error"))
                           (Some
                              (pexp_constant ~loc
                                    (Pconst_string
                                       (pat_name ^ ": unknown error",
                                        loc, None)
    )))) in
  
  let err_res =
    Ast_builder.Default.(pexp_construct
                           ~loc
                           (Loc.make ~loc (lident "Error"))
                           (Some
                              (pexp_apply
                                 ~loc
                                 (pexp_ident
                                    ~loc
                                    (Loc.make ~loc (lident "^")))
                                 [
                                   (Nolabel,
                                    (pexp_constant
                                       ~loc
                                       (Pconst_string
                                          (pat_name ^ ": ",
                                           loc, None))
                                   ));

                                   (Nolabel,
                                    (pexp_ident
                                       ~loc
                                       (Loc.make ~loc (lident "err"))))
                                   
                                 ]
                              )
                            )) in
  
  
  let ok_expr =
    Ast_builder.Default.pexp_construct ~loc
      (Loc.make ~loc (lident "Ok"))
      (Some expr) in
  
  Ast_builder.Default.(pexp_try
                         ~loc
                         ok_expr
                         [
                           case
                             ~lhs:failure_catch_pat
                             ~guard:None
                             ~rhs:err_res;
                           case
                             ~lhs:invarg_catch_pat
                             ~guard:None
                             ~rhs:err_res;
                           case
                             ~lhs:catch_any_pat
                             ~guard:None
                             ~rhs:unknown_err_res
                         ]
  )

let expr_to_str_item bind expr =
  {
    pvb_pat = bind.pvb_pat;
    pvb_expr = expr;
    pvb_attributes = bind.pvb_attributes;
    pvb_loc = bind.pvb_loc;
  } 

let wrap_let_bind loc bind =

  let pat_name =
    match bind.pvb_pat.ppat_desc with
    | Ppat_var lab_loc -> lab_loc.txt
    | _ -> "" in

  let rec find_wrap expr =
    match expr.pexp_desc with
    | Pexp_fun (name, params, pat, fun_exp) ->
       { pexp_desc =
           Pexp_fun (name, params, pat, (find_wrap fun_exp));
         pexp_loc = expr.pexp_loc;
         pexp_loc_stack = expr.pexp_loc_stack;
         pexp_attributes = expr.pexp_attributes;
       }
    | _ -> wrap_expr loc pat_name expr
  in

  let expr = find_wrap bind.pvb_expr in
  {
    pvb_pat = bind.pvb_pat;
    pvb_expr = expr;
    pvb_attributes = bind.pvb_attributes;
    pvb_loc = bind.pvb_loc;
  }

let ext_let_expander ~ctxt arr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let expr_desc = arr.pexp_desc in
  match expr_desc with
  | Pexp_let (recp, bindings, expr) ->
     let eloc = loc in
     let bind = List.hd bindings in

     let new_bind = wrap_let_bind eloc bind in
     Ast_builder.Default.(pexp_let ~loc recp [new_bind] expr)
  | _ ->
     Location.raise_errorf ~loc "Extension applies only to let bindings."

let top_def_expander ~ctxt arr =
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


let exp_let_ext = Extension.V3.declare
              "catch"
              Extension.Context.Expression
              Ast_pattern.(single_expr_payload __)
              ext_let_expander

let top_def_ext = Extension.V3.declare
            "catch"
            Extension.Context.Structure_item
            Ast_pattern.__
            top_def_expander

let exp_let_rule = Context_free.Rule.extension exp_let_ext
let top_def_rule = Context_free.Rule.extension top_def_ext

let () = 
  Driver.register_transformation ~rules:[exp_let_rule] "wrap";
  Driver.register_transformation ~rules:[top_def_rule] "catch";
  ()
