open Ppxlib

let wrap_expr loc expr = 
  let err_str_loc = Loc.make ~loc "err" in
  let catch_pat = Ast_builder.Default.(ppat_construct
                                         ~loc
                                         (Loc.make ~loc (lident "Failure"))
                                         (Some
                                            (ppat_var
                                               ~loc
                                               err_str_loc ))) in
  
  let catch_any_pat = Ast_builder.Default.ppat_any ~loc in

  let unknown_err_res =
    Ast_builder.Default.(pexp_construct
                           ~loc
                           (Loc.make ~loc (lident "Error"))
                           (Some (pexp_constant ~loc
                                    (Pconst_string
                                       ("unknown error", loc, None))))) in
  
  let err_res =
    Ast_builder.Default.(pexp_construct
                           ~loc
                           (Loc.make ~loc (lident "Error"))
                           (Some (pexp_ident
                                    ~loc
                                    (Loc.make ~loc (lident "err"))))) in
  
  
  let ok_expr =
    Ast_builder.Default.pexp_construct ~loc
      (Loc.make ~loc (lident "Ok"))
      (Some expr) in
  
  Ast_builder.Default.(pexp_try
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
  )


let wrap_let loc b =
  let new_expr = wrap_expr loc b.pvb_expr in
  {
    pvb_pat = b.pvb_pat;
    pvb_expr = new_expr;
    pvb_attributes = b.pvb_attributes;
    pvb_loc = b.pvb_loc;
  } 

let wrap_let_bind loc bind =

  let rec find_wrap expr =
    match expr.pexp_desc with
    | Pexp_fun (name, params, pat, fun_exp) ->
       { pexp_desc =
           Pexp_fun (name, params, pat, (find_wrap fun_exp));
         pexp_loc = expr.pexp_loc;
         pexp_loc_stack = expr.pexp_loc_stack;
         pexp_attributes = expr.pexp_attributes;
       }
    | _ -> wrap_expr loc expr
  in

  let new_expr = find_wrap bind.pvb_expr in
  {
    pvb_pat = bind.pvb_pat;
    pvb_expr = new_expr;
    pvb_attributes = bind.pvb_attributes;
    pvb_loc = bind.pvb_loc;
  } 


let ext_fun ~ctxt arr =
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

(* let extracter () = *)
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
  (* | PPat (pat, expr) -> *)
     (* match expr with *)
     (* | Some expr ->  *)
        (* { *)
          (* pstr_desc = PPat(pat, expr); *)
          (* pstr_loc = st_item.pstr_loc; *)
        (* } *)
  | _ ->
     Location.raise_errorf ~loc "Extension applies only to let bindings."


let exten = Extension.V3.declare
              "catch"
              Extension.Context.Expression
              Ast_pattern.(single_expr_payload __)
              ext_fun

let top = Extension.V3.declare
            "catch"
            Extension.Context.Structure_item
            Ast_pattern.__
            top_ext

let rule = Context_free.Rule.extension exten
let top_rule = Context_free.Rule.extension top

let () = 
  Driver.register_transformation ~rules:[rule] "wrap";
  Driver.register_transformation ~rules:[top_rule] "catch";
  ()
