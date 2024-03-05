// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Interpreter

open Rego.Ast
open Rego.Value

type intr=interpreter

let make_new () = {
   rules = [];
   scopes = [];
   loop_exprs = [];
}

let lookup_loop_var (i:intr) (e:expr) =
    List.Tot.find (fun (k,_) -> k = e) i.loop_exprs

let rec lookup_var_in_scopes (scopes:list scope) (name:string) =
    match scopes with
    | hd::tl ->
      (match List.Tot.find (fun (n,_) -> n = name) hd with
      | Some (_, v) -> v
      | None -> lookup_var_in_scopes tl name)
    | _ -> Undefined

let lookup_local_var (i:intr) (name:string) =
    lookup_var_in_scopes i.scopes name

let rec eval_expr (e:expr) (i:intr)  : Tot (value*intr)
=
  match lookup_loop_var i e with
  | Some((_, v)) -> (v, i)
  | None ->
  match e with
  | Var v -> lookup_var v i
  | Value v -> (v, i)
  | Ast.Array elems -> eval_array_elems elems [] i
  | Ast.Set elems -> eval_set_elems elems [] i
  | Ast.Object fields -> eval_fields fields (Object []) i
  | Ast.ArrayCompr query -> eval_query_stmts query.stmts (Value.Array []) i
  | Ast.SetCompr query -> eval_query_stmts query.stmts (Value.Set []) i
  | Ast.ObjectCompr query -> eval_query_stmts query.stmts (Value.Object []) i
  | Ast.RefDot (refr, field) -> eval_ref_dot refr (field, i)
  | Ast.RefBrack (refr, index) -> eval_ref_brack (refr, index) i
  | Ast.AssignExpr (op, lhs, rhs) -> eval_assign_expr (op, lhs, rhs) i
  | _ -> (Undefined, i)


and eval_array_elems (elems:list expr) (values:list value) (i:intr)  : Tot (value*intr)
=
  match elems with
  | [] -> (Value.Array (List.Tot.rev values), i)
  | hd::tl ->
    match eval_expr hd i with
    | (Undefined, i') -> (Undefined, i')
    | (v, i') -> eval_array_elems tl (v::values) i'

and eval_set_elems (elems:list expr) (values:list value) (i:intr)  : Tot (value*intr)
=
  match elems with
  | [] -> (List.Tot.fold_left Value.insert_into_set (Value.Set []) values, i)
  | hd::tl ->
    match eval_expr hd i with
    | (Undefined, i') -> (Undefined, i')
    | (v, i') -> eval_set_elems tl (v::values) i'

and eval_fields (fields:list (expr*expr)) (obj:value) (i:intr)  : Tot (value*intr)
=
  match fields with
  | [] -> (obj, i)
  | (key_expr,value_expr)::tl ->
    match eval_expr key_expr i with
    | (Undefined, i') -> (Undefined, i')
    | (kv, i') ->
      match eval_expr value_expr i' with
      | (Undefined, i'') -> (Undefined, i'')
      | (vv, i'') -> eval_fields tl (insert_into_object obj kv vv) i''

and lookup_var (ident:string) (i:intr)  : Tot (value*intr)
=
  (lookup_local_var i ident, i)

and eval_query_stmts (stmts:list literalStmt) (out:value) (i:intr) : Tot (value*intr)
=
  match stmts with
  | hd::[] ->
    (match hd.literal with
    | ArrayComprOutput e ->
      let (v, i') = eval_expr e i in
      (insert_into_array out v, i')
    | SetComprOutput e ->
      let (v, i') = eval_expr e i in
      (insert_into_set out v, i')
    | ObjectComprOutput (ke, ve) ->
      let (kv, i') = eval_expr ke i in
      let (vv, i'') = eval_expr ve i' in
      (insert_into_object out kv vv, i')
    | _ -> (Undefined, i))
  | hd::tl ->
    let (v, i') = match hd.literal with
    | Expr e -> eval_expr e i
    | _ -> (Undefined, i)
    in eval_query_stmts tl out i'
  | [] -> (Undefined, i)

(* todo chaining *)
and eval_ref_dot (refr:expr) pr  : Tot (value*intr)
=
    let (field, i) = pr in
    let (v, i') = eval_expr refr i in
    (lookup_path v (Value.String field::[]), i')

(* todo chaining *)
and eval_ref_brack (pr:expr*expr) (i:intr)  : Tot (value*intr)
=
    let (refr, index) = pr in
    let (v, i') = eval_expr refr i in
    let (idx, i'') = eval_expr index i' in
    (lookup_path v (idx::[]), i'')

(* todo...actual implementation is quite complex *)
and eval_assign_expr (t: (assignOp*expr*expr)) (i:intr) : Tot (value*intr)
=
  let (_, lhs, rhs) = t in
  match lhs with
  | Var name ->
    (
      let (rv, i') = eval_expr rhs i in
      match rv with
      | Undefined -> (Undefined, i')
      | _ -> (* todo check existing binding *)
        match i'.scopes with
        | hd::tl -> (Bool true, {i' with scopes = ((name, rv)::hd)::tl})
        | _ -> (Undefined, i')
    )
  | _ -> (Undefined, i)



let eval (i:intr) (e:Ast.expr) : (value*intr) =
  eval_expr e i

let eval_user_query (i:intr) (q:Ast.query) : (value*intr) =
  let i' = { i with scopes = []::i.scopes } in
  eval_query_stmts q.stmts (Value.Array []) i'
