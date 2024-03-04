// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Interpreter

open Rego.Ast
open Rego.Value

type intr=interpreter

let make_new () = {
   rules = [];
   scopes = []
}

let rec eval_expr (e:expr) (i:intr)  : Tot (value*intr) =
  match e with
  | Var v -> lookup_var v i
  | Value v -> (v, i)
  | Ast.Array elems -> eval_array_elems elems [] i
  | Ast.Set elems -> eval_set_elems elems [] i
  | Ast.Object fields -> eval_fields fields (Object []) i
  | _ -> (Undefined, i)

and eval_array_elems (elems:list expr) (values:list value) (i:intr)  : Tot (value*intr)
(decreases elems)
=
  match elems with
  | [] -> (Value.Array (List.Tot.rev values), i)
  | hd::tl -> 
    match eval_expr hd i with
    | (Undefined, i') -> (Undefined, i')
    | (v, i') -> eval_array_elems tl (v::values) i'

and eval_set_elems (elems:list expr) (values:list value) (i:intr)  : Tot (value*intr)
(decreases elems)
=
  match elems with
  | [] -> (List.Tot.fold_left Value.insert_into_set (Value.Set []) values, i)
  | hd::tl -> 
    match eval_expr hd i with
    | (Undefined, i') -> (Undefined, i')
    | (v, i') -> eval_set_elems tl (v::values) i'

and eval_fields (fields:list (expr*expr)) (obj:value) (i:intr)  : Tot (value*intr)
(decreases fields)
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

and lookup_var (ident:string) (i:intr)  : Tot (value*intr) (decreases ident) =
  (Undefined, i)

let eval (i:intr) (e:Ast.expr) : (value*intr) =
  eval_expr e i
