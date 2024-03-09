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

type loop_iteration = {
  loop_expr: Ast.expr;
  loop_value: value;
  bindings: list (string & value)
}

let iresult a = intr -> a & intr
let (let!) #a #b (f:iresult a) (g:a -> iresult b) : iresult b = fun i -> let (a', i') = f i in g a' i'
let return #a (x:a) : iresult a = fun i -> (x, i)
let get () : iresult intr = fun i -> (i, i)
let set i : iresult unit = fun _ -> ((), i)
let run #a (f:iresult a) (i:intr) : a & intr = f i

let lookup_loop_expr (e:expr) : iresult _ = 
  let! i = get() in
  return <| List.Tot.find (fun (k,_) -> k = e) i.loop_exprs

let rec lookup_var_in_scopes (scopes:list scope) (name:string) =
    match scopes with
    | hd::tl ->
      (match List.Tot.find (fun (n,_) -> n = name) hd with
      | Some (_, v) -> v
      | None -> lookup_var_in_scopes tl name)
    | _ -> Undefined

let lookup_local_var (name:string) : iresult value =
    let! i = get () in
    return <| lookup_var_in_scopes i.scopes name

let rec make_array_loop_iterations col (le:expr) (idx:string) (idxv:nat)
: Tot (list loop_iteration)
= 
match col with
| [] -> []
| hd::tl ->
  let itr = match idx with
    | "_" -> { loop_expr=le; loop_value=hd; bindings = [] }
    | _ -> { loop_expr=le; loop_value=hd; bindings = [(idx, Number idxv)] }
  in
    itr::make_array_loop_iterations tl le idx (idxv + 1)

let rec make_set_loop_iterations col (le:expr) (idx:string)
: Tot (list loop_iteration)
= 
match col with
| [] -> []
| hd::tl ->
  let itr = match idx with
    | "_" -> { loop_expr=le; loop_value=hd; bindings = [] }
    | _ -> { loop_expr=le; loop_value=hd; bindings = [(idx, hd)] }
  in
    itr::make_set_loop_iterations tl le idx

let rec make_object_loop_iterations col (le:expr) (idx:string)
: Tot (list loop_iteration)
= 
match col with
| [] -> []
| (k,v)::tl ->
  let itr = match idx with
    | "_" -> { loop_expr=le; loop_value=v; bindings = [] }
    | _ -> { loop_expr=le; loop_value=v; bindings = [(idx, k)] }
  in
    itr::make_object_loop_iterations tl le idx


let rec hoist_loops_in_expr (e:expr) (loops:list expr) 
: iresult(list expr)
= match e with
  (* TODO: should we avoid hoisting if the index var is already defined? *)
  | RefBrack (refr, Var _) -> hoist_loops_in_expr refr (e::loops)
  | RefBrack (refr, _) -> hoist_loops_in_expr refr loops
  | Ast.Array arr -> hoist_loops_in_exprs arr loops
  | Ast.Set set -> hoist_loops_in_exprs set loops
  | Ast.Object fields -> hoist_loops_in_fields fields loops
  | Call (_, args) -> hoist_loops_in_exprs args loops
  | UnaryExpr e -> hoist_loops_in_expr e loops
  | RefDot (refr, _) -> hoist_loops_in_expr refr loops
  | BinExpr (_, lhs, rhs)
  | BoolExpr (_, lhs, rhs)
  | ArithExpr (_, lhs, rhs) 
  | AssignExpr (_, lhs, rhs) -> 
    let! lps = hoist_loops_in_expr lhs loops in
    hoist_loops_in_expr rhs lps
  | MembershipExpr (k, v, c) ->
    let! lps1 = hoist_loops_in_expr k loops in
    let! lps2 = hoist_loops_in_expr v lps1 in
    hoist_loops_in_expr c lps2
  | ArrayCompr _ | SetCompr _ | ObjectCompr _  -> return loops
  | Var _ | Value _  -> return loops

and hoist_loops_in_exprs exprs loops
: iresult (list expr)
= match exprs with
  | [] -> return loops
  | hd::tl -> 
    let! lps = hoist_loops_in_expr hd loops in
    hoist_loops_in_exprs tl lps

and hoist_loops_in_fields fields loops
: iresult (list expr)
= match fields with
  | [] -> return loops
  | (k, v)::tl -> 
    let! lps1 = hoist_loops_in_expr k loops in
    let! lps2 = hoist_loops_in_expr v lps1 in
    hoist_loops_in_fields tl lps2

let rec hoist_loops_in_stmts stmts : iresult (list literalStmt)
= match stmts with
| [] -> return []
| hd::tl -> 
  let! tl' = hoist_loops_in_stmts tl in
  match hd.literal with
  | Expr e -> 
    let! exprs = hoist_loops_in_expr e [] in
    let loops = List.Tot.map (fun e -> { literal = LoopExpr e; with_mods = []}) exprs in
    let pfx = List.Tot.append loops [hd] in
    return <| List.Tot.append pfx tl'
  | _ -> return <| List.Tot.append [hd] tl' 


let hoist_loops_in_query q : iresult query
= let! stmts = hoist_loops_in_stmts q.stmts in
  return { stmts = stmts }

let lookup_var (ident:string)
: iresult value
= lookup_local_var ident

let rec eval_expr (e:expr)
: iresult value
= match! lookup_loop_expr e with
  | Some((_, v)) -> return v
  | None ->
    match e with
    | Var v -> lookup_var v
    | Value v -> return v
    | Ast.Array elems -> eval_array_elems elems []
    | Ast.Set elems -> eval_set_elems elems [] 
    | Ast.Object fields -> eval_fields fields (Object [])
    | Ast.ArrayCompr query -> eval_query_stmts query.stmts (Value.Array [])
    | Ast.SetCompr query -> eval_query_stmts query.stmts (Value.Set [])
    | Ast.ObjectCompr query -> eval_query_stmts query.stmts (Value.Object [])
    | Ast.RefDot rf -> eval_ref_dot rf
    | Ast.RefBrack ri -> eval_ref_brack ri
    | Ast.AssignExpr olr -> eval_assign_expr olr
    | _ -> return Undefined

and eval_array_elems (elems:list expr) (values:list value)
: iresult value
= match elems with
  | [] -> return <| Value.Array (List.Tot.rev values)
  | hd::tl ->
    match! eval_expr hd with
    | Undefined -> return Undefined
    | v -> eval_array_elems tl (v::values)

and eval_set_elems (elems:list expr) (values:list value)
: iresult value
= match elems with
  | [] -> return <| List.Tot.fold_left Value.insert_into_set (Value.Set []) values
  | hd::tl ->
    match! eval_expr hd with
    | Undefined -> return Undefined
    | v -> eval_set_elems tl (v::values)

and eval_fields (fields:list (expr*expr)) (obj:value)
: iresult value
= match fields with
  | [] -> return obj
  | (key_expr,value_expr)::tl ->
    match! eval_expr key_expr with
    | Undefined -> return Undefined
    | kv ->
      match! eval_expr value_expr with
      | Undefined -> return Undefined
      | vv -> eval_fields tl (insert_into_object obj kv vv)


and eval_stmts_in_loop (iterations:list loop_iteration) (stmts:list literalStmt) (out:value)
: Tot (iresult value) (decreases %[stmts; 1; iterations])
  (* what decreases is the lexicographic ordering of the list of statements and the list of loop expressions, in that order;
     however, we also make a recursive call to an auxiliary eval_query_stmts, and that explains the additional '1' in the order, 
     i.e., we can call the auxiliary function with the same stmts, so long as that aux function doesn't
     call us back without decreasing the stmts itself.

     See the "trick" mentioned in the note here: https://fstar-lang.org/tutorial/book/part1/part1_termination.html#mutual-recursion
      *)
= 
match iterations with
| [] -> return out
| hd::tl -> 
  let! orig_i = get() in
  set { orig_i with loop_exprs = (hd.loop_expr, hd.loop_value)::orig_i.loop_exprs;
       scopes=hd.bindings::orig_i.scopes};!
  let! out1 = eval_query_stmts stmts out in
  set orig_i ;!
  eval_stmts_in_loop tl stmts out1

and eval_stmt stmt (out:value) 
: iresult (value&value)
= match stmt.literal with
    | ArrayComprOutput e ->
      let! v = eval_expr e in
      return <| (Bool true, insert_into_array out v)
    | SetComprOutput e ->
      let! v = eval_expr e in
      return <| (Bool true, insert_into_set out v)
    | ObjectComprOutput (ke, ve) ->
      let! kv = eval_expr ke in
      let! vv = eval_expr ve in
      return <| (Bool true, insert_into_object out kv vv)
    | Expr e -> 
      let! v = eval_expr e in
      return <| (v, out)
    | _ -> 
      return (Undefined, Undefined)


and eval_query_stmts (stmts:list literalStmt) (out:value)
  : Tot(iresult value) (decreases %[stmts;0])
= match stmts with
  | [] -> return out
  | hd::tl ->
    match hd.literal with
    | LoopExpr le ->
      let! iterations = match le with
      | RefBrack (refr, Var index) ->
        let! col = eval_expr refr in
        (match col with
        | Array arr -> return <| make_array_loop_iterations arr le index 0
        | Set set -> return <| make_set_loop_iterations set le index
        | Object fields -> return <| make_object_loop_iterations fields le index
        | _ -> return [])
      | _ -> return []
      in eval_stmts_in_loop iterations tl out
    | _ ->
      let! (v, out') = eval_stmt hd out in
      match v with
      | Bool false | Undefined -> return Undefined
      | _ -> eval_query_stmts tl out'
  
(* todo chaining *)
and eval_ref_dot rf
: iresult value
= let refr, field = rf in
  let! v = eval_expr refr in
  return <| lookup_path v [Value.String field]

(* todo chaining *)
and eval_ref_brack (pr:expr*expr)
: Tot (iresult value)
= let refr, index = pr in
  let! v = eval_expr refr in
  let! idx = eval_expr index in
  return <| lookup_path v (idx::[])

(* todo...actual implementation is quite complex *)
and eval_assign_expr (t: (assignOp*expr*expr))
: iresult value
= let (_, lhs, rhs) = t in
  match lhs with
  | Var name -> (
    let! rv = eval_expr rhs in
    match rv with
    | Undefined ->
      return Undefined
    | _ -> (* todo check existing binding *)
      let! i = get() in
      match i.scopes with
      | hd::tl -> 
        set {i with scopes = ((name, rv)::hd)::tl};!
        return <| Bool true
      | _ -> 
        return Undefined
    )
  | _ ->
    return Undefined

let eval (i:intr) (e:Ast.expr) : (value*intr) =
  run (eval_expr e) i

let eval_user_query' (q:Ast.query)
: iresult value
= let! i = get() in
  set { i with scopes = []::i.scopes } ;!
  let! q1 = hoist_loops_in_query q in
  eval_query_stmts q1.stmts (Value.Array [])

let eval_user_query (i:intr) (q:Ast.query) : (value*intr) =
  run (eval_user_query' q) i
