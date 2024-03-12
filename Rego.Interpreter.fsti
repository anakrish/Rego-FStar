// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Interpreter

open Rego.Ast

open Rego.Value

type scope = list (string * value)

type array_compr_ctx = {
  ac_output_expr:expr;
  ac_value:value
}

type context =
  | ArrayComprCtx of array_compr_ctx
  | NoneCtx

noeq
type interpreter = {
  rules:list (string * rule);
  scopes:list scope;
  loop_exprs:list (expr * value)
}

val make_new (_: unit) : interpreter

val eval (i: interpreter) (e: expr) : (value * interpreter)

val eval_user_query (i: interpreter) (q: query) : (value * interpreter)