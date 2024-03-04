// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Interpreter

type scope = FStar.Map.t string Value.value

noeq
type interpreter = {
  rules: list (string*Ast.rule);
  scopes: list scope;
}

val make_new () : interpreter

val eval (i:interpreter) (e:Ast.expr) : (Value.value*interpreter)
