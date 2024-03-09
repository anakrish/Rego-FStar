// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Ast

  #push-options "--warn_error -331"
  type binOp =
    | And
    | Or

  and  arithOp =
    | Add
    | Sub
    | Mul
    | Divv
    | Mod

  and boolOp =
    | Lt
    | Le
    | Eq
    | Ge
    | Gt
    | Ne

  and assignOp =
    | EqAssign  (* Changed name to differentiate from Assign in boolOp *)
    | ColEq


  and ident = string

  (* without eqtype annotation "Failed to solve universe inequalities for inductives" *)
  and expr:eqtype = 
    | Value of Rego.Value.value
    | Var of ident
    | Array of (items:list expr)
    | Set of (items:list expr)
    | Object of (fields:list (expr * expr))
    | ArrayCompr of query
    | SetCompr of query
    | ObjectCompr of query
    | Call of (fcn:expr) * (params:list expr)
    | UnaryExpr of expr
    | RefDot of (refr:expr) * (field:string)
    | RefBrack of (refr:expr) * (index:expr)
    | BinExpr of binOp * (lhs:expr) * (rhs:expr)
    | BoolExpr of boolOp * (lhs:expr) * (rhs:expr)
    | ArithExpr of arithOp * (lhs:expr) * (rhs:expr)
    | AssignExpr of assignOp * (lhs:expr) * (rhs:expr)
    | MembershipExpr of (key:expr) * (value:expr) * (collection:expr)

  and literal =
    | SomeVars of (var:list ident)
    | SomeIn of (key:option expr) * (value:expr) * (collection:expr)
    | Expr of expr
    | NotExpr of (a:expr)
    | Every of (key:option ident) * (value:ident) * (domain:expr) * query
    | ArrayComprOutput of (e:expr)
    | SetComprOutput of (e:expr)
    | ObjectComprOutput of (k:expr) * (v:expr)
    | LoopExpr of expr

  and withModifier =  {
      refr: expr;
      as_: expr;
  }

  and literalStmt = {
      literal: literal;
      with_mods: list withModifier;
  }

  and query = {
    stmts: list literalStmt
  }

  and ruleAssign = {
    op: assignOp;
    value: expr;
  }

  and ruleBody = {
    assign: option ruleAssign;
    query: query;
  }

  and ruleHead =
    | Compr of (refr:expr) * (assign: option ruleAssign)
    | Set_ of (refr:expr) * (key:option expr)
    | Func of (refr:expr) * (args:list expr) * (assign: option ruleAssign)

  and rule =
    | Spec of (head:ruleHead) * (bodies: list ruleBody)
    | Default of (refr:expr) * (args:list expr) * (op:assignOp) * (value:expr)

  and package = {
    refr: expr;
  }

  and import = {
    refr: expr;
    as_: option ident;
  }

  and regoModule = {
    package: package;
    imports: list import;
    policy: list rule;
    rego_v1: bool;
  }

 #pop-options
