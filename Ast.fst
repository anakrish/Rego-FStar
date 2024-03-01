// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Ast

  type span = {
    file: string; (* name of rego file *)
    line: n:int { n >= 1 };      (* line and  column numbers start at 1 *)
    col: n:int { n >= 1 };
    start: n:int { n > 0 && n < 65536 }; (* support only 65K file *)
    end_: n:int { n > 0 && n < 65536 };
  }

  and binOp =
    | And
    | Or


  and  arithOp =
    | Add
    | Sub
    | Mul
    | Div
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


  and expr =
    | String of string
    | RawString of span
    | Number of span
    | True_ of span
    | False_ of span
    | Null of span
    | Var of span
    | Array of span * (items:list expr)
    | Set of span * (items:list expr)
    | Object of span * (fields:list (span * expr * expr))
    | ArrayCompr of span * (term:expr) * query
    | SetCompr of span * (term:expr) * query
    | ObjectCompr of span * (key:expr) * (value:expr) * query
    | Call of span * (fcn:expr) * (params:list expr)
    | UnaryExpr of span * expr
    | RefDot of span * (refr:expr) * (field:span)
    | RefBrack of span * (refr:expr) * (index:expr)
    | BinExpr of span * binOp * (lhs:expr) * (rhs:expr)
    | BoolExpr of span * boolOp * (lhs:expr) * (rhs:expr)
    | ArithExpr of span * arithOp * (lhs:expr) * (rhs:expr)
    | AssignExpr of span * assignOp * (lhs:expr) * (rhs:expr)
    | MembershipExpr of span * (key:expr) * (value:expr) * (collection:expr)

  and literal =
    | SomeVars of span * (var:list span)
    | SomeIn of span * (key:option expr) * (value:expr) * (collection:expr)
    | Expr of span * expr
    | NotExpr of span * (a:expr)
    | Every of span * (key:option span) * (value:span) * (domain:expr) * query

  and withModifier =  {
      span: span;
      refr: expr;
      as_: expr;
  }

  and literalStmt = {
      span: span;
      literal: literal;
      with_mods: list withModifier;
  }

  and query = {
    span: span;
    stmts: list literalStmt
  }

  and ruleAssign = {
    span: span;
    op: assignOp;
    value: expr;
  }

  and ruleBody = {
    span: span;
    assign: option ruleAssign;
    query: query;
  }

  and ruleHead =
    | Compr of span * (refr:expr) * (assign: option ruleAssign)
    | Set_ of span * (refr:expr) * (key:option expr)
    | Func of span * (refr:expr) * (args:list expr) * (assign: option ruleAssign)

  and rule =
    | Spec of span * (head:ruleHead) * (bodies: list ruleBody)
    | Default of span * (refr:expr) * (args:list expr) * (op:assignOp) * (value:expr)

  and package = {
    span: span;
    refr: expr;
  }

  and import = {
    span_: span; (* naming this field span causes a compile error *)
    refr: expr;
    as_: option span;
  }

  and regoModule = {
    package: package;
    imports: list import;
    policy: list rule;
    rego_v1: bool;
  }
