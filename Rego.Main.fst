// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Main

open Rego.Ast
open Rego.Interpreter
open Rego.Value


let v1 = insert_at_path (Object []) (String "hello"::String "world"::[]) (Number 42)
let v2 = insert_at_path v1 (String "monde"::[]) v1
let v3 = insert_at_path v2 (v2::[]) v2
let v4 = Array (v1::v2::v3::[])

let expr = Ast.Array (
  Ast.Value (String "hello")::
  Ast.Value (String "world")::
  Ast.Array (
    Ast.Value (Number 42)::
    Ast.Set (
      Ast.Value (Number 3)::
      Ast.Value (Number 2)::
      Ast.Value (Number 1)::
      Ast.Object (
        (Ast.Value (String "Hello"), Ast.Value (String "World"))
        ::[]
      )::
      []
    )::[]
  )::[]
)

let intr = Interpreter.make_new()
let (v5, intr1) = Interpreter.eval intr expr

// [ [e, f, g] |
//  true
//  a = {"English": "Hello", "French": "Salut"}
//  b = a["English"]
//  c = [41, 47]
//  d = [a , v]
//  e = d[f][g] # Double loops
// ]
// Produces:
// [ ["Hello", 0, "English"], ["Salut", 0, "French"], [41, 1, 0], [47, 1, 1]]
let query = {
  stmts= [
    { literal = Expr(Value(Bool true)); with_mods=[]; }; // true
    { literal =
       Expr(AssignExpr
             (ColEq,
              (Var "a"),
              (Ast.Object [
                (Value (String "English"), Value (String "Hello"));
                (Value (String "French"), Value (String "Salut"));
               ]))
       );
       with_mods=[];
    };
    {
      literal =
        Expr(AssignExpr
             (ColEq,
              (Var "b"),
              (RefBrack ((Var "a"), (Value (String "English")))))

        );
      with_mods=[];
    };
    {
      literal =
        Expr(AssignExpr
             (ColEq,
              (Var "c"),
              (Ast.Array [Value (Number 41); Value (Number 47)]))

        );
      with_mods=[];
    };
    {
      literal =
        Expr(AssignExpr
             (ColEq,
              (Var "d"),
              (Ast.Array [Var "a"; Var "c"]))

        );
      with_mods=[];
    };
    {
      literal =
        Expr(AssignExpr
             (ColEq,
              (Var "e"),
              (RefBrack (RefBrack (Var "d", Var "f"), (Var "g"))))

        );
      with_mods=[];
    };
    { literal = ArrayComprOutput (Ast.Array [Var "e"; Var "f"; Var "g"]);
      with_mods = [];
    }
  ];
}

let intr2 = Interpreter.make_new()
let (v6, intr3) = Interpreter.eval_user_query intr2 query

let query1 = {
  stmts = List.Tot.append query.stmts
  [{
    literal = Expr(Value (Bool false)); with_mods=[];
  }]
}

let intr4 = Interpreter.make_new()
let (v7, intr5) = Interpreter.eval_user_query intr4 query1


let main () =
  FStar.IO.print_string (to_json_pretty v1);
  FStar.IO.print_string "\n";
  FStar.IO.print_string (to_json_pretty v2);
  FStar.IO.print_string "\n";
  FStar.IO.print_string (to_json_pretty v3);
  FStar.IO.print_string "\n";
  FStar.IO.print_string (to_json_pretty v4);
  FStar.IO.print_string "\n";
  FStar.IO.print_string (to_json_pretty v5);
  FStar.IO.print_string "\n";
  FStar.IO.print_string (to_json_pretty v6);
  FStar.IO.print_string "\n";
  FStar.IO.print_string (to_json_pretty v7);
  FStar.IO.print_string "\n"

//Run ``main ()`` when the module loads
#push-options "--warn_error -272"
let _ = main ()
#pop-options
