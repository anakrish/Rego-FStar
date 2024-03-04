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
  FStar.IO.print_string "\n"
  
//Run ``main ()`` when the module loads
#push-options "--warn_error -272"
let _ = main ()
#pop-options
