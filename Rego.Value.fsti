// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Value

  [@@ PpxDerivingYoJson; PpxDerivingShow ]
  type value:eqtype =
    | Null
    | Bool of bool
    | Number of int (* TODO: number can be i64, u64, f64 or big decimal *)
    | String of string
    | Array of list value
    | Set of list value
    | Object of list (value * value)
    | Undefined

  type ord = x:int { x = -1 || x = 0 || x = 1}
  
  val cmp (v1:value) (v2:value) : ord
  
  val insert_into_object (obj:value) (key:value) (v:value) : value

  val insert_into_set (set:value) (item:value) : value

  val lookup_path (obj:value) (path:list value) : value

  val insert_at_path (obj:value) (path: list value) (v:value) : value

  val to_json (v:value) : string 
  val to_json_pretty (v:value) : string
