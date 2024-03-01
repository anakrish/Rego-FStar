// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Value
open FStar.List
open FStar.String

  type value =
    | Null
    | Bool of bool
    | Number of int
    | String of string
    | Array of list value
    | Set of list value
    | Object of list (value * value)
    | Undefined

  type ord = x:int { x = -1 || x = 0 || x = 1}
  
  let rec array_cmp (l1:list value) (l2:list value) : ord =
    match l1, l2 with
      | hd1::tl1, hd2::tl2  ->  
        (match cmp hd1 hd2 with
          | 0 -> array_cmp tl1 tl2
          | r -> r)
      | [], [] -> 0      
      | _ , [] -> 1
      | [], _ -> -1
      
  and object_cmp (l1: list (value * value)) (l2 : list (value*value)) : ord =
    match l1, l2 with
      | (k1, v1)::tl1, (k2, v2)::tl2 -> 
        (match cmp k1 k2 with
          | 0 -> 
            (match cmp v1 v2 with
              | 0 -> object_cmp tl1 tl2
              | r -> r)
          | r -> r)
      | [], [] -> 0          
      | _ , [] -> 1
      | [], _ -> -1
      
  and cmp (v1:value) (v2:value) : ord =
    match v1, v2 with 
      | Null, Null -> 0
      | Null, _ -> -1  (* Null is lesser than other values *)
      | _, Null -> 1

      | Bool b1, Bool b2 -> 
        (match b1, b2 with
          | false, false -> 0
          | false, _ -> -1
          | true, true -> 0
          | true, _ -> 1)
      | Bool _, _ -> -1  (* Bool is greater than Null *)
      | _, Bool _ -> 1


      | Number n1, Number n2 ->
        if n1 > n2 then 1 else if n1 = n2 then 0 else -1
      | Number _, _ -> -1  (* Numner is greater than Bool *)
      | _, Number _ -> 1

      | String s1, String s2 -> 
        (match compare s1 s2 with
          | 1 -> 1
          | 0 -> 0
          | _ -> -1)
      | String _, _ -> -1  (* String is greater than Number *)
      | _, String _ -> 1

      | Array a1, Array a2 -> array_cmp a1 a2
      | Array _, _ -> -1  (* Array is greater than String *)
      | _, Array _ -> 1
      
      | Set s1, Set s2 -> array_cmp s1 s2
      | Set _, _ -> -1   (* Set is greater than Array *)
      | _, Set _ -> 1

      | Object o1, Object o2 -> object_cmp o1 o2
      | Object _, Undefined -> -1 (* Object is greater than Set *)
      | Undefined, Object _ -> 1

      | Undefined, Undefined -> 0 


  let insert_into_object (obj:value) (key:value) (value:value) =
    let rec insert_into fields = 
      match fields with
        | (k, v)::tl -> 
          (match cmp k key with
            | 0 -> (key, value)::tl
            | 1 -> (key,value)::(k, v)::tl
            | -1 -> (k,v)::insert_into tl)
        | _ -> (key, value)::[]         
    in
    match obj with 
      | Object fields -> Object (insert_into fields)
      | v -> v

  let insert_into_set (set:value) (item:value) =
    let rec insert_into items = 
      match items with
        | v::tl -> 
          (match cmp v item with
            | 0 -> v::tl
            | 1 -> item::v::tl
            | -1 -> (v::insert_into tl))
        | _ -> item::[]         
    in
    match set with 
      | Set items -> Set (insert_into items)
      | v -> v
