// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rego.Value

open FStar.String

let rec array_cmp (l1 l2: list value) : Tot ord (decreases l1) =
  match l1, l2 with
  | hd1 :: tl1, hd2 :: tl2 ->
    (match cmp hd1 hd2 with
      | 0 -> array_cmp tl1 tl2
      | r -> r)
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> - 1

and object_cmp (l1 l2: list (value * value)) : Tot ord (decreases l1) =
  match l1, l2 with
  | (k1, v1) :: tl1, (k2, v2) :: tl2 ->
    (match cmp k1 k2 with
      | 0 ->
        (match cmp v1 v2 with
          | 0 -> object_cmp tl1 tl2
          | r -> r)
      | r -> r)
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> - 1

and cmp (v1 v2: value) : ord =
  match v1, v2 with
  | Null, Null -> 0
  | Null, _ -> - 1 (* Null is lesser than other values *)
  | _, Null -> 1
  | Bool b1, Bool b2 ->
    (match b1, b2 with
      | false, false -> 0
      | false, _ -> - 1
      | true, true -> 0
      | true, _ -> 1)
  | Bool _, _ -> - 1 (* Bool is greater than Null *)
  | _, Bool _ -> 1
  | Number n1, Number n2 -> if n1 > n2 then 1 else if n1 = n2 then 0 else - 1
  | Number _, _ -> - 1 (* Numner is greater than Bool *)
  | _, Number _ -> 1
  | String s1, String s2 ->
    (match String.compare s1 s2 with
      | 1 -> 1
      | 0 -> 0
      | _ -> - 1)
  | String _, _ -> - 1 (* String is greater than Number *)
  | _, String _ -> 1
  | Array a1, Array a2 -> array_cmp a1 a2
  | Array _, _ -> - 1 (* Array is greater than String *)
  | _, Array _ -> 1
  | Set s1, Set s2 -> array_cmp s1 s2
  | Set _, _ -> - 1 (* Set is greater than Array *)
  | _, Set _ -> 1
  | Object o1, Object o2 -> object_cmp o1 o2
  | Object _, Undefined -> - 1 (* Object is greater than Set *)
  | Undefined, Object _ -> 1
  | Undefined, Undefined -> 0

let insert_into_array (arr v: value) : value =
  match arr with
  | Array a -> Array (List.Tot.append a [v])
  | _ -> Undefined

let rec insert_into_fields (fields: list (value * value)) (key value: value) =
  match fields with
  | (k, v) :: tl ->
    (match cmp k key with
      | 0 -> (key, value) :: tl
      | 1 -> (key, value) :: (k, v) :: tl
      | -1 -> (k, v) :: insert_into_fields tl key value)
  | _ -> [(key, value)]

let insert_into_object (obj key value: value) =
  match obj with
  | Object fields -> Object (insert_into_fields fields key value)
  | _ -> Undefined

let rec insert_into_set_items (items: list value) (item: value) =
  match items with
  | v :: tl ->
    (match cmp v item with
      | 0 -> v :: tl
      | 1 -> item :: v :: tl
      | -1 -> v :: insert_into_set_items tl item)
  | _ -> [item]

let insert_into_set (set item: value) : value =
  match set with
  | Set items -> Set (insert_into_set_items items item)
  | _ -> Undefined

let rec lookup_field (fields: list (value * value)) (key: value) : value =
  match fields with
  | (k, v) :: tl -> (if (cmp k key) = 0 then v else lookup_field tl key)
  | _ -> Undefined

let rec lookup_path (obj: value) (path: list value) : Tot value (decreases (List.length path)) =
  match obj, path with
  | Undefined, _ -> Undefined
  | _, [] -> obj
  | Object fields, field :: tl ->
    (match lookup_field fields field with
      | Undefined -> Undefined
      | obj -> lookup_path obj tl)
  | _ -> Undefined

let rec insert_at_path (obj: value) (path: list value) (v: value)
    : Tot value (decreases (List.length path)) =
  match obj, path with
  | Object _, p :: [] -> insert_into_object obj p v
  | Object _, p :: tl ->
    (match lookup_path obj ([p]) with
      | Undefined ->
        (* The key p does not exist in obj *)
        let subobj = insert_at_path (Object []) tl v (* create subobject *) in
        (* insert subobj as field p into obj *)
        insert_into_object obj p subobj
      | subobj ->
        (* The key p exists in obj *)
        let subobj1 = insert_at_path subobj tl v (* insert v at path tl into subobj *) in
        (match subobj1 with
          | Undefined -> Undefined (* insertion failed *)
          | _ ->
            (* insertion succeeded. Insert subobj as field p in obj *)
            insert_into_object obj p subobj))
  | _ ->
    (* not an object or invalid path *)
    Undefined

let escape_char (c: FStar.Char.char) : list char =
  match c with
  | '\n' -> ['\\'; 'n']
  | '\r' -> ['\\'; 'r']
  | '\t' -> ['\\'; 't']
  | '"' -> ['\\'; '"']
  | _ -> [c]

let escaped (s: string) : Tot string =
  String.string_of_list (List.Tot.collect escape_char (String.list_of_string s))

let rec array_elem_to_json (a: list value) : Tot (list string) (decreases a) =
  match a with
  | [] -> []
  | hd :: tl -> to_json hd :: array_elem_to_json tl

and field_to_json (a: list (value * value)) : Tot (list string) (decreases a) =
  match a with
  | [] -> []
  | (k, v) :: tl ->
    let k_json =
      match k with
      | String s -> Printf.sprintf "\"%s\"" (escaped s)
      | _ -> Printf.sprintf "\"%s\"" (escaped (to_json k))
    in
    let v_json = to_json v in
    (Printf.sprintf "%s: %s" k_json v_json) :: field_to_json tl

and to_json (v: value) : Tot string (decreases v) =
  match v with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number n -> string_of_int n
  | String s -> Printf.sprintf "\"%s\"" s
  | Array a
  | Set a ->
    let elems_json = array_elem_to_json a in
    Printf.sprintf "[%s]" (String.concat "," elems_json)
  | Object fields ->
    let flds_json = field_to_json fields in
    Printf.sprintf "{%s}" (String.concat "," flds_json)
  | Undefined -> "<undefined>"

let rec array_elem_to_json_p (a: list value) (indent: string) : Tot (list string) (decreases a) =
  match a with
  | [] -> []
  | hd :: tl -> to_json_p_impl hd indent :: array_elem_to_json_p tl indent

and field_to_json_p (a: list (value * value)) (indent: string) : Tot (list string) (decreases a) =
  match a with
  | [] -> []
  | (k, v) :: tl ->
    let k_json =
      match k with
      | String s -> Printf.sprintf "%s\"%s\"" indent (escaped s)
      | _ -> Printf.sprintf "%s\"%s\"" indent (escaped (to_json k))
    in
    let v_json = to_json_p_impl v indent in
    (Printf.sprintf "%s: %s" k_json v_json) :: field_to_json_p tl indent

and to_json_p_impl (v: value) (indent: string) : Tot string (decreases v) =
  match v with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number n -> string_of_int n
  | String s -> Printf.sprintf "\"%s\"" s
  | Array a
  | Set a ->
    let indent' = indent ^ "  " in
    let elems_json = array_elem_to_json_p a indent' in
    Printf.sprintf "[\n%s%s\n%s]" indent' (String.concat (",\n" ^ indent') elems_json) indent
  | Object fields ->
    let flds_json = field_to_json_p fields (indent ^ "  ") in
    Printf.sprintf "{\n%s\n%s}" (String.concat ",\n" flds_json) indent
  | Undefined -> "<undefined>"

let to_json_pretty (v: value) : string = to_json_p_impl v ""