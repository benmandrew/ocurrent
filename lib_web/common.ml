open Tyxml.Html

let bool_param name uri =
  match Uri.get_query_param uri name with
  | None | Some "" -> None
  | Some "true" -> Some true
  | Some "false" -> Some false
  | Some x -> Fmt.failwith "Invalid bool value %S in %a" x Uri.pp uri

let string_param name uri =
  match Uri.get_query_param uri name with
  | None | Some "" -> None
  | Some x -> Some x

let nat_param name uri =
  match Uri.get_query_param uri name with
  | None | Some "" -> None
  | Some x ->
    match int_of_string_opt x with
    | Some x when x >= 1 -> Some x
    | _ -> Fmt.failwith "Invalid value %S in %a" x Uri.pp uri

let bool_table ~t ~f = [
  None,       "",      "(any)";
  Some true,  "true",  t;
  Some false, "false", f;
]

let bool_option ?(t="True") ?(f="False") name value =
  select ~a:[a_name name] (
    bool_table ~t ~f |> List.map (fun (v, form_value, label) ->
        let sel = if v = value then [a_selected ()] else [] in
        option ~a:(a_value form_value :: sel) (txt label)
      )
  )

let string_option ~placeholder ~title name value =
  let value = Option.value value ~default:"" in
  input ~a:[a_name name; a_input_type `Text; a_value value; a_placeholder placeholder; a_title title] ()


let enum_option ~choices name (value:string option) =
  let value = Option.value value ~default:"" in
  let choices = "" :: choices in
  select ~a:[a_name name] (
    choices |> List.map (fun form_value ->
        let sel = if form_value = value then [a_selected ()] else [] in
        let label = if form_value = "" then "(any)" else form_value in
        option ~a:(a_value form_value :: sel) (txt label)
      )
  )
