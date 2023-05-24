open Tyxml.Html
module Db = Current_cache.Db

module Job = Current.Job

let running_param name uri =
  match Uri.get_query_param uri name with
  | None | Some "" -> None
  | Some "running" -> Some true
  | Some "ready" -> Some false
  | Some x -> Fmt.failwith "Invalid value %S in %a: must be one of 'running' or 'ready'" x Uri.pp uri

let render_row (id, _job) =
  (* let url = Fmt.str "/job/%s" id in
  let start_time =
    match Lwt.state (Job.start_time job) with
    | Lwt.Sleep -> "(ready to start)"
    | Lwt.Return t -> Utils.string_of_timestamp (Unix.gmtime t)
    | Lwt.Fail f -> Printexc.to_string f
  in
  tr [
    td [ a ~a:[a_href url] [txt id] ];
    td [ txt start_time ];
  ] *)
  tr [
    td [ a ~a:[a_href "URL"] [txt id] ];
    td [ txt "At some point" ];
  ]

let date_tip = "Actually, any prefix of the job ID can be used here."

let filter_jobs ?running ?op ?date (jobs : Job.t Job.Map.t) =
  let optional_filter f input =
    match Option.map f input with
    | Some b -> b
    | None -> true
  in
  let f (id : string) (job : Job.t) =
    let open Astring in
    optional_filter (fun op -> String.is_infix ~affix:op id) op &&
    optional_filter (fun date -> String.is_prefix ~affix:date id) date &&
    optional_filter (fun running ->
      match Lwt.state (Job.start_time job) with
      | Lwt.Sleep when not running -> true (* Ready to run *)
      | Lwt.Return _ when running -> true (* Running *)
      | _ -> false) running
  in
  Job.Map.filter f jobs

let sort_jobs jobs =
  Job.Map.bindings jobs
  |> List.sort (fun (id0, _) (id1, _) -> String.compare id0 id1)

let jobs_per_page = 3

let paginate_jobs page_idx jobs =
  let rec slice l start len =
    match l, start, len with
    | _, _, 0 -> []
    | x :: l, 0, _  -> x :: (slice l 0 (len - 1))
    | _ :: l, n, _ -> slice l (n - 1) len
    | [], _, _ -> []
  in
  slice jobs ((page_idx - 1) * jobs_per_page) jobs_per_page

let paging_buttons page_idx total_pages =
  let input ?(disabled = false) ?(selected_page = false) ?str_override page_idx =
    let i = string_of_int page_idx in
    let s = Option.value ~default:i str_override in
    let a =
      [a_button_type `Submit; a_name "page"; a_value i]
      @ if disabled then [a_disabled ()] else []
      @ if selected_page then [
          a_style "background-color: #EC7D0B; border-top-width: 0px; border-bottom-width: 0px"
        ] else []
    in
    button ~a:(Obj.magic a) [ txt s ];
  in
  let prefix =
    if page_idx = 1 then
      [input (page_idx - 1) ~disabled:true ~str_override:"«"]
    else if page_idx = 2 then [
      input (page_idx - 1) ~str_override:"«";
      input (page_idx - 1);
    ]
    else [
      input (page_idx - 1) ~str_override:"«";
      input (page_idx - 2);
      input (page_idx - 1);
    ]
  in
  let suffix =
    if page_idx = total_pages then
      [input (page_idx + 1) ~disabled:true ~str_override:"»"]
    else if page_idx = total_pages - 1 then [
      input (page_idx + 1);
      input (page_idx + 1) ~str_override:"»";
    ]
    else [
      input (page_idx + 1);
      input (page_idx + 2);
      input (page_idx + 1) ~str_override:"»";
    ]
  in
  let content =
    prefix
    @ [input page_idx ~selected_page:true]
    @ suffix
  in
  div ~a:[a_aria "label" ["pagination"]] content

let r = object
  inherit Resource.t

  val! can_get = `Viewer
  val! can_post = `Builder

  method! private get ctx =
    let uri = Context.uri ctx in
    let running = running_param "running" uri in
    let op = Common.string_param "op" uri in
    let date = Common.string_param "date" uri in
    (* Page number is 1-indexed *)
    let page = Option.value ~default:1 @@ Common.nat_param "page" uri in
    let ops = Db.ops () in
    let job_map =
      Job.jobs ()
      |> filter_jobs ?running ?op ?date
    in
    let jobs =
      sort_jobs job_map
      |> paginate_jobs page
    in
    let n_pages = ((Job.Map.cardinal job_map - 1) / jobs_per_page) + 1 in
    let content =
      if jobs = [] then
        [ txt "No jobs satisfy these criteria." ]
      else
        [
          form ~a:[a_action "/jobs"; a_method `Post] (
            [
              table ~a:[a_class ["table"]]
              ~thead:(thead [
                tr [
                  th [txt "Job"];
                  th [txt "Start time"];
                ]])
              (List.map render_row jobs);
            ]
          );
        ]
    in
    let paging =
      if jobs = [] then [ div [] ]
      else
        [
          form ~a:[a_action "/jobs"; a_method `Get] [
            paging_buttons page n_pages
          ]
        ]
    in
    Context.respond_ok ctx ?refresh:ctx.site.refresh_pipeline (
      [
        form ~a:[a_action "/jobs"; a_method `Get] [
          ul ~a:[a_class ["query-form"]] [
            li [txt "Operation type:"; Common.enum_option ~choices:ops "op" op];
            li [txt "Result:"; Common.bool_option "running" running ~t:"Running" ~f:"Ready"];
            li [txt "Date:"; Common.string_option "date" date ~placeholder:"YYYY-MM-DD" ~title:date_tip];
            li [input ~a:[a_input_type `Submit; a_value "Submit"] ()];
          ];
        ];
      ] @ content @ paging)

  method! nav_link = Some "Jobs"
end
