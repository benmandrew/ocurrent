open Tyxml.Html
module Db = Current_cache.Db

module Job = Current.Job

let render_row (id, job) =
  let url = Fmt.str "/job/%s" id in
  let start_time =
    match Lwt.state (Job.start_time job) with
    | Lwt.Sleep -> "(ready to start)"
    | Lwt.Return t -> Utils.string_of_timestamp (Unix.gmtime t)
    | Lwt.Fail f -> Printexc.to_string f
  in
  tr [
    td [ a ~a:[a_href url] [txt id] ];
    td [ txt start_time ];
  ]

let date_tip = "Actually, any prefix of the job ID can be used here."

let r = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let uri = Context.uri ctx in
    let ok = Common.bool_param "ok" uri in
    let op = Common.string_param "op" uri in
    let date = Common.string_param "date" uri in
    let ops = Db.ops () in
    let jobs = Current.Job.jobs () in
    Context.respond_ok ctx ?refresh:ctx.site.refresh_pipeline 
      [
        form ~a:[a_action "/jobs"; a_method `Get] [
          ul ~a:[a_class ["query-form"]] [
              li [txt "Operation type:"; Common.enum_option ~choices:ops "op" op];
              li [txt "Result:"; Common.bool_option "ok" ok ~t:"Running" ~f:"Ready"];
              li [txt "Date:"; Common.string_option "date" date ~placeholder:"YYYY-MM-DD" ~title:date_tip];
              li [input ~a:[a_input_type `Submit; a_value "Submit"] ()];
            ];
        ];
        form ~a:[a_action "/query"; a_method `Post] [
            table ~a:[a_class ["table"]]
            ~thead:(thead [
                tr [
                  th [txt "Job"];
                  th [txt "Start time"];
                ]
              ])
            (Current.Job.Map.bindings jobs |> List.map render_row)
          ]
      ]

  method! nav_link = Some "Jobs"
end
