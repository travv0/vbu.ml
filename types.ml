open Base

type game = { name : string; path : string; glob : string option }

module Game = struct
  let to_json { name; path; glob } =
    `Assoc
      [ ("name", `String name)
      ; ("path", `String path)
      ; ( "glob"
        , glob
          |> Option.map ~f:(fun a -> `String a)
          |> Option.value ~default:`Null )
      ]

  let of_json json =
    let open Yojson.Basic.Util in
    { name = json |> member "name" |> to_string
    ; path = json |> member "path" |> to_string
    ; glob = json |> member "glob" |> to_string_option
    }
end

type config =
  { path : string; frequency : int; num_to_keep : int; games : game list }

module Config = struct
  let to_json { path; frequency; num_to_keep; games } =
    `Assoc
      [ ("path", `String path)
      ; ("frequency", `Int frequency)
      ; ("num_to_keep", `Int num_to_keep)
      ; ("games", `List (List.map games ~f:Game.to_json))
      ]

  let of_json json =
    let open Yojson.Basic.Util in
    { path = json |> member "path" |> to_string
    ; frequency = json |> member "frequency" |> to_int
    ; num_to_keep = json |> member "num_to_keep" |> to_int
    ; games = json |> member "games" |> to_list |> List.map ~f:Game.of_json
    }
end
