type game =
    { name : string;
      path : string;
      glob : string option }

type config =
    { path : string;
      frequency : int;
      numToKeep : int;
      games : game list }

type 'a app = 'a
