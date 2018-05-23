let apply f v =
  Result.bind (fun f' ->
    Result.bind (fun v' ->
      Ok (f' v')) v) f

let lift2 f v1 v2 =
  apply (apply (Ok f) v1) v2