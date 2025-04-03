module Strategy = struct
  module Opening = struct
    type t = ManAtArms | Archer | Scout
  end

  module FeudalUnit = struct
    type t = Boom | Archer | Scout | ManAtArms | Spear
  end

  module CastleUnit = struct
    type t =
      (* | Uu *)
      | Crossbow
      | Knight
      | Monk
      | Camel
      | Skirmisher
      | Pikeman
      | CavalryArcher
      | Mangonel
      | Longsword
      (* | Ram *)
      | LightCavalry
      | Scorpion
    [@@deriving show { with_path = false }, eq]

    let all =
      [
        (* Uu; *)
        Crossbow;
        Knight;
        Monk;
        Camel;
        Skirmisher;
        Pikeman;
        CavalryArcher;
        Mangonel;
        Longsword;
        (* Ram; *)
        LightCavalry;
        Scorpion;
      ]

    type castleunit = t

    (* let damage_to (from : t) (to_ : t) = *)
    (*   match from with *)
    (*   | Crossbow -> _ *)
    (*   | Knight -> _ *)
    (*   | Monk -> _ *)
    (*   | Camel -> _ *)
    (*   | Skirmisher -> _ *)
    (*   | Pikeman -> _ *)
    (*   | CavalryArcher -> _ *)
    (*   | Mangonel -> _ *)
    (*   | Longsword -> _ *)
    (*   | LightCavalry -> _ *)
    (*   | Scorpion -> _ *)

    (* let t_of_yojson x = *)
    (*   try Result.return @@ t_of_yojson x *)
    (*   with _ -> *)
    (*     Error.json *)
    (*     @@ Format.sprintf "Cannot convert %a to AOE2.CU.t" Yojson.Safe.pp x *)

    (* let all = *)
    (*   Eio.traceln "%a" (List.pp String.pp) (List.map fst Variants.descriptions); *)
    (*   Variants.descriptions *)
    (*   |> Result.map_l (fun x -> t_of_yojson @@ `String (fst x)) *)
    (*   |> function *)
    (*   | Ok x -> x *)
    (*   | Error e -> invalid_arg @@ Error.show e *)

    (* let randomstate = *)
    (*   Random.self_init (); *)
    (*   Random.get_state () *)

    let counter (x : t) : (t * float) list =
      match x with
      | LightCavalry -> [ (Knight, 1.0); (Pikeman, 2.0); (Camel, 2.0) ]
      | Pikeman ->
          [
            (Crossbow, 2.0);
            (CavalryArcher, 2.0);
            (Longsword, 1.0);
            (Scorpion, 2.0);
            (Mangonel, 2.0);
          ]
      | Skirmisher ->
          [
            (LightCavalry, 1.0);
            (Knight, 2.0);
            (Longsword, 1.0);
            (Mangonel, 2.0);
            (Camel, 1.0);
            (Scorpion, 2.0);
          ]
      | Crossbow -> [ (Skirmisher, 2.0); (Mangonel, 2.0); (Scorpion, 2.0) ]
      (* | Uu -> [ Uu ] *)
      | Knight -> [ (Monk, 2.0); (Pikeman, 1.0); (Camel, 2.0) ]
      | Monk -> [ (LightCavalry, 2.0); (Pikeman, 1.0); (Skirmisher, 1.0) ]
      | Camel ->
          [ (Pikeman, 1.3); (Crossbow, 1.5); (Monk, 1.5); (Longsword, 1.0) ]
      | CavalryArcher ->
          [ (Skirmisher, 2.0); (Camel, 2.0); (Scorpion, 1.0); (Mangonel, 1.0) ]
      | Mangonel ->
          [ (LightCavalry, 2.0); (Knight, 2.0); (Camel, 1.0); (Monk, 2.0) ]
      | Longsword -> [ (Knight, 1.0); (CavalryArcher, 1.0); (Crossbow, 1.0) ]
      (* | Ram -> [ Longsword; Knight; Pikeman; Camel ] *)
      | Scorpion ->
          [ (Monk, 2.0); (LightCavalry, 2.0); (Knight, 2.0); (Mangonel, 2.0) ]

    let fight x y =
      let s1 =
        List.Assoc.get ~eq:equal x @@ counter y |> Option.get_or ~default:0.0
      in
      let s2 =
        List.Assoc.get ~eq:equal y @@ counter x |> Option.get_or ~default:0.0
      in
      let res = s1 -. s2 in
      res

    (* let rec choose_two () = *)
    (*   let choose = Random.pick_list all in *)
    (*   let x = choose randomstate in *)
    (*   let y = choose randomstate in *)
    (*   match equal x y with *)
    (*   | true -> choose_two () *)
    (*   | false -> Result.return @@ (x, y) *)

    (* let keep : (t list) Ord.t = *)
    (*   fun l0 l1 -> *)
    (*   match l0, l1 with *)
    (*   | [x0; x1], [y0; y1] -> *)
    (*   (match equal x0 y1 && equal y0 x1 with true -> false | false -> 1) *)
    (*   | _ -> false *)
    (*  fun ([x0, y0); (x1, y1)]) -> *)
  end

  module Cost = struct
    type t = { wood : float; food : float; gold : float; damage : float }

    let sum x = x.wood +. x.food +. x.gold
    let damage_per_res (x : t) = x.damage /. sum x

    let cost (x : CastleUnit.t) : t =
      match x with
      | Crossbow -> { wood = 25.0; food = 0.0; gold = 45.0; damage = 5.0 }
      | Knight -> { wood = 0.0; food = 60.0; gold = 75.0; damage = 10.0 }
      | Monk -> { gold = 100.0; wood = 0.0; food = 0.0; damage = 20.0 }
      | Camel -> { wood = 0.0; gold = 60.0; food = 55.0; damage = 6.0 }
      | Skirmisher -> { wood = 25.0; food = 35.0; gold = 0.0; damage = 2.0 }
      | Pikeman -> { wood = 35.0; food = 25.0; gold = 0.0; damage = 4.0 }
      | CavalryArcher -> { wood = 40.0; gold = 60.0; food = 0.0; damage = 6.0 }
      | Mangonel -> { wood = 160.0; gold = 135.0; food = 0.0; damage = 40.0 }
      | Longsword -> { wood = 0.0; gold = 20.0; food = 60.0; damage = 9.0 }
      (* | Ram -> { wood = 160.0; gold = 75.0; food = 0.0; damage = 1.0 } *)
      | LightCavalry -> { wood = 0.0; gold = 0.0; food = 80.0; damage = 7.0 }
      | Scorpion -> { wood = 75.0; gold = 75.0; food = 0.0; damage = 12.0 }
  end

  module Combo = struct
    type 'a t = {
      primary : CastleUnit.t;
      secondary : CastleUnit.t;
      score : float;
      cost : float;
      damage_per_res : float;
      modified_score : float;
      best : 'a t option;
      worst : 'a t option;
      weaknesses : CastleUnit.t list;
    }
    [@@deriving show]

    let cost x =
      Cost.sum (Cost.cost x.primary) +. Cost.sum (Cost.cost x.secondary)

    let damage_per_res (x : 'a t) =
      let primary_cost = Cost.cost x.primary in
      let secondary_cost = Cost.cost x.secondary in
      let damage = primary_cost.damage +. secondary_cost.damage in
      damage /. cost x

    let strengths (x : 'a t) =
      List.filter
        (fun enemy ->
          let counters_enemy = CastleUnit.counter enemy |> List.map fst in
          let enemy_killed =
            List.mem x.primary counters_enemy
            || List.mem x.secondary counters_enemy
          in
          enemy_killed)
        CastleUnit.all

    let weaknesses (x : 'a t) =
      let strengths = strengths x in
      CastleUnit.counter x.primary @ CastleUnit.counter x.secondary
      |> List.map fst
      |> List.uniq ~eq:CastleUnit.equal
      |> List.filter (fun x -> not @@ List.mem x strengths)

    let winner x y =
      let fight = CastleUnit.fight in
      fight x.primary y.primary
      +. fight x.primary y.secondary
      +. fight x.secondary y.primary
      +. fight x.secondary y.secondary

    let reasonable (x : 'a t) =
      let ( let* ) = Option.( let* ) in
      let* () =
        match CastleUnit.equal x.primary x.secondary with
        | true -> None
        | false -> Some ()
      in
      let* () = match x.primary with Monk -> None | _ -> Some () in
      let* () =
        match x.primary with
        | Crossbow | Knight | Camel | CavalryArcher | LightCavalry -> Some ()
        | _ -> None
      in
      Option.return x

    let make x y =
      let res =
        {
          primary = x;
          secondary = y;
          score = 0.0;
          cost = 0.0;
          damage_per_res = 0.0;
          modified_score = 0.0;
          best = None;
          worst = None;
          weaknesses = [];
        }
      in
      let res = { res with weaknesses = weaknesses res } in
      res

    let of_list l =
      match l with [ x; y ] -> Option.return @@ make x y | _ -> None

    let all () =
      List.cartesian_product [ CastleUnit.all; CastleUnit.all ]
      |> List.filter_map of_list |> List.filter_map reasonable

    let score all (x : 'a t) : 'a t =
      let weaknesses = 0 - List.length x.weaknesses |> Int.to_float in
      let start =
        {
          x with
          score = weaknesses;
          cost = cost x;
          damage_per_res = damage_per_res x;
        }
      in
      let scored =
        List.fold_left
          (fun x competitor ->
            let fight = winner x competitor in
            let best =
              match x.best with
              | None ->
                  { competitor with damage_per_res = damage_per_res competitor }
              | Some best ->
                  if winner x best >. fight then best
                  else
                    {
                      competitor with
                      damage_per_res = damage_per_res competitor;
                    }
            in
            let worst =
              match x.worst with
              | None ->
                  { competitor with damage_per_res = damage_per_res competitor }
              | Some worst ->
                  if winner x worst <. fight then worst
                  else if winner x worst =. fight then
                    let worst_dpr = damage_per_res best in
                    let dpr = damage_per_res competitor in
                    if worst_dpr >=. dpr then worst
                    else
                      {
                        competitor with
                        damage_per_res = damage_per_res competitor;
                      }
                  else
                    {
                      competitor with
                      damage_per_res = damage_per_res competitor;
                    }
            in
            {
              x with
              score = x.score +. fight;
              best = Some best;
              worst = Some worst;
            })
          start all
      in
      let res =
        {
          scored with
          modified_score = scored.damage_per_res *. (scored.score *. 10.0);
        }
      in
      res

    let all_scored =
      let all = all () in
      List.map (score all) all
      |> List.sort (fun x y -> Float.compare x.score y.score)

    let top () =
      Eio.traceln "@[%a@]@." (List.pp (pp Float.pp)) all_scored;
      let best_dpr =
        List.sort
          (fun x y -> Float.compare x.damage_per_res y.damage_per_res)
          all_scored
        |> List.last_opt
        |> Option.get_exn_or "no last"
      in
      Eio.traceln "@[BEST DPR@]";
      Eio.traceln "@[@%a]" (pp Float.pp) best_dpr;
      let crossbow_situation primary orig_secondary (enemy : 'a t) =
        let our_comp = make primary orig_secondary in
        let dpr_on_switch secondary =
          let res = score all_scored { our_comp with secondary } in
          res.damage_per_res
        in
        let score_on_switch secondary =
          winner (score all_scored { our_comp with secondary }) enemy
        in
        let best_switch, _ =
          List.fold_left
            (fun (acc, prev_score) new_secondary ->
              let score = score_on_switch new_secondary in
              (* Eio.traceln "%a %f" CastleUnit.pp new_secondary score; *)
              match score >=. prev_score with
              | true when score =. prev_score ->
                  if dpr_on_switch acc >=. dpr_on_switch new_secondary then
                    (acc, prev_score)
                  else (new_secondary, score)
              | true -> (new_secondary, score)
              | false -> (acc, prev_score))
            (orig_secondary, winner our_comp enemy)
            CastleUnit.all
        in
        let msg =
          match best_switch with
          | _ when CastleUnit.equal orig_secondary best_switch -> "OK"
          | _ -> Format.asprintf "SWITCH %a" CastleUnit.pp best_switch
        in
        Eio.traceln "Enemy %a and %a: %s" CastleUnit.pp enemy.primary
          CastleUnit.pp enemy.secondary msg;
        ()
        (* let reactions = List.map  in *)
      in
      let best_reactions primary secondary =
        List.map (crossbow_situation primary secondary) all_scored
      in
      let _ = best_reactions Crossbow Monk in
      ()
  end
end
