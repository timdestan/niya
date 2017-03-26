module List = struct
  include List

  let range a b = 
    let rec aux a acc =
      if a >= b then acc else aux (a + 1) (a :: acc)
    in aux a []

  let flat_map f l = map f l |> flatten

  let cart_prod (la: 'a list) (lb: 'b list): ('a * 'b) list =
    flat_map (fun a -> map (fun b -> (a, b)) lb) la

  (* Tag each list entry with 30 random bits, sort by the tags, then drop the
     tags. Not quite a perfect random shuffle if any of the tags are duplicates,
     but close enough. *)
  let shuffle l =
    let pair_with_tag c = (Random.bits (), c) in
    map pair_with_tag l |> sort compare |> map snd
end

type faction = Red | Black

let other_faction = function
  | Red -> Black
  | Black -> Red

let string_of_faction = function
  | Red -> "🔴"
  | Black -> "⬤"

type plant = MapleLeaves | CherryTree | PineTree | Iris
type symbol = Sun | Tanzaku | Bird | Cloud

let string_of_plant = function
  | MapleLeaves -> "🍁"
  | CherryTree -> "🌸"
  | PineTree -> "🌲"
  | Iris -> "🌷"

let string_of_symbol = function
  | Sun -> "☼"
  | Tanzaku -> "📜"
  | Bird -> "🐦"
  | Cloud -> "☁"

let all_plants = [MapleLeaves ; CherryTree ; PineTree ; Iris]
let all_symbols = [Sun ; Tanzaku ; Bird ; Cloud]

type garden_tile = plant * symbol

let all_garden_tiles = List.cart_prod all_plants all_symbols

module Board = struct
  module Cell = struct
    type t = GardenTile of garden_tile | Faction of faction

    let garden g = GardenTile g
    let faction f = Faction f

    let string_of_cell = function
      | GardenTile (plant, symbol) ->
            string_of_plant plant ^ string_of_symbol symbol
      | Faction faction -> string_of_faction faction 
  end

  type row = int
  type col = int
  type index = row * col

  let row_size = 4
  let col_size = 4

  let row_indices = List.range 0 row_size
  let col_indices = List.range 0 col_size
  let indices = List.cart_prod row_indices col_indices

  let offset_of_index (r, c) = row_size * r + c
  let index_of_offset o = (o / row_size, o mod row_size)

  type t = Cell.t list

  let at (b: t) (i: index) : Cell.t =
    List.nth b (offset_of_index i)

  let set (b: t) (i: index) (new_value: Cell.t): t =
    let o' = offset_of_index i in
    List.mapi (fun o v -> if o = o' then new_value else v) b

  let open_cells (b: t): (garden_tile * index) list =
    List.mapi (fun o v -> match v with
      | Cell.Faction _ -> []
      | Cell.GardenTile g -> [(g, index_of_offset o)]
    ) b |> List.flatten

  let create_random (): t =
    List.shuffle all_garden_tiles |> List.map Cell.garden

  let as_string (b: t):string =
    let surround s = "[ " ^ s ^ " ]" in
    List.map (fun r ->
      List.map (fun c -> Cell.string_of_cell (at b (r,c))) col_indices
        |> String.concat "   " |> surround
    ) row_indices |> String.concat "\n"
end
module Cell = Board.Cell

type game_state = {
  whose_turn: faction;
  board: Board.t;
  last_move: garden_tile option;
}

let choose_starting_faction () = match Random.bool () with
  | true -> Red
  | false -> Black

let new_game () = {
  whose_turn = choose_starting_faction ();
  board = Board.create_random ();
  last_move = None;
}

let legal_moves (state:game_state): Board.index list =
  let open_cells = Board.open_cells state.board in
  let legal_indexes (garden_tile, i) =
    let (p, s) = garden_tile in
    match state.last_move with
      | None -> [i]
      | Some (p', s') when p = p' || s = s' -> [i]
      | _ -> []
  in List.map legal_indexes open_cells |> List.flatten 

let move (state:game_state) (i:Board.index): game_state option =
  let updated (tile: garden_tile): game_state = {
    last_move = Some tile;
    board = Board.set state.board i (Cell.Faction state.whose_turn);
    whose_turn = other_faction state.whose_turn;
  } in
  match (Board.at state.board i) with
    | Cell.Faction _ -> None (* Already occupied *)
    | Cell.GardenTile (p, s) -> (match state.last_move with
      | None -> Some(updated (p, s))
      | Some (p', s') when p = p' || s = s' -> Some(updated (p, s))
      | _ -> None (* Doesn't match constraints *)
    )

let choose_random_move indexes =
  let choice = Random.int (List.length indexes) in
  List.nth indexes choice

let play_random_game (): game_state list =
  let rec aux s prior_states =
    match (legal_moves s) with
    | [] -> List.rev prior_states
    | nonempty -> (
      let m = choose_random_move nonempty in
      match move s m with
      | None -> failwith "Unexpected illegal move."
      | Some s' -> aux s' (s' :: prior_states)
    )
  in let s = new_game ()
  in aux s [s]
;;

Random.self_init ();;

play_random_game ()
|> List.map (fun g -> Board.as_string g.board)
|> String.concat "\n\n"
|> print_endline
