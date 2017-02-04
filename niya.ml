module L = List

module ListExt = struct
  open List

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
module LE = ListExt

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

let all_garden_tiles = LE.cart_prod all_plants all_symbols

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

  let row_indices = LE.range 0 row_size
  let col_indices = LE.range 0 col_size
  let indices = LE.cart_prod row_indices col_indices

  let offset_of_index (r, c) = row_size * r + c

  type t = Cell.t list

  let at (b: t) (i: index) : Cell.t =
    List.nth b (offset_of_index i)

  let set (b: t) (i: index) (new_value: Cell.t): t =
    let o' = offset_of_index i in
    L.mapi (fun o v -> if o = o' then new_value else v) b

  let create_random (): t =
    LE.shuffle all_garden_tiles |> L.map Cell.garden

  let as_string (b: t):string =
    let surround s = "[ " ^ s ^ " ]" in
    L.map (fun r ->
      L.map (fun c -> Cell.string_of_cell (at b (r,c))) col_indices
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

;;

Random.self_init ();;

let g = new_game ();;

print_endline @@ Board.as_string g.board;;

print_newline ();;

let b' = move g (2, 3);;

print_endline (
  match b' with
    | None -> "Nope"
    | Some {board = b;} -> Board.as_string b
)
