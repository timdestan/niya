module L = List

module Either = struct
  type ('l, 'r) t = Left of 'l 
                  | Right of 'r

  let right r = Right r
  let left l = Left l
end
module E = Either

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
    let tagged = map (fun c -> (Random.bits (), c)) l in
    let sorted = sort compare tagged in
    map snd sorted
end
module LE = ListExt

type faction = Red | Black

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
  type row = int
  type col = int
  type index = row * col

  let row_size = 4
  let col_size = 4

  let row_indices = LE.range 0 row_size
  let col_indices = LE.range 0 col_size
  let indices = LE.cart_prod row_indices col_indices

  let offset_of_index (r, c) = row_size * r + c

  type cell = (garden_tile, faction) Either.t
  type t = cell list

  let create_random (): t =
    LE.shuffle all_garden_tiles |> L.map E.left

  let as_string (b: t):string =
    let surround s = "[ " ^ s ^ " ]" in
    L.map (fun r ->
      L.map (fun c ->
        match L.nth b (offset_of_index (r,c)) with
         | Either.Left (plant, symbol) ->
            string_of_plant plant ^ string_of_symbol symbol
         | Either.Right faction -> string_of_faction faction 
      ) col_indices |> String.concat "   " |> surround
    ) row_indices |> String.concat "\n"
end

type game_state = {
  whose_turn: faction;
  board: Board.t;
  last_move: garden_tile option;
}

let choose_starting_player () = match Random.bool () with
  | true -> Red
  | false -> Black

let new_game () = {
  whose_turn = choose_starting_player ();
  board = Board.create_random ();
  last_move = None;
}

;;

Random.self_init () ;;

print_endline @@ Board.as_string @@ Board.create_random ()
