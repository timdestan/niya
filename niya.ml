module List = struct
  include List

  let range a b =
    let rec aux a acc =
      if a >= b then rev acc else aux (a + 1) (a :: acc)
    in aux a []

  let flat_map f l = map f l |> flatten

  let cart_prod (la: 'a list) (lb: 'b list): ('a * 'b) list =
    flat_map (fun a -> map (fun b -> (a, b)) lb) la

  (** Tag each list entry with 30 random bits, sort by the tags, then drop the
      tags. Not quite a perfect random shuffle if any of the tags are duplicates,
      but close enough. *)
  let shuffle l =
    let pair_with_tag c = (Random.bits (), c) in
    map pair_with_tag l |> sort compare |> map snd

  (** Takes a list of options and returns the inner values, dropping the Nones. *)
  let somes (l: 'a option list): 'a list =
    rev (fold_left (fun acc x -> match x with
        | None -> acc
        | Some x -> x :: acc) [] l)
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

    let faction_of_cell = function
      | GardenTile _ -> None
      | Faction f -> Some f

    (** Finds a common faction among the cells in the list, if such a faction
      * exists. *)
    let common_faction (cells: t list): faction option =
      let module Local = struct
        type 'a result = Found of 'a | NotFound | DunnoYet

        let to_option = function
          | Found a -> Some a
          | NotFound | DunnoYet -> None

        let find_common (f:'a -> 'b option) (alist: 'a list): 'b option =
          List.fold_left (fun acc a -> match f a with
            | None -> NotFound
            | Some b -> (match acc with
              | DunnoYet -> Found b
              | Found b' when b = b' -> acc
              | _ -> NotFound)) DunnoYet alist |> to_option
      end in
      Local.find_common faction_of_cell cells
  end

  type row = Cell.t list
  type t = row list
  type index_pair = (int * int)

  let rows (board: t) : row list = board

  let columns (board: t): row list =
    let rec combine (c: row) (cs: row list) : row list = match (c, cs) with
      | ((x :: xs), (y :: ys)) -> (x :: y) :: combine xs ys
      | _ -> [] in
    let rec aux = function
    | [] -> []
    | [x] -> List.map (fun x -> [x]) x
    | r :: rs -> combine r (aux rs) in
    aux board

  let main_diagonal (board: t) : row =
    List.fold_left (fun (acc, n) r -> (List.nth r n :: acc, n+1)) ([],0) board
    |> fst
    |> List.rev

  let off_diagonal (board: t) : row = main_diagonal (List.map List.rev board)

  let at (b: t) (pair: index_pair) : Cell.t =
    let (row, column) = pair in
    List.nth (List.nth b row) column

  let set (b: t) (pair: index_pair) (new_value: Cell.t): t =
    let (row, column) = pair in
    List.mapi (fun r' old_row ->
        if row = r'
        then List.mapi (fun c' old_value ->
            if column = c'
            then new_value
            else old_value
          ) old_row
        else old_row) b

  (** Produce the cells as a flattened list, bundled
      with the (row, column) index pair. *)
  let cells (board: t): (Cell.t * index_pair) list =
    List.mapi (fun r row ->
        List.mapi (fun c value ->
            (value, (r,c))
          ) row) board
    |> List.flatten

  let open_cells (board: t): (garden_tile * index_pair) list =
    cells board
    |> List.map (function
        | (Cell.Faction _, _) -> None
        | (Cell.GardenTile g, i) -> Some (g, i))
    |> List.somes

  let create_random (): t =
    let deal tiles =
      let rec aux acc tiles = match tiles with
        | a :: b :: c :: d :: rest -> aux ([a;b;c;d] :: acc) rest
        | [] -> acc
        | _ -> failwith "Expected a list that is divisible by 4."
      in aux [] tiles
    in all_garden_tiles
    |> List.map Cell.garden
    |> List.shuffle
    |> deal

  let string_of_row (r: row) =
    let surround s = "[ " ^ s ^ " ]" in
    List.map Cell.string_of_cell r |> String.concat "  " |> surround

  let as_string (b: t):string = List.map string_of_row b |> String.concat "\n"
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

let legal_moves (state:game_state): Board.index_pair list =
  let open_cells = Board.open_cells state.board in
  let legal_indexes (garden_tile, i) =
    let (p, s) = garden_tile in
    match state.last_move with
      | None -> [i]
      | Some (p', s') when p = p' || s = s' -> [i]
      | _ -> []
  in List.map legal_indexes open_cells |> List.flatten

let possible_winning_formations (b : Board.t): Board.row list =
  (
    (** TODO: You can also win by getting a box of 4. *)
    Board.main_diagonal b ::
    Board.off_diagonal b ::
    (Board.rows b) @
    (Board.columns b)
  )

(** Determine whether there is a winner for the given game state. *)
let winner (state: game_state): faction option =
  let possible_wins = possible_winning_formations state.board in
  let common_factions =
    List.map Board.Cell.common_faction possible_wins |> List.somes in
  match common_factions with
    (** In theory, there could be multiple factions, but a real game would end
      * on the move that creates the first 4-in-a-row, so in practice we don't
      * need to check that they are all the same. *)
   | f :: _ -> Some f
   | [] ->
    (match legal_moves state with
      | [] -> Some (other_faction state.whose_turn)
      | _ -> None)

let move (state:game_state) (pair: Board.index_pair): game_state option =
  let updated (tile: garden_tile): game_state = {
    last_move = Some tile;
    board = Board.set state.board pair (Cell.Faction state.whose_turn);
    whose_turn = other_faction state.whose_turn;
  } in
  match (Board.at state.board pair) with
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
    match winner s with
    | Some _ -> List.rev prior_states
    | None ->
      (** If there were no legal moves, there should have been a winner.
        * So assume legal moves are non-empty. *)
      let m = choose_random_move (legal_moves s) in
      match move s m with
      | None -> failwith "Unexpected illegal move."
      | Some s' -> aux s' (s' :: prior_states)
  in let s = new_game ()
  in aux s [s]
;;

Random.self_init ();;

play_random_game ()
|> List.map (fun g -> Board.as_string g.board)
|> String.concat "\n\n"
|> print_endline
