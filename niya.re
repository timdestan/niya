module L = List;

module ListExt = {
  open List;
  let range a b => {
    let rec aux a acc =>
      if (a >= b) {
        acc
      } else {
        aux (a + 1) [a, ...acc]
      };
    aux a []
  };
  let flat_map f l => map f l |> flatten;
  let cart_prod (la: list 'a) (lb: list 'b) :list ('a, 'b) =>
    flat_map (fun a => map (fun b => (a, b)) lb) la;
  /* Tag each list entry with 30 random bits, sort by the tags, then drop the
     tags. Not quite a perfect random shuffle if any of the tags are duplicates,
     but close enough. */
  let shuffle l => {
    let pair_with_tag c => (Random.bits (), c);
    map pair_with_tag l |> sort compare |> map snd
  };
};

module LE = ListExt;

type faction =
  | Red
  | Black;

let other_faction =
  fun
  | Red => Black
  | Black => Red;

let string_of_faction =
  fun
  | Red => "🔴"
  | Black => "⬤";

type plant =
  | MapleLeaves
  | CherryTree
  | PineTree
  | Iris;

type symbol =
  | Sun
  | Tanzaku
  | Bird
  | Cloud;

let string_of_plant =
  fun
  | MapleLeaves => "🍁"
  | CherryTree => "🌸"
  | PineTree => "🌲"
  | Iris => "🌷";

let string_of_symbol =
  fun
  | Sun => "☼"
  | Tanzaku => "📜"
  | Bird => "🐦"
  | Cloud => "☁";

let all_plants = [MapleLeaves, CherryTree, PineTree, Iris];

let all_symbols = [Sun, Tanzaku, Bird, Cloud];

type garden_tile = (plant, symbol);

let all_garden_tiles = LE.cart_prod all_plants all_symbols;

module Board = {
  module Cell = {
    type t =
      | GardenTile garden_tile
      | Faction faction;
    let garden g => GardenTile g;
    let faction f => Faction f;
    let string_of_cell =
      fun
      | GardenTile plant symbol [@implicit_arity] =>
        string_of_plant plant ^ string_of_symbol symbol
      | Faction faction => string_of_faction faction;
  };
  type row = int;
  type col = int;
  type index = (row, col);
  let row_size = 4;
  let col_size = 4;
  let row_indices = LE.range 0 row_size;
  let col_indices = LE.range 0 col_size;
  let indices = LE.cart_prod row_indices col_indices;
  let offset_of_index (r, c) => row_size * r + c;
  let index_of_offset o => (o / row_size, o mod row_size);
  type t = list Cell.t;
  let at (b: t) (i: index) :Cell.t => List.nth b (offset_of_index i);
  let set (b: t) (i: index) (new_value: Cell.t) :t => {
    let o' = offset_of_index i;
    L.mapi
      (
        fun o v =>
          if (o == o') {
            new_value
          } else {
            v
          }
      )
      b
  };
  let open_cells (b: t) :list (garden_tile, index) =>
    L.mapi
      (
        fun o v =>
          switch v {
          | Cell.Faction _ => []
          | Cell.GardenTile g => [(g, index_of_offset o)]
          }
      )
      b |> L.flatten;
  let create_random () :t => LE.shuffle all_garden_tiles |> L.map Cell.garden;
  let as_string (b: t) :string => {
    let surround s => "[ " ^ s ^ " ]";
    L.map
      (
        fun r =>
          L.map (fun c => Cell.string_of_cell (at b (r, c))) col_indices |> String.concat "   " |> surround
      )
      row_indices |>
    String.concat "\n"
  };
};

module Cell = Board.Cell;

type game_state = {whose_turn: faction, board: Board.t, last_move: option garden_tile};

let choose_starting_faction () => Random.bool () ? Red : Black;

let new_game () => {
  whose_turn: choose_starting_faction (),
  board: Board.create_random (),
  last_move: None
};

let legal_moves (state: game_state) :list Board.index => {
  let open_cells = Board.open_cells state.board;
  let legal_indexes (garden_tile, i) => {
    let (p, s) = garden_tile;
    switch state.last_move {
    | None => [i]
    | Some (p', s') when p == p' || s == s' => [i]
    | _ => []
    }
  };
  List.map legal_indexes open_cells |> List.flatten
};

let move (state: game_state) (i: Board.index) :option game_state => {
  let updated (tile: garden_tile) :game_state => {
    last_move: Some tile,
    board: Board.set state.board i (Cell.Faction state.whose_turn),
    whose_turn: other_faction state.whose_turn
  };
  switch (Board.at state.board i) {
  | Cell.Faction _ => None /* Already occupied */
  | Cell.GardenTile p s [@implicit_arity] =>
    switch state.last_move {
    | None => Some (updated (p, s))
    | Some (p', s') when p == p' || s == s' => Some (updated (p, s))
    | _ => None /* Doesn't match constraints */
    }
  }
};

let choose_random_move indexes => {
  let choice = Random.int (List.length indexes);
  List.nth indexes choice
};

let play_random_game () :list game_state => {
  let rec aux s prior_states =>
    switch (legal_moves s) {
    | [] => L.rev prior_states
    | nonempty =>
      let m = choose_random_move nonempty;
      switch (move s m) {
      | None => failwith "Unexpected illegal move."
      | Some s' => aux s' [s', ...prior_states]
      }
    };
  let s = new_game ();
  aux s [s]
};

Random.self_init ();

play_random_game () |> L.map (fun g => Board.as_string g.board) |> String.concat "\n\n" |> print_endline;
