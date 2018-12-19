open Card

type player = {username : string; score: int; 
               cards: card list; collected : card list; acc_score: int}
type t = {discard : card list; round : card list; players : player list; 
          turn: player; first_player: string}

exception PlayerNotFound
exception CardNotFound
exception InvalidCard

(** Create a player with given [name] and [cards] as starting hand. *)
let player name cards acc = 
  {username = name; score = 0; cards = cards; collected = []; acc_score = acc}

(** [start p1 p2 p3] creates an initial state with the given players
    [p1 p2 p3]. The first turn starts with [p1]. *)
let init p1 p2 p3 = 
  {discard = []; round = []; players = (p1::p2::[p3]); turn = p1; 
   first_player = p1.username}

let make_state discard round players turn first_player = 
  {discard = discard; round = round; players = players; turn = turn; 
   first_player = first_player}

let turn st = st.turn.username 

let starting_suit state = 
  try(Some (List.rev state.round |> List.hd).suit)
  with Failure _ -> None

let first_player state = state.first_player 

let username player = player.username 

let get_players state = state.players 

let distribute p1 p2 p3 = 
  let shuffled_deck = shuffle () in 
  let rec distribute' (p1, p2, p3) n lst =
    match lst with 
    | [] -> (p1, p2, p3)
    | h::t -> 
      if n <= 17 then let p1' = {p1 with cards = (h::(p1.cards))} in 
        distribute' (p1', p2, p3) (n+1) (t) 
      else if n <= 34 then let p2' = {p2 with cards = (h::(p2.cards))} in 
        distribute' (p1, p2', p3) (n+1) t 
      else let p3' = {p3 with cards = (h::(p3.cards))} 
        in distribute' (p1, p2, p3') (n+1) t
  in distribute' (p1, p2, p3) 1 shuffled_deck 


let get_player player_name st : player = 
  let rec get_player' player_name = function 
    | h :: t -> if h.username = player_name then h else get_player' player_name t
    | [] -> raise PlayerNotFound
  in get_player' player_name st.players 

let get_points st username = (get_player username st).score
let get_total_points st username = (get_player username st).score + 
                                   (get_player username st).acc_score
let get_collected st username = (get_player username st).collected
let get_hand st username = (get_player username st).cards
let round_getter st = st.round

(** [get_round n lst] takes in a lst and gets the nth element *)
let rec get_round n lst =
  match lst with
  | [] -> None
  | h :: t -> if n = 0 then Some h else get_round (n-1) t

let get_played_card st user_pos =
  match st.first_player with 
  | "Computer 1" -> get_round ((0 + user_pos) mod 3) (List.rev st.round)
  | "Computer 2" -> get_round ((2 + user_pos) mod 3) (List.rev st.round)
  | "You" -> get_round ((1 + user_pos) mod 3) (List.rev st.round)
  | _ -> raise PlayerNotFound

let last_player st = List.length st.round = 2 

let calc_round_score st = 
  let rec calc_round_score' = function 
    | [] -> 0
    | h::t -> score_card h + calc_round_score' t in 
  calc_round_score' st.round 

let choose_highest_rank suit player = 
  let desc_cards = sort_hand player.cards |> List.rev in 
  try (
    let card = List.find (fun x -> 
        (* Do not choose a card of that suit that is worth points, 
           or the 10 of Clubs if the suit is Clubs *)
        if (x.suit = suit && (score_card x = 0) && not (suit = Clubs && 
                                                        x.number = 10)) then true 
        else false) desc_cards 
    in Some card)
  with Not_found -> None 

let highest_suit_played st spec_suit =
  let rec highest_suit_played' st highest lst suit_helper =
    match lst with
    | [] -> highest
    | h :: t -> if (h.number = 1) then 14 else (
        if (h.number > highest && h.suit = suit_helper) then 
          (highest_suit_played' st h.number t suit_helper)
        else (highest_suit_played' st highest t suit_helper)) in
  highest_suit_played' st 0 st.round spec_suit

let jack_diamonds_played state = 
  let rec loop_players = function
    | h :: t -> if List.mem {number = 11; suit = Diamonds} h.collected then true
      else false || loop_players t
    | [] -> false
  in loop_players state.players

let remove_card_from_player card player = 
  let rec remove card l acc= 
    match l with
    | [] -> raise CardNotFound
    | h :: t -> if h.number = card.number && h.suit = card.suit 
      then acc@t else remove card t (h::acc) in 
  let new_cards = remove card player.cards [] in
  {player with cards = new_cards}

let rec update_player player lst = 
  match lst with
  | [] -> raise PlayerNotFound
  | h::t -> 
    if h.username = player.username
    then (player::t) 
    else h::(update_player player t)

let next_player st username = 
  let next_player' username lst = 
    match lst with 
    | a::b::c::[] -> if a.username = username then b else if 
        b.username = username then c else a 
    | _ -> raise PlayerNotFound
  in next_player' username st.players 

let get_card command_lst = 
  match command_lst with 
  | face :: suit :: [] -> {suit = get_suit suit; number = (get_face face)}
  | _ -> raise CardNotFound

(** [highest_card_index state] returns the index of the card in this round 
    who has the highest rank of the starting suit. First card played will 
    be index 1, second card index 2, third card index 3. *)
let highest_card_index state : int = 
  let round_rev = List.rev state.round in 
  let round_suit = (List.hd (round_rev)).suit in 
  let rec highest_card_i n highest highest_n = function
    | [] -> highest_n
    | h::t -> (* This card has the same suit as starting suit and 
                 is higher than the highest card so far *)
      if h.suit = round_suit && compare_card h highest > 0 then 
        highest_card_i (n+1) (h) n t
      else highest_card_i (n+1) (highest) highest_n t 
  in highest_card_i 1 (List.hd round_rev) 1 round_rev

let round_winner state = 
  let index = highest_card_index state in 
  let curr_player = ref (get_player state.first_player state) in 
  let rec round_winner' loop index : player = 
    if loop = index then 
      !curr_player else 
      let _ = curr_player := next_player state (!curr_player).username in 
      round_winner' (loop+1) index 
  in 
  round_winner' 1 index 

let end_round state = 
  List.length state.round = 3


let valid_card suit state =
  let user = get_player "You" state in
  match (starting_suit state) with
  | Some s -> 
    if card_of_suit s user.cards <> None then 
      (if suit = s then true else false)
    else true
  | None -> true

(** [computer_1] and [computer_2] initialize the two AI players*)
let computer_1 = player "Computer 1" [] 0 
let computer_2 = player "Computer 2" [] 0 

let init_state ()= 
  let user = player "You" [] 0 in
  let player_tup = distribute computer_1 computer_2 user in
  match player_tup with
  | (a,b,c) -> init a b c

let updated_acc_points st =
  let user = (get_player "You" st) in
  let comp1 = (get_player "Computer 1" st) in
  let comp2 = (get_player "Computer 2" st) in
  let new_state = init_state () in
  let user' = (get_player "You" new_state) in
  let comp1' = (get_player "Computer 1" new_state) in
  let comp2' = (get_player "Computer 2" new_state) in
  let updated_players = [{user' with acc_score = user.acc_score + user.score};
                         {comp1' with acc_score = comp1.acc_score + comp1.score};
                         {comp2' with acc_score = comp2.acc_score + comp2.score}] in 
  {new_state with players = updated_players}

let end_game state = 
  let rec end_game' = function
    | [] -> true
    | player::t -> if List.length player.cards <> 0 then false else end_game' t
  in end_game' state.players 

let final_end st =
  if (abs (get_total_points st "You") > 999) ||
     (abs (get_total_points st "Computer 1") > 999) || 
     (abs (get_total_points st "Computer 2") > 999)
  then true else false

(** [next_player_username u] is the username of the player next in clockwise 
    order to player with username [u]*)
let next_player_username username = 
  match username with
  | "You" -> "Computer 1"
  | "Computer 1" -> "Computer 2"
  | "Computer 2" -> "You"
  | _ -> "NOT A PLAYER"

(**[update_to_next_player u s] returns the next player to the one having username
   [u]*)
let update_to_next_player username st = 
  {st with turn = get_player (next_player_username username) st}

let get_discard state = state.discard 

(** [count_discard state] is the card list of all the cards of this round
    that will need to be added into the discard pile. *)
let count_discard state = 
  let rec add_card_to_disc lst acc = 
    match lst with
    | h :: t -> if (score_card h) = 0 && h.suit <> Hearts 
                   && h <> {number = 10; suit = Clubs}
      then add_card_to_disc t (h::acc) 
      else add_card_to_disc t acc
    | [] -> acc in
  add_card_to_disc state.round state.discard

let get_collected_cards state = 
  let discard = count_discard state in
  List.filter (fun x -> not (List.mem x discard)) state.round

(** [check_all_hearts lst] is true if lst contains all the hearts, false otherwise*)
let check_all_hearts lst = 
  let all_hearts = List.filter (fun x-> x.suit = Hearts) deck in 
  List.fold_left (fun acc x -> if List.mem x lst then true && acc else false) 
    true all_hearts

(** [check_ten_clubs lst] is true if lst contains 10 of clubs, false otherwise*)
let check_ten_clubs lst = 
  List.fold_left (fun acc x -> if (x.suit = Clubs && x.number = 10) then 
                     (acc || true) else acc || false) false lst

(** [get_collected_score l] is the score of cards in a list [l]*)
let get_collected_score l = 
  let rec score lst acc = 
    match lst with
    | h :: t -> score t (acc + score_card h)
    | [] -> acc in
  score l 0 
(* if check_all_hearts collected_hand then 
   (if doubled_score>0 then doubled_score else -doubled_score)
   else doubled_score *)


(**[winning_player st] is the [player] who is the winner at the end of the
   round. *)
let winning_player state = 
  let fp = state.first_player in
  let sp = next_player_username fp in 
  let tp = next_player_username sp in
  let winner = 
    match state.round with 
    | [c3;c2;c1] -> 
      (*Aces always win*)
      if c1.number = 1 then fp else
      if c2.suit = c1.suit && c2.number = 1 then sp else 
      if c3.suit = c1.suit && c3.number = 1 then tp else
      if c2.number < c1.number && c3.number < c1.number then fp else
      if c2.number > c1.number && c2.suit = c1.suit 
         && c3.number < c2.number then sp else
      if c2.number > c1.number && c2.suit = c1.suit 
         && c3.suit <> c1.suit then sp else
      if c2.number > c1.number && c2.suit = c1.suit && 
         c3.number > c2.number && c3.suit = c1.suit then tp else
      if c2.number < c1.number && c3.number > c1.number 
         && c3.suit = c1.suit then tp
      else fp
    | _ -> fp
  in get_player winner state


(** [new_round st] gives a state of a new round. The next turn is 
    the winning player.
    Precondition: All three players have played and therefore there are 3 cards 
    in the current round. *)
let new_round st =
  let round_collected = get_collected_cards st in
  let old_winner = (round_winner st) in
  let collected_final = (round_collected @ old_winner.collected) in
  let doubled_score = if (check_ten_clubs collected_final) then 
      2 * (get_collected_score collected_final)
    else 
      (get_collected_score collected_final) in
  let score_final = if check_all_hearts collected_final then 
      (if doubled_score > 0 then doubled_score else -doubled_score)
    else doubled_score in
  let winner = {old_winner with 
                collected = collected_final;
                score = score_final} in
  {round = []; turn = winner; discard = count_discard st;
   players = update_player winner st.players; first_player = winner.username} 
