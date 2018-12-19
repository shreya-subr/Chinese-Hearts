open State 
open Card 

(** [highest_suit player suit] chooses the highest risk card from
    [player's] hand to play. *)
let highest_risk player state = 
  let cards' = sort_by_risk (get_hand state (username player)) in 
  List.hd cards' 

(** [match_none_helper player st] is a helper for suit_or_other that selects
    a card to play if the player has run out of cards of the currently played
    suit *)
let match_none_helper player st =
  (* Ran out of this specified suit and player is not going first, 
         so play highest risk card in hand as throw away to another player. *)
  if (first_player st <> (username player)) then 
    highest_risk player st  
    (** [player] is first player so cannot throw away cards, choose 
        a random low card from player's hand. *)
  else List.hd (sort_hand (username player |> get_hand st))

(** [card_of_suit player suit] returns a random card of specified [suit] 
    from [player]'s hand. 
    If the suit was Hearts, it will choose the lowest card of that 
    suit. If a card of Hearts has already been played, then the highest Heart
    card that is still lower than the already played one is played.
    If the suit is Spades, it will choose a card less than the Queen of Spades,
    which is the pig, or if a Spade card was already played with rank higher
    than Queen, it will chose a card less than that already played card.
    If this player has no card of the specified [suit]:
    1. If the player is not the first player, then choose highest risk card to 
    throw away to another player.
    2. Else, player is the first player, so choose a randomly selected card. 
    Precondition: [player] should have at least 1 card left. *)
let suit_or_other player suit st = 
  let cards' = 
    if (suit = Spades || suit = Hearts) then sort_hand 
        (username player |> get_hand st) else (username player |> get_hand st) in
  let high_hearts = highest_suit_played st Hearts in
  let high_spades = highest_suit_played st Spades in
  if (suit = Hearts && high_hearts != 0) then 
    (match card_of_heart cards' None high_hearts with
     | Some c -> c
     | None -> match_none_helper player st)
  else if (suit = Spades && high_spades > 12) then 
    (match card_of_spade cards' None high_spades with
     | Some c -> c
     | None -> match_none_helper player st)
  else if (suit = Spades && high_spades != 0) then 
    (match card_of_spade cards' None 12 with
     | Some c -> c
     | None -> match_none_helper player st)
  else (
    match card_of_suit suit cards' with 
    | Some c -> c
    | None -> match_none_helper player st)


(** [tempting_queen_good st player] determines whether or not [player] 
    should tempt out the Queen of Spades. Returns true if 
    1. player has either K OR A (not both!) of Spades and more than 3 zero-risk 
    Spades. 
    2. player's Spades all have 0 risk. *)
let tempting_queen_good player st = 
  match suit_risk (username player |> get_hand st) Spades with 
  |(score, num) -> 
    if (score >= (-80) && num > 3) then true
    else if (score = 0) then true 
    else false 


(** [choose_lowest_rank suit player] chooses the lowest ranked card 
    of [suit] in players' hand. Returns Some card if the [player] still has 
    card(s) of [suit]. Returns None if [player] no longer has any card of 
    specified [suit]. *)
let choose_lowest_rank suit player st = 
  card_of_suit suit (sort_hand (get_hand st (username player)))

(* [last_player_logic st player suit] if suit is not diamonds AND [player] is 
   last player AND the round has a score of 0, then return Some highest card of 
   that suit in hand. If suit is diamonds and [player] is not the last player,
   play highest diamond. If suit is diamond and [player] is last player,
   play lowest diamond. If no cards left of [suit], return None *)
let last_player_logic st player suit = 
  let curr_player = (get_player player st) in
  (* The conditions to apply last player logic. 
     Do not want to win the round and play a high card if 10 of clubs has 
     been played this round. *)
  if (last_player st && suit <> Diamonds && calc_round_score st = 0 && 
      not (List.mem {number = 10; suit = Clubs} (round_getter st))) then 
    match choose_highest_rank suit curr_player with
    | Some c -> Some c
    | None -> None
    (*If jack of diamonds has been played in this round choose highest card*)
  else if(last_player st && suit = Diamonds && 
          (List.mem {number = 11; suit = Diamonds} (round_getter st))) then
    match choose_highest_rank suit curr_player with
    | Some c -> Some c
    | None -> None
    (*If jack of diamonds has been played in previous round choose lowest card*)
  else if(last_player st && suit = Diamonds && jack_diamonds_played st) then 
    match choose_lowest_rank suit curr_player st with
    | Some c -> Some c
    | None -> None
  else if(not (last_player st) && suit = Diamonds) then
    match choose_highest_rank suit curr_player with
    | Some c -> Some c
    | None -> None
  else None 

(** [computer_choose_card player state] chooses a card from the computer's hand
    based on the round's current suit and whether the player is last player. 
    If the computer is playing first, then default to playing Clubs. *)
let computer_choose_card player (state:t) = 
  (* Find the suit of the first card played *)
  match starting_suit state with
  | None ->   
    (* Player goes first. Check if computer should use tempting queen strategy. 
       If it is not strategic, then default to Clubs.  *)
    let chosen_suit = 
      if (tempting_queen_good player state) then Spades 
      else Clubs in 
    suit_or_other (get_player (turn state) state) chosen_suit state
  | Some suit -> 
    (* There is a starting suit. Use last player logic if applicable. *)
    match last_player_logic state (username player) suit with 
    | Some c -> c
    (* Not last player or does not have starting suit, pick a
       card based on suit of starting round *)
    | None -> suit_or_other (get_player (turn state) state) suit state

let update_computer (state:t) player : t = 
  let comp = (get_player player state) in
  let chosen_card = computer_choose_card comp state in
  let new_hand = remove_card_from_player chosen_card comp in
  let new_turn = next_player state (username comp) in
  let new_round = chosen_card :: (round_getter state) in
  let new_player_lst = update_player new_hand (get_players state) in
  make_state (get_discard state) new_round (new_player_lst) 
    (new_turn) (first_player state)

let update_user (lst:string list) (state:t) = 
  let card = (get_card lst) in
  let you = (get_player "You" state) in
  if (valid_card card.suit state) then
    ( let new_round = card :: round_getter state in
      let new_hand = remove_card_from_player card you in
      let new_player_lst = update_player new_hand (get_players state) in
      let _ = print_string("You played "); in
      let _ = Card.print_card card in
      let new_turn = get_player "Computer 1" state in
      make_state (get_discard state) new_round (new_player_lst) 
        (new_turn) (first_player state)
    ) else if (List.mem card (get_hand state (username you))) 
  then raise InvalidCard 
  else raise CardNotFound

