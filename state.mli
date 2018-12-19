open Card

(** The type to represent a player in the game *)
type player

(** The type to represent a game state *)
type t 

(** [CardNotFound] is thrown when a user tries to play a card not in their
    hand. *)
exception CardNotFound

(** [PlayerNotFound] is thrown if the game doesn't have a player with 
    a given username. *)
exception PlayerNotFound

(** [InvalidCard] is thrown if the card being played is not allowed by the 
    rules *)
exception InvalidCard

(** [distribute players] distributes cards to each player in list
    [players] and returns a tuple of players. *)
val distribute: player -> player -> player -> player * player * player

(** [turn st] is the username of the player whose turn it currently is. I.e.
    this player needs to make the next turn. *)
val turn: t -> string

(** [first_player state] returns the username of the first player 
    of this round. *)
val first_player: t -> string

(** [get_players state] gets the list of [players] of this game [state]. *)
val get_players: t -> player list

(** [make_state discard round players turn first_player] makes a game state
    with these given values. *)
val make_state: Card.card list -> Card.card list -> 
  player list -> player -> string -> t

(** [new_round st] gives a state of a new round. The next turn is always the
    first player (Computer 1). 
    Precondition: All three players have played and therefore there are 3 cards 
    in the current round. *)
val new_round: t -> t

(** [get points st username] gets the points of player with this [username]. *)
val get_points: t -> string -> int

(** [get total_points st username] gets the total points of all games 
    of player with this [username]. *)
val get_total_points: t -> string -> int

(** [get_collected st username] gets the collected card list from player with
    this [username]. *)
val get_collected: t -> string -> Card.card list

(** [get_hand st player] gets the hand of player with this [username]
    Raises: PlayerNotFound*)
val get_hand: t -> string -> Card.card list

(** [get_played_card st user_pos] gets the played card in state depending
    on which user you want as specified by [user_position] *)
val get_played_card: t -> int -> Card.card option

(** [get_player name st] is the player with [username] in [st]
    Raises: PlayerNotFound if there does not exist a player with
    [username] in [st]. *)
val get_player: string -> t -> player

(** [round_getter st] returns the list of played cards in st. *)
val round_getter: t -> Card.card list

(** [remove_card_from_player removes [card] from [player]'s hand and 
    returns player.
    Raises: [CardNotFound] if the card is not in player's hand.*)
val remove_card_from_player: Card.card -> player -> player

(** [round_winner state] returns the winning [player] of this current round 
    given a state. The winner is the [player] who played the highest rank
    of the starting suit. *)
val round_winner: t -> player

(** [last_player st] is true if 2 players have already played and only 1 player
    needs to play. False otherwise. *)
val last_player: t -> bool

(** [username player] is the username of this [player]. *)
val username: player -> string

(** [next_player t username] is the next [player] in a clockwise sequence of 
    players given a state and starting player with [username]. *)
val next_player: t -> string -> player

(** [init_state ()] returns an initial state with 3 players - You, Computer 1 , 
    Computer 2 *)
val init_state: unit -> t

(** [end_round state] is true if all 3 players have played and it's the end
    of a round. Otherwise false. *)
val end_round: t -> bool

(** [calc_round_score st] calculates the current score of this round. It can
    be called anytime during the round, not all players may have necessarily 
    played yet. *)
val calc_round_score: t -> int

(** [choose_highest_rank suit player] chooses the highest ranked card 
    of [suit] in players' hand that is not worth points or 10 of Clubs.
    @return Some card if the [player] still has 
    card(s) of [suit]. None if [player] no longer has any card of 
    specified [suit]. *)
val choose_highest_rank: Card.card_suit -> player -> Card.card option

(** [jack_diamonds_played st] is true if the jack of diamonds has been 
    played already sometime during this game. False otherwise. *)
val jack_diamonds_played: t -> bool

(** [end_game state] returns true if all cards of the players in [state] 
    have been played *)
val end_game: t -> bool

(** [final_end st] returns true is a player has a score less than -999 or greater
    than 999 *)
val final_end: t -> bool

(** [highest_suit_played st spec_suit] returns the rank of the highest rank of 
    suit already played. If no cards of that suit have been played, return 0. *)
val highest_suit_played: t -> Card.card_suit -> Card.rank

(** [starting_suit s] returns ([Some suit]) of the starting card of the round, or
    if round is empty returns [None]. *)
val starting_suit: t -> Card.card_suit option

(** [get_discard s] generates the discard pile after a round. *)
val get_discard: t -> Card.card list

(** [get_collected s] generates the cards to be collected after a round*)
val get_collected_cards: t -> Card.card list

(**[update_to_next_player u s] returns the state with turn updated to the next 
   player of the one having username [u]. Turns always go clockwise.
   E.g. [s] as "Computer 1" gives a state with next player as Computer 2. *)
val update_to_next_player: string -> t -> t

(** [check_all_hearts lst] is true if lst contains all the hearts, false otherwise*)
val check_all_hearts: Card.card list -> bool

(**[update_player] replaces the [player] in [lst] who has the same username as 
   the input [player] with this input [player] (who is an updated version 
   of the old [player] in [lst]). *)
val update_player: player -> player list -> player list

(** [updated_acc_points st] returns a new intial state with updated accumulated 
    points. Used for multiple games.  *)
val updated_acc_points: t -> t

(** [valid_card suit state] is a helper for update user that checks if the
    player is playing a card that is allowed. A valid card is a card of
    the starting suit *)
val valid_card: Card.card_suit -> t -> bool

(** [get_card command_lst] gets the card from a player's hand. 
    Raises: [CardNotFound] if [command_lst] is not of a proper card. 
    Raises: [InvalidRank] if rank in [command_lst] is invalid.
    Raises: [InvalidSuit] if suit in [command_lst] is invalid. *)
val get_card: string list -> Card.card
