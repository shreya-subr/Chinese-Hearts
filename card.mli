(** Represents the four standard card suits *)
type card_suit = Spades | Hearts | Clubs | Diamonds 

(** Rank of a card represented by an integer. 
    Can be 1 (Ace), 2-9, 11 (Jack), 12 (Queen), or 13 (King).
     No Jokers are allowed. *)
type rank = int

(** [card] is a record representing a card. *)
type card = {suit: card_suit; number: rank}

(** [InvalidSuit] is the exception for a suit that is not of a proper suit as 
    defined by [card_suit]. *)
exception InvalidSuit

(** [Invalid] is the exception for a rank that is not of a numerical value
    associating with a proper rank. Any numbers outside of the range [1,13] 
    inclusive on both ends will raise this exception. *)
exception InvalidRank

(** [get_suit str] gets the suit of a card corresponding to the string. 
    Ex: "S" or "s" gets Spades. 
    Raises: [InvalidSuit] if suit is invalid. *)
val get_suit : string -> card_suit

(** [get_face str] returns the numerical value of a rank. For example, 
    Jack is 11, Ace is 1. 
    Raises: [InvalidRank] if the string is not a valid rank. *)
val get_face : string -> int

(** [card_of_suit c lst] takes a list of cards [lst] and a suit [s] and is
    the first [Some card] in [lst] with suit [s]. Is [None] if no such card exists. 
*)
val card_of_suit: card_suit -> card list -> card option

(** [card_of_heart lst highest max] takes a list of cards [lst] and returns
    Some card that is the highest ranking Heart that is less than the rank of max.
    If [lst] still has hearts but no heart less than rank of max, then return
    smallest heart. Is None if there is no more hearts.  *)
val card_of_heart: card list -> card option -> rank -> card option

(** [card_of_spade lst highest max] takes a list of cards [lst] and returns
    Some card that is the highest ranking Spade that is less than the rank of max,
    or the smallest Spade if they have a Spade but not one that is of rank smaller
    than max, else returns None *)
val card_of_spade: card list -> card option -> rank -> card option

(** [mem suit num d] checks if card with this [suit] and [num] are
    a member of list [d]. Returns true if this card is in [d], false 
    otherwise. *)
val mem : card_suit -> rank -> card list -> bool

(** [insert elem n lst] inserts [elem] in [lst] at index n. Indices
    must be between [0, List.length [lst]] inclusive on both ends. 
    Inserting at index List.length [lst] appends the element at the
    very end of the list. *)
val insert : 'a -> int -> 'a list -> 'a list

(** [shuffle d] shuffles deck [d]. This function should be called at least twice
    if being used on default deck. *)
val shuffle_once : 'a list -> 'a list

(** [shuffle ()] creates a completely shuffled three player deck of 51 cards. *)
val shuffle : unit -> card list

(** [deck] creates a three person deck of 52 cards. *)
val deck: card list

(** [three_p_deck] creates a three person deck of 51 cards. *)
val three_p_deck: card list

(** [print_suit card_suit] prints out the [suit]. *)
val print_suit: card_suit -> unit

(** [print_card card] prints out the [card] with its number and suit *)
val print_card: card -> unit

(** [score_card c] is the score associated with card [c]. Scores
    follow the official rules of Hearts. *)
val score_card: card -> int

(** [risk_score card] gives the numerical risk associated with [card]. 
    A higher risk card has a more negative score. 
    e.g. Queen of Spades which has -100 points has the highest risk. 
    Cards worth no points have 0 risk. 
    Jack of Diamonds has positive points (lowest risk) since it's a highly
    desirable card. *)
val risk_score: card -> int

(** [suit_risk cards suit] sums up the total risk points of the a player's
    [cards] of this [suit] and sums the total number of 0 risk cards of [suit] 
    in cards. Returns the tuple (risk score, number). 

    For example:
    A deck of cards with Spades has 3 S, 4 S, Q S, K S. 
    The risk score would be -170 (based on [Card.risk_score card] and the
    number of 0 risk cards is 2. The function would return (-170, 2) *)
val suit_risk: card list -> card_suit -> int * int

(** [compare_card c1 c2] is the compare function for cards. Sorts according to
    [sort_hand cards] except Ace is the highest. *)
val compare_card: card -> card -> int


(** [sort_hand cards] sorts a card list [cards] based on first suit
    and then number. The order is Spades, Hearts, Clubs, Diamonds and numbers
    are sorted in ascending order. 2 is the lowest rank and A is the highest. 
    e.g. If [cards] contained A of Hearts, Q of Hearts, 
    J of Spades, and 3 of Clubs, K of Diamonds, A of Diamonds, 
    then the sorted list would be J of Spades, Q of Hearts, A of Hearts, 3 of 
    Clubs, K of Diamonds, A of Diamonds. 
*)
val sort_hand: card list -> card list

(** [sort_by_risk cards] sorts a card list by risk. Higher risk cards are 
    at front while lower risk cards are in the back. The risk of a card is 
    determined by [Card.risk_score c]. 
    e.g. Queen of Spades has the highest risk of all cards so, if present, 
    it would always be the first card. *)
val sort_by_risk: card list -> card list

(** [sort_by_score cards] sorts a card list by score. More negative scores are 
    at front while more positive scores in the back. The score of a card is 
    determined by [Card.score_card c]. 
    e.g. Queen of Spades has the highest score of all cards so, if present, 
    it would always be the first card. Next would be the Ace of Hearts, etc. *)
val sort_by_score: card list -> card list


