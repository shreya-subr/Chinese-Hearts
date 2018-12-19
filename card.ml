open Random
type card_suit = Spades | Hearts | Clubs | Diamonds 
exception InvalidSuit
exception InvalidRank
exception CardNotFound

(** Rank of a card represented by an integer. 
    Can be 1 (Ace), 2-9, 11 (Jack), 12 (Queen), or 13 (King).
     No Jokers are allowed. *)
type rank = int  
type card = {suit: card_suit; number: rank}
let deck = 
  let rec deck' (n, suit') lst = 
    match n with
    | 2 -> deck' (n+1, suit') ({suit = suit'; number =2} :: lst ) 
    | 3 -> deck' (n+1, suit') ({suit = suit'; number =3} :: lst ) 
    | 4 -> deck' (n+1, suit') ({suit = suit'; number =4} :: lst ) 
    | 5 -> deck' (n+1, suit') ({suit = suit'; number =5} :: lst ) 
    | 6 -> deck' (n+1, suit') ({suit = suit'; number =6} :: lst ) 
    | 7 -> deck' (n+1, suit') ({suit = suit'; number =7} :: lst ) 
    | 8 -> deck' (n+1, suit') ({suit = suit'; number =8} :: lst ) 
    | 9 -> deck' (n+1, suit') ({suit = suit'; number =9} :: lst ) 
    | 10 -> deck' (n+1, suit') ({suit = suit'; number =10} :: lst ) 
    | 11 -> deck' (n+1, suit') ({suit = suit'; number =11} :: lst ) 
    | 12 -> deck' (n+1, suit') ({suit = suit'; number =12} :: lst ) 
    | 13 -> deck' (n+1, suit') ({suit = suit'; number =13} :: lst ) 
    | 14 -> (
        (* Determine what the next suit to add onto the deck is *)
        match suit' with
        | Spades ->  deck' (2, Hearts) ({suit = suit'; number =1} :: lst ) 
        | Hearts -> deck' (2, Clubs) ({suit = suit'; number =1} :: lst ) 
        | Clubs -> deck' (2, Diamonds) ({suit = suit'; number =1} :: lst )
        (* Return the final deck *) 
        | Diamonds -> {suit = suit'; number =1} :: lst )
    | _ -> failwith "More than 13 numbers"
  in deck' (2, Spades) []

let get_suit suit_string = 
  match suit_string with
  | "H" | "h" -> Hearts
  | "S" | "s" -> Spades
  | "D" | "d" -> Diamonds
  | "C" | "c" -> Clubs
  | s -> raise InvalidSuit

let get_face str = 
  match str with 
  | "a" | "A" -> 1
  | "j" | "J" -> 11
  | "q" | "Q" -> 12
  | "k" | "K" -> 13
  | _ -> try (let num = int_of_string str
              in if num < 2 || num > 10 then raise InvalidRank else num) 
    with Failure s -> raise InvalidRank 
    
let rec card_of_suit s (lst:card list)= 
  match lst with 
  | [] -> None 
  | c::t -> if c.suit = s then Some c else card_of_suit s t

let three_p_deck = 
  let rec deck' (n, suit') lst = 
    match n with
    | 1 -> deck' (n+1, suit') ({suit = suit'; number =1} :: lst ) 
    | 2 -> if suit' = Spades then deck' (n+1, suit') lst else 
        deck' (n+1, suit') ({suit = suit'; number =2} :: lst ) 
    | 3 -> deck' (n+1, suit') ({suit = suit'; number =3} :: lst ) 
    | 4 -> deck' (n+1, suit') ({suit = suit'; number =4} :: lst ) 
    | 5 -> deck' (n+1, suit') ({suit = suit'; number =5} :: lst ) 
    | 6 -> deck' (n+1, suit') ({suit = suit'; number =6} :: lst ) 
    | 7 -> deck' (n+1, suit') ({suit = suit'; number =7} :: lst ) 
    | 8 -> deck' (n+1, suit') ({suit = suit'; number =8} :: lst ) 
    | 9 -> deck' (n+1, suit') ({suit = suit'; number =9} :: lst ) 
    | 10 -> deck' (n+1, suit') ({suit = suit'; number =10} :: lst ) 
    | 11 -> deck' (n+1, suit') ({suit = suit'; number =11} :: lst ) 
    | 12 -> deck' (n+1, suit') ({suit = suit'; number =12} :: lst ) 
    | 13 -> (
        (* Determine what the next suit to add onto the deck is *)
        match suit' with
        | Spades ->  deck' (1, Hearts) ({suit = suit'; number =13} :: lst ) 
        | Hearts -> deck' (1, Clubs) ({suit = suit'; number =13} :: lst ) 
        | Clubs -> deck' (1, Diamonds) ({suit = suit'; number =13} :: lst )
        (* Return the final deck *) 
        | Diamonds -> {suit = suit'; number =13} :: lst )
    | _ -> failwith "More than 13 numbers"
  in deck' (1, Spades) []

let rec mem (suit:card_suit) (num: rank) d = 
  match d with 
  | [] -> false
  | {suit = suit'; number = num'}::t -> if suit = suit' && num = num' then
      true else mem suit num t

let insert elem n lst = 
  let rec insert' loop n elem lst = 
    match lst with 
    | [] -> elem::[]
    | h::t -> if loop = n then 
        (elem::lst) 
      else 
        h :: (insert' (loop+1) n elem t)
  in insert' 0 n elem lst

let shuffle_once d = 
  let _ = self_init () in 
  let rec shuffle' n unshuf shuffled = 
    match unshuf with 
    | [] -> shuffled
    | h::t -> 
      let shuffled' = insert h (int (n+1)) shuffled in 
      shuffle' (n-1) t shuffled'
  in shuffle' 52 d []

let shuffle () = shuffle_once three_p_deck |> shuffle_once |> shuffle_once 

let print_suit = function
  | Spades -> print_string 
                "♠"
  | Hearts -> ANSITerminal.print_string [ANSITerminal.red] "♥";
  | Clubs -> print_string 
               "♣"
  | Diamonds -> ANSITerminal.print_string [ANSITerminal.red] "♦"

let print_card (card : card) =
  let suit = card.suit in 
  let number = card.number in
  if (number < 11 && number > 1) then
    (print_string  
       (string_of_int number);
     print_suit suit)
  else (
    match number with
    | 1 -> print_string 
             "A"; 
      print_suit suit
    | 11 -> print_string 
              "J"; 
      print_suit suit
    | 12 -> print_string 
              "Q"; 
      print_suit suit
    | 13 -> print_string 
              "K"; 
      print_suit suit
    | 10 -> print_string 
              "T";
      print_suit suit
    | _ -> ()
  )

let rec card_of_heart lst highest max =
  match lst with
  | [] -> highest
  | c::t -> 
    if c.suit = Spades then card_of_heart t highest max else (
      if c.suit = Hearts then (
        match highest with
        | None -> if (c.number < max) then
            (card_of_heart t (Some c) max) else Some c (*still have to play heart*)
        | Some high -> if (c.number > high.number && c.number < max) then
            (card_of_heart t (Some c) max) else (Some high))
      else (match highest with
          | None -> None
          | Some high' -> highest))

let rec card_of_spade lst highest max =
  match lst with
  | [] -> highest
  | c::t ->
    if c.suit = Spades then (
      if (c.number = 12 && max > 12) then (Some c) else (
        match highest with
        | None -> if (c.number < max) then
            (card_of_spade t (Some c) max) else Some c
        | Some high -> if (c.number > high.number && c.number < max) then
            (card_of_spade t (Some c) max) else (Some high)))
    else (match highest with
        | None -> None
        | Some high' -> highest)


let score_card = function
  | {suit = s; number = n} -> 
    if(s = Spades && n = 12) then -100 else
    if(s = Hearts && n = 1) then -50 else
    if(s = Hearts && n = 13) then -40 else
    if(s = Hearts && n = 12) then -30 else
    if(s = Hearts && n = 11) then -20 else
    if(s = Hearts && n >= 5) then -10 else
    if(s = Hearts && n>= 2) then 0 else
    if(s = Diamonds && n = 11) then 100 else 0

let risk_score card = 
  match (card.suit, card.number) with 
  | (Spades, 12)  -> -100
  | (Hearts, 1) -> -90
  | (Spades, 1) -> -80
  | (Hearts, 13) -> -80
  | (Spades, 13) -> -70
  | (Clubs, 10) -> -65
  | (Hearts, 12) -> -60
  | (Hearts, 11) -> -50
  | (Hearts, 10) -> -40
  | (Diamonds, 11) -> 20
  | (Hearts, s) -> if s > 4 then -30 else 0
  | _ -> 0


let suit_risk cards suit = 
  let rec suit_risk' suit score num = function 
    | [] -> (score, num) 
    | h::t -> 
      if h.suit = suit then 
        let score' = score + risk_score h in 
        let num' = if (risk_score h = 0) then num + 1 else num in 
        suit_risk' suit score' num' t
      else suit_risk' suit score num t in
  suit_risk' suit 0 0 cards

(** [suit_value suit] assigns values to each of the suits. Helper function
    for [sort_hand cards]. *)
let suit_value suit = 
  match suit with
  | Spades -> 100
  | Hearts -> 200
  | Clubs -> 300
  | Diamonds -> 400

let compare_card c1 c2 =
  let c1_value = (suit_value (c1.suit)) + 
                 (if c1.number = 1 then 14 else c1.number) in 
  let c2_value = (suit_value (c2.suit)) + 
                 (if c2.number = 1 then 14 else c2.number) in 
  Pervasives.compare c1_value c2_value 

let sort_hand cards = List.stable_sort compare_card cards 

(** [compare_risk c1 c2] is the compare function to sort cards by risks. 
    Risk is determined by [Card.risk_score card] *)
let compare_risk c1 c2 = Pervasives.compare (risk_score c1) (risk_score c2)

(** [compare_score c1 c2] is the compare function to sort cards by score. 
    Risk is determined by [Card.score_card card] *)
let compare_score c1 c2 = Pervasives.compare (score_card c1) (score_card c2)

let sort_by_risk cards = List.stable_sort compare_risk cards 

let sort_by_score cards = List.stable_sort compare_score cards 










