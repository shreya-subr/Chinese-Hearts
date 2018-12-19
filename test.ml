open OUnit2
open Card



(** [not_eq a b] is true if [a] does not equal [b]. False otherwise. *)
let not_eq a b = a <> b

(** [card_tests] is the test suite for [Card] module *)
let card_tests =  [
  "deck len"  >:: (fun _ -> assert_equal 52 (List.length deck));
  "deck mem A"  >:: (fun _ -> assert_equal true (mem Diamonds 1 deck));
  "deck mem K"  >:: (fun _ -> assert_equal true (mem Diamonds 13 deck));
  "deck mem 2"  >:: (fun _ -> assert_equal true (mem Diamonds 2 deck));
  "deck mem Spades"  >:: (fun _ -> assert_equal true (mem Spades 2 deck));
  "deck mem Hearts"  >:: (fun _ -> assert_equal true (mem Hearts 2 deck));
  "deck mem Clubs"  >:: (fun _ -> assert_equal true (mem Clubs 2 deck));
  "get suit Hearts" >:: (fun _ -> assert_equal Hearts (get_suit "H"));
  "get face a" >:: (fun _ -> assert_equal 1 (get_face "a"));
  "get face 5" >:: (fun _ -> assert_equal 5 (get_face "5"));
  "three player deck mem"  >:: (fun _ -> assert_equal true 
                                   (mem Clubs 2 three_p_deck));

  "shuffle len"  >:: (fun _ -> assert_equal 52 (List.length (shuffle_once deck)));
  "shuffle mem A"  >:: (fun _ -> assert_equal true (mem Diamonds 2 
                                                      (shuffle_once deck)));
  "shuffle mem Spades K"  >:: 
  (fun _ -> assert_equal true (mem Spades 2 
                                 (shuffle_once deck)));
  "shuffle not eq, ran"  >:: 
  (fun _ -> assert_equal (shuffle_once deck) 
      (shuffle_once deck) ~cmp: not_eq);
  "risk score 1" >:: (fun _ -> assert_equal (-100) 
                         (risk_score {suit = Spades; number = 12}));
  "risk score 2" >:: (fun _ -> assert_equal (0) 
                         (risk_score {suit = Spades; number = 10}));
  "score card 1" >:: (fun _ -> assert_equal (-100) 
                         (risk_score {suit = Spades; number = 12}));
  "score card 2" >:: (fun _ -> assert_equal (0) 
                         (risk_score {suit = Spades; number = 10}));
  "score card 3" >:: (fun _ -> assert_equal (100) 
                         (score_card {suit = Diamonds; number = 11}));
  "insert at 2" >:: (fun _ -> assert_equal [1;2;3;4] 
                        (insert 3 2 [1;2;4]));
  "insert at 3" >:: (fun _ -> assert_equal [1;2;3;4] 
                        (insert 4 3 [1;2;3]));
  "compare card different suits" >:: (fun _ -> assert_equal true 
                                         ((compare_card {suit=Spades; number=13} 
                                             {suit = Diamonds;number=2}) < 0));
  "compare card same suits" >:: (fun _ -> assert_equal true 
                                    ((compare_card {suit=Spades; number=13} 
                                        {suit = Spades;number=2}) > 0));
  "compare card Ace" >:: (fun _ -> assert_equal true 
                             ((compare_card {suit=Hearts; number=13} 
                                 {suit = Hearts;number=1}) < 0));
  "suit risk same suits" >:: (fun _ -> 
      assert_equal (-170,2) (suit_risk [
          {suit=Spades; number =2}; {suit=Spades;number=3};
          {suit = Spades;number=12}; {suit=Spades;number=13}] Spades));
  "suit risk different suits" >:: (fun _ -> 
      assert_equal (-170,1) (suit_risk [
          {suit=Spades; number =2}; {suit=Hearts;number=3};
          {suit = Spades;number=12}; {suit=Spades;number=13}] Spades));
  "sort by risk Q of S" >:: (fun _ -> 
      assert_equal ({suit = Spades; number = 12}) (sort_by_risk deck |> List.hd));
  "sort by score Q of S" >:: (fun _ -> 
      assert_equal ({suit = Spades; number = 12}) (sort_by_score deck |> List.hd));
  "sort by score A of H" >:: (fun _ -> 
      assert_equal ({suit = Hearts; number = 1}) (List.nth (sort_by_score deck) 1));
  "card of suit None" >:: (fun _ -> 
      assert_equal None (card_of_suit Diamonds []));
  "card of suit None 2" >:: (fun _ -> 
      assert_equal None 
        (card_of_suit Diamonds 
           [{suit = Hearts; number = 3}; 
            {suit = Spades; number = 4}; {suit = Clubs; number = 5}]));
  "card of suit Some" >:: (fun _ -> 
      assert_equal (Some {suit = Diamonds; number = 4})
        (card_of_suit Diamonds 
           [{suit = Hearts; number = 3}; 
            {suit = Diamonds; number = 4}; {suit = Diamonds; number = 5}]));
  "card of heart" >:: (fun _ ->
      assert_equal (Some {suit=Hearts; number=9}) 
        (card_of_heart [{suit=Spades; number=13}; {suit=Hearts; number=9}; 
                        {suit=Hearts; number=12}] None 10));
  "card of heart big" >:: (fun _ ->
      assert_equal (Some {suit=Hearts; number=12}) 
        (card_of_heart [{suit=Spades; number=13}; {suit=Hearts; number=12}; 
                        {suit=Hearts; number=13}] None 10));
  "card of heart None" >:: (fun _ ->
      assert_equal (None) 
        (card_of_heart [{suit=Spades; number=5}; {suit=Diamonds; number=5}; 
                        {suit=Diamonds; number=6}] None 10));
  "card of spade pig high" >:: (fun _ ->
      assert_equal (Some {suit=Spades; number=10}) 
        (card_of_spade [{suit=Spades; number=5}; {suit=Spades; number=7}; 
                        {suit=Spades; number=10}] None 12));
  "card of spade high" >:: (fun _ ->
      assert_equal (Some {suit=Spades; number=12}) 
        (card_of_spade [{suit=Spades; number=5}; {suit=Spades; number=12}; 
                        {suit=Spades; number=13}] None 14));
  "card of spade None" >:: (fun _ ->
      assert_equal (None) 
        (card_of_spade [{suit=Hearts; number=5}; {suit=Diamonds; number=5}; 
                        {suit=Diamonds; number=6}] None 10));
]

open State
open Ai
(**  Initializing the variables for testing State. *)
let init_st = State.(init_state ())
let players = State.get_players init_st
let user = get_player "You" init_st
let user_hand = get_hand init_st "You" 

(**  [user_suit n] converts a [card_suit] to a string. *)
let user_suit n = 
  match n with 
  | Spades -> "S"
  | Hearts -> "H"
  | Clubs -> "C"
  | Diamonds -> "D"

(**  [user_num n] converts a [card rank] to a string. *)
let user_num number = 
  if (number < 11 && number > 1) then
    string_of_int number
  else 
    match number with
    | 1 ->  "A"
    | 11 -> "J"
    | 12 ->  "Q"
    | 13 -> "K"
    | 10 -> "T"
    | _ -> ""

(**  Initializing the variables for testing State. *)
let comp1 = get_player "Computer 1" init_st
let comp2 = get_player "Computer 2" init_st 
let st2 = update_computer init_st "Computer 1"
let st3 = update_computer st2 "Computer 2"

(** [card_suit st] gets the option card of starting suit of [st] *)
let card_suit st = 
  match starting_suit st with
  | Some s -> s
  | None -> failwith "Something went wrong"

(** [get_another_suit st] gets a suit different than the starting suit
    of [st]. *)
let get_another_suit st =
  match card_suit st with 
  | Hearts -> Diamonds
  | Diamonds -> Spades
  | Spades -> Clubs
  | Clubs -> Hearts 

(** [user_card] gets the card of same suit as starting from option. *)
let user_card st = match card_of_suit (card_suit st) user_hand with
  | Some s -> s
  | None -> failwith "This error is supposed to happen once in a while due
  to the nature of the inability to predict what is in user's hand. 
  The player doesn't have the starting suit due to random shuffling. Just make
  test again. "

let st4 = update_user [user_num (user_card st3).number; 
                       user_suit (user_card st3).suit] st3
(** [clean_round] creates a new round. *)
let clean_round = new_round st4

(** [state_tests] creates a test suite for State *)
let state_tests = [
  "sort" >:: (fun _ -> assert_equal (List.rev deck) (sort_hand deck));
  "sort shuffled" >:: (fun _ -> assert_equal (List.rev deck) 
                          (sort_hand (shuffle_once deck)));
  "collected empty" >:: (fun _ -> assert_equal [] (get_collected st2 "You"));
  "points 0" >:: (fun _ -> assert_equal 0 (get_points st2 "You"));

  "init_state hands" >:: (fun _ -> assert_equal 17
                             (List.length (get_hand init_st "You")));                      
  "init_state players" >:: (fun _ -> assert_equal 3 
                               (List.length players ));
  "init_state player function length" >:: (fun _ -> 
      assert_equal 3 (List.length (State.get_players init_st)));
  "turn" >:: (fun _ -> assert_equal "Computer 1" (turn (init_state ())));
  "init_state collected "  >:: (fun _ -> assert_equal [] 
                                   ((get_collected (init_state ()) "Computer 2")));
  "init_state collected "  >:: (fun _ -> assert_equal [] 
                                   ((get_collected (init_state ()) "You")));
  "end_game" >:: (fun _ -> assert_equal false (end_game (init_state ())));
  "end round false" >:: (fun _ -> assert_equal false (end_round (init_state ())));
  "get_points zero comp1" >:: 
  (fun _ -> assert_equal 0 (get_points (init_state ()) "Computer 1"));
  "get_points zero comp2" >:: 
  (fun _ -> assert_equal 0 (get_points (init_state ()) "Computer 2"));
  "get_points zero you" >:: 
  (fun _ -> assert_equal 0 (get_points (init_state ()) "You"));

  "Round getter init" >:: (fun _ -> assert_equal 0 (round_getter (init_state ()) 
                                                    |> List.length));
  "Round getter st2" >:: (fun _ -> 
      assert_equal 1 (round_getter st2 |> List.length));
  "Round getter st3" >:: (fun _ -> 
      assert_equal 2 (round_getter st3 |> List.length));
  "update_computer1 turn" >:: (fun _ -> assert_equal "Computer 2" (State.turn (
      update_computer (init_state ()) "Computer 1")));
  "update_computer2 turn" >:: (fun _ -> assert_equal "You" (State.turn (
      update_computer st2 "Computer 2")));
  "update_user1" >:: (fun _ -> assert_equal 16 (List.length (
      get_hand (update_user [user_num (user_card st3).number; 
                             user_suit (user_card st3).suit] st3 ) "You")));
  (* Turn should be incremented since update_user and update_computer 
     should increment turn automatically after each turn. *)
  "update_user turn" >:: (fun _ -> assert_equal "Computer 1" (State.turn (
      update_user [user_num (user_card st3).number; 
                   user_suit (user_card st3).suit] st3)));
  "end round true" >:: (fun _ -> assert_equal true (end_round st4));
  "get_collected_cards and get_discard" 
  >:: (fun _ -> assert_equal 3 (List.length (get_collected_cards st4) +
                                List.length (get_discard clean_round)) 
          ~printer:string_of_int); 
  "Round getter" >:: (fun _ -> assert_equal 3 (round_getter st4 |> List.length));
  (* Sets Computer 1 as Winner *)
  "update to next player" >:: (fun _ -> 
      assert_equal "Computer 1" ((update_to_next_player "You" st4) 
                                 |> State.turn)~printer:String.escaped);
  "all hearts t" >:: (fun _ -> assert_equal true (check_all_hearts (sort_hand deck)));
  "all hearts f1" >:: (fun _ -> assert_equal false (check_all_hearts []));
  "all hearts t" >:: (fun _ -> assert_equal true (check_all_hearts 
                                                    [{number = 1; suit = Hearts};
                                                     {number = 2; suit = Hearts};
                                                     {number = 3; suit = Hearts};
                                                     {number = 4; suit = Hearts};
                                                     {number = 5; suit = Hearts};
                                                     {number = 6; suit = Hearts};
                                                     {number = 7; suit = Hearts};
                                                     {number = 8; suit = Hearts};
                                                     {number = 9; suit = Hearts};
                                                     {number = 10; suit = Hearts};
                                                     {number = 11; suit = Hearts};
                                                     {number = 12; suit = Hearts};
                                                     {number = 13; suit = Hearts};
                                                    ]));
  "all hearts f2" >:: (fun _ -> assert_equal false (check_all_hearts 
                                                      [{number = 1; suit = Hearts};
                                                       {number = 2; suit = Hearts};
                                                      ]));
  "username you" >:: (fun _ -> assert_equal "You" (username user));  
  "username comp1" >:: (fun _ -> assert_equal "Computer 1" (username comp1));   
  "first player comp1" >:: (fun _ -> assert_equal "Computer 1" 
                               (first_player init_st));  
  "get_played_card user" >:: (fun _ -> assert_equal (Some (user_card st3)) 
                                 (get_played_card st4 2));      
  "last player true" >:: (fun _ -> assert_equal true
                             (last_player st3));  
  "last player false" >:: (fun _ -> assert_equal false
                              (last_player st4));  
  "init_state round score" >:: (fun _ -> assert_equal 0
                                   (calc_round_score init_st));  
  "valid card false" >:: (fun _ -> assert_equal false 
                             (valid_card (get_another_suit st4) st4));  
  "valid card true" >:: (fun _ -> assert_equal true 
                            (valid_card (card_suit st4) st4));  

]

(** Tests for the Ai module *)
let ai_tests = [
  "InvalidRank for update_user" >:: 
  (fun _ -> assert_raises Card.InvalidRank 
      (fun () -> (update_user ["p"; "h"] (init_state ()))));
  "InvalidSuit for update_user" >:: 
  (fun _ -> assert_raises Card.InvalidSuit 
      (fun () -> (update_user ["5"; "M"] (init_state ()))));
  "update_computer1" >:: (fun _ -> assert_equal 16 (List.length (
      get_hand (update_computer (init_state ()) "Computer 1") "Computer 1")));
  "update_computer2" >:: (fun _ -> assert_equal 16 (List.length (
      get_hand (update_computer st2 "Computer 2") "Computer 2")));

]

(** Combine all the tests *)
let suite = "game and state" >::: [
    "test suite for game and state"  >::: List.flatten [
      card_tests;
      state_tests;
      ai_tests
    ]
  ]
(** Run the tests *)
let _ = run_test_tt_main suite