open Command
open State
open Card
open Pervasives
open Ai 

exception InvalidPlayer 
(** [print_player_turn username] takes in a [username] and prints the header
    for that player's turn  *)
let print_player_turn player_turn st =
  if (List.length (round_getter st)) = 3 
  then print_string 
      ("\n\n-----------------------------" ^ " Results " ^
       "-----------------------------")
  else (
    match player_turn with
    | "You" -> ANSITerminal.print_string [ANSITerminal.cyan] 
                 ("\n\n----------------------------" ^ 
                  " Your Turn " ^ "----------------------------")
    | "Computer 1" -> ANSITerminal.print_string [ANSITerminal.green] 
                        ("\n\n------------------------"^
                         " Computer 1's Turn "^"------------------------")
    | "Computer 2" -> ANSITerminal.print_string [ANSITerminal.green]
                        ("\n\n------------------------" ^
                         " Computer 2's Turn " ^ "------------------------")
    | _ ->  raise InvalidPlayer )

(** [print_empty_line] prints out an empty line in the board with the sides*)
let print_empty_line () =
  print_string 
    ("    |                      "^"                                |\n")

(** [print_score_helper score] helps print out the score by printing out
    some spaces depending on how many digits [score] has to keep the board even *)
let print_score_helper score =
  if (score >= 0) then print_string " " else ();
  let score' = abs score in
  if score' < 10 then print_string "   " 
  else if score' < 100 then print_string "  "
  else if score' < 1000 then print_string " "
  else ()

(** [print_computer_score (s1, s2)] prints out the scores of the two computers *)
let print_computer_score = function
  | (s1, s2) -> 
    print_string 
      ("\n    |  Collected Points: " ^ (string_of_int s1));
    print_score_helper s1;
    print_string 
      ("     Collected Points: " ^ (string_of_int s2));
    print_score_helper s2;
    print_string " |\n"

(** [get_4_collected collected n] prints out up to four cards in collected and 
    returns the remaining collected cards that need to be printed *)
let rec get_4_collected collected n = 
  match collected with
  | [] -> []
  | h :: t -> if n = 0 then collected else 
      (Card.print_card h; if n <> 1 then 
         print_string ", " else (); 
       get_4_collected t (n - 1))

(** [print_collected_helper collected len] uses get_4_collected and specifies
    the number of cards for it to print out*)
let print_collected_helper collected len=
  if len >= 4 then get_4_collected collected 4 
  else if len = 3 then get_4_collected collected 3
  else if len = 2 then get_4_collected collected 2
  else if len = 1 then get_4_collected collected 1
  else []

(** [count_space_10 collected n] returns the number of cards in [collected] 
    with the number 10 *)
let rec count_space_10 collected n m =
  match collected with
  | [] -> n
  | h :: t -> if (h.number = 10 && m > 0) then count_space_10 t (n + 1) (m-1)
    else count_space_10 t n (m-1)

(** [print_space_10_first collected] prints out some padding space depending
    on how many cards in the collected line for Computer 1 have the digit
    10, since 10 uses up an extra space *)
let print_space_10_first collected =
  match count_space_10 collected 0 4 with
  | 0 -> print_string "              "
  | 1 -> print_string "             "
  | 2 -> print_string "            "
  | 3 -> print_string "           "
  | 4 -> print_string "          "
  | _ -> invalid_arg "Invalid argument"

(** [print_space_10_second collected] prints out some padding space depending
    on how many cards in the collected line for Computer 2 have the digit
    10, since 10 uses up an extra space *)
let print_space_10_second collected =
  match count_space_10 collected 0 4 with
  | 0 -> print_string "      |"
  | 1 -> print_string "     |"
  | 2 -> print_string "    |"
  | 3 -> print_string "   |"
  | 4 -> print_string "  |"
  | _ -> invalid_arg "Invalid argument"

(** [print_collected_spaces collected] prints out some padding space depending
    on how many cards are displayed in the collected line as well as 
    returning the remaining cards needed to be printed out, since each line
    can print out up to 4 cards *)
let print_collected_spaces collected =
  let len = List.length collected in
  let remaining = print_collected_helper collected len in
  if len >= 4 then 
    (print_string " "; remaining)
  else if len = 3 then (print_string 
                          "     "; remaining)
  else if len = 2 then (print_string 
                          "         "; remaining)
  else if len = 1 then (print_string 
                          "             "; remaining)
  else (print_string 
          "               "; remaining)

(** print_collected n (lst1, lst2) is a helper for print_comp_collected that 
    handles the printing of the cards onto separate lines, where n is the total 
    number of lines and the tuple is the two lists of collected cards *)
let rec print_collected n = function
  | (lst1, lst2) -> if n <> 0 then
      (let lst1_remaining = print_collected_spaces lst1 in
       print_space_10_first lst1;
       let lst2_remaining = print_collected_spaces lst2 in
       print_space_10_second lst2;
       print_string "\n    |    ";
       print_collected (n-1) (lst1_remaining, lst2_remaining))
    else ()

(** [print_comp_collected (col1, col2) prints out the collected cards for
    Computer 1 and Computer 2 where the first element of the tuple [(col1, col2)]
    represents the list of collected cards for Computer 1 while the second 
    element represents the list of collected cards for Computer 2 *)
let print_comp_collected = function
  | (col1, col2) -> 
    print_string "    |    "; 
    print_collected 4 (col1, col2)

(** [print-played_cards_helper ocard] is a helper that prints out the played 
    card if a card is played, or __ for a blank spot if no card was played *)
let print_played_cards_helper = function
  | None -> print_string "__ "
  | Some c -> Card.print_card c; if c.number = 10 then ANSITerminal.print_string 
        [ANSITerminal.on_default; ANSITerminal.Inverse] ""
    else print_string " "

(** [print_played_cards card1 card2 card3] calls the helper on 
    [card1], [card2], and [card3] which will print out the played card board *)
let print_played_cards card1 card2 card3 =
  print_string "    |                   ";
  print_played_cards_helper card1;
  print_string "           ";
  print_played_cards_helper card2;
  print_string "                  |\n";
  print_empty_line (); print_empty_line ();
  print_string "    |                          ";
  print_played_cards_helper card3;
  print_string ("                         |\n")

(** [print_turn st] is the final print method that takes in a state and prints
    the turn and board for it *)
let print_turn st : unit =
  let player_turn = turn st in
  let comp1_points = get_total_points st "Computer 1" in
  let comp2_points = get_total_points st "Computer 2" in
  let player_points = get_total_points st "You" in
  let comp1_collected = get_collected st "Computer 1" in
  let comp2_collected = get_collected st "Computer 2" in
  let player_collected = get_collected st "You" in
  let comp1_played = get_played_card st 0 in
  let comp2_played = get_played_card st 1 in
  let player_played = get_played_card st 2 in
  let player_hand = sort_hand (get_hand st "You") in 
  print_player_turn player_turn st;
  print_string ("\n     ______________________________________________________" ^
                "\n    | ");
  ANSITerminal.print_string [ANSITerminal.green] 
    "Computer 1                  Computer 2               ";
  print_string "|";
  print_computer_score (comp1_points, comp2_points);
  print_comp_collected (comp1_collected, comp2_collected);
  print_string 
    "                                                  |\n"; 
  print_empty_line (); 
  print_played_cards comp1_played comp2_played player_played;
  print_empty_line (); print_empty_line (); print_empty_line (); 
  print_empty_line (); 
  print_string 
    ("    | ");
  ANSITerminal.print_string [ANSITerminal.cyan] "Your Hand"; 
  print_string (":              Your Collected Points: " ^ 
                string_of_int player_points);
  print_score_helper player_points;
  print_string " |\n    |    ";
  print_collected 5 (player_hand, player_collected);
  print_string ("                                                  |\n" ^ 
                "    |______________________________________________________|");
  flush_all ();
  Unix.sleep(2)

(** [play_command state] takes in a command and applies it to the current [state].
    So the play command would update the state, quit would quit the game, and 
    rules links to the rules *)
let rec play_command (state: State.t)  = 
  flush_all();
  print_string 
    ("\nEnter a command of the form: play <face_number suit_code> \n(eg: play 5 H " 
     ^ "for 5 of hearts), quit, or rules: ");
  let line = read_line() in
  match (Command.parse line) with
  | exception (Command.Empty) -> 
    print_string ("----- You entered an empty command. Try again. ----- \n"); 
    play_command state  
  | exception (Command.Malformed) -> 
    print_string ("----- You entered a malformed command. Try again. ----- \n");
    play_command state
  | Quit -> print_string ("You quit the game\n");
    Pervasives.exit 0
  | Rules -> print_string ("https://en.wikipedia.org/wiki/Gong_Zhu\n" ^
                           "The game ends when a player passes 1000 " ^
                           "points or -1000 points inclusive.\n");
    play_command state
  | Play (lst) -> (* Player's turn, Comp1 turn, Comp2 turn *)
    try (update_user lst state) 
    with
    | InvalidSuit -> 
      print_string("Try again! You entered an invalid suit. "); 
      play_command state
    | InvalidRank ->
      print_string("Try again! You entered an invalid rank. "); 
      play_command state
    | CardNotFound -> 
      print_string("Try again! Card was not in your hand. "); 
      play_command state
    | InvalidCard ->
      print_string("Try again! You need to play a card of the starting suit ");
      let () = match starting_suit state with 
        | Some x -> print_suit x 
        | None -> print_string ("Error: There is no starting suit") 
      in 
      play_command state
    | PlayerNotFound -> 
      print_string("Error: Player is not a valid player. "); play_command state



(** [play_turn st] is a single turn of a player. Returns the new state after
    the current player (whose turn it is) has played. If it is the user's turn,
    prompts for command from user. *)
let rec play_turn (st: State.t) : unit =
  if end_game st then
    let _ = print_turn st in
    if (final_end st = true) then (
      let your_pts = get_total_points st "You" in 
      let comp1_pts = get_total_points st "Computer 1" in 
      let comp2_pts = get_total_points st "Computer 2" in
      print_string("\n The game is over! The final scores are: \n"
                   ^ "You: " ^ string_of_int your_pts ^ "\nComputer 1: "
                   ^ string_of_int comp1_pts ^ "\nComputer 2: " ^ 
                   string_of_int comp2_pts ^
                   "\nThe winner is: ");
      let top_score = max (max your_pts comp1_pts) comp2_pts in
      let _ = (if top_score = your_pts then print_string("You!\n") else
               if top_score = comp1_pts then print_string("Computer 1!\n") else
               if top_score = comp2_pts then print_string("Computer 2!\n")) in
      Pervasives.exit 0) else ( 
      let () = print_string 
          ("\n The match has ended! A new match is being started: \n") in
      play_turn (updated_acc_points st))
  else 
    match (turn st) with 
    | "Computer 1" -> print_turn st;
      let st' = (update_computer st "Computer 1") in 
      let st'' = if (end_round st') then 
          let _ = print_turn st'; in new_round st' else st' in 
      play_turn st'' 
    | "Computer 2" -> print_turn st;
      let st' = (update_computer st "Computer 2") in 
      let st'' = if (end_round st') then 
          let _ = print_turn st'; in new_round st' else st' in 
      play_turn st''
    | _ -> print_turn st;
      let st' = play_command st in 
      (* After the user inputs, if it is the end of the round then clear 
         round to start new round. *)
      let st'' = 
        if (end_round st') then 
          let () = print_turn st' in
          new_round st' 
        else 
          st'
      in play_turn st''

(** [play_game f] starts the adventure in file [f]. *)
let play_game =
  let init_state = (State.init_state ()) in
  (play_turn init_state)