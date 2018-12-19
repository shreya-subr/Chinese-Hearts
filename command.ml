type card_phrase = string list

type command = 
  | Play of card_phrase
  | Quit 
  | Rules

exception Empty

exception Malformed

(** [parse_command head tail] takes in a string [head], the command name, and 
    string list [tail], the [card_phrase] and returns the command. 

    Requires: same as [parse str] for [head] and elements in [tail] but cannot
    contain any empty strings or spaces. 

    Raises: [Malformed] if the command name is not "play" or "quit" or "rules"
    or if the command name is "play" and there is an empty card phrase
    or if the command name is "quit" and there is a non-empty card phrase
    or if the command name is "rules" and there is a non-empty card phrase *)
let parse_command head tail = 
  match head with 
  | "play" -> if List.length tail != 2 then raise Malformed 
    else Play (tail)
  | "quit" -> if List.length tail = 0 then Quit else raise Malformed
  | "rules" -> if List.length tail = 0 then Rules else raise Malformed
  | _ -> raise Malformed 


(** [strip_empty lst] removes empty strings from string list [lst]*)
let rec strip_empty lst = 
  match lst with 
  | [] -> lst
  | h::t -> if h = "" then strip_empty t else h::(strip_empty t)

(** [split_cmd l] returns the command corresponding to [l] *)
let split_cmd = function
  | [] -> raise Empty
  | h::t -> parse_command h t

(**[parse str] is the command corresponding to [str], where [str] is entered by 
   the user *)
let parse str =
  let lst = str |> String.split_on_char ' ' |> strip_empty in
  split_cmd lst 