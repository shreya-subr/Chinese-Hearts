type card_phrase = string list

(** [command] represents valid commands a user can make *)
type command = 
  | Play of card_phrase
  | Quit 
  | Rules

(** [Empty] is thrown when an empty string is parsed. *)
exception Empty

(** [Malformed] is thrown when a non-valid command is parsed. Examples that would
    throw this include too many words entered, too few words entered, and not a valid
    command.  *)
exception Malformed

(**[parse str] is the command corresponding to [str], where [str] is entered by 
   the user *)
val parse: string -> command

