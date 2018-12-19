(** [update_user lst] returns the new state updating the user after 
    they have played card stored in [lst] where [lst] is of the form [face;suit]
    Raises: 
    [InvalidSuit] if the suit entered is not a valid suit. 
    [InvalidRank] if the rank is invalid
    [CardNotFound] if the card is not in hand
    [InvalidCard] if the card is not a valid card 
    [PlayerNotFound] if the player is not a player in this game
*)
val update_user: string list -> State.t -> State.t

(** [update_computer s player] makes computer [player] play a card. 
    The card chosen depends on what the AI determines is the best strategy given 
    this [state].*)
val update_computer: State.t -> string -> State.t