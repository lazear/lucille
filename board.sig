signature BOARD =
sig
  exception Invalid
  
  type board
  type position

  (* board construction and operations *)
  (* create a board with all squares set to Piece.Empty *)
  val empty : unit -> board
  
  (* create a board with the standard starting position *)
  val init  : unit -> board
  val copy : board -> board 
  val sub : board -> position -> Piece.piece
  val update : board -> position -> Piece.piece -> board

  (* is a position on the board? *)
  val valid : position -> bool

  (* Convert a 64-based square number to a position. Positions returned are guaranteed to be valid. Raises Iter *)
  val position : int -> position

  (* iterate over the board, visiting pieces annotated with their positions *)  
  val foldli : (position * Piece.piece  * 'a -> 'a) -> 'a -> board -> 'a 

  (* from 0-based rank and file to positon *)
  val rf : int * int -> position
  val toRankFile : position -> int * int
  val diff : position * position -> int * int


  (* convert to algebraic notation *)
  val toAlg : position -> string
  (* convert from algebraic notation *)
  val fromAlg : string -> position 

  (* movement *)
  datatype dir = N | S | W | E | NW | NE | SW | SE

  val sliding : dir list -> position -> position list
  val stepping: dir list -> position -> position list
  val knight: position -> position list

  val moves : Piece.piece -> position -> bool -> position list

end
