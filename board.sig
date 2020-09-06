signature BOARD =
sig
  exception Invalid
  exception Iter
  
  structure Piece : PIECE  
  type board
  type position

  (* board construction and operations *)
  val foldli : (int * Piece.piece  * 'a -> 'a) -> 'a -> board -> 'a 
  (* create a board with all squares set to Piece.Empty *)
  val empty : unit -> board
  
  (* create a board with the standard starting position *)
  val init  : unit -> board
  val copy : board -> board 
  val sub : board -> position -> Piece.piece
  val update : board -> position -> Piece.piece -> board

  (* is a position on the board? *)
  val valid : position -> bool
  (* iterate over positions. Positions returned are not guaranteed to be valid *)
  val iter : position -> position

  (* return the positions of all pieces of a given rank *)
  val findRanks : board -> Piece.rank -> position list

  (* from 0-based rank and file to positon *)
  val rf : int * int -> position
  val toRankFile : position -> int * int
  val diff : position * position -> int * int

  val unsafeFromInt : int -> position

  (* convert to algebraic notation *)
  val toAlg : position -> string
  (* convert from algebraic notation *)
  val fromAlg : string -> position 
  
  val toFen   : board -> string
  val fromFen : string -> board
end
