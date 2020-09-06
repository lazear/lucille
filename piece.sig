signature PIECE = sig
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside

  type algebraic = 
    { piece: rank
    , start: (int * int)
    , final: (int * int) 
    }
 
  val rank : piece -> rank 
  val parse : string -> algebraic
  val toChar : piece -> char
  val fromChar : char -> piece
end
