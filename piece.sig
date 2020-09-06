signature PIECE = sig
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside


  val rank : piece -> rank 
(*  val parse : string -> algebraic*)
  val toChar : piece -> char
  val fromChar : char -> piece
end
