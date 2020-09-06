signature X88_SIG =
sig
  exception Invalid

  (* is a position on the board? *)
  val valid : Word8.word -> bool
  
  (* Convert a 64-square based integer to an internal position. This is guaranteed to be O(1) *)
  val position : int -> Word8.word
  
  (* Foldl over board positions *)
  val foldl : (Word8.word * 'a -> 'a) -> 'a -> 'a

  (* from 0-based rank and file to positon *)
  val rf : int * int -> Word8.word
  val toRankFile : Word8.word -> int * int
  val diff : Word8.word * Word8.word -> int * int

  (* convert to algebraic notation *)
  val toAlg : Word8.word -> string
  (* convert from algebraic notation *)
  val fromAlg : string -> Word8.word 
end

structure X88 :> X88_SIG =
struct
  local
    open Word8
  in
    val >> = >>
    val << = <<
    val && = andb
    val || = orb
    val ^^ = xorb
    infix 4 >> <<
    infix 3 && || ^^
  end

  exception Invalid
  fun $ (f, g) = f g
  infixr 4 $

  val valid = fn x => (x && 0wx88) = 0wx0
  
  val squares = Vector.fromList $ List.filter valid $ List.tabulate (0x80, Word8.fromInt)

  fun position pos = Vector.sub (squares, pos) handle Subscript => raise Invalid
  
  fun foldl f acc = Vector.foldl f acc squares

  fun rf (rank,file) =
    let val r = Word8.fromInt rank
        val f = Word8.fromInt file
        val w = (r << 0wx4) || f
    in if valid w then w else raise Invalid end
  
  fun toRankFile w = 
    let val r = Word8.toInt (w >> 0wx4)
        val f = Word8.toInt (w && 0wx7)
    in (r, f) end
  
  fun fromAlg (f::r::nil) = 
    let val r = ord r - ord #"1"
        val f = ord f - ord #"a"
    in rf (r, f) end
    | fromAlg _ = raise Fail "invalid algebraic notation"
  val fromAlg = fromAlg o explode

  fun toAlg w = 
    let val (r, f) = toRankFile w
        val r = chr (r + ord #"1")
        val f = chr (f + ord #"a")
    in implode [f,r] end
  
  fun diff (a, b) = 
    let val x = Word8.- (a, b) in if valid x then toRankFile x else
        toRankFile (Word8.notb x) end
end
