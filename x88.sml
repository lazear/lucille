signature X88_SIG =
sig
  exception Iter
  exception Invalid

  (* is a position on the board? *)
  val valid : Word8.word -> bool
	(* iterate over Word8.words. Positions returned are not guaranteed to be valid *)
  val iter : Word8.word -> Word8.word

  val deidx : int -> int * int

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

  exception Iter
  exception Invalid

  val valid = fn x => (x && 0wx88) = 0wx0

  fun iter pos =
    let val x = Word8.+ (pos, 0wx1)
    in if x < 0wx80 then x else raise Iter end

  fun rf (rank,file) =
    let val r = Word8.fromInt rank
        val f = Word8.fromInt file
        val w = (r << 0wx4) || f
    in if valid w then w else raise Invalid end
  
  fun toRankFile w = 
    let val r = Word8.toInt (w >> 0wx4)
        val f = Word8.toInt (w && 0wx7)
    in (r, f) end
  
  fun deidx x = 
    let val w = Word8.fromInt x
        val (r,f) = if valid w then toRankFile w else raise Invalid 
   in (r,f) end

  fun fromAlg (f::r::nil) = 
    let val r = ord r - ord #"1"
        val f = ord f - ord #"a"
    in rf (r, f) end

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