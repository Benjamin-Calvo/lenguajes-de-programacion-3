
fun suma (x,y)=
	if x=y
	then x*2
	else (x+y)

fun copyTextFile(infile: string, outfile: string) =
  let
    val ins = TextIO.openIn infile
    val outs = TextIO.openOut outfile
    fun helper(copt: char option) =
      case copt of
           NONE => (TextIO.closeIn ins; TextIO.closeOut outs)
         | SOME(c) =>  (TextIO.output1(outs,c); helper(TextIO.input1 ins))
  in
    helper(TextIO.input1 ins)
  end

(*
fun readlist(infile: string) = 
    let val ins = TextIO.openIn infile 
	val l = TextIO.inputLine ins 
	val list = (l::nil) 
	fun createlist() = 
	    let val l = TextIO.inputLine ins
	    in case l of
		   NONE => (TextIO.closeIn ins)
		 | SOME l => list = l::list; createlist()
     in createlist() end *)

fun readlist (infile : string) = let
  val ins = TextIO.openIn infile
  fun loop ins =
      case TextIO.inputLine ins of
	  SOME line => line :: loop ins
	| NONE      => []
   in
       loop ins(* before TextIO.closeIn ins*)
   end
(*
fun readdate(hilera : string) =
    if hilera=""
    then NONE
    else if sub(hilera,0)
*)

(*fun copiarTexto(infile: string) =
    val ins = TextIO.openIn infile*)
 

(*fun get infile = 
    TextIO.inputLine TextIO.stdIn;
    ( TextIO.output(TextIO.stdOut, prompt)
                 ; TextIO.flushOut(TextIO.stdOut)
                 ; TextIO.inputLine infile
                 )*)

