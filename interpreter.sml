fun interpreter(input: string, output: string) =
  let
    val ins = TextIO.openIn input
    val outs = TextIO.openOut output
    val inputstack = []
    val spaceSep = String.tokens
    fun fileread ins =
      case TextIO.inputLine ins of
           NONE => []
         | SOME c => c :: fileread ins

    val spaceSet = String.tokens (fn c => c = #" ") (hd (fileread ins))
    fun command input : string =
          
    fun interpret list : string list =
           if null list
           then []
           else if String.isSubstring "push " (hd list) andalso spaceSet
                 then String.extract(hd list, 5,NONE) :: interpret (tl list)
                 else  ":error:" :: interpret (tl list)
     in
     interpret (fileread ins)
    end
