datatype token = Err of string | Str of string | Int of string | Name of string * string * string | Boolean of string | Unit of string | Fun of string | IOFun of string

(*datatype for binding
datatype for functions*)

fun interpreter(input: string, output: string) =
  let
    val ins = TextIO.openIn input
    val outs = TextIO.openOut output
    val inputstack = []
    fun fileread ins =
      case TextIO.inputLine ins of
           NONE => []
         | SOME c => c :: fileread ins


    fun interpret (instruction_list : string list, s : token list) =
          case instruction_list of [] => (s)
          | x::xs =>
          let
               val spaceSep = String.tokens (fn c => c = #" ") x

               fun concat y =
                  case y of [] => ""
                  | x::xs => x ^ " " ^ concat xs
               (*checks if it's an int*)
               fun check xs = List.all (Char.isDigit) (explode xs)

               (*checks if it's a name*)
               fun checkAlpha z =
                  case z of [] => false
                  | firstLetter::restLetter => if Char.isAlpha firstLetter
                                               then true
                                               else false


          in
            case spaceSep of commandHead::commandTail => (*case s of [] | x::xs => xs*)
            if commandHead = "push"
            then if String.isPrefix "\"" (concat commandTail) andalso String.isSuffix "\"" (concat commandTail) (*if it's a string*)
                 then Str(concat commandTail)::s
                 else if check (concat commandTail)(*if it's an int*)
                      then Int(concat commandTail)::s
                      else if checkAlpha(String.explode (concat commandTail))(*you're pushing in a name*)
                           then Name(concat commandTail , "None", "None")::s
                           else interpret(xs,s)
            else if commandHead = "add"
                 then (*add the two top things in the stack*)
                      case s of [] =>(*if the stack is empty*) s @ [Err(":error:")]
                      | x::[] => (*if the stack only has 1 item*) s @ [Err(":error:")]
                      | x::x1::xs => xs
                 else if commandHead = "sub"
                      then (*minus the top*)
                      case s of [] =>(*if the stack is empty*) s @ [Err(":error:")]
                      | x::[] => (*if the stack only has 1 item*) s @ [Err(":error:")]
                      | x::x1::xs => xs

                      else if commandHead = "mul"
                           then (*multiply the thing*)
                           case s of [] =>(*if the stack is empty*) s @ [Err(":error:")]
                           | x::[] => (*if the stack only has 1 item*) s @ [Err(":error:")]
                           | x::x1::xs => xs
                           else if commandHead = "div"
                                then (*divid*)
                                case s of [] =>(*if the stack is empty*) s @ [Err(":error:")]
                                | x::[] => (*if the stack only has 1 item*) s @ [Err(":error:")]
                                | x::x1::xs => xs
                                else interpret(xs,s)




          end
      fun writefile (s : token list, outs) =
          case s of [] => TextIO.closeOut outs
          | x::xs => case x of Str(c) => TextIO.output1(outs , c)| Int(c) => TextIO.output1(outs , c)| Name(c) => TextIO.output1(outs , c) | Boolean(c) => TextIO.output1(outs , c)| Err(c) => TextIO.output1(outs , c)
           writefile(xs , outs)
      fun printlen(s : token list) =
          print(length s)


  in
  5
(*
     (writefile(interpret (fileread ins, []),outs);
     printlen(interpret(fileread ins, [])))*)

  end

val _ = interpreter("sample_input1.txt","out1.txt")
