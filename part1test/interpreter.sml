datatype token = Err of string | Str of string | Int of int | Name of string * token | Boolean of string | Unit of string | Fun of string | IOFun of string| NoValue of string

(*datatype for binding
datatype for functions*)

fun interpreter(input: string, output: string) =
  let
    val ins = TextIO.openIn input
    val outs = TextIO.openOut output
    fun fileread ins =
      case TextIO.inputLine ins of
           NONE => []
         | SOME c => c :: fileread ins


    fun interpret (instruction_list : string list, s : token list) =
          case instruction_list of [] => (s)
          | x::xs =>
          let
               val spaceSep = String.tokens (fn c => c = #" ") x


               (*checks if it's a name*)
               fun checkAlpha z =
                  case z of [] => false
                  | firstLetter::restLetter => if Char.isAlpha firstLetter
                                               then true
                                               else false
              
               fun negativeFun (b,s) =
                   case  b of [] => s
                   | x::xs => if Char.isDigit(x)
                              then negativeFun(xs,true)
                              else false



               fun checkInt(b,s) =
                   case List.rev b of [] => s
                   | x::xs => if Char.contains "-" x
                              then  negativeFun(xs,false)
                              else if Char.isDigit(x)
                                   then checkInt(xs,true)
                                   else false




              (*custom string concat function //need to add spaces between the things*)

          in
            case spaceSep of commandHead::commandTail => (*case s of [] | x::xs => xs*)
            if commandHead = "push"
            then
                if String.isPrefix "\""  (String.concat commandTail) andalso String.isSuffix "\"\n" (String.concat commandTail) (*if it's a string*)
                     then interpret(xs,Str(String.substring((String.concat commandTail), 1 , String.size(String.concat commandTail) - 3)^ "\n")::s)

                     else if checkInt( List.drop(List.rev(String.explode (String.concat commandTail)) ,1 ) , false)(*if it's an int*)
                          then interpret(xs,Int(valOf(Int.fromString(String.concat commandTail)))::s)

                          else if checkAlpha(String.explode (String.concat commandTail))(*you're pushing in a name*)
                               then interpret(xs,Name(String.concat commandTail , NoValue("nothing"))::s)

                               else interpret(xs,Err(":error:\n")::s)
            else if commandHead = "add\n"
                 then (*add the two top things in the stack*)
                      case s of [] =>(*if the stack is empty*) interpret(xs,Err(":error:\n")::s)
                      | b::[] => (*if the stack only has 1 item*) interpret(xs,Err(":error:\n")::s)
                      | Int(b)::Int(b1)::bs => interpret(xs, Int(b + b1)::bs)
                      | _ => interpret(xs,Err(":error:\n")::s)

                 else if commandHead = "sub\n"
                      then (*minus the top*)
                      case s of [] =>(*if the stack is empty*) interpret(xs,Err(":error:\n")::s)
                      | b::[] => (*if the stack only has 1 item*) interpret(xs,Err(":error:\n")::s)
                      | Int(b)::Int(b1)::bs => interpret(xs, Int(b1 - b):: bs)
                      | _ => interpret(xs,Err(":error:\n")::s)


                      else if commandHead = "mul\n"
                           then (*multiply the thing*)
                           case s of [] =>(*if the stack is empty*) interpret(xs,Err(":error:\n")::s)
                           | b::[] => (*if the stack only has 1 item*) interpret(xs,Err(":error:\n")::s)
                           | Int(b)::Int(b1)::bs => interpret(xs, Int(b * b1)::bs)
                           | _ => interpret(xs,Err(":error:\n")::s)

                           else if commandHead = "div\n"
                                then (*divid*)
                                case s of [] =>(*if the stack is empty*) interpret(xs,Err(":error:\n")::s)
                                | b::[] => (*if the stack only has 1 item*) interpret(xs,Err(":error:\n")::s)
                                | Int(b)::Int(b1)::bs =>
                                if b = 0
                                then interpret(xs,(Err(":error:\n")::s))
                                else interpret(xs, Int(b1 div b)::bs)
                                | _ => interpret(xs,Err(":error:\n")::s)

                                else if commandHead = "rem\n"
                                     then (*remainder*)
                                     case s of [] =>(*if the stack is empty*) interpret(xs,Err(":error:\n")::s)
                                     | b::[] => (*if the stack only has 1 item*) interpret(xs,Err(":error:\n")::s)
                                     | Int(b)::Int(b1)::bs =>
                                     if b = 0
                                     then interpret(xs,(Err(":error:\n")::s))
                                     else interpret(xs, Int(b1 mod b)::bs)
                                     | _ => interpret(xs,Err(":error:\n")::s)

                                     else if commandHead = "neg\n"
                                          then (*negative*)
                                          case s of [] =>(*if the stack is empty*) interpret(xs,Err(":error:\n")::s)
                                          | Int(b)::bs => interpret(xs,Int(~b)::bs)
                                          | _ => interpret(xs,Err(":error:\n")::s)
                                          else if commandHead = "swap\n"
                                               then (*swap*)
                                               case s of [] =>(*if the stack is empty*) interpret(xs,Err(":error:\n")::s)
                                               | b::[] => (*if the stack only has 1 item*) interpret(xs,Err(":error:\n")::s)
                                               | b::b1::bs => interpret(xs, b1::b::bs)
                                           else if commandHead = "pop\n"
                                                 then
                                                 case s of [] => interpret(xs,Err(":error:\n")::s)
                                                 | b::bs => interpret(xs, bs)
                                                 else if commandHead = ":true:\n" (*insert true*)
                                                      then interpret(xs,Boolean(commandHead)::s)
                                                      else if commandHead = ":false:\n" (*insert false*)
                                                           then interpret(xs,Boolean(commandHead)::s)
                                                           else if commandHead = ":error:\n" (*insert error*)
                                                                then interpret(xs,Err(commandHead)::s)
                                                                else interpret(xs,s)






          end

      fun writefile (s : token list, outs) =
      let
          fun negate i =
              if i < 0 then "-" ^ Int.toString (~i) else Int.toString i
      in
          case s of [] => TextIO.closeOut outs
          | x::xs => case x of Str(c) => (TextIO.output(outs , c); writefile(xs , outs))
                        | Int(c) => (TextIO.output(outs , negate c ^ "\n"); writefile(xs , outs))
                        | Name(c, d) => (TextIO.output(outs , c); writefile(xs , outs))
                        | Boolean(c) => (TextIO.output(outs , c); writefile(xs , outs))
                        | Err(c) => (TextIO.output(outs , c); writefile(xs,outs))
      end




  in


     writefile(interpret (fileread ins, []),outs)



  end

(*val _ = interpreter("sample_input1.txt","out1.txt")
val _ = interpreter("sample_input2.txt","out2.txt")
val _ = interpreter("sample_input3.txt","out3.txt")
val _ = interpreter("sample_input4.txt","out4.txt")
val _ = interpreter("sample_input5.txt","out5.txt")
val _ = interpreter("sample_input6.txt","out6.txt")*)
