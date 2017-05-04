datatype token = Err of string | Str of string | Int of int | Name of string | Boolean of string | Unit of string |Function of (string list * token list * (string * token * string) list * string) | IOfun of (string list * token list * (string * token * string) list * string)

fun interpreter(input: string, output: string) =
  let
    val ins = TextIO.openIn input
    val outs = TextIO.openOut output





    fun fileread ins =
      case TextIO.inputLine ins of
           NONE => []
         | SOME c => c :: fileread ins
(*checks if a key exist in the bindStack*)



    fun interpret (instruction_list : string list, s : token list list, bindStack : (string * token * string) list list) =
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


                fun returnValue(key,bindStack : (string * token * string) list) =
                case bindStack of [] => ":error:"
                |x::xs => if #1 x = key
                          then case #2 x of Str(c) => c
                               |Int(c) => Int.toString(c)
                               |Boolean(c) => c
                               |Unit(c) => c
                          else returnValue(key,xs)


                fun returnType(key,bindStack : (string * token * string) list) =
                case bindStack of [] => ":error:"
                |x::xs => if #1 x = key
                          then #3 x
                          else returnType(key,xs)

                fun concat(sentence : string list,s) =
                case sentence of [] => s
                |x::xs => concat(xs, s ^ x ^ " ")

                fun addtoBind(bind : (string * token * string) list list, toAdd) =
                case bind of [] => (toAdd::[])::bind
                |x::xs => (toAdd::x)::xs

                fun getHead(list) =
                case list of [] => []
                |x::xs => x

                fun Front(list) =
                case list of [] => ""
                |x::xs => x

                fun getLast(list) =
                case list of [] => getLast(list)
                |x::xs => x

                fun getTail(list) =
                case list of [] => list
                |x::xs => xs

                fun isFun (list) =
                if List.nth(list,0) = "fun"
                then if checkAlpha(String.explode (List.nth(list,1)))
                     then if checkAlpha(String.explode (List.nth(list,2)))
                          then true
                          else false
                      else false
                else false

                fun isIO (list) =
                if List.nth(list,0) = "inOutFun"
                then if checkAlpha(String.explode (List.nth(list,1)))
                     then if checkAlpha(String.explode (List.nth(list,2)))
                          then true
                          else false
                      else false
                else false

                fun makeInstruction (list,s) =
                case list of [] => s
                |x::xs => if x = "funEnd\n"
                          then x::s
                          else makeInstruction(xs,x::s)

                fun makeIOnstruction(list,s) =
                case list of [] => s
                |x::xs => if x = "return\n"
                          then makeIOnstruction(xs,"returnIO\n"::s)
                          else if x = "funEnd\n"
                               then x::s
                          else makeIOnstruction(xs,x::s)

                fun SkipInstruction(list) =
                case list of [] => []
                |x::xs => if x = "funEnd\n"
                          then xs
                          else SkipInstruction(xs)

                fun theBind(list,s) =
                case list of [] => (s::[])
                |x::xs => s::list

                fun getInstruction(token) =
                case token of Function(c) => #1 c
                |IOfun(c) => #1 c
                | _ => []

                fun getBind(token,arg,Type) =
                case token of Function(c) => addtoBind([]::bindStack, (#4 c, arg ,Type))
                |IOfun(c) => addtoBind([]::bindStack, (#4 c, arg ,Type))
                | _ => []

                fun getFun(key,bindStack : (string * token * string) list) =
                case bindStack of [] => Err(":error:\n")
                |x::xs => if #1 x = key
                          then #2 x
                          else getFun(key,xs)



          in
            case spaceSep of  [] => interpret(xs,s,bindStack)(*case s of [] | x::xs => xs*)
            |commandHead::commandTail =>
            if commandHead = "push"
            then
            case s of [] =>
                          if String.isPrefix "\""  (String.concat commandTail) andalso String.isSuffix "\"\n" (String.concat commandTail) (*if it's a string*)
                               then interpret(xs,(Str(String.substring(concat (commandTail, ""), 1 , String.size(concat (commandTail,"")) - 4)^ "\n")::[])::[],bindStack)

                               else if checkInt( List.drop(List.rev(String.explode (String.concat commandTail)) ,1 ) , false)(*if it's an int*)
                                    then interpret(xs,(Int(valOf(Int.fromString(String.concat commandTail)))::[])::([]),bindStack)

                                    else if checkAlpha(String.explode (String.concat commandTail))(*you're pushing in a name*)
                                         then interpret(xs,(Name(String.concat commandTail)::[])::([]),bindStack)

                                         else interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
             |t::ts =>
                if String.isPrefix "\""  (String.concat commandTail) andalso String.isSuffix "\"\n" (String.concat commandTail) (*if it's a string*)
                     then interpret(xs,(Str(String.substring(concat (commandTail, ""), 1 , String.size(concat (commandTail,"")) - 4)^ "\n")::(t))::(ts),bindStack)

                     else if checkInt( List.drop(List.rev(String.explode (String.concat commandTail)) ,1 ) , false)(*if it's an int*)
                          then (print(Front(commandTail));interpret(xs,(Int(valOf(Int.fromString(String.concat commandTail)))::(t))::(ts),bindStack))

                          else if checkAlpha(String.explode (String.concat commandTail))(*you're pushing in a name*)
                               then interpret(xs,(Name(String.concat commandTail)::(t))::(ts),bindStack)

                               else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
            else if commandHead = "add\n"
                 then (*add*)
                      case s of [] =>
                                     interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)


                      | t::ts =>
                                    case t of [] =>(*if the stack is empty*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    | b::[] => (*if the stack only has 1 item*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    | Int(b)::Int(b1)::bs => interpret(xs, (Int(b + b1)::(bs))::(ts),bindStack)
                                    | Name(b)::Int(b1)::bs => if returnType(b,getHead(bindStack)) = "INT"
                                                              then (*do something if name's value is a int*)
                                                              interpret(xs,(Int(b1 + valOf(Int.fromString (returnValue(b,getHead(bindStack)))))::(bs))::(ts),bindStack)
                                                              else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    |Int(b)::Name(b1)::bs => if returnType(b1,getHead(bindStack)) = "INT"
                                                             then
                                                             interpret(xs,(Int(b + valOf( Int.fromString( returnValue(b1 , getHead(bindStack)))))::(bs))::(ts),bindStack)
                                                             else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    |Name(b)::Name(b1)::bs => if returnType(b,getHead(bindStack)) = "INT" andalso returnType(b1,getHead(bindStack)) = "INT"
                                                             then
                                                             interpret(xs, (Int( valOf(Int.fromString (returnValue(b,getHead(bindStack)))) + valOf( Int.fromString( returnValue(b1 , getHead(bindStack)))))::(bs))::(ts), bindStack)
                                                             else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                    | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                 else if commandHead = "sub\n"
                      then (*minus*)
                      case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)

                      | t::ts =>
                                    case t of [] =>(*if the stack is empty*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    | b::[] => (*if the stack only has 1 item*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    | Int(b)::Int(b1)::bs => interpret(xs, (Int(b1 - b)::bs)::(ts),bindStack)
                                    | Name(b)::Int(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT"
                                                              then (*do something if name's value is a int*)
                                                              interpret(xs,(Int(b1 - valOf(Int.fromString (returnValue(b,getHead(bindStack)))))::(bs))::(ts),bindStack)
                                                              else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    |Int(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "INT"
                                                             then
                                                             interpret(xs,(Int(valOf( Int.fromString( returnValue(b1 , getHead(bindStack)))) - b)::(bs))::(ts),bindStack)
                                                             else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                    |Name(b)::Name(b1)::bs => if    returnType(b,getHead(bindStack)) = "INT" andalso returnType(b1,getHead(bindStack)) = "INT"
                                                             then
                                                             interpret(xs, (Int( valOf(Int.fromString (returnValue(b1,getHead(bindStack)))) - valOf( Int.fromString( returnValue(b , getHead(bindStack)))))::(bs))::(ts), bindStack)
                                                             else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                    | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)


                      else if commandHead = "mul\n"
                           then (*multiply*)
                           case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)

                           |t::ts =>
                                         case t of [] =>(*if the stack is empty*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                         | b::[] => (*if the stack only has 1 item*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                         | Int(b)::Int(b1)::bs => interpret(xs, (Int(b * b1)::(bs))::(ts),bindStack)
                                         | Name(b)::Int(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT"
                                                                   then (*do something if name's value is a int*)
                                                                   interpret(xs,(Int(b1 * valOf(Int.fromString (returnValue(b,getHead(bindStack)))))::(bs))::(ts),bindStack)
                                                                   else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                         |Int(b)::Name(b1)::bs => if   returnType(b1,getHead(bindStack)) = "INT"
                                                                  then
                                                                  interpret(xs,(Int(b * valOf( Int.fromString( returnValue(b1 , getHead(bindStack)))))::(bs ))::(ts),bindStack)
                                                                  else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                         |Name(b)::Name(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT" andalso returnType(b1,getHead(bindStack)) = "INT"
                                                                  then
                                                                  interpret(xs, (Int( valOf(Int.fromString (returnValue(b,getHead(bindStack)))) * valOf( Int.fromString( returnValue(b1 , getHead(bindStack)))))::(bs))::(ts), bindStack)
                                                                  else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                         | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                           else if commandHead = "div\n"
                                then (*divide*)
                                case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                |t::ts =>
                                                case t of [] =>(*if the stack is empty*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                | b::[] => (*if the stack only has 1 item*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                | Int(b)::Int(b1)::bs =>
                                                if b = 0
                                                then interpret(xs,((Err(":error:\n")::(t))::(ts)),bindStack)
                                                else interpret(xs,(Int(b1 div b)::(bs))::(ts),bindStack)
                                                | Name(b)::Int(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT" andalso valOf(Int.fromString (returnValue(b,getHead(bindStack)))) <> 0
                                                                          then (*do something if name's value is a int*)
                                                                          interpret(xs,(Int(b1 div valOf(Int.fromString (returnValue(b,getHead(bindStack)))))::(bs))::(ts),bindStack)
                                                                          else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                |Int(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "INT" andalso b <> 0
                                                                         then
                                                                         interpret(xs,(Int(valOf( Int.fromString( returnValue(b1 , getHead(bindStack)))) div b)::(bs))::(ts),bindStack)
                                                                         else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                |Name(b)::Name(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT" andalso returnType(b1,getHead(bindStack)) = "INT" andalso valOf(Int.fromString (returnValue(b,getHead(bindStack)))) <> 0
                                                                         then
                                                                         interpret(xs, (Int( valOf(Int.fromString (returnValue(b1,getHead(bindStack)))) div valOf( Int.fromString( returnValue(b , getHead(bindStack)))))::(bs))::(ts), bindStack)
                                                                         else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                else if commandHead = "rem\n"
                                     then (*remainder*)
                                     case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                     |t::ts =>
                                                   case t of [] =>(*if the stack is empty*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                   | b::[] => (*if the stack only has 1 item*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                   | Int(b)::Int(b1)::bs =>
                                                   if b = 0
                                                   then interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                   else interpret(xs, (Int(b1 mod b)::(bs))::(ts),bindStack)
                                                   | Name(b)::Int(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT" andalso valOf(Int.fromString (returnValue(b,getHead(bindStack)))) <> 0
                                                                             then (*do something if name's value is a int*)
                                                                             interpret(xs,(Int(b1 mod valOf(Int.fromString (returnValue(b,getHead(bindStack)))))::(bs))::(ts),bindStack)
                                                                             else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                   |Int(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "INT" andalso b <> 0
                                                                            then
                                                                            interpret(xs,(Int(valOf( Int.fromString( returnValue(b1 , getHead(bindStack)))) mod b)::(bs))::(ts),bindStack)
                                                                            else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                   |Name(b)::Name(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT" andalso returnType(b1,getHead(bindStack)) = "INT" andalso valOf(Int.fromString (returnValue(b,getHead(bindStack)))) <> 0
                                                                            then
                                                                            interpret(xs, (Int( valOf(Int.fromString (returnValue(b1,getHead(bindStack)))) mod valOf( Int.fromString( returnValue(b , getHead(bindStack)))))::(bs))::(ts), bindStack)
                                                                            else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                   | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                     else if commandHead = "neg\n"
                                          then (*negative*)
                                          case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                          |t::ts =>
                                                        case t of [] =>(*if the stack is empty*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                        | Int(b)::bs => interpret(xs,(Int(~b)::(bs))::(ts),bindStack)
                                                        | Name(b)::bs => if  returnType(b,getHead(bindStack)) = "INT"
                                                                         then interpret(xs,(Int((valOf(Int.fromString (returnValue(b,getHead(bindStack)))) * ~1))::(bs))::(ts),bindStack)
                                                                         else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                        | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)


                                          else if commandHead = "swap\n"
                                               then (*swap*)
                                               case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                               |t::ts =>

                                                             case t of [] =>(*if the stack is empty*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                             | b::[] => (*if the stack only has 1 item*) interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                             | b::b1::bs => interpret(xs, (b1::b::(bs))::(ts),bindStack)
                                           else if commandHead = "pop\n"
                                                 then
                                                 case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                 |t::ts =>

                                                               case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                               | b::bs => interpret(xs, (bs)::ts,bindStack)
                                                 else if commandHead = ":true:\n" (*true*)
                                                      then
                                                      case s of [] =>
                                                                    interpret(xs,(Boolean(commandHead)::[])::([]),bindStack)
                                                      |t :: ts =>
                                                                 interpret(xs,(Boolean(commandHead)::t)::(ts),bindStack)

                                                      else if commandHead = ":false:\n" (*false*)
                                                           then
                                                           case s of [] =>
                                                                         interpret(xs,(Boolean(commandHead)::([]))::([]),bindStack)
                                                          |t :: ts =>
                                                                     interpret(xs,(Boolean(commandHead)::(t))::(ts),bindStack)


                                                           else if commandHead = ":error:\n" (*error*)
                                                           then
                                                                case s of [] =>
                                                                                 interpret(xs,(Err(commandHead)::([]))::([]),bindStack)
                                                                | t :: ts =>
                                                                               interpret(xs,(Err(commandHead)::(t))::(ts),bindStack)
                                                                else if commandHead = "and\n"
                                                                     then
                                                                     case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                                     |t::ts =>
                                                                                   case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)(*AND*)
                                                                                   | b :: [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                   | Boolean(b)::Boolean(b1)::bs =>  if b = ":true:\n" andalso b1 = ":true:\n"
                                                                                                                     then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                     else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                   | Name(b)::Boolean(b1)::bs => if  returnType(b,getHead(bindStack)) = "BOOL"
                                                                                                                 then if returnValue(b,getHead(bindStack)) = ":true:\n" andalso b1 = ":true:\n"
                                                                                                                      then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                      else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                 else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                   | Boolean(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "BOOL"
                                                                                                                 then if returnValue(b1,getHead(bindStack)) = ":true:\n" andalso b = ":true:\n"
                                                                                                                      then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                      else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                 else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                   | Name(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "BOOL"  andalso returnType(b,getHead(bindStack)) = "BOOL"
                                                                                                              then if returnValue(b1,getHead(bindStack)) = ":true:\n" andalso returnValue(b,getHead(bindStack)) = ":true:\n"
                                                                                                                   then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                   else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                              else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                   | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                     else if commandHead = "or\n"
                                                                          then
                                                                          case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                                          |t::ts =>

                                                                                        case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)(*OR*)
                                                                                        | b :: [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                        | Boolean(b)::Boolean(b1)::bs => if b = ":true:\n" orelse b1 = ":true:\n"
                                                                                                                         then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                         else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                        | Name(b)::Boolean(b1)::bs => if  returnType(b,getHead(bindStack)) = "BOOL"
                                                                                                                      then if returnValue(b,getHead(bindStack)) = ":true:\n" orelse b1 = ":true:\n"
                                                                                                                           then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                           else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                      else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                        | Boolean(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "BOOL"
                                                                                                                      then if returnValue(b1,getHead(bindStack)) = ":true:\n" orelse b = ":true:\n"
                                                                                                                           then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                           else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                      else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                        | Name(b)::Name(b1)::bs => if returnType(b1,getHead(bindStack)) = "BOOL" andalso returnType(b,getHead(bindStack)) = "BOOL"
                                                                                                                   then if returnValue(b1,getHead(bindStack)) = ":true:\n" orelse returnValue(b,getHead(bindStack)) = ":true:\n"
                                                                                                                        then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                        else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                   else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                        | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                          else if commandHead = "not\n"
                                                                               then
                                                                               case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                                               |t::ts =>
                                                                                             case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)(*NOT*)
                                                                                             |Boolean(b)::bs => if b = ":true:"
                                                                                                                then interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                else interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                             |Name(b)::bs => if  returnType(b,getHead(bindStack)) = "BOOL"
                                                                                                             then if returnValue(b,getHead(bindStack)) = ":false:"
                                                                                                                  then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                  else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                             else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                             | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                               else if commandHead = "equal\n"
                                                                                    then
                                                                                    case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                                                    |t::ts =>
                                                                                                  case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)(*equal*)
                                                                                                  |b :: [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                  |Int(b)::Int(b1)::bs => if b = b1
                                                                                                                          then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                          else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                  |Name(b)::Int(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT"
                                                                                                                          then if valOf(Int.fromString (returnValue(b,getHead(bindStack)))) = b1
                                                                                                                               then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                               else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                          else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                  |Int(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "INT"
                                                                                                                           then if valOf(Int.fromString (returnValue(b1,getHead(bindStack)))) = b
                                                                                                                                then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                                else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                           else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                  |Name(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "INT" andalso returnType(b,getHead(bindStack)) = "INT"
                                                                                                                           then if valOf(Int.fromString (returnValue(b1,getHead(bindStack)))) = valOf(Int.fromString (returnValue(b,getHead(bindStack))))
                                                                                                                                then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                                else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                           else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                                                  | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                                    else if commandHead = "lessThan\n"
                                                                                         then
                                                                                         case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                                                         |t::ts =>

                                                                                                       case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                       | b:: [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                       |Int(b)::Int(b1)::bs => if b1 < b
                                                                                                                               then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                               else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                       |Name(b)::Int(b1)::bs => if  returnType(b,getHead(bindStack)) = "INT"
                                                                                                                                then if valOf(Int.fromString (returnValue(b,getHead(bindStack)))) > b1
                                                                                                                                     then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                                     else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                                else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                       |Int(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "INT"
                                                                                                                                then if valOf(Int.fromString (returnValue(b1,getHead(bindStack)))) < b
                                                                                                                                     then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                                     else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                                else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                       |Name(b)::Name(b1)::bs => if  returnType(b1,getHead(bindStack)) = "INT" andalso returnType(b,getHead(bindStack)) = "INT"
                                                                                                                                 then if valOf(Int.fromString (returnValue(b1,getHead(bindStack)))) < valOf(Int.fromString (returnValue(b,getHead(bindStack))))
                                                                                                                                      then interpret(xs,(Boolean(":true:\n")::(bs))::(ts),bindStack)
                                                                                                                                      else interpret(xs,(Boolean(":false:\n")::(bs))::(ts),bindStack)
                                                                                                                                 else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                                                       | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                                         else if commandHead = "bind\n"(*bind*)
                                                                                              then
                                                                                              case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                                                              |t::ts =>
                                                                                                      case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                       | b::[] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                       |Str(Value)::Name(bindName)::bs =>  interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack,(bindName,Str(Value),"STRING")))
                                                                                                       |Int(Value)::Name(bindName)::bs => interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack,(bindName ,Int(Value),"INT")))
                                                                                                       |Boolean(Value)::Name(bindName)::bs => interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack,(bindName ,Boolean(Value),"BOOL")))
                                                                                                       |Unit(Value)::Name(bindName)::bs => interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack ,(bindName ,Unit(Value), "UNIT")))
                                                                                                       |Name(Value)::Name(bindName)::bs =>   if returnType(Value,getHead(bindStack)) = "INT"
                                                                                                                                             then interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack,(bindName ,Int(valOf(Int.fromString(returnValue(Value,getHead(bindStack))))),"INT")))(*EDIT THIS ONE*)
                                                                                                                                             else if returnType(Value,getHead(bindStack)) = "STRING"
                                                                                                                                                  then interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack,(bindName ,Str(returnValue(Value,getHead(bindStack))),"STRING")))(*EDIT THIS ONE*)
                                                                                                                                                  else if returnType(Value,getHead(bindStack)) = "BOOL"
                                                                                                                                                       then interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack,(bindName ,Boolean(returnValue(Value,getHead(bindStack))),"BOOL")))(*EDIT THIS ONE*)
                                                                                                                                                       else if returnType(Value,getHead(bindStack)) = "UNIT"
                                                                                                                                                            then interpret(xs,(Unit(":unit:\n")::(bs))::(ts),addtoBind(bindStack,(bindName ,Unit(returnValue(Value,getHead(bindStack))),"UNIT")))(*EDIT THIS ONE*)
                                                                                                                                                            else interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                                                      | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                              else if commandHead = "if\n"
                                                                                                   then
                                                                                                   case s of [] => interpret(xs,(Err(":error:\n")::([]))::([]),bindStack)
                                                                                                   |t::ts =>
                                                                                                                 case t of [] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                                 | b::[] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                                 | b::b1::[] => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)
                                                                                                                 | b::b1::Boolean(b2)::bs => if b2 = ":true:\n"
                                                                                                                                      then interpret(xs,(b::(bs))::(ts),bindStack)
                                                                                                                                      else interpret(xs,(b1::(bs))::(ts),bindStack)
                                                                                                                 | b::b1::Name(b2)::bs => if returnValue(b2, getHead(bindStack)) = ":true:\n"
                                                                                                                                      then interpret(xs,(b::(bs))::(ts),bindStack)
                                                                                                                                      else interpret(xs,(b1::(bs))::(ts),bindStack)
                                                                                                                 | _ => interpret(xs,(Err(":error:\n")::(t))::(ts),bindStack)

                                                                                                   else if commandHead = "let\n"
                                                                                                        then
                                                                                                        case s of [] => interpret(xs,[]::[]::s,(getHead(bindStack))::[]::bindStack)
                                                                                                        |p::ps => interpret(xs,[]::s,(getHead(bindStack))::bindStack)
                                                                                                  else if commandHead = "end\n"
                                                                                                      then
                                                                                                      case s of [] => interpret(xs,s,bindStack)
                                                                                                      |n::[] => interpret(xs,s,bindStack)
                                                                                                      |n::n1::ns => interpret(xs,(getLast(n)::n1)::ns,getTail(bindStack))

                                                                                                      else if isFun(commandHead::commandTail) (*fun declare*)
                                                                                                           then
                                                                                                           case s of [] => interpret(SkipInstruction(xs) , (Unit(":unit:\n")::[])::[] , addtoBind(bindStack,(Front(commandTail) ^ "\n",Function(makeInstruction(xs,[]),[],getHead(bindStack),List.nth(spaceSep,2)),"FUNC")))
                                                                                                           |m::ms => interpret(SkipInstruction(xs),(Unit(":unit:\n")::m)::ms,addtoBind(bindStack,(Front(commandTail) ^ "\n",Function(makeInstruction(xs,[]),[],getHead(bindStack),List.nth(spaceSep,2)),"FUNC")))
                                                                                                           else if commandHead = "funEnd\n"
                                                                                                                then
                                                                                                                case s of [] => interpret(xs,s,bindStack)
                                                                                                                |m::[] => interpret(xs,s,bindStack)
                                                                                                                |m::ms => interpret(xs,ms,getTail(bindStack))
                                                                                                                else if commandHead = "call\n"
                                                                                                                     then
                                                                                                                     case getHead(s) of
                                                                                                                     Name(b)::b1::bs =>    (case returnType(b,getHead(bindStack)) of "FUNC" =>
                                                                                                                                                                                      (case b1 of Str(c) => interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, []::(bs::getTail(s)),(*STACK*)getBind(getFun(b,getHead(bindStack)),Str(c),"STRING"))
                                                                                                                                                                                      |Int(c) => interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, []::(bs::getTail(s)),(*STACK*)getBind(getFun(b,getHead(bindStack)),Int(c),"INT"))
                                                                                                                                                                                      |Boolean(c) => interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, []::(bs::getTail(s)),(*STACK*)getBind(getFun(b,getHead(bindStack)),Boolean(c),"BOOL"))
                                                                                                                                                                                      |Unit(c) => interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, []::(bs::getTail(s)),(*STACK*)getBind(getFun(b,getHead(bindStack)),Unit(c),"UNIT"))
                                                                                                                                                                                      |Name(c) => if returnType(b,getHead(bindStack)) = "INT"
                                                                                                                                                                                                  then interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, [] ::(bs::getTail(s)),(*STACK*) getBind(getFun(b,getHead(bindStack)),getFun(c,getHead(bindStack)),"INT"))
                                                                                                                                                                                                  else if returnType(b,getHead(bindStack)) = "BOOL"
                                                                                                                                                                                                       then interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, [] ::(bs::getTail(s)),(*STACK*) getBind(getFun(b,getHead(bindStack)),getFun(c,getHead(bindStack)),"BOOL"))
                                                                                                                                                                                                       else if returnType(b,getHead(bindStack)) = "STRING"
                                                                                                                                                                                                            then interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, [] ::(bs::getTail(s)),(*STACK*) getBind(getFun(b,getHead(bindStack)),getFun(c,getHead(bindStack)),"STRING"))
                                                                                                                                                                                                            else if returnType(b,getHead(bindStack)) = "UNIT"
                                                                                                                                                                                                                 then interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, [] ::(bs::getTail(s)),(*STACK*) getBind(getFun(b,getHead(bindStack)),getFun(c,getHead(bindStack)),"UNIT"))
                                                                                                                                                                                                                 else if returnType(b,getHead(bindStack)) = "FUNC"
                                                                                                                                                                                                                      then interpret((*INSTRUCTIONS*)List.rev (getInstruction(getFun(b,getHead(bindStack)))) @ xs, [] ::(bs::getTail(s)),(*STACK*) getBind(getFun(b,getHead(bindStack)),getFun(c,getHead(bindStack)),"FUNC"))
                                                                                                                                                                                                                      else (print("SDLFKJ");interpret(xs,(Err(":error:\n")::getHead(s))::getTail(s),bindStack)))

                                                                                                                                          |_ => (print("123123");interpret(xs,(Err(":error:\n")::getHead(s))::getTail(s),bindStack)))
                                                                                                                     | _ => (print("DSLFKJ");interpret(xs,(Err(":error:\n")::getHead(s))::getTail(s),bindStack))
                                                                                                                     else if commandHead = "return\n"
                                                                                                                          then
                                                                                                                          case s of [] => interpret(xs,s,bindStack)
                                                                                                                          |n::[] => interpret(xs,s,bindStack)
                                                                                                                          |n::n1::ns =>
                                                                                                                                      (case getLast(n) of Name(c) => interpret(xs,(getFun(c,getHead(bindStack))::n1)::ns,getTail(bindStack))
                                                                                                                                      | _ => interpret(xs,(getLast(n)::n1)::ns,getTail(bindStack)))

                                                                                                                         else if isIO(commandHead::commandTail)
                                                                                                                              then
                                                                                                                              case s of [] => (print("DFLKSJ" ^ "\n");interpret(SkipInstruction(xs) , (Unit(":unit:\n")::[])::[] , addtoBind(bindStack,(Front(commandTail) ^ "\n",IOfun(makeIOnstruction(xs,[]),[],getHead(bindStack),List.nth(spaceSep,2)),"FUNC"))))
                                                                                                                              |m::ms => (print(Front(commandTail) ^ "\n");interpret(SkipInstruction(xs),(Unit(":unit:\n")::m)::ms,addtoBind(bindStack,(Front(commandTail) ^ "\n",IOfun(makeIOnstruction(xs,[]),[],getHead(bindStack),List.nth(spaceSep,2)),"FUNC"))))

                                                                                                                              else if commandHead = "returnIO\n"
                                                                                                                                   then
                                                                                                                                   case s of [] => interpret(xs,s,bindStack)
                                                                                                                                   |n ::[] => interpret(xs,s,bindStack)
                                                                                                                                   |n::n1::ns =>
                                                                                                                                                (case getLast(n) of Name(c) => interpret(xs,(getFun(c,getHead(bindStack))::n1)::ns,addtoBind(getTail(bindStack),(List.last (getHead(bindStack)))))
                                                                                                                                                | _ => interpret(xs,(getLast(n)::n1)::ns,addtoBind(getTail(bindStack),(List.last (getHead(bindStack)))))
                                                                                                                                                )





                                                                                               else interpret(xs,s,bindStack)

                                                                                              (*I have to make it take arguments my function*)





          end

      fun writefile (s : token list, outs) =
      let
          fun negate i =
              if i < 0 then "-" ^ Int.toString (~i) else Int.toString i
      in
          case s of [] => TextIO.closeOut outs
          | x::xs => case x of Str(c) => (TextIO.output(outs , c); writefile(xs , outs))
                        | Int(c) => (TextIO.output(outs , negate c ^ "\n"); writefile(xs , outs))
                        | Name(c) => (TextIO.output(outs , c); writefile(xs , outs))
                        | Boolean(c) => (TextIO.output(outs , c); writefile(xs , outs))
                        | Err(c) => (TextIO.output(outs , c); writefile(xs,outs))
                        | Unit(c) => (TextIO.output(outs , c); writefile(xs,outs))
                        | _ => writefile(xs,outs)
      end




  in


     writefile(hd (interpret (fileread ins, [],[])),outs)



  end
  val _ = interpreter("input1.txt","output1.txt")

(*val _ = interpreter("input_1.txt","output1.txt")*)
val _ = interpreter("input_2.txt","output2.txt")
val _ = interpreter("input_3.txt","output3.txt")
val _ = interpreter("input_4.txt","output4.txt")
val _ = interpreter("input_5.txt","output5.txt")
val _ = interpreter("input_6.txt","output6.txt")
val _ = interpreter("input_7.txt","output7.txt")
val _ = interpreter("input_8.txt","output8.txt")
val _ = interpreter("input_9.txt","output9.txt")
val _ = interpreter("input_10.txt","output10.txt")
val _ = interpreter("input_11.txt", "output11.txt")
