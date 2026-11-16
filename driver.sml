(* Part 1: Replace each occurrence of oldValue with two copies of newValue *)
fun replaceAll ([], _, _) = []
 |  replaceAll (item :: rest, oldValue, newValue) =
        if item = oldValue
        then newValue :: newValue :: replaceAll (rest, oldValue, newValue)
        else item :: replaceAll (rest, oldValue, newValue)

(* Part 2: Write a function named 'removeDuplicates' that removes all the duplicate elements from a list *)
fun removeDuplicates [] = []
 |  removeDuplicates [x] = [x]              (* If list contains single element *)
 |  removeDuplicates list = 
        let 
            fun member (x, []) = false      (* If member list is empty *)
             |  member (x, y::ys) = 
                if (x = y)
                then true
                else member (x, ys)         (* Recursively checks element with rest of list *)

            fun checker ([], seenList) = [] (* If empty list, return empty list *)
             |  checker (x::xs, seenList) =
                if (member (x, seenList))     (* If element has been seen then skip that element *)
                then checker (xs, seenList)
                else x :: checker (xs, x::seenList)     (* Appends element if not seen in checker *)
        in
            checker (list, [])
        end;

(* Part 3: Circulate List in Both Directions *)

fun circulate [] = []
 |  circulate (x::xs) = 
        let 
            fun append ([], ys) = ys
             | append (a::rest, ys) = a::append (rest, ys)
        in
            append (xs, [x])
        end;

fun circulateDir ([], _) = []
 |  circulateDir (list, leftOrRight) =
        let
            fun left [] = []
             |  left (x::xs) = xs @ [x]
            fun right [] = []
             |  right (xs) =            (* Reverses to get the last element and prepends to reversed tail *)
                    let                 (* The above operation is equivalent to a right list rotation *)
                        val reversed = rev xs
                    in 
                        hd reversed :: rev (tl reversed)
                    end;
        in
            if (leftOrRight = "left")
            then left list
            else if (leftOrRight = "right")
            then right list
            else
            list
        end;

(* Part 4: Function that takes a non-empty list of integers and returns largest value and size of list *)

fun largestCount (list) = 
    let
        fun size [] = 0
        |   size (list) = 
            1 + size (tl list)
        fun listMax [x] = x
         |  listMax (x::xs) = 
            let 
                val maxTail = listMax (xs) 
            in 
                if x > maxTail 
                then x 
                else maxTail 
            end;
    in
        (listMax(list), size(list))
    end;

(* Part 5: Function that converts an integer to a string *)
fun int2String n =
    let
        fun digitToChar digit = chr (ord #"0" + digit)
        fun digits 0 acc = rev (#"0" :: acc)
          | digits value acc =
                digits (value div 10) (digitToChar (abs (value mod 10)) :: acc)
    in
        if n < 0 then
            String.implode (#"~" :: digits (~n) [])
        else
            String.implode (digits n [])
    end;

(* Part 6: Function that converts a string to an integer *)
fun string2Int str =
    let
        val chars = String.explode str
        val negative =
            case chars of
                #"~" :: _ => true
              | _ => false
        val digits =
            if negative then tl chars else chars
        fun parse [] = 0
          | parse (c :: rest) =
                (ord c - ord #"0") + 10 * parse rest
    in
        if negative then ~ (parse digits) else parse digits
    end;
        
(* Part 7: Function to removeLastChar from a string *)

fun removeLastChar s =
    let
        fun dropLast [] = []
          | dropLast [_] = []      
          | dropLast (c::cs) = c :: dropLast cs
    in
        String.implode (dropLast (String.explode s))
    end;

(* Part 8: Function that handles data input and processing from file *)

fun listSize [] = 0
  | listSize (_ :: xs) = 1 + listSize xs

fun largestNumber [x] = x
  | largestNumber (x :: xs) =
      let
          val tailLargest = largestNumber xs
      in
          if x > tailLargest then x else tailLargest
      end

fun averageOfList [] = 0.0
  | averageOfList numbers =
      let
          val total = foldl (op +) 0 numbers
          val count = Real.fromInt (listSize numbers)
      in
          Real.fromInt total / count
      end

fun readFile filename =
    let
        val inStream = TextIO.openIn filename
        fun loop acc =
            case TextIO.inputLine inStream of
                NONE => rev acc
              | SOME line =>
                    let
                        val trimmed =
                            if String.size line > 0 andalso String.sub (line, String.size line - 1) = #"\n"
                            then removeLastChar line
                            else line
                        val finalTrimmed =
                            if String.size trimmed > 0 andalso String.sub (trimmed, String.size trimmed - 1) = #"\r"
                            then removeLastChar trimmed
                            else trimmed
                    in
                        if finalTrimmed = ""
                        then loop acc
                        else loop (string2Int finalTrimmed :: acc)
                    end
        val numbers = loop []
        val _ = TextIO.closeIn inStream
    in
        numbers
    end

fun displayFileSummary filename =
    let
        val numbers = readFile filename
        val size = listSize numbers
        val largest = largestNumber numbers
        val avg = averageOfList numbers
        val _ = print ("Enter a file name: " ^ filename ^ "\n")
        val _ = print ("List size: " ^ int2String size ^ "\n")
        val _ = print ("Largest number is: " ^ int2String largest ^ "\n")
        val _ = print ("Average: " ^ Real.toString avg ^ "\n")
    in
        ()
    end

(* Spec-based test cases: each function is invoked in the same order as the project specification. *)
val testReplaceAll1 = replaceAll ([1, 2, 5, 2, 10], 2, 100)
val testReplaceAll2 = replaceAll (["the", "to", "you", "the"], "the", "hi")

val testRemoveDuplicates1 = removeDuplicates ([1, 2, 3, 2, 4, 1, 5, 3])

val testCirculateLeft = circulateDir ([1, 2, 3, 4], "left")
val testCirculateRight = circulateDir ([1, 2, 3, 4], "right")

val testLargestCount = largestCount [10, 1, 7, 40, ~5, 20]

val testInt2String1 = int2String 123
val testInt2String2 = int2String ~123

val testString2Int1 = string2Int "123"
val testString2Int2 = string2Int "~123"

val testRemoveLastChar1 = removeLastChar "abc"
val testRemoveLastChar2 = removeLastChar "725"
val testRemoveLastChar3 = removeLastChar ""

val testDisplaySummary = displayFileSummary "data.txt"

(*
Questions to Ask
- Expected handling of data types for each part
- driver + multiple files? How to run.
- part 3, unexpected data handling? If second param not "left" or "right" Test case will always hanldle left right
- int to string only expected input? Only numbers as chars? Negative numbers?
- int to string only positive numbers?
- name of function 8? Io something
- Can we ignore warnings? Ie nonexhautive match. Do not ignore warnings for non exhaustive 
- Is part 8 file name input keyboard or text input? If text input, how to handle file not found? Keyboard input

- Helper function for negative nmbers in 6

*)
