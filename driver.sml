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

(* Part 5: Function that converts an integer to a string*)
fun int2String (int) = 
    let 

    fun separate ([], 0) = [0]
    |   separate (list, 0) = list      (* Base case *)
    |   separate (list, int) = 
        if (int div 10 = 0)
        then ((int mod 10)::list)
        else
            (int mod 10)::separate(list, int div 10)
    
    fun individualDigitToChar digit = chr (digit + 48)

    fun toChars [] = []
     |  toChars (list) = 
        individualDigitToChar(hd list) :: toChars(tl list)
    in
        implode (rev(toChars(separate([],int))))
    end;

(* Part 6: Function that converts a string to an integer*)
fun string2Int (string) =
    let 
        val exploded = String.explode string;
        fun singleDigit d = ord d - ord #"0"
    fun tens ([], total) = total
     |  tens (head::tail, total) = 
        tens (tail, total * 10 + singleDigit(head))
    in
        tens (exploded, 0)
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

(* Part 1 Unit Tests: Tests for integer values, strings, and the empty-list case. *)
val replaceAllIntegerTest =
    replaceAll ([1, 2, 5, 2, 10], 2, 100)
val replaceAllIntegerTestASSERT =
    replaceAll ([1, 2, 5, 2, 10], 2, 100) = [1, 100, 100, 5, 100, 100, 10]
val replaceAllStringTest =
    replaceAll (["the", "to", "you", "the"], "the", "hi")
val replaceAllStringTestASSERT =
    replaceAll (["the", "to", "you", "the"], "the", "hi") =
    ["hi", "hi", "to", "you", "hi", "hi"]
val replaceAllEmptyTest = replaceAll ([], 2, 100)
val replaceAllEmptyTestASSERT = replaceAll ([], 2, 100) = []

(* Part 2 Unit Tests: Tests for expected input, empty list, reverse order list, and single element list *)
val removeDuplicatesTest = 
    removeDuplicates ([1, 2, 3, 2, 4, 1, 5, 3])

val removeDuplicatesTest2 = 
    removeDuplicates([])

val removeDuplidatesTest3 = 
    removeDuplicates ([5,4,4,3,2,1])

val removeDuplidatesTest4 = 
    removeDuplicates ([5])

(* Part 3 Unit Tests: Test for right and left rotation with strings and int *)
val circulateTest1 = 
    circulate[1,2,3,4]

val circulateTestRight = 
    circulateDir([1,2,3], "right")

val circulateTestLeft = 
    circulateDir([1,2,3], "left")

val circulateTestRightString = 
    circulateDir(["a","b","c"], "right")

val circulateTestLeftString = 
    circulateDir(["a","b","c"], "left")

val circulateTestWrong = 
    circulateDir([1,2,3], "wrong")

val largestCountTest1 = 
    largestCount([1,2,3,4,5,6,7,8])

val largestCountTest2 = 
    largestCount([1,2,3,4,12,5,6,7,8])

val string2IntTest1 = 
    string2Int("123")

val int2StringTest1 = 
    int2String(123456789)

val removeLastCharTest =
    removeLastChar("abc")

val part8Test = displayFileSummary "data.txt"

(*
Questions to Ask
- Expected handling of data types for each part
- driver + multiple files? How to run.
- part 3, unexpected data handling? If second param not "left" or "right"
- int to string only expected input? Only numbers as chars? Negative numbers?
- int to string only positive numbers?
- name of function 8?
- Can we ignore warnings? Ie nonexhautive match.
*)