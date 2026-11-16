(*
   Student Name: Duc Tuan "Terry" Nguyen
   Project 2: Simple ML Operations
   This program implements the eight required functions for the assignment,
   tests them, and prints a neat driver summary.

   Function descriptions:
   1. replaceAll replaces every occurrence of one value with two copies of another.
   2. removeDuplicates removes duplicate elements while preserving order.
   3. circulate and circulateDir rotate a list left or right by one position.
   4. largestCount returns the largest integer and the list size.
   5. int2String converts a positive or negative integer into a string.
   6. string2Int converts a numeric string into an integer, including negatives.
   7. removeLastChar removes the final character from a string.
   8. readFile and displayFileSummary read numeric data from a file and print a summary.
*)

(* Replace each matching value with two copies of its replacement. *)
fun replaceAll ([], _, _) = []
  | replaceAll (item :: remainingItems, oldValue, newValue) =
      if item = oldValue
      then newValue :: newValue :: replaceAll (remainingItems, oldValue, newValue)
      else item :: replaceAll (remainingItems, oldValue, newValue);

(* Keep the first occurrence of each value and skip later occurrences. *)
fun removeDuplicates inputList =
    let
        fun contains (_, []) = false
          | contains (searchedItem, firstItem :: remainingItems) =
                if searchedItem = firstItem
                then true
                else contains (searchedItem, remainingItems)

        fun build ([], _) = []
          | build (item :: remainingItems, seenItems) =
                if contains (item, seenItems)
                then build (remainingItems, seenItems)
                else item :: build (remainingItems, item :: seenItems)
    in
        build (inputList, [])
    end;

(* Move the first list element to the end. *)
fun circulate [] = []
  | circulate (firstItem :: remainingItems) =
        let
            fun append ([], secondList) = secondList
              | append (firstValue :: remainingValues, secondList) =
                    firstValue :: append (remainingValues, secondList)
        in
            append (remainingItems, [firstItem])
        end;

(* Rotate left or right by one element; unknown directions leave the list unchanged. *)
fun circulateDir ([], _) = []
  | circulateDir (inputList, direction) =
        let
            fun leftRotate [] = []
              | leftRotate (firstItem :: remainingItems) =
                    remainingItems @ [firstItem]

            fun rightRotate [] = []
              | rightRotate inputValues =
                    let
                        val reversedValues = rev inputValues
                    in
                        (* Reversing puts the last element first; reverse the remainder back. *)
                        hd reversedValues :: rev (tl reversedValues)
                    end
        in
            if direction = "left"
            then leftRotate inputList
            else if direction = "right"
            then rightRotate inputList
            else inputList
        end;

(* Return the largest value and size of a non-empty integer list. *)
fun largestCount [] = raise Fail "largestCount: empty list"
  | largestCount (firstValue :: remainingValues) =
        let
            fun countValues [] = 0
              | countValues (_ :: remainingValues) = 1 + countValues remainingValues

            fun listMax currentMaximum [] = currentMaximum
              | listMax currentMaximum (nextValue :: valuesAfterNext) =
                    if nextValue > currentMaximum
                    then listMax nextValue valuesAfterNext
                    else listMax currentMaximum valuesAfterNext
        in
            (listMax firstValue remainingValues,
             countValues (firstValue :: remainingValues))
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
          | dropLast (firstCharacter :: remainingCharacters) =
                firstCharacter :: dropLast remainingCharacters
    in
        String.implode (dropLast (String.explode inputString))
    end;

(* Part 8: Read positive integers from a file and summarize the list *)
fun listSize [] = 0
  | listSize (_ :: remainingValues) = 1 + listSize remainingValues;

fun largestNumber [] = raise Fail "largestNumber: empty list"
  | largestNumber [onlyValue] = onlyValue
  | largestNumber (firstValue :: remainingValues) =
        let
            val largestOfTail = largestNumber remainingValues
        in
            if firstValue > largestOfTail then firstValue else largestOfTail
        end;

fun averageOfList [] = 0.0
  | averageOfList listOfNumbers =
        let
            fun sumList [] = 0
              | sumList (nextNumber :: remainingNumbers) =
                    nextNumber + sumList remainingNumbers

            val total = sumList listOfNumbers
            val count = Real.fromInt (listSize listOfNumbers)
        in
            Real.fromInt total / count
        end;

fun trimLine lineText =
    let
        val lineLength = String.size lineText
        val withoutNewline =
            if lineLength > 0 andalso
               String.sub (lineText, lineLength - 1) = #"\n"
            then removeLastChar lineText
            else lineText
        val trimmedLength = String.size withoutNewline
    in
        (* Strip CR after LF so files with Windows line endings are handled too. *)
        if trimmedLength > 0 andalso
           String.sub (withoutNewline, trimmedLength - 1) = #"\r"
        then removeLastChar withoutNewline
        else withoutNewline
    end;

fun readFile fileName =
    let
        val inputStream = TextIO.openIn fileName

        fun readLines reversedNumbers =
            case TextIO.inputLine inputStream of
                NONE => rev reversedNumbers
              | SOME lineText =>
                    let
                        val cleanedLine = trimLine lineText
                    in
                        if cleanedLine = ""
                        then readLines reversedNumbers
                        else readLines (string2Int cleanedLine :: reversedNumbers)
                    end

        (* Reverse once at the end to retain the file's line order. *)
        val fileNumbers = readLines []
        val _ = TextIO.closeIn inputStream
    in
        fileNumbers
    end;

fun promptForFilename () =
    let
        val _ = print "Enter a file name: "
        val _ = TextIO.flushOut TextIO.stdOut
        val inputLine = TextIO.inputLine TextIO.stdIn
    in
        case inputLine of
            NONE => raise Fail "No file name was entered."
          | SOME fileNameText => trimLine fileNameText
    end;

fun displayFileSummary fileName =
    let
        val fileNumbers = readFile fileName
        val numberCount = listSize fileNumbers
        val _ = print ("\nFile summary for " ^ fileName ^ "\n")
        val _ = print ("List size: " ^ int2String numberCount ^ "\n")
        val _ =
            case fileNumbers of
                [] => print "Largest number: none (empty file)\n"
              | _ => print ("Largest number is: " ^
                     int2String (largestNumber fileNumbers) ^ "\n")
        val averageText =
            case fileNumbers of
                [] => "0.00"
              | _ => Real.toString (averageOfList fileNumbers)
        val _ = print ("Average: " ^ averageText ^ "\n")
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
- part 3, unexpected data handling? If second param not "left" or "right" Test case will always hanldle left right
- int to string only expected input? Only numbers as chars? Negative numbers?
- int to string only positive numbers?
- name of function 8? Io something
- Can we ignore warnings? Ie nonexhautive match. Do not ignore warnings for non exhaustive 
- Is part 8 file name input keyboard or text input? If text input, how to handle file not found? Keyboard input

- Helper function for negative nmbers in 6

*)
