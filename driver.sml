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

(* Convert an integer to decimal digits, preserving the SML negative sign. *)
fun int2String integerValue =
    if integerValue = 0 then "0"
    else
        let
            (* Correct SML's floor division for negative values to truncate toward zero. *)
            fun quotientTowardZero currentValue =
                let
                    val quotient = currentValue div 10
                    val remainder = currentValue mod 10
                in
                    if currentValue < 0 andalso remainder <> 0
                    then quotient + 1
                    else quotient
                end

            fun digitValue currentValue =
                let
                    val remainder = currentValue mod 10
                in
                    if currentValue < 0 andalso remainder <> 0
                    then 10 - remainder
                    else remainder
                end

            fun digits 0 = []
              | digits currentValue =
                    (chr (ord #"0" + digitValue currentValue)) ::
                    digits (quotientTowardZero currentValue)

            val signPrefix = if integerValue < 0 then "~" else ""
        in
            signPrefix ^ implode (rev (digits integerValue))
        end;

(* Convert a valid integer string to its numeric value. *)
fun string2Int inputString =
    let
        val characters = String.explode inputString

        fun digitValue character = ord character - ord #"0"

        (* Fold each digit into the value accumulated so far. *)
        fun parseDigits digitCharacters =
            let
                fun accumulateDigits [] currentValue = currentValue
                  | accumulateDigits
                        (character :: remainingCharacters) currentValue =
                        accumulateDigits remainingCharacters
                            (currentValue * 10 + digitValue character)
            in
                accumulateDigits digitCharacters 0
            end

        (* The SML tilde prefix applies a negative sign to the parsed magnitude. *)
        fun parseCharacters [] = 0
          | parseCharacters [#"~"] = 0
          | parseCharacters (#"~" :: digitCharacters) =
                ~ (parseDigits digitCharacters)
          | parseCharacters digitCharacters = parseDigits digitCharacters
    in
        if inputString = "" then 0 else parseCharacters characters
    end;

(* Remove the final character, returning an empty string unchanged. *)
fun removeLastChar inputString =
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
    end;

fun promptDisplayFileSummary () =
    let
        val fileName = promptForFilename ()
    in
        displayFileSummary fileName
    end;

fun checkTest (testDescription, passed) =
    print (testDescription ^ ": " ^ (if passed then "PASS\n" else "FAIL\n"));

(* Tests the examples supplied in the project specification, in assignment order. *)
fun runSpecTests () =
    let
        val _ = print "\nSpecification test cases\n"
        val _ = checkTest ("replaceAll integers",
            replaceAll ([1, 2, 5, 2, 10], 2, 100) =
            [1, 100, 100, 5, 100, 100, 10])
        val _ = checkTest ("replaceAll strings",
            replaceAll (["the", "to", "you", "the"], "the", "hi") =
            ["hi", "hi", "to", "you", "hi", "hi"])
        val _ = checkTest ("removeDuplicates",
            removeDuplicates [1, 2, 3, 2, 4, 1, 5, 3] = [1, 2, 3, 4, 5])
        val _ = checkTest ("circulate",
            circulate [1, 2, 3, 4] = [2, 3, 4, 1])
        val _ = checkTest ("circulateDir left",
            circulateDir ([1, 2, 3, 4], "left") = [2, 3, 4, 1])
        val _ = checkTest ("circulateDir right",
            circulateDir ([1, 2, 3, 4], "right") = [4, 1, 2, 3])
        val _ = checkTest ("largestCount",
            largestCount [10, 1, 7, 40, ~5, 20] = (40, 6))
        val _ = checkTest ("int2String positive", int2String 123 = "123")
        val _ = checkTest ("int2String negative", int2String (~123) = "~123")
        val _ = checkTest ("string2Int positive", string2Int "123" = 123)
        val _ = checkTest ("string2Int negative", string2Int "~123" = ~123)
        val _ = checkTest ("removeLastChar abc", removeLastChar "abc" = "ab")
        val _ = checkTest ("removeLastChar 725", removeLastChar "725" = "72")
        val _ = checkTest ("removeLastChar empty", removeLastChar "" = "")
    in
        ()
    end;

(* Tests edge cases after the specification examples. *)
fun runExtraTests () =
    let
        val _ = print "\nAdditional test cases\n"
        val _ = checkTest ("replaceAll empty list",
            replaceAll ([], 2, 100) = [])
        val _ = checkTest ("removeDuplicates empty list",
            removeDuplicates ([] : int list) = [])
        val _ = checkTest ("removeDuplicates reverse order",
            removeDuplicates [5, 4, 4, 3, 2, 1] = [5, 4, 3, 2, 1])
        val _ = checkTest ("removeDuplicates single element",
            removeDuplicates [5] = [5])
        val _ = checkTest ("circulateDir right",
            circulateDir ([1, 2, 3], "right") = [3, 1, 2])
        val _ = checkTest ("circulateDir left",
            circulateDir ([1, 2, 3], "left") = [2, 3, 1])
        val _ = checkTest ("circulateDir invalid direction",
            circulateDir ([1, 2, 3], "wrong") = [1, 2, 3])
        val _ = checkTest ("largestCount later maximum",
            largestCount [1, 2, 3, 4, 12, 5, 6, 7, 8] = (12, 9))
        val _ = checkTest ("int2String zero", int2String 0 = "0")
        val _ = checkTest ("string2Int zero", string2Int "0" = 0)
        val _ = checkTest ("removeLastChar single character",
            removeLastChar "a" = "")
        val _ = checkTest ("readFile data.txt",
            readFile "data.txt" = [12, 2, 100, 51])
    in
        ()
    end;

(* Part 8: run file processing last, after all list and conversion tests. *)
fun runDriver () =
    let
        val _ = print
            ("COSC 341 - Project 2: Simple ML Operations\n" ^
             "Student: Duc Tuan \"Terry\" Nguyen\n")
        val _ = runSpecTests ()
        val _ = runExtraTests ()
        val _ = promptDisplayFileSummary ()
    in
        ()
    end;

val _ = runDriver ();

(* Sample output for data.txt:
   COSC 341 - Project 2: Simple ML Operations
   Student: Duc Tuan "Terry" Nguyen

   Specification test cases
   replaceAll integers: PASS
   replaceAll strings: PASS
   removeDuplicates: PASS
   circulate: PASS
   circulateDir left: PASS
   circulateDir right: PASS
   largestCount: PASS
   int2String positive: PASS
   int2String negative: PASS
   string2Int positive: PASS
   string2Int negative: PASS
   removeLastChar abc: PASS
   removeLastChar 725: PASS
   removeLastChar empty: PASS

   Additional test cases
   replaceAll empty list: PASS
   removeDuplicates empty list: PASS
   removeDuplicates reverse order: PASS
   removeDuplicates single element: PASS
   circulateDir right: PASS
   circulateDir left: PASS
   circulateDir invalid direction: PASS
   largestCount later maximum: PASS
   int2String zero: PASS
   string2Int zero: PASS
   removeLastChar single character: PASS
   readFile data.txt: PASS
   Enter a file name: data.txt
   File summary for data.txt
   List size: 4
   Largest number is: 100
   Average: 41.25
*)
