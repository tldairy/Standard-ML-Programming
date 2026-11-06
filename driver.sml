(* Replace each occurrence of oldValue with two copies of newValue. *)
fun replaceAll ([], _, _) = []
  | replaceAll (item :: rest, oldValue, newValue) =
      if item = oldValue
      then newValue :: newValue :: replaceAll (rest, oldValue, newValue)
      else item :: replaceAll (rest, oldValue, newValue)

(* Tests for integer values, strings, and the empty-list case. *)
val replaceAllIntegerTest =
    replaceAll ([1, 2, 5, 2, 10], 2, 100) = [1, 100, 100, 5, 100, 100, 10]

val replaceAllStringTest =
    replaceAll (["the", "to", "you", "the"], "the", "hi") =
    ["hi", "hi", "to", "you", "hi", "hi"]

val replaceAllEmptyTest = replaceAll ([], 2, 100) = []