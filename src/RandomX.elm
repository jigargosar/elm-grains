module RandomX exposing (listOfGeneratorToGeneratorOfList)

import Random exposing (Generator)


listOfGeneratorToGeneratorOfList : List (Generator a) -> Generator (List a)
listOfGeneratorToGeneratorOfList =
    List.foldr (Random.map2 (::)) (Random.constant [])
