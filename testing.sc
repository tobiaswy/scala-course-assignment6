

/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]

def addToSentence(sentence:Sentence, word: Word): Sentence = sentence :+ word

/** `Occurrences` is a `List` of pairs of characters and positive integers saying
  * how often the character appears.
  * This list is sorted alphabetically w.r.t. to the character in each pair.
  * All characters in the occurrence list are lowercase.
  *
  * Any list of pairs of lowercase characters and their frequency which is not sorted
  * is **not** an occurrence list.
  *
  * Note: If the frequency of some character is zero, then that character should not be
  * in the list.
  */
type Occurrences = List[(Char, Int)]

def sortOccurancePairs(pair: (Char, Int), pair2: (Char, Int)): Boolean =
  (pair, pair2) match {
    case ((ltr, _), (ltr2, _)) => ltr < ltr2
  }

/**
  * Function to combine to occurances
  *
  * @param occur1
  * @param occur2
  * @return
  */
def combineOccurrences(occur1: Occurrences, occur2: Occurrences): Occurrences = {
  def reducer(list: List[(Char, Int)]): (Char, Int) = list.reduce((pair: (Char, Int), pair2: (Char, Int)) => (pair._1, pair._2 + pair2._2))

  (((occur1 ++ occur2) groupBy (letter => letter._1)) mapValues[(Char, Int)] (reducer)).values.toList.sortWith(sortOccurancePairs)
  // ^ that makes my head hurt >.<
}


/** The dictionary is simply a sequence of words.
  * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
  */
val dictionary: List[Word] = List("Hello", "World","OMG","FML","I","love", "Scala", "you", "olive")

/** Converts the word into its character occurrence list.
  *
  * Note: the uppercase and lowercase version of the character are treated as the
  * same character, and are represented as a lowercase character in the occurrence list.
  *
  * Note: you must use `groupBy` to implement this method!
  */
def wordOccurrences(w: Word): Occurrences =
  (w.toList groupBy (letter => letter.toLower) transform ((_, sequence) => sequence.length)).toList.sortWith(sortOccurancePairs)


/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences = (for (word <- s) yield wordOccurrences(word)) reduce ((occurA, occurB) => combineOccurrences(occurA, occurB))

/** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
  * the words that have that occurrence count.
  * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
  *
  * For example, the word "eat" has the following character occurrence list:
  *
  * `List(('a', 1), ('e', 1), ('t', 1))`
  *
  * Incidentally, so do the words "ate" and "tea".
  *
  * This means that the `dictionaryByOccurrences` map will contain an entry:
  *
  * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
  *
  */
lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
  def pairToWord(pairs: List[(Occurrences, Word)], acc: List[Word]): List[Word] = pairs match {
    case List() => acc
    case (_, word) :: xs => pairToWord(xs, word :: acc)
  }

  (for (word <- dictionary) yield ((wordOccurrences(word), word))) groupBy ((pair: (Occurrences, Word)) => pair._1) transform ((_, pair: List[(Occurrences, Word)]) => pairToWord(pair, List()))
}


/** Returns all the anagrams of a given word. */
def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())

/** Returns the list of all subsets of the occurrence list.
  * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
  * is a subset of `List(('k', 1), ('o', 1))`.
  * It also include the empty subset `List()`.
  *
  * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
  *
  * List(
  * List(),
  * List(('a', 1)),
  * List(('a', 2)),
  * List(('b', 1)),
  * List(('a', 1), ('b', 1)),
  * List(('a', 2), ('b', 1)),
  * List(('b', 2)),
  * List(('a', 1), ('b', 2)),
  * List(('a', 2), ('b', 2))
  * )
  *
  * Note that the order of the occurrence list subsets does not matter -- the subsets
  * in the example above could have been displayed in some other order.
  */
def combinations(occurrences: Occurrences): List[Occurrences] = {

  // out of the base pairs it generates the combinations of all letters with all their respective occurrence counts
  def letterCombos(base_occurences: List[Occurrences], acc: Occurrences): List[Occurrences] = {
    base_occurences match {
      case List() => List(List())
      case (pair :: pair_tail) :: tail => {
        List(acc :+ pair) ++ letterCombos(tail, acc :+ pair) ++ letterCombos(pair_tail +: tail, acc)
      }
      case (List()) :: tail => {
        letterCombos(tail, acc)
      }
    }
  }


  // generates list of pairs x,i for i from 0 to n for each x,n
  def baseCombos(occurrences: Occurrences): List[Occurrences] = {
    (for {
      occurance <- occurrences
    } yield (for {
      count <- (1 to occurance._2)
    } yield ((occurance._1, count))).toList)
  }

  letterCombos(baseCombos(occurrences), List())
}

/** Subtracts occurrence list `y` from occurrence list `x`.
  *
  * The precondition is that the occurrence list `y` is a subset of
  * the occurrence list `x` -- any character appearing in `y` must
  * appear in `x`, and its frequency in `y` must be smaller or equal
  * than its frequency in `x`.
  *
  * Note: the resulting value is an occurrence - meaning it is sorted
  * and has no zero-entries.
  */
def subtract(x: Occurrences, y: Occurrences): Occurrences = (x, y) match {
  case (_, List()) => x
  case (List(), _) => List()
  case ((xChar, xInt) :: xs, (yChar, yInt) :: ys) =>
    if (xChar == yChar) {
      if (yInt == xInt) subtract(xs, ys) // match & same size, "remove"
      else (xChar, xInt - yInt) +: subtract(xs, ys) // match & != 0, subtract and add TODO this ignores the y > x case, as stated in method comment, could throw an error here
    }
    else (xChar, xInt) +: subtract(xs, y) //different char, add original and move on
}



/** Returns a list of all anagram sentences of the given sentence.
  *
  * An anagram of a sentence is formed by taking the occurrences of all the characters of
  * all the words in the sentence, and producing all possible combinations of words with those characters,
  * such that the words have to be from the dictionary.
  *
  * The number of words in the sentence and its anagrams does not have to correspond.
  * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
  *
  * Also, two sentences with the same words but in a different order are considered two different anagrams.
  * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
  * `List("I", "love", "you")`.
  *
  * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
  *
  * List(
  * List(en, as, my),
  * List(en, my, as),
  * List(man, yes),
  * List(men, say),
  * List(as, en, my),
  * List(as, my, en),
  * List(sane, my),
  * List(Sean, my),
  * List(my, en, as),
  * List(my, as, en),
  * List(my, sane),
  * List(my, Sean),
  * List(say, men),
  * List(yes, man)
  * )
  *
  * The different sentences do not have to be output in the order shown above - any order is fine as long as
  * all the anagrams are there. Every returned word has to exist in the dictionary.
  *
  * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
  * so it has to be returned in this list.
  *
  * Note: There is only one anagram of an empty sentence.
  */
def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def generatePicks(occurrences: Occurrences) : Map[Occurrences,List[Word]] = {
    val options = combinations(occurrences)
    dictionaryByOccurrences.filterKeys((occurrence) => (options.contains(occurrence)))
  }

  def generateWordLists(remainder : Occurrences,wordsList : List[List[Word]]) : List[List[Word]] = {
    remainder match {
      case List() => List()
      case _ => (for ((occur,words) <- generatePicks(remainder) ) yield (generateWordLists(subtract(remainder,occur),(wordsList :+ words))) flatten).toList
    }

    //generatePicks(remainder).foreach((pair:(Occurrences,List[Word]))=> generateWordLists(subtract(remainder,pair._1),(wordsList :+ pair._2)))
  }
  sentence match {
    case List() => List(List())
    case _ => generateWordLists(sentenceOccurrences(sentence),List())
  }

}

def generateWordLists(remainder : Occurrences,wordsList : List[List[Word]]) : List[Sentence] = {
  def generatePicks(occurrences: Occurrences) : Map[Occurrences,List[Word]] = {
    val options = combinations(occurrences)
    dictionaryByOccurrences.filterKeys((occurrence) => (options.contains(occurrence)))
  }

  def generateSentences(possibilities: List[List[Word]], sentences:List[Sentence]):List[Sentence] = {
    possibilities match {
      case List() => sentences
      case head :: tail => generateSentences(tail,for (sentence <- sentences; word <- head) yield ((sentence :+ word)))
    }
  }

  remainder match {
    case List() => generateSentences(wordsList,List(List()))
    case _ => (for ((occur, words) <- generatePicks(remainder)) yield (generateWordLists(subtract(remainder, occur), (wordsList :+ words)))flatten).toList
  }
}

def generateSentences(possibilities: List[List[Word]], sentences:List[Sentence]):List[Sentence] = {
  possibilities match {
    case List() => sentences
    case head :: tail => generateSentences(tail,for (sentence <- sentences; word <- head) yield ((sentence :+ word)))
  }
}


def generatePicks(occurrences: Occurrences) : Map[Occurrences,List[Word]] = {
  val options = combinations(occurrences)
  dictionaryByOccurrences.filterKeys((occurrence) => (options.contains(occurrence)))
}

generatePicks(sentenceOccurrences(List("You","Olive")))
"generate Word List"
generateWordLists(sentenceOccurrences(List("You","Olive")),List(List()))
"generate Word List Remainder test"
generateWordLists(List(),List(List("I","Love"),List("Love","I")))


wordOccurrences("Blaaaabbwqerhrt2io4th130-4t93q40tghfbdnl")

wordOccurrences("AAAaaaBBbbb")




//def combineOccurrences(occur1: Occurrences, occur2: Occurrences) : Occurrences =
  //(occur1 ++ occur2) groupBy( (pair:(Char,Int)) => pair._1) transform((_,sequence) => sequence.sum)


/**
  {
    def addToMap(pair:(Char,Int), result_map:Map[Char,Int]) = result_map.get(pair._1) match {
      case None => result_map + pair
      case (letter,count) =>
  }
  }
**/

/**

def combineOccurrences(occur1: Occurrences, occur2: Occurrences) : Occurrences =
{
  def reducer(list:List[(Char,Int)]) : (Char,Int) = list.reduce((pair:(Char,Int),pair2:(Char,Int)) => (pair._1,pair._2 + pair2._2))
  (((occur1 ++ occur2) groupBy(letter => letter._1)) mapValues[(Char,Int)](reducer)).values.toList.sortWith(sortOccurancePairs)
  // ^ that makes my head hurt >.<
}



def sentenceOccurrences(s: Sentence): Occurrences = (for ( word <- s ) yield wordOccurrences(word)) reduce((occurA, occurB)=> combineOccurrences(occurA,occurB) )
//(for ( (x,y) <- occur1 zip occur2 ) yield (x._1,x._2+y._2)).toList



//combineOccurrences(aab,bcc)
**/
val myFirstSentence = List("OMG","Sentence")

val dict = List("OMG", "GMO", "MOG", "AAA")
val aab = wordOccurrences("aaab")
val bcc = wordOccurrences("bcc")


def reducer(list:List[(Char,Int)]) : (Char,Int) = list.reduce((pair:(Char,Int),pair2:(Char,Int)) => (pair._1,pair._2 + pair2._2))





((aab ++ bcc) groupBy(letter => letter._1)) mapValues[(Char,Int)](reducer)

val o1 = combineOccurrences(aab,bcc)


sentenceOccurrences(myFirstSentence)



//(list:List[(Char,Int)]) => list.fold(0)((pair,pair2) => pair._2 + pair2._2)
def dictionaryByOccurrences(dictionary:List[Word]): Any = {
  def pairToWord(pairs:List[(Occurrences,Word)], acc:List[Word]) : List[Word] = pairs match  {
    case List() => acc
    case (_, word) :: xs => pairToWord(xs, word::acc)
  }

  (for ( word <- dictionary) yield((wordOccurrences(word),word))) groupBy((pair:(Occurrences,Word))=> pair._1) transform((_,pair:List[(Occurrences,Word)])=> pairToWord(pair,List()))
}



dictionaryByOccurrences(dict)







/**
//  def combinations(occurrences: Occurrences): List[Occurrences] = {

val x = (for {
  occurance <- o1
} yield ( for {
  count <- (1 to occurance._2)
} yield ( (occurance._1,count))).toList)





def letterCombos(base_occurences:List[Occurrences], acc:Occurrences) : List[Occurrences] = {
  base_occurences match {
    case List() => List(List())
    case (pair :: pair_tail) :: tail => {
      List(acc :+ pair) ++ letterCombos(tail, acc :+ pair ) ++ letterCombos(pair_tail +: tail, acc)
    }
    case (List()) :: tail => {
      letterCombos(tail,acc)
    }
  }
}
//List(pair+:acc) ++ letterCombos(tail,pair+:acc)

**/
def ppPair(pair:(Char,Int)):String = {
  pair match { case (ltr,int) => ltr + "" + int
}}

def ppOccurance(occurrences:Occurrences):String = {
  occurrences match {
    case List()=>""
    case head :: tail => ppPair(head) + " " + ppOccurance(tail)
  }
}

def ppOccuranceList(list:List[Occurrences]):String = {
  list match {
    case List() => ""
    case head :: tail => ppOccurance(head) + " |\n" + ppOccuranceList(tail)
  }
}

//ppOccuranceList(letterCombos(x,List()))




//lazy val dictionaryByOccurrences(dictionary:List[Word]): Map[Occurrences, List[Word]] = ((for ( word <- dictionary ) yield ( (wordOccurrences(word),word))) groupBy (pair:(Occurrences,(Occurrences,List[Word])=> pair._1))