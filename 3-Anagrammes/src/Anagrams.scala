import scala.collection.immutable._
import scala.io.Source
import scala.util.matching.Regex


object Anagrams extends App {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** A fingerprint is a string which represents a sorted sequence of characters:
   *  Examples: 
   *
   *    "aaccx"
   *    "abyz"
   *    "ppp"
   *    ""
   */

  type FingerPrint = String


  /** The dictionary is simply a sequence of words.
   *  You can begin your development with this simple example. 
   *  A dictionary of English words is given to you as an external file (linuxwords.txt)  
   *  that you can load to use with your program  
   */
  val path = "/home/james/Documents/HEIG-VD/Cours/SCALA/Labo/SCALA_Labos/3-Anagrammes/src/linuxwords.txt"
  val dictionary: List[Word] = Source.fromFile(path).getLines.toList


  /** Converts a word/sentence into its fingerprint.
   *  The fingerprint has the same characters as the word, with the same
   *  number of occurrences, but the characters appear in sorted order.
   */

  def fingerPrint(s: Word): FingerPrint = {
    s.toLowerCase.toList.sorted.mkString // Sorted all the letters of the word to make the fingerPrint
  }

  def fingerPrint(s: Sentence): FingerPrint = {
    fingerPrint(s.mkString) // take the fingerprint of all the words of the sentence
  }


  /** `matchingWords` is a `Map` from fingerprints to a sequence of all
   *  the words that have that fingerprint.
   *  This map serves as an easy way to obtain all the anagrams of a word given its fingerprint.
   *
   *  For example, the word "eat" has the fingerprint "aet".
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `matchingWords` map will contain an entry:
   *
   *   "aet"-> List("ate", "eat", "tea")
   */

  val matchingWords: Map[FingerPrint, List[Word]] = (for(word <- dictionary) yield (fingerPrint(word.toLowerCase), word.toLowerCase)).groupBy(_._1).mapValues(x => x.map(_._2))


  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    if(matchingWords.contains(fingerPrint(word))) matchingWords(fingerPrint(word)) else List() // If the words does not have anny matching words, return an empty List
  }

  // Test code with for example:
  //println(wordAnagrams("i"))
  //println(wordAnagrams("jbdikb"))


  /** Returns the list of all subsequences of a fingerprint.
   *  This includes the fingerprint itself, i.e.
   *  "ko" is a subsequence of "kkoo". It also always includes
   *  the empty string "".
   *
   *  Example: the subsequences of the fingerprint "abbc" are
   *
   *    List("", "c", "b", "bc", "bb", "bbc", "a", "ac", "ab", "abc", "abb", "abbc")
   *
   *  Note that the order of the subsequences does not matter -- the subsequences
   *  in the example above could have been displayed in some other order.
    * Aide : https://stackoverflow.com/questions/1070859/listing-combinations-with-repetitions-in-scala
   */

  def subseqs(fp: FingerPrint): List[FingerPrint] = {
    (for {
      i <- 0 to fp.length
      x <- fp.combinations(i) // Return all the combination of length i with the letter of fp
    } yield x).toList
  }

  // Test code with for example:
  // println(subseqs("aabbc"))


  /** Subtracts fingerprint `y` from fingerprint `x`.
   *
   *  The precondition is that the fingerprint `y` is a subsequence of
   *  the fingerprint `x` -- any character appearing in `y` must
   *  appear in `x`.
    * Source : https://stackoverflow.com/questions/14495613/removing-multiple-character-types-from-a-string
   */

  def subtract(x: FingerPrint, y: FingerPrint): FingerPrint = x.toList.diff(y.toList).mkString

  // Test code with for example:
  // println(subtract("", "aabb"))


  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the fingerprint of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive","you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def findAnagrams(fp : FingerPrint, wa : List[Word], acc : Sentence = List(), accSentence: List[Sentence] = List()) : List[Sentence] = {
      wa match {
        case Nil => accSentence // this is the end of the list of word anagram possible, return the list of sentence made with the anagram
        case _ if fp == "" => acc :: accSentence // we used all the letters of the finger print, so it's a correct sentence to saved
        case x::xs => {
            // check if the fingerprint contain the letter of the first word of the list of possible words anagram list
            if (subtract(x, fp) == "") {
              /*
              Continue to explore the possiblities with the first word saved
              and concataine the resut with the result with exploring the other possibilties of the list without saving this word
              This is not a terminal recursive but didn't find a better way.
                */
              findAnagrams(subtract(fp, x), wa, x :: acc, accSentence) ++ findAnagrams(fp, xs, acc, accSentence)
            } else {
              // Pass to the next word
              findAnagrams(fp, xs, acc, accSentence)
            }
        }
      }
    }
    val fpInitial = fingerPrint(sentence)
    findAnagrams(fpInitial, subseqs(fpInitial).flatMap(wordAnagrams))
  }



  // Test code with for example:
  println(sentenceAnagrams(List("eat", "tea")))
  println(sentenceAnagrams(List("you", "olive")))
  println(sentenceAnagrams(List("I", "love", "you")))

}