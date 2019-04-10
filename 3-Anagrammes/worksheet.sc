import scala.collection.immutable.List
import scala.io.Source

/*val path = "/home/james/Documents/HEIG-VD/Cours/SCALA/Labo/SCALA_Labos/3-Anagrammes/src/linuxwords.txt"
val dictionary: List[Word] = Source.fromFile(path).getLines.toList
(for(word <- dictionary) yield (word, fingerPrint(word))).groupBy(_._1).mapValues(x => x.map(_._2))
*/

val s = "aabbccdd".toList

val t = "ab".toList

s diff t

