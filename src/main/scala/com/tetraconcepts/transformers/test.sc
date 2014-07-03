import com.tetraconcepts.transformers.FrequentSequences
import com.tetraconcepts.utilities.SequenceMethods._

val fs = new FrequentSequences
val sdb: SequenceDB =List(
  (10, List(List("a"),List("a","b","c"),List("a","c"),List("d"),List("c","f"))),
  (20, List(List("a","d"), List("c"), List("b","c"), List("a","e"))),
  (30, List(List("e","f"), List("a","b"), List("d","f"),List("c"),List("b"))),
  (40, List(List("e"), List("g"), List("a","f"), List("c"), List("b"), List("c"))))

val fp1 = fs.findLength1FrequentPatterns(sdb.map(_._2),2)
sdb.map(s => s._2.frequentProjection(fp1))

val apdb = fs.projectedDB((List(List("a")),4), sdb.map(_._2),fp1)
fs.projectedDB((List(List("b")),4), sdb.map(_._2),fp1)
fs.projectedDB((List(List("c")),4), sdb.map(_._2),fp1)
fs.projectedDB((List(List("d")),3), sdb.map(_._2),fp1)
fs.projectedDB((List(List("e")),2), sdb.map(_._2),fp1)
fs.projectedDB((List(List("f")),2), sdb.map(_._2),fp1)

val fapdb = fs.findLength1FrequentPatterns(apdb,2)
