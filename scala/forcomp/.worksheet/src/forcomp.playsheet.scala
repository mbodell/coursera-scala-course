package forcomp



object playsheet {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(753); 
  
  val w = "HelloMaamHowAreYou";System.out.println("""w  : String = """ + $show(w ));$skip(30); 
  
  val wc = w.toLowerCase();System.out.println("""wc  : String = """ + $show(wc ));$skip(43); 
  
  val wgb = wc groupBy ((c: Char) => c);System.out.println("""wgb  : scala.collection.immutable.Map[Char,String] = """ + $show(wgb ));$skip(66); 
  val om = wgb.map (((t: (Char, String)) => (t._1, t._2.length)));System.out.println("""om  : scala.collection.immutable.Map[Char,Int] = """ + $show(om ));$skip(27); 
  val o = om.toList.sorted;System.out.println("""o  : List[(Char, Int)] = """ + $show(o ));$skip(168); 
 
  def wordOccurrences(w: Word): Occurrences = {
    ((w.toLowerCase() groupBy ((c: Char) => c)).map (((t: (Char, String)) => (t._1, t._2.length)))).toList.sorted
  };System.out.println("""wordOccurrences: (w: forcomp.playsheet.Word)forcomp.playsheet.Occurrences""");$skip(22); val res$0 = 

  wordOccurrences(w);System.out.println("""res0: forcomp.playsheet.Occurrences = """ + $show(res$0));$skip(51); 
  
  val s:Sentence = List("Foo", "Bar", w, "Man");System.out.println("""s  : forcomp.playsheet.Sentence = """ + $show(s ));$skip(33); 
  
  val xs = s.flatten.mkString;System.out.println("""xs  : String = """ + $show(xs ));$skip(25); val res$1 = 
  
  wordOccurrences(xs);System.out.println("""res1: forcomp.playsheet.Occurrences = """ + $show(res$1));$skip(103); 
  
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.flatten.mkString)
  };System.out.println("""sentenceOccurrences: (s: forcomp.playsheet.Sentence)forcomp.playsheet.Occurrences""");$skip(49); 
  
  val dictionary: List[Word] = loadDictionary;System.out.println("""dictionary  : List[forcomp.playsheet.Word] = """ + $show(dictionary ));$skip(123); 
  
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy ((w: Word) => wordOccurrences(w));System.out.println("""dictionaryByOccurrences: => Map[forcomp.playsheet.Occurrences,List[forcomp.playsheet.Word]]""");$skip(213); 
                                                  
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)) match {
    case Some(words) => words
    case None => List()
  };System.out.println("""wordAnagrams: (word: forcomp.playsheet.Word)List[forcomp.playsheet.Word]""");$skip(21); val res$2 = 
  
  wordAnagrams(w);System.out.println("""res2: List[forcomp.playsheet.Word] = """ + $show(res$2));$skip(30); val res$3 = 
  
  wordAnagrams("Iloveyou");System.out.println("""res3: List[forcomp.playsheet.Word] = """ + $show(res$3));$skip(26); val res$4 = 
  
  wordAnagrams("abet");System.out.println("""res4: List[forcomp.playsheet.Word] = """ + $show(res$4));$skip(53); 
  
  val occ: Occurrences = List(('a', 2), ('b', 2));System.out.println("""occ  : forcomp.playsheet.Occurrences = """ + $show(occ ));$skip(166); 
  
  def combine(in: List[Char]): Seq[String] =
    for {
        len <- 1 to in.length
        combinations <- in combinations len
    } yield combinations.mkString;System.out.println("""combine: (in: List[Char])Seq[String]""");$skip(100); 

  def tu2st(t: (Char, Int)): String =
    if (t._2 <= 0) ""
    else t._1 + tu2st((t._1, t._2 -1));System.out.println("""tu2st: (t: (Char, Int))String""");$skip(160); 
  
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    List() +: combine(occurrences.map(tu2st).flatten).map(wordOccurrences(_)).toList
  };System.out.println("""combinations: (occurrences: forcomp.playsheet.Occurrences)List[forcomp.playsheet.Occurrences]""");$skip(23); val res$5 = 
  
  combinations(occ);System.out.println("""res5: List[forcomp.playsheet.Occurrences] = """ + $show(res$5));$skip(475); 
  
  
 
 def subtract(x: Occurrences, y: Occurrences): Occurrences = x match {
   case Nil => x
   case (c, i) :: xs => y match {
     case Nil => (c, i) :: subtract(xs, y)
     case (cy, iy) :: ys => { if(c != cy) subtract(List((c, i)), ys) ::: subtract(xs, y)
                              else { if(i == iy) subtract(xs, y)
                                     else (c, i - iy) :: subtract(xs, y)
                                   }
                            }
   }
 };System.out.println("""subtract: (x: forcomp.playsheet.Occurrences, y: forcomp.playsheet.Occurrences)forcomp.playsheet.Occurrences""");$skip(32); 
  val sent = List("Yes", "man");System.out.println("""sent  : List[String] = """ + $show(sent ));$skip(54); 
  
  val ct = combinations(sentenceOccurrences(sent));System.out.println("""ct  : List[forcomp.playsheet.Occurrences] = """ + $show(ct ));$skip(70); 
                                                  
  val ct1 = ct(26);System.out.println("""ct1  : forcomp.playsheet.Occurrences = """ + $show(ct1 ));$skip(29); val res$6 = 
  
  ct1.map(tu2st).mkString;System.out.println("""res6: String = """ + $show(res$6));$skip(124); 

  val wds = ct.filterNot((o: Occurrences) => wordAnagrams(o.map(tu2st).mkString).isEmpty).flatMap(dictionaryByOccurrences);System.out.println("""wds  : List[forcomp.playsheet.Word] = """ + $show(wds ));$skip(142); val res$7 = 
  
  for {
    words <- wds
    remaining = subtract(sentenceOccurrences(sent), wordOccurrences(words))
    
  } yield List(words, remaining);System.out.println("""res7: List[List[Object]] = """ + $show(res$7));$skip(23); 
  val wd: Word = "foo";System.out.println("""wd  : forcomp.playsheet.Word = """ + $show(wd ));$skip(41); 
  val se1: Sentence = List("bar", "baz");System.out.println("""se1  : forcomp.playsheet.Sentence = """ + $show(se1 ));$skip(42); 
  val se2: Sentence = List("beep", "bop");System.out.println("""se2  : forcomp.playsheet.Sentence = """ + $show(se2 ));$skip(42); 
  val ls: List[Sentence] = List(se1, se2);System.out.println("""ls  : List[forcomp.playsheet.Sentence] = """ + $show(ls ));$skip(15); val res$8 = 
  
  wd +: se1;System.out.println("""res8: List[forcomp.playsheet.Word] = """ + $show(res$8));$skip(20); val res$9 = 
 
  ls.map(wd :: _);System.out.println("""res9: List[List[forcomp.playsheet.Word]] = """ + $show(res$9));$skip(397); 

  def sentenceOccurrenceAnagrams(sent: Occurrences): List[Sentence] = {
    if(sent.isEmpty) List(Nil)
    else {
      for {
        i <- combinations(sent).filterNot((o: Occurrences) => wordAnagrams(o.map(tu2st).mkString).isEmpty).flatMap(dictionaryByOccurrences)
        if(!i.isEmpty)
        j <- sentenceOccurrenceAnagrams(subtract(sent, wordOccurrences(i)))
      } yield i +: j
    }
  };System.out.println("""sentenceOccurrenceAnagrams: (sent: forcomp.playsheet.Occurrences)List[forcomp.playsheet.Sentence]""");$skip(511); 

 
 /*
  def sentenceOccurrenceAnagrams(sent: Occurrences): List[Sentence] = {
    if(sent.isEmpty) List(Nil)
    else {
      for {
        i <- combinations(sent)
        if(!i.isEmpty)
        if(!wordAnagrams(i.map(tu2st).mkString).isEmpty)
        j <- wordAnagrams(i.map(tu2st).mkString)
      } yield List(j) :: sentenceOccurrenceAnagrams(subtract(sent, i))
    }.flatten
  }
  
*/
  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentenceOccurrenceAnagrams(sentenceOccurrences(sentence));System.out.println("""sentenceAnagrams: (sentence: forcomp.playsheet.Sentence)List[forcomp.playsheet.Sentence]""");$skip(25); val res$10 = 
  sentenceAnagrams(sent);System.out.println("""res10: List[forcomp.playsheet.Sentence] = """ + $show(res$10));$skip(33); val res$11 = 
  
 
  sentenceAnagrams(List());System.out.println("""res11: List[forcomp.playsheet.Sentence] = """ + $show(res$11));$skip(48); 

  
  println("Welcome to the Scala worksheet")}
}
