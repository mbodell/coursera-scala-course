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
  type Occurrences = List[(Char, Int)]
  
  val w = "HelloMaamHowAreYou"                    //> w  : String = HelloMaamHowAreYou
  
  val wc = w.toLowerCase()                        //> wc  : String = hellomaamhowareyou
  
  val wgb = wc groupBy ((c: Char) => c)           //> wgb  : scala.collection.immutable.Map[Char,String] = Map(e -> ee, y -> y, u 
                                                  //| -> u, a -> aaa, m -> mm, l -> ll, h -> hh, r -> r, w -> w, o -> ooo)
  val om = wgb.map (((t: (Char, String)) => (t._1, t._2.length)))
                                                  //> om  : scala.collection.immutable.Map[Char,Int] = Map(e -> 2, y -> 1, u -> 1,
                                                  //|  a -> 3, m -> 2, l -> 2, h -> 2, r -> 1, w -> 1, o -> 3)
  val o = om.toList.sorted                        //> o  : List[(Char, Int)] = List((a,3), (e,2), (h,2), (l,2), (m,2), (o,3), (r,1
                                                  //| ), (u,1), (w,1), (y,1))
 
  def wordOccurrences(w: Word): Occurrences = {
    ((w.toLowerCase() groupBy ((c: Char) => c)).map (((t: (Char, String)) => (t._1, t._2.length)))).toList.sorted
  }                                               //> wordOccurrences: (w: forcomp.playsheet.Word)forcomp.playsheet.Occurrences

  wordOccurrences(w)                              //> res0: forcomp.playsheet.Occurrences = List((a,3), (e,2), (h,2), (l,2), (m,2
                                                  //| ), (o,3), (r,1), (u,1), (w,1), (y,1))
  
  val s:Sentence = List("Foo", "Bar", w, "Man")   //> s  : forcomp.playsheet.Sentence = List(Foo, Bar, HelloMaamHowAreYou, Man)
  
  val xs = s.flatten.mkString                     //> xs  : String = FooBarHelloMaamHowAreYouMan
  
  wordOccurrences(xs)                             //> res1: forcomp.playsheet.Occurrences = List((a,5), (b,1), (e,2), (f,1), (h,2
                                                  //| ), (l,2), (m,3), (n,1), (o,5), (r,2), (u,1), (w,1), (y,1))
  
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.flatten.mkString)
  }                                               //> sentenceOccurrences: (s: forcomp.playsheet.Sentence)forcomp.playsheet.Occur
                                                  //| rences
  
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.playsheet.Word] = List(Aarhus, Aaron, Ababa, aba
                                                  //| ck, abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, ab
                                                  //| ased, abasement, abasements, abases, abash, abashed, abashes, abashing, aba
                                                  //| sing, abate, abated, abatement, abatements, abater, abates, abating, Abba, 
                                                  //| abbe, abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbrev
                                                  //| iates, abbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, 
                                                  //| abdominal, abduct, abducted, abduction, abductions, abductor, abductors, ab
                                                  //| ducts, Abe, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, ab
                                                  //| erration, aberrations, abet, abets, abetted, abetter, abetting, abeyance, a
                                                  //| bhor, abhorred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abid
                                                  //| es, abiding, Abidjan, Abigail, Abilene, abilities, ability, abject, abjecti
                                                  //| on, abjections, abjectly, abjectness, abjure, abjured, abjures, abjuring, a
                                                  //| blate, ablated, ablates
                                                  //| Output exceeds cutoff limit.
  
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy ((w: Word) => wordOccurrences(w))
                                                  //> dictionaryByOccurrences: => Map[forcomp.playsheet.Occurrences,List[forcomp.
                                                  //| playsheet.Word]]
                                                  
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)) match {
    case Some(words) => words
    case None => List()
  }                                               //> wordAnagrams: (word: forcomp.playsheet.Word)List[forcomp.playsheet.Word]
  
  wordAnagrams(w)                                 //> res2: List[forcomp.playsheet.Word] = List()
  
  wordAnagrams("Iloveyou")                        //> res3: List[forcomp.playsheet.Word] = List()
  
  wordAnagrams("abet")                            //> res4: List[forcomp.playsheet.Word] = List(abet, beat, beta)
  
  val occ: Occurrences = List(('a', 2), ('b', 2)) //> occ  : forcomp.playsheet.Occurrences = List((a,2), (b,2))
  
  def combine(in: List[Char]): Seq[String] =
    for {
        len <- 1 to in.length
        combinations <- in combinations len
    } yield combinations.mkString                 //> combine: (in: List[Char])Seq[String]

  def tu2st(t: (Char, Int)): String =
    if (t._2 <= 0) ""
    else t._1 + tu2st((t._1, t._2 -1))            //> tu2st: (t: (Char, Int))String
  
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    List() +: combine(occurrences.map(tu2st).flatten).map(wordOccurrences(_)).toList
  }                                               //> combinations: (occurrences: forcomp.playsheet.Occurrences)List[forcomp.play
                                                  //| sheet.Occurrences]
  
  combinations(occ)                               //> res5: List[forcomp.playsheet.Occurrences] = List(List(), List((a,1)), List(
                                                  //| (b,1)), List((a,2)), List((a,1), (b,1)), List((b,2)), List((a,2), (b,1)), L
                                                  //| ist((a,1), (b,2)), List((a,2), (b,2)))
  
  
 
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
 }                                                //> subtract: (x: forcomp.playsheet.Occurrences, y: forcomp.playsheet.Occurrenc
                                                  //| es)forcomp.playsheet.Occurrences
  val sent = List("Yes", "man")                   //> sent  : List[String] = List(Yes, man)
  
  val ct = combinations(sentenceOccurrences(sent))//> ct  : List[forcomp.playsheet.Occurrences] = List(List(), List((a,1)), List(
                                                  //| (e,1)), List((m,1)), List((n,1)), List((s,1)), List((y,1)), List((a,1), (e,
                                                  //| 1)), List((a,1), (m,1)), List((a,1), (n,1)), List((a,1), (s,1)), List((a,1)
                                                  //| , (y,1)), List((e,1), (m,1)), List((e,1), (n,1)), List((e,1), (s,1)), List(
                                                  //| (e,1), (y,1)), List((m,1), (n,1)), List((m,1), (s,1)), List((m,1), (y,1)), 
                                                  //| List((n,1), (s,1)), List((n,1), (y,1)), List((s,1), (y,1)), List((a,1), (e,
                                                  //| 1), (m,1)), List((a,1), (e,1), (n,1)), List((a,1), (e,1), (s,1)), List((a,1
                                                  //| ), (e,1), (y,1)), List((a,1), (m,1), (n,1)), List((a,1), (m,1), (s,1)), Lis
                                                  //| t((a,1), (m,1), (y,1)), List((a,1), (n,1), (s,1)), List((a,1), (n,1), (y,1)
                                                  //| ), List((a,1), (s,1), (y,1)), List((e,1), (m,1), (n,1)), List((e,1), (m,1),
                                                  //|  (s,1)), List((e,1), (m,1), (y,1)), List((e,1), (n,1), (s,1)), List((e,1), 
                                                  //| (n,1), (y,1)), List((e,1), (s,1), (y,1)), List((m,1), (n,1), (s,1)), List((
                                                  //| m,1), (n,1), (y,1)), Li
                                                  //| Output exceeds cutoff limit.
                                                  
  val ct1 = ct(26)                                //> ct1  : forcomp.playsheet.Occurrences = List((a,1), (m,1), (n,1))
  
  ct1.map(tu2st).mkString                         //> res6: String = amn

  val wds = ct.filterNot((o: Occurrences) => wordAnagrams(o.map(tu2st).mkString).isEmpty).flatMap(dictionaryByOccurrences)
                                                  //> wds  : List[forcomp.playsheet.Word] = List(am, an, as, em, me, en, my, Mae,
                                                  //|  sea, aye, yea, man, Sam, Amy, May, San, any, nay, say, men, yes, amen, man
                                                  //| e, mean, name, Ames, same, seam, sane, Sean, ayes, easy, yeas, Mans, many, 
                                                  //| mens, manes, means, names, seamy)
  
  for {
    words <- wds
    remaining = subtract(sentenceOccurrences(sent), wordOccurrences(words))
    
  } yield List(words, remaining)                  //> res7: List[List[Object]] = List(List(am, List((e,1), (n,1), (s,1), (y,1))),
                                                  //|  List(an, List((e,1), (m,1), (s,1), (y,1))), List(as, List((e,1), (m,1), (n
                                                  //| ,1), (y,1))), List(em, List((a,1), (n,1), (s,1), (y,1))), List(me, List((a,
                                                  //| 1), (n,1), (s,1), (y,1))), List(en, List((a,1), (m,1), (s,1), (y,1))), List
                                                  //| (my, List((a,1), (e,1), (n,1), (s,1))), List(Mae, List((n,1), (s,1), (y,1))
                                                  //| ), List(sea, List((m,1), (n,1), (y,1))), List(aye, List((m,1), (n,1), (s,1)
                                                  //| )), List(yea, List((m,1), (n,1), (s,1))), List(man, List((e,1), (s,1), (y,1
                                                  //| ))), List(Sam, List((e,1), (n,1), (y,1))), List(Amy, List((e,1), (n,1), (s,
                                                  //| 1))), List(May, List((e,1), (n,1), (s,1))), List(San, List((e,1), (m,1), (y
                                                  //| ,1))), List(any, List((e,1), (m,1), (s,1))), List(nay, List((e,1), (m,1), (
                                                  //| s,1))), List(say, List((e,1), (m,1), (n,1))), List(men, List((a,1), (s,1), 
                                                  //| (y,1))), List(yes, List((a,1), (m,1), (n,1))), List(amen, List((s,1), (y,1)
                                                  //| )), List(mane, List((s,
                                                  //| Output exceeds cutoff limit.
  val wd: Word = "foo"                            //> wd  : forcomp.playsheet.Word = foo
  val se1: Sentence = List("bar", "baz")          //> se1  : forcomp.playsheet.Sentence = List(bar, baz)
  val se2: Sentence = List("beep", "bop")         //> se2  : forcomp.playsheet.Sentence = List(beep, bop)
  val ls: List[Sentence] = List(se1, se2)         //> ls  : List[forcomp.playsheet.Sentence] = List(List(bar, baz), List(beep, bo
                                                  //| p))
  
  wd +: se1                                       //> res8: List[forcomp.playsheet.Word] = List(foo, bar, baz)
 
  ls.map(wd :: _)                                 //> res9: List[List[forcomp.playsheet.Word]] = List(List(foo, bar, baz), List(f
                                                  //| oo, beep, bop))

  def sentenceOccurrenceAnagrams(sent: Occurrences): List[Sentence] = {
    if(sent.isEmpty) List(Nil)
    else {
      for {
        i <- combinations(sent).filterNot((o: Occurrences) => wordAnagrams(o.map(tu2st).mkString).isEmpty).flatMap(dictionaryByOccurrences)
        if(!i.isEmpty)
        j <- sentenceOccurrenceAnagrams(subtract(sent, wordOccurrences(i)))
      } yield i +: j
    }
  }                                               //> sentenceOccurrenceAnagrams: (sent: forcomp.playsheet.Occurrences)List[forco
                                                  //| mp.playsheet.Sentence]

 
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
  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentenceOccurrenceAnagrams(sentenceOccurrences(sentence))
                                                  //> sentenceAnagrams: (sentence: forcomp.playsheet.Sentence)List[forcomp.playsh
                                                  //| eet.Sentence]
  sentenceAnagrams(sent)                          //> res10: List[forcomp.playsheet.Sentence] = List(List(as, en, my), List(as, m
                                                  //| y, en), List(en, as, my), List(en, my, as), List(my, as, en), List(my, en, 
                                                  //| as), List(my, sane), List(my, Sean), List(man, yes), List(say, men), List(m
                                                  //| en, say), List(yes, man), List(sane, my), List(Sean, my))
  
 
  sentenceAnagrams(List())                        //> res11: List[forcomp.playsheet.Sentence] = List(List())

  
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}