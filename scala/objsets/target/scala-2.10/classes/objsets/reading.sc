package objsets

object reading {
  val set1 = new Empty                            //> set1  : objsets.Empty = objsets.Empty@2f616fcf
  val set2 = set1.incl(new Tweet("a", "a body", 20))
                                                  //> set2  : objsets.TweetSet = objsets.NonEmpty@6261b548
  val set3 = set2.incl(new Tweet("b", "b body", 20))
                                                  //> set3  : objsets.TweetSet = objsets.NonEmpty@73cd1e4e
  val c = new Tweet("c", "c body", 7)             //> c  : objsets.Tweet = User: c
                                                  //| Text: c body [7]
  val d = new Tweet("d", "d body", 9)             //> d  : objsets.Tweet = User: d
                                                  //| Text: d body [9]
  val set4c = set3.incl(c)                        //> set4c  : objsets.TweetSet = objsets.NonEmpty@1f80ce47
  val set4d = set3.incl(d)                        //> set4d  : objsets.TweetSet = objsets.NonEmpty@4166a779
  val set5 = set4c.incl(d)                        //> set5  : objsets.TweetSet = objsets.NonEmpty@3a7d1a7a


  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  set5.filter(tw => tw.retweets == 20)            //> res0: objsets.TweetSet = objsets.NonEmpty@7b3d2b1c
  
  
}