package streams
import Bloxorz._

object works {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(92); 
  println("Welcome to the Scala worksheet")
  
  
    trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  };$skip(860); val res$0 = 

   Level1.level;System.out.println("""res0: String = """ + $show(res$0));$skip(74); 
   val foo = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'));System.out.println("""foo  : scala.collection.immutable.Vector[scala.collection.immutable.Vector[Char]] = """ + $show(foo ));$skip(63); val res$1 = 
   foo.indexWhere((v: Vector[Char]) => (v.indexOf('o') != -1));System.out.println("""res1: Int = """ + $show(res$1));$skip(63); val res$2 = 
   foo.indexWhere((v: Vector[Char]) => (v.indexOf('T') != -1));System.out.println("""res2: Int = """ + $show(res$2))}
   
    
}
