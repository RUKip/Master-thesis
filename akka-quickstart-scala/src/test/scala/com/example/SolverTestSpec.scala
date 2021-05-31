package com.example

import com.example.solver.SolverScalaWrapper
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SolverTestSpec extends AnyFlatSpec with should.Matchers {

  var mapping: Map[Int, Variable] = Map()

  /* Graph structure like:
      1
      |
      2
     / \
    3 - 4
    |   |
    5 - 6
 */
  val node1 = Variable(1, List(2), "Blank")
  val node2 = Variable(2, List(1,3,4), "Blank")
  val node3: Variable = Variable(3, List(2,4,5), "Blank")
  val node4 = Variable(4, List(2,3,6), "Blank")
  val node5 = Variable(5, List(3,6), "Blank")
  val node6 = Variable(6, List(4,5), "Blank")

  mapping += (1 -> node1)
  mapping += (2 -> node2)
  mapping += (3 -> node3)
  mapping += (4 -> node4)
  mapping += (5 -> node5)
  mapping += (6 -> node6)

  /* Decomposition of graph:
    (2,3,4)
     /   \
 (3,4,5) (1,2)
    |
 (4,5,6)
*/
  val root = TreeNode(1, 0, List(2, 3), List(2,3,4), mapping, List(), Mapping(1, Map(2 -> List(3,4), 3 -> List(2))))

  "A solution" should "give all solutions" in {

    val solutions: List[Map[Int, String]] = SolverScalaWrapper.calcSolutions(root)
    solutions should be (List(
      Map(2 -> "red", 3 -> "blue", 4 -> "yellow"),
      Map(2 -> "red", 3 -> "yellow", 4 -> "blue"),
      Map(2 -> "blue", 3 -> "red", 4 -> "yellow"),
      Map(2 -> "blue", 3 -> "yellow", 4 -> "red"),
      Map(2 -> "yellow", 3 -> "red", 4 -> "blue"),
      Map(2 -> "yellow", 3 -> "blue", 4 -> "red")
    ))

    val new_root = root.updateNodes(Map(2 -> "red", 3 -> "blue"))
    val solutions2: List[Map[Int, String]] = SolverScalaWrapper.calcSolutions(new_root)
    solutions2 should be (List(Map(4 -> "yellow")))
  }
}
