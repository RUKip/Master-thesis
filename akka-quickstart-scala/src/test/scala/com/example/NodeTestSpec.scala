package com.example

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, LogCapturing, TestInbox}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeTestSpec  extends AnyWordSpec
  with BeforeAndAfterAll
  with LogCapturing
  with Matchers {
  //val testKit = ActorTestKit()

  //Init graph and tree
  val graph: Map[Int, Variable] = InitializationHelper.initAGraph()

  //Calculate graph tree-decomposition
  val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

  "should spawn NodeSearch after receive solution" in {
//    val base = InitializationHelper.generateBase(0, Range(1, 10), 3, List())
//    val base = InitializationHelper.createTreeStructure(3,4,1,1,1,10)
//    println(base.toMap)
//    val tree = InitializationHelper.createUsableTree(base)
//    println("******")
////    println(tree.head._2.full_graph_mapping)
////    tree.foreach { case (id, node) => println(id + " " + node.copy(full_graph_mapping = Map()))}
//    val file_name = "8nodes_B5_W7_D3_C2.json"
//    InitializationHelper.storeTree(base, file_name)
//    val loaded_base = InitializationHelper.loadTree(file_name)
//    assert(base.toString() == loaded_base.toString())
  }
}