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
    val base = InitializationHelper.createTreeStructure(3,4,1,1,1,10)
    println(base.toMap)
    val tree = InitializationHelper.createUsableTree(base)
    println("******")
    println(tree.head._2.full_graph_mapping)
//    println(InitializationHelper.bindGroupings(Map(base.head), base.tail, base, 2, Map()))
    //override def afterAll(): Unit = testKit.shutdownTestKit()
  }
}