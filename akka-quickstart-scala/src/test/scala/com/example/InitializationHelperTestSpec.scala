package com.example

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, LogCapturing, TestInbox}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InitializationHelperTestSpec  extends AnyWordSpec
  with BeforeAndAfterAll
  with LogCapturing
  with Matchers {

  "should load some tree as before storing" in {
    //    val base = InitializationHelper.generateBase(0, Range(1, 10), 3, List())
    //    val base = InitializationHelper.createTreeStructure(3,4,1,1,1,10)
    val base = InitializationHelper.init()
    println(base.toMap)
    val tree = InitializationHelper.createUsableTree(base)
    println("******")
    //    println(tree.head._2.full_graph_mapping)
    //    tree.foreach { case (id, node) => println(id + " " + node.copy(full_graph_mapping = Map()))}
    val file_name = "test_generated_tree.json"
    InitializationHelper.storeTree(base, file_name)
    val loaded_base = InitializationHelper.loadTree(file_name)
    assert(base.toMap === loaded_base.toMap)
  }
}