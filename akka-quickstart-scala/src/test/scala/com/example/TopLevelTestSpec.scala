package com.example

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, LogCapturing}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.{Node, TopLevel}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TopLevelTestSpec
  extends AnyWordSpec
    with BeforeAndAfterAll
    with LogCapturing
    with Matchers {
  val testKit = ActorTestKit()

  //Init graph and tree
  val graph: Map[Int, Variable] = InitializationHelper.initAGraph()

  //Calculate graph tree-decomposition
  val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

  "should spawn NodeSearch after receive solution" in {
    val testKit = BehaviorTestKit(TopLevel(tree_decomposition.values.toSeq))
    testKit.run(Hello.CreateChild("child"))
    testKit.expectEffect(Spawned(childActor, "child"))
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}