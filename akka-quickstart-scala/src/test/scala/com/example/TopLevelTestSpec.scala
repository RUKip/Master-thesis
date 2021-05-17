package com.example

import akka.actor.testkit.typed.Effect.Spawned
import akka.actor.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, LogCapturing}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.TopLevel
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TopLevelTestSpec
  extends AnyWordSpec
    with BeforeAndAfterAll
    with LogCapturing
    with Matchers {
  val testKit = ActorTestKit()

  val childActor: Behaviors.Receive[String] = Behaviors.receiveMessage[String] { _ =>
    Behaviors.same[String]
  }

  //Init graph and tree
  val graph: Map[Int, Variable] = InitializationHelper.initAGraph()

  //Calculate graph tree-decomposition
  val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

  "should spawn NodeSearch after receive solution" in {
    val testKit = BehaviorTestKit(TopLevel(tree_decomposition.values.toSeq))
//    testKit.run(Hello.CreateChild("child"))
    testKit.expectEffect(Spawned(childActor, "1"))
    testKit.expectEffect(Spawned(childActor, "2"))
    testKit.expectEffect(Spawned(childActor, "3"))
    testKit.expectEffect(Spawned(childActor, "4"))
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}