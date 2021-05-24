package com.example

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, LogCapturing}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.Node.ReceiveSolution
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
//    testKit.run(Hello.CreateChild("child"))

    val probe = testKit.createTestProbe[ReceiveSolution]()
    val toplevel = testKit.spawn(TopLevel(tree_decomposition.values.toSeq))
    probe.expectMessageType[ReceiveSolution]

    //    testKit.expectEffect(Spawned(Node(tree_decomposition.get(1).orNull), "1"))
//    testKit.expectEffect(Spawned(Node(tree_decomposition.get(2).orNull), "2"))
//    testKit.expectEffect(Spawned(Node(tree_decomposition.get(3).orNull), "3"))
//    testKit.expectEffect(Spawned(Node(tree_decomposition.get(4).orNull), "4"))
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}