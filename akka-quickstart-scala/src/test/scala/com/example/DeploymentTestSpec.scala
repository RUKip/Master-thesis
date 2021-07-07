package com.example

import akka.actor.testkit.typed.scaladsl.{LogCapturing, TestInbox}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.deployment.RandomDeployment
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeploymentTestSpec extends AnyWordSpec
  with BeforeAndAfterAll
  with LogCapturing
  with Matchers {

  "should return random deployment" in {
    val graph = InitializationHelper.initAGraph()
    val (root, nodes) = InitializationHelper.getHTD(graph)
    println(nodes.keys)
    val result = RandomDeployment().deploy(nodes, Set(TestInbox[SolutionEvent]().ref, TestInbox[SolutionEvent]().ref))
    result.foreach { case (k,v) =>
      println("Node " + k + " has to be deployed on: " + v)
    }
  }
}
