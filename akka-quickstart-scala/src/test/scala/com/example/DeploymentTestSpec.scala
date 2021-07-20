package com.example

import akka.actor.testkit.typed.scaladsl.{LogCapturing, TestInbox}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.deployment.{BranchDeployment, RandomDeployment, WeightDeployment}
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

  "should return branch deployment" in {
    val base = InitializationHelper.loadTree("generated_trees/8nodes_B5_W7_D3_C2.json")
    val tree_decomposition = InitializationHelper.createUsableTree(base)
//    val graph = InitializationHelper.initAGraph()
//    val (root, tree_decomposition) = InitializationHelper.getHTD(graph)
    println(tree_decomposition.keys + ", size: " + tree_decomposition.keys.size)
    val result = BranchDeployment().deploy(tree_decomposition, Set(TestInbox[SolutionEvent]().ref, TestInbox[SolutionEvent]().ref))
    println("Size of result: " + result.size)
    result.foreach { case (k,v) =>
      println("Node " + k + " has to be deployed on: " + v)
    }
  }

  "should return weight deployment" in {
    val base = InitializationHelper.loadTree("generated_trees/8nodes_B5_W7_D3_C2.json")
    val tree_decomposition = InitializationHelper.createUsableTree(base)
//    val graph = InitializationHelper.initAGraph()
//    val (root, tree_decomposition) = InitializationHelper.getHTD(graph)
    println(tree_decomposition.keys + ", size: " + tree_decomposition.keys.size)
    val result = WeightDeployment().deploy(tree_decomposition, Set(TestInbox[SolutionEvent]().ref, TestInbox[SolutionEvent]().ref))
    result.foreach { case (k,v) =>
      println("Node " + k + " has to be deployed on: " + v)
    }
  }
}
