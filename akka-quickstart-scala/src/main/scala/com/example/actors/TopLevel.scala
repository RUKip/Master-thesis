package com.example.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.NodeSearch.SendOptimalSolution

class TopLevel {
}

object TopLevel {

  def apply(tree_nodes: Seq[TreeNode]): Behavior[Node.Event] = Behaviors.setup[Node.Event] { context =>
    //Initialize all tree node actors
    val actors = tree_nodes map { tree_node => {
       context.spawn(Node(tree_node), tree_node.id.toString)
    }}

    //Start algorithm
    actors.head ! ReceiveSolution(Map(): Map[Int, String], null)

    //Wait for final solution
    Behaviors.receive { (context, message) =>
      message match {
        case SendOptimalSolution(solution: Map[Int, String]) => {
          println("Final solution is:")
          println(solution.toString())
          Behaviors.stopped
        }
      }
    }

    //Terminate all still running tree nodes
    actors.head ! Terminate()

    Behaviors.stopped
  }
}
