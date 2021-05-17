package com.example.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.NodeSearch.SendOptimalSolution

class TopLevel {
}

object TopLevel {

  def apply(tree_nodes: Seq[TreeNode]): Behavior[NodeSearch.Event] = Behaviors.setup[NodeSearch.Event] { context =>
    //Initialize all tree node actors
    val actors = tree_nodes map { tree_node => {
      context.spawn(Node(tree_node), tree_node.id.toString)
    }
    }

    //This part only has to run for one TopLevel in the distributed actorsystem
    if (tree_nodes.head.id == 1) {

      //Start algorithm
      actors.head ! ReceiveSolution(Map(): Map[Int, String], null)

      //Wait for final solution
      Behaviors.receive { (context, message) =>
        message match {
          case SendOptimalSolution(solution: Map[Int, String]) => {
            println("Final solution is:")
            println(solution.toString())

            //Terminate all still running tree nodes
            actors.head ! Terminate()

            Behaviors.stopped
          }
        }
      }
    } else {
      Behaviors.stopped
    }
  }
}
