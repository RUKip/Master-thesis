package com.example.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.SolutionNode.SendSolution

class TopLevel {
}

object TopLevel {

  def apply(tree_nodes: Seq[TreeNode]): Behavior[SolutionNode.SolutionEvent] = Behaviors.setup[SolutionNode.SolutionEvent] { context =>

    context.log.info("starting toplevel actor")
    //Initialize all tree node actors
    val actors = tree_nodes map { tree_node => {
        context.log.info("Spawning actor for tree_node: " + tree_node.id.toString)
        context.spawn(Node(tree_node), tree_node.id.toString)
      }
    }

    //This part only has to run for one TopLevel in the distributed actorsystem
    if (tree_nodes.head.id == 1) {

      //Start algorithm
      actors.head ! ReceiveSolution(Map(): Map[Int, String], context.self)

      //Wait for final solution
      Behaviors.receive { (context, message) =>
        message match {
          case SendSolution(solution: Map[Int, String], score) =>
            context.log.info("Final solution is: {} {}", solution, score)

            //Terminate all still running tree nodes
            actors.head ! Terminate()

            Behaviors.stopped
          case _ =>
            context.log.error("Unexpected message: " + message)
            Behaviors.stopped
        }
      }
    } else {
      Behaviors.stopped
    }
  }
}
