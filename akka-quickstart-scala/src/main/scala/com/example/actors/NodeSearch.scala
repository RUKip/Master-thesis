package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.NodeSearch.{Event, PrintGraph, SendOptimalSolution}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{Solution, TreeNode}
import com.example.solver.SolverScalaWrapper

class NodeSearch (node: TreeNode, child_refs: Map[Int, ActorRef[Node.Event]], parent_solution_node: ActorRef[SolutionEvent], solutions: List[Map[Int, String]], context: ActorContext[Event]) {

  //For now lets just say we want the most amount of color 'red'
  def calcCost(color_mapping: Map[Int, String]): Int = {
    var cost: Int = 0
    color_mapping.foreach {
      case (_, color) =>
        if (color == "red") {
          cost += 1
        }
    }
    cost
  }

  def mainLoop(context: ActorContext[NodeSearch.Event], best_solution: Map[Int, String], best_score: Int, index: Int): Behavior[Event] = {
    if (solutions.size > index) {
      val current_solution = node.updateNodes(solutions(index)).full_graph_mapping
      val solution_id = node.id.toString + "_" + index

      val solution_actor = context.spawn(
        SolutionNode(Solution(solution_id, node, current_solution, 0), node.child_connected, node.tree_children, context.self, child_refs),
        solution_id
      )
      Behaviors.receive { (context, message) =>
        message match {
          case SendOptimalSolution(solution) =>
            val cost = this.calcCost(solution)
            if (cost > best_score) {
              mainLoop(context, solution, cost, index + 1)
            } else {
              mainLoop(context, best_solution, best_score, index + 1)
            }
          case PrintGraph() =>
            context.log.info(
              "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
              node.id,
              node.graph_variables,
              node.parent,
              node.tree_children,
              context.self.path,
            )
            Behaviors.same
          case _ =>
            context.log.error("Unexpected message: " + message)
            Behaviors.stopped
        }
      }
    } else {
      context.log.info("Found local best solution: {}, score: {}, stopping...", best_solution, best_score)
      parent_solution_node ! SolutionNode.SendSolution(best_solution, best_score)
      Behaviors.stopped
    }
  }

  def receiveNodeRef(): Behavior[Event] = {
    context.log.info("Node search ready to receive for node: " + node.id.toString)
    mainLoop(context, null, 0, 0)
  }
}


object NodeSearch {
  sealed trait Event extends SolutionEvent//Scalas enum
  final case class PrintGraph() extends Event
  final case class SendOptimalSolution(solution: Map[Int, String]) extends Event

  def apply(node: TreeNode, child_refs: Map[Int, ActorRef[Node.Event]], parent_node: ActorRef[Node.Event], parent_solution_node: ActorRef[SolutionEvent]): Behavior[Event] = Behaviors.setup { context =>
    val solutions = SolverScalaWrapper.calcSolutions(node)
    context.log.info("For node " + node.graph_variables + " Solution: {}", solutions)

    val node_search = new NodeSearch(node, child_refs, parent_solution_node, solutions, context)

    if(solutions.isEmpty) {
      context.log.info("No solutions, stopping")
      parent_solution_node ! SolutionNode.SendSolution(Map(), 0)
      Behaviors.stopped
    } else {
      node_search.receiveNodeRef()
    }
  }
}
