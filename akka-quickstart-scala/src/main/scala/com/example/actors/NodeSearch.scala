package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.NodeSearch.{Event, PrintGraph, SendOptimalSolution}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{Solution, TreeNode, Variable}
import com.example.solver.Solver

import scala.jdk.CollectionConverters._

//TODO: this doesnt yet send anything to SolutionNode or TopLevel
class NodeSearch (node: TreeNode) {

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

  //TODO; does this wait activly for a solution? As we also not to create the agent and need to send
  def mainLoop(context: ActorContext[NodeSearch.Event], solutions: List[Map[Int, String]], best_solution: Map[Int, String], best_score: Int, index: Int): Behavior[Event] = {
    val current_solution = solutions(index)
    val solution_id = node.id.toString + "_" + index

    val solution_actor = context.spawn(
      SolutionNode(Solution(solution_id, node, current_solution), node.child_connected, node.tree_childeren, context.self),
      solution_id
    )
    Behaviors.receive { (context, message) =>
      message match {
        case SendOptimalSolution(solution) =>
          val cost = this.calcCost(solution)
          if (cost > best_score) {
            mainLoop(context, solutions, solution, cost, index + 1)
          } else {
            mainLoop(context, solutions, best_solution, best_score, index + 1)
          }
        case PrintGraph() =>
          context.log.info(
            "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
            node.id,
            node.graph_variables,
            node.parent,
            node.tree_childeren,
            context.self.path,
          )
          Behaviors.same
      }
    }
  }

}


object NodeSearch {
  sealed trait Event extends SolutionEvent//Scalas enum
  final case class PrintGraph() extends Event
  final case class Initialize(parent_color_mapping: Map[Int, String]) extends Event
  final case class BackTrack() extends Event
  final case class SendOptimalSolution(solution: Map[Int, String]) extends Event

  def apply(node: TreeNode, parent_node: ActorRef[Node.Event], parent_solution_node: ActorRef[SolutionEvent]): Behavior[Event] = Behaviors.setup { context =>
    val node_search = new NodeSearch(node)

    val solutions = this.initializeNodes(node)
    context.log.info("Solution: {}", solutions)

    if(solutions.isEmpty) {
      parent_solution_node ! SolutionNode.SendSolution(Map())
      Behaviors.stopped
    } else {
      node_search.mainLoop(context, solutions, null, 0, 0)
    }
  }

  def initializeNodes(node: TreeNode): List[Map[Int, String]] = {
    val java_mapping: Map[Integer, Variable] = node.full_graph_mapping map {case (key, value) => (key:java.lang.Integer, value)}
    val solutions: List[Map[Int, String]]  = Solver.solve(node.getGraphNodes.asJava, java_mapping.asJava)
      .asScala
      .toList
      .map(internal_map => internal_map.asScala.toMap map {case (key, value) => (key.toInt, value) })
    solutions
  }
}
