package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.NodeSearch.{Event, PrintGraph, SendOptimalSolution}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{RecordedGoods, RecordedNoGoods, Solution, TreeNode}
import com.example.solver.SolverScalaWrapper
import com.fasterxml.jackson.databind.annotation.JsonDeserialize

class NodeSearch (node: TreeNode, child_refs: Map[ActorRef[Node.Event], List[Int]], parent_solution_node: ActorRef[SolutionEvent], context: ActorContext[Event], parent_node: ActorRef[Node.Event]) {

  val COLOR_SCORE_MAPPING = Map(
    ("red" -> 5),
    ("blue" -> 4),
    ("yellow" -> 1),
    ("green" -> 2),
    ("white" -> 0),
    ("black" -> 0),
    ("orange" -> 2)
  )

  /** Calculate the score of the local solution */
  def calcScore(color_mapping: Map[Int, String]): Int = {
    var score: Int = 0
    color_mapping.foreach {
      case (_, color) =>
        score += COLOR_SCORE_MAPPING.getOrElse(color, 0)
    }
    score
  }

  def mainLoop(context: ActorContext[NodeSearch.Event], best_solution: Option[Map[Int, String]], best_score: Int, solutions: List[Map[Int, String]], recorded_goods: RecordedGoods, recorded_no_goods: RecordedNoGoods): Behavior[Event] = {
    if (solutions.nonEmpty) {
      val solution: Map[Int, String] = solutions.head
      val new_node: TreeNode = node.updateNodes(solution)
      val applicable_solution = new_node.graph_variables.map(id =>(id -> new_node.full_graph_mapping(id))).toMap
      val solution_id = node.id.toString + "_" + solutions.size

      if (recorded_no_goods.hasRecordedNoGood(solution)) {
        //Solution is in recorded no good, so skip
        //context.log.info("No good found for: {}", solution)
        mainLoop(context, best_solution, best_score, solutions.tail, recorded_goods, recorded_no_goods)
      } else {
        val (recorded_score, recording_solution): (Int, Map[Int, String]) = recorded_goods.hasRecordedGood(solution)
        if (recorded_score >= 0) {
          if (best_score >= recorded_score) {
            mainLoop(context, best_solution, best_score, solutions.tail, recorded_goods, recorded_no_goods)
          } else {
            mainLoop(context, Option(recording_solution ++ solution), recorded_score, solutions.tail, recorded_goods, recorded_no_goods)
          }
        } else {
          val solution_actor = context.spawn(
            SolutionNode(Solution(solution_id, new_node, applicable_solution, 0), new_node.tree_children, context.self, child_refs),
            solution_id
          )
          Behaviors.receive { (context, message) =>
            message match {
              case SendOptimalSolution(received_solution: Option[Map[Int, String]]) =>
                val new_solution: Map[Int, String] = if (received_solution.isEmpty) Map() else received_solution.get
                val score = this.calcScore(new_solution)

                val new_recorded_goods = if (received_solution.isEmpty) recorded_goods else recorded_goods.recordGood(solution, new_solution, score)
                val new_recorded_no_goods = if (received_solution.isEmpty) recorded_no_goods.recordNoGood(solution) else recorded_no_goods

                if (score >= best_score) {
                  mainLoop(context, Option(new_solution), score, solutions.tail, new_recorded_goods, new_recorded_no_goods)
                } else {
                  mainLoop(context, best_solution, best_score, solutions.tail, new_recorded_goods, new_recorded_no_goods)
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
        }
      }
    } else {
      //context.log.info("Found local best solution: {}, score: {}, stopping...", best_solution, best_score)
      parent_node ! Node.SendRecording(recorded_goods.asBasic(), recorded_no_goods.asBasic())
      if (best_solution.isEmpty) {
        parent_solution_node ! SolutionNode.SendSolution(Map(), 0)
      } else {
        parent_solution_node ! SolutionNode.SendSolution(best_solution.get, best_score)
      }
      Behaviors.stopped
    }
  }

  def receiveNodeRef(solutions: List[Map[Int, String]],  recorded_goods: RecordedGoods, recorded_no_goods: RecordedNoGoods): Behavior[Event] = {
    //context.log.info("Node search ready to receive for node: " + node.id.toString)
    mainLoop(context, None, 0, solutions, recorded_goods, recorded_no_goods)
  }

}

object NodeSearch {
  sealed trait Event extends SolutionEvent
  final case class PrintGraph() extends Event
  final case class SendOptimalSolution(@JsonDeserialize(keyAs = classOf[Int]) solution: Option[Map[Int, String]]) extends Event

  def apply(node: TreeNode, child_refs: Map[ActorRef[Node.Event], List[Int]], parent_node: ActorRef[Node.Event], parent_solution_node: ActorRef[SolutionEvent],  recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])], recorded_no_goods: Map[List[(Int, String)], Boolean]): Behavior[Event] = Behaviors.setup { context =>
    val solutions = SolverScalaWrapper.calcSolutions(node)
    //context.log.info("For variables " + node.graph_variables + " Solution: {}", solutions)

    val node_search = new NodeSearch(node, child_refs, parent_solution_node, context, parent_node)

    if(solutions.isEmpty) {
      //context.log.info("No solutions, stopping")
      parent_solution_node ! SolutionNode.SendSolution(Map(), 0)
      Behaviors.stopped
    } else {
      node_search.receiveNodeRef(solutions, RecordedGoods(recorded_goods, child_refs), RecordedNoGoods(recorded_no_goods, child_refs))
    }
  }
}
