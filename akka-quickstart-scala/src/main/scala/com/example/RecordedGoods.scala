package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import com.example.actors.Node
import com.example.actors.NodeSearch.Event

case class RecordedGoods(recordedGoods: Map[List[(Int, String)], (Int, Map[Int, String])], child_refs: Map[ActorRef[Node.Event], List[Int]]) {

  def recordGood(received_solution: Map[Int, String], received_score: Int, context: ActorContext[Event]): RecordedGoods = {
    val recording = getRecording(received_solution)
    this.copy(recordedGoods = recordedGoods + (recording -> (received_score, received_solution)))
  }

  /** Gets recording, eg. the variables and there assignment that overlap with child node and the optimal solution */
  def getRecording(send_solution: Map[Int, String]): List[(Int, String)] =  {
    val intersecting_variables = child_refs.values.flatten.toList.distinct.sorted
    intersecting_variables.map { variable =>
      (variable -> send_solution(variable))
    }
  }

  /** returns -1 if not found else the score */
  def hasRecordedGood(solution: Map[Int, String]): (Int, Map[Int, String]) = {
    val recording = getRecording(solution)
    recordedGoods.getOrElse(recording, (-1, Map()))
  }

//
//  /** For comparing local good vs new good */
//  def compareLocalGood(new_solution: Map[Int, String], recording: Map[Int, String], recorded_score: Int, calcScore: (Map[Int, String]) => Int, context: ActorContext[Event]): (Int, Map[Int, String], RecordedGoods) = {
//    val local_score = calcScore(new_solution)
//    context.log.info("New solution: " + new_solution + ", score: " + local_score + "| vs | " + recorded_score + " and recording: " + recording)
//
//    if (local_score > recorded_score) {
//      (local_score, new_solution, recordGood(new_solution, local_score, context))
//    } else {
//      (recorded_score, recording, this)
//    }
//  }

  def asBasic(): Map[List[(Int, String)], (Int, Map[Int, String])] = {
    recordedGoods
  }
}

