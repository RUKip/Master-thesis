package com.example

import akka.actor.typed.ActorRef
import com.example.actors.Node

case class RecordedGoods(recordedGoods: Map[List[(Int, String)], (Int, Map[Int, String])], child_refs: Map[ActorRef[Node.Event], List[Int]]) {

  def recordGood(send_solution: Map[Int, String], received_solution: Map[Int, String], received_score: Int): RecordedGoods = {
    val recording = getRecording(send_solution)
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

  /** For comparing local good vs new good */
  def compareLocalGood(solution: Map[Int, String], recording: Map[Int, String], recorded_score: Int, calcScore: (Map[Int, String]) => Int): (Int, Map[Int, String], RecordedGoods) = {
    val local_solution = recording ++ solution
    val local_score = calcScore(local_solution)
    if (local_score > recorded_score) {
      (local_score, local_solution, recordGood(solution, local_solution, local_score))
    } else {
      (recorded_score, recording, this)
    }
  }

  def asBasic(): Map[List[(Int, String)], (Int, Map[Int, String])] = {
    recordedGoods
  }
}

