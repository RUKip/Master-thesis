package com.example

import akka.actor.typed.ActorRef
import com.example.actors.Node

case class RecordedNoGoods(recorded_no_goods: Map[List[(Int, String)], Boolean], child_refs: Map[ActorRef[Node.Event], List[Int]]) {

  def recordNoGood(send_solution: Map[Int, String]): RecordedNoGoods = {
    val recording = getRecording(send_solution)
    this.copy(recorded_no_goods = recorded_no_goods + (recording -> false))
  }

  /** Gets recording, eg. the variables and there assignment that overlap with child node and the optimal solution */
  def getRecording(send_solution: Map[Int, String]): List[(Int, String)] =  {
    val intersecting_variables = child_refs.values.flatten.toList.distinct.sorted
    intersecting_variables.map { variable =>
      (variable -> send_solution(variable))
    }
  }

  def hasRecordedNoGood(solution: Map[Int, String]): Boolean = {
    val recording = getRecording(solution)
    recorded_no_goods.contains(recording)
  }

  def asBasic(): Map[List[(Int, String)], Boolean] = {
    recorded_no_goods
  }
}
