package com.example

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, LogCapturing}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.Node
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeTestSpec
  extends AnyWordSpec
    with BeforeAndAfterAll
    with LogCapturing
    with Matchers {
  val testKit = ActorTestKit()

  "should spawn NodeSearch after receive solution" in {
//    val node = testKit.spawn(Node())
//    val probe = testKit.createTestProbe[Echo.Pong]()
//    node ! Node.ReceiveSolution.Ping("hello", probe.ref)
//    probe.expectMessage(Echo.Pong("hello"))
//
//    val mockedBehavior = Behaviors.receiveMessage[Event] { msg =>
//      msg.replyTo ! Success(msg.i)
//      Behaviors.same
//    }
//    val probe = testKit.createTestProbe[Message]()
//    val mockedPublisher = testKit.spawn(Behaviors.monitor(probe.ref, mockedBehavior))
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}