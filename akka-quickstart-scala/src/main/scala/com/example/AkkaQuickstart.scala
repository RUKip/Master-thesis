//#full-example
package com.example

import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import com.example.actors.Node
import com.typesafe.config.ConfigFactory

object AkkaQuickstart extends App {
//  //#actor-system
//  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")
//  //#actor-system
//
//  //#main-send-messages
//  greeterMain ! SayHello("Charles")
//  //#main-send-messages

    val (tree, mapping): (TreeNode, Map[Int, TreeNode]) = TreeHelper.initATree()
    val system: ActorSystem[Node.PrintGraph] = ActorSystem(Node(tree, mapping), "AkkaQuickStart")

    system ! Node.PrintGraph()

    val ports = Seq(25251, 25252, 0)
    ports.foreach(startup)

    def startup(port: Int): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(s"""
      akka.remote.artery.canonical.port=$port
      """).withFallback(ConfigFactory.load())

        // Create an Akka system
        ActorSystem[Nothing](RootBehavior(), "ClusterSystem", config)
    }

    object RootBehavior {
        def apply(): Behavior[Nothing] = Behaviors.setup[Nothing] { context =>
            // Create an actor that handles cluster domain events
            context.spawn(ClusterListener(), "ClusterListener")

            Behaviors.empty
        }
    }
}
