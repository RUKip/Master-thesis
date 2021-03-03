package com.example

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import com.example.actors.Node
import com.typesafe.config.ConfigFactory

object COPSolver extends App {

    //Init graph and tree
    val graph: Map[Int, GraphNode] = InitializationHelper.initAGraph()

    //Calculate graph tree-decomposition
    val (root_node, mapping): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD()

    //Initialize based on tree decmposition (here also use cluster)

    //Startup cluster?
    //This matches with the config file
    val ports = Seq(25251, 25252, 0)
    ports.foreach(startup) //TODO: here split up graph over cluster and start cluster and init


  //Divide here the nodes over the cluster based on tree-decomposition
    def startup(port: Int, graph: Map[Int, GraphNode]): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(s"""
      akka.remote.artery.canonical.port=$port
      """).withFallback(ConfigFactory.load())

        // Create an Akka system
        val system: ActorSystem[Node.PrintGraph] = ActorSystem(RootBehavior(graph), name= "COPSolver", config = config)

      //TODO: here should start the algorithm of init + smart backtracking
    }

    object RootBehavior {
        def apply(graph: Map[Int, GraphNode]): Behavior[Node.PrintGraph] = Behaviors.setup[Node.PrintGraph] { context =>
            // Create an actor that handles cluster domain events
            for ((id, node) <- graph) {
              val actor = context.spawn(Node(node, graph), id.toString)
              actor ! Node.PrintGraph()
            }

            Behaviors.empty
        }
    }
}
