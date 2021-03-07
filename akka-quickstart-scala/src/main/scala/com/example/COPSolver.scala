package com.example

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import com.example.actors.Node
import com.typesafe.config.ConfigFactory

object COPSolver extends App {

    //Init graph and tree
    val graph: Map[Int, GraphNode] = InitializationHelper.initAGraph()

    //Calculate graph tree-decomposition
    val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

    //Initialize based on tree decomposition (here also use cluster)



    //Startup cluster?
    //This matches with the config file
    val ports = Seq(25251, 25252, 0)
    val divided_nodes = tree_decomposition.values.toSeq.grouped(
        math.ceil(tree_decomposition.values.size.doubleValue() / ports.length.doubleValue()).toInt
    )
    val cluster_divided_nodes: Seq[(Int, Seq[TreeNode])] = ports zip divided_nodes

    cluster_divided_nodes.foreach {
        case (port, tree_nodes) => startup(port, tree_nodes)
    }

    //TODO: here split up graph over cluster and start cluster and init


  //Divide here the nodes over the cluster based on tree-decomposition
    def startup(port: Int, tree_nodes: Seq[TreeNode]): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(s"""
      akka.remote.artery.canonical.port=$port
      """).withFallback(ConfigFactory.load())

        // Create an Akka system
        val system: ActorSystem[Node.PrintGraph] = ActorSystem(RootBehavior(tree_nodes), name= "COPSolver", config = config)

      //TODO: here should start the algorithm of init + smart backtracking
    }

    object RootBehavior {
        def apply(tree_nodes: Seq[TreeNode]): Behavior[Node.PrintGraph] = Behaviors.setup[Node.PrintGraph] { context =>
            // Create an actor that handles cluster domain events
            tree_nodes.foreach(tree_node => {
                    val actor = context.spawn(Node(tree_node), tree_node.id.toString)
                    actor ! Node.PrintGraph()
                }
            )

            Behaviors.empty
        }
    }
}
