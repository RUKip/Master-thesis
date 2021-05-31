package com.example

import akka.actor.typed.ActorSystem
import com.example.actors.{NodeSearch, TopLevel}
import com.typesafe.config.ConfigFactory

object COPSolver extends App {

    //Init graph and tree
    val graph: Map[Int, Variable] = InitializationHelper.initAGraph()

    //Calculate graph tree-decomposition
    val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

    //Initialize based on tree decomposition (here also use cluster)

    //Startup cluster?
    //This matches with the config file
    val ports = Seq(25251, 25252)

    //TODO: Here the nodes are divided (here should be to test different cluster deployments)
    val divided_nodes = tree_decomposition.values.toSeq.grouped(
        math.ceil(tree_decomposition.values.size.doubleValue() / ports.length.doubleValue()).toInt
    )
    val cluster_divided_nodes: Seq[(Int, Seq[TreeNode])] = ports zip divided_nodes

//    cluster_divided_nodes.foreach {
//        case (port, tree_nodes) => startup(port, tree_nodes)
//    }
    startup(ports.head, tree_decomposition.values.toSeq)

  //Divide here the nodes over the cluster based on tree-decomposition
    def startup(port: Int, tree_nodes: Seq[TreeNode]): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(s"""
      akka.remote.artery.canonical.port=$port
      """).withFallback(ConfigFactory.load())

        // Create an Akka system
        val system: ActorSystem[NodeSearch.Event] = ActorSystem(TopLevel(tree_nodes), name= "COPSolver", config= config)
    }
}
