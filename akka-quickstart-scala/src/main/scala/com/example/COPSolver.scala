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

//    //TODO: Here the nodes are divided (here should be to test different cluster deployments)
//    val divided_nodes = tree_decomposition.values.toSeq.grouped(
//        math.ceil(tree_decomposition.values.size.doubleValue() / ports.length.doubleValue()).toInt
//    )
//    val cluster_divided_nodes: Seq[(Int, Seq[TreeNode])] = ports zip divided_nodes

    val master_port = ports.head
    startup("master", master_port, tree_decomposition)
    ports.tail.foreach(port => startup("worker", port, tree_decomposition))

//    startup(ports.head, tree_decomposition.values.toSeq)

  //Divide here the nodes over the cluster based on tree-decomposition
    def startup(role: String, port: Int, tree_nodes: Map[Int, TreeNode]): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(s"""
      akka.remote.artery.canonical.port=$port
      akka.cluster.roles = [$role]
      """).withFallback(ConfigFactory.load())

        // Create an Akka system
        val system: ActorSystem[NodeSearch.Event] = ActorSystem(TopLevel(tree_nodes, ports.size), name= "COPSolver", config= config)
    }
}
