package com.example.solver;

import com.example.GraphNode;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.IntVar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solver {

     public static String solve(List<GraphNode> nodes, Map<Integer, GraphNode> mapping)
     {
        HashMap<String, Integer> color_options = new HashMap<>();
        color_options.put("red", 1);
        color_options.put("blue", 2);
        color_options.put("yellow", 3);
        color_options.put("Blank", 4);

        Model model = new Model("Graph coloring problem");

         //Create variable to calculate (the color of the node)
        List<IntVar> variables = new ArrayList<IntVar>();
        for (GraphNode node : nodes) {
            if (node.color().equals("Blank")) {
                IntVar node_color = model.intVar("Color_" + node.id(), color_options.values().stream().mapToInt(Integer::intValue).toArray());
                variables.add(node_color);

                //Create constraints for already set colors
                for (Integer id : node.connectedAsJava()) {
                    GraphNode connected_node = mapping.get(id);
                    System.out.println("ID: " + id);
                    System.out.println("Connected node: " + connected_node);

                    model.arithm(node_color, "!=", color_options.get(connected_node.color())).post();
                }
            }
        }

         model.allDifferent(variables.toArray(new IntVar[0])).post();

        //Solve
        Solution solution = model.getSolver().findSolution();
        if(solution != null){
            System.out.println(solution.toString());
        }

        return solution.toString();
     }
}
