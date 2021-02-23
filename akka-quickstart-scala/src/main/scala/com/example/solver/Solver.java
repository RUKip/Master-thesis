package com.example.solver;

import com.example.TreeNode;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashMap;

public class Solver {

//    public String solve(TreeNode node, HashMap<Integer, TreeNode> mapping)
//    {
//        HashMap<String, Integer> color_options = new HashMap<>();
//        color_options.put("red", 1);
//        color_options.put("blue", 2);
//        color_options.put("yellow", 3);
//
//        Model model = new Model("Graph coloring problem");
//
//        //Create variable to calculate (the color of the node)
//        IntVar node_color =  model.intVar("Color_"+node.id(), color_options.values().stream().mapToInt(Integer::intValue).toArray());
//
//        //Create constraints TODO: not sure yet how to convert here scala to java
//        for (Integer id : node.children_ids()) {
//            TreeNode connected_node = mapping.get(id);
//            model.arithm(node_color, "!=", color_options.get(connected_node.color())).post();
//        }
//
//        //Solve
//        Solution solution = model.getSolver().findSolution();
//        if(solution != null){
//            System.out.println(solution.toString());
//        }
//
//        return solution.toString();
//    }
}
