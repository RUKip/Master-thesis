package com.example.solver;

import com.example.Variable;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.IntVar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solver {

    //Set of values
    public static final String[] COLOR_MAPPING = {"red", "blue", "yellow", "Blank"};

     public static List<Map<Integer, String>> solve(List<Variable> nodes, Map<Integer, Variable> mapping)
     {
         System.out.println("Solving: " + nodes + " with mapping: " + mapping);
        HashMap<String, Integer> color_options = new HashMap<>();
        for (int i = 0; i < COLOR_MAPPING.length - 1; i++) {
            color_options.put(COLOR_MAPPING[i], i);
        }

        Model model = new Model("Graph coloring problem");

         //Create variable to calculate (the color of the node)
        List<IntVar> variables = new ArrayList<IntVar>();
        for (Variable node : nodes) {
            if (node.color().equals("Blank")) {
                IntVar node_color = model.intVar("" + node.id(), color_options.values().stream().mapToInt(Integer::intValue).toArray());
                variables.add(node_color);

                //Create constraints for already set colors
                for (Integer id : node.connectedAsJava()) {
                    Variable connected_node = mapping.get(id);
                    System.out.println("ID: " + id);
                    System.out.println("Connected node: " + connected_node);

                    if ( ! connected_node.color().equals("Blank")) {
                        model.arithm(node_color, "!=", color_options.get(connected_node.color())).post();
                    }
                }
            }
        }

         model.allDifferent(variables.toArray(new IntVar[0])).post();

        //Solve
        List<Solution> solutions = model.getSolver().findAllSolutions();

         ArrayList<Map<Integer, String>> color_mapped_solutions = new ArrayList<>();
         for (Solution solution : solutions) {
             color_mapped_solutions.add(solutionToColorMapping(solution));
         }

//         //Below is debug, to see what is the solution
//        if( ! solutions.isEmpty()){
//            System.out.println(solutions.toString());
//        }

        return color_mapped_solutions;
     }


    private static Map<Integer, String> solutionToColorMapping(Solution solution) {
        HashMap<Integer, String> color_mapping = new HashMap<>();
        List<IntVar> variables = solution.retrieveIntVars(false);
        for (IntVar variable : variables) {
             String color = COLOR_MAPPING[solution.getIntVal(variable)];
             color_mapping.put(Integer.valueOf(variable.getName()), color);
         }
         return color_mapping;
    }
}
