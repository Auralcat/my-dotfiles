/* Template for Project Euler problems.
 * Problem header goes here.*/

import eulerlib;

public class Euler<num_goes_here> extends ProjectEulerProblem{

    public void compute(){

    }

    public static void main(String[] args){
        Euler<num_goes_here> p = new Euler<num_goes_here>();
        EulerLib.benchmark(p.compute());
    }
}
