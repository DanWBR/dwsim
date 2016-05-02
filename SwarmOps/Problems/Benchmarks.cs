/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;

namespace SwarmOps.Problems
{
    /// <summary>
    /// Contains list of all implemented benchmark problems.
    /// </summary>
    public static class Benchmarks
    {
        /// <summary>
        /// Enumeration of all benchmark problem IDs.
        /// </summary>
        public enum ID
        { 
            Ackley, 
            Griewank, 
            Penalized1, 
            Penalized2, 
            QuarticNoise, 
            Rastrigin, 
            Rosenbrock, 
            Schwefel12, 
            Schwefel221, 
            Schwefel222, 
            Sphere, 
            Step 
        }

        /// <summary>
        /// Array containing all benchmark problem IDs.
        /// </summary>
        public static ID[] IDs =
        {
            ID.Ackley, 
            ID.Griewank, 
            ID.Penalized1, 
            ID.Penalized2, 
            ID.QuarticNoise, 
            ID.Rastrigin, 
            ID.Rosenbrock, 
            ID.Schwefel12, 
            ID.Schwefel221, 
            ID.Schwefel222, 
            ID.Sphere, 
            ID.Step 
        };

        /// <summary>
        /// Create a new instance of a benchmark problem.
        /// </summary>
        /// <param name="id">Benchmark problem ID.</param>
        /// <param name="dimensionality">Dimensionality of problem.</param>
        /// <returns></returns>
        public static Benchmark CreateInstance(this ID id, int dimensionality, int maxIterations)
        {
            Benchmark benchmark;

            switch (id)
            {
                case ID.Ackley:
                    benchmark = new Ackley(dimensionality, maxIterations);
                    break;

                case ID.Griewank:
                    benchmark = new Griewank(dimensionality, maxIterations);
                    break;

                case ID.Penalized1:
                    benchmark = new Penalized1(dimensionality, maxIterations);
                    break;

                case ID.Penalized2:
                    benchmark = new Penalized2(dimensionality, maxIterations);
                    break;

                case ID.QuarticNoise:
                    benchmark = new QuarticNoise(dimensionality, maxIterations);
                    break;

                case ID.Rastrigin:
                    benchmark = new Rastrigin(dimensionality, maxIterations);
                    break;

                case ID.Rosenbrock:
                    benchmark = new Rosenbrock(dimensionality, maxIterations);
                    break;

                case ID.Schwefel12:
                    benchmark = new Schwefel12(dimensionality, maxIterations);
                    break;

                case ID.Schwefel221:
                    benchmark = new Schwefel221(dimensionality, maxIterations);
                    break;

                case ID.Schwefel222:
                    benchmark = new Schwefel222(dimensionality, maxIterations);
                    break;

                case ID.Sphere:
                    benchmark = new Sphere(dimensionality, maxIterations);
                    break;

                case ID.Step:
                    benchmark = new Step(dimensionality, maxIterations);
                    break;

                default:
                    throw new ArgumentException();
            }

            return benchmark;
        }
    }
}
