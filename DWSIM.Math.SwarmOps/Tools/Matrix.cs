/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    public static partial class Tools
    {
        /// <summary>
        /// Allocate and return a new matrix double[dim1][dim2].
        /// </summary>
        public static double[][] NewMatrix(int dim1, int dim2)
        {
            double[][] matrix = new double[dim1][];

            for (int i = 0; i < dim1; i++)
            {
                matrix[i] = new double[dim2];
            }

            return matrix;
        }
    }
}
