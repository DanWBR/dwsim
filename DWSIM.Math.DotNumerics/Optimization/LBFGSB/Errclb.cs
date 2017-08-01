#region Translated by Jose Antonio De Santiago-Castillo.

//Translated by Jose Antonio De Santiago-Castillo.
//E-mail:JAntonioDeSantiago@gmail.com
//Website: www.DotNumerics.com
//
//Fortran to C# Translation.
//Translated by:
//F2CSharp Version 0.72 (Dicember 7, 2009)
//Code Optimizations: , assignment operator, for-loop: array indexes
//
#endregion

using System;
using DotNumerics.FortranLibrary;

namespace DotNumerics.Optimization.LBFGSB
{
    public class ERRCLB
    {
    

        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public ERRCLB()
        {
    
        }
    
        public void Run(int N, int M, double FACTR, double[] L, int offset_l, double[] U, int offset_u, int[] NBD, int offset_nbd
                         , ref BFGSTask TASK, ref int INFO, ref int K)
        {

            #region Variables
            
            int I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_nbd = -1 + offset_nbd; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine errclb
            // c
            // c     This subroutine checks the validity of the input data.
            // c
            // c
            // c                           *  *  *
            // c
            // c     NEOS, November 1994. (Latest revision June 1996.)
            // c     Optimization Technology Center.
            // c     Argonne National Laboratory and Northwestern University.
            // c     Written by
            // c                        Ciyou Zhu
            // c     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
            // c
            // c
            // c     ************
            
            
            // c     Check the input arguments for errors.
            

            #endregion


            #region Body

             if (N <= 0) TASK = BFGSTask.ERROR;
             if (M <= 0) TASK = BFGSTask.ERROR;
             if (FACTR < ZERO) TASK = BFGSTask.ERROR;
            
            // c     Check the validity of the arrays nbd(i), u(i), and l(i).
            
            for (I = 1; I <= N; I++)
            {
                if (NBD[I + o_nbd] < 0 || NBD[I + o_nbd] > 3)
                {
                    // c                                                   return
                    //"ERROR: INVALID NBD";
                    TASK = BFGSTask.ERROR;
                    INFO =  - 6;
                    K = I;
                }
                if (NBD[I + o_nbd] == 2)
                {
                    if (L[I + o_l] > U[I + o_u])
                    {
                        // c                                    return
                        //"ERROR: NO FEASIBLE SOLUTION"
                        TASK = BFGSTask.ERROR;
                        INFO =  - 7;
                        K = I;
                    }
                }
            }
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of errclb =============================
}
