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
    public class PROJGR
    {
    

        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public PROJGR()
        {
    
        }
    
        public void Run(int N, double[] L, int offset_l, double[] U, int offset_u, int[] NBD, int offset_nbd, double[] X, int offset_x, double[] G, int offset_g
                         , ref double SBGNRM)
        {

            #region Variables
            
            int I = 0; double GI = 0; 

            #endregion


            #region Array Index Correction
            
             int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_nbd = -1 + offset_nbd;  int o_x = -1 + offset_x; 
             int o_g = -1 + offset_g;

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine projgr
            // c
            // c     This subroutine computes the infinity norm of the projected
            // c       gradient.
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
            
            

            #endregion

            SBGNRM = ZERO;
            for (I = 1; I <= N; I++)
            {
                GI = G[I + o_g];
                if (NBD[I + o_nbd] != 0)
                {
                    if (GI < ZERO)
                    {
                        if (NBD[I + o_nbd] >= 2) GI = Math.Max((X[I + o_x] - U[I + o_u]), GI);
                    }
                    else
                    {
                        if (NBD[I + o_nbd] <= 2) GI = Math.Min((X[I + o_x] - L[I + o_l]), GI);
                    }
                }
                SBGNRM = Math.Max(SBGNRM, Math.Abs(GI));
            }
            
            return;
            
        }
    }
    
    // c======================= The end of projgr =============================
}
