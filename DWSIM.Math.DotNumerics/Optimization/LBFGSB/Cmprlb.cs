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
    public class CMPRLB
    {
    

        #region Dependencies
        
        BMV _bmv; 

        #endregion

        public CMPRLB(BMV bmv)
        {
    

            #region Set Dependencies
            
            this._bmv = bmv; 

            #endregion

        }
    
        public CMPRLB()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DTRSL dtrsl = new DTRSL(ddot, daxpy);
            BMV bmv = new BMV(dtrsl);

            #endregion


            #region Set Dependencies
            
            this._bmv = bmv; 

            #endregion

        }
        public void Run(int N, int M, double[] X, int offset_x, double[] G, int offset_g, double[] WS, int offset_ws, double[] WY, int offset_wy
                         , double[] SY, int offset_sy, double[] WT, int offset_wt, double[] Z, int offset_z, ref double[] R, int offset_r, ref double[] WA, int offset_wa, int[] INDEX, int offset_index
                         , double THETA, int COL, int HEAD, int NFREE, bool CNSTND, ref int INFO)
        {

            #region Variables
            
            int I = 0; int J = 0; int K = 0; int POINTR = 0; double A1 = 0; double A2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_g = -1 + offset_g;  int o_ws = -1 - N + offset_ws;  int o_wy = -1 - N + offset_wy; 
             int o_sy = -1 - M + offset_sy; int o_wt = -1 - M + offset_wt;  int o_z = -1 + offset_z;  int o_r = -1 + offset_r; 
             int o_wa = -1 + offset_wa; int o_index = -1 + offset_index; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine cmprlb 
            // c
            // c       This subroutine computes r=-Z'B(xcp-xk)-Z'g by using 
            // c         wa(2m+1)=W'(xcp-x) from subroutine cauchy.
            // c
            // c     Subprograms called:
            // c
            // c       L-BFGS-B Library ... bmv.
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


            #region Body
            
            if (!CNSTND && COL > 0)
            {
                for (I = 1; I <= N; I++)
                {
                    R[I + o_r] =  - G[I + o_g];
                }
            }
            else
            {
                for (I = 1; I <= NFREE; I++)
                {
                    K = INDEX[I + o_index];
                    R[I + o_r] =  - THETA * (Z[K + o_z] - X[K + o_x]) - G[K + o_g];
                }
                this._bmv.Run(M, SY, offset_sy, WT, offset_wt, COL, WA, 2 * M + 1 + o_wa, ref WA, 1 + o_wa
                              , ref INFO);
                if (INFO != 0)
                {
                    INFO =  - 8;
                    return;
                }
                POINTR = HEAD;
                for (J = 1; J <= COL; J++)
                {
                    A1 = WA[J + o_wa];
                    A2 = THETA * WA[COL + J + o_wa];
                    for (I = 1; I <= NFREE; I++)
                    {
                        K = INDEX[I + o_index];
                        R[I + o_r] += WY[K+POINTR * N + o_wy] * A1 + WS[K+POINTR * N + o_ws] * A2;
                    }
                    POINTR = FortranLib.Mod(POINTR,M) + 1;
                }
            }
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of cmprlb =============================
}
