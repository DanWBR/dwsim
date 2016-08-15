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
    public class BMV
    {
    

        #region Dependencies
        
        DTRSL _dtrsl; 

        #endregion

        public BMV(DTRSL dtrsl)
        {
    

            #region Set Dependencies
            
            this._dtrsl = dtrsl; 

            #endregion

        }
    
        public BMV()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DTRSL dtrsl = new DTRSL(ddot, daxpy);

            #endregion


            #region Set Dependencies
            
            this._dtrsl = dtrsl; 

            #endregion

        }
        /// <param name="M">
        /// is an integer variable.
        /// On entry m is the maximum number of variable metric corrections
        /// used to define the limited memory matrix.
        /// On exit m is unchanged.
        ///</param>
        /// <param name="SY">
        /// is a double precision array of dimension m x m.
        /// On entry sy specifies the matrix S'Y.
        /// On exit sy is unchanged.
        ///</param>
        /// <param name="WT">
        /// is a double precision array of dimension m x m.
        /// On entry wt specifies the upper triangular matrix J' which is 
        /// the Cholesky factor of (thetaS'S+LD^(-1)L').
        /// On exit wt is unchanged.
        ///</param>
        /// <param name="COL">
        /// is an integer variable.
        /// On entry col specifies the number of s-vectors (or y-vectors)
        /// stored in the compact L-BFGS formula.
        /// On exit col is unchanged.
        ///</param>
        /// <param name="V">
        /// is a double precision array of dimension 2col.
        /// On entry v specifies vector v.
        /// On exit v is unchanged.
        ///</param>
        /// <param name="P">
        /// is a double precision array of dimension 2col.
        /// On entry p is unspecified.
        /// On exit p is the product Mv.
        ///</param>
        /// <param name="INFO">
        /// is an integer variable.
        /// On entry info is unspecified.
        /// On exit info = 0       for normal return,
        /// = nonzero for abnormal return when the system
        /// to be solved by dtrsl is singular.
        ///</param>
        public void Run(int M, double[] SY, int offset_sy, double[] WT, int offset_wt, int COL, double[] V, int offset_v, ref double[] P, int offset_p
                         , ref int INFO)
        {

            #region Variables
            
            int I = 0; int K = 0; int I2 = 0; double SUM = 0; 

            #endregion


            #region Implicit Variables
            
            int SY_I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_sy = -1 - M + offset_sy;  int o_wt = -1 - M + offset_wt;  int o_v = -1 + offset_v;  int o_p = -1 + offset_p; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine bmv
            // c
            // c     This subroutine computes the product of the 2m x 2m middle matrix 
            // c	in the compact L-BFGS formula of B and a 2m vector v;  
            // c	it returns the product in p.
            // c	
            // c     m is an integer variable.
            // c       On entry m is the maximum number of variable metric corrections
            // c         used to define the limited memory matrix.
            // c       On exit m is unchanged.
            // c
            // c     sy is a double precision array of dimension m x m.
            // c       On entry sy specifies the matrix S'Y.
            // c       On exit sy is unchanged.
            // c
            // c     wt is a double precision array of dimension m x m.
            // c       On entry wt specifies the upper triangular matrix J' which is 
            // c         the Cholesky factor of (thetaS'S+LD^(-1)L').
            // c       On exit wt is unchanged.
            // c
            // c     col is an integer variable.
            // c       On entry col specifies the number of s-vectors (or y-vectors)
            // c         stored in the compact L-BFGS formula.
            // c       On exit col is unchanged.
            // c
            // c     v is a double precision array of dimension 2col.
            // c       On entry v specifies vector v.
            // c       On exit v is unchanged.
            // c
            // c     p is a double precision array of dimension 2col.
            // c       On entry p is unspecified.
            // c       On exit p is the product Mv.
            // c
            // c     info is an integer variable.
            // c       On entry info is unspecified.
            // c       On exit info = 0       for normal return,
            // c                    = nonzero for abnormal return when the system
            // c                                to be solved by dtrsl is singular.
            // c
            // c     Subprograms called:
            // c
            // c       Linpack ... dtrsl.
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
            
            if (COL == 0) return;
            
            // c     PART I: solve [  D^(1/2)      O ] [ p1 ] = [ v1 ]
            // c                   [ -L*D^(-1/2)   J ] [ p2 ]   [ v2 ].
            
            // c 	solve Jp2=v2+LD^(-1)v1.
            P[COL + 1 + o_p] = V[COL + 1 + o_v];
            for (I = 2; I <= COL; I++)
            {
                I2 = COL + I;
                SUM = 0.0E0;
                for (K = 1; K <= I - 1; K++)
                {
                    SUM += SY[I+K * M + o_sy] * V[K + o_v] / SY[K+K * M + o_sy];
                }
                P[I2 + o_p] = V[I2 + o_v] + SUM;
            }
            // c     Solve the triangular system
            this._dtrsl.Run(WT, offset_wt, M, COL, ref P, COL + 1 + o_p, 11, ref INFO);
            if (INFO != 0) return;
            
            // c     	solve D^(1/2)p1=v1.
            for (I = 1; I <= COL; I++)
            {
                P[I + o_p] = V[I + o_v] / Math.Sqrt(SY[I+I * M + o_sy]);
            }
            
            // c     PART II: solve [ -D^(1/2)   D^(-1/2)*L'  ] [ p1 ] = [ p1 ]
            // c                    [  0         J'           ] [ p2 ]   [ p2 ]. 
            
            // c       solve J^Tp2=p2. 
            this._dtrsl.Run(WT, offset_wt, M, COL, ref P, COL + 1 + o_p, 01, ref INFO);
            if (INFO != 0) return;
            
            // c       compute p1=-D^(-1/2)(p1-D^(-1/2)L'p2)
            // c                 =-D^(-1/2)p1+D^(-1)L'p2.  
            for (I = 1; I <= COL; I++)
            {
                P[I + o_p] =  - P[I + o_p] / Math.Sqrt(SY[I+I * M + o_sy]);
            }
            for (I = 1; I <= COL; I++)
            {
                SUM = 0.0E0;
                SY_I = I * M + o_sy;
                for (K = I + 1; K <= COL; K++)
                {
                    SUM += SY[K + SY_I] * P[COL + K + o_p] / SY[I+I * M + o_sy];
                }
                P[I + o_p] += SUM;
            }
            
            return;
            

            #endregion

        }
    }
    
    // c======================== The end of bmv ===============================
}
