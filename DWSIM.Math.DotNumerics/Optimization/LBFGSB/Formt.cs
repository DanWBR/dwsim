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
    public class FORMT
    {
    

        #region Dependencies
        
        DPOFA _dpofa; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; 

        #endregion

        public FORMT(DPOFA dpofa)
        {
    

            #region Set Dependencies
            
            this._dpofa = dpofa; 

            #endregion

        }
    
        public FORMT()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DPOFA dpofa = new DPOFA(ddot);

            #endregion


            #region Set Dependencies
            
            this._dpofa = dpofa; 

            #endregion

        }
        public void Run(int M, ref double[] WT, int offset_wt, double[] SY, int offset_sy, double[] SS, int offset_ss, int COL, double THETA
                         , ref int INFO)
        {

            #region Variables
            
            int I = 0; int J = 0; int K = 0; int K1 = 0; double DDUM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_wt = -1 - M + offset_wt;  int o_sy = -1 - M + offset_sy;  int o_ss = -1 - M + offset_ss; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine formt
            // c
            // c       This subroutine forms the upper half of the pos. def. and symm.
            // c         T = theta*SS + L*D^(-1)*L', stores T in the upper triangle
            // c         of the array wt, and performs the Cholesky factorization of T
            // c         to produce J*J', with J' stored in the upper triangle of wt.
            // c
            // c     Subprograms called:
            // c
            // c       Linpack ... dpofa.
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
            
            
            
            
            
            // c     Form the upper half of  T = theta*SS + L*D^(-1)*L',
            // c        store T in the upper triangle of the array wt.
            

            #endregion


            #region Body
            
            for (J = 1; J <= COL; J++)
            {
                WT[1+J * M + o_wt] = THETA * SS[1+J * M + o_ss];
            }
            for (I = 2; I <= COL; I++)
            {
                for (J = I; J <= COL; J++)
                {
                    K1 = Math.Min(I, J) - 1;
                    DDUM = ZERO;
                    for (K = 1; K <= K1; K++)
                    {
                        DDUM += SY[I+K * M + o_sy] * SY[J+K * M + o_sy] / SY[K+K * M + o_sy];
                    }
                    WT[I+J * M + o_wt] = DDUM + THETA * SS[I+J * M + o_ss];
                }
            }
            
            // c     Cholesky factorize T to J*J' with 
            // c        J' stored in the upper triangle of wt.
            
            this._dpofa.Run(ref WT, offset_wt, M, COL, ref INFO);
            if (INFO != 0)
            {
                INFO =  - 3;
            }
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of formt ==============================
}
