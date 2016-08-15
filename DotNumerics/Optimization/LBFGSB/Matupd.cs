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
    public class MATUPD
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DDOT _ddot; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; 

        #endregion

        public MATUPD(DCOPY dcopy, DDOT ddot)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._ddot = ddot; 

            #endregion

        }
    
        public MATUPD()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            DDOT ddot = new DDOT();

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._ddot = ddot; 

            #endregion

        }
        public void Run(int N, int M, ref double[] WS, int offset_ws, ref double[] WY, int offset_wy, ref double[] SY, int offset_sy, ref double[] SS, int offset_ss
                         , double[] D, int offset_d, double[] R, int offset_r, ref int ITAIL, int IUPDAT, ref int COL, ref int HEAD
                         , ref double THETA, double RR, double DR, double STP, double DTD)
        {

            #region Variables
            
            int J = 0; int POINTR = 0; double DDOT = 0; 

            #endregion


            #region Implicit Variables
            
            int SS_COL = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ws = -1 - N + offset_ws;  int o_wy = -1 - N + offset_wy;  int o_sy = -1 - M + offset_sy; 
             int o_ss = -1 - M + offset_ss; int o_d = -1 + offset_d;  int o_r = -1 + offset_r; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine matupd
            // c
            // c       This subroutine updates matrices WS and WY, and forms the
            // c         middle matrix in B.
            // c
            // c     Subprograms called:
            // c
            // c       Linpack ... dcopy, ddot.
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
            
            
            
            
            // c     Set pointers for matrices WS and WY.
            

            #endregion


            #region Body
            
            if (IUPDAT <= M)
            {
                COL = IUPDAT;
                ITAIL = FortranLib.Mod(HEAD + IUPDAT - 2,M) + 1;
            }
            else
            {
                ITAIL = FortranLib.Mod(ITAIL,M) + 1;
                HEAD = FortranLib.Mod(HEAD,M) + 1;
            }
            
            // c     Update matrices WS and WY.
            
            this._dcopy.Run(N, D, offset_d, 1, ref WS, 1+ITAIL * N + o_ws, 1);
            this._dcopy.Run(N, R, offset_r, 1, ref WY, 1+ITAIL * N + o_wy, 1);
            
            // c     Set theta=yy/ys.
            
            THETA = RR / DR;
            
            // c     Form the middle matrix in B.
            
            // c        update the upper triangle of SS,
            // c                                         and the lower triangle of SY:
            if (IUPDAT > M)
            {
                // c                              move old information
                for (J = 1; J <= COL - 1; J++)
                {
                    this._dcopy.Run(J, SS, 2+(J + 1) * M + o_ss, 1, ref SS, 1+J * M + o_ss, 1);
                    this._dcopy.Run(COL - J, SY, J + 1+(J + 1) * M + o_sy, 1, ref SY, J+J * M + o_sy, 1);
                }
            }
            // c        add new information: the last row of SY
            // c                                             and the last column of SS:
            POINTR = HEAD;
            SS_COL = COL * M + o_ss;
            for (J = 1; J <= COL - 1; J++)
            {
                SY[COL+J * M + o_sy] = this._ddot.Run(N, D, offset_d, 1, WY, 1+POINTR * N + o_wy, 1);
                SS[J + SS_COL] = this._ddot.Run(N, WS, 1+POINTR * N + o_ws, 1, D, offset_d, 1);
                POINTR = FortranLib.Mod(POINTR,M) + 1;
            }
            if (STP == ONE)
            {
                SS[COL+COL * M + o_ss] = DTD;
            }
            else
            {
                SS[COL+COL * M + o_ss] = STP * STP * DTD;
            }
            SY[COL+COL * M + o_sy] = DR;
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of matupd =============================
}
