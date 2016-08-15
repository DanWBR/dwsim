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
    public class LNSRLB
    {
    

        #region Dependencies
        
        DTRSL _dtrsl; DDOT _ddot; DCSRCH _dcsrch; DCOPY _dcopy; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; const double BIG = 1.0E+10; const double FTOL = 1.0E-3; 
        const double GTOL = 0.9E0;const double XTOL = 0.1E0; 

        #endregion

        public LNSRLB(DTRSL dtrsl, DDOT ddot, DCSRCH dcsrch, DCOPY dcopy)
        {
    

            #region Set Dependencies
            
            this._dtrsl = dtrsl; this._ddot = ddot; this._dcsrch = dcsrch; this._dcopy = dcopy; 

            #endregion

        }
    
        public LNSRLB()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DCSTEP dcstep = new DCSTEP();
            DCOPY dcopy = new DCOPY();
            DTRSL dtrsl = new DTRSL(ddot, daxpy);
            DCSRCH dcsrch = new DCSRCH(dcstep);

            #endregion


            #region Set Dependencies
            
            this._dtrsl = dtrsl; this._ddot = ddot; this._dcsrch = dcsrch; this._dcopy = dcopy; 

            #endregion

        }
        public void Run(int N, double[] L, int offset_l, double[] U, int offset_u, int[] NBD, int offset_nbd, ref double[] X, int offset_x, double F
                         , ref double FOLD, ref double GD, ref double GDOLD, double[] G, int offset_g, double[] D, int offset_d, ref double[] R, int offset_r
                         , ref double[] T, int offset_t, double[] Z, int offset_z, ref double STP, ref double DNORM, ref double DTD, ref double XSTEP
                         , ref double STPMX, int ITER, ref int IFUN, ref int IBACK, ref int NFGV, ref int INFO
                         , ref BFGSTask TASK, bool BOXED, bool CNSTND, ref BFGSTask CSAVE, ref int[] ISAVE, int offset_isave, ref double[] DSAVE, int offset_dsave)
        {

            #region Variables
            
            int I = 0; double DDOT = 0; double A1 = 0; double A2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_nbd = -1 + offset_nbd;  int o_x = -1 + offset_x; 
             int o_g = -1 + offset_g; int o_d = -1 + offset_d;  int o_r = -1 + offset_r;  int o_t = -1 + offset_t; 
             int o_z = -1 + offset_z; int o_isave = -1 + offset_isave;  int o_dsave = -1 + offset_dsave; 

            #endregion



            #region Prolog
            
            
            // c     **********
            // c
            // c     Subroutine lnsrlb
            // c
            // c     This subroutine calls subroutine dcsrch from the Minpack2 library
            // c       to perform the line search.  Subroutine dscrch is safeguarded so
            // c       that all trial points lie within the feasible region.
            // c
            // c     Subprograms called:
            // c
            // c       Minpack2 Library ... dcsrch.
            // c
            // c       Linpack ... dtrsl, ddot.
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
            // c     **********
            
            
            
            

            #endregion


            #region Body
            
            if (TASK == BFGSTask.FG_LNSRCH) goto LABEL556;
            
            DTD = this._ddot.Run(N, D, offset_d, 1, D, offset_d, 1);
            DNORM = Math.Sqrt(DTD);
            
            // c     Determine the maximum step length.
            
            STPMX = BIG;
            if (CNSTND)
            {
                if (ITER == 0)
                {
                    STPMX = ONE;
                }
                else
                {
                    for (I = 1; I <= N; I++)
                    {
                        A1 = D[I + o_d];
                        if (NBD[I + o_nbd] != 0)
                        {
                            if (A1 < ZERO && NBD[I + o_nbd] <= 2)
                            {
                                A2 = L[I + o_l] - X[I + o_x];
                                if (A2 >= ZERO)
                                {
                                    STPMX = ZERO;
                                }
                                else
                                {
                                    if (A1 * STPMX < A2)
                                    {
                                        STPMX = A2 / A1;
                                    }
                                }
                            }
                            else
                            {
                                if (A1 > ZERO && NBD[I + o_nbd] >= 2)
                                {
                                    A2 = U[I + o_u] - X[I + o_x];
                                    if (A2 <= ZERO)
                                    {
                                        STPMX = ZERO;
                                    }
                                    else
                                    {
                                        if (A1 * STPMX > A2)
                                        {
                                            STPMX = A2 / A1;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            if (ITER == 0 && !BOXED)
            {
                STP = Math.Min(ONE / DNORM, STPMX);
            }
            else
            {
                STP = ONE;
            }
            
            this._dcopy.Run(N, X, offset_x, 1, ref T, offset_t, 1);
            this._dcopy.Run(N, G, offset_g, 1, ref R, offset_r, 1);
            FOLD = F;
            IFUN = 0;
            IBACK = 0;
            CSAVE = BFGSTask.START;
        LABEL556:;
            GD = this._ddot.Run(N, G, offset_g, 1, D, offset_d, 1);
            if (IFUN == 0)
            {
                GDOLD = GD;
                if (GD >= ZERO)
                {
                    // c                               the directional derivative >=0.
                    // c                               Line search is impossible.
                    INFO =  - 4;
                    return;
                }
            }
            
            this._dcsrch.Run(F, GD, ref STP, FTOL, GTOL, XTOL
                             , ZERO, STPMX, ref CSAVE, ref ISAVE, offset_isave, ref DSAVE, offset_dsave);
            
            XSTEP = STP * DNORM;
            if (CSAVE != BFGSTask.CONV && CSAVE != BFGSTask.WARNING)
            {
                TASK = BFGSTask.FG_LNSRCH;
                IFUN += 1;
                NFGV += 1;
                IBACK = IFUN - 1;
                if (STP == ONE)
                {
                    this._dcopy.Run(N, Z, offset_z, 1, ref X, offset_x, 1);
                }
                else
                {
                    for (I = 1; I <= N; I++)
                    {
                        X[I + o_x] = STP * D[I + o_d] + T[I + o_t];
                    }
                }
            }
            else
            {
                TASK = BFGSTask.NEW_X;
            }
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of lnsrlb =============================
}
