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

namespace DotNumerics.LinearAlgebra.CSLapack
{
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLARTG generate a plane rotation so that
    /// 
    /// [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
    /// [ -SN  CS  ]     [ G ]     [ 0 ]
    /// 
    /// This is a slower, more accurate version of the BLAS1 routine DROTG,
    /// with the following other differences:
    /// F and G are unchanged on return.
    /// If G=0, then CS=1 and SN=0.
    /// If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
    /// floating point operations (saves work in DBDSQR when
    /// there are zeros on the diagonal).
    /// 
    /// If F exceeds G in magnitude, CS will be positive.
    /// 
    ///</summary>
    public class DLARTG
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 

        #endregion

        public DLARTG(DLAMCH dlamch)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; 

            #endregion

        }
    
        public DLARTG()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARTG generate a plane rotation so that
        /// 
        /// [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
        /// [ -SN  CS  ]     [ G ]     [ 0 ]
        /// 
        /// This is a slower, more accurate version of the BLAS1 routine DROTG,
        /// with the following other differences:
        /// F and G are unchanged on return.
        /// If G=0, then CS=1 and SN=0.
        /// If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
        /// floating point operations (saves work in DBDSQR when
        /// there are zeros on the diagonal).
        /// 
        /// If F exceeds G in magnitude, CS will be positive.
        /// 
        ///</summary>
        /// <param name="F">
        /// (input) DOUBLE PRECISION
        /// The first component of vector to be rotated.
        ///</param>
        /// <param name="G">
        /// (input) DOUBLE PRECISION
        /// The second component of vector to be rotated.
        ///</param>
        /// <param name="CS">
        /// (output) DOUBLE PRECISION
        /// The cosine of the rotation.
        ///</param>
        /// <param name="SN">
        /// (output) DOUBLE PRECISION
        /// The sine of the rotation.
        ///</param>
        /// <param name="R">
        /// (output) DOUBLE PRECISION
        /// The nonzero component of the rotated vector.
        ///</param>
        public void Run(double F, double G, ref double CS, ref double SN, ref double R)
        {

            #region Variables
            
            int COUNT = 0; int I = 0; double EPS = 0; double F1 = 0; double G1 = 0; double SAFMIN = 0; double SAFMN2 = 0; 
            double SAFMX2 = 0;double SCALE = 0; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLARTG generate a plane rotation so that
            // *
            // *     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
            // *     [ -SN  CS  ]     [ G ]     [ 0 ]
            // *
            // *  This is a slower, more accurate version of the BLAS1 routine DROTG,
            // *  with the following other differences:
            // *     F and G are unchanged on return.
            // *     If G=0, then CS=1 and SN=0.
            // *     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
            // *        floating point operations (saves work in DBDSQR when
            // *        there are zeros on the diagonal).
            // *
            // *  If F exceeds G in magnitude, CS will be positive.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  F       (input) DOUBLE PRECISION
            // *          The first component of vector to be rotated.
            // *
            // *  G       (input) DOUBLE PRECISION
            // *          The second component of vector to be rotated.
            // *
            // *  CS      (output) DOUBLE PRECISION
            // *          The cosine of the rotation.
            // *
            // *  SN      (output) DOUBLE PRECISION
            // *          The sine of the rotation.
            // *
            // *  R       (output) DOUBLE PRECISION
            // *          The nonzero component of the rotated vector.
            // *
            // *  This version has a few statements commented out for thread safety
            // *  (machine parameters are computed on each entry). 10 feb 03, SJH.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     LOGICAL            FIRST
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, INT, LOG, MAX, SQRT;
            // *     ..
            // *     .. Save statement ..
            // *     SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2
            // *     ..
            // *     .. Data statements ..
            // *     DATA               FIRST / .TRUE. /
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     IF( FIRST ) THEN

            #endregion


            #region Body
            
            SAFMIN = this._dlamch.Run("S");
            EPS = this._dlamch.Run("E");
            SAFMN2 = Math.Pow(this._dlamch.Run("B"),Convert.ToInt32(Math.Truncate(Math.Log(SAFMIN / EPS) / Math.Log(this._dlamch.Run("B")) / TWO)));
            SAFMX2 = ONE / SAFMN2;
            // *        FIRST = .FALSE.
            // *     END IF
            if (G == ZERO)
            {
                CS = ONE;
                SN = ZERO;
                R = F;
            }
            else
            {
                if (F == ZERO)
                {
                    CS = ZERO;
                    SN = ONE;
                    R = G;
                }
                else
                {
                    F1 = F;
                    G1 = G;
                    SCALE = Math.Max(Math.Abs(F1), Math.Abs(G1));
                    if (SCALE >= SAFMX2)
                    {
                        COUNT = 0;
                    LABEL10:;
                        COUNT += 1;
                        F1 *= SAFMN2;
                        G1 *= SAFMN2;
                        SCALE = Math.Max(Math.Abs(F1), Math.Abs(G1));
                        if (SCALE >= SAFMX2) goto LABEL10;
                        R = Math.Sqrt(Math.Pow(F1,2) + Math.Pow(G1,2));
                        CS = F1 / R;
                        SN = G1 / R;
                        for (I = 1; I <= COUNT; I++)
                        {
                            R *= SAFMX2;
                        }
                    }
                    else
                    {
                        if (SCALE <= SAFMN2)
                        {
                            COUNT = 0;
                        LABEL30:;
                            COUNT += 1;
                            F1 *= SAFMX2;
                            G1 *= SAFMX2;
                            SCALE = Math.Max(Math.Abs(F1), Math.Abs(G1));
                            if (SCALE <= SAFMN2) goto LABEL30;
                            R = Math.Sqrt(Math.Pow(F1,2) + Math.Pow(G1,2));
                            CS = F1 / R;
                            SN = G1 / R;
                            for (I = 1; I <= COUNT; I++)
                            {
                                R *= SAFMN2;
                            }
                        }
                        else
                        {
                            R = Math.Sqrt(Math.Pow(F1,2) + Math.Pow(G1,2));
                            CS = F1 / R;
                            SN = G1 / R;
                        }
                    }
                    if (Math.Abs(F) > Math.Abs(G) && CS < ZERO)
                    {
                        CS =  - CS;
                        SN =  - SN;
                        R =  - R;
                    }
                }
            }
            return;
            // *
            // *     End of DLARTG
            // *

            #endregion

        }
    }
}
