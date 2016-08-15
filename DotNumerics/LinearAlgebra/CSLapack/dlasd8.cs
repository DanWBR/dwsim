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
    /// DLASD8 finds the square roots of the roots of the secular equation,
    /// as defined by the values in DSIGMA and Z. It makes the appropriate
    /// calls to DLASD4, and stores, for each  element in D, the distance
    /// to its two nearest poles (elements in DSIGMA). It also updates
    /// the arrays VF and VL, the first and last components of all the
    /// right singular vectors of the original bidiagonal matrix.
    /// 
    /// DLASD8 is called from DLASD6.
    /// 
    ///</summary>
    public class DLASD8
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DLASCL _dlascl; DLASD4 _dlasd4; DLASET _dlaset; XERBLA _xerbla; DDOT _ddot; DLAMC3 _dlamc3; DNRM2 _dnrm2; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DLASD8(DCOPY dcopy, DLASCL dlascl, DLASD4 dlasd4, DLASET dlaset, XERBLA xerbla, DDOT ddot, DLAMC3 dlamc3, DNRM2 dnrm2)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlascl = dlascl; this._dlasd4 = dlasd4; this._dlaset = dlaset; this._xerbla = xerbla; 
            this._ddot = ddot;this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; 

            #endregion

        }
    
        public DLASD8()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            XERBLA xerbla = new XERBLA();
            DLASD5 dlasd5 = new DLASD5();
            DDOT ddot = new DDOT();
            DNRM2 dnrm2 = new DNRM2();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLASD4 dlasd4 = new DLASD4(dlaed6, dlasd5, dlamch);
            DLASET dlaset = new DLASET(lsame);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlascl = dlascl; this._dlasd4 = dlasd4; this._dlaset = dlaset; this._xerbla = xerbla; 
            this._ddot = ddot;this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASD8 finds the square roots of the roots of the secular equation,
        /// as defined by the values in DSIGMA and Z. It makes the appropriate
        /// calls to DLASD4, and stores, for each  element in D, the distance
        /// to its two nearest poles (elements in DSIGMA). It also updates
        /// the arrays VF and VL, the first and last components of all the
        /// right singular vectors of the original bidiagonal matrix.
        /// 
        /// DLASD8 is called from DLASD6.
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// Specifies whether singular vectors are to be computed in
        /// factored form in the calling routine:
        /// = 0: Compute singular values only.
        /// = 1: Compute singular vectors in factored form as well.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The number of terms in the rational function to be solved
        /// by DLASD4.  K .GE. 1.
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension ( K )
        /// On output, D contains the updated singular values.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension ( K )
        /// The first K elements of this array contain the components
        /// of the deflation-adjusted updating row vector.
        ///</param>
        /// <param name="VF">
        /// (input/output) DOUBLE PRECISION array, dimension ( K )
        /// On entry, VF contains  information passed through DBEDE8.
        /// On exit, VF contains the first K components of the first
        /// components of all right singular vectors of the bidiagonal
        /// matrix.
        ///</param>
        /// <param name="VL">
        /// (input/output) DOUBLE PRECISION array, dimension ( K )
        /// On entry, VL contains  information passed through DBEDE8.
        /// On exit, VL contains the first K components of the last
        /// components of all right singular vectors of the bidiagonal
        /// matrix.
        ///</param>
        /// <param name="DIFL">
        /// (output) DOUBLE PRECISION array, dimension ( K )
        /// On exit, DIFL(I) = D(I) - DSIGMA(I).
        ///</param>
        /// <param name="DIFR">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDDIFR, 2 ) if ICOMPQ = 1 and
        /// dimension ( K ) if ICOMPQ = 0.
        /// On exit, DIFR(I,1) = D(I) - DSIGMA(I+1), DIFR(K,1) is not
        /// defined and will not be referenced.
        /// 
        /// If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
        /// normalizing factors for the right singular vector matrix.
        ///</param>
        /// <param name="LDDIFR">
        /// (input) INTEGER
        /// The leading dimension of DIFR, must be at least K.
        ///</param>
        /// <param name="DSIGMA">
        /// (input) DOUBLE PRECISION array, dimension ( K )
        /// The first K elements of this array contain the old roots
        /// of the deflated updating problem.  These are the poles
        /// of the secular equation.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension at least 3 * K
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an singular value did not converge
        ///</param>
        public void Run(int ICOMPQ, int K, ref double[] D, int offset_d, ref double[] Z, int offset_z, ref double[] VF, int offset_vf, ref double[] VL, int offset_vl
                         , ref double[] DIFL, int offset_difl, ref double[] DIFR, int offset_difr, int LDDIFR, ref double[] DSIGMA, int offset_dsigma, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; int IWK1 = 0; int IWK2 = 0; int IWK2I = 0; int IWK3 = 0; int IWK3I = 0; int J = 0; double DIFLJ = 0; 
            double DIFRJ = 0;double DJ = 0; double DSIGJ = 0; double DSIGJP = 0; double RHO = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int DIFR_1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z;  int o_vf = -1 + offset_vf;  int o_vl = -1 + offset_vl; 
             int o_difl = -1 + offset_difl; int o_difr = -1 - LDDIFR + offset_difr;  int o_dsigma = -1 + offset_dsigma; 
             int o_work = -1 + offset_work;

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLASD8 finds the square roots of the roots of the secular equation,
            // *  as defined by the values in DSIGMA and Z. It makes the appropriate
            // *  calls to DLASD4, and stores, for each  element in D, the distance
            // *  to its two nearest poles (elements in DSIGMA). It also updates
            // *  the arrays VF and VL, the first and last components of all the
            // *  right singular vectors of the original bidiagonal matrix.
            // *
            // *  DLASD8 is called from DLASD6.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ICOMPQ  (input) INTEGER
            // *          Specifies whether singular vectors are to be computed in
            // *          factored form in the calling routine:
            // *          = 0: Compute singular values only.
            // *          = 1: Compute singular vectors in factored form as well.
            // *
            // *  K       (input) INTEGER
            // *          The number of terms in the rational function to be solved
            // *          by DLASD4.  K >= 1.
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension ( K )
            // *          On output, D contains the updated singular values.
            // *
            // *  Z       (input) DOUBLE PRECISION array, dimension ( K )
            // *          The first K elements of this array contain the components
            // *          of the deflation-adjusted updating row vector.
            // *
            // *  VF      (input/output) DOUBLE PRECISION array, dimension ( K )
            // *          On entry, VF contains  information passed through DBEDE8.
            // *          On exit, VF contains the first K components of the first
            // *          components of all right singular vectors of the bidiagonal
            // *          matrix.
            // *
            // *  VL      (input/output) DOUBLE PRECISION array, dimension ( K )
            // *          On entry, VL contains  information passed through DBEDE8.
            // *          On exit, VL contains the first K components of the last
            // *          components of all right singular vectors of the bidiagonal
            // *          matrix.
            // *
            // *  DIFL    (output) DOUBLE PRECISION array, dimension ( K )
            // *          On exit, DIFL(I) = D(I) - DSIGMA(I).
            // *
            // *  DIFR    (output) DOUBLE PRECISION array,
            // *                   dimension ( LDDIFR, 2 ) if ICOMPQ = 1 and
            // *                   dimension ( K ) if ICOMPQ = 0.
            // *          On exit, DIFR(I,1) = D(I) - DSIGMA(I+1), DIFR(K,1) is not
            // *          defined and will not be referenced.
            // *
            // *          If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
            // *          normalizing factors for the right singular vector matrix.
            // *
            // *  LDDIFR  (input) INTEGER
            // *          The leading dimension of DIFR, must be at least K.
            // *
            // *  DSIGMA  (input) DOUBLE PRECISION array, dimension ( K )
            // *          The first K elements of this array contain the old roots
            // *          of the deflated updating problem.  These are the poles
            // *          of the secular equation.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension at least 3 * K
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if INFO = 1, an singular value did not converge
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ming Gu and Huan Ren, Computer Science Division, University of
            // *     California at Berkeley, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, SIGN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if ((ICOMPQ < 0) || (ICOMPQ > 1))
            {
                INFO =  - 1;
            }
            else
            {
                if (K < 1)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (LDDIFR < K)
                    {
                        INFO =  - 9;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASD8",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (K == 1)
            {
                D[1 + o_d] = Math.Abs(Z[1 + o_z]);
                DIFL[1 + o_difl] = D[1 + o_d];
                if (ICOMPQ == 1)
                {
                    DIFL[2 + o_difl] = ONE;
                    DIFR[1+2 * LDDIFR + o_difr] = ONE;
                }
                return;
            }
            // *
            // *     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can
            // *     be computed with high relative accuracy (barring over/underflow).
            // *     This is a problem on machines without a guard digit in
            // *     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
            // *     The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),
            // *     which on any of these machines zeros out the bottommost
            // *     bit of DSIGMA(I) if it is 1; this makes the subsequent
            // *     subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation
            // *     occurs. On binary machines with a guard digit (almost all
            // *     machines) it does not change DSIGMA(I) at all. On hexadecimal
            // *     and decimal machines with a guard digit, it slightly
            // *     changes the bottommost bits of DSIGMA(I). It does not account
            // *     for hexadecimal or decimal machines without guard digits
            // *     (we know of none). We use a subroutine call to compute
            // *     2*DSIGMA(I) to prevent optimizing compilers from eliminating
            // *     this code.
            // *
            for (I = 1; I <= K; I++)
            {
                DSIGMA[I + o_dsigma] = this._dlamc3.Run(DSIGMA[I + o_dsigma], DSIGMA[I + o_dsigma]) - DSIGMA[I + o_dsigma];
            }
            // *
            // *     Book keeping.
            // *
            IWK1 = 1;
            IWK2 = IWK1 + K;
            IWK3 = IWK2 + K;
            IWK2I = IWK2 - 1;
            IWK3I = IWK3 - 1;
            // *
            // *     Normalize Z.
            // *
            RHO = this._dnrm2.Run(K, Z, offset_z, 1);
            this._dlascl.Run("G", 0, 0, RHO, ONE, K
                             , 1, ref Z, offset_z, K, ref INFO);
            RHO *= RHO;
            // *
            // *     Initialize WORK(IWK3).
            // *
            this._dlaset.Run("A", K, 1, ONE, ONE, ref WORK, IWK3 + o_work
                             , K);
            // *
            // *     Compute the updated singular values, the arrays DIFL, DIFR,
            // *     and the updated Z.
            // *
            DIFR_1 = 1 * LDDIFR + o_difr;
            for (J = 1; J <= K; J++)
            {
                this._dlasd4.Run(K, J, DSIGMA, offset_dsigma, Z, offset_z, ref WORK, IWK1 + o_work, RHO
                                 , ref D[J + o_d], ref WORK, IWK2 + o_work, ref INFO);
                // *
                // *        If the root finder fails, the computation is terminated.
                // *
                if (INFO != 0)
                {
                    return;
                }
                WORK[IWK3I + J + o_work] = WORK[IWK3I + J + o_work] * WORK[J + o_work] * WORK[IWK2I + J + o_work];
                DIFL[J + o_difl] =  - WORK[J + o_work];
                DIFR[J + DIFR_1] =  - WORK[J + 1 + o_work];
                for (I = 1; I <= J - 1; I++)
                {
                    WORK[IWK3I + I + o_work] = WORK[IWK3I + I + o_work] * WORK[I + o_work] * WORK[IWK2I + I + o_work] / (DSIGMA[I + o_dsigma] - DSIGMA[J + o_dsigma]) / (DSIGMA[I + o_dsigma] + DSIGMA[J + o_dsigma]);
                }
                for (I = J + 1; I <= K; I++)
                {
                    WORK[IWK3I + I + o_work] = WORK[IWK3I + I + o_work] * WORK[I + o_work] * WORK[IWK2I + I + o_work] / (DSIGMA[I + o_dsigma] - DSIGMA[J + o_dsigma]) / (DSIGMA[I + o_dsigma] + DSIGMA[J + o_dsigma]);
                }
            }
            // *
            // *     Compute updated Z.
            // *
            for (I = 1; I <= K; I++)
            {
                Z[I + o_z] = FortranLib.Sign(Math.Sqrt(Math.Abs(WORK[IWK3I + I + o_work])),Z[I + o_z]);
            }
            // *
            // *     Update VF and VL.
            // *
            for (J = 1; J <= K; J++)
            {
                DIFLJ = DIFL[J + o_difl];
                DJ = D[J + o_d];
                DSIGJ =  - DSIGMA[J + o_dsigma];
                if (J < K)
                {
                    DIFRJ =  - DIFR[J+1 * LDDIFR + o_difr];
                    DSIGJP =  - DSIGMA[J + 1 + o_dsigma];
                }
                WORK[J + o_work] =  - Z[J + o_z] / DIFLJ / (DSIGMA[J + o_dsigma] + DJ);
                for (I = 1; I <= J - 1; I++)
                {
                    WORK[I + o_work] = Z[I + o_z] / (this._dlamc3.Run(DSIGMA[I + o_dsigma], DSIGJ) - DIFLJ) / (DSIGMA[I + o_dsigma] + DJ);
                }
                for (I = J + 1; I <= K; I++)
                {
                    WORK[I + o_work] = Z[I + o_z] / (this._dlamc3.Run(DSIGMA[I + o_dsigma], DSIGJP) + DIFRJ) / (DSIGMA[I + o_dsigma] + DJ);
                }
                TEMP = this._dnrm2.Run(K, WORK, offset_work, 1);
                WORK[IWK2I + J + o_work] = this._ddot.Run(K, WORK, offset_work, 1, VF, offset_vf, 1) / TEMP;
                WORK[IWK3I + J + o_work] = this._ddot.Run(K, WORK, offset_work, 1, VL, offset_vl, 1) / TEMP;
                if (ICOMPQ == 1)
                {
                    DIFR[J+2 * LDDIFR + o_difr] = TEMP;
                }
            }
            // *
            this._dcopy.Run(K, WORK, IWK2 + o_work, 1, ref VF, offset_vf, 1);
            this._dcopy.Run(K, WORK, IWK3 + o_work, 1, ref VL, offset_vl, 1);
            // *
            return;
            // *
            // *     End of DLASD8
            // *

            #endregion

        }
    }
}
