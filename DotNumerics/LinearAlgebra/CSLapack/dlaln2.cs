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
    /// DLALN2 solves a system of the form  (ca A - w D ) X = s B
    /// or (ca A' - w D) X = s B   with possible scaling ("s") and
    /// perturbation of A.  (A' means A-transpose.)
    /// 
    /// A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
    /// real diagonal matrix, w is a real or complex value, and X and B are
    /// NA x 1 matrices -- real if w is real, complex if w is complex.  NA
    /// may be 1 or 2.
    /// 
    /// If w is complex, X and B are represented as NA x 2 matrices,
    /// the first column of each being the real part and the second
    /// being the imaginary part.
    /// 
    /// "s" is a scaling factor (.LE. 1), computed by DLALN2, which is
    /// so chosen that X can be computed without overflow.  X is further
    /// scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
    /// than overflow.
    /// 
    /// If both singular values of (ca A - w D) are less than SMIN,
    /// SMIN*identity will be used instead of (ca A - w D).  If only one
    /// singular value is less than SMIN, one element of (ca A - w D) will be
    /// perturbed enough to make the smallest singular value roughly SMIN.
    /// If both singular values are at least SMIN, (ca A - w D) will not be
    /// perturbed.  In any case, the perturbation will be at most some small
    /// multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
    /// are computed by infinity-norm approximations, and thus will only be
    /// correct to a factor of 2 or so.
    /// 
    /// Note: all input quantities are assumed to be smaller than overflow
    /// by a reasonable factor.  (See BIGNUM.)
    /// 
    ///</summary>
    public class DLALN2
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DLADIV _dladiv; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 
        bool[] RSWAP = new bool[4]; int o_rswap = -1;bool[] ZSWAP = new bool[4]; int o_zswap = -1; 
        int[] IPIVOT = new int[4 * 4]; int o_ipivot = -5;double[] CIV = new double[4]; 
        double[] CRV = new double[4];

        #endregion

        public DLALN2(DLAMCH dlamch, DLADIV dladiv)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dladiv = dladiv; 

            #endregion


            #region Data Initialization
            
            //ZSWAP/.FALSE.,.FALSE.,.TRUE.,.TRUE.
            ZSWAP[1 + o_zswap] = false;
            ZSWAP[2 + o_zswap] = false;
            ZSWAP[3 + o_zswap] = true;
            ZSWAP[4 + o_zswap] = true;
            //RSWAP/.FALSE.,.TRUE.,.FALSE.,.TRUE.
            RSWAP[1 + o_rswap] = false;
            RSWAP[2 + o_rswap] = true;
            RSWAP[3 + o_rswap] = false;
            RSWAP[4 + o_rswap] = true;
            //IPIVOT/1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1
            IPIVOT[0] = 1;
            IPIVOT[1] = 2;
            IPIVOT[2] = 3;
            IPIVOT[3] = 4;
            IPIVOT[4] = 2;
            IPIVOT[5] = 1;
            IPIVOT[6] = 4;
            IPIVOT[7] = 3;
            IPIVOT[8] = 3;
            IPIVOT[9] = 4;
            IPIVOT[10] = 1;
            IPIVOT[11] = 2;
            IPIVOT[12] = 4;
            IPIVOT[12] = 3;
            IPIVOT[14] = 2;
            IPIVOT[15] = 1;

            #endregion

        }
    
        public DLALN2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLADIV dladiv = new DLADIV();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dladiv = dladiv; 

            #endregion


            #region Data Initialization
            
            //ZSWAP/.FALSE.,.FALSE.,.TRUE.,.TRUE.
            ZSWAP[1 + o_zswap] = false;
            ZSWAP[2 + o_zswap] = false;
            ZSWAP[3 + o_zswap] = true;
            ZSWAP[4 + o_zswap] = true;
            //RSWAP/.FALSE.,.TRUE.,.FALSE.,.TRUE.
            RSWAP[1 + o_rswap] = false;
            RSWAP[2 + o_rswap] = true;
            RSWAP[3 + o_rswap] = false;
            RSWAP[4 + o_rswap] = true;
            //IPIVOT/1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1
            IPIVOT[0] = 1;
            IPIVOT[1] = 2;
            IPIVOT[2] = 3;
            IPIVOT[3] = 4;
            IPIVOT[4] = 2;
            IPIVOT[5] = 1;
            IPIVOT[6] = 4;
            IPIVOT[7] = 3;
            IPIVOT[8] = 3;
            IPIVOT[9] = 4;
            IPIVOT[10] = 1;
            IPIVOT[11] = 2;
            IPIVOT[12] = 4;
            IPIVOT[12] = 3;
            IPIVOT[14] = 2;
            IPIVOT[15] = 1;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLALN2 solves a system of the form  (ca A - w D ) X = s B
        /// or (ca A' - w D) X = s B   with possible scaling ("s") and
        /// perturbation of A.  (A' means A-transpose.)
        /// 
        /// A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
        /// real diagonal matrix, w is a real or complex value, and X and B are
        /// NA x 1 matrices -- real if w is real, complex if w is complex.  NA
        /// may be 1 or 2.
        /// 
        /// If w is complex, X and B are represented as NA x 2 matrices,
        /// the first column of each being the real part and the second
        /// being the imaginary part.
        /// 
        /// "s" is a scaling factor (.LE. 1), computed by DLALN2, which is
        /// so chosen that X can be computed without overflow.  X is further
        /// scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
        /// than overflow.
        /// 
        /// If both singular values of (ca A - w D) are less than SMIN,
        /// SMIN*identity will be used instead of (ca A - w D).  If only one
        /// singular value is less than SMIN, one element of (ca A - w D) will be
        /// perturbed enough to make the smallest singular value roughly SMIN.
        /// If both singular values are at least SMIN, (ca A - w D) will not be
        /// perturbed.  In any case, the perturbation will be at most some small
        /// multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
        /// are computed by infinity-norm approximations, and thus will only be
        /// correct to a factor of 2 or so.
        /// 
        /// Note: all input quantities are assumed to be smaller than overflow
        /// by a reasonable factor.  (See BIGNUM.)
        /// 
        ///</summary>
        /// <param name="LTRANS">
        /// (input) LOGICAL
        /// =.TRUE.:  A-transpose will be used.
        /// =.FALSE.: A will be used (not transposed.)
        ///</param>
        /// <param name="NA">
        /// (input) INTEGER
        /// The size of the matrix A.  It may (only) be 1 or 2.
        ///</param>
        /// <param name="NW">
        /// (input) INTEGER
        /// 1 if "w" is real, 2 if "w" is complex.  It may only be 1
        /// or 2.
        ///</param>
        /// <param name="SMIN">
        /// (input) DOUBLE PRECISION
        /// The desired lower bound on the singular values of A.  This
        /// should be a safe distance away from underflow or overflow,
        /// say, between (underflow/machine precision) and  (machine
        /// precision * overflow ).  (See BIGNUM and ULP.)
        ///</param>
        /// <param name="CA">
        /// (input) DOUBLE PRECISION
        /// The coefficient c, which A is multiplied by.
        ///</param>
        /// <param name="A">
        /// is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of A.  It must be at least NA.
        ///</param>
        /// <param name="D1">
        /// (input) DOUBLE PRECISION
        /// The 1,1 element in the diagonal matrix D.
        ///</param>
        /// <param name="D2">
        /// (input) DOUBLE PRECISION
        /// The 2,2 element in the diagonal matrix D.  Not used if NW=1.
        ///</param>
        /// <param name="B">
        /// (input) DOUBLE PRECISION array, dimension (LDB,NW)
        /// The NA x NW matrix B (right-hand side).  If NW=2 ("w" is
        /// complex), column 1 contains the real part of B and column 2
        /// contains the imaginary part.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of B.  It must be at least NA.
        ///</param>
        /// <param name="WR">
        /// (input) DOUBLE PRECISION
        /// The real part of the scalar "w".
        ///</param>
        /// <param name="WI">
        /// (input) DOUBLE PRECISION
        /// The imaginary part of the scalar "w".  Not used if NW=1.
        ///</param>
        /// <param name="X">
        /// (output) DOUBLE PRECISION array, dimension (LDX,NW)
        /// The NA x NW matrix X (unknowns), as computed by DLALN2.
        /// If NW=2 ("w" is complex), on exit, column 1 will contain
        /// the real part of X and column 2 will contain the imaginary
        /// part.
        ///</param>
        /// <param name="LDX">
        /// (input) INTEGER
        /// The leading dimension of X.  It must be at least NA.
        ///</param>
        /// <param name="SCALE">
        /// (output) DOUBLE PRECISION
        /// The scale factor that B must be multiplied by to insure
        /// that overflow does not occur when computing X.  Thus,
        /// (ca A - w D) X  will be SCALE*B, not B (ignoring
        /// perturbations of A.)  It will be at most 1.
        ///</param>
        /// <param name="XNORM">
        /// (output) DOUBLE PRECISION
        /// The infinity-norm of X, when X is regarded as an NA x NW
        /// real matrix.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// An error flag.  It will be set to zero if no error occurs,
        /// a negative number if an argument is in error, or a positive
        /// number if  ca A - w D  had to be perturbed.
        /// The possible values are:
        /// = 0: No error occurred, and (ca A - w D) did not have to be
        /// perturbed.
        /// = 1: (ca A - w D) had to be perturbed to make its smallest
        /// (or only) singular value greater than SMIN.
        /// NOTE: In the interests of speed, this routine does not
        /// check the inputs for errors.
        ///</param>
        public void Run(bool LTRANS, int NA, int NW, double SMIN, double CA, double[] A, int offset_a
                         , int LDA, double D1, double D2, double[] B, int offset_b, int LDB, double WR
                         , double WI, ref double[] X, int offset_x, int LDX, ref double SCALE, ref double XNORM, ref int INFO)
        {

            #region Variables
            
            int ICMAX = 0; int J = 0; double BBND = 0; double BI1 = 0; double BI2 = 0; double BIGNUM = 0; double BNORM = 0; 
            double BR1 = 0;double BR2 = 0; double CI21 = 0; double CI22 = 0; double CMAX = 0; double CNORM = 0; double CR21 = 0; 
            double CR22 = 0;double CSI = 0; double CSR = 0; double LI21 = 0; double LR21 = 0; double SMINI = 0; double SMLNUM = 0; 
            double TEMP = 0;double U22ABS = 0; double UI11 = 0; double UI11R = 0; double UI12 = 0; double UI12S = 0; 
            double UI22 = 0;double UR11 = 0; double UR11R = 0; double UR12 = 0; double UR12S = 0; double UR22 = 0; double XI1 = 0; 
            double XI2 = 0;double XR1 = 0; double XR2 = 0; int o_civ = -1; int o_crv = -1; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_x = -1 - LDX + offset_x; 

            #endregion


            #region Prolog
            
            
            
            // *
            // *	SUBRUTINA MODIFICADA, SE ELIMINO LA EQUIVALENCIA 
            // *
            // *      EQUIVALENCE        ( CI( 1, 1 ), CIV( 1 ) ),
            // *     $                   ( CR( 1, 1 ), CRV( 1 ) )
            // *
            // *	DEBIDO A QUE EL TRADUCTOR NO LA SOPORTA
            
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
            // *  DLALN2 solves a system of the form  (ca A - w D ) X = s B
            // *  or (ca A' - w D) X = s B   with possible scaling ("s") and
            // *  perturbation of A.  (A' means A-transpose.)
            // *
            // *  A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
            // *  real diagonal matrix, w is a real or complex value, and X and B are
            // *  NA x 1 matrices -- real if w is real, complex if w is complex.  NA
            // *  may be 1 or 2.
            // *
            // *  If w is complex, X and B are represented as NA x 2 matrices,
            // *  the first column of each being the real part and the second
            // *  being the imaginary part.
            // *
            // *  "s" is a scaling factor (.LE. 1), computed by DLALN2, which is
            // *  so chosen that X can be computed without overflow.  X is further
            // *  scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
            // *  than overflow.
            // *
            // *  If both singular values of (ca A - w D) are less than SMIN,
            // *  SMIN*identity will be used instead of (ca A - w D).  If only one
            // *  singular value is less than SMIN, one element of (ca A - w D) will be
            // *  perturbed enough to make the smallest singular value roughly SMIN.
            // *  If both singular values are at least SMIN, (ca A - w D) will not be
            // *  perturbed.  In any case, the perturbation will be at most some small
            // *  multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
            // *  are computed by infinity-norm approximations, and thus will only be
            // *  correct to a factor of 2 or so.
            // *
            // *  Note: all input quantities are assumed to be smaller than overflow
            // *  by a reasonable factor.  (See BIGNUM.)
            // *
            // *  Arguments
            // *  ==========
            // *
            // *  LTRANS  (input) LOGICAL
            // *          =.TRUE.:  A-transpose will be used.
            // *          =.FALSE.: A will be used (not transposed.)
            // *
            // *  NA      (input) INTEGER
            // *          The size of the matrix A.  It may (only) be 1 or 2.
            // *
            // *  NW      (input) INTEGER
            // *          1 if "w" is real, 2 if "w" is complex.  It may only be 1
            // *          or 2.
            // *
            // *  SMIN    (input) DOUBLE PRECISION
            // *          The desired lower bound on the singular values of A.  This
            // *          should be a safe distance away from underflow or overflow,
            // *          say, between (underflow/machine precision) and  (machine
            // *          precision * overflow ).  (See BIGNUM and ULP.)
            // *
            // *  CA      (input) DOUBLE PRECISION
            // *          The coefficient c, which A is multiplied by.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,NA)
            // *          The NA x NA matrix A.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of A.  It must be at least NA.
            // *
            // *  D1      (input) DOUBLE PRECISION
            // *          The 1,1 element in the diagonal matrix D.
            // *
            // *  D2      (input) DOUBLE PRECISION
            // *          The 2,2 element in the diagonal matrix D.  Not used if NW=1.
            // *
            // *  B       (input) DOUBLE PRECISION array, dimension (LDB,NW)
            // *          The NA x NW matrix B (right-hand side).  If NW=2 ("w" is
            // *          complex), column 1 contains the real part of B and column 2
            // *          contains the imaginary part.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of B.  It must be at least NA.
            // *
            // *  WR      (input) DOUBLE PRECISION
            // *          The real part of the scalar "w".
            // *
            // *  WI      (input) DOUBLE PRECISION
            // *          The imaginary part of the scalar "w".  Not used if NW=1.
            // *
            // *  X       (output) DOUBLE PRECISION array, dimension (LDX,NW)
            // *          The NA x NW matrix X (unknowns), as computed by DLALN2.
            // *          If NW=2 ("w" is complex), on exit, column 1 will contain
            // *          the real part of X and column 2 will contain the imaginary
            // *          part.
            // *
            // *  LDX     (input) INTEGER
            // *          The leading dimension of X.  It must be at least NA.
            // *
            // *  SCALE   (output) DOUBLE PRECISION
            // *          The scale factor that B must be multiplied by to insure
            // *          that overflow does not occur when computing X.  Thus,
            // *          (ca A - w D) X  will be SCALE*B, not B (ignoring
            // *          perturbations of A.)  It will be at most 1.
            // *
            // *  XNORM   (output) DOUBLE PRECISION
            // *          The infinity-norm of X, when X is regarded as an NA x NW
            // *          real matrix.
            // *
            // *  INFO    (output) INTEGER
            // *          An error flag.  It will be set to zero if no error occurs,
            // *          a negative number if an argument is in error, or a positive
            // *          number if  ca A - w D  had to be perturbed.
            // *          The possible values are:
            // *          = 0: No error occurred, and (ca A - w D) did not have to be
            // *                 perturbed.
            // *          = 1: (ca A - w D) had to be perturbed to make its smallest
            // *               (or only) singular value greater than SMIN.
            // *          NOTE: In the interests of speed, this routine does not
            // *                check the inputs for errors.
            // *
            // * =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
            
            
            
            // *-----------------------------INICIA MI MODIFICACION--------------------------
            
            // *      DOUBLE PRECISION   CI( 2, 2 ), CIV( 4 ), CR( 2, 2 ), CRV( 4 )  
            // *-----------------------------TERMINA MI MODIFICACION-------------------------
            
            
            
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX;
            // *     ..
            // *     .. Equivalences ..
            
            
            // *-----------------------------INICIA MI MODIFICACION--------------------------
            // *      EQUIVALENCE        ( CI( 1, 1 ), CIV( 1 ) ),       
            // *     $                   ( CR( 1, 1 ), CRV( 1 ) )
            // *-----------------------------TERMINA MI MODIFICACION-------------------------
            
            
            
            // *     .. Data statements ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Compute BIGNUM
            // *

            #endregion


            #region Body
            
            SMLNUM = TWO * this._dlamch.Run("Safe minimum");
            BIGNUM = ONE / SMLNUM;
            SMINI = Math.Max(SMIN, SMLNUM);
            // *
            // *     Don't check for input errors
            // *
            INFO = 0;
            // *
            // *     Standard Initializations
            // *
            SCALE = ONE;
            // *
            if (NA == 1)
            {
                // *
                // *        1 x 1  (i.e., scalar) system   C X = B
                // *
                if (NW == 1)
                {
                    // *
                    // *           Real 1x1 system.
                    // *
                    // *           C = ca A - w D
                    // *
                    CSR = CA * A[1+1 * LDA + o_a] - WR * D1;
                    CNORM = Math.Abs(CSR);
                    // *
                    // *           If | C | < SMINI, use C = SMINI
                    // *
                    if (CNORM < SMINI)
                    {
                        CSR = SMINI;
                        CNORM = SMINI;
                        INFO = 1;
                    }
                    // *
                    // *           Check scaling for  X = B / C
                    // *
                    BNORM = Math.Abs(B[1+1 * LDB + o_b]);
                    if (CNORM < ONE && BNORM > ONE)
                    {
                        if (BNORM > BIGNUM * CNORM) SCALE = ONE / BNORM;
                    }
                    // *
                    // *           Compute X
                    // *
                    X[1+1 * LDX + o_x] = (B[1+1 * LDB + o_b] * SCALE) / CSR;
                    XNORM = Math.Abs(X[1+1 * LDX + o_x]);
                }
                else
                {
                    // *
                    // *           Complex 1x1 system (w is complex)
                    // *
                    // *           C = ca A - w D
                    // *
                    CSR = CA * A[1+1 * LDA + o_a] - WR * D1;
                    CSI =  - WI * D1;
                    CNORM = Math.Abs(CSR) + Math.Abs(CSI);
                    // *
                    // *           If | C | < SMINI, use C = SMINI
                    // *
                    if (CNORM < SMINI)
                    {
                        CSR = SMINI;
                        CSI = ZERO;
                        CNORM = SMINI;
                        INFO = 1;
                    }
                    // *
                    // *           Check scaling for  X = B / C
                    // *
                    BNORM = Math.Abs(B[1+1 * LDB + o_b]) + Math.Abs(B[1+2 * LDB + o_b]);
                    if (CNORM < ONE && BNORM > ONE)
                    {
                        if (BNORM > BIGNUM * CNORM) SCALE = ONE / BNORM;
                    }
                    // *
                    // *           Compute X
                    // *
                    this._dladiv.Run(SCALE * B[1+1 * LDB + o_b], SCALE * B[1+2 * LDB + o_b], CSR, CSI, ref X[1+1 * LDX + o_x], ref X[1+2 * LDX + o_x]);
                    XNORM = Math.Abs(X[1+1 * LDX + o_x]) + Math.Abs(X[1+2 * LDX + o_x]);
                }
                // *
            }
            else
            {
                // *
                // *        2x2 System
                // *
                // *        Compute the real part of  C = ca A - w D  (or  ca A' - w D )
                // *
                
                
                
                
                // *-----------------------------INICIA MI MODIFICACION--------------------------
                
                // *         CR( 1, 1 ) = CA*A( 1, 1 ) - WR*D1
                // *         CR( 2, 2 ) = CA*A( 2, 2 ) - WR*D2
                
                CRV[1 + o_crv] = CA * A[1+1 * LDA + o_a] - WR * D1;
                CRV[4 + o_crv] = CA * A[2+2 * LDA + o_a] - WR * D2;
                
                if (LTRANS)
                {
                    // *            CR( 1, 2 ) = CA*A( 2, 1 )
                    // *            CR( 2, 1 ) = CA*A( 1, 2 )
                    
                    CRV[3 + o_crv] = CA * A[2+1 * LDA + o_a];
                    CRV[2 + o_crv] = CA * A[1+2 * LDA + o_a];
                    
                }
                else
                {
                    // *            CR( 2, 1 ) = CA*A( 2, 1 )
                    // *            CR( 1, 2 ) = CA*A( 1, 2 )
                    
                    
                    CRV[2 + o_crv] = CA * A[2+1 * LDA + o_a];
                    CRV[3 + o_crv] = CA * A[1+2 * LDA + o_a];
                    
                }
                
                // *-----------------------------TERMINA MI MODIFICACION-------------------------
                
                
                
                // *
                if (NW == 1)
                {
                    // *
                    // *           Real 2x2 system  (w is real)
                    // *
                    // *           Find the largest element in C
                    // *
                    CMAX = ZERO;
                    ICMAX = 0;
                    // *
                    for (J = 1; J <= 4; J++)
                    {
                        if (Math.Abs(CRV[J + o_crv]) > CMAX)
                        {
                            CMAX = Math.Abs(CRV[J + o_crv]);
                            ICMAX = J;
                        }
                    }
                    // *
                    // *           If norm(C) < SMINI, use SMINI*identity.
                    // *
                    if (CMAX < SMINI)
                    {
                        BNORM = Math.Max(Math.Abs(B[1+1 * LDB + o_b]), Math.Abs(B[2+1 * LDB + o_b]));
                        if (SMINI < ONE && BNORM > ONE)
                        {
                            if (BNORM > BIGNUM * SMINI) SCALE = ONE / BNORM;
                        }
                        TEMP = SCALE / SMINI;
                        X[1+1 * LDX + o_x] = TEMP * B[1+1 * LDB + o_b];
                        X[2+1 * LDX + o_x] = TEMP * B[2+1 * LDB + o_b];
                        XNORM = TEMP * BNORM;
                        INFO = 1;
                        return;
                    }
                    // *
                    // *           Gaussian elimination with complete pivoting.
                    // *
                    UR11 = CRV[ICMAX + o_crv];
                    CR21 = CRV[IPIVOT[2+ICMAX * 4 + o_ipivot] + o_crv];
                    UR12 = CRV[IPIVOT[3+ICMAX * 4 + o_ipivot] + o_crv];
                    CR22 = CRV[IPIVOT[4+ICMAX * 4 + o_ipivot] + o_crv];
                    UR11R = ONE / UR11;
                    LR21 = UR11R * CR21;
                    UR22 = CR22 - UR12 * LR21;
                    // *
                    // *           If smaller pivot < SMINI, use SMINI
                    // *
                    if (Math.Abs(UR22) < SMINI)
                    {
                        UR22 = SMINI;
                        INFO = 1;
                    }
                    if (RSWAP[ICMAX + o_rswap])
                    {
                        BR1 = B[2+1 * LDB + o_b];
                        BR2 = B[1+1 * LDB + o_b];
                    }
                    else
                    {
                        BR1 = B[1+1 * LDB + o_b];
                        BR2 = B[2+1 * LDB + o_b];
                    }
                    BR2 +=  - LR21 * BR1;
                    BBND = Math.Max(Math.Abs(BR1 * (UR22 * UR11R)), Math.Abs(BR2));
                    if (BBND > ONE && Math.Abs(UR22) < ONE)
                    {
                        if (BBND >= BIGNUM * Math.Abs(UR22)) SCALE = ONE / BBND;
                    }
                    // *
                    XR2 = (BR2 * SCALE) / UR22;
                    XR1 = (SCALE * BR1) * UR11R - XR2 * (UR11R * UR12);
                    if (ZSWAP[ICMAX + o_zswap])
                    {
                        X[1+1 * LDX + o_x] = XR2;
                        X[2+1 * LDX + o_x] = XR1;
                    }
                    else
                    {
                        X[1+1 * LDX + o_x] = XR1;
                        X[2+1 * LDX + o_x] = XR2;
                    }
                    XNORM = Math.Max(Math.Abs(XR1), Math.Abs(XR2));
                    // *
                    // *           Further scaling if  norm(A) norm(X) > overflow
                    // *
                    if (XNORM > ONE && CMAX > ONE)
                    {
                        if (XNORM > BIGNUM / CMAX)
                        {
                            TEMP = CMAX / BIGNUM;
                            X[1+1 * LDX + o_x] *= TEMP;
                            X[2+1 * LDX + o_x] *= TEMP;
                            XNORM *= TEMP;
                            SCALE *= TEMP;
                        }
                    }
                }
                else
                {
                    // *
                    // *           Complex 2x2 system  (w is complex)
                    // *
                    // *           Find the largest element in C
                    // *
                    
                    
                    // *-----------------------------INICIA MI MODIFICACION--------------------------
                    // *            CI( 1, 1 ) = -WI*D1
                    // *            CI( 2, 1 ) = ZERO
                    // *            CI( 1, 2 ) = ZERO
                    // *            CI( 2, 2 ) = -WI*D2
                    CIV[1 + o_civ] =  - WI * D1;
                    CIV[2 + o_civ] = ZERO;
                    CIV[3 + o_civ] = ZERO;
                    CIV[4 + o_civ] =  - WI * D2;
                    // *-----------------------------TERMINA MI MODIFICACION-------------------------
                    
                    
                    
                    
                    
                    CMAX = ZERO;
                    ICMAX = 0;
                    // *
                    for (J = 1; J <= 4; J++)
                    {
                        if (Math.Abs(CRV[J + o_crv]) + Math.Abs(CIV[J + o_civ]) > CMAX)
                        {
                            CMAX = Math.Abs(CRV[J + o_crv]) + Math.Abs(CIV[J + o_civ]);
                            ICMAX = J;
                        }
                    }
                    // *
                    // *           If norm(C) < SMINI, use SMINI*identity.
                    // *
                    if (CMAX < SMINI)
                    {
                        BNORM = Math.Max(Math.Abs(B[1+1 * LDB + o_b]) + Math.Abs(B[1+2 * LDB + o_b]), Math.Abs(B[2+1 * LDB + o_b]) + Math.Abs(B[2+2 * LDB + o_b]));
                        if (SMINI < ONE && BNORM > ONE)
                        {
                            if (BNORM > BIGNUM * SMINI) SCALE = ONE / BNORM;
                        }
                        TEMP = SCALE / SMINI;
                        X[1+1 * LDX + o_x] = TEMP * B[1+1 * LDB + o_b];
                        X[2+1 * LDX + o_x] = TEMP * B[2+1 * LDB + o_b];
                        X[1+2 * LDX + o_x] = TEMP * B[1+2 * LDB + o_b];
                        X[2+2 * LDX + o_x] = TEMP * B[2+2 * LDB + o_b];
                        XNORM = TEMP * BNORM;
                        INFO = 1;
                        return;
                    }
                    // *
                    // *           Gaussian elimination with complete pivoting.
                    // *
                    UR11 = CRV[ICMAX + o_crv];
                    UI11 = CIV[ICMAX + o_civ];
                    CR21 = CRV[IPIVOT[2+ICMAX * 4 + o_ipivot] + o_crv];
                    CI21 = CIV[IPIVOT[2+ICMAX * 4 + o_ipivot] + o_civ];
                    UR12 = CRV[IPIVOT[3+ICMAX * 4 + o_ipivot] + o_crv];
                    UI12 = CIV[IPIVOT[3+ICMAX * 4 + o_ipivot] + o_civ];
                    CR22 = CRV[IPIVOT[4+ICMAX * 4 + o_ipivot] + o_crv];
                    CI22 = CIV[IPIVOT[4+ICMAX * 4 + o_ipivot] + o_civ];
                    if (ICMAX == 1 || ICMAX == 4)
                    {
                        // *
                        // *              Code when off-diagonals of pivoted C are real
                        // *
                        if (Math.Abs(UR11) > Math.Abs(UI11))
                        {
                            TEMP = UI11 / UR11;
                            UR11R = ONE / (UR11 * (ONE + Math.Pow(TEMP,2)));
                            UI11R =  - TEMP * UR11R;
                        }
                        else
                        {
                            TEMP = UR11 / UI11;
                            UI11R =  - ONE / (UI11 * (ONE + Math.Pow(TEMP,2)));
                            UR11R =  - TEMP * UI11R;
                        }
                        LR21 = CR21 * UR11R;
                        LI21 = CR21 * UI11R;
                        UR12S = UR12 * UR11R;
                        UI12S = UR12 * UI11R;
                        UR22 = CR22 - UR12 * LR21;
                        UI22 = CI22 - UR12 * LI21;
                    }
                    else
                    {
                        // *
                        // *              Code when diagonals of pivoted C are real
                        // *
                        UR11R = ONE / UR11;
                        UI11R = ZERO;
                        LR21 = CR21 * UR11R;
                        LI21 = CI21 * UR11R;
                        UR12S = UR12 * UR11R;
                        UI12S = UI12 * UR11R;
                        UR22 = CR22 - UR12 * LR21 + UI12 * LI21;
                        UI22 =  - UR12 * LI21 - UI12 * LR21;
                    }
                    U22ABS = Math.Abs(UR22) + Math.Abs(UI22);
                    // *
                    // *           If smaller pivot < SMINI, use SMINI
                    // *
                    if (U22ABS < SMINI)
                    {
                        UR22 = SMINI;
                        UI22 = ZERO;
                        INFO = 1;
                    }
                    if (RSWAP[ICMAX + o_rswap])
                    {
                        BR2 = B[1+1 * LDB + o_b];
                        BR1 = B[2+1 * LDB + o_b];
                        BI2 = B[1+2 * LDB + o_b];
                        BI1 = B[2+2 * LDB + o_b];
                    }
                    else
                    {
                        BR1 = B[1+1 * LDB + o_b];
                        BR2 = B[2+1 * LDB + o_b];
                        BI1 = B[1+2 * LDB + o_b];
                        BI2 = B[2+2 * LDB + o_b];
                    }
                    BR2 +=  - LR21 * BR1 + LI21 * BI1;
                    BI2 +=  - LI21 * BR1 - LR21 * BI1;
                    BBND = Math.Max((Math.Abs(BR1) + Math.Abs(BI1)) * (U22ABS * (Math.Abs(UR11R) + Math.Abs(UI11R))), Math.Abs(BR2) + Math.Abs(BI2));
                    if (BBND > ONE && U22ABS < ONE)
                    {
                        if (BBND >= BIGNUM * U22ABS)
                        {
                            SCALE = ONE / BBND;
                            BR1 *= SCALE;
                            BI1 *= SCALE;
                            BR2 *= SCALE;
                            BI2 *= SCALE;
                        }
                    }
                    // *
                    this._dladiv.Run(BR2, BI2, UR22, UI22, ref XR2, ref XI2);
                    XR1 = UR11R * BR1 - UI11R * BI1 - UR12S * XR2 + UI12S * XI2;
                    XI1 = UI11R * BR1 + UR11R * BI1 - UI12S * XR2 - UR12S * XI2;
                    if (ZSWAP[ICMAX + o_zswap])
                    {
                        X[1+1 * LDX + o_x] = XR2;
                        X[2+1 * LDX + o_x] = XR1;
                        X[1+2 * LDX + o_x] = XI2;
                        X[2+2 * LDX + o_x] = XI1;
                    }
                    else
                    {
                        X[1+1 * LDX + o_x] = XR1;
                        X[2+1 * LDX + o_x] = XR2;
                        X[1+2 * LDX + o_x] = XI1;
                        X[2+2 * LDX + o_x] = XI2;
                    }
                    XNORM = Math.Max(Math.Abs(XR1) + Math.Abs(XI1), Math.Abs(XR2) + Math.Abs(XI2));
                    // *
                    // *           Further scaling if  norm(A) norm(X) > overflow
                    // *
                    if (XNORM > ONE && CMAX > ONE)
                    {
                        if (XNORM > BIGNUM / CMAX)
                        {
                            TEMP = CMAX / BIGNUM;
                            X[1+1 * LDX + o_x] *= TEMP;
                            X[2+1 * LDX + o_x] *= TEMP;
                            X[1+2 * LDX + o_x] *= TEMP;
                            X[2+2 * LDX + o_x] *= TEMP;
                            XNORM *= TEMP;
                            SCALE *= TEMP;
                        }
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DLALN2
            // *

            #endregion

        }
    }
}
