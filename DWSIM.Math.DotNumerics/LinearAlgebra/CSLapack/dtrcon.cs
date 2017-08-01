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
    /// -- LAPACK routine (version 3.0) --
    /// Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
    /// Courant Institute, Argonne National Lab, and Rice University
    /// March 31, 1993
    /// Purpose
    /// =======
    /// 
    /// DTRCON estimates the reciprocal of the condition number of a
    /// triangular matrix A, in either the 1-norm or the infinity-norm.
    /// 
    /// The norm of A is computed and an estimate is obtained for
    /// norm(inv(A)), then the reciprocal of the condition number is
    /// computed as
    /// RCOND = 1 / ( norm(A) * norm(inv(A)) ).
    /// 
    ///</summary>
    public class DTRCON
    {
    

        #region Dependencies
        
        LSAME _lsame; IDAMAX _idamax; DLAMCH _dlamch; DLANTR _dlantr; DLACON _dlacon; DLATRS _dlatrs; DRSCL _drscl; 
        XERBLA _xerbla;

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DTRCON(LSAME lsame, IDAMAX idamax, DLAMCH dlamch, DLANTR dlantr, DLACON dlacon, DLATRS dlatrs, DRSCL drscl, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._dlamch = dlamch; this._dlantr = dlantr; this._dlacon = dlacon; 
            this._dlatrs = dlatrs;this._drscl = drscl; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTRCON()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IDAMAX idamax = new IDAMAX();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            DASUM dasum = new DASUM();
            DCOPY dcopy = new DCOPY();
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            DLABAD dlabad = new DLABAD();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANTR dlantr = new DLANTR(dlassq, lsame);
            DLACON dlacon = new DLACON(idamax, dasum, dcopy);
            DTRSV dtrsv = new DTRSV(lsame, xerbla);
            DLATRS dlatrs = new DLATRS(lsame, idamax, dasum, ddot, dlamch, daxpy, dscal, dtrsv, xerbla);
            DRSCL drscl = new DRSCL(dlamch, dscal, dlabad);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._dlamch = dlamch; this._dlantr = dlantr; this._dlacon = dlacon; 
            this._dlatrs = dlatrs;this._drscl = drscl; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTRCON estimates the reciprocal of the condition number of a
        /// triangular matrix A, in either the 1-norm or the infinity-norm.
        /// 
        /// The norm of A is computed and an estimate is obtained for
        /// norm(inv(A)), then the reciprocal of the condition number is
        /// computed as
        /// RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        /// 
        ///</summary>
        /// <param name="NORM">
        /// (input) CHARACTER*1
        /// Specifies whether the 1-norm condition number or the
        /// infinity-norm condition number is required:
        /// = '1' or 'O':  1-norm;
        /// = 'I':         Infinity-norm.
        ///</param>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  A is upper triangular;
        /// = 'L':  A is lower triangular.
        ///</param>
        /// <param name="DIAG">
        /// (input) CHARACTER*1
        /// = 'N':  A is non-unit triangular;
        /// = 'U':  A is unit triangular.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension (LDA,N)
        /// The triangular matrix A.  If UPLO = 'U', the leading N-by-N
        /// upper triangular part of the array A contains the upper
        /// triangular matrix, and the strictly lower triangular part of
        /// A is not referenced.  If UPLO = 'L', the leading N-by-N lower
        /// triangular part of the array A contains the lower triangular
        /// matrix, and the strictly upper triangular part of A is not
        /// referenced.  If DIAG = 'U', the diagonal elements of A are
        /// also not referenced and are assumed to be 1.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="RCOND">
        /// (output) DOUBLE PRECISION
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(norm(A) * norm(inv(A))).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (3*N)
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string NORM, string UPLO, string DIAG, int N, double[] A, int offset_a, int LDA
                         , ref double RCOND, ref double[] WORK, int offset_work, ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            bool NOUNIT = false; bool ONENRM = false; bool UPPER = false; string NORMIN = new string(' ', 1); int IX = 0; 
            int KASE = 0;int KASE1 = 0; double AINVNM = 0; double ANORM = 0; double SCALE = 0; double SMLNUM = 0; 
            double XNORM = 0;

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_work = -1 + offset_work;  int o_iwork = -1 + offset_iwork; 

            #endregion


            #region Strings
            
            NORM = NORM.Substring(0, 1);  UPLO = UPLO.Substring(0, 1);  DIAG = DIAG.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.0) --
            // *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
            // *     Courant Institute, Argonne National Lab, and Rice University
            // *     March 31, 1993
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DTRCON estimates the reciprocal of the condition number of a
            // *  triangular matrix A, in either the 1-norm or the infinity-norm.
            // *
            // *  The norm of A is computed and an estimate is obtained for
            // *  norm(inv(A)), then the reciprocal of the condition number is
            // *  computed as
            // *     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
            // *
            // *  Arguments
            // *  =========
            // *
            // *  NORM    (input) CHARACTER*1
            // *          Specifies whether the 1-norm condition number or the
            // *          infinity-norm condition number is required:
            // *          = '1' or 'O':  1-norm;
            // *          = 'I':         Infinity-norm.
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  A is upper triangular;
            // *          = 'L':  A is lower triangular.
            // *
            // *  DIAG    (input) CHARACTER*1
            // *          = 'N':  A is non-unit triangular;
            // *          = 'U':  A is unit triangular.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The triangular matrix A.  If UPLO = 'U', the leading N-by-N
            // *          upper triangular part of the array A contains the upper
            // *          triangular matrix, and the strictly lower triangular part of
            // *          A is not referenced.  If UPLO = 'L', the leading N-by-N lower
            // *          triangular part of the array A contains the lower triangular
            // *          matrix, and the strictly upper triangular part of A is not
            // *          referenced.  If DIAG = 'U', the diagonal elements of A are
            // *          also not referenced and are assumed to be 1.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  RCOND   (output) DOUBLE PRECISION
            // *          The reciprocal of the condition number of the matrix A,
            // *          computed as RCOND = 1/(norm(A) * norm(inv(A))).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
            // *
            // *  IWORK   (workspace) INTEGER array, dimension (N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, DBLE, MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            UPPER = this._lsame.Run(UPLO, "U");
            ONENRM = NORM == "1" || this._lsame.Run(NORM, "O");
            NOUNIT = this._lsame.Run(DIAG, "N");
            // *
            if (!ONENRM && !this._lsame.Run(NORM, "I"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!UPPER && !this._lsame.Run(UPLO, "L"))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (!NOUNIT && !this._lsame.Run(DIAG, "U"))
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (N < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDA < Math.Max(1, N))
                            {
                                INFO =  - 6;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTRCON",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0)
            {
                RCOND = ONE;
                return;
            }
            // *
            RCOND = ZERO;
            SMLNUM = this._dlamch.Run("Safe minimum") * Convert.ToDouble(Math.Max(1, N));
            // *
            // *     Compute the norm of the triangular matrix A.
            // *
            ANORM = this._dlantr.Run(NORM, UPLO, DIAG, N, N, A, offset_a, LDA, ref WORK, offset_work);
            // *
            // *     Continue only if ANORM > 0.
            // *
            if (ANORM > ZERO)
            {
                // *
                // *        Estimate the norm of the inverse of A.
                // *
                AINVNM = ZERO;
                FortranLib.Copy(ref NORMIN , "N");
                if (ONENRM)
                {
                    KASE1 = 1;
                }
                else
                {
                    KASE1 = 2;
                }
                KASE = 0;
            LABEL10:;
                this._dlacon.Run(N, ref WORK, N + 1 + o_work, ref WORK, offset_work, ref IWORK, offset_iwork, ref AINVNM, ref KASE);
                if (KASE != 0)
                {
                    if (KASE == KASE1)
                    {
                        // *
                        // *              Multiply by inv(A).
                        // *
                        this._dlatrs.Run(UPLO, "No transpose", DIAG, NORMIN, N, A, offset_a
                                         , LDA, ref WORK, offset_work, ref SCALE, ref WORK, 2 * N + 1 + o_work, ref INFO);
                    }
                    else
                    {
                        // *
                        // *              Multiply by inv(A').
                        // *
                        this._dlatrs.Run(UPLO, "Transpose", DIAG, NORMIN, N, A, offset_a
                                         , LDA, ref WORK, offset_work, ref SCALE, ref WORK, 2 * N + 1 + o_work, ref INFO);
                    }
                    FortranLib.Copy(ref NORMIN , "Y");
                    // *
                    // *           Multiply by 1/SCALE if doing so will not cause overflow.
                    // *
                    if (SCALE != ONE)
                    {
                        IX = this._idamax.Run(N, WORK, offset_work, 1);
                        XNORM = Math.Abs(WORK[IX + o_work]);
                        if (SCALE < XNORM * SMLNUM || SCALE == ZERO) goto LABEL20;
                        this._drscl.Run(N, SCALE, ref WORK, offset_work, 1);
                    }
                    goto LABEL10;
                }
                // *
                // *        Compute the estimate of the reciprocal condition number.
                // *
                if (AINVNM != ZERO) RCOND = (ONE / ANORM) / AINVNM;
            }
            // *
        LABEL20:;
            return;
            // *
            // *     End of DTRCON
            // *

            #endregion

        }
    }
}
