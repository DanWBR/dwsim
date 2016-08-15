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
    /// -- LAPACK routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DSYTRD reduces a real symmetric matrix A to real symmetric
    /// tridiagonal form T by an orthogonal similarity transformation:
    /// Q**T * A * Q = T.
    /// 
    ///</summary>
    public class DSYTRD
    {
    

        #region Dependencies
        
        DLATRD _dlatrd; DSYR2K _dsyr2k; DSYTD2 _dsytd2; XERBLA _xerbla; LSAME _lsame; ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DSYTRD(DLATRD dlatrd, DSYR2K dsyr2k, DSYTD2 dsytd2, XERBLA xerbla, LSAME lsame, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dlatrd = dlatrd; this._dsyr2k = dsyr2k; this._dsytd2 = dsytd2; this._xerbla = xerbla; this._lsame = lsame; 
            this._ilaenv = ilaenv;

            #endregion

        }
    
        public DSYTRD()
        {
    

            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DDOT ddot = new DDOT();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DSYMV dsymv = new DSYMV(lsame, xerbla);
            DLATRD dlatrd = new DLATRD(daxpy, dgemv, dlarfg, dscal, dsymv, lsame, ddot);
            DSYR2K dsyr2k = new DSYR2K(lsame, xerbla);
            DSYR2 dsyr2 = new DSYR2(lsame, xerbla);
            DSYTD2 dsytd2 = new DSYTD2(daxpy, dlarfg, dsymv, dsyr2, xerbla, lsame, ddot);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);

            #endregion


            #region Set Dependencies
            
            this._dlatrd = dlatrd; this._dsyr2k = dsyr2k; this._dsytd2 = dsytd2; this._xerbla = xerbla; this._lsame = lsame; 
            this._ilaenv = ilaenv;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSYTRD reduces a real symmetric matrix A to real symmetric
        /// tridiagonal form T by an orthogonal similarity transformation:
        /// Q**T * A * Q = T.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  Upper triangle of A is stored;
        /// = 'L':  Lower triangle of A is stored.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the symmetric matrix A.  If UPLO = 'U', the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = 'L', the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// On exit, if UPLO = 'U', the diagonal and first superdiagonal
        /// of A are overwritten by the corresponding elements of the
        /// tridiagonal matrix T, and the elements above the first
        /// superdiagonal, with the array TAU, represent the orthogonal
        /// matrix Q as a product of elementary reflectors; if UPLO
        /// = 'L', the diagonal and first subdiagonal of A are over-
        /// written by the corresponding elements of the tridiagonal
        /// matrix T, and the elements below the first subdiagonal, with
        /// the array TAU, represent the orthogonal matrix Q as a product
        /// of elementary reflectors. See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// The diagonal elements of the tridiagonal matrix T:
        /// D(i) = A(i,i).
        ///</param>
        /// <param name="E">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// The off-diagonal elements of the tridiagonal matrix T:
        /// E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// The scalar factors of the elementary reflectors (see Further
        /// Details).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.  LWORK .GE. 1.
        /// For optimum performance LWORK .GE. N*NB, where NB is the
        /// optimal blocksize.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string UPLO, int N, ref double[] A, int offset_a, int LDA, ref double[] D, int offset_d, ref double[] E, int offset_e
                         , ref double[] TAU, int offset_tau, ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; bool UPPER = false; int I = 0; int IINFO = 0; int IWS = 0; int J = 0; int KK = 0; int LDWORK = 0; 
            int LWKOPT = 0;int NB = 0; int NBMIN = 0; int NX = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_tau = -1 + offset_tau; 
             int o_work = -1 + offset_work;

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
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
            // *  DSYTRD reduces a real symmetric matrix A to real symmetric
            // *  tridiagonal form T by an orthogonal similarity transformation:
            // *  Q**T * A * Q = T.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  Upper triangle of A is stored;
            // *          = 'L':  Lower triangle of A is stored.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
            // *          N-by-N upper triangular part of A contains the upper
            // *          triangular part of the matrix A, and the strictly lower
            // *          triangular part of A is not referenced.  If UPLO = 'L', the
            // *          leading N-by-N lower triangular part of A contains the lower
            // *          triangular part of the matrix A, and the strictly upper
            // *          triangular part of A is not referenced.
            // *          On exit, if UPLO = 'U', the diagonal and first superdiagonal
            // *          of A are overwritten by the corresponding elements of the
            // *          tridiagonal matrix T, and the elements above the first
            // *          superdiagonal, with the array TAU, represent the orthogonal
            // *          matrix Q as a product of elementary reflectors; if UPLO
            // *          = 'L', the diagonal and first subdiagonal of A are over-
            // *          written by the corresponding elements of the tridiagonal
            // *          matrix T, and the elements below the first subdiagonal, with
            // *          the array TAU, represent the orthogonal matrix Q as a product
            // *          of elementary reflectors. See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension (N)
            // *          The diagonal elements of the tridiagonal matrix T:
            // *          D(i) = A(i,i).
            // *
            // *  E       (output) DOUBLE PRECISION array, dimension (N-1)
            // *          The off-diagonal elements of the tridiagonal matrix T:
            // *          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
            // *          The scalar factors of the elementary reflectors (see Further
            // *          Details).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.  LWORK >= 1.
            // *          For optimum performance LWORK >= N*NB, where NB is the
            // *          optimal blocksize.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  If UPLO = 'U', the matrix Q is represented as a product of elementary
            // *  reflectors
            // *
            // *     Q = H(n-1) . . . H(2) H(1).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
            // *  A(1:i-1,i+1), and tau in TAU(i).
            // *
            // *  If UPLO = 'L', the matrix Q is represented as a product of elementary
            // *  reflectors
            // *
            // *     Q = H(1) H(2) . . . H(n-1).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
            // *  and tau in TAU(i).
            // *
            // *  The contents of A on exit are illustrated by the following examples
            // *  with n = 5:
            // *
            // *  if UPLO = 'U':                       if UPLO = 'L':
            // *
            // *    (  d   e   v2  v3  v4 )              (  d                  )
            // *    (      d   e   v3  v4 )              (  e   d              )
            // *    (          d   e   v4 )              (  v1  e   d          )
            // *    (              d   e  )              (  v1  v2  e   d      )
            // *    (                  d  )              (  v1  v2  v3  e   d  )
            // *
            // *  where d and e denote diagonal and off-diagonal elements of T, and vi
            // *  denotes an element of the vector defining H(i).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
            UPPER = this._lsame.Run(UPLO, "U");
            LQUERY = (LWORK ==  - 1);
            if (!UPPER && !this._lsame.Run(UPLO, "L"))
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (LDA < Math.Max(1, N))
                    {
                        INFO =  - 4;
                    }
                    else
                    {
                        if (LWORK < 1 && !LQUERY)
                        {
                            INFO =  - 9;
                        }
                    }
                }
            }
            // *
            if (INFO == 0)
            {
                // *
                // *        Determine the block size.
                // *
                NB = this._ilaenv.Run(1, "DSYTRD", UPLO, N,  - 1,  - 1,  - 1);
                LWKOPT = N * NB;
                WORK[1 + o_work] = LWKOPT;
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DSYTRD",  - INFO);
                return;
            }
            else
            {
                if (LQUERY)
                {
                    return;
                }
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0)
            {
                WORK[1 + o_work] = 1;
                return;
            }
            // *
            NX = N;
            IWS = 1;
            if (NB > 1 && NB < N)
            {
                // *
                // *        Determine when to cross over from blocked to unblocked code
                // *        (last block is always handled by unblocked code).
                // *
                NX = Math.Max(NB, this._ilaenv.Run(3, "DSYTRD", UPLO, N,  - 1,  - 1,  - 1));
                if (NX < N)
                {
                    // *
                    // *           Determine if workspace is large enough for blocked code.
                    // *
                    LDWORK = N;
                    IWS = LDWORK * NB;
                    if (LWORK < IWS)
                    {
                        // *
                        // *              Not enough workspace to use optimal NB:  determine the
                        // *              minimum value of NB, and reduce NB or force use of
                        // *              unblocked code by setting NX = N.
                        // *
                        NB = Math.Max(LWORK / LDWORK, 1);
                        NBMIN = this._ilaenv.Run(2, "DSYTRD", UPLO, N,  - 1,  - 1,  - 1);
                        if (NB < NBMIN) NX = N;
                    }
                }
                else
                {
                    NX = N;
                }
            }
            else
            {
                NB = 1;
            }
            // *
            if (UPPER)
            {
                // *
                // *        Reduce the upper triangle of A.
                // *        Columns 1:kk are handled by the unblocked method.
                // *
                KK = N - ((N - NX + NB - 1) / NB) * NB;
                for (I = N - NB + 1; ( - NB >= 0) ? (I <= KK + 1) : (I >= KK + 1); I +=  - NB)
                {
                    // *
                    // *           Reduce columns i:i+nb-1 to tridiagonal form and form the
                    // *           matrix W which is needed to update the unreduced part of
                    // *           the matrix
                    // *
                    this._dlatrd.Run(UPLO, I + NB - 1, NB, ref A, offset_a, LDA, ref E, offset_e
                                     , ref TAU, offset_tau, ref WORK, offset_work, LDWORK);
                    // *
                    // *           Update the unreduced submatrix A(1:i-1,1:i-1), using an
                    // *           update of the form:  A := A - V*W' - W*V'
                    // *
                    this._dsyr2k.Run(UPLO, "No transpose", I - 1, NB,  - ONE, A, 1+I * LDA + o_a
                                     , LDA, WORK, offset_work, LDWORK, ONE, ref A, offset_a, LDA);
                    // *
                    // *           Copy superdiagonal elements back into A, and diagonal
                    // *           elements into D
                    // *
                    for (J = I; J <= I + NB - 1; J++)
                    {
                        A[J - 1+J * LDA + o_a] = E[J - 1 + o_e];
                        D[J + o_d] = A[J+J * LDA + o_a];
                    }
                }
                // *
                // *        Use unblocked code to reduce the last or only block
                // *
                this._dsytd2.Run(UPLO, KK, ref A, offset_a, LDA, ref D, offset_d, ref E, offset_e
                                 , ref TAU, offset_tau, ref IINFO);
            }
            else
            {
                // *
                // *        Reduce the lower triangle of A
                // *
                for (I = 1; (NB >= 0) ? (I <= N - NX) : (I >= N - NX); I += NB)
                {
                    // *
                    // *           Reduce columns i:i+nb-1 to tridiagonal form and form the
                    // *           matrix W which is needed to update the unreduced part of
                    // *           the matrix
                    // *
                    this._dlatrd.Run(UPLO, N - I + 1, NB, ref A, I+I * LDA + o_a, LDA, ref E, I + o_e
                                     , ref TAU, I + o_tau, ref WORK, offset_work, LDWORK);
                    // *
                    // *           Update the unreduced submatrix A(i+ib:n,i+ib:n), using
                    // *           an update of the form:  A := A - V*W' - W*V'
                    // *
                    this._dsyr2k.Run(UPLO, "No transpose", N - I - NB + 1, NB,  - ONE, A, I + NB+I * LDA + o_a
                                     , LDA, WORK, NB + 1 + o_work, LDWORK, ONE, ref A, I + NB+(I + NB) * LDA + o_a, LDA);
                    // *
                    // *           Copy subdiagonal elements back into A, and diagonal
                    // *           elements into D
                    // *
                    for (J = I; J <= I + NB - 1; J++)
                    {
                        A[J + 1+J * LDA + o_a] = E[J + o_e];
                        D[J + o_d] = A[J+J * LDA + o_a];
                    }
                }
                // *
                // *        Use unblocked code to reduce the last or only block
                // *
                this._dsytd2.Run(UPLO, N - I + 1, ref A, I+I * LDA + o_a, LDA, ref D, I + o_d, ref E, I + o_e
                                 , ref TAU, I + o_tau, ref IINFO);
            }
            // *
            WORK[1 + o_work] = LWKOPT;
            return;
            // *
            // *     End of DSYTRD
            // *

            #endregion

        }
    }
}
