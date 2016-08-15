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
    /// DGETRI computes the inverse of a matrix using the LU factorization
    /// computed by DGETRF.
    /// 
    /// This method inverts U and then computes inv(A) by solving the system
    /// inv(A)*L = inv(U) for inv(A).
    /// 
    ///</summary>
    public class DGETRI
    {
    

        #region Dependencies
        
        ILAENV _ilaenv; DGEMM _dgemm; DGEMV _dgemv; DSWAP _dswap; DTRSM _dtrsm; DTRTRI _dtrtri; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DGETRI(ILAENV ilaenv, DGEMM dgemm, DGEMV dgemv, DSWAP dswap, DTRSM dtrsm, DTRTRI dtrtri, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._ilaenv = ilaenv; this._dgemm = dgemm; this._dgemv = dgemv; this._dswap = dswap; this._dtrsm = dtrsm; 
            this._dtrtri = dtrtri;this._xerbla = xerbla; 

            #endregion

        }
    
        public DGETRI()
        {
    

            #region Dependencies (Initialization)
            
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DSWAP dswap = new DSWAP();
            DSCAL dscal = new DSCAL();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DTRTI2 dtrti2 = new DTRTI2(lsame, dscal, dtrmv, xerbla);
            DTRTRI dtrtri = new DTRTRI(lsame, ilaenv, dtrmm, dtrsm, dtrti2, xerbla);

            #endregion


            #region Set Dependencies
            
            this._ilaenv = ilaenv; this._dgemm = dgemm; this._dgemv = dgemv; this._dswap = dswap; this._dtrsm = dtrsm; 
            this._dtrtri = dtrtri;this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGETRI computes the inverse of a matrix using the LU factorization
        /// computed by DGETRF.
        /// 
        /// This method inverts U and then computes inv(A) by solving the system
        /// inv(A)*L = inv(U) for inv(A).
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the factors L and U from the factorization
        /// A = P*L*U as computed by DGETRF.
        /// On exit, if INFO = 0, the inverse of the original matrix A.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="IPIV">
        /// (input) INTEGER array, dimension (N)
        /// The pivot indices from DGETRF; for 1.LE.i.LE.N, row i of the
        /// matrix was interchanged with row IPIV(i).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO=0, then WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.  LWORK .GE. max(1,N).
        /// For optimal performance LWORK .GE. N*NB, where NB is
        /// the optimal blocksize returned by ILAENV.
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
        /// .GT. 0:  if INFO = i, U(i,i) is exactly zero; the matrix is
        /// singular and its inverse could not be computed.
        ///</param>
        public void Run(int N, ref double[] A, int offset_a, int LDA, int[] IPIV, int offset_ipiv, ref double[] WORK, int offset_work, int LWORK
                         , ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int I = 0; int IWS = 0; int J = 0; int JB = 0; int JJ = 0; int JP = 0; int LDWORK = 0; 
            int LWKOPT = 0;int NB = 0; int NBMIN = 0; int NN = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; int A_JJ = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_ipiv = -1 + offset_ipiv;  int o_work = -1 + offset_work; 

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
            // *  DGETRI computes the inverse of a matrix using the LU factorization
            // *  computed by DGETRF.
            // *
            // *  This method inverts U and then computes inv(A) by solving the system
            // *  inv(A)*L = inv(U) for inv(A).
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the factors L and U from the factorization
            // *          A = P*L*U as computed by DGETRF.
            // *          On exit, if INFO = 0, the inverse of the original matrix A.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  IPIV    (input) INTEGER array, dimension (N)
            // *          The pivot indices from DGETRF; for 1<=i<=N, row i of the
            // *          matrix was interchanged with row IPIV(i).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO=0, then WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.  LWORK >= max(1,N).
            // *          For optimal performance LWORK >= N*NB, where NB is
            // *          the optimal blocksize returned by ILAENV.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  if INFO = i, U(i,i) is exactly zero; the matrix is
            // *                singular and its inverse could not be computed.
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
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            NB = this._ilaenv.Run(1, "DGETRI", " ", N,  - 1,  - 1,  - 1);
            LWKOPT = N * NB;
            WORK[1 + o_work] = LWKOPT;
            LQUERY = (LWORK ==  - 1);
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (LDA < Math.Max(1, N))
                {
                    INFO =  - 3;
                }
                else
                {
                    if (LWORK < Math.Max(1, N) && !LQUERY)
                    {
                        INFO =  - 6;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGETRI",  - INFO);
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
            if (N == 0) return;
            // *
            // *     Form inv(U).  If INFO > 0 from DTRTRI, then U is singular,
            // *     and the inverse is not computed.
            // *
            this._dtrtri.Run("Upper", "Non-unit", N, ref A, offset_a, LDA, ref INFO);
            if (INFO > 0) return;
            // *
            NBMIN = 2;
            LDWORK = N;
            if (NB > 1 && NB < N)
            {
                IWS = Math.Max(LDWORK * NB, 1);
                if (LWORK < IWS)
                {
                    NB = LWORK / LDWORK;
                    NBMIN = Math.Max(2, this._ilaenv.Run(2, "DGETRI", " ", N,  - 1,  - 1,  - 1));
                }
            }
            else
            {
                IWS = N;
            }
            // *
            // *     Solve the equation inv(A)*L = inv(U) for inv(A).
            // *
            if (NB < NBMIN || NB >= N)
            {
                // *
                // *        Use unblocked code.
                // *
                for (J = N; J >= 1; J +=  - 1)
                {
                    // *
                    // *           Copy current column of L to WORK and replace with zeros.
                    // *
                    A_J = J * LDA + o_a;
                    for (I = J + 1; I <= N; I++)
                    {
                        WORK[I + o_work] = A[I + A_J];
                        A[I + A_J] = ZERO;
                    }
                    // *
                    // *           Compute current column of inv(A).
                    // *
                    if (J < N)
                    {
                        this._dgemv.Run("No transpose", N, N - J,  - ONE, A, 1+(J + 1) * LDA + o_a, LDA
                                        , WORK, J + 1 + o_work, 1, ONE, ref A, 1+J * LDA + o_a, 1);
                    }
                }
            }
            else
            {
                // *
                // *        Use blocked code.
                // *
                NN = ((N - 1) / NB) * NB + 1;
                for (J = NN; ( - NB >= 0) ? (J <= 1) : (J >= 1); J +=  - NB)
                {
                    JB = Math.Min(NB, N - J + 1);
                    // *
                    // *           Copy current block column of L to WORK and replace with
                    // *           zeros.
                    // *
                    for (JJ = J; JJ <= J + JB - 1; JJ++)
                    {
                        A_JJ = JJ * LDA + o_a;
                        for (I = JJ + 1; I <= N; I++)
                        {
                            WORK[I + (JJ - J) * LDWORK + o_work] = A[I + A_JJ];
                            A[I + A_JJ] = ZERO;
                        }
                    }
                    // *
                    // *           Compute current block column of inv(A).
                    // *
                    if (J + JB <= N)
                    {
                        this._dgemm.Run("No transpose", "No transpose", N, JB, N - J - JB + 1,  - ONE
                                        , A, 1+(J + JB) * LDA + o_a, LDA, WORK, J + JB + o_work, LDWORK, ONE, ref A, 1+J * LDA + o_a
                                        , LDA);
                    }
                    this._dtrsm.Run("Right", "Lower", "No transpose", "Unit", N, JB
                                    , ONE, WORK, J + o_work, LDWORK, ref A, 1+J * LDA + o_a, LDA);
                }
            }
            // *
            // *     Apply column interchanges.
            // *
            for (J = N - 1; J >= 1; J +=  - 1)
            {
                JP = IPIV[J + o_ipiv];
                if (JP != J) this._dswap.Run(N, ref A, 1+J * LDA + o_a, 1, ref A, 1+JP * LDA + o_a, 1);
            }
            // *
            WORK[1 + o_work] = IWS;
            return;
            // *
            // *     End of DGETRI
            // *

            #endregion

        }
    }
}
