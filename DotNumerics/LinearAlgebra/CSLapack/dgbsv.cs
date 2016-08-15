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
    /// -- LAPACK driver routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DGBSV computes the solution to a real system of linear equations
    /// A * X = B, where A is a band matrix of order N with KL subdiagonals
    /// and KU superdiagonals, and X and B are N-by-NRHS matrices.
    /// 
    /// The LU decomposition with partial pivoting and row interchanges is
    /// used to factor A as A = L * U, where L is a product of permutation
    /// and unit lower triangular matrices with KL subdiagonals, and U is
    /// upper triangular with KL+KU superdiagonals.  The factored form of A
    /// is then used to solve the system of equations A * X = B.
    /// 
    ///</summary>
    public class DGBSV
    {
    

        #region Dependencies
        
        DGBTRF _dgbtrf; DGBTRS _dgbtrs; XERBLA _xerbla; 

        #endregion

        public DGBSV(DGBTRF dgbtrf, DGBTRS dgbtrs, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dgbtrf = dgbtrf; this._dgbtrs = dgbtrs; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGBSV()
        {
    

            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            LSAME lsame = new LSAME();
            DLASWP dlaswp = new DLASWP();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGER dger = new DGER(xerbla);
            DGBTF2 dgbtf2 = new DGBTF2(idamax, dger, dscal, dswap, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);
            DGBTRF dgbtrf = new DGBTRF(idamax, ilaenv, dcopy, dgbtf2, dgemm, dger, dlaswp, dscal, dswap, dtrsm
                                       , xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTBSV dtbsv = new DTBSV(lsame, xerbla);
            DGBTRS dgbtrs = new DGBTRS(lsame, dgemv, dger, dswap, dtbsv, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgbtrf = dgbtrf; this._dgbtrs = dgbtrs; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGBSV computes the solution to a real system of linear equations
        /// A * X = B, where A is a band matrix of order N with KL subdiagonals
        /// and KU superdiagonals, and X and B are N-by-NRHS matrices.
        /// 
        /// The LU decomposition with partial pivoting and row interchanges is
        /// used to factor A as A = L * U, where L is a product of permutation
        /// and unit lower triangular matrices with KL subdiagonals, and U is
        /// upper triangular with KL+KU superdiagonals.  The factored form of A
        /// is then used to solve the system of equations A * X = B.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of linear equations, i.e., the order of the
        /// matrix A.  N .GE. 0.
        ///</param>
        /// <param name="KL">
        /// (input) INTEGER
        /// The number of subdiagonals within the band of A.  KL .GE. 0.
        ///</param>
        /// <param name="KU">
        /// (input) INTEGER
        /// The number of superdiagonals within the band of A.  KU .GE. 0.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS .GE. 0.
        ///</param>
        /// <param name="AB">
        /// (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
        /// On entry, the matrix A in band storage, in rows KL+1 to
        /// 2*KL+KU+1; rows 1 to KL of the array need not be set.
        /// The j-th column of A is stored in the j-th column of the
        /// array AB as follows:
        /// AB(KL+KU+1+i-j,j) = A(i,j) for max(1,j-KU).LE.i.LE.min(N,j+KL)
        /// On exit, details of the factorization: U is stored as an
        /// upper triangular band matrix with KL+KU superdiagonals in
        /// rows 1 to KL+KU+1, and the multipliers used during the
        /// factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
        /// See below for further details.
        ///</param>
        /// <param name="LDAB">
        /// (input) INTEGER
        /// The leading dimension of the array AB.  LDAB .GE. 2*KL+KU+1.
        ///</param>
        /// <param name="IPIV">
        /// (output) INTEGER array, dimension (N)
        /// The pivot indices that define the permutation matrix P;
        /// row i of the matrix was interchanged with row IPIV(i).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit, if INFO = 0, the N-by-NRHS solution matrix X.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B.  LDB .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and the solution has not been computed.
        ///</param>
        public void Run(int N, int KL, int KU, int NRHS, ref double[] AB, int offset_ab, int LDAB
                         , ref int[] IPIV, int offset_ipiv, ref double[] B, int offset_b, int LDB, ref int INFO)
        {

            #region Array Index Correction
            
             int o_ab = -1 - LDAB + offset_ab;  int o_ipiv = -1 + offset_ipiv;  int o_b = -1 - LDB + offset_b; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK driver routine (version 3.1) --
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
            // *  DGBSV computes the solution to a real system of linear equations
            // *  A * X = B, where A is a band matrix of order N with KL subdiagonals
            // *  and KU superdiagonals, and X and B are N-by-NRHS matrices.
            // *
            // *  The LU decomposition with partial pivoting and row interchanges is
            // *  used to factor A as A = L * U, where L is a product of permutation
            // *  and unit lower triangular matrices with KL subdiagonals, and U is
            // *  upper triangular with KL+KU superdiagonals.  The factored form of A
            // *  is then used to solve the system of equations A * X = B.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of linear equations, i.e., the order of the
            // *          matrix A.  N >= 0.
            // *
            // *  KL      (input) INTEGER
            // *          The number of subdiagonals within the band of A.  KL >= 0.
            // *
            // *  KU      (input) INTEGER
            // *          The number of superdiagonals within the band of A.  KU >= 0.
            // *
            // *  NRHS    (input) INTEGER
            // *          The number of right hand sides, i.e., the number of columns
            // *          of the matrix B.  NRHS >= 0.
            // *
            // *  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
            // *          On entry, the matrix A in band storage, in rows KL+1 to
            // *          2*KL+KU+1; rows 1 to KL of the array need not be set.
            // *          The j-th column of A is stored in the j-th column of the
            // *          array AB as follows:
            // *          AB(KL+KU+1+i-j,j) = A(i,j) for max(1,j-KU)<=i<=min(N,j+KL)
            // *          On exit, details of the factorization: U is stored as an
            // *          upper triangular band matrix with KL+KU superdiagonals in
            // *          rows 1 to KL+KU+1, and the multipliers used during the
            // *          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
            // *          See below for further details.
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
            // *
            // *  IPIV    (output) INTEGER array, dimension (N)
            // *          The pivot indices that define the permutation matrix P;
            // *          row i of the matrix was interchanged with row IPIV(i).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the N-by-NRHS right hand side matrix B.
            // *          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B.  LDB >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
            // *                has been completed, but the factor U is exactly
            // *                singular, and the solution has not been computed.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The band storage scheme is illustrated by the following example, when
            // *  M = N = 6, KL = 2, KU = 1:
            // *
            // *  On entry:                       On exit:
            // *
            // *      *    *    *    +    +    +       *    *    *   u14  u25  u36
            // *      *    *    +    +    +    +       *    *   u13  u24  u35  u46
            // *      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
            // *     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
            // *     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
            // *     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
            // *
            // *  Array elements marked * are not used by the routine; elements marked
            // *  + need not be set on entry, but are required by the routine to store
            // *  elements of U because of fill-in resulting from the row interchanges.
            // *
            // *  =====================================================================
            // *
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (KL < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (KU < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (NRHS < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDAB < 2 * KL + KU + 1)
                            {
                                INFO =  - 6;
                            }
                            else
                            {
                                if (LDB < Math.Max(N, 1))
                                {
                                    INFO =  - 9;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGBSV ",  - INFO);
                return;
            }
            // *
            // *     Compute the LU factorization of the band matrix A.
            // *
            this._dgbtrf.Run(N, N, KL, KU, ref AB, offset_ab, LDAB
                             , ref IPIV, offset_ipiv, ref INFO);
            if (INFO == 0)
            {
                // *
                // *        Solve the system A*X = B, overwriting B with X.
                // *
                this._dgbtrs.Run("No transpose", N, KL, KU, NRHS, AB, offset_ab
                                 , LDAB, IPIV, offset_ipiv, ref B, offset_b, LDB, ref INFO);
            }
            return;
            // *
            // *     End of DGBSV
            // *

            #endregion

        }
    }
}
