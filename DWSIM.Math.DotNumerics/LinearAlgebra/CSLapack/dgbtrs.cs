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
    /// DGBTRS solves a system of linear equations
    /// A * X = B  or  A' * X = B
    /// with a general band matrix A using the LU factorization computed
    /// by DGBTRF.
    /// 
    ///</summary>
    public class DGBTRS
    {
    

        #region Dependencies
        
        LSAME _lsame; DGEMV _dgemv; DGER _dger; DSWAP _dswap; DTBSV _dtbsv; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGBTRS(LSAME lsame, DGEMV dgemv, DGER dger, DSWAP dswap, DTBSV dtbsv, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dgemv = dgemv; this._dger = dger; this._dswap = dswap; this._dtbsv = dtbsv; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DGBTRS()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DSWAP dswap = new DSWAP();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DTBSV dtbsv = new DTBSV(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dgemv = dgemv; this._dger = dger; this._dswap = dswap; this._dtbsv = dtbsv; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGBTRS solves a system of linear equations
        /// A * X = B  or  A' * X = B
        /// with a general band matrix A using the LU factorization computed
        /// by DGBTRF.
        /// 
        ///</summary>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// Specifies the form of the system of equations.
        /// = 'N':  A * X = B  (No transpose)
        /// = 'T':  A'* X = B  (Transpose)
        /// = 'C':  A'* X = B  (Conjugate transpose = Transpose)
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
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
        /// (input) DOUBLE PRECISION array, dimension (LDAB,N)
        /// Details of the LU factorization of the band matrix A, as
        /// computed by DGBTRF.  U is stored as an upper triangular band
        /// matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
        /// the multipliers used during the factorization are stored in
        /// rows KL+KU+2 to 2*KL+KU+1.
        ///</param>
        /// <param name="LDAB">
        /// (input) INTEGER
        /// The leading dimension of the array AB.  LDAB .GE. 2*KL+KU+1.
        ///</param>
        /// <param name="IPIV">
        /// (input) INTEGER array, dimension (N)
        /// The pivot indices; for 1 .LE. i .LE. N, row i of the matrix was
        /// interchanged with row IPIV(i).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the right hand side matrix B.
        /// On exit, the solution matrix X.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B.  LDB .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string TRANS, int N, int KL, int KU, int NRHS, double[] AB, int offset_ab
                         , int LDAB, int[] IPIV, int offset_ipiv, ref double[] B, int offset_b, int LDB, ref int INFO)
        {

            #region Variables
            
            bool LNOTI = false; bool NOTRAN = false; int I = 0; int J = 0; int KD = 0; int L = 0; int LM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ab = -1 - LDAB + offset_ab;  int o_ipiv = -1 + offset_ipiv;  int o_b = -1 - LDB + offset_b; 

            #endregion


            #region Strings
            
            TRANS = TRANS.Substring(0, 1);  

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
            // *  DGBTRS solves a system of linear equations
            // *     A * X = B  or  A' * X = B
            // *  with a general band matrix A using the LU factorization computed
            // *  by DGBTRF.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          Specifies the form of the system of equations.
            // *          = 'N':  A * X = B  (No transpose)
            // *          = 'T':  A'* X = B  (Transpose)
            // *          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
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
            // *  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
            // *          Details of the LU factorization of the band matrix A, as
            // *          computed by DGBTRF.  U is stored as an upper triangular band
            // *          matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
            // *          the multipliers used during the factorization are stored in
            // *          rows KL+KU+2 to 2*KL+KU+1.
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
            // *
            // *  IPIV    (input) INTEGER array, dimension (N)
            // *          The pivot indices; for 1 <= i <= N, row i of the matrix was
            // *          interchanged with row IPIV(i).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the right hand side matrix B.
            // *          On exit, the solution matrix X.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B.  LDB >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
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
            NOTRAN = this._lsame.Run(TRANS, "N");
            if (!NOTRAN && !this._lsame.Run(TRANS, "T") && !this._lsame.Run(TRANS, "C"))
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
                    if (KL < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (KU < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (NRHS < 0)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (LDAB < (2 * KL + KU + 1))
                                {
                                    INFO =  - 7;
                                }
                                else
                                {
                                    if (LDB < Math.Max(1, N))
                                    {
                                        INFO =  - 10;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGBTRS",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0 || NRHS == 0) return;
            // *
            KD = KU + KL + 1;
            LNOTI = KL > 0;
            // *
            if (NOTRAN)
            {
                // *
                // *        Solve  A*X = B.
                // *
                // *        Solve L*X = B, overwriting B with X.
                // *
                // *        L is represented as a product of permutations and unit lower
                // *        triangular matrices L = P(1) * L(1) * ... * P(n-1) * L(n-1),
                // *        where each transformation L(i) is a rank-one modification of
                // *        the identity matrix.
                // *
                if (LNOTI)
                {
                    for (J = 1; J <= N - 1; J++)
                    {
                        LM = Math.Min(KL, N - J);
                        L = IPIV[J + o_ipiv];
                        if (L != J) this._dswap.Run(NRHS, ref B, L+1 * LDB + o_b, LDB, ref B, J+1 * LDB + o_b, LDB);
                        this._dger.Run(LM, NRHS,  - ONE, AB, KD + 1+J * LDAB + o_ab, 1, B, J+1 * LDB + o_b
                                       , LDB, ref B, J + 1+1 * LDB + o_b, LDB);
                    }
                }
                // *
                for (I = 1; I <= NRHS; I++)
                {
                    // *
                    // *           Solve U*X = B, overwriting B with X.
                    // *
                    this._dtbsv.Run("Upper", "No transpose", "Non-unit", N, KL + KU, AB, offset_ab
                                    , LDAB, ref B, 1+I * LDB + o_b, 1);
                }
                // *
            }
            else
            {
                // *
                // *        Solve A'*X = B.
                // *
                for (I = 1; I <= NRHS; I++)
                {
                    // *
                    // *           Solve U'*X = B, overwriting B with X.
                    // *
                    this._dtbsv.Run("Upper", "Transpose", "Non-unit", N, KL + KU, AB, offset_ab
                                    , LDAB, ref B, 1+I * LDB + o_b, 1);
                }
                // *
                // *        Solve L'*X = B, overwriting B with X.
                // *
                if (LNOTI)
                {
                    for (J = N - 1; J >= 1; J +=  - 1)
                    {
                        LM = Math.Min(KL, N - J);
                        this._dgemv.Run("Transpose", LM, NRHS,  - ONE, B, J + 1+1 * LDB + o_b, LDB
                                        , AB, KD + 1+J * LDAB + o_ab, 1, ONE, ref B, J+1 * LDB + o_b, LDB);
                        L = IPIV[J + o_ipiv];
                        if (L != J) this._dswap.Run(NRHS, ref B, L+1 * LDB + o_b, LDB, ref B, J+1 * LDB + o_b, LDB);
                    }
                }
            }
            return;
            // *
            // *     End of DGBTRS
            // *

            #endregion

        }
    }
}
