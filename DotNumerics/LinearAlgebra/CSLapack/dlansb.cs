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
    /// DLANSB  returns the value of the one norm,  or the Frobenius norm, or
    /// the  infinity norm,  or the element of  largest absolute value  of an
    /// n by n symmetric band matrix A,  with k super-diagonals.
    /// 
    /// Description
    /// ===========
    /// 
    /// DLANSB returns the value
    /// 
    /// DLANSB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
    /// (
    /// ( norm1(A),         NORM = '1', 'O' or 'o'
    /// (
    /// ( normI(A),         NORM = 'I' or 'i'
    /// (
    /// ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
    /// 
    /// where  norm1  denotes the  one norm of a matrix (maximum column sum),
    /// normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
    /// normF  denotes the  Frobenius norm of a matrix (square root of sum of
    /// squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
    /// 
    ///</summary>
    public class DLANSB
    {
    

        #region Dependencies
        
        DLASSQ _dlassq; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLANSB(DLASSQ dlassq, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dlassq = dlassq; this._lsame = lsame; 

            #endregion

        }
    
        public DLANSB()
        {
    

            #region Dependencies (Initialization)
            
            DLASSQ dlassq = new DLASSQ();
            LSAME lsame = new LSAME();

            #endregion


            #region Set Dependencies
            
            this._dlassq = dlassq; this._lsame = lsame; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLANSB  returns the value of the one norm,  or the Frobenius norm, or
        /// the  infinity norm,  or the element of  largest absolute value  of an
        /// n by n symmetric band matrix A,  with k super-diagonals.
        /// 
        /// Description
        /// ===========
        /// 
        /// DLANSB returns the value
        /// 
        /// DLANSB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
        /// (
        /// ( norm1(A),         NORM = '1', 'O' or 'o'
        /// (
        /// ( normI(A),         NORM = 'I' or 'i'
        /// (
        /// ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
        /// 
        /// where  norm1  denotes the  one norm of a matrix (maximum column sum),
        /// normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
        /// normF  denotes the  Frobenius norm of a matrix (square root of sum of
        /// squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
        /// 
        ///</summary>
        /// <param name="NORM">
        /// (input) CHARACTER*1
        /// Specifies the value to be returned in DLANSB as described
        /// above.
        ///</param>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies whether the upper or lower triangular part of the
        /// band matrix A is supplied.
        /// = 'U':  Upper triangular part is supplied
        /// = 'L':  Lower triangular part is supplied
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.  When N = 0, DLANSB is
        /// set to zero.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The number of super-diagonals or sub-diagonals of the
        /// band matrix A.  K .GE. 0.
        ///</param>
        /// <param name="AB">
        /// (input) DOUBLE PRECISION array, dimension (LDAB,N)
        /// The upper or lower triangle of the symmetric band matrix A,
        /// stored in the first K+1 rows of AB.  The j-th column of A is
        /// stored in the j-th column of the array AB as follows:
        /// if UPLO = 'U', AB(k+1+i-j,j) = A(i,j) for max(1,j-k).LE.i.LE.j;
        /// if UPLO = 'L', AB(1+i-j,j)   = A(i,j) for j.LE.i.LE.min(n,j+k).
        ///</param>
        /// <param name="LDAB">
        /// (input) INTEGER
        /// The leading dimension of the array AB.  LDAB .GE. K+1.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
        /// where LWORK .GE. N when NORM = 'I' or '1' or 'O'; otherwise,
        /// WORK is not referenced.
        ///</param>
        public double Run(string NORM, string UPLO, int N, int K, double[] AB, int offset_ab, int LDAB
                           , ref double[] WORK, int offset_work)
        {
        double dlansb = 0;

            #region Variables
            
            int I = 0; int J = 0; int L = 0; double ABSA = 0; double SCALE = 0; double SUM = 0; double VALUE = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ab = -1 - LDAB + offset_ab;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            NORM = NORM.Substring(0, 1);  UPLO = UPLO.Substring(0, 1);  

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
            // *  DLANSB  returns the value of the one norm,  or the Frobenius norm, or
            // *  the  infinity norm,  or the element of  largest absolute value  of an
            // *  n by n symmetric band matrix A,  with k super-diagonals.
            // *
            // *  Description
            // *  ===========
            // *
            // *  DLANSB returns the value
            // *
            // *     DLANSB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
            // *              (
            // *              ( norm1(A),         NORM = '1', 'O' or 'o'
            // *              (
            // *              ( normI(A),         NORM = 'I' or 'i'
            // *              (
            // *              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
            // *
            // *  where  norm1  denotes the  one norm of a matrix (maximum column sum),
            // *  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
            // *  normF  denotes the  Frobenius norm of a matrix (square root of sum of
            // *  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  NORM    (input) CHARACTER*1
            // *          Specifies the value to be returned in DLANSB as described
            // *          above.
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies whether the upper or lower triangular part of the
            // *          band matrix A is supplied.
            // *          = 'U':  Upper triangular part is supplied
            // *          = 'L':  Lower triangular part is supplied
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.  When N = 0, DLANSB is
            // *          set to zero.
            // *
            // *  K       (input) INTEGER
            // *          The number of super-diagonals or sub-diagonals of the
            // *          band matrix A.  K >= 0.
            // *
            // *  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
            // *          The upper or lower triangle of the symmetric band matrix A,
            // *          stored in the first K+1 rows of AB.  The j-th column of A is
            // *          stored in the j-th column of the array AB as follows:
            // *          if UPLO = 'U', AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
            // *          if UPLO = 'L', AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= K+1.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
            // *          where LWORK >= N when NORM = 'I' or '1' or 'O'; otherwise,
            // *          WORK is not referenced.
            // *
            // * =====================================================================
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
            //      INTRINSIC          ABS, MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (N == 0)
            {
                VALUE = ZERO;
            }
            else
            {
                if (this._lsame.Run(NORM, "M"))
                {
                    // *
                    // *        Find max(abs(A(i,j))).
                    // *
                    VALUE = ZERO;
                    if (this._lsame.Run(UPLO, "U"))
                    {
                        for (J = 1; J <= N; J++)
                        {
                            for (I = Math.Max(K + 2 - J, 1); I <= K + 1; I++)
                            {
                                VALUE = Math.Max(VALUE, Math.Abs(AB[I+J * LDAB + o_ab]));
                            }
                        }
                    }
                    else
                    {
                        for (J = 1; J <= N; J++)
                        {
                            for (I = 1; I <= Math.Min(N + 1 - J, K + 1); I++)
                            {
                                VALUE = Math.Max(VALUE, Math.Abs(AB[I+J * LDAB + o_ab]));
                            }
                        }
                    }
                }
                else
                {
                    if ((this._lsame.Run(NORM, "I")) || (this._lsame.Run(NORM, "O")) || (NORM == "1"))
                    {
                        // *
                        // *        Find normI(A) ( = norm1(A), since A is symmetric).
                        // *
                        VALUE = ZERO;
                        if (this._lsame.Run(UPLO, "U"))
                        {
                            for (J = 1; J <= N; J++)
                            {
                                SUM = ZERO;
                                L = K + 1 - J;
                                for (I = Math.Max(1, J - K); I <= J - 1; I++)
                                {
                                    ABSA = Math.Abs(AB[L + I+J * LDAB + o_ab]);
                                    SUM += ABSA;
                                    WORK[I + o_work] += ABSA;
                                }
                                WORK[J + o_work] = SUM + Math.Abs(AB[K + 1+J * LDAB + o_ab]);
                            }
                            for (I = 1; I <= N; I++)
                            {
                                VALUE = Math.Max(VALUE, WORK[I + o_work]);
                            }
                        }
                        else
                        {
                            for (I = 1; I <= N; I++)
                            {
                                WORK[I + o_work] = ZERO;
                            }
                            for (J = 1; J <= N; J++)
                            {
                                SUM = WORK[J + o_work] + Math.Abs(AB[1+J * LDAB + o_ab]);
                                L = 1 - J;
                                for (I = J + 1; I <= Math.Min(N, J + K); I++)
                                {
                                    ABSA = Math.Abs(AB[L + I+J * LDAB + o_ab]);
                                    SUM += ABSA;
                                    WORK[I + o_work] += ABSA;
                                }
                                VALUE = Math.Max(VALUE, SUM);
                            }
                        }
                    }
                    else
                    {
                        if ((this._lsame.Run(NORM, "F")) || (this._lsame.Run(NORM, "E")))
                        {
                            // *
                            // *        Find normF(A).
                            // *
                            SCALE = ZERO;
                            SUM = ONE;
                            if (K > 0)
                            {
                                if (this._lsame.Run(UPLO, "U"))
                                {
                                    for (J = 2; J <= N; J++)
                                    {
                                        this._dlassq.Run(Math.Min(J - 1, K), AB, Math.Max(K + 2 - J, 1)+J * LDAB + o_ab, 1, ref SCALE, ref SUM);
                                    }
                                    L = K + 1;
                                }
                                else
                                {
                                    for (J = 1; J <= N - 1; J++)
                                    {
                                        this._dlassq.Run(Math.Min(N - J, K), AB, 2+J * LDAB + o_ab, 1, ref SCALE, ref SUM);
                                    }
                                    L = 1;
                                }
                                SUM *= 2;
                            }
                            else
                            {
                                L = 1;
                            }
                            this._dlassq.Run(N, AB, L+1 * LDAB + o_ab, LDAB, ref SCALE, ref SUM);
                            VALUE = SCALE * Math.Sqrt(SUM);
                        }
                    }
                }
            }
            // *
            dlansb = VALUE;
            return dlansb;
            // *
            // *     End of DLANSB
            // *

            #endregion

        }
    }
}
