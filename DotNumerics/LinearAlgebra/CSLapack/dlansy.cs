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
    /// DLANSY  returns the value of the one norm,  or the Frobenius norm, or
    /// the  infinity norm,  or the  element of  largest absolute value  of a
    /// real symmetric matrix A.
    /// 
    /// Description
    /// ===========
    /// 
    /// DLANSY returns the value
    /// 
    /// DLANSY = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
    public class DLANSY
    {
    

        #region Dependencies
        
        DLASSQ _dlassq; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLANSY(DLASSQ dlassq, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dlassq = dlassq; this._lsame = lsame; 

            #endregion

        }
    
        public DLANSY()
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
        /// DLANSY  returns the value of the one norm,  or the Frobenius norm, or
        /// the  infinity norm,  or the  element of  largest absolute value  of a
        /// real symmetric matrix A.
        /// 
        /// Description
        /// ===========
        /// 
        /// DLANSY returns the value
        /// 
        /// DLANSY = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
        /// Specifies the value to be returned in DLANSY as described
        /// above.
        ///</param>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies whether the upper or lower triangular part of the
        /// symmetric matrix A is to be referenced.
        /// = 'U':  Upper triangular part of A is referenced
        /// = 'L':  Lower triangular part of A is referenced
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.  When N = 0, DLANSY is
        /// set to zero.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension (LDA,N)
        /// The symmetric matrix A.  If UPLO = 'U', the leading n by n
        /// upper triangular part of A contains the upper triangular part
        /// of the matrix A, and the strictly lower triangular part of A
        /// is not referenced.  If UPLO = 'L', the leading n by n lower
        /// triangular part of A contains the lower triangular part of
        /// the matrix A, and the strictly upper triangular part of A is
        /// not referenced.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(N,1).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
        /// where LWORK .GE. N when NORM = 'I' or '1' or 'O'; otherwise,
        /// WORK is not referenced.
        ///</param>
        public double Run(string NORM, string UPLO, int N, double[] A, int offset_a, int LDA, ref double[] WORK, int offset_work)
        {
        double dlansy = 0;

            #region Variables
            
            int I = 0; int J = 0; double ABSA = 0; double SCALE = 0; double SUM = 0; double VALUE = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_work = -1 + offset_work; 

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
            // *  DLANSY  returns the value of the one norm,  or the Frobenius norm, or
            // *  the  infinity norm,  or the  element of  largest absolute value  of a
            // *  real symmetric matrix A.
            // *
            // *  Description
            // *  ===========
            // *
            // *  DLANSY returns the value
            // *
            // *     DLANSY = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
            // *          Specifies the value to be returned in DLANSY as described
            // *          above.
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies whether the upper or lower triangular part of the
            // *          symmetric matrix A is to be referenced.
            // *          = 'U':  Upper triangular part of A is referenced
            // *          = 'L':  Lower triangular part of A is referenced
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.  When N = 0, DLANSY is
            // *          set to zero.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The symmetric matrix A.  If UPLO = 'U', the leading n by n
            // *          upper triangular part of A contains the upper triangular part
            // *          of the matrix A, and the strictly lower triangular part of A
            // *          is not referenced.  If UPLO = 'L', the leading n by n lower
            // *          triangular part of A contains the lower triangular part of
            // *          the matrix A, and the strictly upper triangular part of A is
            // *          not referenced.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(N,1).
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
            //      INTRINSIC          ABS, MAX, SQRT;
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
                            for (I = 1; I <= J; I++)
                            {
                                VALUE = Math.Max(VALUE, Math.Abs(A[I+J * LDA + o_a]));
                            }
                        }
                    }
                    else
                    {
                        for (J = 1; J <= N; J++)
                        {
                            for (I = J; I <= N; I++)
                            {
                                VALUE = Math.Max(VALUE, Math.Abs(A[I+J * LDA + o_a]));
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
                                for (I = 1; I <= J - 1; I++)
                                {
                                    ABSA = Math.Abs(A[I+J * LDA + o_a]);
                                    SUM += ABSA;
                                    WORK[I + o_work] += ABSA;
                                }
                                WORK[J + o_work] = SUM + Math.Abs(A[J+J * LDA + o_a]);
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
                                SUM = WORK[J + o_work] + Math.Abs(A[J+J * LDA + o_a]);
                                for (I = J + 1; I <= N; I++)
                                {
                                    ABSA = Math.Abs(A[I+J * LDA + o_a]);
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
                            if (this._lsame.Run(UPLO, "U"))
                            {
                                for (J = 2; J <= N; J++)
                                {
                                    this._dlassq.Run(J - 1, A, 1+J * LDA + o_a, 1, ref SCALE, ref SUM);
                                }
                            }
                            else
                            {
                                for (J = 1; J <= N - 1; J++)
                                {
                                    this._dlassq.Run(N - J, A, J + 1+J * LDA + o_a, 1, ref SCALE, ref SUM);
                                }
                            }
                            SUM *= 2;
                            this._dlassq.Run(N, A, offset_a, LDA + 1, ref SCALE, ref SUM);
                            VALUE = SCALE * Math.Sqrt(SUM);
                        }
                    }
                }
            }
            // *
            dlansy = VALUE;
            return dlansy;
            // *
            // *     End of DLANSY
            // *

            #endregion

        }
    }
}
