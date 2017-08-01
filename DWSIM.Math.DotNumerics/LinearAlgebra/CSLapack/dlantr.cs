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
    /// -- LAPACK auxiliary routine (version 3.0) --
    /// Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
    /// Courant Institute, Argonne National Lab, and Rice University
    /// October 31, 1992
    /// Purpose
    /// =======
    /// 
    /// DLANTR  returns the value of the one norm,  or the Frobenius norm, or
    /// the  infinity norm,  or the  element of  largest absolute value  of a
    /// trapezoidal or triangular matrix A.
    /// 
    /// Description
    /// ===========
    /// 
    /// DLANTR returns the value
    /// 
    /// DLANTR = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
    /// squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
    /// 
    ///</summary>
    public class DLANTR
    {
    

        #region Dependencies
        
        DLASSQ _dlassq; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLANTR(DLASSQ dlassq, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dlassq = dlassq; this._lsame = lsame; 

            #endregion

        }
    
        public DLANTR()
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
        /// DLANTR  returns the value of the one norm,  or the Frobenius norm, or
        /// the  infinity norm,  or the  element of  largest absolute value  of a
        /// trapezoidal or triangular matrix A.
        /// 
        /// Description
        /// ===========
        /// 
        /// DLANTR returns the value
        /// 
        /// DLANTR = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
        /// squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
        /// 
        ///</summary>
        /// <param name="NORM">
        /// (input) CHARACTER*1
        /// Specifies the value to be returned in DLANTR as described
        /// above.
        ///</param>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies whether the matrix A is upper or lower trapezoidal.
        /// = 'U':  Upper trapezoidal
        /// = 'L':  Lower trapezoidal
        /// Note that A is triangular instead of trapezoidal if M = N.
        ///</param>
        /// <param name="DIAG">
        /// (input) CHARACTER*1
        /// Specifies whether or not the matrix A has unit diagonal.
        /// = 'N':  Non-unit diagonal
        /// = 'U':  Unit diagonal
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0, and if
        /// UPLO = 'U', M .LE. N.  When M = 0, DLANTR is set to zero.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0, and if
        /// UPLO = 'L', N .LE. M.  When N = 0, DLANTR is set to zero.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension (LDA,N)
        /// The trapezoidal matrix A (A is triangular if M = N).
        /// If UPLO = 'U', the leading m by n upper trapezoidal part of
        /// the array A contains the upper trapezoidal matrix, and the
        /// strictly lower triangular part of A is not referenced.
        /// If UPLO = 'L', the leading m by n lower trapezoidal part of
        /// the array A contains the lower trapezoidal matrix, and the
        /// strictly upper triangular part of A is not referenced.  Note
        /// that when DIAG = 'U', the diagonal elements of A are not
        /// referenced and are assumed to be one.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(M,1).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (LWORK),
        /// where LWORK .GE. M when NORM = 'I'; otherwise, WORK is not
        /// referenced.
        ///</param>
        public double Run(string NORM, string UPLO, string DIAG, int M, int N, double[] A, int offset_a
                           , int LDA, ref double[] WORK, int offset_work)
        {
        double dlantr = 0;

            #region Variables
            
            bool UDIAG = false; int I = 0; int J = 0; double SCALE = 0; double SUM = 0; double VALUE = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            NORM = NORM.Substring(0, 1);  UPLO = UPLO.Substring(0, 1);  DIAG = DIAG.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.0) --
            // *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
            // *     Courant Institute, Argonne National Lab, and Rice University
            // *     October 31, 1992
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLANTR  returns the value of the one norm,  or the Frobenius norm, or
            // *  the  infinity norm,  or the  element of  largest absolute value  of a
            // *  trapezoidal or triangular matrix A.
            // *
            // *  Description
            // *  ===========
            // *
            // *  DLANTR returns the value
            // *
            // *     DLANTR = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
            // *  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  NORM    (input) CHARACTER*1
            // *          Specifies the value to be returned in DLANTR as described
            // *          above.
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies whether the matrix A is upper or lower trapezoidal.
            // *          = 'U':  Upper trapezoidal
            // *          = 'L':  Lower trapezoidal
            // *          Note that A is triangular instead of trapezoidal if M = N.
            // *
            // *  DIAG    (input) CHARACTER*1
            // *          Specifies whether or not the matrix A has unit diagonal.
            // *          = 'N':  Non-unit diagonal
            // *          = 'U':  Unit diagonal
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0, and if
            // *          UPLO = 'U', M <= N.  When M = 0, DLANTR is set to zero.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0, and if
            // *          UPLO = 'L', N <= M.  When N = 0, DLANTR is set to zero.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The trapezoidal matrix A (A is triangular if M = N).
            // *          If UPLO = 'U', the leading m by n upper trapezoidal part of
            // *          the array A contains the upper trapezoidal matrix, and the
            // *          strictly lower triangular part of A is not referenced.
            // *          If UPLO = 'L', the leading m by n lower trapezoidal part of
            // *          the array A contains the lower trapezoidal matrix, and the
            // *          strictly upper triangular part of A is not referenced.  Note
            // *          that when DIAG = 'U', the diagonal elements of A are not
            // *          referenced and are assumed to be one.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(M,1).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
            // *          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
            // *          referenced.
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
            
            if (Math.Min(M, N) == 0)
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
                    if (this._lsame.Run(DIAG, "U"))
                    {
                        VALUE = ONE;
                        if (this._lsame.Run(UPLO, "U"))
                        {
                            for (J = 1; J <= N; J++)
                            {
                                for (I = 1; I <= Math.Min(M, J - 1); I++)
                                {
                                    VALUE = Math.Max(VALUE, Math.Abs(A[I+J * LDA + o_a]));
                                }
                            }
                        }
                        else
                        {
                            for (J = 1; J <= N; J++)
                            {
                                for (I = J + 1; I <= M; I++)
                                {
                                    VALUE = Math.Max(VALUE, Math.Abs(A[I+J * LDA + o_a]));
                                }
                            }
                        }
                    }
                    else
                    {
                        VALUE = ZERO;
                        if (this._lsame.Run(UPLO, "U"))
                        {
                            for (J = 1; J <= N; J++)
                            {
                                for (I = 1; I <= Math.Min(M, J); I++)
                                {
                                    VALUE = Math.Max(VALUE, Math.Abs(A[I+J * LDA + o_a]));
                                }
                            }
                        }
                        else
                        {
                            for (J = 1; J <= N; J++)
                            {
                                for (I = J; I <= M; I++)
                                {
                                    VALUE = Math.Max(VALUE, Math.Abs(A[I+J * LDA + o_a]));
                                }
                            }
                        }
                    }
                }
                else
                {
                    if ((this._lsame.Run(NORM, "O")) || (NORM == "1"))
                    {
                        // *
                        // *        Find norm1(A).
                        // *
                        VALUE = ZERO;
                        UDIAG = this._lsame.Run(DIAG, "U");
                        if (this._lsame.Run(UPLO, "U"))
                        {
                            for (J = 1; J <= N; J++)
                            {
                                if ((UDIAG) && (J <= M))
                                {
                                    SUM = ONE;
                                    for (I = 1; I <= J - 1; I++)
                                    {
                                        SUM += Math.Abs(A[I+J * LDA + o_a]);
                                    }
                                }
                                else
                                {
                                    SUM = ZERO;
                                    for (I = 1; I <= Math.Min(M, J); I++)
                                    {
                                        SUM += Math.Abs(A[I+J * LDA + o_a]);
                                    }
                                }
                                VALUE = Math.Max(VALUE, SUM);
                            }
                        }
                        else
                        {
                            for (J = 1; J <= N; J++)
                            {
                                if (UDIAG)
                                {
                                    SUM = ONE;
                                    for (I = J + 1; I <= M; I++)
                                    {
                                        SUM += Math.Abs(A[I+J * LDA + o_a]);
                                    }
                                }
                                else
                                {
                                    SUM = ZERO;
                                    for (I = J; I <= M; I++)
                                    {
                                        SUM += Math.Abs(A[I+J * LDA + o_a]);
                                    }
                                }
                                VALUE = Math.Max(VALUE, SUM);
                            }
                        }
                    }
                    else
                    {
                        if (this._lsame.Run(NORM, "I"))
                        {
                            // *
                            // *        Find normI(A).
                            // *
                            if (this._lsame.Run(UPLO, "U"))
                            {
                                if (this._lsame.Run(DIAG, "U"))
                                {
                                    for (I = 1; I <= M; I++)
                                    {
                                        WORK[I + o_work] = ONE;
                                    }
                                    for (J = 1; J <= N; J++)
                                    {
                                        for (I = 1; I <= Math.Min(M, J - 1); I++)
                                        {
                                            WORK[I + o_work] += Math.Abs(A[I+J * LDA + o_a]);
                                        }
                                    }
                                }
                                else
                                {
                                    for (I = 1; I <= M; I++)
                                    {
                                        WORK[I + o_work] = ZERO;
                                    }
                                    for (J = 1; J <= N; J++)
                                    {
                                        for (I = 1; I <= Math.Min(M, J); I++)
                                        {
                                            WORK[I + o_work] += Math.Abs(A[I+J * LDA + o_a]);
                                        }
                                    }
                                }
                            }
                            else
                            {
                                if (this._lsame.Run(DIAG, "U"))
                                {
                                    for (I = 1; I <= N; I++)
                                    {
                                        WORK[I + o_work] = ONE;
                                    }
                                    for (I = N + 1; I <= M; I++)
                                    {
                                        WORK[I + o_work] = ZERO;
                                    }
                                    for (J = 1; J <= N; J++)
                                    {
                                        for (I = J + 1; I <= M; I++)
                                        {
                                            WORK[I + o_work] += Math.Abs(A[I+J * LDA + o_a]);
                                        }
                                    }
                                }
                                else
                                {
                                    for (I = 1; I <= M; I++)
                                    {
                                        WORK[I + o_work] = ZERO;
                                    }
                                    for (J = 1; J <= N; J++)
                                    {
                                        for (I = J; I <= M; I++)
                                        {
                                            WORK[I + o_work] += Math.Abs(A[I+J * LDA + o_a]);
                                        }
                                    }
                                }
                            }
                            VALUE = ZERO;
                            for (I = 1; I <= M; I++)
                            {
                                VALUE = Math.Max(VALUE, WORK[I + o_work]);
                            }
                        }
                        else
                        {
                            if ((this._lsame.Run(NORM, "F")) || (this._lsame.Run(NORM, "E")))
                            {
                                // *
                                // *        Find normF(A).
                                // *
                                if (this._lsame.Run(UPLO, "U"))
                                {
                                    if (this._lsame.Run(DIAG, "U"))
                                    {
                                        SCALE = ONE;
                                        SUM = Math.Min(M, N);
                                        for (J = 2; J <= N; J++)
                                        {
                                            this._dlassq.Run(Math.Min(M, J - 1), A, 1+J * LDA + o_a, 1, ref SCALE, ref SUM);
                                        }
                                    }
                                    else
                                    {
                                        SCALE = ZERO;
                                        SUM = ONE;
                                        for (J = 1; J <= N; J++)
                                        {
                                            this._dlassq.Run(Math.Min(M, J), A, 1+J * LDA + o_a, 1, ref SCALE, ref SUM);
                                        }
                                    }
                                }
                                else
                                {
                                    if (this._lsame.Run(DIAG, "U"))
                                    {
                                        SCALE = ONE;
                                        SUM = Math.Min(M, N);
                                        for (J = 1; J <= N; J++)
                                        {
                                            this._dlassq.Run(M - J, A, Math.Min(M, J + 1)+J * LDA + o_a, 1, ref SCALE, ref SUM);
                                        }
                                    }
                                    else
                                    {
                                        SCALE = ZERO;
                                        SUM = ONE;
                                        for (J = 1; J <= N; J++)
                                        {
                                            this._dlassq.Run(M - J + 1, A, J+J * LDA + o_a, 1, ref SCALE, ref SUM);
                                        }
                                    }
                                }
                                VALUE = SCALE * Math.Sqrt(SUM);
                            }
                        }
                    }
                }
            }
            // *
            dlantr = VALUE;
            return dlantr;
            // *
            // *     End of DLANTR
            // *

            #endregion

        }
    }
}
