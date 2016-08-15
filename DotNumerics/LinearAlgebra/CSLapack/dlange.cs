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
    /// DLANGE  returns the value of the one norm,  or the Frobenius norm, or
    /// the  infinity norm,  or the  element of  largest absolute value  of a
    /// real matrix A.
    /// 
    /// Description
    /// ===========
    /// 
    /// DLANGE returns the value
    /// 
    /// DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
    public class DLANGE
    {
    

        #region Dependencies
        
        DLASSQ _dlassq; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLANGE(DLASSQ dlassq, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dlassq = dlassq; this._lsame = lsame; 

            #endregion

        }
    
        public DLANGE()
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
        /// DLANGE  returns the value of the one norm,  or the Frobenius norm, or
        /// the  infinity norm,  or the  element of  largest absolute value  of a
        /// real matrix A.
        /// 
        /// Description
        /// ===========
        /// 
        /// DLANGE returns the value
        /// 
        /// DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
        /// Specifies the value to be returned in DLANGE as described
        /// above.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.  When M = 0,
        /// DLANGE is set to zero.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.  When N = 0,
        /// DLANGE is set to zero.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension (LDA,N)
        /// The m by n matrix A.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(M,1).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
        /// where LWORK .GE. M when NORM = 'I'; otherwise, WORK is not
        /// referenced.
        ///</param>
        public double Run(string NORM, int M, int N, double[] A, int offset_a, int LDA, ref double[] WORK, int offset_work)
        {
        double dlange = 0;

            #region Variables
            
            int I = 0; int J = 0; double SCALE = 0; double SUM = 0; double VALUE = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            NORM = NORM.Substring(0, 1);  

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
            // *  DLANGE  returns the value of the one norm,  or the Frobenius norm, or
            // *  the  infinity norm,  or the  element of  largest absolute value  of a
            // *  real matrix A.
            // *
            // *  Description
            // *  ===========
            // *
            // *  DLANGE returns the value
            // *
            // *     DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
            // *          Specifies the value to be returned in DLANGE as described
            // *          above.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.  When M = 0,
            // *          DLANGE is set to zero.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.  When N = 0,
            // *          DLANGE is set to zero.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The m by n matrix A.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(M,1).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
                    VALUE = ZERO;
                    for (J = 1; J <= N; J++)
                    {
                        for (I = 1; I <= M; I++)
                        {
                            VALUE = Math.Max(VALUE, Math.Abs(A[I+J * LDA + o_a]));
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
                        for (J = 1; J <= N; J++)
                        {
                            SUM = ZERO;
                            for (I = 1; I <= M; I++)
                            {
                                SUM += Math.Abs(A[I+J * LDA + o_a]);
                            }
                            VALUE = Math.Max(VALUE, SUM);
                        }
                    }
                    else
                    {
                        if (this._lsame.Run(NORM, "I"))
                        {
                            // *
                            // *        Find normI(A).
                            // *
                            for (I = 1; I <= M; I++)
                            {
                                WORK[I + o_work] = ZERO;
                            }
                            for (J = 1; J <= N; J++)
                            {
                                for (I = 1; I <= M; I++)
                                {
                                    WORK[I + o_work] += Math.Abs(A[I+J * LDA + o_a]);
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
                                SCALE = ZERO;
                                SUM = ONE;
                                for (J = 1; J <= N; J++)
                                {
                                    this._dlassq.Run(M, A, 1+J * LDA + o_a, 1, ref SCALE, ref SUM);
                                }
                                VALUE = SCALE * Math.Sqrt(SUM);
                            }
                        }
                    }
                }
            }
            // *
            dlange = VALUE;
            return dlange;
            // *
            // *     End of DLANGE
            // *

            #endregion

        }
    }
}
