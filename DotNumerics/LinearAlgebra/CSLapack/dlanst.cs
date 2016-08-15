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
    /// DLANST  returns the value of the one norm,  or the Frobenius norm, or
    /// the  infinity norm,  or the  element of  largest absolute value  of a
    /// real symmetric tridiagonal matrix A.
    /// 
    /// Description
    /// ===========
    /// 
    /// DLANST returns the value
    /// 
    /// DLANST = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
    public class DLANST
    {
    

        #region Dependencies
        
        LSAME _lsame; DLASSQ _dlassq; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLANST(LSAME lsame, DLASSQ dlassq)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlassq = dlassq; 

            #endregion

        }
    
        public DLANST()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLASSQ dlassq = new DLASSQ();

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlassq = dlassq; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLANST  returns the value of the one norm,  or the Frobenius norm, or
        /// the  infinity norm,  or the  element of  largest absolute value  of a
        /// real symmetric tridiagonal matrix A.
        /// 
        /// Description
        /// ===========
        /// 
        /// DLANST returns the value
        /// 
        /// DLANST = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
        /// Specifies the value to be returned in DLANST as described
        /// above.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.  When N = 0, DLANST is
        /// set to zero.
        ///</param>
        /// <param name="D">
        /// (input) DOUBLE PRECISION array, dimension (N)
        /// The diagonal elements of A.
        ///</param>
        /// <param name="E">
        /// (input) DOUBLE PRECISION array, dimension (N-1)
        /// The (n-1) sub-diagonal or super-diagonal elements of A.
        ///</param>
        public double Run(string NORM, int N, double[] D, int offset_d, double[] E, int offset_e)
        {
        double dlanst = 0;

            #region Variables
            
            int I = 0; double ANORM = 0; double SCALE = 0; double SUM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e; 

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
            // *  DLANST  returns the value of the one norm,  or the Frobenius norm, or
            // *  the  infinity norm,  or the  element of  largest absolute value  of a
            // *  real symmetric tridiagonal matrix A.
            // *
            // *  Description
            // *  ===========
            // *
            // *  DLANST returns the value
            // *
            // *     DLANST = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
            // *          Specifies the value to be returned in DLANST as described
            // *          above.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.  When N = 0, DLANST is
            // *          set to zero.
            // *
            // *  D       (input) DOUBLE PRECISION array, dimension (N)
            // *          The diagonal elements of A.
            // *
            // *  E       (input) DOUBLE PRECISION array, dimension (N-1)
            // *          The (n-1) sub-diagonal or super-diagonal elements of A.
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
            //      INTRINSIC          ABS, MAX, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (N <= 0)
            {
                ANORM = ZERO;
            }
            else
            {
                if (this._lsame.Run(NORM, "M"))
                {
                    // *
                    // *        Find max(abs(A(i,j))).
                    // *
                    ANORM = Math.Abs(D[N + o_d]);
                    for (I = 1; I <= N - 1; I++)
                    {
                        ANORM = Math.Max(ANORM, Math.Abs(D[I + o_d]));
                        ANORM = Math.Max(ANORM, Math.Abs(E[I + o_e]));
                    }
                }
                else
                {
                    if (this._lsame.Run(NORM, "O") || NORM == "1" || this._lsame.Run(NORM, "I"))
                    {
                        // *
                        // *        Find norm1(A).
                        // *
                        if (N == 1)
                        {
                            ANORM = Math.Abs(D[1 + o_d]);
                        }
                        else
                        {
                            ANORM = Math.Max(Math.Abs(D[1 + o_d]) + Math.Abs(E[1 + o_e]), Math.Abs(E[N - 1 + o_e]) + Math.Abs(D[N + o_d]));
                            for (I = 2; I <= N - 1; I++)
                            {
                                ANORM = Math.Max(ANORM, Math.Abs(D[I + o_d]) + Math.Abs(E[I + o_e]) + Math.Abs(E[I - 1 + o_e]));
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
                            if (N > 1)
                            {
                                this._dlassq.Run(N - 1, E, offset_e, 1, ref SCALE, ref SUM);
                                SUM *= 2;
                            }
                            this._dlassq.Run(N, D, offset_d, 1, ref SCALE, ref SUM);
                            ANORM = SCALE * Math.Sqrt(SUM);
                        }
                    }
                }
            }
            // *
            dlanst = ANORM;
            return dlanst;
            // *
            // *     End of DLANST
            // *

            #endregion

        }
    }
}
