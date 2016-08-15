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
    /// DLASET initializes an m-by-n matrix A to BETA on the diagonal and
    /// ALPHA on the offdiagonals.
    /// 
    ///</summary>
    public class DLASET
    {
    

        #region Dependencies
        
        LSAME _lsame; 

        #endregion

        public DLASET(LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; 

            #endregion

        }
    
        public DLASET()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASET initializes an m-by-n matrix A to BETA on the diagonal and
        /// ALPHA on the offdiagonals.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies the part of the matrix A to be set.
        /// = 'U':      Upper triangular part is set; the strictly lower
        /// triangular part of A is not changed.
        /// = 'L':      Lower triangular part is set; the strictly upper
        /// triangular part of A is not changed.
        /// Otherwise:  All of the matrix A is set.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="ALPHA">
        /// (input) DOUBLE PRECISION
        /// The constant to which the offdiagonal elements are to be set.
        ///</param>
        /// <param name="BETA">
        /// (input) DOUBLE PRECISION
        /// The constant to which the diagonal elements are to be set.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On exit, the leading m-by-n submatrix of A is set as follows:
        /// 
        /// if UPLO = 'U', A(i,j) = ALPHA, 1.LE.i.LE.j-1, 1.LE.j.LE.n,
        /// if UPLO = 'L', A(i,j) = ALPHA, j+1.LE.i.LE.m, 1.LE.j.LE.n,
        /// otherwise,     A(i,j) = ALPHA, 1.LE.i.LE.m, 1.LE.j.LE.n, i.ne.j,
        /// 
        /// and, for all UPLO, A(i,i) = BETA, 1.LE.i.LE.min(m,n).
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        public void Run(string UPLO, int M, int N, double ALPHA, double BETA, ref double[] A, int offset_a
                         , int LDA)
        {

            #region Variables
            
            int I = 0; int J = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

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
            // *  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
            // *  ALPHA on the offdiagonals.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies the part of the matrix A to be set.
            // *          = 'U':      Upper triangular part is set; the strictly lower
            // *                      triangular part of A is not changed.
            // *          = 'L':      Lower triangular part is set; the strictly upper
            // *                      triangular part of A is not changed.
            // *          Otherwise:  All of the matrix A is set.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  ALPHA   (input) DOUBLE PRECISION
            // *          The constant to which the offdiagonal elements are to be set.
            // *
            // *  BETA    (input) DOUBLE PRECISION
            // *          The constant to which the diagonal elements are to be set.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On exit, the leading m-by-n submatrix of A is set as follows:
            // *
            // *          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
            // *          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
            // *          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
            // *
            // *          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // * =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (this._lsame.Run(UPLO, "U"))
            {
                // *
                // *        Set the strictly upper triangular or trapezoidal part of the
                // *        array to ALPHA.
                // *
                for (J = 2; J <= N; J++)
                {
                    A_J = J * LDA + o_a;
                    for (I = 1; I <= Math.Min(J - 1, M); I++)
                    {
                        A[I + A_J] = ALPHA;
                    }
                }
                // *
            }
            else
            {
                if (this._lsame.Run(UPLO, "L"))
                {
                    // *
                    // *        Set the strictly lower triangular or trapezoidal part of the
                    // *        array to ALPHA.
                    // *
                    for (J = 1; J <= Math.Min(M, N); J++)
                    {
                        A_J = J * LDA + o_a;
                        for (I = J + 1; I <= M; I++)
                        {
                            A[I + A_J] = ALPHA;
                        }
                    }
                    // *
                }
                else
                {
                    // *
                    // *        Set the leading m-by-n submatrix to ALPHA.
                    // *
                    for (J = 1; J <= N; J++)
                    {
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= M; I++)
                        {
                            A[I + A_J] = ALPHA;
                        }
                    }
                }
            }
            // *
            // *     Set the first min(M,N) diagonal elements to BETA.
            // *
            for (I = 1; I <= Math.Min(M, N); I++)
            {
                A[I+I * LDA + o_a] = BETA;
            }
            // *
            return;
            // *
            // *     End of DLASET
            // *

            #endregion

        }
    }
}
