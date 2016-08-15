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
    /// DLACPY copies all or part of a two-dimensional matrix A to another
    /// matrix B.
    /// 
    ///</summary>
    public class DLACPY
    {
    

        #region Dependencies
        
        LSAME _lsame; 

        #endregion

        public DLACPY(LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; 

            #endregion

        }
    
        public DLACPY()
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
        /// DLACPY copies all or part of a two-dimensional matrix A to another
        /// matrix B.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies the part of the matrix A to be copied to B.
        /// = 'U':      Upper triangular part
        /// = 'L':      Lower triangular part
        /// Otherwise:  All of the matrix A
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension (LDA,N)
        /// The m by n matrix A.  If UPLO = 'U', only the upper triangle
        /// or trapezoid is accessed; if UPLO = 'L', only the lower
        /// triangle or trapezoid is accessed.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="B">
        /// (output) DOUBLE PRECISION array, dimension (LDB,N)
        /// On exit, B = A in the locations specified by UPLO.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B.  LDB .GE. max(1,M).
        ///</param>
        public void Run(string UPLO, int M, int N, double[] A, int offset_a, int LDA, ref double[] B, int offset_b
                         , int LDB)
        {

            #region Variables
            
            int I = 0; int J = 0; 

            #endregion


            #region Implicit Variables
            
            int B_J = 0; int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b; 

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
            // *  DLACPY copies all or part of a two-dimensional matrix A to another
            // *  matrix B.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies the part of the matrix A to be copied to B.
            // *          = 'U':      Upper triangular part
            // *          = 'L':      Lower triangular part
            // *          Otherwise:  All of the matrix A
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The m by n matrix A.  If UPLO = 'U', only the upper triangle
            // *          or trapezoid is accessed; if UPLO = 'L', only the lower
            // *          triangle or trapezoid is accessed.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  B       (output) DOUBLE PRECISION array, dimension (LDB,N)
            // *          On exit, B = A in the locations specified by UPLO.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B.  LDB >= max(1,M).
            // *
            // *  =====================================================================
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
                for (J = 1; J <= N; J++)
                {
                    B_J = J * LDB + o_b;
                    A_J = J * LDA + o_a;
                    for (I = 1; I <= Math.Min(J, M); I++)
                    {
                        B[I + B_J] = A[I + A_J];
                    }
                }
            }
            else
            {
                if (this._lsame.Run(UPLO, "L"))
                {
                    for (J = 1; J <= N; J++)
                    {
                        B_J = J * LDB + o_b;
                        A_J = J * LDA + o_a;
                        for (I = J; I <= M; I++)
                        {
                            B[I + B_J] = A[I + A_J];
                        }
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        B_J = J * LDB + o_b;
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= M; I++)
                        {
                            B[I + B_J] = A[I + A_J];
                        }
                    }
                }
            }
            return;
            // *
            // *     End of DLACPY
            // *

            #endregion

        }
    }
}
