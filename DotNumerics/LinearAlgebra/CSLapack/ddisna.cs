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
    /// -- LAPACK routine (version 3.0) --
    /// Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
    /// Courant Institute, Argonne National Lab, and Rice University
    /// September 30, 1994
    /// Purpose
    /// =======
    /// 
    /// DDISNA computes the reciprocal condition numbers for the eigenvectors
    /// of a real symmetric or complex Hermitian matrix or for the left or
    /// right singular vectors of a general m-by-n matrix. The reciprocal
    /// condition number is the 'gap' between the corresponding eigenvalue or
    /// singular value and the nearest other one.
    /// 
    /// The bound on the error, measured by angle in radians, in the I-th
    /// computed vector is given by
    /// 
    /// DLAMCH( 'E' ) * ( ANORM / SEP( I ) )
    /// 
    /// where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
    /// to be smaller than DLAMCH( 'E' )*ANORM in order to limit the size of
    /// the error bound.
    /// 
    /// DDISNA may also be used to compute error bounds for eigenvectors of
    /// the generalized symmetric definite eigenproblem.
    /// 
    ///</summary>
    public class DDISNA
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAMCH _dlamch; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DDISNA(LSAME lsame, DLAMCH dlamch, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._xerbla = xerbla; 

            #endregion

        }
    
        public DDISNA()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DDISNA computes the reciprocal condition numbers for the eigenvectors
        /// of a real symmetric or complex Hermitian matrix or for the left or
        /// right singular vectors of a general m-by-n matrix. The reciprocal
        /// condition number is the 'gap' between the corresponding eigenvalue or
        /// singular value and the nearest other one.
        /// 
        /// The bound on the error, measured by angle in radians, in the I-th
        /// computed vector is given by
        /// 
        /// DLAMCH( 'E' ) * ( ANORM / SEP( I ) )
        /// 
        /// where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
        /// to be smaller than DLAMCH( 'E' )*ANORM in order to limit the size of
        /// the error bound.
        /// 
        /// DDISNA may also be used to compute error bounds for eigenvectors of
        /// the generalized symmetric definite eigenproblem.
        /// 
        ///</summary>
        /// <param name="JOB">
        /// (input) CHARACTER*1
        /// Specifies for which problem the reciprocal condition numbers
        /// should be computed:
        /// = 'E':  the eigenvectors of a symmetric/Hermitian matrix;
        /// = 'L':  the left singular vectors of a general matrix;
        /// = 'R':  the right singular vectors of a general matrix.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// If JOB = 'L' or 'R', the number of columns of the matrix,
        /// in which case N .GE. 0. Ignored if JOB = 'E'.
        ///</param>
        /// <param name="D">
        /// (input) DOUBLE PRECISION array, dimension (M) if JOB = 'E'
        /// dimension (min(M,N)) if JOB = 'L' or 'R'
        /// The eigenvalues (if JOB = 'E') or singular values (if JOB =
        /// 'L' or 'R') of the matrix, in either increasing or decreasing
        /// order. If singular values, they must be non-negative.
        ///</param>
        /// <param name="SEP">
        /// (output) DOUBLE PRECISION array, dimension (M) if JOB = 'E'
        /// dimension (min(M,N)) if JOB = 'L' or 'R'
        /// The reciprocal condition numbers of the vectors.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(string JOB, int M, int N, double[] D, int offset_d, ref double[] SEP, int offset_sep, ref int INFO)
        {

            #region Variables
            
            bool DECR = false; bool EIGEN = false; bool INCR = false; bool LEFT = false; bool RIGHT = false; bool SING = false; 
            int I = 0;int K = 0; double ANORM = 0; double EPS = 0; double NEWGAP = 0; double OLDGAP = 0; double SAFMIN = 0; 
            double THRESH = 0;

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_sep = -1 + offset_sep; 

            #endregion


            #region Strings
            
            JOB = JOB.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.0) --
            // *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
            // *     Courant Institute, Argonne National Lab, and Rice University
            // *     September 30, 1994
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DDISNA computes the reciprocal condition numbers for the eigenvectors
            // *  of a real symmetric or complex Hermitian matrix or for the left or
            // *  right singular vectors of a general m-by-n matrix. The reciprocal
            // *  condition number is the 'gap' between the corresponding eigenvalue or
            // *  singular value and the nearest other one.
            // *
            // *  The bound on the error, measured by angle in radians, in the I-th
            // *  computed vector is given by
            // *
            // *         DLAMCH( 'E' ) * ( ANORM / SEP( I ) )
            // *
            // *  where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
            // *  to be smaller than DLAMCH( 'E' )*ANORM in order to limit the size of
            // *  the error bound.
            // *
            // *  DDISNA may also be used to compute error bounds for eigenvectors of
            // *  the generalized symmetric definite eigenproblem.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOB     (input) CHARACTER*1
            // *          Specifies for which problem the reciprocal condition numbers
            // *          should be computed:
            // *          = 'E':  the eigenvectors of a symmetric/Hermitian matrix;
            // *          = 'L':  the left singular vectors of a general matrix;
            // *          = 'R':  the right singular vectors of a general matrix.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          If JOB = 'L' or 'R', the number of columns of the matrix,
            // *          in which case N >= 0. Ignored if JOB = 'E'.
            // *
            // *  D       (input) DOUBLE PRECISION array, dimension (M) if JOB = 'E'
            // *                              dimension (min(M,N)) if JOB = 'L' or 'R'
            // *          The eigenvalues (if JOB = 'E') or singular values (if JOB =
            // *          'L' or 'R') of the matrix, in either increasing or decreasing
            // *          order. If singular values, they must be non-negative.
            // *
            // *  SEP     (output) DOUBLE PRECISION array, dimension (M) if JOB = 'E'
            // *                               dimension (min(M,N)) if JOB = 'L' or 'R'
            // *          The reciprocal condition numbers of the vectors.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN;
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            EIGEN = this._lsame.Run(JOB, "E");
            LEFT = this._lsame.Run(JOB, "L");
            RIGHT = this._lsame.Run(JOB, "R");
            SING = LEFT || RIGHT;
            if (EIGEN)
            {
                K = M;
            }
            else
            {
                if (SING)
                {
                    K = Math.Min(M, N);
                }
            }
            if (!EIGEN && !SING)
            {
                INFO =  - 1;
            }
            else
            {
                if (M < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (K < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        INCR = true;
                        DECR = true;
                        for (I = 1; I <= K - 1; I++)
                        {
                            if (INCR) INCR = INCR && D[I + o_d] <= D[I + 1 + o_d];
                            if (DECR) DECR = DECR && D[I + o_d] >= D[I + 1 + o_d];
                        }
                        if (SING && K > 0)
                        {
                            if (INCR) INCR = INCR && ZERO <= D[1 + o_d];
                            if (DECR) DECR = DECR && D[K + o_d] >= ZERO;
                        }
                        if (!(INCR || DECR)) INFO =  - 4;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DDISNA",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (K == 0) return;
            // *
            // *     Compute reciprocal condition numbers
            // *
            if (K == 1)
            {
                SEP[1 + o_sep] = this._dlamch.Run("O");
            }
            else
            {
                OLDGAP = Math.Abs(D[2 + o_d] - D[1 + o_d]);
                SEP[1 + o_sep] = OLDGAP;
                for (I = 2; I <= K - 1; I++)
                {
                    NEWGAP = Math.Abs(D[I + 1 + o_d] - D[I + o_d]);
                    SEP[I + o_sep] = Math.Min(OLDGAP, NEWGAP);
                    OLDGAP = NEWGAP;
                }
                SEP[K + o_sep] = OLDGAP;
            }
            if (SING)
            {
                if ((LEFT && M > N) || (RIGHT && M < N))
                {
                    if (INCR) SEP[1 + o_sep] = Math.Min(SEP[1 + o_sep], D[1 + o_d]);
                    if (DECR) SEP[K + o_sep] = Math.Min(SEP[K + o_sep], D[K + o_d]);
                }
            }
            // *
            // *     Ensure that reciprocal condition numbers are not less than
            // *     threshold, in order to limit the size of the error bound
            // *
            EPS = this._dlamch.Run("E");
            SAFMIN = this._dlamch.Run("S");
            ANORM = Math.Max(Math.Abs(D[1 + o_d]), Math.Abs(D[K + o_d]));
            if (ANORM == ZERO)
            {
                THRESH = EPS;
            }
            else
            {
                THRESH = Math.Max(EPS * ANORM, SAFMIN);
            }
            for (I = 1; I <= K; I++)
            {
                SEP[I + o_sep] = Math.Max(SEP[I + o_sep], THRESH);
            }
            // *
            return;
            // *
            // *     End of DDISNA
            // *

            #endregion

        }
    }
}
