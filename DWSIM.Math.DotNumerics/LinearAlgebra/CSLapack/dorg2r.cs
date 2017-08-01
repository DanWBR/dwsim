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
    /// DORG2R generates an m by n real matrix Q with orthonormal columns,
    /// which is defined as the first n columns of a product of k elementary
    /// reflectors of order m
    /// 
    /// Q  =  H(1) H(2) . . . H(k)
    /// 
    /// as returned by DGEQRF.
    /// 
    ///</summary>
    public class DORG2R
    {
    

        #region Dependencies
        
        DLARF _dlarf; DSCAL _dscal; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DORG2R(DLARF dlarf, DSCAL dscal, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlarf = dlarf; this._dscal = dscal; this._xerbla = xerbla; 

            #endregion

        }
    
        public DORG2R()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);

            #endregion


            #region Set Dependencies
            
            this._dlarf = dlarf; this._dscal = dscal; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DORG2R generates an m by n real matrix Q with orthonormal columns,
        /// which is defined as the first n columns of a product of k elementary
        /// reflectors of order m
        /// 
        /// Q  =  H(1) H(2) . . . H(k)
        /// 
        /// as returned by DGEQRF.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix Q. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix Q. M .GE. N .GE. 0.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The number of elementary reflectors whose product defines the
        /// matrix Q. N .GE. K .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the i-th column must contain the vector which
        /// defines the elementary reflector H(i), for i = 1,2,...,k, as
        /// returned by DGEQRF in the first k columns of its array
        /// argument A.
        /// On exit, the m-by-n matrix Q.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The first dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension (K)
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DGEQRF.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if INFO = -i, the i-th argument has an illegal value
        ///</param>
        public void Run(int M, int N, int K, ref double[] A, int offset_a, int LDA, double[] TAU, int offset_tau
                         , ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; int J = 0; int L = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; int A_I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_work = -1 + offset_work; 

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
            // *  DORG2R generates an m by n real matrix Q with orthonormal columns,
            // *  which is defined as the first n columns of a product of k elementary
            // *  reflectors of order m
            // *
            // *        Q  =  H(1) H(2) . . . H(k)
            // *
            // *  as returned by DGEQRF.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix Q. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix Q. M >= N >= 0.
            // *
            // *  K       (input) INTEGER
            // *          The number of elementary reflectors whose product defines the
            // *          matrix Q. N >= K >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the i-th column must contain the vector which
            // *          defines the elementary reflector H(i), for i = 1,2,...,k, as
            // *          returned by DGEQRF in the first k columns of its array
            // *          argument A.
            // *          On exit, the m-by-n matrix Q.
            // *
            // *  LDA     (input) INTEGER
            // *          The first dimension of the array A. LDA >= max(1,M).
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension (K)
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i), as returned by DGEQRF.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -i, the i-th argument has an illegal value
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (M < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0 || N > M)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (K < 0 || K > N)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, M))
                        {
                            INFO =  - 5;
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DORG2R",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N <= 0) return;
            // *
            // *     Initialise columns k+1:n to columns of the unit matrix
            // *
            for (J = K + 1; J <= N; J++)
            {
                A_J = J * LDA + o_a;
                for (L = 1; L <= M; L++)
                {
                    A[L + A_J] = ZERO;
                }
                A[J+J * LDA + o_a] = ONE;
            }
            // *
            for (I = K; I >= 1; I +=  - 1)
            {
                // *
                // *        Apply H(i) to A(i:m,i:n) from the left
                // *
                if (I < N)
                {
                    A[I+I * LDA + o_a] = ONE;
                    this._dlarf.Run("Left", M - I + 1, N - I, A, I+I * LDA + o_a, 1, TAU[I + o_tau]
                                    , ref A, I+(I + 1) * LDA + o_a, LDA, ref WORK, offset_work);
                }
                if (I < M) this._dscal.Run(M - I,  - TAU[I + o_tau], ref A, I + 1+I * LDA + o_a, 1);
                A[I+I * LDA + o_a] = ONE - TAU[I + o_tau];
                // *
                // *        Set A(1:i-1,i) to zero
                // *
                A_I = I * LDA + o_a;
                for (L = 1; L <= I - 1; L++)
                {
                    A[L + A_I] = ZERO;
                }
            }
            return;
            // *
            // *     End of DORG2R
            // *

            #endregion

        }
    }
}
