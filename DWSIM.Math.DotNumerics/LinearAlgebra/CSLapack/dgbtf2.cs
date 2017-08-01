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
    /// DGBTF2 computes an LU factorization of a real m-by-n band matrix A
    /// using partial pivoting with row interchanges.
    /// 
    /// This is the unblocked version of the algorithm, calling Level 2 BLAS.
    /// 
    ///</summary>
    public class DGBTF2
    {
    

        #region Dependencies
        
        IDAMAX _idamax; DGER _dger; DSCAL _dscal; DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DGBTF2(IDAMAX idamax, DGER dger, DSCAL dscal, DSWAP dswap, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._idamax = idamax; this._dger = dger; this._dscal = dscal; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGBTF2()
        {
    

            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            DGER dger = new DGER(xerbla);

            #endregion


            #region Set Dependencies
            
            this._idamax = idamax; this._dger = dger; this._dscal = dscal; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGBTF2 computes an LU factorization of a real m-by-n band matrix A
        /// using partial pivoting with row interchanges.
        /// 
        /// This is the unblocked version of the algorithm, calling Level 2 BLAS.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="KL">
        /// (input) INTEGER
        /// The number of subdiagonals within the band of A.  KL .GE. 0.
        ///</param>
        /// <param name="KU">
        /// (input) INTEGER
        /// The number of superdiagonals within the band of A.  KU .GE. 0.
        ///</param>
        /// <param name="AB">
        /// (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
        /// On entry, the matrix A in band storage, in rows KL+1 to
        /// 2*KL+KU+1; rows 1 to KL of the array need not be set.
        /// The j-th column of A is stored in the j-th column of the
        /// array AB as follows:
        /// AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku).LE.i.LE.min(m,j+kl)
        /// 
        /// On exit, details of the factorization: U is stored as an
        /// upper triangular band matrix with KL+KU superdiagonals in
        /// rows 1 to KL+KU+1, and the multipliers used during the
        /// factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
        /// See below for further details.
        ///</param>
        /// <param name="LDAB">
        /// (input) INTEGER
        /// The leading dimension of the array AB.  LDAB .GE. 2*KL+KU+1.
        ///</param>
        /// <param name="IPIV">
        /// (output) INTEGER array, dimension (min(M,N))
        /// The pivot indices; for 1 .LE. i .LE. min(M,N), row i of the
        /// matrix was interchanged with row IPIV(i).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0: if INFO = +i, U(i,i) is exactly zero. The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and division by zero will occur if it is used
        /// to solve a system of equations.
        ///</param>
        public void Run(int M, int N, int KL, int KU, ref double[] AB, int offset_ab, int LDAB
                         , ref int[] IPIV, int offset_ipiv, ref int INFO)
        {

            #region Variables
            
            int I = 0; int J = 0; int JP = 0; int JU = 0; int KM = 0; int KV = 0; 

            #endregion


            #region Implicit Variables
            
            int AB_J = 0; int AB_0 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ab = -1 - LDAB + offset_ab;  int o_ipiv = -1 + offset_ipiv; 

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
            // *  DGBTF2 computes an LU factorization of a real m-by-n band matrix A
            // *  using partial pivoting with row interchanges.
            // *
            // *  This is the unblocked version of the algorithm, calling Level 2 BLAS.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  KL      (input) INTEGER
            // *          The number of subdiagonals within the band of A.  KL >= 0.
            // *
            // *  KU      (input) INTEGER
            // *          The number of superdiagonals within the band of A.  KU >= 0.
            // *
            // *  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
            // *          On entry, the matrix A in band storage, in rows KL+1 to
            // *          2*KL+KU+1; rows 1 to KL of the array need not be set.
            // *          The j-th column of A is stored in the j-th column of the
            // *          array AB as follows:
            // *          AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)
            // *
            // *          On exit, details of the factorization: U is stored as an
            // *          upper triangular band matrix with KL+KU superdiagonals in
            // *          rows 1 to KL+KU+1, and the multipliers used during the
            // *          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
            // *          See below for further details.
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
            // *
            // *  IPIV    (output) INTEGER array, dimension (min(M,N))
            // *          The pivot indices; for 1 <= i <= min(M,N), row i of the
            // *          matrix was interchanged with row IPIV(i).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
            // *          > 0: if INFO = +i, U(i,i) is exactly zero. The factorization
            // *               has been completed, but the factor U is exactly
            // *               singular, and division by zero will occur if it is used
            // *               to solve a system of equations.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The band storage scheme is illustrated by the following example, when
            // *  M = N = 6, KL = 2, KU = 1:
            // *
            // *  On entry:                       On exit:
            // *
            // *      *    *    *    +    +    +       *    *    *   u14  u25  u36
            // *      *    *    +    +    +    +       *    *   u13  u24  u35  u46
            // *      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
            // *     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
            // *     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
            // *     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
            // *
            // *  Array elements marked * are not used by the routine; elements marked
            // *  + need not be set on entry, but are required by the routine to store
            // *  elements of U, because of fill-in resulting from the row
            // *  interchanges.
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
            // *     KV is the number of superdiagonals in the factor U, allowing for
            // *     fill-in.
            // *

            #endregion


            #region Body
            
            KV = KU + KL;
            // *
            // *     Test the input parameters.
            // *
            INFO = 0;
            if (M < 0)
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
                            if (LDAB < KL + KV + 1)
                            {
                                INFO =  - 6;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGBTF2",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (M == 0 || N == 0) return;
            // *
            // *     Gaussian elimination with partial pivoting
            // *
            // *     Set fill-in elements in columns KU+2 to KV to zero.
            // *
            for (J = KU + 2; J <= Math.Min(KV, N); J++)
            {
                AB_J = J * LDAB + o_ab;
                for (I = KV - J + 2; I <= KL; I++)
                {
                    AB[I + AB_J] = ZERO;
                }
            }
            // *
            // *     JU is the index of the last column affected by the current stage
            // *     of the factorization.
            // *
            JU = 1;
            // *
            for (J = 1; J <= Math.Min(M, N); J++)
            {
                // *
                // *        Set fill-in elements in column J+KV to zero.
                // *
                if (J + KV <= N)
                {
                    AB_0 = (J + KV) * LDAB + o_ab;
                    for (I = 1; I <= KL; I++)
                    {
                        AB[I + AB_0] = ZERO;
                    }
                }
                // *
                // *        Find pivot and test for singularity. KM is the number of
                // *        subdiagonal elements in the current column.
                // *
                KM = Math.Min(KL, M - J);
                JP = this._idamax.Run(KM + 1, AB, KV + 1+J * LDAB + o_ab, 1);
                IPIV[J + o_ipiv] = JP + J - 1;
                if (AB[KV + JP+J * LDAB + o_ab] != ZERO)
                {
                    JU = Math.Max(JU, Math.Min(J + KU + JP - 1, N));
                    // *
                    // *           Apply interchange to columns J to JU.
                    // *
                    if (JP != 1) this._dswap.Run(JU - J + 1, ref AB, KV + JP+J * LDAB + o_ab, LDAB - 1, ref AB, KV + 1+J * LDAB + o_ab, LDAB - 1);
                    // *
                    if (KM > 0)
                    {
                        // *
                        // *              Compute multipliers.
                        // *
                        this._dscal.Run(KM, ONE / AB[KV + 1+J * LDAB + o_ab], ref AB, KV + 2+J * LDAB + o_ab, 1);
                        // *
                        // *              Update trailing submatrix within the band.
                        // *
                        if (JU > J)
                        {
                            this._dger.Run(KM, JU - J,  - ONE, AB, KV + 2+J * LDAB + o_ab, 1, AB, KV+(J + 1) * LDAB + o_ab
                                           , LDAB - 1, ref AB, KV + 1+(J + 1) * LDAB + o_ab, LDAB - 1);
                        }
                    }
                }
                else
                {
                    // *
                    // *           If pivot is zero, set INFO to the index of the pivot
                    // *           unless a zero pivot has already been found.
                    // *
                    if (INFO == 0) INFO = J;
                }
            }
            return;
            // *
            // *     End of DGBTF2
            // *

            #endregion

        }
    }
}
