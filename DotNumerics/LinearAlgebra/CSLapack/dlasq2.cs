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
    /// DLASQ2 computes all the eigenvalues of the symmetric positive 
    /// definite tridiagonal matrix associated with the qd array Z to high
    /// relative accuracy are computed to high relative accuracy, in the
    /// absence of denormalization, underflow and overflow.
    /// 
    /// To see the relation of Z to the tridiagonal matrix, let L be a
    /// unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
    /// let U be an upper bidiagonal matrix with 1's above and diagonal
    /// Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
    /// symmetric tridiagonal to which it is similar.
    /// 
    /// Note : DLASQ2 defines a logical variable, IEEE, which is true
    /// on machines which follow ieee-754 floating-point standard in their
    /// handling of infinities and NaNs, and false otherwise. This variable
    /// is passed to DLAZQ3.
    /// 
    ///</summary>
    public class DLASQ2
    {
    

        #region Dependencies
        
        DLAZQ3 _dlazq3; DLASRT _dlasrt; XERBLA _xerbla; DLAMCH _dlamch; ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const double CBIAS = 1.50E0; const double ZERO = 0.0E0; const double HALF = 0.5E0; const double ONE = 1.0E0; 
        const double TWO = 2.0E0;const double FOUR = 4.0E0; const double HUNDRD = 100.0E0; 

        #endregion

        public DLASQ2(DLAZQ3 dlazq3, DLASRT dlasrt, XERBLA xerbla, DLAMCH dlamch, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dlazq3 = dlazq3; this._dlasrt = dlasrt; this._xerbla = xerbla; this._dlamch = dlamch; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DLASQ2()
        {
    

            #region Dependencies (Initialization)
            
            DLASQ5 dlasq5 = new DLASQ5();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAZQ4 dlazq4 = new DLAZQ4();
            XERBLA xerbla = new XERBLA();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLASQ6 dlasq6 = new DLASQ6(dlamch);
            DLAZQ3 dlazq3 = new DLAZQ3(dlasq5, dlasq6, dlazq4, dlamch);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);

            #endregion


            #region Set Dependencies
            
            this._dlazq3 = dlazq3; this._dlasrt = dlasrt; this._xerbla = xerbla; this._dlamch = dlamch; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASQ2 computes all the eigenvalues of the symmetric positive 
        /// definite tridiagonal matrix associated with the qd array Z to high
        /// relative accuracy are computed to high relative accuracy, in the
        /// absence of denormalization, underflow and overflow.
        /// 
        /// To see the relation of Z to the tridiagonal matrix, let L be a
        /// unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
        /// let U be an upper bidiagonal matrix with 1's above and diagonal
        /// Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
        /// symmetric tridiagonal to which it is similar.
        /// 
        /// Note : DLASQ2 defines a logical variable, IEEE, which is true
        /// on machines which follow ieee-754 floating-point standard in their
        /// handling of infinities and NaNs, and false otherwise. This variable
        /// is passed to DLAZQ3.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of rows and columns in the matrix. N .GE. 0.
        ///</param>
        /// <param name="Z">
        /// (workspace) DOUBLE PRECISION array, dimension ( 4*N )
        /// On entry Z holds the qd array. On exit, entries 1 to N hold
        /// the eigenvalues in decreasing order, Z( 2*N+1 ) holds the
        /// trace, and Z( 2*N+2 ) holds the sum of the eigenvalues. If
        /// N .GT. 2, then Z( 2*N+3 ) holds the iteration count, Z( 2*N+4 )
        /// holds NDIVS/NIN^2, and Z( 2*N+5 ) holds the percentage of
        /// shifts that failed.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if the i-th argument is a scalar and had an illegal
        /// value, then INFO = -i, if the i-th argument is an
        /// array and the j-entry had an illegal value, then
        /// INFO = -(i*100+j)
        /// .GT. 0: the algorithm failed
        /// = 1, a split was marked by a positive value in E
        /// = 2, current block of Z not diagonalized after 30*N
        /// iterations (in inner while loop)
        /// = 3, termination criterion of outer while loop not met 
        /// (program created more than N unreduced blocks)
        ///</param>
        public void Run(int N, ref double[] Z, int offset_z, ref int INFO)
        {

            #region Variables
            
            bool IEEE = false; int I0 = 0; int I4 = 0; int IINFO = 0; int IPN4 = 0; int ITER = 0; int IWHILA = 0; int IWHILB = 0; 
            int K = 0;int N0 = 0; int NBIG = 0; int NDIV = 0; int NFAIL = 0; int PP = 0; int SPLT = 0; int TTYPE = 0; 
            double D = 0;double DESIG = 0; double DMIN = 0; double DMIN1 = 0; double DMIN2 = 0; double DN = 0; double DN1 = 0; 
            double DN2 = 0;double E = 0; double EMAX = 0; double EMIN = 0; double EPS = 0; double OLDEMN = 0; double QMAX = 0; 
            double QMIN = 0;double S = 0; double SAFMIN = 0; double SIGMA = 0; double T = 0; double TAU = 0; double TEMP = 0; 
            double TOL = 0;double TOL2 = 0; double TRACE = 0; double ZMAX = 0; 

            #endregion


            #region Array Index Correction
            
             int o_z = -1 + offset_z; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     Modified to call DLAZQ3 in place of DLASQ3, 13 Feb 03, SJH.
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLASQ2 computes all the eigenvalues of the symmetric positive 
            // *  definite tridiagonal matrix associated with the qd array Z to high
            // *  relative accuracy are computed to high relative accuracy, in the
            // *  absence of denormalization, underflow and overflow.
            // *
            // *  To see the relation of Z to the tridiagonal matrix, let L be a
            // *  unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
            // *  let U be an upper bidiagonal matrix with 1's above and diagonal
            // *  Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
            // *  symmetric tridiagonal to which it is similar.
            // *
            // *  Note : DLASQ2 defines a logical variable, IEEE, which is true
            // *  on machines which follow ieee-754 floating-point standard in their
            // *  handling of infinities and NaNs, and false otherwise. This variable
            // *  is passed to DLAZQ3.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N     (input) INTEGER
            // *        The number of rows and columns in the matrix. N >= 0.
            // *
            // *  Z     (workspace) DOUBLE PRECISION array, dimension ( 4*N )
            // *        On entry Z holds the qd array. On exit, entries 1 to N hold
            // *        the eigenvalues in decreasing order, Z( 2*N+1 ) holds the
            // *        trace, and Z( 2*N+2 ) holds the sum of the eigenvalues. If
            // *        N > 2, then Z( 2*N+3 ) holds the iteration count, Z( 2*N+4 )
            // *        holds NDIVS/NIN^2, and Z( 2*N+5 ) holds the percentage of
            // *        shifts that failed.
            // *
            // *  INFO  (output) INTEGER
            // *        = 0: successful exit
            // *        < 0: if the i-th argument is a scalar and had an illegal
            // *             value, then INFO = -i, if the i-th argument is an
            // *             array and the j-entry had an illegal value, then
            // *             INFO = -(i*100+j)
            // *        > 0: the algorithm failed
            // *              = 1, a split was marked by a positive value in E
            // *              = 2, current block of Z not diagonalized after 30*N
            // *                   iterations (in inner while loop)
            // *              = 3, termination criterion of outer while loop not met 
            // *                   (program created more than N unreduced blocks)
            // *
            // *  Further Details
            // *  ===============
            // *  Local Variables: I0:N0 defines a current unreduced segment of Z.
            // *  The shifts are accumulated in SIGMA. Iteration count is in ITER.
            // *  Ping-pong is controlled by PP (alternates between 0 and 1).
            // *
            // *  =====================================================================
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
            //      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *      
            // *     Test the input arguments.
            // *     (in case DLASQ2 is not called by DLASQ1)
            // *

            #endregion


            #region Body
            
            INFO = 0;
            EPS = this._dlamch.Run("Precision");
            SAFMIN = this._dlamch.Run("Safe minimum");
            TOL = EPS * HUNDRD;
            TOL2 = Math.Pow(TOL,2);
            // *
            if (N < 0)
            {
                INFO =  - 1;
                this._xerbla.Run("DLASQ2", 1);
                return;
            }
            else
            {
                if (N == 0)
                {
                    return;
                }
                else
                {
                    if (N == 1)
                    {
                        // *
                        // *        1-by-1 case.
                        // *
                        if (Z[1 + o_z] < ZERO)
                        {
                            INFO =  - 201;
                            this._xerbla.Run("DLASQ2", 2);
                        }
                        return;
                    }
                    else
                    {
                        if (N == 2)
                        {
                            // *
                            // *        2-by-2 case.
                            // *
                            if (Z[2 + o_z] < ZERO || Z[3 + o_z] < ZERO)
                            {
                                INFO =  - 2;
                                this._xerbla.Run("DLASQ2", 2);
                                return;
                            }
                            else
                            {
                                if (Z[3 + o_z] > Z[1 + o_z])
                                {
                                    D = Z[3 + o_z];
                                    Z[3 + o_z] = Z[1 + o_z];
                                    Z[1 + o_z] = D;
                                }
                            }
                            Z[5 + o_z] = Z[1 + o_z] + Z[2 + o_z] + Z[3 + o_z];
                            if (Z[2 + o_z] > Z[3 + o_z] * TOL2)
                            {
                                T = HALF * ((Z[1 + o_z] - Z[3 + o_z]) + Z[2 + o_z]);
                                S = Z[3 + o_z] * (Z[2 + o_z] / T);
                                if (S <= T)
                                {
                                    S = Z[3 + o_z] * (Z[2 + o_z] / (T * (ONE + Math.Sqrt(ONE + S / T))));
                                }
                                else
                                {
                                    S = Z[3 + o_z] * (Z[2 + o_z] / (T + Math.Sqrt(T) * Math.Sqrt(T + S)));
                                }
                                T = Z[1 + o_z] + (S + Z[2 + o_z]);
                                Z[3 + o_z] = Z[3 + o_z] * (Z[1 + o_z] / T);
                                Z[1 + o_z] = T;
                            }
                            Z[2 + o_z] = Z[3 + o_z];
                            Z[6 + o_z] = Z[2 + o_z] + Z[1 + o_z];
                            return;
                        }
                    }
                }
            }
            // *
            // *     Check for negative data and compute sums of q's and e's.
            // *
            Z[2 * N + o_z] = ZERO;
            EMIN = Z[2 + o_z];
            QMAX = ZERO;
            ZMAX = ZERO;
            D = ZERO;
            E = ZERO;
            // *
            for (K = 1; K <= 2 * (N - 1); K += 2)
            {
                if (Z[K + o_z] < ZERO)
                {
                    INFO =  - (200 + K);
                    this._xerbla.Run("DLASQ2", 2);
                    return;
                }
                else
                {
                    if (Z[K + 1 + o_z] < ZERO)
                    {
                        INFO =  - (200 + K + 1);
                        this._xerbla.Run("DLASQ2", 2);
                        return;
                    }
                }
                D += Z[K + o_z];
                E += Z[K + 1 + o_z];
                QMAX = Math.Max(QMAX, Z[K + o_z]);
                EMIN = Math.Min(EMIN, Z[K + 1 + o_z]);
                ZMAX = Math.Max(QMAX, Math.Max(ZMAX, Z[K + 1 + o_z]));
            }
            if (Z[2 * N - 1 + o_z] < ZERO)
            {
                INFO =  - (200 + 2 * N - 1);
                this._xerbla.Run("DLASQ2", 2);
                return;
            }
            D += Z[2 * N - 1 + o_z];
            QMAX = Math.Max(QMAX, Z[2 * N - 1 + o_z]);
            ZMAX = Math.Max(QMAX, ZMAX);
            // *
            // *     Check for diagonality.
            // *
            if (E == ZERO)
            {
                for (K = 2; K <= N; K++)
                {
                    Z[K + o_z] = Z[2 * K - 1 + o_z];
                }
                this._dlasrt.Run("D", N, ref Z, offset_z, ref IINFO);
                Z[2 * N - 1 + o_z] = D;
                return;
            }
            // *
            TRACE = D + E;
            // *
            // *     Check for zero data.
            // *
            if (TRACE == ZERO)
            {
                Z[2 * N - 1 + o_z] = ZERO;
                return;
            }
            // *         
            // *     Check whether the machine is IEEE conformable.
            // *         
            IEEE = this._ilaenv.Run(10, "DLASQ2", "N", 1, 2, 3, 4) == 1 && this._ilaenv.Run(11, "DLASQ2", "N", 1, 2, 3, 4) == 1;
            // *         
            // *     Rearrange data for locality: Z=(q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
            // *
            for (K = 2 * N; K >= 2; K +=  - 2)
            {
                Z[2 * K + o_z] = ZERO;
                Z[2 * K - 1 + o_z] = Z[K + o_z];
                Z[2 * K - 2 + o_z] = ZERO;
                Z[2 * K - 3 + o_z] = Z[K - 1 + o_z];
            }
            // *
            I0 = 1;
            N0 = N;
            // *
            // *     Reverse the qd-array, if warranted.
            // *
            if (CBIAS * Z[4 * I0 - 3 + o_z] < Z[4 * N0 - 3 + o_z])
            {
                IPN4 = 4 * (I0 + N0);
                for (I4 = 4 * I0; I4 <= 2 * (I0 + N0 - 1); I4 += 4)
                {
                    TEMP = Z[I4 - 3 + o_z];
                    Z[I4 - 3 + o_z] = Z[IPN4 - I4 - 3 + o_z];
                    Z[IPN4 - I4 - 3 + o_z] = TEMP;
                    TEMP = Z[I4 - 1 + o_z];
                    Z[I4 - 1 + o_z] = Z[IPN4 - I4 - 5 + o_z];
                    Z[IPN4 - I4 - 5 + o_z] = TEMP;
                }
            }
            // *
            // *     Initial split checking via dqd and Li's test.
            // *
            PP = 0;
            // *
            for (K = 1; K <= 2; K++)
            {
                // *
                D = Z[4 * N0 + PP - 3 + o_z];
                for (I4 = 4 * (N0 - 1) + PP; I4 >= 4 * I0 + PP; I4 +=  - 4)
                {
                    if (Z[I4 - 1 + o_z] <= TOL2 * D)
                    {
                        Z[I4 - 1 + o_z] =  - ZERO;
                        D = Z[I4 - 3 + o_z];
                    }
                    else
                    {
                        D = Z[I4 - 3 + o_z] * (D / (D + Z[I4 - 1 + o_z]));
                    }
                }
                // *
                // *        dqd maps Z to ZZ plus Li's test.
                // *
                EMIN = Z[4 * I0 + PP + 1 + o_z];
                D = Z[4 * I0 + PP - 3 + o_z];
                for (I4 = 4 * I0 + PP; I4 <= 4 * (N0 - 1) + PP; I4 += 4)
                {
                    Z[I4 - 2 * PP - 2 + o_z] = D + Z[I4 - 1 + o_z];
                    if (Z[I4 - 1 + o_z] <= TOL2 * D)
                    {
                        Z[I4 - 1 + o_z] =  - ZERO;
                        Z[I4 - 2 * PP - 2 + o_z] = D;
                        Z[I4 - 2 * PP + o_z] = ZERO;
                        D = Z[I4 + 1 + o_z];
                    }
                    else
                    {
                        if (SAFMIN * Z[I4 + 1 + o_z] < Z[I4 - 2 * PP - 2 + o_z] && SAFMIN * Z[I4 - 2 * PP - 2 + o_z] < Z[I4 + 1 + o_z])
                        {
                            TEMP = Z[I4 + 1 + o_z] / Z[I4 - 2 * PP - 2 + o_z];
                            Z[I4 - 2 * PP + o_z] = Z[I4 - 1 + o_z] * TEMP;
                            D *= TEMP;
                        }
                        else
                        {
                            Z[I4 - 2 * PP + o_z] = Z[I4 + 1 + o_z] * (Z[I4 - 1 + o_z] / Z[I4 - 2 * PP - 2 + o_z]);
                            D = Z[I4 + 1 + o_z] * (D / Z[I4 - 2 * PP - 2 + o_z]);
                        }
                    }
                    EMIN = Math.Min(EMIN, Z[I4 - 2 * PP + o_z]);
                }
                Z[4 * N0 - PP - 2 + o_z] = D;
                // *
                // *        Now find qmax.
                // *
                QMAX = Z[4 * I0 - PP - 2 + o_z];
                for (I4 = 4 * I0 - PP + 2; I4 <= 4 * N0 - PP - 2; I4 += 4)
                {
                    QMAX = Math.Max(QMAX, Z[I4 + o_z]);
                }
                // *
                // *        Prepare for the next iteration on K.
                // *
                PP = 1 - PP;
            }
            // *
            // *     Initialise variables to pass to DLAZQ3
            // *
            TTYPE = 0;
            DMIN1 = ZERO;
            DMIN2 = ZERO;
            DN = ZERO;
            DN1 = ZERO;
            DN2 = ZERO;
            TAU = ZERO;
            // *
            ITER = 2;
            NFAIL = 0;
            NDIV = 2 * (N0 - I0);
            // *
            for (IWHILA = 1; IWHILA <= N + 1; IWHILA++)
            {
                if (N0 < 1) goto LABEL150;
                // *
                // *        While array unfinished do 
                // *
                // *        E(N0) holds the value of SIGMA when submatrix in I0:N0
                // *        splits from the rest of the array, but is negated.
                // *      
                DESIG = ZERO;
                if (N0 == N)
                {
                    SIGMA = ZERO;
                }
                else
                {
                    SIGMA =  - Z[4 * N0 - 1 + o_z];
                }
                if (SIGMA < ZERO)
                {
                    INFO = 1;
                    return;
                }
                // *
                // *        Find last unreduced submatrix's top index I0, find QMAX and
                // *        EMIN. Find Gershgorin-type bound if Q's much greater than E's.
                // *
                EMAX = ZERO;
                if (N0 > I0)
                {
                    EMIN = Math.Abs(Z[4 * N0 - 5 + o_z]);
                }
                else
                {
                    EMIN = ZERO;
                }
                QMIN = Z[4 * N0 - 3 + o_z];
                QMAX = QMIN;
                for (I4 = 4 * N0; I4 >= 8; I4 +=  - 4)
                {
                    if (Z[I4 - 5 + o_z] <= ZERO) goto LABEL100;
                    if (QMIN >= FOUR * EMAX)
                    {
                        QMIN = Math.Min(QMIN, Z[I4 - 3 + o_z]);
                        EMAX = Math.Max(EMAX, Z[I4 - 5 + o_z]);
                    }
                    QMAX = Math.Max(QMAX, Z[I4 - 7 + o_z] + Z[I4 - 5 + o_z]);
                    EMIN = Math.Min(EMIN, Z[I4 - 5 + o_z]);
                }
                I4 = 4;
                // *
            LABEL100:;
                I0 = I4 / 4;
                // *
                // *        Store EMIN for passing to DLAZQ3.
                // *
                Z[4 * N0 - 1 + o_z] = EMIN;
                // *
                // *        Put -(initial shift) into DMIN.
                // *
                DMIN =  - Math.Max(ZERO, QMIN - TWO * Math.Sqrt(QMIN) * Math.Sqrt(EMAX));
                // *
                // *        Now I0:N0 is unreduced. PP = 0 for ping, PP = 1 for pong.
                // *
                PP = 0;
                // *
                NBIG = 30 * (N0 - I0 + 1);
                for (IWHILB = 1; IWHILB <= NBIG; IWHILB++)
                {
                    if (I0 > N0) goto LABEL130;
                    // *
                    // *           While submatrix unfinished take a good dqds step.
                    // *
                    this._dlazq3.Run(I0, ref N0, ref Z, offset_z, PP, ref DMIN, ref SIGMA
                                     , ref DESIG, ref QMAX, ref NFAIL, ref ITER, ref NDIV, IEEE
                                     , ref TTYPE, ref DMIN1, ref DMIN2, ref DN, ref DN1, ref DN2
                                     , ref TAU);
                    // *
                    PP = 1 - PP;
                    // *
                    // *           When EMIN is very small check for splits.
                    // *
                    if (PP == 0 && N0 - I0 >= 3)
                    {
                        if (Z[4 * N0 + o_z] <= TOL2 * QMAX || Z[4 * N0 - 1 + o_z] <= TOL2 * SIGMA)
                        {
                            SPLT = I0 - 1;
                            QMAX = Z[4 * I0 - 3 + o_z];
                            EMIN = Z[4 * I0 - 1 + o_z];
                            OLDEMN = Z[4 * I0 + o_z];
                            for (I4 = 4 * I0; I4 <= 4 * (N0 - 3); I4 += 4)
                            {
                                if (Z[I4 + o_z] <= TOL2 * Z[I4 - 3 + o_z] || Z[I4 - 1 + o_z] <= TOL2 * SIGMA)
                                {
                                    Z[I4 - 1 + o_z] =  - SIGMA;
                                    SPLT = I4 / 4;
                                    QMAX = ZERO;
                                    EMIN = Z[I4 + 3 + o_z];
                                    OLDEMN = Z[I4 + 4 + o_z];
                                }
                                else
                                {
                                    QMAX = Math.Max(QMAX, Z[I4 + 1 + o_z]);
                                    EMIN = Math.Min(EMIN, Z[I4 - 1 + o_z]);
                                    OLDEMN = Math.Min(OLDEMN, Z[I4 + o_z]);
                                }
                            }
                            Z[4 * N0 - 1 + o_z] = EMIN;
                            Z[4 * N0 + o_z] = OLDEMN;
                            I0 = SPLT + 1;
                        }
                    }
                    // *
                }
                // *
                INFO = 2;
                return;
                // *
                // *        end IWHILB
                // *
            LABEL130:;
                // *
            }
            // *
            INFO = 3;
            return;
            // *
            // *     end IWHILA   
            // *
        LABEL150:;
            // *      
            // *     Move q's to the front.
            // *      
            for (K = 2; K <= N; K++)
            {
                Z[K + o_z] = Z[4 * K - 3 + o_z];
            }
            // *      
            // *     Sort and compute sum of eigenvalues.
            // *
            this._dlasrt.Run("D", N, ref Z, offset_z, ref IINFO);
            // *
            E = ZERO;
            for (K = N; K >= 1; K +=  - 1)
            {
                E += Z[K + o_z];
            }
            // *
            // *     Store trace, sum(eigenvalues) and information on performance.
            // *
            Z[2 * N + 1 + o_z] = TRACE;
            Z[2 * N + 2 + o_z] = E;
            Z[2 * N + 3 + o_z] = Convert.ToDouble(ITER);
            Z[2 * N + 4 + o_z] = Convert.ToDouble(NDIV) / Convert.ToDouble(Math.Pow(N,2));
            Z[2 * N + 5 + o_z] = HUNDRD * NFAIL / Convert.ToDouble(ITER);
            return;
            // *
            // *     End of DLASQ2
            // *

            #endregion

        }
    }
}
