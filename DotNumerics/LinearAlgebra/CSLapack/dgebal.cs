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
    /// DGEBAL balances a general real matrix A.  This involves, first,
    /// permuting A by a similarity transformation to isolate eigenvalues
    /// in the first 1 to ILO-1 and last IHI+1 to N elements on the
    /// diagonal; and second, applying a diagonal similarity transformation
    /// to rows and columns ILO to IHI to make the rows and columns as
    /// close in norm as possible.  Both steps are optional.
    /// 
    /// Balancing may reduce the 1-norm of the matrix, and improve the
    /// accuracy of the computed eigenvalues and/or eigenvectors.
    /// 
    ///</summary>
    public class DGEBAL
    {
    

        #region Dependencies
        
        LSAME _lsame; IDAMAX _idamax; DLAMCH _dlamch; DSCAL _dscal; DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double SCLFAC = 2.0E+0; const double FACTOR = 0.95E+0; 

        #endregion

        public DGEBAL(LSAME lsame, IDAMAX idamax, DLAMCH dlamch, DSCAL dscal, DSWAP dswap, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._dlamch = dlamch; this._dscal = dscal; this._dswap = dswap; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DGEBAL()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IDAMAX idamax = new IDAMAX();
            DLAMC3 dlamc3 = new DLAMC3();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._dlamch = dlamch; this._dscal = dscal; this._dswap = dswap; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEBAL balances a general real matrix A.  This involves, first,
        /// permuting A by a similarity transformation to isolate eigenvalues
        /// in the first 1 to ILO-1 and last IHI+1 to N elements on the
        /// diagonal; and second, applying a diagonal similarity transformation
        /// to rows and columns ILO to IHI to make the rows and columns as
        /// close in norm as possible.  Both steps are optional.
        /// 
        /// Balancing may reduce the 1-norm of the matrix, and improve the
        /// accuracy of the computed eigenvalues and/or eigenvectors.
        /// 
        ///</summary>
        /// <param name="JOB">
        /// (input) CHARACTER*1
        /// Specifies the operations to be performed on A:
        /// = 'N':  none:  simply set ILO = 1, IHI = N, SCALE(I) = 1.0
        /// for i = 1,...,N;
        /// = 'P':  permute only;
        /// = 'S':  scale only;
        /// = 'B':  both permute and scale.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the input matrix A.
        /// On exit,  A is overwritten by the balanced matrix.
        /// If JOB = 'N', A is not referenced.
        /// See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="ILO">
        /// (output) INTEGER
        ///</param>
        /// <param name="IHI">
        /// (output) INTEGER
        /// ILO and IHI are set to integers such that on exit
        /// A(i,j) = 0 if i .GT. j and j = 1,...,ILO-1 or I = IHI+1,...,N.
        /// If JOB = 'N' or 'S', ILO = 1 and IHI = N.
        ///</param>
        /// <param name="SCALE">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// Details of the permutations and scaling factors applied to
        /// A.  If P(j) is the index of the row and column interchanged
        /// with row and column j and D(j) is the scaling factor
        /// applied to row and column j, then
        /// SCALE(j) = P(j)    for j = 1,...,ILO-1
        /// = D(j)    for j = ILO,...,IHI
        /// = P(j)    for j = IHI+1,...,N.
        /// The order in which the interchanges are made is N to IHI+1,
        /// then 1 to ILO-1.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(string JOB, int N, ref double[] A, int offset_a, int LDA, ref int ILO, ref int IHI
                         , ref double[] SCALE, int offset_scale, ref int INFO)
        {

            #region Variables
            
            bool NOCONV = false; int I = 0; int ICA = 0; int IEXC = 0; int IRA = 0; int J = 0; int K = 0; int L = 0; int M = 0; 
            double C = 0;double CA = 0; double F = 0; double G = 0; double R = 0; double RA = 0; double S = 0; double SFMAX1 = 0; 
            double SFMAX2 = 0;double SFMIN1 = 0; double SFMIN2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_scale = -1 + offset_scale; 

            #endregion


            #region Strings
            
            JOB = JOB.Substring(0, 1);  

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
            // *  DGEBAL balances a general real matrix A.  This involves, first,
            // *  permuting A by a similarity transformation to isolate eigenvalues
            // *  in the first 1 to ILO-1 and last IHI+1 to N elements on the
            // *  diagonal; and second, applying a diagonal similarity transformation
            // *  to rows and columns ILO to IHI to make the rows and columns as
            // *  close in norm as possible.  Both steps are optional.
            // *
            // *  Balancing may reduce the 1-norm of the matrix, and improve the
            // *  accuracy of the computed eigenvalues and/or eigenvectors.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOB     (input) CHARACTER*1
            // *          Specifies the operations to be performed on A:
            // *          = 'N':  none:  simply set ILO = 1, IHI = N, SCALE(I) = 1.0
            // *                  for i = 1,...,N;
            // *          = 'P':  permute only;
            // *          = 'S':  scale only;
            // *          = 'B':  both permute and scale.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the input matrix A.
            // *          On exit,  A is overwritten by the balanced matrix.
            // *          If JOB = 'N', A is not referenced.
            // *          See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  ILO     (output) INTEGER
            // *  IHI     (output) INTEGER
            // *          ILO and IHI are set to integers such that on exit
            // *          A(i,j) = 0 if i > j and j = 1,...,ILO-1 or I = IHI+1,...,N.
            // *          If JOB = 'N' or 'S', ILO = 1 and IHI = N.
            // *
            // *  SCALE   (output) DOUBLE PRECISION array, dimension (N)
            // *          Details of the permutations and scaling factors applied to
            // *          A.  If P(j) is the index of the row and column interchanged
            // *          with row and column j and D(j) is the scaling factor
            // *          applied to row and column j, then
            // *          SCALE(j) = P(j)    for j = 1,...,ILO-1
            // *                   = D(j)    for j = ILO,...,IHI
            // *                   = P(j)    for j = IHI+1,...,N.
            // *          The order in which the interchanges are made is N to IHI+1,
            // *          then 1 to ILO-1.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The permutations consist of row and column interchanges which put
            // *  the matrix in the form
            // *
            // *             ( T1   X   Y  )
            // *     P A P = (  0   B   Z  )
            // *             (  0   0   T2 )
            // *
            // *  where T1 and T2 are upper triangular matrices whose eigenvalues lie
            // *  along the diagonal.  The column indices ILO and IHI mark the starting
            // *  and ending columns of the submatrix B. Balancing consists of applying
            // *  a diagonal similarity transformation inv(D) * B * D to make the
            // *  1-norms of each row of B and its corresponding column nearly equal.
            // *  The output matrix is
            // *
            // *     ( T1     X*D          Y    )
            // *     (  0  inv(D)*B*D  inv(D)*Z ).
            // *     (  0      0           T2   )
            // *
            // *  Information about the permutations P and the diagonal matrix D is
            // *  returned in the vector SCALE.
            // *
            // *  This subroutine is based on the EISPACK routine BALANC.
            // *
            // *  Modified by Tzu-Yi Chen, Computer Science Division, University of
            // *    California at Berkeley, USA
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
            //      INTRINSIC          ABS, MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (!this._lsame.Run(JOB, "N") && !this._lsame.Run(JOB, "P") && !this._lsame.Run(JOB, "S") && !this._lsame.Run(JOB, "B"))
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
                    if (LDA < Math.Max(1, N))
                    {
                        INFO =  - 4;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGEBAL",  - INFO);
                return;
            }
            // *
            K = 1;
            L = N;
            // *
            if (N == 0) goto LABEL210;
            // *
            if (this._lsame.Run(JOB, "N"))
            {
                for (I = 1; I <= N; I++)
                {
                    SCALE[I + o_scale] = ONE;
                }
                goto LABEL210;
            }
            // *
            if (this._lsame.Run(JOB, "S")) goto LABEL120;
            // *
            // *     Permutation to isolate eigenvalues if possible
            // *
            goto LABEL50;
            // *
            // *     Row and column exchange.
            // *
        LABEL20:;
            SCALE[M + o_scale] = J;
            if (J == M) goto LABEL30;
            // *
            this._dswap.Run(L, ref A, 1+J * LDA + o_a, 1, ref A, 1+M * LDA + o_a, 1);
            this._dswap.Run(N - K + 1, ref A, J+K * LDA + o_a, LDA, ref A, M+K * LDA + o_a, LDA);
            // *
        LABEL30:;
            switch (IEXC)
            {
                case 1: goto LABEL40;
                case 2: goto LABEL80;
            }
            // *
            // *     Search for rows isolating an eigenvalue and push them down.
            // *
        LABEL40:;
            if (L == 1) goto LABEL210;
            L -= 1;
            // *
        LABEL50:;
            for (J = L; J >= 1; J +=  - 1)
            {
                // *
                for (I = 1; I <= L; I++)
                {
                    if (I == J) goto LABEL60;
                    if (A[J+I * LDA + o_a] != ZERO) goto LABEL70;
                LABEL60:;
                }
                // *
                M = L;
                IEXC = 1;
                goto LABEL20;
            LABEL70:;
            }
            // *
            goto LABEL90;
            // *
            // *     Search for columns isolating an eigenvalue and push them left.
            // *
        LABEL80:;
            K += 1;
            // *
        LABEL90:;
            for (J = K; J <= L; J++)
            {
                // *
                for (I = K; I <= L; I++)
                {
                    if (I == J) goto LABEL100;
                    if (A[I+J * LDA + o_a] != ZERO) goto LABEL110;
                LABEL100:;
                }
                // *
                M = K;
                IEXC = 2;
                goto LABEL20;
            LABEL110:;
            }
            // *
        LABEL120:;
            for (I = K; I <= L; I++)
            {
                SCALE[I + o_scale] = ONE;
            }
            // *
            if (this._lsame.Run(JOB, "P")) goto LABEL210;
            // *
            // *     Balance the submatrix in rows K to L.
            // *
            // *     Iterative loop for norm reduction
            // *
            SFMIN1 = this._dlamch.Run("S") / this._dlamch.Run("P");
            SFMAX1 = ONE / SFMIN1;
            SFMIN2 = SFMIN1 * SCLFAC;
            SFMAX2 = ONE / SFMIN2;
        LABEL140:;
            NOCONV = false;
            // *
            for (I = K; I <= L; I++)
            {
                C = ZERO;
                R = ZERO;
                // *
                for (J = K; J <= L; J++)
                {
                    if (J == I) goto LABEL150;
                    C += Math.Abs(A[J+I * LDA + o_a]);
                    R += Math.Abs(A[I+J * LDA + o_a]);
                LABEL150:;
                }
                ICA = this._idamax.Run(L, A, 1+I * LDA + o_a, 1);
                CA = Math.Abs(A[ICA+I * LDA + o_a]);
                IRA = this._idamax.Run(N - K + 1, A, I+K * LDA + o_a, LDA);
                RA = Math.Abs(A[I+(IRA + K - 1) * LDA + o_a]);
                // *
                // *        Guard against zero C or R due to underflow.
                // *
                if (C == ZERO || R == ZERO) goto LABEL200;
                G = R / SCLFAC;
                F = ONE;
                S = C + R;
            LABEL160:;
                if (C >= G || Math.Max(F, Math.Max(C, CA)) >= SFMAX2 || Math.Min(R, Math.Min(G, RA)) <= SFMIN2) goto LABEL170;
                F *= SCLFAC;
                C *= SCLFAC;
                CA *= SCLFAC;
                R /= SCLFAC;
                G /= SCLFAC;
                RA /= SCLFAC;
                goto LABEL160;
                // *
            LABEL170:;
                G = C / SCLFAC;
            LABEL180:;
                if (G < R || Math.Max(R, RA) >= SFMAX2 || Math.Min(F, Math.Min(C, Math.Min(G, CA))) <= SFMIN2) goto LABEL190;
                F /= SCLFAC;
                C /= SCLFAC;
                G /= SCLFAC;
                CA /= SCLFAC;
                R *= SCLFAC;
                RA *= SCLFAC;
                goto LABEL180;
                // *
                // *        Now balance.
                // *
            LABEL190:;
                if ((C + R) >= FACTOR * S) goto LABEL200;
                if (F < ONE && SCALE[I + o_scale] < ONE)
                {
                    if (F * SCALE[I + o_scale] <= SFMIN1) goto LABEL200;
                }
                if (F > ONE && SCALE[I + o_scale] > ONE)
                {
                    if (SCALE[I + o_scale] >= SFMAX1 / F) goto LABEL200;
                }
                G = ONE / F;
                SCALE[I + o_scale] *= F;
                NOCONV = true;
                // *
                this._dscal.Run(N - K + 1, G, ref A, I+K * LDA + o_a, LDA);
                this._dscal.Run(L, F, ref A, 1+I * LDA + o_a, 1);
                // *
            LABEL200:;
            }
            // *
            if (NOCONV) goto LABEL140;
            // *
        LABEL210:;
            ILO = K;
            IHI = L;
            // *
            return;
            // *
            // *     End of DGEBAL
            // *

            #endregion

        }
    }
}
