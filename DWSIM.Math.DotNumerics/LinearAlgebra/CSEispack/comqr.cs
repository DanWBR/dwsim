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

namespace DotNumerics.LinearAlgebra.CSEispack
{
    public class COMQR
    {
    

        #region Dependencies
        
        PYTHAG _pythag; CSROOT _csroot; CDIV _cdiv; 

        #endregion

        public COMQR(PYTHAG pythag, CSROOT csroot, CDIV cdiv)
        {
    

            #region Set Dependencies
            
            this._pythag = pythag; this._csroot = csroot; this._cdiv = cdiv; 

            #endregion

        }
    
        public COMQR()
        {
    

            #region Dependencies (Initialization)
            
            PYTHAG pythag = new PYTHAG();
            CDIV cdiv = new CDIV();
            CSROOT csroot = new CSROOT(pythag);

            #endregion


            #region Set Dependencies
            
            this._pythag = pythag; this._csroot = csroot; this._cdiv = cdiv; 

            #endregion

        }
        /// <param name="NM">
        /// must be set to the row dimension of two-dimensional
        /// array parameters as declared in the calling program
        /// dimension statement.
        ///</param>
        /// <param name="N">
        /// is the order of the matrix.
        ///</param>
        /// <param name="LOW">
        /// and igh are integers determined by the balancing
        /// subroutine  cbal.  if  cbal  has not been used,
        /// set low=1, igh=n.
        ///</param>
        /// <param name="HR">
        /// and hi contain the real and imaginary parts,
        /// respectively, of the complex upper hessenberg matrix.
        /// their lower triangles below the subdiagonal contain
        /// information about the unitary transformations used in
        /// the reduction by  corth, if performed.
        ///</param>
        /// <param name="WR">
        /// and wi contain the real and imaginary parts,
        /// respectively, of the eigenvalues.  if an error
        /// exit is made, the eigenvalues should be correct
        /// for indices ierr+1,...,n.
        ///</param>
        /// <param name="IERR">
        /// is set to
        /// zero       for normal return,
        /// j          if the limit of 30*n iterations is exhausted
        /// while the j-th eigenvalue is being sought.
        ///</param>
        public void Run(int NM, int N, int LOW, int IGH, ref double[] HR, int offset_hr, ref double[] HI, int offset_hi
                         , ref double[] WR, int offset_wr, ref double[] WI, int offset_wi, ref int IERR)
        {

            #region Variables
            
            int I = 0; int J = 0; int L = 0; int EN = 0; int LL = 0; int ITN = 0; int ITS = 0; int LP1 = 0; int ENM1 = 0; 
            double SI = 0;double SR = 0; double TI = 0; double TR = 0; double XI = 0; double XR = 0; double YI = 0; double YR = 0; 
            double ZZI = 0;double ZZR = 0; double NORM = 0; double TST1 = 0; double TST2 = 0; 

            #endregion


            #region Implicit Variables
            
            int HI_I = 0; int HR_I = 0; int HR_0 = 0; int HR_J = 0; int HI_J = 0; int HI_0 = 0; int HI_1 = 0; int HR_EN = 0; 
            int HI_EN = 0;

            #endregion


            #region Array Index Correction
            
             int o_hr = -1 - NM + offset_hr;  int o_hi = -1 - NM + offset_hi;  int o_wr = -1 + offset_wr; 
             int o_wi = -1 + offset_wi;

            #endregion


            #region Prolog
            
            // c
            // c
            // c     this subroutine is a translation of a unitary analogue of the
            // c     algol procedure  comlr, num. math. 12, 369-376(1968) by martin
            // c     and wilkinson.
            // c     handbook for auto. comp., vol.ii-linear algebra, 396-403(1971).
            // c     the unitary analogue substitutes the qr algorithm of francis
            // c     (comp. jour. 4, 332-345(1962)) for the lr algorithm.
            // c
            // c     this subroutine finds the eigenvalues of a complex
            // c     upper hessenberg matrix by the qr method.
            // c
            // c     on input
            // c
            // c        nm must be set to the row dimension of two-dimensional
            // c          array parameters as declared in the calling program
            // c          dimension statement.
            // c
            // c        n is the order of the matrix.
            // c
            // c        low and igh are integers determined by the balancing
            // c          subroutine  cbal.  if  cbal  has not been used,
            // c          set low=1, igh=n.
            // c
            // c        hr and hi contain the real and imaginary parts,
            // c          respectively, of the complex upper hessenberg matrix.
            // c          their lower triangles below the subdiagonal contain
            // c          information about the unitary transformations used in
            // c          the reduction by  corth, if performed.
            // c
            // c     on output
            // c
            // c        the upper hessenberg portions of hr and hi have been
            // c          destroyed.  therefore, they must be saved before
            // c          calling  comqr  if subsequent calculation of
            // c          eigenvectors is to be performed.
            // c
            // c        wr and wi contain the real and imaginary parts,
            // c          respectively, of the eigenvalues.  if an error
            // c          exit is made, the eigenvalues should be correct
            // c          for indices ierr+1,...,n.
            // c
            // c        ierr is set to
            // c          zero       for normal return,
            // c          j          if the limit of 30*n iterations is exhausted
            // c                     while the j-th eigenvalue is being sought.
            // c
            // c     calls cdiv for complex division.
            // c     calls csroot for complex square root.
            // c     calls pythag for  dsqrt(a*a + b*b) .
            // c
            // c     questions and comments should be directed to burton s. garbow,
            // c     mathematics and computer science div, argonne national laboratory
            // c
            // c     this version dated august 1983.
            // c
            // c     ------------------------------------------------------------------
            // c

            #endregion


            #region Body
            
            IERR = 0;
            if (LOW == IGH) goto LABEL180;
            // c     .......... create real subdiagonal elements ..........
            L = LOW + 1;
            // c
            for (I = L; I <= IGH; I++)
            {
                LL = Math.Min(I + 1, IGH);
                if (HI[I+(I - 1) * NM + o_hi] == 0.0E0) goto LABEL170;
                NORM = this._pythag.Run(HR[I+(I - 1) * NM + o_hr], HI[I+(I - 1) * NM + o_hi]);
                YR = HR[I+(I - 1) * NM + o_hr] / NORM;
                YI = HI[I+(I - 1) * NM + o_hi] / NORM;
                HR[I+(I - 1) * NM + o_hr] = NORM;
                HI[I+(I - 1) * NM + o_hi] = 0.0E0;
                // c
                for (J = I; J <= IGH; J++)
                {
                    SI = YR * HI[I+J * NM + o_hi] - YI * HR[I+J * NM + o_hr];
                    HR[I+J * NM + o_hr] = YR * HR[I+J * NM + o_hr] + YI * HI[I+J * NM + o_hi];
                    HI[I+J * NM + o_hi] = SI;
                }
                // c
                HI_I = I * NM + o_hi;
                HR_I = I * NM + o_hr;
                for (J = LOW; J <= LL; J++)
                {
                    SI = YR * HI[J + HI_I] + YI * HR[J + HR_I];
                    HR[J + HR_I] = YR * HR[J + HR_I] - YI * HI[J + HI_I];
                    HI[J + HI_I] = SI;
                }
                // c
            LABEL170:;
            }
            // c     .......... store roots isolated by cbal ..........
        LABEL180:  
            for (I = 1; I <= N; I++)
            {
                if (I >= LOW && I <= IGH) goto LABEL200;
                WR[I + o_wr] = HR[I+I * NM + o_hr];
                WI[I + o_wi] = HI[I+I * NM + o_hi];
            LABEL200:;
            }
            // c
            EN = IGH;
            TR = 0.0E0;
            TI = 0.0E0;
            ITN = 30 * N;
            // c     .......... search for next eigenvalue ..........
        LABEL220:  
            if (EN < LOW) goto LABEL1001;
            ITS = 0;
            ENM1 = EN - 1;
            // c     .......... look for single small sub-diagonal element
            // c                for l=en step -1 until low d0 -- ..........
        LABEL240:  
            for (LL = LOW; LL <= EN; LL++)
            {
                L = EN + LOW - LL;
                if (L == LOW) goto LABEL300;
                TST1 = Math.Abs(HR[L - 1+(L - 1) * NM + o_hr]) + Math.Abs(HI[L - 1+(L - 1) * NM + o_hi]) + Math.Abs(HR[L+L * NM + o_hr]) + Math.Abs(HI[L+L * NM + o_hi]);
                TST2 = TST1 + Math.Abs(HR[L+(L - 1) * NM + o_hr]);
                if (TST2 == TST1) goto LABEL300;
            }
            // c     .......... form shift ..........
        LABEL300:  
            if (L == EN) goto LABEL660;
            if (ITN == 0) goto LABEL1000;
            if (ITS == 10 || ITS == 20) goto LABEL320;
            SR = HR[EN+EN * NM + o_hr];
            SI = HI[EN+EN * NM + o_hi];
            XR = HR[ENM1+EN * NM + o_hr] * HR[EN+ENM1 * NM + o_hr];
            XI = HI[ENM1+EN * NM + o_hi] * HR[EN+ENM1 * NM + o_hr];
            if (XR == 0.0E0 && XI == 0.0E0) goto LABEL340;
            YR = (HR[ENM1+ENM1 * NM + o_hr] - SR) / 2.0E0;
            YI = (HI[ENM1+ENM1 * NM + o_hi] - SI) / 2.0E0;
            this._csroot.Run(Math.Pow(YR,2) - Math.Pow(YI,2) + XR, 2.0E0 * YR * YI + XI, ref ZZR, ref ZZI);
            if (YR * ZZR + YI * ZZI >= 0.0E0) goto LABEL310;
            ZZR =  - ZZR;
            ZZI =  - ZZI;
        LABEL310:  this._cdiv.Run(XR, XI, YR + ZZR, YI + ZZI, ref XR, ref XI);
            SR -= XR;
            SI -= XI;
            goto LABEL340;
            // c     .......... form exceptional shift ..........
        LABEL320:  SR = Math.Abs(HR[EN+ENM1 * NM + o_hr]) + Math.Abs(HR[ENM1+(EN - 2) * NM + o_hr]);
            SI = 0.0E0;
            // c
        LABEL340:  
            for (I = LOW; I <= EN; I++)
            {
                HR[I+I * NM + o_hr] -= SR;
                HI[I+I * NM + o_hi] -= SI;
            }
            // c
            TR += SR;
            TI += SI;
            ITS += 1;
            ITN -= 1;
            // c     .......... reduce to triangle (rows) ..........
            LP1 = L + 1;
            // c
            for (I = LP1; I <= EN; I++)
            {
                SR = HR[I+(I - 1) * NM + o_hr];
                HR[I+(I - 1) * NM + o_hr] = 0.0E0;
                NORM = this._pythag.Run(this._pythag.Run(HR[I - 1+(I - 1) * NM + o_hr], HI[I - 1+(I - 1) * NM + o_hi]), SR);
                XR = HR[I - 1+(I - 1) * NM + o_hr] / NORM;
                WR[I - 1 + o_wr] = XR;
                XI = HI[I - 1+(I - 1) * NM + o_hi] / NORM;
                WI[I - 1 + o_wi] = XI;
                HR[I - 1+(I - 1) * NM + o_hr] = NORM;
                HI[I - 1+(I - 1) * NM + o_hi] = 0.0E0;
                HI[I+(I - 1) * NM + o_hi] = SR / NORM;
                // c
                for (J = I; J <= EN; J++)
                {
                    YR = HR[I - 1+J * NM + o_hr];
                    YI = HI[I - 1+J * NM + o_hi];
                    ZZR = HR[I+J * NM + o_hr];
                    ZZI = HI[I+J * NM + o_hi];
                    HR[I - 1+J * NM + o_hr] = XR * YR + XI * YI + HI[I+(I - 1) * NM + o_hi] * ZZR;
                    HI[I - 1+J * NM + o_hi] = XR * YI - XI * YR + HI[I+(I - 1) * NM + o_hi] * ZZI;
                    HR[I+J * NM + o_hr] = XR * ZZR - XI * ZZI - HI[I+(I - 1) * NM + o_hi] * YR;
                    HI[I+J * NM + o_hi] = XR * ZZI + XI * ZZR - HI[I+(I - 1) * NM + o_hi] * YI;
                }
                // c
            }
            // c
            SI = HI[EN+EN * NM + o_hi];
            if (SI == 0.0E0) goto LABEL540;
            NORM = this._pythag.Run(HR[EN+EN * NM + o_hr], SI);
            SR = HR[EN+EN * NM + o_hr] / NORM;
            SI /= NORM;
            HR[EN+EN * NM + o_hr] = NORM;
            HI[EN+EN * NM + o_hi] = 0.0E0;
            // c     .......... inverse operation (columns) ..........
        LABEL540:  
            for (J = LP1; J <= EN; J++)
            {
                XR = WR[J - 1 + o_wr];
                XI = WI[J - 1 + o_wi];
                // c
                HR_0 = (J - 1) * NM + o_hr;
                HR_J = J * NM + o_hr;
                HI_J = J * NM + o_hi;
                HI_0 = (J - 1) * NM + o_hi;
                HI_1 = (J - 1) * NM + o_hi;
                for (I = L; I <= J; I++)
                {
                    YR = HR[I + HR_0];
                    YI = 0.0E0;
                    ZZR = HR[I + HR_J];
                    ZZI = HI[I + HI_J];
                    if (I == J) goto LABEL560;
                    YI = HI[I + HI_0];
                    HI[I + HI_1] = XR * YI + XI * YR + HI[J+(J - 1) * NM + o_hi] * ZZI;
                LABEL560:  HR[I+(J - 1) * NM + o_hr] = XR * YR - XI * YI + HI[J+(J - 1) * NM + o_hi] * ZZR;
                    HR[I + HR_J] = XR * ZZR + XI * ZZI - HI[J+(J - 1) * NM + o_hi] * YR;
                    HI[I + HI_J] = XR * ZZI - XI * ZZR - HI[J+(J - 1) * NM + o_hi] * YI;
                }
                // c
            }
            // c
            if (SI == 0.0E0) goto LABEL240;
            // c
            HR_EN = EN * NM + o_hr;
            HI_EN = EN * NM + o_hi;
            for (I = L; I <= EN; I++)
            {
                YR = HR[I + HR_EN];
                YI = HI[I + HI_EN];
                HR[I + HR_EN] = SR * YR - SI * YI;
                HI[I + HI_EN] = SR * YI + SI * YR;
            }
            // c
            goto LABEL240;
            // c     .......... a root found ..........
        LABEL660:  WR[EN + o_wr] = HR[EN+EN * NM + o_hr] + TR;
            WI[EN + o_wi] = HI[EN+EN * NM + o_hi] + TI;
            EN = ENM1;
            goto LABEL220;
            // c     .......... set error -- all eigenvalues have not
            // c                converged after 30*n iterations ..........
        LABEL1000:  IERR = EN;
        LABEL1001:  return;

            #endregion

        }
    }
}
