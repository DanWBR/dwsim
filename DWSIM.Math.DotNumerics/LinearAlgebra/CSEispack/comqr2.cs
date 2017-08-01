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
    public class COMQR2
    {
    

        #region Dependencies
        
        PYTHAG _pythag; CSROOT _csroot; CDIV _cdiv; 

        #endregion

        public COMQR2(PYTHAG pythag, CSROOT csroot, CDIV cdiv)
        {
    

            #region Set Dependencies
            
            this._pythag = pythag; this._csroot = csroot; this._cdiv = cdiv; 

            #endregion

        }
    
        public COMQR2()
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
        /// <param name="ORTR">
        /// and orti contain information about the unitary trans-
        /// formations used in the reduction by  corth, if performed.
        /// only elements low through igh are used.  if the eigenvectors
        /// of the hessenberg matrix are desired, set ortr(j) and
        /// orti(j) to 0.0d0 for these elements.
        ///</param>
        /// <param name="HR">
        /// and hi contain the real and imaginary parts,
        /// respectively, of the complex upper hessenberg matrix.
        /// their lower triangles below the subdiagonal contain further
        /// information about the transformations which were used in the
        /// reduction by  corth, if performed.  if the eigenvectors of
        /// the hessenberg matrix are desired, these elements may be
        /// arbitrary.
        ///</param>
        /// <param name="WR">
        /// and wi contain the real and imaginary parts,
        /// respectively, of the eigenvalues.  if an error
        /// exit is made, the eigenvalues should be correct
        /// for indices ierr+1,...,n.
        ///</param>
        /// <param name="ZR">
        /// and zi contain the real and imaginary parts,
        /// respectively, of the eigenvectors.  the eigenvectors
        /// are unnormalized.  if an error exit is made, none of
        /// the eigenvectors has been found.
        ///</param>
        /// <param name="IERR">
        /// is set to
        /// zero       for normal return,
        /// j          if the limit of 30*n iterations is exhausted
        /// while the j-th eigenvalue is being sought.
        ///</param>
        public void Run(int NM, int N, int LOW, int IGH, ref double[] ORTR, int offset_ortr, ref double[] ORTI, int offset_orti
                         , ref double[] HR, int offset_hr, ref double[] HI, int offset_hi, ref double[] WR, int offset_wr, ref double[] WI, int offset_wi, ref double[] ZR, int offset_zr, ref double[] ZI, int offset_zi
                         , ref int IERR)
        {

            #region Variables
            
            int I = 0; int J = 0; int K = 0; int L = 0; int M = 0; int EN = 0; int II = 0; int JJ = 0; int LL = 0; int NN = 0; 
            int IP1 = 0;int ITN = 0; int ITS = 0; int LP1 = 0; int ENM1 = 0; int IEND = 0; double SI = 0; double SR = 0; 
            double TI = 0;double TR = 0; double XI = 0; double XR = 0; double YI = 0; double YR = 0; double ZZI = 0; 
            double ZZR = 0;double NORM = 0; double TST1 = 0; double TST2 = 0; 

            #endregion


            #region Implicit Variables
            
            int ZR_J = 0; int ZI_J = 0; int HR_0 = 0; int HI_0 = 0; int HI_I = 0; int HR_I = 0; int ZI_I = 0; int ZR_I = 0; 
            int HR_1 = 0;int HR_J = 0; int HI_J = 0; int HI_1 = 0; int HI_2 = 0; int ZR_0 = 0; int ZI_0 = 0; int ZR_1 = 0; 
            int ZI_1 = 0;int HR_EN = 0; int HI_EN = 0; int ZR_EN = 0; int ZI_EN = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ortr = -1 + offset_ortr;  int o_orti = -1 + offset_orti;  int o_hr = -1 - NM + offset_hr; 
             int o_hi = -1 - NM + offset_hi; int o_wr = -1 + offset_wr;  int o_wi = -1 + offset_wi; 
             int o_zr = -1 - NM + offset_zr; int o_zi = -1 - NM + offset_zi; 

            #endregion


            #region Prolog
            
            // C  MESHED overflow control WITH vectors of isolated roots (10/19/89 BSG)
            // C  MESHED overflow control WITH triangular multiply (10/30/89 BSG)
            // c
            // c
            // c     this subroutine is a translation of a unitary analogue of the
            // c     algol procedure  comlr2, num. math. 16, 181-204(1970) by peters
            // c     and wilkinson.
            // c     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
            // c     the unitary analogue substitutes the qr algorithm of francis
            // c     (comp. jour. 4, 332-345(1962)) for the lr algorithm.
            // c
            // c     this subroutine finds the eigenvalues and eigenvectors
            // c     of a complex upper hessenberg matrix by the qr
            // c     method.  the eigenvectors of a complex general matrix
            // c     can also be found if  corth  has been used to reduce
            // c     this general matrix to hessenberg form.
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
            // c        ortr and orti contain information about the unitary trans-
            // c          formations used in the reduction by  corth, if performed.
            // c          only elements low through igh are used.  if the eigenvectors
            // c          of the hessenberg matrix are desired, set ortr(j) and
            // c          orti(j) to 0.0d0 for these elements.
            // c
            // c        hr and hi contain the real and imaginary parts,
            // c          respectively, of the complex upper hessenberg matrix.
            // c          their lower triangles below the subdiagonal contain further
            // c          information about the transformations which were used in the
            // c          reduction by  corth, if performed.  if the eigenvectors of
            // c          the hessenberg matrix are desired, these elements may be
            // c          arbitrary.
            // c
            // c     on output
            // c
            // c        ortr, orti, and the upper hessenberg portions of hr and hi
            // c          have been destroyed.
            // c
            // c        wr and wi contain the real and imaginary parts,
            // c          respectively, of the eigenvalues.  if an error
            // c          exit is made, the eigenvalues should be correct
            // c          for indices ierr+1,...,n.
            // c
            // c        zr and zi contain the real and imaginary parts,
            // c          respectively, of the eigenvectors.  the eigenvectors
            // c          are unnormalized.  if an error exit is made, none of
            // c          the eigenvectors has been found.
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
            // c     this version dated october 1989.
            // c
            // c     ------------------------------------------------------------------
            // c

            #endregion


            #region Body
            
            IERR = 0;
            // c     .......... initialize eigenvector matrix ..........
            for (J = 1; J <= N; J++)
            {
                // c
                ZR_J = J * NM + o_zr;
                ZI_J = J * NM + o_zi;
                for (I = 1; I <= N; I++)
                {
                    ZR[I + ZR_J] = 0.0E0;
                    ZI[I + ZI_J] = 0.0E0;
                }
                ZR[J+J * NM + o_zr] = 1.0E0;
            }
            // c     .......... form the matrix of accumulated transformations
            // c                from the information left by corth ..........
            IEND = IGH - LOW - 1;
            if (IEND < 0) goto LABEL180;
            else
            {
                if (IEND > 0) goto LABEL105;
                else goto LABEL150;
            }
            // c     .......... for i=igh-1 step -1 until low+1 do -- ..........
        LABEL105:  
            for (II = 1; II <= IEND; II++)
            {
                I = IGH - II;
                if (ORTR[I + o_ortr] == 0.0E0 && ORTI[I + o_orti] == 0.0E0) goto LABEL140;
                if (HR[I+(I - 1) * NM + o_hr] == 0.0E0 && HI[I+(I - 1) * NM + o_hi] == 0.0E0) goto LABEL140;
                // c     .......... norm below is negative of h formed in corth ..........
                NORM = HR[I+(I - 1) * NM + o_hr] * ORTR[I + o_ortr] + HI[I+(I - 1) * NM + o_hi] * ORTI[I + o_orti];
                IP1 = I + 1;
                // c
                HR_0 = (I - 1) * NM + o_hr;
                HI_0 = (I - 1) * NM + o_hi;
                for (K = IP1; K <= IGH; K++)
                {
                    ORTR[K + o_ortr] = HR[K + HR_0];
                    ORTI[K + o_orti] = HI[K + HI_0];
                }
                // c
                for (J = I; J <= IGH; J++)
                {
                    SR = 0.0E0;
                    SI = 0.0E0;
                    // c
                    ZR_J = J * NM + o_zr;
                    ZI_J = J * NM + o_zi;
                    for (K = I; K <= IGH; K++)
                    {
                        SR += ORTR[K + o_ortr] * ZR[K + ZR_J] + ORTI[K + o_orti] * ZI[K + ZI_J];
                        SI += ORTR[K + o_ortr] * ZI[K + ZI_J] - ORTI[K + o_orti] * ZR[K + ZR_J];
                    }
                    // c
                    SR /= NORM;
                    SI /= NORM;
                    // c
                    ZR_J = J * NM + o_zr;
                    ZI_J = J * NM + o_zi;
                    for (K = I; K <= IGH; K++)
                    {
                        ZR[K + ZR_J] += SR * ORTR[K + o_ortr] - SI * ORTI[K + o_orti];
                        ZI[K + ZI_J] += SR * ORTI[K + o_orti] + SI * ORTR[K + o_ortr];
                    }
                    // c
                }
                // c
            LABEL140:;
            }
            // c     .......... create real subdiagonal elements ..........
        LABEL150:  L = LOW + 1;
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
                for (J = I; J <= N; J++)
                {
                    SI = YR * HI[I+J * NM + o_hi] - YI * HR[I+J * NM + o_hr];
                    HR[I+J * NM + o_hr] = YR * HR[I+J * NM + o_hr] + YI * HI[I+J * NM + o_hi];
                    HI[I+J * NM + o_hi] = SI;
                }
                // c
                HI_I = I * NM + o_hi;
                HR_I = I * NM + o_hr;
                for (J = 1; J <= LL; J++)
                {
                    SI = YR * HI[J + HI_I] + YI * HR[J + HR_I];
                    HR[J + HR_I] = YR * HR[J + HR_I] - YI * HI[J + HI_I];
                    HI[J + HI_I] = SI;
                }
                // c
                ZI_I = I * NM + o_zi;
                ZR_I = I * NM + o_zr;
                for (J = LOW; J <= IGH; J++)
                {
                    SI = YR * ZI[J + ZI_I] + YI * ZR[J + ZR_I];
                    ZR[J + ZR_I] = YR * ZR[J + ZR_I] - YI * ZI[J + ZI_I];
                    ZI[J + ZI_I] = SI;
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
            if (EN < LOW) goto LABEL680;
            ITS = 0;
            ENM1 = EN - 1;
            // c     .......... look for single small sub-diagonal element
            // c                for l=en step -1 until low do -- ..........
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
                for (J = I; J <= N; J++)
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
            if (EN == N) goto LABEL540;
            IP1 = EN + 1;
            // c
            for (J = IP1; J <= N; J++)
            {
                YR = HR[EN+J * NM + o_hr];
                YI = HI[EN+J * NM + o_hi];
                HR[EN+J * NM + o_hr] = SR * YR + SI * YI;
                HI[EN+J * NM + o_hi] = SR * YI - SI * YR;
            }
            // c     .......... inverse operation (columns) ..........
        LABEL540:  
            for (J = LP1; J <= EN; J++)
            {
                XR = WR[J - 1 + o_wr];
                XI = WI[J - 1 + o_wi];
                // c
                HR_1 = (J - 1) * NM + o_hr;
                HR_J = J * NM + o_hr;
                HI_J = J * NM + o_hi;
                HI_1 = (J - 1) * NM + o_hi;
                HI_2 = (J - 1) * NM + o_hi;
                for (I = 1; I <= J; I++)
                {
                    YR = HR[I + HR_1];
                    YI = 0.0E0;
                    ZZR = HR[I + HR_J];
                    ZZI = HI[I + HI_J];
                    if (I == J) goto LABEL560;
                    YI = HI[I + HI_1];
                    HI[I + HI_2] = XR * YI + XI * YR + HI[J+(J - 1) * NM + o_hi] * ZZI;
                LABEL560:  HR[I+(J - 1) * NM + o_hr] = XR * YR - XI * YI + HI[J+(J - 1) * NM + o_hi] * ZZR;
                    HR[I + HR_J] = XR * ZZR + XI * ZZI - HI[J+(J - 1) * NM + o_hi] * YR;
                    HI[I + HI_J] = XR * ZZI - XI * ZZR - HI[J+(J - 1) * NM + o_hi] * YI;
                }
                // c
                ZR_0 = (J - 1) * NM + o_zr;
                ZI_0 = (J - 1) * NM + o_zi;
                ZR_J = J * NM + o_zr;
                ZI_J = J * NM + o_zi;
                ZR_1 = (J - 1) * NM + o_zr;
                ZI_1 = (J - 1) * NM + o_zi;
                for (I = LOW; I <= IGH; I++)
                {
                    YR = ZR[I + ZR_0];
                    YI = ZI[I + ZI_0];
                    ZZR = ZR[I + ZR_J];
                    ZZI = ZI[I + ZI_J];
                    ZR[I + ZR_1] = XR * YR - XI * YI + HI[J+(J - 1) * NM + o_hi] * ZZR;
                    ZI[I + ZI_1] = XR * YI + XI * YR + HI[J+(J - 1) * NM + o_hi] * ZZI;
                    ZR[I + ZR_J] = XR * ZZR + XI * ZZI - HI[J+(J - 1) * NM + o_hi] * YR;
                    ZI[I + ZI_J] = XR * ZZI - XI * ZZR - HI[J+(J - 1) * NM + o_hi] * YI;
                }
                // c
            }
            // c
            if (SI == 0.0E0) goto LABEL240;
            // c
            HR_EN = EN * NM + o_hr;
            HI_EN = EN * NM + o_hi;
            for (I = 1; I <= EN; I++)
            {
                YR = HR[I + HR_EN];
                YI = HI[I + HI_EN];
                HR[I + HR_EN] = SR * YR - SI * YI;
                HI[I + HI_EN] = SR * YI + SI * YR;
            }
            // c
            ZR_EN = EN * NM + o_zr;
            ZI_EN = EN * NM + o_zi;
            for (I = LOW; I <= IGH; I++)
            {
                YR = ZR[I + ZR_EN];
                YI = ZI[I + ZI_EN];
                ZR[I + ZR_EN] = SR * YR - SI * YI;
                ZI[I + ZI_EN] = SR * YI + SI * YR;
            }
            // c
            goto LABEL240;
            // c     .......... a root found ..........
        LABEL660:  HR[EN+EN * NM + o_hr] += TR;
            WR[EN + o_wr] = HR[EN+EN * NM + o_hr];
            HI[EN+EN * NM + o_hi] += TI;
            WI[EN + o_wi] = HI[EN+EN * NM + o_hi];
            EN = ENM1;
            goto LABEL220;
            // c     .......... all roots found.  backsubstitute to find
            // c                vectors of upper triangular form ..........
        LABEL680:  NORM = 0.0E0;
            // c
            for (I = 1; I <= N; I++)
            {
                // c
                for (J = I; J <= N; J++)
                {
                    TR = Math.Abs(HR[I+J * NM + o_hr]) + Math.Abs(HI[I+J * NM + o_hi]);
                    if (TR > NORM) NORM = TR;
                }
            }
            // c
            if (N == 1 || NORM == 0.0E0) goto LABEL1001;
            // c     .......... for en=n step -1 until 2 do -- ..........
            for (NN = 2; NN <= N; NN++)
            {
                EN = N + 2 - NN;
                XR = WR[EN + o_wr];
                XI = WI[EN + o_wi];
                HR[EN+EN * NM + o_hr] = 1.0E0;
                HI[EN+EN * NM + o_hi] = 0.0E0;
                ENM1 = EN - 1;
                // c     .......... for i=en-1 step -1 until 1 do -- ..........
                for (II = 1; II <= ENM1; II++)
                {
                    I = EN - II;
                    ZZR = 0.0E0;
                    ZZI = 0.0E0;
                    IP1 = I + 1;
                    // c
                    HR_EN = EN * NM + o_hr;
                    HI_EN = EN * NM + o_hi;
                    for (J = IP1; J <= EN; J++)
                    {
                        ZZR += HR[I+J * NM + o_hr] * HR[J + HR_EN] - HI[I+J * NM + o_hi] * HI[J + HI_EN];
                        ZZI += HR[I+J * NM + o_hr] * HI[J + HI_EN] + HI[I+J * NM + o_hi] * HR[J + HR_EN];
                    }
                    // c
                    YR = XR - WR[I + o_wr];
                    YI = XI - WI[I + o_wi];
                    if (YR != 0.0E0 || YI != 0.0E0) goto LABEL765;
                    TST1 = NORM;
                    YR = TST1;
                LABEL760:  YR *= 0.01E0;
                    TST2 = NORM + YR;
                    if (TST2 > TST1) goto LABEL760;
                LABEL765:;
                    this._cdiv.Run(ZZR, ZZI, YR, YI, ref HR[I+EN * NM + o_hr], ref HI[I+EN * NM + o_hi]);
                    // c     .......... overflow control ..........
                    TR = Math.Abs(HR[I+EN * NM + o_hr]) + Math.Abs(HI[I+EN * NM + o_hi]);
                    if (TR == 0.0E0) goto LABEL780;
                    TST1 = TR;
                    TST2 = TST1 + 1.0E0 / TST1;
                    if (TST2 > TST1) goto LABEL780;
                    HR_EN = EN * NM + o_hr;
                    HI_EN = EN * NM + o_hi;
                    for (J = I; J <= EN; J++)
                    {
                        HR[J + HR_EN] /= TR;
                        HI[J + HI_EN] /= TR;
                    }
                    // c
                LABEL780:;
                }
                // c
            }
            // c     .......... end backsubstitution ..........
            // c     .......... vectors of isolated roots ..........
            for (I = 1; I <= N; I++)
            {
                if (I >= LOW && I <= IGH) goto LABEL840;
                // c
                for (J = I; J <= N; J++)
                {
                    ZR[I+J * NM + o_zr] = HR[I+J * NM + o_hr];
                    ZI[I+J * NM + o_zi] = HI[I+J * NM + o_hi];
                }
                // c
            LABEL840:;
            }
            // c     .......... multiply by transformation matrix to give
            // c                vectors of original full matrix.
            // c                for j=n step -1 until low do -- ..........
            for (JJ = LOW; JJ <= N; JJ++)
            {
                J = N + LOW - JJ;
                M = Math.Min(J, IGH);
                // c
                ZR_J = J * NM + o_zr;
                ZI_J = J * NM + o_zi;
                for (I = LOW; I <= IGH; I++)
                {
                    ZZR = 0.0E0;
                    ZZI = 0.0E0;
                    // c
                    HR_J = J * NM + o_hr;
                    HI_J = J * NM + o_hi;
                    for (K = LOW; K <= M; K++)
                    {
                        ZZR += ZR[I+K * NM + o_zr] * HR[K + HR_J] - ZI[I+K * NM + o_zi] * HI[K + HI_J];
                        ZZI += ZR[I+K * NM + o_zr] * HI[K + HI_J] + ZI[I+K * NM + o_zi] * HR[K + HR_J];
                    }
                    // c
                    ZR[I + ZR_J] = ZZR;
                    ZI[I + ZI_J] = ZZI;
                }
            }
            // c
            goto LABEL1001;
            // c     .......... set error -- all eigenvalues have not
            // c                converged after 30*n iterations ..........
        LABEL1000:  IERR = EN;
        LABEL1001:  return;

            #endregion

        }
    }
}
