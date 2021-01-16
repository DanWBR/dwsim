﻿'    Critical Point Calculation Routines (PR & SRK) 
'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DWSIM.MathOps.MathEx.Common
Imports DWSIM.MathOps.MathEx

Namespace Utilities.TCP

    <System.Serializable()> Public Class Methods

        Sub New()

        End Sub

        Function CRITPT_PR(ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj, Optional ByVal Vinf = 0) As ArrayList

            Dim res As New ArrayList

            Dim V, Vc_sup, Vc_inf As Double

            Dim stmp(2)
            Dim n, R As Double
            Dim i As Integer

            n = Vz.Length - 1

            Dim Tc(n), Pc(n)
            Dim b As Double

            'estimar temperatura e pressao criticas iniciais

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Pc(i) = VPc(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            b = 0
            Do
                b += Vz(i) * 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            'estimar temperatura e pressao criticas iniciais

            Vc_inf = 4 * b
            If Vinf <> 0 Then Vc_inf = Vinf
            Vc_sup = b

            Dim fV, fV_inf, nsub, delta_Vc As Double

            nsub = 25

            delta_Vc = (Vc_sup - Vc_inf) / nsub

            Do
restart:        fV = TRIPLESUM(Vc_inf, Vz, VTc, VPc, VVc, Vw, VKIj)
                Vc_inf = Vc_inf + delta_Vc
                fV_inf = TRIPLESUM(Vc_inf, Vz, VTc, VPc, VVc, Vw, VKIj)
            Loop Until fV * fV_inf < 0 Or Vc_inf <= b
            Vc_sup = Vc_inf - delta_Vc
            'Vc_inf = Vc_inf - delta_Vc

            If Vc_inf <= b Then GoTo Final2

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Vc_inf
            bbb = Vc_sup
            ccc = Vc_sup

            faa = TRIPLESUM(Vc_inf, Vz, VTc, VPc, VVc, Vw, VKIj)
            fbb = TRIPLESUM(Vc_sup, Vz, VTc, VPc, VVc, Vw, VKIj)
            fcc = fbb

            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 1.0E-17
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If Math.Abs(fbb) < tol11 Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                fbb = TRIPLESUM(bbb, Vz, VTc, VPc, VVc, Vw, VKIj)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:

            V = bbb

            Dim T, P

            T = TCRIT2(V, Vz, VTc, VPc, VVc, Vw, VKIj)

            P = PCRIT(T, V, Vz, VTc, VPc, Vw, VKIj)
            'P = 0.307 * 8.314 * T / V

            If P < 0 Then
                Vc_inf += 2 * delta_Vc
                GoTo restart
            End If

            stmp(0) = T
            stmp(1) = P
            stmp(2) = V

            If Math.Abs(fbb) < 0.01 And fbb <> 0.0# Then
                If res.Count > 0 Then
                    If Math.Abs(T - res(0)(0)) > 5 And P < 100000000 Then res.Add(stmp.Clone)
                Else
                    res.Add(stmp.Clone)
                End If
            End If

            If Vc_inf <= b Then
                GoTo Final2
            Else
                Vc_inf += 2 * delta_Vc
                GoTo restart
            End If

Final2:

            CRITPT_PR = res

        End Function

        Function QIJ_HES_MAT(ByVal T, ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj) As Mapack.Matrix

            Dim i, j As Integer
            Dim n As Double

            Dim am, bm, R As Double

            n = Vz.Length - 1

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            'estimar temperatura e pressao criticas iniciais

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Tr(i) = T / Tc(i)
                    Pc(i) = VPc(i)
                    w(i) = Vw(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0 Then
                    alpha(i) = (1 + (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                    ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                    b(i) = 0.0778 * R * Tc(i) / Pc(i)
                    c(i) = (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKIj(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                am2(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            am = 0
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    am2(i) = am2(i) + Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            bm = 0
            Do
                bm = bm + Vz(i) * b(i)
                i = i + 1
            Loop Until i = n + 1

            Dim F1, F2, F3, F4, F5, F6, F7, F8 As Double
            Dim K, delta1, delta2 As Double

            K = V / bm
            delta1 = 2.414
            delta2 = -0.414

            F1 = 1 / (K - 1)
            F2 = 2 / (delta1 - delta2) * (delta1 / (K + delta1) - delta2 / (K + delta2))
            F3 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 2 - (delta2 / (K + delta2)) ^ 2)
            F4 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 3 - (delta2 / (K + delta2)) ^ 3)
            F5 = 2 / (delta1 - delta2) * Math.Log((K + delta1) / (K + delta2))
            F6 = F2 - F5
            F7 = -F2 / (1 + F1)
            F8 = F3 / (1 + F1)

            Dim alfa As Double

            alfa = am / (bm * R * T)

            Dim dnb_dn(n), dnalfa_dn(n) As Double
            Dim d2nb_dn(n, n), d2nalfa_dn(n, n) As Double
            Dim d3nalfa_dn(n, n, n) As Double

            Dim sum_xa(n) As Double

            Dim dta(n, n) As Integer

            i = 0
            Do
                j = 0
                Do
                    If i = j Then
                        dta(i, j) = 1
                    Else
                        dta(i, j) = 0
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                sum_xa(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    sum_xa(i) += Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim alfa_(n), beta_(n) As Double

            i = 0
            Do
                beta_(i) = b(i) / bm
                alfa_(i) = sum_xa(i) / am
                i = i + 1
            Loop Until i = n + 1

            Dim Q As Mapack.Matrix = New Mapack.Matrix(n + 1, n + 1)

            i = 0
            Do
                j = 0
                Do
                    Q(i, j) = R * T * (dta(i, j) / Vz(i) + (beta_(i) + beta_(j)) * F1 + beta_(i) * beta_(j) * F1 ^ 2) + am / bm * (beta_(i) * beta_(j) * F3 - a(i, j) * F5 / am + (beta_(i) * beta_(j) - alfa_(i) * beta_(j) - alfa_(j) * beta_(i)) * F6)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            'get maximum value
            Dim max As Double = 0
            i = 0
            Do
                j = 0
                Do
                    If Math.Abs(Q(i, j)) > max Then max = Math.Abs(Q(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            'i = 0
            'Do
            '    j = 0
            '    Do
            '        Q(i, j) = Q(i, j) / max
            '        j = j + 1
            '    Loop Until j = n + 1
            '    i = i + 1
            'Loop Until i = n + 1

            QIJ_HES_MAT = Q

        End Function

        Function TRIPLESUM(ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj) As Double

            Dim T, Tc_sup, Tc_inf As Double

            Dim i, j As Integer
            Dim n As Double

            Dim am, bm, R As Double

            n = Vz.Length - 1

            Dim Dn(n)

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            Tc_inf = MathEx.Common.Min(VTc) * 0.5
            Tc_sup = MathEx.Common.Max(VTc) * 1.5

            Dim fT, fT_inf, nsub, delta_Tc As Double

            nsub = 10

            delta_Tc = (Tc_sup - Tc_inf) / nsub

            Do
                fT = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                Tc_inf = Tc_inf + delta_Tc
                fT_inf = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            Loop Until fT * fT_inf < 0 Or Tc_inf > Tc_sup
            Tc_sup = Tc_inf
            Tc_inf = Tc_inf - delta_Tc

            'metodo de Brent para encontrar Tc

            Dim aa, bb, cc, dd, ee, min1, min2, fa, fb, fc, pp, qq, rr, ss, tol1, xm As Double
            Dim ITMAX As Integer = 1000
            Dim iter As Integer

            aa = Tc_inf
            bb = Tc_sup
            cc = Tc_sup

            fa = QIJ_HES_MAT(aa, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fc = fb

            iter = 0
            Do
                If (fb > 0 And fc > 0) Or (fb < 0 And fc < 0) Then
                    cc = aa
                    fc = fa
                    dd = bb - aa
                    ee = dd
                End If
                If Math.Abs(fc) < Math.Abs(fb) Then
                    aa = bb
                    bb = cc
                    cc = aa
                    fa = fb
                    fb = fc
                    fc = fa
                End If
                tol1 = 0.00000001
                xm = 0.5 * (cc - bb)
                If (Math.Abs(xm) <= tol1) Or (fb = 0) Then GoTo Final
                If Math.Abs(fb) < tol1 Then GoTo Final
                If (Math.Abs(ee) >= tol1) And (Math.Abs(fa) > Math.Abs(fb)) Then
                    ss = fb / fa
                    If aa = cc Then
                        pp = 2 * xm * ss
                        qq = 1 - ss
                    Else
                        qq = fa / fc
                        rr = fb / fc
                        pp = ss * (2 * xm * qq * (qq - rr) - (bb - aa) * (rr - 1))
                        qq = (qq - 1) * (rr - 1) * (ss - 1)
                    End If
                    If pp > 0 Then qq = -qq
                    pp = Math.Abs(pp)
                    min1 = 3 * xm * qq - Math.Abs(tol1 * qq)
                    min2 = Math.Abs(ee * qq)
                    Dim tvar As Double
                    If min1 < min2 Then tvar = min1
                    If min1 > min2 Then tvar = min2
                    If 2 * pp < tvar Then
                        ee = dd
                        dd = pp / qq
                    Else
                        dd = xm
                        ee = dd
                    End If
                Else
                    dd = xm
                    ee = dd
                End If
                aa = bb
                fa = fb
                If (Math.Abs(dd) > tol1) Then
                    bb += dd
                Else
                    bb += Math.Sign(xm) * tol1
                End If
                fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                iter += 1
            Loop Until iter = ITMAX
Final:
            T = bb
            If iter = ITMAX Then GoTo Final2

            Dim MA As Mapack.Matrix, Dn0(n) As Double
            Dim MA_(n, n), MB_(n), Dn0_(n) As Double

            'Dim MP As New DLLXnumbers.Xnumbers
            MA = QIJ_HES_MAT(T, V, Vz, VTc, VPc, VVc, Vw, VKIj)

            Dim m2 As Mapack.Matrix = New Mapack.Matrix(MA.Rows, 1)

            For i = 0 To n
                For j = 0 To n
                    MA_(i, j) = MA(i, j)
                Next
                MB_(i) = Double.Epsilon
            Next

            'Dim solved As Boolean = False
            'solved = MathEx.SysLin.rsolve.rmatrixsolve(MA_, MB_, n, Dn0)

            Try
                Dim trg As Mapack.ILuDecomposition = MA.GetLuDecomposition
                'Console.WriteLine(trg.UpperTriangularFactor.ToString)
                i = 0
                Do
                    m2(i, 0) = 0
                    i = i + 1
                Loop Until i = n + 1
                m2(n, 0) = trg.UpperTriangularFactor(n, n)
                Dim m3 As Mapack.Matrix = trg.UpperTriangularFactor.Solve(m2)
                i = 0
                Do
                    Dn0(i) = m3(i, 0)
                    i = i + 1
                Loop Until i = n + 1
            Catch ex As Exception
                i = 0
                Do
                    Dn0(i) = 0
                    i = i + 1
                Loop Until i = n + 1
            End Try

            Dim soma_Dn = 0.0#
            i = 0
            Do
                soma_Dn += Math.Abs(Dn0(i))
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                Dn(i) = Dn0(i) / soma_Dn
                i = i + 1
            Loop Until i = n + 1

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Tr(i) = T / Tc(i)
                    Pc(i) = VPc(i)
                    w(i) = Vw(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0 Then
                    alpha(i) = (1 + (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                    ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                    b(i) = 0.0778 * R * Tc(i) / Pc(i)
                    c(i) = (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKIj(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                am2(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            am = 0
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    am2(i) = am2(i) + Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            bm = 0
            Do
                bm = bm + Vz(i) * b(i)
                i = i + 1
            Loop Until i = n + 1

            Dim F1, F2, F3, F4, F5, F6, F7, F8 As Double
            Dim K, delta1, delta2 As Double

            K = V / bm
            delta1 = 2.414
            delta2 = -0.414

            F1 = 1 / (K - 1)
            F2 = 2 / (delta1 - delta2) * (delta1 / (K + delta1) - delta2 / (K + delta2))
            F3 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 2 - (delta2 / (K + delta2)) ^ 2)
            F4 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 3 - (delta2 / (K + delta2)) ^ 3)
            F5 = 2 / (delta1 - delta2) * Math.Log((K + delta1) / (K + delta2))
            F6 = F2 - F5
            F7 = -F2 / (1 + F1)
            F8 = F3 / (1 + F1)

            Dim sum_xa(n) As Double

            i = 0
            Do
                sum_xa(i) = 0
                j = 0
                Do
                    sum_xa(i) += Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim alfa_(n), beta_(n) As Double

            i = 0
            Do
                beta_(i) = b(i) / bm
                alfa_(i) = sum_xa(i) / am
                i = i + 1
            Loop Until i = n + 1

            Dim a_ As Double = 0
            Dim b_ As Double = 0
            Dim af_ As Double = 0

            i = 0
            Do
                j = 0
                Do
                    a_ += a(i, j) * Dn(i) * Dn(j) / am
                    j = j + 1
                Loop Until j = n + 1
                b_ += beta_(i) * Dn(i)
                af_ += alfa_(i) * Dn(i)
                i = i + 1
            Loop Until i = n + 1

            Dim n_, sum_Dn3 As Double

            n_ = 0
            sum_Dn3 = 0

            i = 0
            Do
                n_ = n_ + Dn(i)
                sum_Dn3 = sum_Dn3 + Dn(i) ^ 3 / Vz(i) ^ 2
                i = i + 1
            Loop Until i = n + 1

            Dim TS As Double

            TS = R * T * (-sum_Dn3 + 3 * n_ * (b_ * F1) ^ 2 + 2 * (b_ * F1) ^ 3) + am / bm * (3 * b_ ^ 2 * (2 * af_ - b_) * (F3 + F6) - 2 * b_ ^ 3 * F4 - 3 * b_ * a_ * F6)

Final2:

            TRIPLESUM = TS

        End Function

        Function TCRIT(ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj)

            'Dim MP As New DLLXnumbers.Xnumbers
            'MP.DigitsMax = 20

            Dim T, Tc_sup, Tc_inf As Double

            Dim n As Double

            n = Vz.Length - 1

            Dim Dn(n)

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            Tc_inf = MathEx.Common.Min(VTc) * 0.5
            Tc_sup = MathEx.Common.Min(VTc) * 1.5

            Dim fT, fT_inf, nsub, delta_Tc As Double

            nsub = 50

            delta_Tc = (Tc_sup - Tc_inf) / nsub

            Do
                fT = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                Tc_inf = Tc_inf + delta_Tc
                fT_inf = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            Loop Until fT * fT_inf < 0 Or Tc_inf > Tc_sup
            Tc_inf = Tc_inf - delta_Tc
            Tc_sup = Tc_inf

            'metodo de Brent para encontrar Tc

            Dim aa, bb, cc, dd, ee, min1, min2, fa, fb, fc, pp, qq, rr, ss, tol1, xm As Double
            Dim ITMAX As Integer = 100
            Dim iter As Integer

            aa = Tc_inf
            bb = Tc_sup
            cc = Tc_sup

            fa = QIJ_HES_MAT(aa, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fc = fb

            iter = 0
            Do
                If (fb > 0 And fc > 0) Or (fb < 0 And fc < 0) Then
                    cc = aa
                    fc = fa
                    dd = bb - aa
                    ee = dd
                End If
                If Math.Abs(fc) < Math.Abs(fb) Then
                    aa = bb
                    bb = cc
                    cc = aa
                    fa = fb
                    fb = fc
                    fc = fa
                End If
                tol1 = 0.000000000001
                xm = 0.5 * (cc - bb)
                If (Math.Abs(xm) <= tol1) Or (fb = 0) Then GoTo Final
                If (Math.Abs(ee) >= tol1) And (Math.Abs(fa) > Math.Abs(fb)) Then
                    ss = fb / fa
                    If aa = cc Then
                        pp = 2 * xm * ss
                        qq = 1 - ss
                    Else
                        qq = fa / fc
                        rr = fb / fc
                        pp = ss * (2 * xm * qq * (qq - rr) - (bb - aa) * (rr - 1))
                        qq = (qq - 1) * (rr - 1) * (ss - 1)
                    End If
                    If pp > 0 Then qq = -qq
                    pp = Math.Abs(pp)
                    min1 = 3 * xm * qq - Math.Abs(tol1 * qq)
                    min2 = Math.Abs(ee * qq)
                    Dim tvar As Double
                    If min1 < min2 Then tvar = min1
                    If min1 > min2 Then tvar = min2
                    If 2 * pp < tvar Then
                        ee = dd
                        dd = pp / qq
                    Else
                        dd = xm
                        ee = dd
                    End If
                Else
                    dd = xm
                    ee = dd
                End If
                aa = bb
                fa = fb
                If (Math.Abs(dd) > tol1) Then
                    bb += dd
                Else
                    bb += Math.Sign(xm) * tol1
                End If
                fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                iter += 1
            Loop Until iter = ITMAX
Final:
            T = bb
            If iter = ITMAX Then GoTo Final2

Final2:

            TCRIT = T

        End Function

        Function PCRIT(ByVal T, ByVal V, ByVal Vx, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VKIj)

            Dim ai(), bi() As Double
            Dim n, R, P, coeff(3), tmp() As Double
            Dim Tc(), Pc(), W(), alpha(), Vant(0, 4), m(), a(,), b(,), Tr() As Double

            n = Vx.Length - 1

            ReDim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n)
            ReDim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n)

            R = 8.314

            Dim i, j As Integer
            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                W(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                alpha(i) = (1 + (0.37464 + 1.54226 * W(i) - 0.26992 * W(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKIj(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim aml = 0.0#
            Do
                j = 0
                Do
                    aml = aml + Vx(i) * Vx(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bml = 0.0#
            Do
                bml = bml + Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            P = R * T / (V - bml) - aml / (V ^ 2 + 2 * bml * V - bml ^ 2)

            PCRIT = P

        End Function

        Function STABILITY_CURVE(ByVal Vz As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal VVc As Object, ByVal Vw As Object, ByVal VKIj As Object, Optional ByVal Vmax As Double = 0, Optional ByVal delta As Double = 40, Optional ByVal multipl As Integer = 15) As ArrayList

            'Dim MP As New DLLXnumbers.Xnumbers

            Dim V, Vmin, deltaV As Double

            Dim stmp(2)
            Dim n, R, P, T As Double
            Dim i As Integer

            n = Vz.Length - 1

            Dim Tc(n), Pc(n)
            Dim b As Double

            'estimar temperatura e pressao criticas iniciais

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Pc(i) = VPc(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            b = 0
            Do
                b += Vz(i) * 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            'estimar temperatura e pressao criticas iniciais

            If Vmax = 0 Then Vmax = b * multipl
            Vmin = b * 1.05

            deltaV = (Vmax - Vmin) / 100 ' delta

            Dim result As ArrayList = New ArrayList()

            V = Vmax
            Do
                T = TCRIT2(V, Vz, VTc, VPc, VVc, Vw, VKIj)
                'P = 0.307 * 8.314 * T / V
                P = PCRIT(T, V, Vz, VTc, VPc, Vw, VKIj)
                If P < 0 Then Exit Do
                result.Add(New Object() {T, P})
                V -= deltaV
            Loop Until V <= Vmin

            STABILITY_CURVE = result

        End Function

        Function TCRIT2(ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj)

            'Dim MP As New DLLXnumbers.Xnumbers
            'MP.DigitsMax = 20

            Dim T, Tc_sup, Tc_inf As Double

            Dim n As Double

            n = Vz.Length - 1

            Dim Dn(n)

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            Tc_inf = Min(VTc) * 0.5
            Tc_sup = Max(VTc) * 1.5

            Dim fT, fT_inf, nsub, delta_Tc As Double

            nsub = 50

            delta_Tc = (Tc_sup - Tc_inf) / nsub

            Do
                fT = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                Tc_inf = Tc_inf + delta_Tc
                fT_inf = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            Loop Until fT * fT_inf < 0 Or Tc_inf > Tc_sup
            Tc_sup = Tc_inf
            Tc_inf = Tc_inf - delta_Tc

            'metodo de Brent para encontrar Tc

            Dim aa, bb, cc, dd, ee, min1, min2, fa, fb, fc, pp, qq, rr, ss, tol1, xm As Double
            Dim ITMAX As Integer = 100
            Dim iter As Integer

            aa = Tc_inf
            bb = Tc_sup
            cc = Tc_sup

            fa = QIJ_HES_MAT(aa, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fc = fb

            iter = 0
            Do
                If (fb > 0 And fc > 0) Or (fb < 0 And fc < 0) Then
                    cc = aa
                    fc = fa
                    dd = bb - aa
                    ee = dd
                End If
                If Math.Abs(fc) < Math.Abs(fb) Then
                    aa = bb
                    bb = cc
                    cc = aa
                    fa = fb
                    fb = fc
                    fc = fa
                End If
                tol1 = 1.0E-100
                xm = 0.5 * (cc - bb)
                If (Math.Abs(xm) <= tol1) Or (fb = 0) Then GoTo Final
                If Math.Abs(fb) < tol1 Then GoTo Final
                If (Math.Abs(ee) >= tol1) And (Math.Abs(fa) > Math.Abs(fb)) Then
                    ss = fb / fa
                    If aa = cc Then
                        pp = 2 * xm * ss
                        qq = 1 - ss
                    Else
                        qq = fa / fc
                        rr = fb / fc
                        pp = ss * (2 * xm * qq * (qq - rr) - (bb - aa) * (rr - 1))
                        qq = (qq - 1) * (rr - 1) * (ss - 1)
                    End If
                    If pp > 0 Then qq = -qq
                    pp = Math.Abs(pp)
                    min1 = 3 * xm * qq - Math.Abs(tol1 * qq)
                    min2 = Math.Abs(ee * qq)
                    Dim tvar As Double
                    If min1 < min2 Then tvar = min1
                    If min1 > min2 Then tvar = min2
                    If 2 * pp < tvar Then
                        ee = dd
                        dd = pp / qq
                    Else
                        dd = xm
                        ee = dd
                    End If
                Else
                    dd = xm
                    ee = dd
                End If
                aa = bb
                fa = fb
                If (Math.Abs(dd) > tol1) Then
                    bb += dd
                Else
                    bb += Math.Sign(xm) * tol1
                End If
                fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                iter += 1
            Loop Until iter = ITMAX
Final:
            T = bb
            If iter = ITMAX Then GoTo Final2

Final2:

            TCRIT2 = T

        End Function

    End Class

    <System.Serializable()> Public Class Methods_SRK

        Sub New()

        End Sub

        Function CRITPT_PR(ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj, Optional ByVal Vinf = 0)

            Dim res As New ArrayList

            Dim V, Vc_sup, Vc_inf As Double

            Dim stmp(2)
            Dim n, R As Double
            Dim i As Integer

            n = Vz.Length - 1

            Dim Tc(n), Pc(n)
            Dim b As Double

            'estimar temperatura e pressao criticas iniciais

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Pc(i) = VPc(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            b = 0
            Do
                b += Vz(i) * 0.08664 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            'estimar temperatura e pressao criticas iniciais

            Vc_inf = 4 * b
            If Vinf <> 0 Then Vc_inf = Vinf
            Vc_sup = b

            Dim fV, fV_inf, nsub, delta_Vc As Double

            nsub = 25

            delta_Vc = (Vc_sup - Vc_inf) / nsub

            Do
restart:        fV = TRIPLESUM(Vc_inf, Vz, VTc, VPc, VVc, Vw, VKIj)
                Vc_inf = Vc_inf + delta_Vc
                fV_inf = TRIPLESUM(Vc_inf, Vz, VTc, VPc, VVc, Vw, VKIj)
            Loop Until fV * fV_inf < 0 Or Vc_inf <= b
            Vc_sup = Vc_inf - delta_Vc
            'Vc_inf = Vc_inf - delta_Vc

            If Vc_inf <= b Then GoTo Final2

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Vc_inf
            bbb = Vc_sup
            ccc = Vc_sup

            faa = TRIPLESUM(Vc_inf, Vz, VTc, VPc, VVc, Vw, VKIj)
            fbb = TRIPLESUM(Vc_sup, Vz, VTc, VPc, VVc, Vw, VKIj)
            fcc = fbb

            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 1.0E-17
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If Math.Abs(fbb) < tol11 Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                fbb = TRIPLESUM(bbb, Vz, VTc, VPc, VVc, Vw, VKIj)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:

            V = bbb

            Dim T, P

            T = TCRIT2(V, Vz, VTc, VPc, VVc, Vw, VKIj)

            P = PCRIT(T, V, Vz, VTc, VPc, Vw, VKIj)
            'P = 0.307 * 8.314 * T / V


            If P < 0 Then
                Vc_inf += 2 * delta_Vc
                GoTo restart
            End If

            stmp(0) = T
            stmp(1) = P
            stmp(2) = V

            If Math.Abs(fbb) < 0.01 And fbb <> 0.0# Then
                If res.Count > 0 Then
                    If Math.Abs(T - res(0)(0)) > 5 And P < 100000000 Then res.Add(stmp.Clone)
                Else
                    res.Add(stmp.Clone)
                End If
            End If

            If Vc_inf <= b Then
                GoTo Final2
            Else
                Vc_inf += 2 * delta_Vc
                GoTo restart
            End If

Final2:

            CRITPT_PR = res

        End Function

        Function QIJ_HES_MAT(ByVal T, ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj) As Mapack.Matrix

            Dim i, j As Integer
            Dim n As Double

            Dim am, bm, R As Double

            n = Vz.Length - 1

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            'estimar temperatura e pressao criticas iniciais

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Tr(i) = T / Tc(i)
                    Pc(i) = VPc(i)
                    w(i) = Vw(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                alpha(i) = (1 + (0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.42748 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                b(i) = 0.08664 * R * Tc(i) / Pc(i)
                c(i) = 0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKIj(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                am2(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            am = 0
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    am2(i) = am2(i) + Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            bm = 0
            Do
                bm = bm + Vz(i) * b(i)
                i = i + 1
            Loop Until i = n + 1

            Dim F1, F2, F3, F4, F5, F6, F7, F8 As Double
            Dim K, delta1, delta2 As Double

            K = V / bm
            delta1 = 1
            delta2 = 0

            F1 = 1 / (K - 1)
            F2 = 2 / (delta1 - delta2) * (delta1 / (K + delta1) - delta2 / (K + delta2))
            F3 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 2 - (delta2 / (K + delta2)) ^ 2)
            F4 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 3 - (delta2 / (K + delta2)) ^ 3)
            F5 = 2 / (delta1 - delta2) * Math.Log((K + delta1) / (K + delta2))
            F6 = F2 - F5
            F7 = -F2 / (1 + F1)
            F8 = F3 / (1 + F1)

            Dim alfa As Double

            alfa = am / (bm * R * T)

            Dim dnb_dn(n), dnalfa_dn(n) As Double
            Dim d2nb_dn(n, n), d2nalfa_dn(n, n) As Double
            Dim d3nalfa_dn(n, n, n) As Double

            Dim sum_xa(n) As Double

            Dim dta(n, n) As Integer

            i = 0
            Do
                j = 0
                Do
                    If i = j Then
                        dta(i, j) = 1
                    Else
                        dta(i, j) = 0
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                sum_xa(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    sum_xa(i) += Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim alfa_(n), beta_(n) As Double

            i = 0
            Do
                beta_(i) = b(i) / bm
                alfa_(i) = sum_xa(i) / am
                i = i + 1
            Loop Until i = n + 1

            Dim Q As Mapack.Matrix = New Mapack.Matrix(n + 1, n + 1)

            i = 0
            Do
                j = 0
                Do
                    Q(i, j) = R * T * (dta(i, j) / Vz(i) + (beta_(i) + beta_(j)) * F1 + beta_(i) * beta_(j) * F1 ^ 2) + am / bm * (beta_(i) * beta_(j) * F3 - a(i, j) * F5 / am + (beta_(i) * beta_(j) - alfa_(i) * beta_(j) - alfa_(j) * beta_(i)) * F6)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            'get maximum value
            Dim max As Double = 0
            i = 0
            Do
                j = 0
                Do
                    If Math.Abs(Q(i, j)) > max Then max = Math.Abs(Q(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            'i = 0
            'Do
            '    j = 0
            '    Do
            '        Q(i, j) = Q(i, j) / max
            '        j = j + 1
            '    Loop Until j = n + 1
            '    i = i + 1
            'Loop Until i = n + 1

            QIJ_HES_MAT = Q

        End Function

        Function TRIPLESUM(ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj)

            'Dim MP As New DLLXnumbers.Xnumbers
            'MP.DigitsMax = 20

            Dim T, Tc_sup, Tc_inf As Double

            Dim i, j As Integer
            Dim n As Double

            Dim am, bm, R As Double

            n = Vz.Length - 1

            Dim Dn(n)

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            Tc_inf = Min(VTc) * 0.5
            Tc_sup = Max(VTc) * 1.5

            Dim fT, fT_inf, nsub, delta_Tc As Double

            nsub = 10

            delta_Tc = (Tc_sup - Tc_inf) / nsub

            Do
                fT = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                Tc_inf = Tc_inf + delta_Tc
                fT_inf = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            Loop Until fT * fT_inf < 0 Or Tc_inf > Tc_sup
            Tc_sup = Tc_inf
            Tc_inf = Tc_inf - delta_Tc

            'metodo de Brent para encontrar Tc

            Dim aa, bb, cc, dd, ee, min1, min2, fa, fb, fc, pp, qq, rr, ss, tol1, xm As Double
            Dim ITMAX As Integer = 100
            Dim iter As Integer

            aa = Tc_inf
            bb = Tc_sup
            cc = Tc_sup

            fa = QIJ_HES_MAT(aa, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fc = fb

            iter = 0
            Do
                If (fb > 0 And fc > 0) Or (fb < 0 And fc < 0) Then
                    cc = aa
                    fc = fa
                    dd = bb - aa
                    ee = dd
                End If
                If Math.Abs(fc) < Math.Abs(fb) Then
                    aa = bb
                    bb = cc
                    cc = aa
                    fa = fb
                    fb = fc
                    fc = fa
                End If
                tol1 = 0.000000000001
                xm = 0.5 * (cc - bb)
                'If (Math.Abs(xm) <= tol1) Or (fb = 0) Then GoTo Final
                If Math.Abs(fb) < tol1 Then GoTo Final
                If (Math.Abs(ee) >= tol1) And (Math.Abs(fa) > Math.Abs(fb)) Then
                    ss = fb / fa
                    If aa = cc Then
                        pp = 2 * xm * ss
                        qq = 1 - ss
                    Else
                        qq = fa / fc
                        rr = fb / fc
                        pp = ss * (2 * xm * qq * (qq - rr) - (bb - aa) * (rr - 1))
                        qq = (qq - 1) * (rr - 1) * (ss - 1)
                    End If
                    If pp > 0 Then qq = -qq
                    pp = Math.Abs(pp)
                    min1 = 3 * xm * qq - Math.Abs(tol1 * qq)
                    min2 = Math.Abs(ee * qq)
                    Dim tvar As Double
                    If min1 < min2 Then tvar = min1
                    If min1 > min2 Then tvar = min2
                    If 2 * pp < tvar Then
                        ee = dd
                        dd = pp / qq
                    Else
                        dd = xm
                        ee = dd
                    End If
                Else
                    dd = xm
                    ee = dd
                End If
                aa = bb
                fa = fb
                If (Math.Abs(dd) > tol1) Then
                    bb += dd
                Else
                    bb += Math.Sign(xm) * tol1
                End If
                fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                iter += 1
            Loop Until iter = ITMAX
Final:
            T = bb
            If iter = ITMAX Then GoTo Final2

            Dim MA As Mapack.Matrix, Dn0(n) As Double
            Dim MA_(n, n), MB_(n), Dn0_(n) As Double

            'Dim MP As New DLLXnumbers.Xnumbers
            MA = QIJ_HES_MAT(T, V, Vz, VTc, VPc, VVc, Vw, VKIj)

            Dim m2 As Mapack.Matrix = New Mapack.Matrix(MA.Rows, 1)

            For i = 0 To n
                For j = 0 To n
                    MA_(i, j) = MA(i, j)
                Next
                MB_(i) = Double.Epsilon
            Next

            'Dim solved As Boolean = False
            'solved = MathEx.SysLin.rsolve.rmatrixsolve(MA_, MB_, n, Dn0)

            Try
                Dim trg As Mapack.ILuDecomposition = MA.GetLuDecomposition
                'Console.WriteLine(trg.UpperTriangularFactor.ToString)
                i = 0
                Do
                    m2(i, 0) = 0
                    i = i + 1
                Loop Until i = n + 1
                m2(n, 0) = trg.UpperTriangularFactor(n, n)
                Dim m3 As Mapack.Matrix = trg.UpperTriangularFactor.Solve(m2)
                i = 0
                Do
                    Dn0(i) = m3(i, 0)
                    i = i + 1
                Loop Until i = n + 1
            Catch ex As Exception
                i = 0
                Do
                    Dn0(i) = 0
                    i = i + 1
                Loop Until i = n + 1
            End Try

            Dim soma_Dn = 0.0#
            i = 0
            Do
                soma_Dn += Math.Abs(Dn0(i))
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                Dn(i) = Dn0(i) / soma_Dn
                i = i + 1
            Loop Until i = n + 1

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Tr(i) = T / Tc(i)
                    Pc(i) = VPc(i)
                    w(i) = Vw(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                alpha(i) = (1 + (0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.42748 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                b(i) = 0.08664 * R * Tc(i) / Pc(i)
                c(i) = 0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKIj(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                am2(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            am = 0
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    am2(i) = am2(i) + Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            bm = 0
            Do
                bm = bm + Vz(i) * b(i)
                i = i + 1
            Loop Until i = n + 1

            Dim F1, F2, F3, F4, F5, F6, F7, F8 As Double
            Dim K, delta1, delta2 As Double

            K = V / bm
            delta1 = 1
            delta2 = 0

            F1 = 1 / (K - 1)
            F2 = 2 / (delta1 - delta2) * (delta1 / (K + delta1) - delta2 / (K + delta2))
            F3 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 2 - (delta2 / (K + delta2)) ^ 2)
            F4 = 1 / (delta1 - delta2) * ((delta1 / (K + delta1)) ^ 3 - (delta2 / (K + delta2)) ^ 3)
            F5 = 2 / (delta1 - delta2) * Math.Log((K + delta1) / (K + delta2))
            F6 = F2 - F5
            F7 = -F2 / (1 + F1)
            F8 = F3 / (1 + F1)

            Dim sum_xa(n) As Double

            i = 0
            Do
                sum_xa(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    sum_xa(i) += Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim alfa_(n), beta_(n) As Double

            i = 0
            Do
                beta_(i) = b(i) / bm
                alfa_(i) = sum_xa(i) / am
                i = i + 1
            Loop Until i = n + 1

            Dim a_ As Double = 0
            Dim b_ As Double = 0
            Dim af_ As Double = 0

            i = 0
            Do
                j = 0
                Do
                    a_ += a(i, j) * Dn(i) * Dn(j) / am
                    j = j + 1
                Loop Until j = n + 1
                b_ += beta_(i) * Dn(i)
                af_ += alfa_(i) * Dn(i)
                i = i + 1
            Loop Until i = n + 1

            Dim n_, sum_Dn3 As Double

            n_ = 0
            sum_Dn3 = 0

            i = 0
            Do
                n_ = n_ + Dn(i)
                sum_Dn3 = sum_Dn3 + Dn(i) ^ 3 / Vz(i) ^ 2
                i = i + 1
            Loop Until i = n + 1

            Dim TS As Double

            TS = R * T * (-sum_Dn3 + 3 * n_ * (b_ * F1) ^ 2 + 2 * (b_ * F1) ^ 3) + am / bm * (3 * b_ ^ 2 * (2 * af_ - b_) * (F3 + F6) - 2 * b_ ^ 3 * F4 - 3 * b_ * a_ * F6)

Final2:

            TRIPLESUM = TS

        End Function

        Function TCRIT(ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj)

            'Dim MP As New DLLXnumbers.Xnumbers
            'MP.DigitsMax = 20

            Dim T, Tc_sup, Tc_inf As Double

            Dim n As Double

            n = Vz.Length - 1

            Dim Dn(n)

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            Tc_inf = Min(VTc) * 0.5
            Tc_sup = Max(VTc) * 1.5

            Dim fT, fT_inf, nsub, delta_Tc As Double

            nsub = 10

            delta_Tc = (Tc_sup - Tc_inf) / nsub

            Do
                fT = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                Tc_inf = Tc_inf + delta_Tc
                fT_inf = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            Loop Until fT * fT_inf < 0 Or Tc_inf > Tc_sup
            Tc_inf = Tc_inf - delta_Tc
            Tc_sup = Tc_inf

            'metodo de Brent para encontrar Tc

            Dim aa, bb, cc, dd, ee, min1, min2, fa, fb, fc, pp, qq, rr, ss, tol1, xm As Double
            Dim ITMAX As Integer = 100
            Dim iter As Integer

            aa = Tc_inf
            bb = Tc_sup
            cc = Tc_sup

            fa = QIJ_HES_MAT(aa, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fc = fb

            iter = 0
            Do
                If (fb > 0 And fc > 0) Or (fb < 0 And fc < 0) Then
                    cc = aa
                    fc = fa
                    dd = bb - aa
                    ee = dd
                End If
                If Math.Abs(fc) < Math.Abs(fb) Then
                    aa = bb
                    bb = cc
                    cc = aa
                    fa = fb
                    fb = fc
                    fc = fa
                End If
                tol1 = 0.000000000001
                xm = 0.5 * (cc - bb)
                If (Math.Abs(xm) <= tol1) Or (fb = 0) Then GoTo Final
                If (Math.Abs(ee) >= tol1) And (Math.Abs(fa) > Math.Abs(fb)) Then
                    ss = fb / fa
                    If aa = cc Then
                        pp = 2 * xm * ss
                        qq = 1 - ss
                    Else
                        qq = fa / fc
                        rr = fb / fc
                        pp = ss * (2 * xm * qq * (qq - rr) - (bb - aa) * (rr - 1))
                        qq = (qq - 1) * (rr - 1) * (ss - 1)
                    End If
                    If pp > 0 Then qq = -qq
                    pp = Math.Abs(pp)
                    min1 = 3 * xm * qq - Math.Abs(tol1 * qq)
                    min2 = Math.Abs(ee * qq)
                    Dim tvar As Double
                    If min1 < min2 Then tvar = min1
                    If min1 > min2 Then tvar = min2
                    If 2 * pp < tvar Then
                        ee = dd
                        dd = pp / qq
                    Else
                        dd = xm
                        ee = dd
                    End If
                Else
                    dd = xm
                    ee = dd
                End If
                aa = bb
                fa = fb
                If (Math.Abs(dd) > tol1) Then
                    bb += dd
                Else
                    bb += Math.Sign(xm) * tol1
                End If
                fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                iter += 1
            Loop Until iter = ITMAX
Final:
            T = bb
            If iter = ITMAX Then GoTo Final2

Final2:

            TCRIT = T

        End Function

        Function PCRIT(ByVal T, ByVal V, ByVal Vx, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VKIj)

            Dim ai(), bi() As Double
            Dim n, R, P, coeff(3), tmp() As Double
            Dim Tc(), Pc(), W(), alpha(), Vant(0, 4), m(), a(,), b(,), Tr() As Double

            n = Vx.Length - 1

            ReDim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n)
            ReDim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n)

            R = 8.314

            Dim i, j As Integer
            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                W(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                alpha(i) = (1 + (0.48 + 1.574 * W(i) - 0.176 * W(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.42748 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.08664 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKIj(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim aml = 0.0#
            Do
                j = 0
                Do
                    aml = aml + Vx(i) * Vx(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bml = 0.0#
            Do
                bml = bml + Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            P = R * T / (V - bml) - aml / (V * (V + bml))

            PCRIT = P

        End Function

        Function STABILITY_CURVE(ByVal Vz As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal VVc As Object, ByVal Vw As Object, ByVal VKIj As Object, Optional ByVal Vmax As Double = 0, Optional ByVal delta As Double = 40, Optional ByVal multipl As Integer = 15) As ArrayList

            'Dim MP As New DLLXnumbers.Xnumbers

            Dim V, Vmin, deltaV As Double

            Dim stmp(2)
            Dim n, R, P, T As Double
            Dim i As Integer

            n = Vz.Length - 1

            Dim Tc(n), Pc(n)
            Dim b As Double

            'estimar temperatura e pressao criticas iniciais

            R = 8.314

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = VTc(i)
                    Pc(i) = VPc(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            b = 0
            Do
                b += Vz(i) * 0.08664 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            'estimar temperatura e pressao criticas iniciais

            If Vmax = 0 Then Vmax = b * multipl
            Vmin = b * 1.05

            deltaV = (Vmax - Vmin) / 100 'delta

            Dim result As ArrayList = New ArrayList()

            V = Vmax
            Do
                T = TCRIT2(V, Vz, VTc, VPc, VVc, Vw, VKIj)
                'P = 0.307 * 8.314 * T / V
                P = PCRIT(T, V, Vz, VTc, VPc, Vw, VKIj)
                If P < 0 Then Exit Do
                result.Add(New Object() {T, P})
                V -= deltaV
            Loop Until V <= Vmin

            STABILITY_CURVE = result

        End Function

        Function TCRIT2(ByVal V, ByVal Vz, ByVal VTc, ByVal VPc, ByVal VVc, ByVal Vw, ByVal VKIj)

            'Dim MP As New DLLXnumbers.Xnumbers
            'MP.DigitsMax = 20

            Dim T, Tc_sup, Tc_inf As Double

            Dim n As Double

            n = Vz.Length - 1

            Dim Dn(n)

            Dim ai(n), b(n), c(n), tmp(2, n + 1), a(n, n), am2(n)
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), Tr(n)

            Tc_inf = MathEx.Common.Min(VTc) * 0.5
            Tc_sup = MathEx.Common.Max(VTc) * 1.5

            Dim fT, fT_inf, nsub, delta_Tc As Double

            nsub = 50

            delta_Tc = (Tc_sup - Tc_inf) / nsub

            Do
                fT = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                Tc_inf = Tc_inf + delta_Tc
                fT_inf = QIJ_HES_MAT(Tc_inf, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            Loop Until fT * fT_inf < 0 Or Tc_inf > Tc_sup
            Tc_sup = Tc_inf
            Tc_inf = Tc_inf - delta_Tc

            'metodo de Brent para encontrar Tc

            Dim aa, bb, cc, dd, ee, min1, min2, fa, fb, fc, pp, qq, rr, ss, tol1, xm As Double
            Dim ITMAX As Integer = 100
            Dim iter As Integer

            aa = Tc_inf
            bb = Tc_sup
            cc = Tc_sup

            fa = QIJ_HES_MAT(aa, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
            fc = fb

            iter = 0
            Do
                If (fb > 0 And fc > 0) Or (fb < 0 And fc < 0) Then
                    cc = aa
                    fc = fa
                    dd = bb - aa
                    ee = dd
                End If
                If Math.Abs(fc) < Math.Abs(fb) Then
                    aa = bb
                    bb = cc
                    cc = aa
                    fa = fb
                    fb = fc
                    fc = fa
                End If
                tol1 = 0.000000000001
                xm = 0.5 * (cc - bb)
                'If (Math.Abs(xm) <= tol1) Or (fb = 0) Then GoTo Final
                If Math.Abs(fb) < tol1 Then GoTo Final
                If (Math.Abs(ee) >= tol1) And (Math.Abs(fa) > Math.Abs(fb)) Then
                    ss = fb / fa
                    If aa = cc Then
                        pp = 2 * xm * ss
                        qq = 1 - ss
                    Else
                        qq = fa / fc
                        rr = fb / fc
                        pp = ss * (2 * xm * qq * (qq - rr) - (bb - aa) * (rr - 1))
                        qq = (qq - 1) * (rr - 1) * (ss - 1)
                    End If
                    If pp > 0 Then qq = -qq
                    pp = Math.Abs(pp)
                    min1 = 3 * xm * qq - Math.Abs(tol1 * qq)
                    min2 = Math.Abs(ee * qq)
                    Dim tvar As Double
                    If min1 < min2 Then tvar = min1
                    If min1 > min2 Then tvar = min2
                    If 2 * pp < tvar Then
                        ee = dd
                        dd = pp / qq
                    Else
                        dd = xm
                        ee = dd
                    End If
                Else
                    dd = xm
                    ee = dd
                End If
                aa = bb
                fa = fb
                If (Math.Abs(dd) > tol1) Then
                    bb += dd
                Else
                    bb += Math.Sign(xm) * tol1
                End If
                fb = QIJ_HES_MAT(bb, V, Vz, VTc, VPc, VVc, Vw, VKIj).Determinant
                iter += 1
            Loop Until iter = ITMAX
Final:
            T = bb
            If iter = ITMAX Then GoTo Final2

Final2:

            TCRIT2 = T

        End Function

    End Class

End Namespace