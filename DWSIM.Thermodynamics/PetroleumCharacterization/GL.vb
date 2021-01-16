'    GL Method Calculation Routines
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

Imports System.math

Namespace Utilities.PetroleumCharacterization.Methods

    <System.Serializable()> Public Class GL

        Sub New()

        End Sub

        Public Function calculate_Hf_Sf(ByVal SG As Double, ByVal M As Double, ByVal TB As Double)

            Dim I, IR As Double

            I = 0.02266 * Exp(0.0003905 * (1.8 * TB) + 2.468 * SG - 0.0005704 * 1.8 * TB * SG) * (1.8 * TB) ^ 0.0572 * SG ^ -0.72
            IR = ((1 + 2 * I) / (1 - I)) ^ 0.5

            'TB = TB - 273.15

            Dim CH, Hf, Sf, L, Lcor, G, Gcor, Mc, Nar, Nar1, Nar2, Narnf, Nc2, Nc, Nh, _
                NH_, m0, Nnf, Nnf1, Nnf2, Npn, P, Rar, Rnf, Rt, alpha, Car, Cnf, Cpn As Double

            G = M * (SG - 0.8513) / SG + 23.6
            L = M * (IR - 1.4752) / IR ^ 2 + 4.51

            Gcor = G
            Lcor = L

            If G < 16 Then
                Rnf = (Gcor - 4.6154 * Lcor) / 4.45
                Rar = (Lcor - 2.178 * Rnf) / 5.46
                Rt = Rar + Rnf
            ElseIf G >= 16 And G < 26 Then
                alpha = 3.685 - 0.58 * Gcor / Lcor
                If alpha < 0 Then alpha = 0
                If alpha > 1 Then alpha = 1
                Rt = 1 + (Lcor - (2.18 + 3.28 * alpha ^ 1.5)) / (2.32 + 4.45 * alpha ^ 1.5)
                Rar = Rt * alpha
                Rnf = Rt - Rar
            ElseIf G >= 26 Then
                alpha = 2.512 - 0.484 * (Gcor - 13.1) / (Lcor - 1.45)
                If alpha < 0 Then alpha = 0
                If alpha > 1 Then alpha = 1
                Rt = 1 + (Lcor - (2.18 + 3.28 * alpha ^ 1.5)) / (2.32 + 4.45 * alpha ^ 1.5)
                Rar = Rt * alpha
                Rnf = Rt - Rar
            End If

            'Riazi MR, Daubert TE. Prediction of molecular-type analysis of
            'petroleum fraction and coal liquids. Ind Eng Chem Process Des Dev
            '1986;25:1009–15.

            'carbon-to-hydrogen weight ratio
            CH = 17.22 * Exp(0.00825 * (1.8 * TB) + 16.94 * SG - 0.00694 * (1.8 * TB) * SG) * (1.8 * TB) ^ -2.725 * SG ^ -6.798
            'paraffin content (%)
            P = 257 - 287.7 * SG + 2.876 * CH

            NH_ = (100 + P) / 100

            If Rt < 1 Then m0 = 6
            If Rt > 1 Then m0 = 2 - 2 * (Rt - 1) / Rt

            If (Rt - 1) < 0 Then
                Mc = 0.85632 * (M - NH_ + m0 * Rar + Rt)
            Else
                Mc = 0.85632 * (M - NH_ + m0 * Rar + Rt + 2 * (Rt - 1))
            End If

            Car = 100 * (12.011 * m0 * Rar) / Mc
            Cnf = 100 * (12.011 * m0 * Rnf) / Mc
            Cpn = 100 - Car - Cnf

            If Car + Cnf > 100 Then
                Cnf = 100 - Car
                Cpn = 0
            End If

            Npn = (0.01 * Cpn * Mc / 12.011)
            Nc2 = Npn
            Nar = (0.01 * Car * Mc / 12.011)
            Nnf = (0.01 * Cnf * Mc / 12.011)
            Nar2 = 2 * (Rar - 1)
            Nnf2 = 2 * (Rnf - 1)
            Nnf1 = Nnf - Nnf2
            If Nar2 < 0 Then Nar2 = 0
            If Nnf2 < 0 Then Nnf2 = 0

            Nar1 = Nar - Nar2 - Narnf
            Nnf1 = Nnf - Nnf2

            Nc = Npn + Nar + Nnf
            Nh = Nc * (12 / CH)

            Hf = -0.4354 * P - 20.63 * Nc2 - 21.94 * Nnf1 - 3.96 * Nnf2 + 11.71 * Nar1 + 21.76 * Nar2 + 27.26 * Narnf + 6.64 * Rt * (1 - Npn)
            Sf = 1.542 * P + 39 * Nc2 + 46.9 * Nar1 - 16.6 * Nar2 - 16.6 * Narnf + 50.8 * Nnf1 - 15 * Nnf2 + 2.9 * Rt * (1 - Npn)
            Hf = Hf / M
            Sf = Sf / M

            If Double.IsNaN(Nc) Then Nc = 0.0
            If Double.IsNaN(Nh) Then Nh = 0.0

            Return New Double() {Hf, Sf, Nc, Nh}

        End Function


    End Class

End Namespace

