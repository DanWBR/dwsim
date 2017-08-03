'    Riazi Method Calculation Routines 
'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Namespace Utilities.PetroleumCharacterization.Methods

    <System.Serializable()> Public Class Riazi

        Function Distr_Riazi(ByVal n As Integer, ByVal MW As Double, ByVal SG As Double, ByVal WK As Double, ByVal T1 As Double, ByVal T2 As Double, ByVal V1 As Double, ByVal V2 As Double)

            Dim i As Integer
            Dim MW_0, SG_0, TB, TB_0, v1_0, v2_0 As Double

            Dim MW_A, MW_B, MW_avx, MW_gi_1, MW_gi_2 As Double
            Dim MW_d(999), MW_z(n - 1), MW_q(n - 1), MW_p(n - 1) As Double

            Dim SG_A, SG_B, SG_avx, SG_gi_1, SG_gi_2 As Double
            Dim SG_d(999), SG_z(n - 1), SG_q(n - 1), SG_p(n - 1) As Double

            Dim TB_A, TB_B, TB_avx, TB_gi_1, TB_gi_2 As Double
            Dim TB_d(999), TB_z(n - 1), TB_q(n - 1), TB_p(n - 1) As Double

            Dim v1_A, v1_B, v1_avx, v1_gi_1, v1_gi_2 As Double
            Dim v1_d(999), v1_z(n - 1), v1_q(n - 1), v1_p(n - 1) As Double

            Dim v2_A, v2_B, v2_avx, v2_gi_1, v2_gi_2 As Double
            Dim v2_d(999), v2_z(n - 1), v2_q(n - 1), v2_p(n - 1) As Double

            Dim Vx_dist(n - 1) As Double

            Dim a, b, c, d As Double

            If MW <> 0 Then

                MW_0 = 90
                MW_avx = MW / MW_0 - 1
                MW_A = MW_avx
                MW_B = 1

                i = 0
                Do
                    MW_d(i) = (MW_A / MW_B * Math.Log(1 / (1 - i / 1000))) ^ (1 / MW_B)
                    i = i + 1
                Loop Until i = 1000

                i = 1
                Do
                    MW_z(i - 1) = Math.Exp(-MW_B / MW_A * MW_d((i - 1) * 999 / (n)) ^ MW_B) - Math.Exp(-MW_B / MW_A * MW_d(i * 999 / (n)) ^ MW_B)
                    i = i + 1
                Loop Until i = n + 1

                Vx_dist = MW_z

                i = 0
                Do
                    MW_q(i) = MW_B / MW_A * MW_d((i + 1) * 1000 / (n + 1)) ^ MW_B
                    i = i + 1
                Loop Until i = n

                a = 1.0047729176981601
                b = 0.00836759925436675
                c = 0.31579045418979956
                d = 1.4969389791593306

                i = 1
                Do
                    If i <> n Then
                        MW_gi_1 = b - (b - a) * Math.Exp(-c * (MW_q(i - 1) ^ d))
                        MW_gi_2 = b - (b - a) * Math.Exp(-c * (MW_q(i) ^ d))
                    Else
                        MW_gi_1 = b - (b - a) * Math.Exp(-c * (MW_q(i - 2) ^ d))
                        MW_gi_2 = b - (b - a) * Math.Exp(-c * (MW_q(i - 1) ^ d))
                    End If
                    MW_p(i - 1) = 1 / MW_z(i - 1) * (MW_A / MW_B) ^ (1 / MW_B) * (MW_gi_1 - MW_gi_2)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    MW_p(i) = MW_0 * (1 + MW_p(i))
                    i = i + 1
                Loop Until i = n

            End If

            If SG <> 0 Then

                SG_0 = 0.7
                SG_avx = SG / SG_0 - 1
                SG_A = (SG_avx / 0.619) ^ 3
                SG_B = 3

                i = 0
                Do
                    SG_d(i) = (SG_A / SG_B * Math.Log(1 / (1 - i / 1000))) ^ (1 / SG_B)
                    i = i + 1
                Loop Until i = 1000

                i = 1
                Do
                    SG_z(i - 1) = Math.Exp(-SG_B / SG_A * SG_d((i - 1) * 999 / (n + 1)) ^ SG_B) - Math.Exp(-SG_B / SG_A * SG_d(i * 999 / (n + 1)) ^ SG_B)
                    i = i + 1
                Loop Until i = n + 1

                Vx_dist = SG_z

                i = 0
                Do
                    SG_q(i) = SG_B / SG_A * SG_d((i + 1) * 1000 / (n + 1)) ^ SG_B
                    i = i + 1
                Loop Until i = n

                a = 0.90042013692039768
                b = -0.0049102522593429
                c = 0.76738843430465553
                d = 0.98214367448668149

                i = 1
                Do
                    If i <> n Then
                        SG_gi_1 = b - (b - a) * Math.Exp(-c * (SG_q(i - 1) ^ d))
                        SG_gi_2 = b - (b - a) * Math.Exp(-c * (SG_q(i) ^ d))
                    Else
                        SG_gi_1 = b - (b - a) * Math.Exp(-c * (SG_q(i - 2) ^ d))
                        SG_gi_2 = b - (b - a) * Math.Exp(-c * (SG_q(i - 1) ^ d))
                    End If
                    SG_p(i - 1) = 1 / SG_z(i - 1) * (SG_A / SG_B) ^ (1 / SG_B) * (SG_gi_1 - SG_gi_2)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    SG_p(i) = SG_0 * (1 + SG_p(i))
                    i = i + 1
                Loop Until i = n

            End If

            If WK <> 0 Then

                TB_0 = 1080 - Math.Exp(6.97996 - 0.01964 * 90 ^ (2 / 3))

                TB = ((SG * WK) ^ 3) * 0.55556

                TB_avx = TB / TB_0 - 1
                TB_A = (TB_avx / 0.689) ^ (3 / 2)
                TB_B = 1.5

                i = 0
                Do
                    TB_d(i) = (TB_A / TB_B * Math.Log(1 / (1 - i / 1000))) ^ (1 / TB_B)
                    i = i + 1
                Loop Until i = 1000

                i = 1
                Do
                    TB_z(i - 1) = Math.Exp(-TB_B / TB_A * TB_d((i - 1) * 999 / (n + 1)) ^ TB_B) - Math.Exp(-TB_B / TB_A * TB_d(i * 999 / (n + 1)) ^ TB_B)
                    i = i + 1
                Loop Until i = n + 1

                Vx_dist = TB_z

                i = 0
                Do
                    TB_q(i) = TB_B / TB_A * TB_d((i + 1) * 1000 / (n + 1)) ^ TB_B
                    i = i + 1
                Loop Until i = n

                a = 0.9055123773075332
                b = 0.023814486817079358
                c = 0.28514645349487877
                d = 1.9566747404253064

                i = 1
                Do
                    If i <> n Then
                        TB_gi_1 = b - (b - a) * Math.Exp(-c * (TB_q(i - 1) ^ d))
                        TB_gi_2 = b - (b - a) * Math.Exp(-c * (TB_q(i) ^ d))
                    Else
                        TB_gi_1 = b - (b - a) * Math.Exp(-c * (TB_q(i - 2) ^ d))
                        TB_gi_2 = b - (b - a) * Math.Exp(-c * (TB_q(i - 1) ^ d))
                    End If
                    TB_p(i - 1) = 1 / TB_z(i - 1) * (TB_A / TB_B) ^ (1 / TB_B) * (TB_gi_1 - TB_gi_2)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    TB_p(i) = TB_0 * (1 + TB_p(i))
                    i = i + 1
                Loop Until i = n

            End If

            If MW <> 0 And SG = 0 And WK = 0 Then

                i = 0
                Do
                    TB_p(i) = 1080 - Math.Exp(6.97996 - 0.01964 * MW_p(i) ^ (2 / 3))
                    SG_p(i) = 1.07 - Math.Exp(3.56073 - 2.93886 * MW_p(i) ^ 0.1)
                    i = i + 1
                Loop Until i = n

            End If

            If MW <> 0 And SG <> 0 And WK = 0 Then

                i = 0
                Do
                    TB_p(i) = 1080 - Math.Exp(6.97996 - 0.01964 * MW_p(i) ^ (2 / 3))
                    i = i + 1
                Loop Until i = n

            End If

            If MW <> 0 And SG = 0 And WK <> 0 Then

                i = 0
                Do
                    SG_p(i) = 1.07 - Math.Exp(3.56073 - 2.93886 * MW_p(i) ^ 0.1)
                    i = i + 1
                Loop Until i = n

            End If

            If MW = 0 And SG <> 0 And WK = 0 Then

                i = 0
                Do
                    MW_p(i) = ((Math.Log(1.07 - SG_p(i)) - 3.56073) / (-2.93886)) ^ 10
                    TB_p(i) = 1080 - Math.Exp(6.97996 - 0.01964 * MW_p(i) ^ (2 / 3))
                    i = i + 1
                Loop Until i = n

            End If

            If MW = 0 And SG <> 0 And WK <> 0 Then

                i = 0
                Do
                    MW_p(i) = ((Math.Log(1.07 - SG_p(i)) - 3.56073) / (-2.93886)) ^ 10
                    i = i + 1
                Loop Until i = n

            End If

            If MW = 0 And SG = 0 And WK <> 0 Then

                i = 0
                Do
                    MW_p(i) = (1 / 0.01964 * (6.97996 - Math.Log(1080 - TB_p(i)))) ^ (3 / 2)
                    SG_p(i) = 1080 - Math.Exp(6.97996 - 0.01964 * MW_p(i) ^ (2 / 3))
                    i = i + 1
                Loop Until i = n

            End If

            If MW = 0 And SG = 0 And WK <> 0 Then

                i = 0
                Do
                    MW_p(i) = (1 / 0.01964 * (6.97996 - Math.Log(1080 - TB_p(i)))) ^ (3 / 2)
                    SG_p(i) = 1080 - Math.Exp(6.97996 - 0.01964 * MW_p(i) ^ (2 / 3))
                    i = i + 1
                Loop Until i = n

            End If

            'Calculo das propriedades criticas

            Dim Tc_p(n - 1), Pc_p(n - 1), w_p(n - 1) As Double

            i = 0
            Do
                Tc_p(i) = 189.8 + 450.6 * SG_p(i) + (0.4244 + 0.1174 * SG_p(i)) * TB_p(i) + (0.1441 - 1.0069 * SG_p(i)) * 100000.0 / TB_p(i)
                Pc_p(i) = Math.Exp(5.689 - 0.0566 / SG_p(i) - (0.43639 + 4.1216 / SG_p(i) + 0.21343 / SG_p(i) ^ 2) * 0.001 * TB_p(i) + (0.47579 + 1.182 / SG_p(i) + 0.15302 / SG_p(i) ^ 2) * 0.000001 * TB_p(i) ^ 2 - (2.4505 + 9.9099 / SG_p(i) ^ 2) * 0.0000000001 * TB_p(i) ^ 3)
                w_p(i) = (-Math.Log(Pc_p(i) / 1.01325) - 5.92714 + 6.09648 / (TB_p(i) / Tc_p(i)) + 1.28862 * Math.Log(TB_p(i) / Tc_p(i)) - 0.169347 * (TB_p(i) / Tc_p(i)) ^ 6) / (15.2518 - 15.6875 / (TB_p(i) / Tc_p(i)) - 13.4721 * Math.Log(TB_p(i) / Tc_p(i)) + 0.43577 * (TB_p(i) / Tc_p(i)) ^ 6)
                Pc_p(i) = 0.986923 * Pc_p(i)
                i = i + 1
            Loop Until i = n

            'Calculo das viscosidades

            If V1 = 0 Then

                i = 0
                Do
                    v1_p(i) = 1000 * PropertyPackages.Auxiliary.PROPS.viscl_letsti(T1 + 273.15, Tc_p(i), Pc_p(i) * 100000.0, w_p(i), MW_p(i))
                    i = i + 1
                Loop Until i = n

            Else

                v1_0 = 0.1 * V1
                v1_avx = V1 / v1_0 - 1
                v1_B = 0.7
                v1_A = (v1_avx / (0.992814 - 0.504242 * v1_B ^ -1 + 0.696215 * v1_B ^ -2 - 0.272936 * v1_B ^ -3 + 0.088362 * v1_B ^ -4)) ^ v1_B * v1_B

                i = 0
                Do
                    v1_d(i) = (v1_A / v1_B * Math.Log(1 / (1 - i / 1000))) ^ (1 / v1_B)
                    i = i + 1
                Loop Until i = 1000

                i = 1
                Do
                    v1_z(i - 1) = Math.Exp(-v1_B / v1_A * v1_d((i - 1) * 999 / (n + 1)) ^ v1_B) - Math.Exp(-v1_B / v1_A * v1_d(i * 999 / (n + 1)) ^ v1_B)
                    i = i + 1
                Loop Until i = n + 1

                Vx_dist = v1_z

                i = 0
                Do
                    v1_q(i) = v1_B / v1_A * v1_d((i + 1) * 1000 / (n + 1)) ^ v1_B
                    i = i + 1
                Loop Until i = n

                a = 1.2779512667511526
                b = 0.0097469995695727268
                c = 0.19222719403601637
                d = 1.6783369425946355

                i = 1
                Do
                    If i <> n Then
                        v1_gi_1 = b - (b - a) * Math.Exp(-c * (v1_q(i - 1) ^ d))
                        v1_gi_2 = b - (b - a) * Math.Exp(-c * (v1_q(i) ^ d))
                    Else
                        v1_gi_1 = b - (b - a) * Math.Exp(-c * (v1_q(i - 2) ^ d))
                        v1_gi_2 = b - (b - a) * Math.Exp(-c * (v1_q(i - 1) ^ d))
                    End If
                    v1_p(i - 1) = 1 / v1_z(i - 1) * (v1_A / v1_B) ^ (1 / v1_B) * (v1_gi_1 - v1_gi_2)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    v1_p(i) = v1_0 * (1 + v1_p(i))
                    i = i + 1
                Loop Until i = n

            End If

            If V2 = 0 Then

                i = 0
                Do
                    v2_p(i) = 1000 * PropertyPackages.Auxiliary.PROPS.viscl_letsti(T2 + 273.15, Tc_p(i), Pc_p(i) * 100000.0, w_p(i), MW_p(i))
                    i = i + 1
                Loop Until i = n

            Else

                v2_0 = 0.1 * V2
                v2_avx = V2 / v2_0 - 1

                v2_B = 0.7
                v2_A = (v2_avx / (0.992814 - 0.504242 * v2_B ^ -1 + 0.696215 * v2_B ^ -2 - 0.272936 * v2_B ^ -3 + 0.088362 * v2_B ^ -4)) ^ v2_B * v2_B

                i = 0
                Do
                    v2_d(i) = (v2_A / v2_B * Math.Log(1 / (1 - i / 1000))) ^ (1 / v2_B)
                    i = i + 1
                Loop Until i = 1000

                i = 1
                Do
                    v2_z(i - 1) = Math.Exp(-v2_B / v2_A * v2_d((i - 1) * 999 / (n + 1)) ^ v2_B) - Math.Exp(-v2_B / v2_A * v2_d(i * 999 / (n + 1)) ^ v2_B)
                    i = i + 1
                Loop Until i = n + 1

                Vx_dist = v2_z

                i = 0
                Do
                    v2_q(i) = v2_B / v2_A * v2_d((i + 1) * 1000 / (n + 1)) ^ v2_B
                    i = i + 1
                Loop Until i = n

                a = 1.2779512667511526
                b = 0.0097469995695727268
                c = 0.19222719403601637
                d = 1.6783369425946355

                i = 1
                Do
                    If i <> n Then
                        v2_gi_1 = b - (b - a) * Math.Exp(-c * (v2_q(i - 1) ^ d))
                        v2_gi_2 = b - (b - a) * Math.Exp(-c * (v2_q(i) ^ d))
                    Else
                        v2_gi_1 = b - (b - a) * Math.Exp(-c * (v2_q(i - 2) ^ d))
                        v2_gi_2 = b - (b - a) * Math.Exp(-c * (v2_q(i - 1) ^ d))
                    End If
                    v2_p(i - 1) = 1 / v2_z(i - 1) * (v2_A / v2_B) ^ (1 / v2_B) * (v2_gi_1 - v2_gi_2)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    v2_p(i) = v2_0 * (1 + v2_p(i))
                    i = i + 1
                Loop Until i = n

            End If



            Dim somax As Double
            i = 0
            somax = 0
            Do
                somax += Vx_dist(i)
                i = i + 1
            Loop Until i = n

            Dim mat(n - 1, 10)

            i = 0
            Do
                mat(i, 0) = i + 1
                mat(i, 1) = Vx_dist(i) / somax
                If i = 0 Then
                    mat(i, 2) = Convert.ToDouble(mat(i, 1))
                Else
                    mat(i, 2) = Convert.ToDouble(mat(i, 1)) + Convert.ToDouble(mat(i - 1, 2))
                End If
                mat(i, 3) = TB_p(i)
                mat(i, 4) = MW_p(i)
                mat(i, 5) = SG_p(i) * 1000
                mat(i, 6) = v1_p(i)
                mat(i, 7) = v2_p(i)
                mat(i, 8) = Tc_p(i)
                mat(i, 9) = Pc_p(i) * 100000.0
                mat(i, 10) = w_p(i)
                i += 1
            Loop Until i = n

            Distr_Riazi = mat

        End Function

    End Class

End Namespace
