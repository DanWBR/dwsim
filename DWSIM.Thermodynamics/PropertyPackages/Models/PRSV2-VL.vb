'    Peng-Robinson-Stryjek-Vera 2 w/ Van Laar Mixing Rules Property Package 
'    Copyright 2012-2024 Daniel Wagner O. de Medeiros
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

Imports FileHelpers
Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.PolySolve

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class PRSV2VL

        Dim m_pr As New PropertyPackages.Auxiliary.PROPS
        Private _ip As Dictionary(Of String, Dictionary(Of String, PRSV2_IPData))
        Public _data As Dictionary(Of String, PRSV2Param)

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, PRSV2_IPData))
            Get
                Return _ip
            End Get
        End Property

        Sub New()

            _data = New Dictionary(Of String, PRSV2Param)
            _ip = New Dictionary(Of String, Dictionary(Of String, PRSV2_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim prsv2data As PRSV2Param
            Dim prsv2datac() As PRSV2Param
            Dim fh0 As New FileHelperEngine(Of PRSV2Param)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.prsv2.dat")
                Using t As New IO.StreamReader(filestr)
                    prsv2datac = fh0.ReadStream(t)
                End Using
            End Using

            For Each prsv2data In prsv2datac
                If Not _data.ContainsKey(prsv2data.compound.ToLower) Then _data.Add(prsv2data.compound.ToLower, prsv2data)
            Next

            fh0 = Nothing

            Dim prip As PRSV2_IPData
            Dim pripc() As PRSV2_IPData
            Dim fh1 As New FileHelperEngine(Of PRSV2_IPData)
        
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.prsv2_ip_vl.dat")
                Using t As New IO.StreamReader(filestr)
                    pripc = fh1.ReadStream(t)
                End Using
            End Using

            For Each prip In pripc
                If Me.InteractionParameters.ContainsKey(prip.id1.ToLower) Then
                    If Me.InteractionParameters(prip.id1.ToLower).ContainsKey(prip.id2.ToLower) Then
                    Else
                        Me.InteractionParameters(prip.id1.ToLower).Add(prip.id2.ToLower, prip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(prip.id1.ToLower, New Dictionary(Of String, PRSV2_IPData))
                    Me.InteractionParameters(prip.id1.ToLower).Add(prip.id2.ToLower, prip.Clone)
                End If
            Next
            For Each prip In pripc
                If Me.InteractionParameters.ContainsKey(prip.id1.ToLower) Then
                    If Me.InteractionParameters((prip.id1.ToLower)).ContainsKey((prip.id2.ToLower)) Then
                    Else
                        Me.InteractionParameters((prip.id1.ToLower)).Add((prip.id2.ToLower), prip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(prip.id1.ToLower, New Dictionary(Of String, PRSV2_IPData))
                    Me.InteractionParameters((prip.id1.ToLower)).Add((prip.id2.ToLower), prip.Clone)
                End If
            Next
            prip = Nothing
            pripc = Nothing
            fh1 = Nothing
        End Sub

        Function CpCvR(ByVal TIPO, ByVal T, ByVal P, ByVal Vz, ByVal VKij, ByVal VKij2, ByVal Vk1, ByVal Vk2, ByVal Vk3, ByVal Vzmass, ByVal VTc, ByVal VPc, ByVal VCpig, ByVal VMM, ByVal Vw, ByVal VZRa)

            Dim ai(), bi(), ci() As Double
            Dim n, R As Double
            Dim Tc(), Pc(), Vc(), w(), Zc(), alpha(), m(), a(,), b(,), Z, Tr() As Double
            Dim i, j, dadT

            n = Vz.Length - 1

            ReDim ai(n), bi(n), ci(n), a(n, n), b(n, n)
            ReDim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), m(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vk1(i) * Vk2(i) * Vk3(i) <> 0.0 Then
                    ci(i) = (0.378893 + 1.4897153 * w(i) - 0.17131848 * w(i) ^ 2 + 0.0196544 * w(i) ^ 3) + (Vk1(i) + Vk2(i) * (Vk3(i) - Tr(i)) * (1 - Tr(i) ^ 0.5)) * (1 + Tr(i) ^ 0.5) * (0.7 - Tr(i))
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                Else
                    If w(i) <= 0.491 Then
                        ci(i) = 0.37464 + 1.5422 * w(i) - 0.26992 * w(i) ^ 2
                    Else
                        ci(i) = 0.379642 + 1.48503 * w(i) - 0.164423 * w(i) ^ 2 + 0.016666 * w(i) ^ 3
                    End If
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                End If
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vz(i) * VKij(i, j) + Vz(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - term)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim am = 0.0#
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bm = 0.0#
            Do
                bm = bm + Vz(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            'Dim dadT = 

            Dim AG1 = am * P / (R * T) ^ 2
            Dim BG1 = bm * P / (R * T)

            Dim coeff(3) As Double

            coeff(0) = -AG1 * BG1 + BG1 ^ 2 + BG1 ^ 3
            coeff(1) = AG1 - 3 * BG1 ^ 2 - 2 * BG1
            coeff(2) = BG1 - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv
            Dim tv2

            If Not IsNumeric(temp1) Then

                If temp1(0, 0) > temp1(1, 0) Then
                    tv = temp1(1, 0)
                    tv2 = temp1(1, 1)
                    temp1(1, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    temp1(1, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(0, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(1, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(1, 0)
                    temp1(1, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(1, 1)
                    temp1(1, 1) = tv2
                End If

                If TIPO = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                    If Z < 0 Then Z = temp1(1, 0)
                ElseIf TIPO = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If

            Else

                Dim findZV, dfdz, zant As Double
                If TIPO = "V" Then Z = 1 Else Z = 0.05
                Do
                    findZV = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                    dfdz = 3 * coeff(3) * Z ^ 2 + 2 * coeff(2) * Z + coeff(1)
                    zant = Z
                    Z = Z - findZV / dfdz
                    If Z < 0 Then Z = 1
                Loop Until Math.Abs(findZV) < 0.0001 Or Double.IsNaN(Z)


            End If

            Dim V = (Z * R * T / P) ' m3/mol

            Dim tmp1 = MMm / V / 1000

            Dim aux1 = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 = 0.0#
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vz(i) * VKij(i, j) + Vz(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    aux2 += Vz(i) * Vz(j) * (1 - term) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadT = aux1 * aux2
            Dim d2adt2 = R / 4 * (0.45724 / T) ^ 0.5 * (1 / T) * aux2
            'Dim d2adt2 = 0.169049 * R / (T ^ (3 / 2))

            Dim dP_dT_V = R / (V - bm) - dadT / (V ^ 2 + 2 * bm * V - bm ^ 2)

            Dim dV_dT_P = dP_dT_V / (R * T / (V - bm) ^ 2 - am * (2 * V + 2 * bm) / (V * (V + bm) + bm * (V - bm)) ^ 2)

            Dim dP_dV_T = -R * T * (V - bm) ^ -2 - am * (V ^ 2 + 2 * bm * V - bm ^ 2) ^ -2 * (2 * V + 2 * bm)

            Dim d2P_dT2 = -1 / (V ^ 2 + 2 * bm * V - bm ^ 2) * d2adt2

            Dim var = (bm + V) / (2 ^ 0.5 * bm)

            Dim Int_d2P_dT2_V_dV = -d2adt2 * Math.Log((-(2 ^ 0.5) * bm + bm + V) / ((2 ^ 0.5) * bm + bm + V)) / (8 ^ 0.5 * bm)

            Dim Cpm_ig = 0.0#
            i = 0
            Do
                Cpm_ig += Vzmass(i) * VCpig(i) * MMm
                i += 1
            Loop Until i = n + 1

            Dim Cv = T * Int_d2P_dT2_V_dV + Cpm_ig - 2 * R - T * dP_dT_V ^ 2 / dP_dV_T
            'Dim Cp = Cpm_ig + T * Int_d2P_dT2_V_dV - T * dP_dT_V ^ 2 / dP_dV_T - R
            Dim Cp = Cpm_ig - R + T * dP_dT_V * dV_dT_P - T * d2adt2 / (8 ^ 0.5 * bm) * Math.Log((V + (1 - 2 ^ 0.5) * bm) / (V + (1 + 2 ^ 0.5) * bm))

            Dim Cp_Cv2 = Cp / Cv

            Dim Cp_Cv = 1 - (T * dP_dT_V ^ 2 / dP_dV_T) / (Cpm_ig - R + T * Int_d2P_dT2_V_dV)
            'Cv = Cp / Cp_Cv

            Dim tmp(2) As Double
            tmp(0) = Cp_Cv2
            tmp(1) = Cp / MMm
            tmp(2) = Cv / MMm

            CpCvR = tmp

        End Function

        Function bi(ByVal omega As Double, ByVal Tc As Double, ByVal Pc As Double) As Double

            Return omega * 8.314 * Tc / Pc

        End Function

        Function Z_PR(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij(,) As Double, ByVal VKij2(,) As Double,
                          ByVal Vk1 As Double(), ByVal Vk2 As Double(), ByVal Vk3 As Double(),
                          ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal TIPO As String)

            Dim ai(), bi(), ci(), aml2(), amv2() As Double
            Dim n, R, coeff(3), tmp() As Double
            Dim Tc(), Pc(), w(), alpha(), Vant(0, 4), m(), a(,), b(,), Tr() As Double

            n = Vx.Length - 1

            ReDim ai(n), bi(n), ci(n), tmp(n + 1), a(n, n), b(n, n)
            ReDim aml2(n), amv2(n)
            ReDim Tc(n), Pc(n), w(n), alpha(n), m(n), Tr(n)

            R = 8.314

            Dim i, j, k As Integer
            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vk1(i) * Vk2(i) * Vk3(i) <> 0.0 Then
                    ci(i) = (0.378893 + 1.4897153 * w(i) - 0.17131848 * w(i) ^ 2 + 0.0196544 * w(i) ^ 3) + (Vk1(i) + Vk2(i) * (Vk3(i) - Tr(i)) * (1 - Tr(i) ^ 0.5)) * (1 + Tr(i) ^ 0.5) * (0.7 - Tr(i))
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                Else
                    If w(i) <= 0.491 Then
                        ci(i) = 0.37464 + 1.5422 * w(i) - 0.26992 * w(i) ^ 2
                    Else
                        ci(i) = 0.379642 + 1.48503 * w(i) - 0.164423 * w(i) ^ 2 + 0.016666 * w(i) ^ 3
                    End If
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                End If
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vx(i) * VKij(i, j) + Vx(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - term)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim sum1(n), sum2(n), sum3(n), aml, tosum As Double
            i = 0
            Do
                sum1(i) = 0
                sum2(i) = 0
                sum3(i) = 0
                j = 0
                Do
                    If i <> j Then
                        tosum = Vx(i) * Vx(j) * (a(i, i) * a(j, j)) ^ 0.5 * VKij(i, j) * VKij2(j, i) * ((1 - Vx(i)) * VKij(i, j) - Vx(j) * VKij2(j, i)) / (Vx(i) * VKij(i, j) + Vx(j) * VKij(j, i)) ^ 2
                        If Not Double.IsNaN(tosum) Then sum2(i) += tosum
                    End If
                    k = 0
                    Do
                        If i <> j And k > j And k <> i Then
                            sum3(i) += Vx(j) * Vx(k) * (-a(j, j) * a(k, k)) ^ 0.5 * (VKij(j, k) * VKij2(k, j)) / (Vx(j) * VKij(j, k) + Vx(k) * VKij2(k, j))
                        End If
                        k += 1
                    Loop Until k = n + 1
                    sum1(i) += Vx(j) * a(i, j)
                    j += 1
                Loop Until j = n + 1
                i += 1
            Loop Until i = n + 1


            i = 0
            aml = 0
            Do
                aml2(i) = 0
                j = 0
                Do
                    aml = aml + Vx(i) * Vx(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                aml2(i) += sum1(i) + sum2(i) + sum3(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bml = 0.0#
            Do
                bml = bml + Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            Dim AG = aml * P / (R * T) ^ 2
            Dim BG = bml * P / (R * T)

            coeff(0) = -AG * BG + BG ^ 2 + BG ^ 3
            coeff(1) = AG - 3 * BG ^ 2 - 2 * BG
            coeff(2) = BG - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0.0#
            Dim ZV, tv2

            If Not IsNumeric(temp1) Then

                If temp1(0, 0) > temp1(1, 0) Then
                    tv = temp1(1, 0)
                    temp1(1, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(1, 1)
                    temp1(1, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(0, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(1, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(1, 0)
                    temp1(1, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(1, 1)
                    temp1(1, 1) = tv2
                End If

                ZV = temp1(2, 0)
                If temp1(2, 1) <> 0 Then
                    ZV = temp1(1, 0)
                    If temp1(1, 1) <> 0 Then
                        ZV = temp1(0, 0)
                    End If
                End If

            Else

                Dim findZV, dfdz, zant As Double
                If TIPO = "V" Then ZV = 1 Else ZV = 0.05
                Do
                    findZV = coeff(3) * ZV ^ 3 + coeff(2) * ZV ^ 2 + coeff(1) * ZV + coeff(0)
                    dfdz = 3 * coeff(3) * ZV ^ 2 + 2 * coeff(2) * ZV + coeff(1)
                    zant = ZV
                    ZV = ZV - findZV / dfdz
                    If ZV < 0 Then ZV = 1
                Loop Until Math.Abs(findZV) < 0.0001 Or Double.IsNaN(ZV)

                Return ZV

            End If

            Z_PR = 0
            If TIPO = "L" Then
                Z_PR = temp1(0, 0)
            ElseIf TIPO = "V" Then
                Z_PR = temp1(2, 0)
            End If

        End Function

        Function H_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double,
                          ByVal Vz As Double(), ByVal VKij(,) As Double, ByVal VKij2(,) As Double,
                          ByVal Vk1 As Double(), ByVal Vk2 As Double(), ByVal Vk3 As Double(),
                          ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(),
                          ByVal VMM As Double(), ByVal Hid As Double) As Double

            Dim ai(), bi(), ci() As Double
            Dim n, R As Double
            Dim Tc(), Pc(), Vc(), w(), Zc(), alpha(), m(), a(,), b(,), Z, Tr() As Double
            Dim i, j, dadT

            n = Vz.Length - 1

            ReDim ai(n), bi(n), ci(n), a(n, n), b(n, n)
            ReDim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), m(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vk1(i) * Vk2(i) * Vk3(i) <> 0.0 Then
                    ci(i) = (0.378893 + 1.4897153 * w(i) - 0.17131848 * w(i) ^ 2 + 0.0196544 * w(i) ^ 3) + (Vk1(i) + Vk2(i) * (Vk3(i) - Tr(i)) * (1 - Tr(i) ^ 0.5)) * (1 + Tr(i) ^ 0.5) * (0.7 - Tr(i))
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                Else
                    If w(i) <= 0.491 Then
                        ci(i) = 0.37464 + 1.5422 * w(i) - 0.26992 * w(i) ^ 2
                    Else
                        ci(i) = 0.379642 + 1.48503 * w(i) - 0.164423 * w(i) ^ 2 + 0.016666 * w(i) ^ 3
                    End If
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                End If
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vz(i) * VKij(i, j) + Vz(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - term)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim am = 0.0#
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bm = 0.0#
            Do
                bm = bm + Vz(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            Dim AG1 = am * P / (R * T) ^ 2
            Dim BG1 = bm * P / (R * T)

            Dim coeff(3) As Double

            coeff(0) = -AG1 * BG1 + BG1 ^ 2 + BG1 ^ 3
            coeff(1) = AG1 - 3 * BG1 ^ 2 - 2 * BG1
            coeff(2) = BG1 - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv
            Dim tv2

            If Not IsNumeric(temp1) Then

                If temp1(0, 0) > temp1(1, 0) Then
                    tv = temp1(1, 0)
                    tv2 = temp1(1, 1)
                    temp1(1, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    temp1(1, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(0, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(1, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(1, 0)
                    temp1(1, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(1, 1)
                    temp1(1, 1) = tv2
                End If

                If TIPO = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                    If Z < 0 Then Z = temp1(1, 0)
                ElseIf TIPO = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If

            Else

                Dim findZV, dfdz, zant As Double
                If TIPO = "V" Then Z = 1 Else Z = 0.05
                Do
                    findZV = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                    dfdz = 3 * coeff(3) * Z ^ 2 + 2 * coeff(2) * Z + coeff(1)
                    zant = Z
                    Z = Z - findZV / dfdz
                    If Z < 0 Then Z = 1
                Loop Until Math.Abs(findZV) < 0.0001 Or Double.IsNaN(Z)


            End If

            Dim V = (Z * R * T / P) ' m3/mol

            Dim tmp1 = MMm / V / 1000

            Dim aux1 = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 = 0.0#
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vz(i) * VKij(i, j) + Vz(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    aux2 += Vz(i) * Vz(j) * (1 - term) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadT = aux1 * aux2

            Dim uu, ww As Double
            uu = 2
            ww = -1

            Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(Z)
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            H_PR_MIX = Hid + DHres / MMm '/ 1000

        End Function

        Function S_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double,
                          ByVal Vz As Double(), ByVal VKij(,) As Double, ByVal VKij2(,) As Double,
                          ByVal Vk1 As Double(), ByVal Vk2 As Double(), ByVal Vk3 As Double(),
                          ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(),
                          ByVal VMM As Array, ByVal Sid As Double) As Double

            Dim ai(), bi(), ci() As Double
            Dim n, R As Double
            Dim Tc(), Pc(), Vc(), w(), Zc(), alpha(), m(), a(,), b(,), Z, Tr() As Double
            Dim i, j, dadT

            n = Vz.Length - 1

            ReDim ai(n), bi(n), ci(n), a(n, n), b(n, n)
            ReDim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), m(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vk1(i) * Vk2(i) * Vk3(i) <> 0.0 Then
                    ci(i) = (0.378893 + 1.4897153 * w(i) - 0.17131848 * w(i) ^ 2 + 0.0196544 * w(i) ^ 3) + (Vk1(i) + Vk2(i) * (Vk3(i) - Tr(i)) * (1 - Tr(i) ^ 0.5)) * (1 + Tr(i) ^ 0.5) * (0.7 - Tr(i))
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                Else
                    If w(i) <= 0.491 Then
                        ci(i) = 0.37464 + 1.5422 * w(i) - 0.26992 * w(i) ^ 2
                    Else
                        ci(i) = 0.379642 + 1.48503 * w(i) - 0.164423 * w(i) ^ 2 + 0.016666 * w(i) ^ 3
                    End If
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                End If
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vz(i) * VKij(i, j) + Vz(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - term)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim am = 0.0#
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bm = 0.0#
            Do
                bm = bm + Vz(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            'Dim dadT = 

            Dim AG1 = am * P / (R * T) ^ 2
            Dim BG1 = bm * P / (R * T)

            Dim coeff(3) As Double

            coeff(0) = -AG1 * BG1 + BG1 ^ 2 + BG1 ^ 3
            coeff(1) = AG1 - 3 * BG1 ^ 2 - 2 * BG1
            coeff(2) = BG1 - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0.0#
            Dim tv2 = 0.0#
            If Not IsNumeric(temp1) Then

                If temp1(0, 0) > temp1(1, 0) Then
                    tv = temp1(1, 0)
                    tv2 = temp1(1, 1)
                    temp1(1, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    temp1(1, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(0, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(1, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(1, 0)
                    temp1(1, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(1, 1)
                    temp1(1, 1) = tv2
                End If

                If TIPO = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                    If Z < 0 Then Z = temp1(1, 0)
                ElseIf TIPO = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If
            Else

                Dim findZV, dfdz, zant As Double
                If TIPO = "V" Then Z = 1 Else Z = 0.05
                Do
                    findZV = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                    dfdz = 3 * coeff(3) * Z ^ 2 + 2 * coeff(2) * Z + coeff(1)
                    zant = Z
                    Z = Z - findZV / dfdz
                    If Z < 0 Then Z = 1
                Loop Until Math.Abs(findZV) < 0.0001 Or Double.IsNaN(Z)

            End If

            Dim V = (Z * R * T / P) ' m3/mol

            Dim tmp1 = MMm / V / 1000

            Dim aux1 = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 = 0.0#
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vz(i) * VKij(i, j) + Vz(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    aux2 += Vz(i) * Vz(j) * (1 - term) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadT = aux1 * aux2

            Dim V0 As Double = R * 298.15 / 101325
            'Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V / V0) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))

            S_PR_MIX = Sid + DSres / MMm '/ 1000

        End Function

        Function G_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Array, ByVal VKij As Object, ByVal VKij2 As Object, ByVal Vk1 As Object, ByVal Vk2 As Object, ByVal Vk3 As Object, ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, ByVal VMM As Array, ByVal Sid As Double, ByVal Hid As Double) As Double

            Dim h As Double = H_PR_MIX(TIPO, T, P, Vz, VKij, VKij2, Vk1, Vk2, Vk3, VTc, VPc, Vw, VMM, Hid)
            Dim s As Double = S_PR_MIX(TIPO, T, P, Vz, VKij, VKij2, Vk1, Vk2, Vk3, VTc, VPc, Vw, VMM, Sid)

            Return h - T * s

        End Function

        Function ESTIMAR_V(ByVal Vz As Object, ByVal KI As Object) As Double

            Dim n = Vz.Length - 1

            Dim i As Integer

            Dim Vinf, Vsup As Double

            Dim fV, fV_inf, nsub, delta_V As Double

            Vinf = 0
            Vsup = 1

            nsub = 20

            delta_V = (Vsup - Vinf) / nsub

            i = 0
            Do
                i = i + 1
                fV = OF_V(Vinf, Vz, KI)
                Vinf = Vinf + delta_V
                fV_inf = OF_V(Vinf, Vz, KI)
            Loop Until fV * fV_inf < 0 Or Vinf > 1
            Vsup = Vinf
            Vinf = Vinf - delta_V

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Vinf
            bbb = Vsup
            ccc = Vsup

            faa = OF_V(aaa, Vz, KI)
            fbb = OF_V(bbb, Vz, KI)
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
                tol11 = 0.000001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
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
                fbb = OF_V(bbb, Vz, KI)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final2:     'bbb = -100

Final3:

            Return bbb

        End Function

        Function OF_V(ByVal V As Double, ByVal Vz As Object, ByVal KI As Object) As Double

            Dim i As Integer
            Dim n = Vz.Length - 1
            Dim result As Double

            i = 0
            Do
                result += Vz(i) * (1 - KI(i)) / (1 - V + V * KI(i))
                i = i + 1
            Loop Until i = n + 1

            Return result

        End Function

        Function GeneratePseudoRoot(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VKij2, ByVal Vk1, ByVal Vk2, ByVal Vk3, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VTb, ByVal TIPO)

            Dim ai(), bi(), ci(), aml2(), amv2() As Double
            Dim n, R, coeff(3), tmp() As Double
            Dim Tc(), Pc(), W(), alpha(), Vant(0, 4), m(), a(,), b(,), Tr() As Double
            Dim beta As Double
            Dim criterioOK As Boolean = False
            Dim hbcIndex, counter As Integer
            Dim soma_x As Double
            Dim ZV As Double
            Dim AG, BG, aml, bml As Double

            n = Vx.Length - 1

            ReDim ai(n), bi(n), ci(n), tmp(n + 1), a(n, n), b(n, n)
            ReDim aml2(n), amv2(n)
            ReDim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n)

            R = 8.314

            Dim i, j, k As Integer
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
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                ci(i) = (0.378893 + 1.4897153 * W(i) - 0.17131848 * W(i) ^ 2 + 0.0196544 * W(i) ^ 3) + (Vk1(i) + Vk2(i) * (Vk3(i) - Tr(i)) * (1 - Tr(i) ^ 0.5)) * (0.7 - Tr(i))
                alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            counter = 0
            Do

                i = 0
                Do
                    j = 0
                    Do
                        Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vx(i) * VKij(i, j) + Vx(j) * VKij2(j, i))
                        If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                        a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - term)
                        j = j + 1
                    Loop Until j = n + 1
                    i = i + 1
                Loop Until i = n + 1

                Dim sum1(n), sum2(n), sum3(n) As Double
                i = 0
                Do
                    sum1(i) = 0
                    sum2(i) = 0
                    sum3(i) = 0
                    j = 0
                    Do
                        If i <> j Then
                            sum2(i) += Vx(i) * Vx(j) * (a(i, i) * a(j, j)) ^ 0.5 * VKij(i, j) * VKij2(j, i) * ((1 - Vx(i)) * VKij(i, j) - Vx(j) * VKij2(j, i)) / (Vx(i) * VKij(i, j) + Vx(j) * VKij(j, i)) ^ 2
                        End If
                        k = 0
                        Do
                            If i <> j And k > j And k <> i Then
                                sum3(i) += Vx(j) * Vx(k) * (-a(j, j) * a(k, k)) ^ 0.5 * (VKij(j, k) * VKij2(k, j)) / (Vx(j) * VKij(j, k) + Vx(k) * VKij2(k, j))
                            End If
                            k += 1
                        Loop Until k = n + 1
                        sum1(i) += Vx(j) * a(i, j)
                        j += 1
                    Loop Until j = n + 1
                    i += 1
                Loop Until i = n + 1


                i = 0
                aml = 0
                Do
                    aml2(i) = 0
                    j = 0
                    Do
                        aml = aml + Vx(i) * Vx(j) * a(i, j)
                        j = j + 1
                    Loop Until j = n + 1
                    aml2(i) += sum1(i) + sum2(i) + sum3(i)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                bml = 0
                Do
                    bml = bml + Vx(i) * bi(i)
                    i = i + 1
                Loop Until i = n + 1

                AG = aml * P / (R * T) ^ 2
                BG = bml * P / (R * T)

                coeff(0) = -AG * BG + BG ^ 2 + BG ^ 3
                coeff(1) = AG - 3 * BG ^ 2 - 2 * BG
                coeff(2) = BG - 1
                coeff(3) = 1

                Dim temp1 = Poly_Roots(coeff)
                Dim tv = 0.0#
                Dim tv2 = 0.0#

                If Not IsNumeric(temp1) Then

                    If temp1(0, 0) > temp1(1, 0) Then
                        tv = temp1(1, 0)
                        temp1(1, 0) = temp1(0, 0)
                        temp1(0, 0) = tv
                        tv2 = temp1(1, 1)
                        temp1(1, 1) = temp1(0, 1)
                        temp1(0, 1) = tv2
                    End If
                    If temp1(0, 0) > temp1(2, 0) Then
                        tv = temp1(2, 0)
                        temp1(2, 0) = temp1(0, 0)
                        temp1(0, 0) = tv
                        tv2 = temp1(2, 1)
                        temp1(2, 1) = temp1(0, 1)
                        temp1(0, 1) = tv2
                    End If
                    If temp1(1, 0) > temp1(2, 0) Then
                        tv = temp1(2, 0)
                        temp1(2, 0) = temp1(1, 0)
                        temp1(1, 0) = tv
                        tv2 = temp1(2, 1)
                        temp1(2, 1) = temp1(1, 1)
                        temp1(1, 1) = tv2
                    End If

                    ZV = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        ZV = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            ZV = temp1(0, 0)
                        End If
                    End If

                    ZV = 0
                    If TIPO = "L" Then
                        ZV = temp1(0, 0)
                    ElseIf TIPO = "V" Then
                        ZV = temp1(2, 0)
                    End If

                Else

                    Dim findZV, dfdz, zant As Double
                    If TIPO = "V" Then ZV = 1 Else ZV = 0.05
                    Do
                        findZV = coeff(3) * ZV ^ 3 + coeff(2) * ZV ^ 2 + coeff(1) * ZV + coeff(0)
                        dfdz = 3 * coeff(3) * ZV ^ 2 + 2 * coeff(2) * ZV + coeff(1)
                        zant = ZV
                        ZV = ZV - findZV / dfdz
                        If ZV < 0 Then ZV = 1
                    Loop Until Math.Abs(findZV) < 0.0001 Or Double.IsNaN(ZV)

                End If


                beta = 1 / P * (1 - (BG * ZV ^ 2 + AG * ZV - 6 * BG ^ 2 * ZV - 2 * BG * ZV - 2 * AG * BG + 2 * BG ^ 2 + 2 * BG) / (ZV * (3 * ZV ^ 2 - 2 * ZV + 2 * BG * ZV + AG - 3 * BG ^ 2 - 2 * BG)))

                If TIPO = "L" Then
                    If beta < 0.005 / 101325 Then criterioOK = True
                Else
                    If beta < 3 / (P / 101325) And beta > 0.9 / (P / 101325) Then criterioOK = True
                    If ZV > 0.8 Then criterioOK = True
                End If

                If Not criterioOK Then
                    If TIPO = "L" Then
                        'verificar qual componente e o mais pesado
                        i = 1
                        'hbcindex e o indice do componente mais pesado
                        hbcIndex = i
                        i = 0
                        Do
                            If VTb(i) > VTb(hbcIndex) And Vx(i) <> 0 Then
                                hbcIndex = i
                            End If
                            i += 1
                        Loop Until i = n + 1
                        'aumenta-se a fracao molar do componente hbc...
                        Vx(hbcIndex) += 1
                        'e em seguida normaliza-se a composicao.
                        i = 0
                        soma_x = 0
                        Do
                            soma_x = soma_x + Vx(i)
                            i = i + 1
                        Loop Until i = n + 1
                        i = 0
                        Do
                            Vx(i) = Vx(i) / soma_x
                            i = i + 1
                        Loop Until i = n + 1
                    Else
                        P = P * 0.75
                    End If
                End If

                If P <= 1000 Then
                    Return Nothing
                End If

                counter += 1

            Loop Until criterioOK = True Or counter > 50

            Return New Object() {ZV, AG, BG, aml, bml}

        End Function

        Function CalcLnFug(ByVal T As Double, ByVal P As Double, ByVal Vx() As Double,
                           ByVal VKij(,) As Double, ByVal VKij2(,) As Double,
                          ByVal Vk1 As Double(), ByVal Vk2 As Double(), ByVal Vk3 As Double(),
                          ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal VTb As Double(), ByVal TIPO As String)

            Dim n, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim criterioOK As Boolean = False
            Dim ZV As Double
            Dim AG, BG, aml, bml As Double
            Dim t1, t2, t3, t4, t5 As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), ci(n), tmp(n + 1), a(n, n), b(n, n)
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n)
            Dim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n)
            Dim Zcalc As Double

            R = 8.314

            Dim i, j, k As Integer
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
                If Vk1(i) * Vk2(i) * Vk3(i) <> 0.0 Then
                    ci(i) = (0.378893 + 1.4897153 * W(i) - 0.17131848 * W(i) ^ 2 + 0.0196544 * W(i) ^ 3) + (Vk1(i) + Vk2(i) * (Vk3(i) - Tr(i)) * (1 - Tr(i) ^ 0.5)) * (1 + Tr(i) ^ 0.5) * (0.7 - Tr(i))
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                Else
                    If W(i) <= 0.491 Then
                        ci(i) = 0.37464 + 1.5422 * W(i) - 0.26992 * W(i) ^ 2
                    Else
                        ci(i) = 0.379642 + 1.48503 * W(i) - 0.164423 * W(i) ^ 2 + 0.016666 * W(i) ^ 3
                    End If
                    alpha(i) = (1 + ci(i) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                End If
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    Dim term As Double = VKij(i, j) * VKij2(j, i) / (Vx(i) * VKij(i, j) + Vx(j) * VKij2(j, i))
                    If Double.IsNaN(term) Or Double.IsInfinity(term) Then term = 0.0#
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - term)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim sum1(n), sum2(n), sum3(n), tosum As Double
            i = 0
            Do
                sum1(i) = 0
                sum2(i) = 0
                sum3(i) = 0
                j = 0
                Do
                    If i <> j Then
                        tosum = Vx(i) * Vx(j) * (a(i, i) * a(j, j)) ^ 0.5 * VKij(i, j) * VKij2(j, i) * ((1 - Vx(i)) * VKij(i, j) - Vx(j) * VKij2(j, i)) / (Vx(i) * VKij(i, j) + Vx(j) * VKij(j, i)) ^ 2
                        If Not Double.IsNaN(tosum) Then sum2(i) += tosum
                    End If
                    k = 0
                    Do
                        If i <> j And k > j And k <> i Then
                            sum3(i) += Vx(j) * Vx(k) * (-a(j, j) * a(k, k)) ^ 0.5 * (VKij(j, k) * VKij2(k, j)) / (Vx(j) * VKij(j, k) + Vx(k) * VKij2(k, j))
                        End If
                        k += 1
                    Loop Until k = n + 1
                    sum1(i) += Vx(j) * a(i, j)
                    j += 1
                Loop Until j = n + 1
                i += 1
            Loop Until i = n + 1


            i = 0
            aml = 0
            Do
                aml2(i) = 0
                j = 0
                Do
                    aml = aml + Vx(i) * Vx(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                If Not Double.IsNaN(sum3(i)) Then aml2(i) += sum1(i) + sum2(i) + sum3(i) Else aml2(i) += sum1(i) + sum2(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            bml = 0
            Do
                bml = bml + Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            AG = aml * P / (R * T) ^ 2
            BG = bml * P / (R * T)

            coeff(0) = -AG * BG + BG ^ 2 + BG ^ 3
            coeff(1) = AG - 3 * BG ^ 2 - 2 * BG
            coeff(2) = BG - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0
            Dim tv2

            If Not IsNumeric(temp1) Then

                If temp1(0, 0) > temp1(1, 0) Then
                    tv = temp1(1, 0)
                    temp1(1, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(1, 1)
                    temp1(1, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(0, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(1, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(1, 0)
                    temp1(1, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(1, 1)
                    temp1(1, 1) = tv2
                End If

                ZV = temp1(2, 0)
                If temp1(2, 1) <> 0 Then
                    ZV = temp1(1, 0)
                    If temp1(1, 1) <> 0 Then
                        ZV = temp1(0, 0)
                    End If
                End If

                ZV = 0
                If TIPO = "L" Then
                    ZV = temp1(0, 0)
                ElseIf TIPO = "V" Then
                    ZV = temp1(2, 0)
                End If

            Else

                Dim findZV, dfdz, zant As Double
                If TIPO = "V" Then ZV = 1 Else ZV = 0.05
                Do
                    findZV = coeff(3) * ZV ^ 3 + coeff(2) * ZV ^ 2 + coeff(1) * ZV + coeff(0)
                    dfdz = 3 * coeff(3) * ZV ^ 2 + 2 * coeff(2) * ZV + coeff(1)
                    zant = ZV
                    ZV = ZV - findZV / dfdz
                    If ZV < 0 Then ZV = 1
                Loop Until Math.Abs(findZV) < 0.0001 Or Double.IsNaN(ZV)

            End If

            Dim Pcorr As Double = P
            Dim ZP As Double() = ThermoPlugs.PR.CheckRoot(ZV, aml, bml, P, T, TIPO)
            ZV = ZP(0)
            Pcorr = ZP(1)

            If TIPO = "L" Then
                Zcalc = ZV
                ' CALCULO DO COEFICIENTE DE FUGACIDADE DA Phase LIQUIDA
                i = 0
                Do
                    t1 = bi(i) * (Zcalc - 1) / bml
                    t2 = -Math.Log(Zcalc - BG)
                    t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                    t4 = Math.Log((Zcalc + (1 + 2 ^ 0.5) * BG) / (Zcalc + (1 - 2 ^ 0.5) * BG))
                    t5 = 2 * 2 ^ 0.5 * BG
                    LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                    LN_CF(i) = LN_CF(i) + Math.Log(Pcorr / P)
                    i = i + 1
                Loop Until i = n + 1
                Return LN_CF
            Else
                Zcalc = ZV
                ' CALCULO DO COEFICIENTE DE FUGACIDADE DA Phase VAPOR
                i = 0
                Do
                    t1 = bi(i) * (Zcalc - 1) / bml
                    t2 = -Math.Log(Zcalc - BG)
                    t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                    t4 = Math.Log((Zcalc + (1 + 2 ^ 0.5) * BG) / (Zcalc + (1 - 2 ^ 0.5) * BG))
                    t5 = 2 * 2 ^ 0.5 * BG
                    LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                    LN_CF(i) = LN_CF(i) + Math.Log(Pcorr / P)
                    i = i + 1
                Loop Until i = n + 1
                Return LN_CF
            End If

        End Function

        Function CalcPartialVolume(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VKij2, ByVal Vk1, ByVal Vk2, ByVal Vk3, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VTb, ByVal TIPO, ByVal deltaP)

            Dim lnfug1, lnfug2 As Object
            Dim P1, P2 As Double
            P1 = P
            P2 = P + deltaP

            lnfug1 = Me.CalcLnFug(T, P1, Vx, VKij, VKij2, Vk1, Vk2, Vk3, VTc, VPc, Vw, VTb, TIPO)
            lnfug2 = Me.CalcLnFug(T, P2, Vx, VKij, VKij2, Vk1, Vk2, Vk3, VTc, VPc, Vw, VTb, TIPO)

            Dim i As Integer
            Dim n As Integer = UBound(lnfug1)

            Dim partvol(n) As Double

            i = 0
            For i = 0 To n
                partvol(i) = (Math.Log(Math.Exp(lnfug2(i)) * Vx(i) * P2) - Math.Log(Math.Exp(lnfug1(i)) * Vx(i) * P1)) / deltaP * (8.314 * T) 'm3/mol
                If Double.IsNaN(partvol(i)) Then partvol(i) = 0
            Next

            Return partvol

        End Function

        Function ESTIMAR_RhoLim(ByVal am As Double, ByVal bm As Double, ByVal T As Double, ByVal P As Double) As Double

            Dim i As Integer

            Dim rinf, rsup As Double

            Dim fr, fr_inf, nsub, delta_r As Double

            rinf = 0
            rsup = P / (8.314 * T)

            nsub = 10

            delta_r = (rsup - rinf) / nsub

            i = 0
            Do
                i = i + 1
                fr = OF_Rho(rinf, am, bm, T)
                rinf = rinf + delta_r
                fr_inf = OF_Rho(rinf, am, bm, T)
            Loop Until fr * fr_inf < 0 Or i = 11
            If i = 11 Then GoTo Final2
            rsup = rinf
            rinf = rinf - delta_r

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = rinf
            bbb = rsup
            ccc = rsup

            faa = OF_Rho(aaa, am, bm, T)
            fbb = OF_Rho(bbb, am, bm, T)
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
                tol11 = 0.000001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
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
                fbb = OF_Rho(bbb, am, bm, T)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final2:     bbb = -100

Final3:

            Return bbb

        End Function

        Function OF_Rho(ByVal rho As Double, ByVal aml As Double, ByVal bml As Double, ByVal T As Double) As Double

            Dim R As Double = 8.314
            Return 0.1 * 8.314 * T - _
                        bml * rho * R * T * (1 - bml * rho) ^ -2 + R * T * (1 - bml * rho) ^ -1 + _
                        aml * rho ^ 2 * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -2 * (2 * bml - 2 * bml ^ 2 * rho) + _
                        2 * aml * rho * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -1

        End Function

    End Class

End Namespace


