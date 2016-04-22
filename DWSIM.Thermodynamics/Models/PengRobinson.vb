'    Peng-Robinson Property Package 
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
Imports FileHelpers
Imports System.Linq
Imports DWSIM.Thermodynamics.PropertyPackages.ThermoPlugs.PR
Imports System.Threading.Tasks
Imports DWSIM.MathOps.MathEx.PolySolve

Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(";")> <IgnoreFirst()> <System.Serializable()> _
    Public Class PR_IPData

        Implements ICloneable

        Public ID1 As Integer = -1
        Public ID2 As Integer = -1
        Public kij As Double = 0.0#
        Public comment As String = ""

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New PR_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .kij = Me.kij
                .comment = Me.comment
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class PengRobinson

        Dim m_pr As New PropertyPackages.Auxiliary.PROPS
        Private _ip As Dictionary(Of String, Dictionary(Of String, PR_IPData))

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, PR_IPData))
            Get
                Return _ip
            End Get
        End Property

        Sub New()
            _ip = New Dictionary(Of String, Dictionary(Of String, PR_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim prip As PR_IPData
            Dim pripc() As PR_IPData
            Dim fh1 As New FileHelperEngine(Of PR_IPData)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.pr_ip.dat")
                Using t As New IO.StreamReader(filestr)
                    pripc = fh1.ReadStream(t)
                End Using
            End Using

            Dim csdb As New ChemSepHelper.ChemSepIDConverter
            For Each prip In pripc
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(prip.ID1)) Then
                    If Me.InteractionParameters(csdb.GetCSName(prip.ID1)).ContainsKey(csdb.GetCSName(prip.ID2)) Then
                    Else
                        Me.InteractionParameters(csdb.GetCSName(prip.ID1)).Add(csdb.GetCSName(prip.ID2), prip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(prip.ID1), New Dictionary(Of String, PR_IPData))
                    Me.InteractionParameters(csdb.GetCSName(prip.ID1)).Add(csdb.GetCSName(prip.ID2), prip.Clone)
                End If
            Next
            For Each prip In pripc
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(prip.ID1)) Then
                    If Me.InteractionParameters(csdb.GetDWSIMName(prip.ID1)).ContainsKey(csdb.GetDWSIMName(prip.ID2)) Then
                    Else
                        Me.InteractionParameters(csdb.GetDWSIMName(prip.ID1)).Add(csdb.GetDWSIMName(prip.ID2), prip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(prip.ID1), New Dictionary(Of String, PR_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(prip.ID1)).Add(csdb.GetDWSIMName(prip.ID2), prip.Clone)
                End If
            Next
            prip = Nothing
            pripc = Nothing
            fh1 = Nothing
        End Sub

        Function Zc1(ByVal w As Double) As Double

            Zc1 = 0.291 - 0.08 * w

        End Function

        Function bi(ByVal omega As Double, ByVal Tc As Double, ByVal Pc As Double) As Double

            Return omega * 8.314 * Tc / Pc

        End Function

        Function Z_PR(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal TIPO As String) As Double

            Calculator.WriteToConsole("PR cubic equation root finder (Z) for T = " & T & " K, P = " & P & " Pa and Phase = " & TIPO, 3)
            Calculator.WriteToConsole("Mole fractions: " & DirectCast(Vx, Double()).ToArrayString, 3)

            Dim n, R, coeff(3) As Double

            n = UBound(Vx)

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n) As Double
            Dim aml2(n), amv2(n) As Double
            Dim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n) As Double

            R = 8.314

            Dim i As Integer
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
                If Tc(i) > 0.0# Then
                    alpha(i) = (1 + (0.37464 + 1.54226 * W(i) - 0.26992 * W(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                    ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                    bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            a = Calc_SUM1(n, ai, VKij)

            Dim tmpa As Object = Calc_SUM2(n, Vx, a)

            aml2 = tmpa(0)
            Dim aml As Double = tmpa(1)

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
            Dim ZV, tv2 As Double

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

            Z_PR = 0
            If TIPO = "L" Then
                Z_PR = temp1(0, 0)
            ElseIf TIPO = "V" Then
                Z_PR = temp1(2, 0)
            End If

            Calculator.WriteToConsole("Result: Z = " & Z_PR, 3)

        End Function

        Function H_PR(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Tc As Double, ByVal Pc As Double, ByVal w As Double, ByVal MM As Double, Optional ByVal ZRa As Double = 0) As Double

            Dim R As Double
            Dim a, c As Double

            R = 8.314

            a = 0.45724 * R ^ 2 * Tc ^ 2 / Pc
            c = 0.37464 + 1.54226 * w - 0.26992 * w ^ 2

            Dim alpha, ai, bi As Double

            alpha = (1 + c * (1 - (T / Tc) ^ 0.5)) ^ 2
            ai = a * alpha
            bi = 0.0778 * R * Tc / Pc

            Dim dadT = -a / T * (1 + c * (1 - (T / Tc) ^ 0.5)) * (c * (T / Tc) ^ 0.5)

            Dim AG1 As Double = ai * P / (R * T) ^ 2
            Dim BG1 As Double = bi * P / (R * T)

            Dim coeff(3) As Double

            coeff(0) = -AG1 * BG1 + BG1 ^ 2 + BG1 ^ 3
            coeff(1) = AG1 - 3 * BG1 ^ 2 - 2 * BG1
            coeff(2) = BG1 - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0.0#

            If temp1(0, 0) > temp1(1, 0) Then
                tv = temp1(1, 0)
                temp1(1, 0) = temp1(0, 0)
                temp1(0, 0) = tv
            End If
            If temp1(0, 0) > temp1(2, 0) Then
                tv = temp1(2, 0)
                temp1(2, 0) = temp1(0, 0)
                temp1(0, 0) = tv
            End If
            If temp1(1, 0) > temp1(2, 0) Then
                tv = temp1(2, 0)
                temp1(2, 0) = temp1(1, 0)
                temp1(1, 0) = tv
            End If

            Dim Z = 0.0#

            If TIPO = "L" Then
                Z = temp1(0, 0)
            ElseIf TIPO = "V" Then
                Z = temp1(2, 0)
            End If

            Dim V = 0.0#
            If TIPO = "L" Then

                V = (Z * R * T / P) ' m3/mol

            ElseIf TIPO = "V" Then

                V = (Z * R * T / P) ' m3/mol

            End If

            Dim tmp1 = MM / V / 1000

            Dim DHres = R * T * (Z - 1) + (T * dadT - ai) / (2 ^ 1.5 * bi) * Math.Log((Z + 2.44 * BG1) / (Z - 0.414 * BG1))

            H_PR = DHres / MM

        End Function

        Function H_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal VMM As Double(), ByVal Hid As Double) As Double

            Dim H As Double = 0.0#

            H = H_PR_MIX_CPU(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Hid)

            Return H

        End Function

        Function H_PR_MIX_CPU(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal Tc As Double(), ByVal Pc As Double(), ByVal w As Double(), ByVal VMM As Double(), ByVal Hid As Double) As Double

            Dim n As Integer, R, Z, dadT As Double
            Dim i As Integer

            n = UBound(Vz)

            Dim ai(n), bi(n), ci(n), a(n, n), b(n, n) As Double
            Dim Vc(n), Zc(n), alpha(n), m(n), Tr(n) As Double

            R = 8.314

            i = 0
            Do
                Tr(i) = T / Tc(i)
                i = i + 1
            Loop Until i = n + 1

            Dim MMm As Double = Vz.MultiplyY(VMM).SumY

            If Settings.EnableParallelProcessing Then
                Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = Settings.MaxDegreeOfParallelism, .TaskScheduler = Settings.AppTaskScheduler}
                Parallel.For(0, n + 1, poptions, Sub(ii)
                                                     alpha(ii) = (1 + (0.37464 + 1.54226 * w(ii) - 0.26992 * w(ii) ^ 2) * (1 - (T / Tc(ii)) ^ 0.5)) ^ 2
                                                     ai(ii) = 0.45724 * alpha(ii) * R ^ 2 * Tc(ii) ^ 2 / Pc(ii)
                                                     bi(ii) = 0.0778 * R * Tc(ii) / Pc(ii)
                                                     ci(ii) = 0.37464 + 1.54226 * w(ii) - 0.26992 * w(ii) ^ 2
                                                 End Sub)
            Else
                i = 0
                Do
                    alpha(i) = (1 + (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                    ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                    bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                    ci(i) = 0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2
                    i = i + 1
                Loop Until i = n + 1
            End If

            a = Calc_SUM1(n, ai, VKij)

            Dim tmpa As Object = Calc_SUM2(n, Vz, a)

            Dim am As Double = tmpa(1)

            Dim bm As Double = Vz.MultiplyY(bi).SumY

            Dim AG1 = am * P / (R * T) ^ 2
            Dim BG1 = bm * P / (R * T)

            Dim _zarray As List(Of Double) = CalcZ2(AG1, BG1)

            If TIPO = "L" Then
                Z = _zarray.Min
            ElseIf TIPO = "V" Then
                Z = _zarray.Max
            End If

            Dim V = (Z * R * T / P) ' m3/mol

            Dim tmp1 = MMm / V / 1000

            dadT = ThermoPlugs.PR.Calc_dadT(T, Vz, VKij, Tc, Pc, ai, ci)

            Dim uu, ww As Double
            uu = 2
            ww = -1

            Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(Z)
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            If MathEx.Common.Sum(Vz) = 0.0# Then
                Return 0.0#
            Else
                Return Hid + DHres / MMm
            End If

        End Function

        Function S_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Array, ByVal VKij As Object, ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, ByVal VMM As Array, ByVal Sid As Double) As Double

            Dim ai(), bi(), ci() As Double
            Dim n, R As Double
            Dim Tc(), Pc(), Vc(), w(), Zc(), alpha(), m(), a(,), b(,), Z, Tr() As Double
            Dim i, j, dadT

            n = UBound(Vz)

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
                alpha(i) = (1 + (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                ci(i) = 0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKij(i, j))
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

            dadT = ThermoPlugs.PR.Calc_dadT(T, Vz, VKij, Tc, Pc, ai, ci)

            Dim V0 As Double = R * 298.15 / 101325
            'Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V / V0) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))

            If MathEx.Common.Sum(Vz) = 0.0# Then
                S_PR_MIX = 0.0#
            Else
                S_PR_MIX = Sid + DSres / MMm '/ 1000
            End If

        End Function

        Function G_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Array, ByVal VKij As Object, ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, ByVal VMM As Array, ByVal Sid As Double, ByVal Hid As Double) As Double

            Dim h As Double = H_PR_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Hid)
            Dim s As Double = S_PR_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Sid)

            Return h - T * s

        End Function

        Function CalcLnFug(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VTb, ByVal TIPO) As Double()

            Dim n, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim beta As Double
            Dim criterioOK As Boolean = False
            Dim ZV As Double
            Dim AG, BG, aml, bml As Double
            Dim t1, t2, t3, t4, t5 As Double

            n = UBound(Vx)

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n) As Double
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n) As Double
            Dim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n) As Double
            Dim rho, rho0, rho_mc, Tmc, dPdrho, dPdrho_, Zcalc As Double

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
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKij(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                aml2(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            aml = 0
            Do
                j = 0
                Do
                    aml = aml + Vx(i) * Vx(j) * a(i, j)
                    aml2(i) = aml2(i) + Vx(j) * a(j, i)
                    j = j + 1
                Loop Until j = n + 1
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
            Dim tv2

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

            beta = 1 / P * (1 - (BG * ZV ^ 2 + AG * ZV - 6 * BG ^ 2 * ZV - 2 * BG * ZV - 2 * AG * BG + 2 * BG ^ 2 + 2 * BG) / (ZV * (3 * ZV ^ 2 - 2 * ZV + 2 * BG * ZV + AG - 3 * BG ^ 2 - 2 * BG)))

            rho0 = 1 / bml
            rho_mc = 0.2599 / bml
            Tmc = 0.20268 * aml / (R * bml)
            rho = P / (ZV * R * T)
            dPdrho_ = 0.1 * R * T
            dPdrho = bml * rho * R * T * (1 - bml * rho) ^ -2 + R * T * (1 - bml * rho) ^ -1 + _
                    aml * rho ^ 2 * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -2 * (2 * bml - 2 * bml ^ 2 * rho) + _
                    2 * aml * rho * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -1

            If TIPO = "L" Then
                Zcalc = ZV
                i = 0
                Do
                    t1 = bi(i) * (Zcalc - 1) / bml
                    t2 = -Math.Log(Zcalc - BG)
                    t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                    t4 = Math.Log((Zcalc + (1 + 2 ^ 0.5) * BG) / (Zcalc + (1 - 2 ^ 0.5) * BG))
                    t5 = 2 * 2 ^ 0.5 * BG
                    LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                    LN_CF(i) = LN_CF(i) '* Pcalc / P
                    i = i + 1
                Loop Until i = n + 1
                Return LN_CF
            Else
                Zcalc = ZV
                i = 0
                Do
                    t1 = bi(i) * (Zcalc - 1) / bml
                    t2 = -Math.Log(Zcalc - BG)
                    t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                    t4 = Math.Log((Zcalc + (1 + 2 ^ 0.5) * BG) / (Zcalc + (1 - 2 ^ 0.5) * BG))
                    t5 = 2 * 2 ^ 0.5 * BG
                    LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                    LN_CF(i) = LN_CF(i)
                    i = i + 1
                Loop Until i = n + 1
                Return LN_CF
            End If

        End Function

        Function CalcPartialVolume(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VTb, ByVal TIPO, ByVal deltaP)

            Dim lnfug1, lnfug2 As Double()
            Dim P1, P2 As Double
            P1 = P
            P2 = P + deltaP

            lnfug1 = Me.CalcLnFug(T, P1, Vx, VKij, VTc, VPc, Vw, VTb, TIPO)
            lnfug2 = Me.CalcLnFug(T, P2, Vx, VKij, VTc, VPc, Vw, VTb, TIPO)

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

