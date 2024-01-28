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
Imports DWSIM.Thermodynamics.PropertyPackages.ThermoPlugs.PR
Imports DWSIM.MathOps.MathEx.PolySolve
Imports Cudafy.Host
Imports Cudafy

Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(";")> <IgnoreFirst()> <System.Serializable()>
    Public Class PR_IPData

        Implements ICloneable

        Public ID1 As Integer = -1
        Public ID2 As Integer = -1

        <FieldHidden> Public Name1 As String = ""
        <FieldHidden> Public Name2 As String = ""

        Private _kij As Double = 0.0

        Public comment As String = ""

        <FieldHidden> Public Owner As Object

        Public Property kij As Double
            Get
                Return _kij
            End Get
            Set(value As Double)
                _kij = value
            End Set
        End Property

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New PR_IPData
            With newclass
                .ID1 = ID1
                .ID2 = ID2
                .Owner = Owner
                .kij = kij
                .comment = comment
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
                prip.Owner = Me
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(prip.ID1)) Then
                    If Me.InteractionParameters(csdb.GetCSName(prip.ID1)).ContainsKey(csdb.GetCSName(prip.ID2)) Then
                    Else
                        Me.InteractionParameters(csdb.GetCSName(prip.ID1)).Add(csdb.GetCSName(prip.ID2), prip.Clone)
                    End If
                ElseIf Me.InteractionParameters.ContainsKey(csdb.GetCSName(prip.ID2)) Then
                    If Me.InteractionParameters(csdb.GetCSName(prip.ID2)).ContainsKey(csdb.GetCSName(prip.ID1)) Then
                    Else
                        Me.InteractionParameters(csdb.GetCSName(prip.ID2)).Add(csdb.GetCSName(prip.ID1), prip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(prip.ID1), New Dictionary(Of String, PR_IPData))
                    Me.InteractionParameters(csdb.GetCSName(prip.ID1)).Add(csdb.GetCSName(prip.ID2), prip.Clone)
                End If
            Next
            For Each prip In pripc
                prip.Owner = Me
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

        Function JT_PR(ByVal Z As Double, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VMM As Double(), ByVal VZc As Double(), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Cp As Double, ByVal Vw As Double()) As Double

            Dim n, R As Double
            Dim vetor(8) As Double
            Dim Tc(), Pc(), Vc(), W(), Zc(), a, b, c, Tr() As Double

            n = Vz.Length - 1

            ReDim Zc(n), Tc(n), Pc(n), Vc(n), W(n), Tr(n)

            R = 8.314

            Dim i, j As Integer
            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                W(i) = Vw(i)
                Zc(i) = VZc(i)
                Vc(i) = Zc(i) * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim Vcm = 0.0#
            Dim wm = 0.0#
            Dim Zcm = 0.0#
            Dim MMm = 0.0#
            Do
                If Vz(i) <> 0 Then
                    Vcm += Vz(i) * Vc(i)
                    wm += Vz(i) * W(i)
                    Zcm += Vz(i) * Zc(i)
                    MMm += Vz(i) * VMM(i)
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Dim Tcm = 0.0#
            Do
                j = 0
                Do
                    If Vz(i) <> 0 And Vz(j) <> 0 Then Tcm += Vz(i) * Vz(j) * (Tc(i) * Tc(j)) ^ 0.5
                    j += 1
                Loop Until j = n + 1
                i += 1
            Loop Until i = n + 1

            Dim Pcm = Zcm * R * Tcm / (Vcm)

            a = 0.45724 * R ^ 2 * Tcm ^ 2 / Pcm
            b = 0.0778 * R * Tcm / Pcm
            c = 0.37464 + 1.54226 * wm - 0.26992 * wm ^ 2

            Dim Trm = T / Tcm
            Dim AG = -b * R * T
            Dim BG = -2 * a * T * (1 + c - c * Trm ^ 0.5) * (-0.5 * c * Trm ^ 0.5)
            Dim CG = a * (1 + c - c * Trm ^ 0.5) ^ 2

            Dim V As Double = Z * 8.314 * T / P

            Dim dP_dT_V = R / (V - b) - (a * (1 + c) * c * Tcm ^ -0.5 * T ^ -0.5 + a * c ^ 2 * Tcm ^ -1) / (V ^ 2 + 2 * b * V - b ^ 2)

            Dim dP_dV_T = -R * T / (V - b) ^ 2 + (2 * b + 2 * V) * (a * (1 + c) ^ 2 + 2 * a * (1 + c) * c * Tcm ^ -0.5 * T ^ 0.5 + a * c ^ 2 * Tcm ^ -1 * T) / (V ^ 2 + 2 * b * V - b ^ 2) ^ 2

            Dim JT = -(T * dP_dT_V / dP_dV_T + V) / (Cp * MMm)

            JT_PR = JT

        End Function


        Function Zc1(ByVal w As Double) As Double

            Zc1 = 0.291 - 0.08 * w

        End Function

        Function bi(ByVal omega As Double, ByVal Tc As Double, ByVal Pc As Double) As Double

            Return omega * 8.314 * Tc / Pc

        End Function

        Function Z_PR(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal TIPO As String) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Z_PR", "Peng-Robinson EOS Compressibility Factor", "Peng-Robinson EOS Compressibility Factor Calculation Routine")

            IObj?.Paragraphs.Add("The compressibility factor (liquid or vapor) can be obtained from the equation")
            IObj?.Paragraphs.Add("<math>Z^ {3} - (1 - b)Z^{2}+(A-3B^{2}-2B)Z-(AB-B^{2}-2B)=0,</math>")
            IObj?.Paragraphs.Add("<math>A =\frac{a_{m}P}{R^{2}T^{2}}</math>")
            IObj?.Paragraphs.Add("<math>B =\frac{b_{m}P}{RT}</math>")
            IObj?.Paragraphs.Add("<math>Z =\frac{PV}{RT}</math>")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", VTc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", VPc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", Vw.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

            Calculator.WriteToConsole("PR cubic equation root finder (Z) for T = " & T & " K, P = " & P & " Pa and Phase = " & TIPO, 3)
            Calculator.WriteToConsole("Mole fractions: " & DirectCast(Vx, Double()).ToMathArrayString, 3)

            Dim n, R, coeff(3) As Double

            n = Vx.Length - 1

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

            Dim tmpa As Double()() = Calc_SUM2(n, Vx, a)

            aml2 = tmpa(0)
            Dim aml As Double = tmpa(1)(0)

            i = 0
            Dim bml = 0.0#
            Do
                bml = bml + Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            IObj?.Paragraphs.Add("<math_inline>a_{i}</math_inline>: " & ai.ToMathArrayString)
            IObj?.Paragraphs.Add("<math_inline>b_{i}</math_inline>: " & bi.ToMathArrayString)

            IObj?.Paragraphs.Add("<math_inline>a_{m}</math_inline>: " & aml)
            IObj?.Paragraphs.Add("<math_inline>b_{m}</math_inline>: " & bml)

            Dim AG = aml * P / (R * T) ^ 2
            Dim BG = bml * P / (R * T)

            IObj?.Paragraphs.Add(String.Format("<math_inline>A</math_inline>: {0}", AG))
            IObj?.Paragraphs.Add(String.Format("<math_inline>B</math_inline>: {0}", BG))

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

            IObj?.Paragraphs.Add(String.Format("<math_inline>Z</math_inline>: {0}", Z_PR))

            IObj?.Close()

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

            Dim DHres = R * T * (Z - 1) + (T * dadT - ai) / (2 ^ 1.5 * bi) * Math.Log((Z + 2.44 * BG1) / (Z - 0.414 * BG1))

            H_PR = DHres / MM

        End Function

        Function H_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal VMM As Double(), ByVal Hid As Double) As Double

            Dim H As Double = 0.0#

            H = H_PR_MIX_CPU(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Hid)

            Return H

        End Function

        Function H_PR_MIX_CPU(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal Tc As Double(), ByVal Pc As Double(), ByVal w As Double(), ByVal VMM As Double(), ByVal Hid As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "H_PR_MIX_CPU", "Peng-Robinson EOS Enthalpy", "Peng-Robinson EOS Enthalpy Calculation Routine")

            IObj?.Paragraphs.Add("The Peng-Robinson equation is a cubic Equation of State (characteristic related to the exponent of the molar volume) 
                                    which relates temperature, pressure And molar volume of a pure component or a mixture of components at equilibrium. The cubic 
                                    equations are, in fact, The simplest equations capable of representing The behavior of liquid And vapor phases simultaneously.
                                    The Peng-Robinson EOS is written in the following form")
            IObj?.Paragraphs.Add("<math>P=\frac{RT}{(V-b)}-\frac{a(T)}{V(V+b)+b(V-b)}<math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>P</math_inline> pressure")
            IObj?.Paragraphs.Add("<math_inline>R</math_inline> ideal gas universal constant")
            IObj?.Paragraphs.Add("<math_inline>v</math_inline> molar volume")
            IObj?.Paragraphs.Add("<math_inline>b</math_inline> parameter related to hard-sphere volume")
            IObj?.Paragraphs.Add("<math_inline>a</math_inline> parameter related to intermolecular forces")
            IObj?.Paragraphs.Add("For pure substances, the a and b parameters are given by:")
            IObj?.Paragraphs.Add("<math>a(T)=[1+(0.37464+1.54226\omega-0.26992\omega^{2})(1-T_{r}^{(1/2)})]^{2}0.45724(R^{2}T_{c}^{2})/P_{c}</math>")
            IObj?.Paragraphs.Add("<math>b=0.07780(RT_{c})/P_{c}</math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>\omega</math_inline> acentric factor")
            IObj?.Paragraphs.Add("<math_inline>T_{c}</math_inline> critical temperature ")
            IObj?.Paragraphs.Add("<math_inline>P_{c}</math_inline> critical pressure")
            IObj?.Paragraphs.Add("<math_inline>T_{r}</math_inline> reduced temperature, T/Tc")
            IObj?.Paragraphs.Add("For mixtures, the above equation can be used, replacing a and b by mixture-representative values. Mixture a and b values are normally given by the basic mixing rule,")
            IObj?.Paragraphs.Add("<math>a_{m}=\sum_{i}\sum_{j}x_{i}x_{j}\sqrt{(a_{i}a_{j})}(1-k_{ij})</math>")
            IObj?.Paragraphs.Add("<math>b_{m}=\sum_{i}x_{i}b_{i}</math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>x_{i,j}</math_inline> molar fraction of the i Or j component in the phase (liquid Or vapor)")
            IObj?.Paragraphs.Add("<math_inline>a_{i,j}</math_inline> i Or j component a constant ")
            IObj?.Paragraphs.Add("<math_inline>b_{i,j}</math_inline> i Or j component b constant")
            IObj?.Paragraphs.Add("<math_inline>k_{ij}</math_inline> binary interaction parameter which characterizes the i-j pair")

            IObj?.Paragraphs.Add("For the PR EOS, Enthalpy is calculated by a departure function, which represents the difference between the current state and the ideal gas state.")

            IObj?.Paragraphs.Add("<math>\frac{H-H^{id}}{RT}=Z-1-\frac{1}{2^{1.5}bRT}[a-T\frac{da}{dT}]\ln[\frac{V+2.414b}{V-0.414b}]</math>")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Ideal Enthalpy (Ideal Gas State): {0} kJ/kg", Hid))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", Tc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", Pc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", w.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

            Dim n As Integer, R, Z, dadT As Double
            Dim i As Integer

            n = Vz.Length - 1

            Dim ai(n), bi(n), ci(n), a(n, n), b(n, n) As Double
            Dim Vc(n), Zc(n), alpha(n), m(n), Tr(n) As Double

            R = 8.314

            Dim MMm As Double = Vz.MultiplyY(VMM).SumY

            i = 0
            Do
                ci(i) = 0.37464 + 1.54226 * w(i) - 0.26992 * Math.Pow(w(i), 2)
                alpha(i) = Math.Pow(1 + ci(i) * (1 - Math.Sqrt(T / Tc(i))), 2)
                ai(i) = 0.45724 * alpha(i) * Math.Pow(R * Tc(i), 2) / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            a = Calc_SUM1(n, ai, VKij)

            Dim tmpa As Double()() = Calc_SUM2(n, Vz, a)

            Dim am As Double = tmpa(1)(0)

            Dim bm As Double = Vz.MultiplyY(bi).SumY

            IObj?.Paragraphs.Add("<math_inline>a_{i}</math_inline>: " & ai.ToMathArrayString)
            IObj?.Paragraphs.Add("<math_inline>b_{i}</math_inline>: " & bi.ToMathArrayString)

            IObj?.Paragraphs.Add("<math_inline>a_{m}</math_inline>: " & am)
            IObj?.Paragraphs.Add("<math_inline>b_{m}</math_inline>: " & bm)

            Dim AG1 = am * P / Math.Pow(R * T, 2)
            Dim BG1 = bm * P / (R * T)

            IObj?.Paragraphs.Add(String.Format("<math_inline>A</math_inline>: {0}", AG1))
            IObj?.Paragraphs.Add(String.Format("<math_inline>B</math_inline>: {0}", BG1))

            IObj?.SetCurrent()
            Dim _zarray As List(Of Double) = CalcZ2(AG1, BG1)

            If _zarray.Count = 0 Then
                Throw New Exception("PR EOS: unable to calculate a valid compressibility factor.")
            End If

            If TIPO = "L" Then
                Z = _zarray.Min
            ElseIf TIPO = "V" Then
                Z = _zarray.Max
            End If

            IObj?.Paragraphs.Add(String.Format("<math_inline>Z</math_inline>: {0}", Z))

            Dim V = (Z * R * T / P) ' m3/mol

            IObj?.Paragraphs.Add(String.Format("<math_inline>V</math_inline>: {0}", V))

            Dim tmp1 = MMm / V / 1000

            dadT = ThermoPlugs.PR.Calc_dadT(T, Vz, VKij, Tc, Pc, ai, ci)

            Dim uu, ww As Double
            uu = 2
            ww = -1

            Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(Z)
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Calculated Enthalpy Departure: {0} kJ/kmol", DHres))
            IObj?.Paragraphs.Add(String.Format("Calculated Enthalpy Departure: {0} kJ/kg", DHres / MMm))

            IObj?.Close()

            If MathEx.Common.Sum(Vz) = 0.0# Then
                Return 0.0#
            Else
                IObj?.Paragraphs.Add(String.Format("Calculated Total Enthalpy (Ideal + Departure): {0} kJ/kg", Hid + DHres / MMm))
                Return Hid + DHres / MMm
            End If

        End Function

        Function S_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz() As Double, ByVal VKij(,) As Double, ByVal VTc() As Double, ByVal VPc() As Double, ByVal Vw() As Double, ByVal VMM() As Double, ByVal Sid As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcLnFugCPU", "Peng-Robinson Entropy", "Peng-Robinson EOS Entropy Calculation Routine")

            IObj?.Paragraphs.Add("The Peng-Robinson equation is a cubic Equation of State (characteristic related to the exponent of the molar volume) 
                                    which relates temperature, pressure And molar volume of a pure component or a mixture of components at equilibrium. The cubic 
                                    equations are, in fact, The simplest equations capable of representing The behavior of liquid And vapor phases simultaneously.
                                    The Peng-Robinson EOS is written in the following form")
            IObj?.Paragraphs.Add("<math>P=\frac{RT}{(V-b)}-\frac{a(T)}{V(V+b)+b(V-b)}<math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>P</math_inline> pressure")
            IObj?.Paragraphs.Add("<math_inline>R</math_inline> ideal gas universal constant")
            IObj?.Paragraphs.Add("<math_inline>v</math_inline> molar volume")
            IObj?.Paragraphs.Add("<math_inline>b</math_inline> parameter related to hard-sphere volume")
            IObj?.Paragraphs.Add("<math_inline>a</math_inline> parameter related to intermolecular forces")
            IObj?.Paragraphs.Add("For pure substances, the a and b parameters are given by:")
            IObj?.Paragraphs.Add("<math>a(T)=[1+(0.37464+1.54226\omega-0.26992\omega^{2})(1-T_{r}^{(1/2)})]^{2}0.45724(R^{2}T_{c}^{2})/P_{c}</math>")
            IObj?.Paragraphs.Add("<math>b=0.07780(RT_{c})/P_{c}</math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>\omega</math_inline> acentric factor")
            IObj?.Paragraphs.Add("<math_inline>T_{c}</math_inline> critical temperature ")
            IObj?.Paragraphs.Add("<math_inline>P_{c}</math_inline> critical pressure")
            IObj?.Paragraphs.Add("<math_inline>T_{r}</math_inline> reduced temperature, T/Tc")
            IObj?.Paragraphs.Add("For mixtures, the above equation can be used, replacing a and b by mixture-representative values. Mixture a and b values are normally given by the basic mixing rule,")
            IObj?.Paragraphs.Add("<math>a_{m}=\sum_{i}\sum_{j}x_{i}x_{j}\sqrt{(a_{i}a_{j})}(1-k_{ij})</math>")
            IObj?.Paragraphs.Add("<math>b_{m}=\sum_{i}x_{i}b_{i}</math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>x_{i,j}</math_inline> molar fraction of the i Or j component in the phase (liquid Or vapor)")
            IObj?.Paragraphs.Add("<math_inline>a_{i,j}</math_inline> i Or j component a constant ")
            IObj?.Paragraphs.Add("<math_inline>b_{i,j}</math_inline> i Or j component b constant")
            IObj?.Paragraphs.Add("<math_inline>k_{ij}</math_inline> binary interaction parameter which characterizes the i-j pair")

            IObj?.Paragraphs.Add("For the PR EOS, Entropy is calculated by a departure function, which represents the difference between the current state and the ideal gas state.")

            IObj?.Paragraphs.Add("<math>\frac{S-S^{id}}{RT}=\ln(Z-B)-\ln\frac{P}{P^{0}}-\frac{A}{2^{1.5}bRT}[\frac{T}{a}\frac{da}{dT}]\ln[\frac{V+2.414b}{V+0.414b}]</math>")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Ideal Enthalpy (Ideal Gas State): {0} kJ/kg", Sid))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", VTc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", VPc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", Vw.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

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

            IObj?.Paragraphs.Add("<math_inline>a_{i}</math_inline>: " & ai.ToMathArrayString)
            IObj?.Paragraphs.Add("<math_inline>b_{i}</math_inline>: " & bi.ToMathArrayString)

            IObj?.Paragraphs.Add("<math_inline>a_{m}</math_inline>: " & am)
            IObj?.Paragraphs.Add("<math_inline>b_{m}</math_inline>: " & bm)

            Dim AG1 = am * P / (R * T) ^ 2
            Dim BG1 = bm * P / (R * T)

            IObj?.Paragraphs.Add(String.Format("<math_inline>A</math_inline>: {0}", AG1))
            IObj?.Paragraphs.Add(String.Format("<math_inline>B</math_inline>: {0}", BG1))

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

            IObj?.Paragraphs.Add(String.Format("<math_inline>Z</math_inline>: {0}", Z))

            Dim V = (Z * R * T / P) ' m3/mol

            IObj?.Paragraphs.Add(String.Format("<math_inline>V</math_inline>: {0}", V))

            Dim tmp1 = MMm / V / 1000

            dadT = ThermoPlugs.PR.Calc_dadT(T, Vz, VKij, Tc, Pc, ai, ci)

            Dim V0 As Double = R * 298.15 / 101325
            'Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V / V0) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Calculated Entropy Departure: {0} kJ/kmol.K", DSres))
            IObj?.Paragraphs.Add(String.Format("Calculated Entropy Departure: {0} kJ/kg.K", DSres / MMm))

            IObj?.Close()

            If MathEx.Common.Sum(Vz) = 0.0# Then
                S_PR_MIX = 0.0#
            Else
                IObj?.Paragraphs.Add(String.Format("Calculated Total Entropy (Ideal + Departure): {0} kJ/kg", Sid + DSres / MMm))
                S_PR_MIX = Sid + DSres / MMm '/ 1000
            End If

        End Function

        Function G_PR_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Array, ByVal VKij As Object, ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, ByVal VMM As Array, ByVal Sid As Double, ByVal Hid As Double) As Double

            Dim h As Double = H_PR_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Hid)
            Dim s As Double = S_PR_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Sid)

            Return h - T * s

        End Function

        Function CalcPartialVolume(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal VTb As Double(), ByVal TIPO As String, ByVal deltaP As Double)

            Dim n As Integer = UBound(Vx)
            Dim partvol(n) As Double

            If Double.IsNaN(Vx.Sum) Then
                Return partvol
            End If

            Dim lnfug1, lnfug2 As Double()
            Dim P1, P2 As Double
            P1 = P
            P2 = P + deltaP

            Dim pr2 As New ThermoPlugs.PR

            lnfug1 = pr2.CalcLnFug(T, P1, Vx, VKij, VTc, VPc, Vw, TIPO)
            lnfug2 = pr2.CalcLnFug(T, P2, Vx, VKij, VTc, VPc, Vw, TIPO)

            Dim i As Integer


            'V/RT = d(log f)/dP, const T

            i = 0
            For i = 0 To n
                partvol(i) = (Math.Log(Math.Exp(lnfug2(i)) * Vx(i) * P2) - Math.Log(Math.Exp(lnfug1(i)) * Vx(i) * P1)) / deltaP * (8314 * T) 'm3/kmol
                If Double.IsNaN(partvol(i)) Then partvol(i) = 0
            Next

            Return partvol

        End Function

        Function OF_Rho(ByVal rho As Double, ByVal aml As Double, ByVal bml As Double, ByVal T As Double) As Double

            Dim R As Double = 8.314
            Return 0.1 * 8.314 * T -
                        bml * rho * R * T * (1 - bml * rho) ^ -2 + R * T * (1 - bml * rho) ^ -1 +
                        aml * rho ^ 2 * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -2 * (2 * bml - 2 * bml ^ 2 * rho) +
                        2 * aml * rho * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -1

        End Function

    End Class

End Namespace


Namespace PropertyPackages.ThermoPlugs

    <System.Serializable()> Public Class PR

        Inherits ThermoPlug

        Shared Function Calc_dadT(T As Double, Vz As Double(), VKij As Double(,), Tc As Double(), Pc As Double(), ai As Double(), ci As Double()) As Double

            Dim n As Integer = Vz.Length - 1

            Dim aux1, aux2, auxtmp(n) As Double
            aux1 = -8.314 / 2 * (0.45724 / T) ^ 0.5

            Dim i, j As Integer
            aux2 = 0.0#
            Do
                j = 0
                Do
                    aux2 += Vz(i) * Vz(j) * (1 - VKij(i, j)) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Return aux1 * aux2

        End Function

        Shared Function Calc_SUM1(n As Integer, ai As Double(), vkij As Double(,))

            Dim a(n, n) As Double

            Dim i, j As Integer
            i = 0
            Do
                j = 0
                Do
                    a(i, j) = Math.Sqrt(ai(i) * ai(j)) * (1 - vkij(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Return a

        End Function

        Shared Function Calc_SUM2(n As Integer, Vx As Double(), a As Double(,)) As Double()()

            Dim saml, aml(n), aml2(n) As Double

            Dim i, j As Integer
            i = 0
            Do
                j = 0
                Do
                    saml = saml + Vx(i) * Vx(j) * a(i, j)
                    aml2(i) = aml2(i) + Vx(j) * a(j, i)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Return {aml2, New Double() {saml}}

        End Function

        Shared Function ReturnParameters(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double())

            Dim n, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim criterioOK As Boolean = False
            Dim AG, BG, aml, bml As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), ci(n), tmp(n + 1), a(n, n), b(n, n) As Double
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n) As Double
            Dim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n) As Double

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
                ci(i) = 0.37464 + 1.54226 * W(i) - 0.26992 * W(i) ^ 2
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

            Dim _zarray As List(Of Double), _mingz As Double(), Z As Double
            _zarray = CalcZ(T, P, Vx, VKij, VTc, VPc, Vw)
            _mingz = ZtoMinG(_zarray.ToArray(), T, P, Vx, VKij, VTc, VPc, Vw)
            Z = _zarray(_mingz(0))

            Dim aux1 = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 = 0.0#
            Do
                j = 0
                Do
                    aux2 += Vx(i) * Vx(j) * (1 - VKij(i, j)) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim dadT = aux1 * aux2

            Return New Double() {aml, bml, Z * R * T / P, dadT}

        End Function

        Shared Function ZtoMinG(ByVal Z_ As Double(), ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Object, ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double()) As Double()

            Calculator.WriteToConsole("PR min-G root finder (Z) for T = " & T & " K, P = " & P & " Pa and Z = " & DirectCast(Z_, Double()).ToMathArrayString, 3)

            Dim S, H, Z As Double

            Dim n As Integer, R, dadT As Double
            Dim i, j, k, l As Integer

            n = Vz.Length - 1

            Dim G(UBound(Z_)) As Double

            Dim ai(n), bi(n), ci(n), a(n, n), b(n, n) As Double
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), m(n), Tr(n) As Double

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
            Do
                alpha(i) = (1 + (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                ci(i) = 0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2
                i = i + 1
            Loop Until i = n + 1

            a = Calc_SUM1(n, ai, VKij)

            Dim tmpa As Double()() = Calc_SUM2(n, Vz, a)

            Dim am As Double = tmpa(1)(0)

            Dim bm As Double = Vz.MultiplyY(bi).SumY

            'i = 0
            'Dim bm = 0.0#
            'Do
            '    bm = bm + Vz(i) * bi(i)
            '    i = i + 1
            'Loop Until i = n + 1

            Dim AG1 = am * P / (R * T) ^ 2
            Dim BG1 = bm * P / (R * T)

            l = 0
            For Each Z In Z_

                Dim V = (Z * R * T / P) ' m3/mol

                Dim tmp1 = 1 / V / 1000

                dadT = Calc_dadT(T, Vz, VKij, Tc, Pc, ai, ci)

                Dim uu, ww As Double
                uu = 2
                ww = -1

                Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(V * 101325 / (R * 298.15))
                Dim V0 As Double = R * 298.15 / 101325
                Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
                Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

                H = DHres

                S = DSres

                G(l) = H - T * S

                If j = 0 Then
                    k = 0
                Else
                    i = 0
                    Do
                        If G(l) <= G(k) Then k = l
                        i = i + 1
                    Loop Until i = UBound(G) + 1
                End If

                l = l + 1

            Next

            Calculator.WriteToConsole("Result: Min-G Z Index = " & k, 3)

            Return New Double() {k, G(k)}

        End Function

        Public Overrides Function CalcLnFug(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VTc As Double(),
                                            ByVal VPc As Double(), ByVal Vw As Double(), Optional ByVal otherargs As Object = Nothing, Optional ByVal phase As Integer = -1) As Double()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcLnFug", "Peng-Robinson EOS Fugacity Coefficient", "Property Package Fugacity Coefficient Calculation Routine")

            IObj?.SetCurrent()

            Dim result As Double()

            If Settings.EnableGPUProcessing Then
                IObj?.Paragraphs.Add("DWSIM will calculate PR EOS Fugacity Coefficient using the GPU.")
                result = CalcLnFugGPU(T, P, Vx, VKij, VTc, VPc, Vw, otherargs, phase)
            Else
                IObj?.Paragraphs.Add("DWSIM will calculate PR EOS Fugacity Coefficient using the CPU.")
                result = CalcLnFugCPU(T, P, Vx, VKij, VTc, VPc, Vw, otherargs, phase)
            End If

            IObj?.Close()

            Return result

        End Function

        Private Function CalcLnFugCPU(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal Tc As Double(), ByVal Pc As Double(), ByVal w As Double(), Optional ByVal otherargs As Object = Nothing, Optional ByVal phase As Integer = -1)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcLnFugCPU", "Peng-Robinson EOS Fugacity Coefficient (CPU)", "Peng-Robinson EOS Fugacity Coefficient Calculation Routine")

            IObj?.Paragraphs.Add("The Peng-Robinson equation is a cubic Equation of State (characteristic related to the exponent of the molar volume) 
                                    which relates temperature, pressure And molar volume of a pure component or a mixture of components at equilibrium. The cubic 
                                    equations are, in fact, The simplest equations capable of representing The behavior of liquid And vapor phases simultaneously.
                                    The Peng-Robinson EOS is written in the following form")
            IObj?.Paragraphs.Add("<math>P=\frac{RT}{(V-b)}-\frac{a(T)}{V(V+b)+b(V-b)}<math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>P</math_inline> pressure")
            IObj?.Paragraphs.Add("<math_inline>R</math_inline> ideal gas universal constant")
            IObj?.Paragraphs.Add("<math_inline>v</math_inline> molar volume")
            IObj?.Paragraphs.Add("<math_inline>b</math_inline> parameter related to hard-sphere volume")
            IObj?.Paragraphs.Add("<math_inline>a</math_inline> parameter related to intermolecular forces")
            IObj?.Paragraphs.Add("For pure substances, the a and b parameters are given by:")
            IObj?.Paragraphs.Add("<math>a(T)=[1+(0.37464+1.54226\omega-0.26992\omega^{2})(1-T_{r}^{(1/2)})]^{2}0.45724(R^{2}T_{c}^{2})/P_{c}</math>")
            IObj?.Paragraphs.Add("<math>b=0.07780(RT_{c})/P_{c}</math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>\omega</math_inline> acentric factor")
            IObj?.Paragraphs.Add("<math_inline>T_{c}</math_inline> critical temperature ")
            IObj?.Paragraphs.Add("<math_inline>P_{c}</math_inline> critical pressure")
            IObj?.Paragraphs.Add("<math_inline>T_{r}</math_inline> reduced temperature, T/Tc")
            IObj?.Paragraphs.Add("For mixtures, the above equation can be used, replacing a and b by mixture-representative values. Mixture a and b values are normally given by the basic mixing rule,")
            IObj?.Paragraphs.Add("<math>a_{m}=\sum_{i}\sum_{j}x_{i}x_{j}\sqrt{(a_{i}a_{j})}(1-k_{ij})</math>")
            IObj?.Paragraphs.Add("<math>b_{m}=\sum_{i}x_{i}b_{i}</math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>x_{i,j}</math_inline> molar fraction of the i Or j component in the phase (liquid Or vapor)")
            IObj?.Paragraphs.Add("<math_inline>a_{i,j}</math_inline> i Or j component a constant ")
            IObj?.Paragraphs.Add("<math_inline>b_{i,j}</math_inline> i Or j component b constant")
            IObj?.Paragraphs.Add("<math_inline>k_{ij}</math_inline> binary interaction parameter which characterizes the i-j pair")
            IObj?.Paragraphs.Add("The fugacity coefficient obtained with the Peng-Robinson EOS in given by")
            IObj?.Paragraphs.Add("<math>\ln\frac{f_{i}}{x_{i}P}=\frac{b_{i}}{b_{m}}\left(Z-1\right)-\ln\left(Z-B\right)-\frac{A}{2\sqrt{2}B}\left(\frac{2\sum_{k}x_{k}a_{ki}}{a_{m}}-\frac{b_{i}}{b_{m}}\right)\ln\left(\frac{Z+2,414B}{Z-0,414B}\right),</math>")
            IObj?.Paragraphs.Add("where Z Is the phase compressibility factor (liquid or vapor) and can be obtained from the equation")
            IObj?.Paragraphs.Add("<math>Z^ {3} - (1 - b)Z^{2}+(A-3B^{2}-2B)Z-(AB-B^{2}-2B)=0,</math>")
            IObj?.Paragraphs.Add("<math>A =\frac{a_{m}P}{R^{2}T^{2}}</math>")
            IObj?.Paragraphs.Add("<math>B =\frac{b_{m}P}{RT}</math>")
            IObj?.Paragraphs.Add("<math>Z =\frac{PV}{RT}</math>")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", Tc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", Pc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", w.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

            Dim n As Integer, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim criterioOK As Boolean = False
            Dim AG, BG, aml, bml As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n) As Double
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n) As Double
            Dim alpha(n), m(n), Tr(n) As Double

            R = 8.314

            Dim i As Integer

            i = 0
            Do
                alpha(i) = Math.Pow(1 + (0.37464 + 1.54226 * w(i) - 0.26992 * Math.Pow(w(i), 2)) * (1 - Math.Sqrt(T / Tc(i))), 2)
                ai(i) = 0.45724 * alpha(i) * Math.Pow(R * Tc(i), 2) / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            a = Calc_SUM1(n, ai, VKij)

            IObj?.Paragraphs.Add("<math_inline>a_{i}</math_inline>: " & ai.ToMathArrayString)
            IObj?.Paragraphs.Add("<math_inline>b_{i}</math_inline>: " & bi.ToMathArrayString)

            Dim tmpa As Double()() = Calc_SUM2(n, Vx, a)

            aml2 = tmpa(0)
            aml = tmpa(1)(0)

            bml = Vx.MultiplyY(bi).SumY

            IObj?.Paragraphs.Add("<math_inline>a_{m}</math_inline>: " & aml)
            IObj?.Paragraphs.Add("<math_inline>b_{m}</math_inline>: " & bml)

            AG = aml * P / Math.Pow(R * T, 2)
            BG = bml * P / (R * T)

            IObj?.Paragraphs.Add(String.Format("<math_inline>A</math_inline>: {0}", AG))
            IObj?.Paragraphs.Add(String.Format("<math_inline>B</math_inline>: {0}", BG))

            Dim _zarray As List(Of Double), _mingz As Double(), Z As Double

            IObj?.SetCurrent()

            _zarray = CalcZ2(AG, BG)
            If _zarray.Count = 0 Then
                Dim ex As New Exception(String.Format("PR EOS: unable to find a root with provided parameters [T = {0} K, P = {1} Pa, MoleFracs={2}]", T.ToString, P.ToString, Vx.ToArrayString))
                ex.Data.Add("DetailedDescription", "This error occurs when the PR EOS is unable to find a density root with the given parameters.")
                ex.Data.Add("UserAction", "Check if the parameters are valid (T, P, composition). If this error keeps occuring, try another Property Package or check the Material Stream / Unit Operation properties.")
                Throw ex
            End If
            If phase = 0 Then
                Z = _zarray.Min
            ElseIf phase = 1 Then
                Z = _zarray.Max
            Else
                _mingz = ZtoMinG(_zarray.ToArray, T, P, Vx, VKij, Tc, Pc, w)
                Z = _zarray(_mingz(0))
            End If

            IObj?.Paragraphs.Add(String.Format("<math_inline>Z</math_inline>: {0}", Z))

            Dim t1, t2, t3, t4, t5 As Double
            i = 0
            Do
                t1 = bi(i) * (Z - 1) / bml
                t2 = -Math.Log(Z - BG)
                t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                t4 = Math.Log((Z + (1 + 1.414213) * BG) / (Z + (1 - 1.414213) * BG))
                t5 = 2 * 1.414213 * BG
                LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                LN_CF(i) = LN_CF(i)
                i = i + 1
            Loop Until i = n + 1

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Fugacity Coefficients: {0}", LN_CF.ExpY().ToMathArrayString))

            IObj?.Close()

            Return LN_CF

        End Function

        Private Function CalcLnFugGPU(ByVal T As Double, ByVal P As Double, ByVal Vx As Array, ByVal VKij As Double(,), ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, Optional ByVal otherargs As Object = Nothing, Optional ByVal phase As Integer = -1)

            Dim n, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim criterioOK As Boolean = False
            Dim AG, BG, aml, bml As Double
            Dim t1, t2, t3, t4, t5 As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n) As Double
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n) As Double
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

            Dim aml_temp(n), aml2_temp(n), bml_temp(n) As Double

            pr_gpu_func(n, Vx, VKij, Tc, Pc, W, T, alpha, ai, bi, a, aml_temp, bml_temp, aml2_temp)

            aml2 = aml2_temp
            aml = MathEx.Common.Sum(aml_temp)
            bml = MathEx.Common.Sum(bml_temp)

            AG = aml * P / Math.Pow(R * T, 2)
            BG = bml * P / (R * T)

            Dim _zarray As List(Of Double), _mingz As Double(), Z As Double

            _zarray = CalcZ2(AG, BG)
            If phase = 0 Then
                Z = _zarray.Min
            ElseIf phase = 1 Then
                Z = _zarray.Max
            Else
                _mingz = ZtoMinG(_zarray.ToArray, T, P, Vx, VKij, Tc, Pc, W)
                Z = _zarray(_mingz(0))
            End If

            i = 0
            Do
                t1 = bi(i) * (Z - 1) / bml
                t2 = -Math.Log(Z - BG)
                t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                t4 = Math.Log((Z + (1 + 2 ^ 0.5) * BG) / (Z + (1 - 2 ^ 0.5) * BG))
                t5 = 2 * 2 ^ 0.5 * BG
                LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                i = i + 1
            Loop Until i = n + 1

            Return LN_CF

        End Function

        Public Shared Sub pr_gpu_func(n As Integer, Vx As Double(), VKij As Double(,), Tc As Double(), Pc As Double(), w As Double(), T As Double, alpha As Double(), ai As Double(), bi As Double(), a As Double(,), aml_temp As Double(), bml_temp As Double(), aml2_temp As Double())

            Dim gpu As GPGPU = Settings.gpu

            'gpu.Lock()
            gpu.SetCurrentContext()

            Dim dev_alpha As Double() = Nothing
            Dim dev_ai As Double() = Nothing
            Dim dev_bi As Double() = Nothing
            Dim dev_Tc As Double() = Nothing
            Dim dev_Pc As Double() = Nothing
            Dim dev_W As Double() = Nothing
            Dim dev_a As Double(,) = Nothing
            Dim dev_vkij As Double(,) = Nothing
            Dim dev_Vx As Double() = Nothing
            Dim dev_aml2_temp As Double() = Nothing
            Dim dev_aml_temp As Double() = Nothing
            Dim dev_bml_temp As Double() = Nothing

            ' allocate the memory on the GPU
            dev_alpha = gpu.Allocate(Of Double)(alpha)
            dev_ai = gpu.Allocate(Of Double)(ai)
            dev_bi = gpu.Allocate(Of Double)(bi)
            dev_Tc = gpu.Allocate(Of Double)(Tc)
            dev_Pc = gpu.Allocate(Of Double)(Pc)
            dev_W = gpu.Allocate(Of Double)(w)
            dev_a = gpu.Allocate(Of Double)(a)
            dev_vkij = gpu.Allocate(Of Double)(VKij)
            dev_Vx = gpu.Allocate(Of Double)(Vx)
            dev_aml2_temp = gpu.Allocate(Of Double)(aml2_temp)
            dev_aml_temp = gpu.Allocate(Of Double)(aml_temp)
            dev_bml_temp = gpu.Allocate(Of Double)(bml_temp)

            ' copy the arrays to the GPU
            gpu.CopyToDevice(alpha, dev_alpha)
            gpu.CopyToDevice(ai, dev_ai)
            gpu.CopyToDevice(bi, dev_bi)
            gpu.CopyToDevice(Tc, dev_Tc)
            gpu.CopyToDevice(Pc, dev_Pc)
            gpu.CopyToDevice(w, dev_W)
            gpu.CopyToDevice(a, dev_a)
            gpu.CopyToDevice(VKij, dev_vkij)
            gpu.CopyToDevice(Vx, dev_Vx)
            gpu.CopyToDevice(aml2_temp, dev_aml2_temp)
            gpu.CopyToDevice(aml_temp, dev_aml_temp)
            gpu.CopyToDevice(bml_temp, dev_bml_temp)

            ' launch subs
            gpu.Launch(n + 1, 1, "pr_gpu_sum1", dev_alpha, dev_ai, dev_bi, dev_Tc, dev_Pc, dev_W, T)
            gpu.Launch(New dim3(n + 1, n + 1), 1, "pr_gpu_sum2", dev_a, dev_ai, dev_vkij)
            gpu.Launch(n + 1, 1, "pr_gpu_sum3", dev_Vx, dev_a, dev_aml_temp, dev_aml2_temp)
            gpu.Launch(n + 1, 1, "pr_gpu_sum4", dev_Vx, dev_bi, dev_bml_temp)

            ' copy the arrays back from the GPU to the CPU
            gpu.CopyFromDevice(dev_alpha, alpha)
            gpu.CopyFromDevice(dev_ai, ai)
            gpu.CopyFromDevice(dev_bi, bi)
            gpu.CopyFromDevice(dev_Tc, Tc)
            gpu.CopyFromDevice(dev_Pc, Pc)
            gpu.CopyFromDevice(dev_W, w)
            gpu.CopyFromDevice(dev_a, a)
            gpu.CopyFromDevice(dev_vkij, VKij)
            gpu.CopyFromDevice(dev_Vx, Vx)
            gpu.CopyFromDevice(dev_aml2_temp, aml2_temp)
            gpu.CopyFromDevice(dev_aml_temp, aml_temp)
            gpu.CopyFromDevice(dev_bml_temp, bml_temp)

            ' free the memory allocated on the GPU
            gpu.Free(dev_alpha)
            gpu.Free(dev_ai)
            gpu.Free(dev_bi)
            gpu.Free(dev_Tc)
            gpu.Free(dev_Pc)
            gpu.Free(dev_W)
            gpu.Free(dev_a)
            gpu.Free(dev_vkij)
            gpu.Free(dev_Vx)
            gpu.Free(dev_aml2_temp)
            gpu.Free(dev_aml_temp)
            gpu.Free(dev_bml_temp)

            'gpu.Unlock()

        End Sub

        <Cudafy.Cudafy()> Public Shared Sub pr_gpu_sum1(thread As Cudafy.GThread, alpha As Double(), ai As Double(), bi As Double(), Tc As Double(), Pc As Double(), W As Double(), T As Double)

            Dim i As Integer = thread.blockIdx.x

            alpha(i) = (1 + (0.37464 + 1.54226 * W(i) - 0.26992 * W(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
            ai(i) = 0.45724 * alpha(i) * 8.314 ^ 2 * Tc(i) ^ 2 / Pc(i)
            bi(i) = 0.0778 * 8.314 * Tc(i) / Pc(i)

        End Sub

        <Cudafy.Cudafy()> Public Shared Sub pr_gpu_sum2(thread As Cudafy.GThread, a As Double(,), ai As Double(), VKij As Double(,))

            Dim i As Integer = thread.blockIdx.x
            Dim j As Integer = thread.blockIdx.y

            a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKij(i, j))

        End Sub

        <Cudafy.Cudafy()> Public Shared Sub pr_gpu_sum3(thread As Cudafy.GThread, Vx As Double(), a As Double(,), aml_temp As Double(), aml2_temp As Double())

            Dim i As Integer = thread.blockIdx.x

            aml_temp(i) = 0
            aml2_temp(i) = 0
            For k As Integer = 0 To Vx.Length - 1
                aml_temp(i) += Vx(i) * Vx(k) * a(i, k)
                aml2_temp(i) += Vx(k) * a(k, i)
            Next

        End Sub

        <Cudafy.Cudafy()> Public Shared Sub pr_gpu_sum4(thread As Cudafy.GThread, Vx As Double(), bi As Double(), bml_temp As Double())

            Dim i As Integer = thread.blockIdx.x

            bml_temp(i) = Vx(i) * bi(i)

        End Sub

        Shared Function CalcZ(ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double()) As List(Of Double)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcZ", "Peng-Robinson EOS Compressibility Factor", "Peng-Robinson EOS Compressibility Factor Calculation Routine")

            IObj?.Paragraphs.Add("The compressibility factor (liquid or vapor) can be obtained from the equation")
            IObj?.Paragraphs.Add("<math>Z^ {3} - (1 - b)Z^{2}+(A-3B^{2}-2B)Z-(AB-B^{2}-2B)=0,</math>")
            IObj?.Paragraphs.Add("<math>A =\frac{a_{m}P}{R^{2}T^{2}}</math>")
            IObj?.Paragraphs.Add("<math>B =\frac{b_{m}P}{RT}</math>")
            IObj?.Paragraphs.Add("<math>Z =\frac{PV}{RT}</math>")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", VTc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", VPc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", Vw.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

            Dim n As Integer, R, coeff(3) As Double
            Dim Vant(0, 4) As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n) As Double
            Dim aml, aml2(n), amv2(n) As Double
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
                alpha(i) = (1 + (0.37464 + 1.54226 * W(i) - 0.26992 * W(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            a = Calc_SUM1(n, ai, VKij)

            Dim tmpa As Double()() = Calc_SUM2(n, Vx, a)

            aml2 = tmpa(0)
            aml = tmpa(1)(0)

            Dim bml As Double = Vx.MultiplyY(bi).SumY

            Dim AG = aml * P / (R * T) ^ 2
            Dim BG = bml * P / (R * T)

            IObj?.Paragraphs.Add("<math_inline>a_{i}</math_inline>: " & ai.ToMathArrayString)
            IObj?.Paragraphs.Add("<math_inline>b_{i}</math_inline>: " & bi.ToMathArrayString)

            IObj?.Paragraphs.Add("<math_inline>a_{m}</math_inline>: " & aml)
            IObj?.Paragraphs.Add("<math_inline>b_{m}</math_inline>: " & bml)

            IObj?.Paragraphs.Add(String.Format("<math_inline>A</math_inline>: {0}", AG))
            IObj?.Paragraphs.Add(String.Format("<math_inline>B</math_inline>: {0}", BG))

            coeff(0) = -AG * BG + BG ^ 2 + BG ^ 3
            coeff(1) = AG - 3 * BG ^ 2 - 2 * BG
            coeff(2) = BG - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0.0#
            Dim tv2 As Double

            Dim result As New List(Of Double)

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

            If temp1(0, 1) = 0.0# And temp1(0, 0) > 0.0# Then result.Add(temp1(0, 0))
            If temp1(1, 1) = 0.0# And temp1(1, 0) > 0.0# Then result.Add(temp1(1, 0))
            If temp1(2, 1) = 0.0# And temp1(2, 0) > 0.0# Then result.Add(temp1(2, 0))

            IObj?.Paragraphs.Add(String.Format("Found {0} roots for the cubic equation.", result.Count))
            For Each item In result
                IObj?.Paragraphs.Add(String.Format("<math_inline>Z</math_inline>: {0}", item))
            Next

            IObj?.Close()

            If result.Count = 0 Then
                Throw New Exception("PR EOS: Unable to calculate compressility factor at given conditions.")
            End If

            Return result

        End Function

        Shared Function CalcZ2(AG As Double, BG As Double) As List(Of Double)

            Dim coeff(3) As Double
            Dim Vant(0, 4) As Double

            coeff(0) = -AG * BG + BG ^ 2 + BG ^ 3
            coeff(1) = AG - 3 * BG ^ 2 - 2 * BG
            coeff(2) = BG - 1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0.0#
            Dim ZV, tv2 As Double

            Dim result As New List(Of Double)

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

            If temp1(0, 1) = 0.0# And temp1(0, 0) > 0.0# Then result.Add(temp1(0, 0))
            If temp1(1, 1) = 0.0# And temp1(1, 0) > 0.0# Then result.Add(temp1(1, 0))
            If temp1(2, 1) = 0.0# And temp1(2, 0) > 0.0# Then result.Add(temp1(2, 0))

            If result.Count = 0 Then
                Throw New Exception("PR EOS: Unable to calculate compressility factor at given conditions.")
            End If

            Return result

        End Function

        Public Overrides Function PhaseType(ByVal T As Double, ByVal P As Double, ByVal Vx As Array, ByVal VKij As Object, ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, Optional ByVal otherargs As Object = Nothing)

            Dim n, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim beta As Double
            Dim criterioOK As Boolean = False
            Dim AG, BG, aml, bml As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n)
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n) As Double
            Dim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n)

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

            Dim _zarray As List(Of Double), _mingz As Double(), Z As Double

            _zarray = CalcZ(T, P, Vx, VKij, VTc, VPc, Vw)
            _mingz = ZtoMinG(_zarray.ToArray(), T, P, Vx, VKij, VTc, VPc, Vw)
            Z = _zarray(_mingz(0))

            beta = 1 / P * (1 - (BG * Z ^ 2 + AG * Z - 6 * BG ^ 2 * Z - 2 * BG * Z - 2 * AG * BG + 2 * BG ^ 2 + 2 * BG) / (Z * (3 * Z ^ 2 - 2 * Z + 2 * BG * Z + AG - 3 * BG ^ 2 - 2 * BG)))
            Dim phase As String = "Unknown"

            'If beta < 0.005 / 101322 Then phase = "L" Else phase = "V"
            'If beta > 0.9 / P And beta < 3 / P Then phase = "V" Else phase = "L"
            If Z < 0.302 Then phase = "L" Else phase = "V"

            Return New Object() {phase, beta}

        End Function

        Public Overrides Function CalcEnthalpy(ByVal phasetype As String, ByVal T As Double, ByVal P As Double, ByVal Vz As System.Array, ByVal VKij As Object, ByVal VTc As System.Array, ByVal VPc As System.Array, ByVal Vw As System.Array, ByVal Hid As Double, Optional ByVal otherargs As Object = Nothing) As Double

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
            Try

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

                If phasetype = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                ElseIf phasetype = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If
            Catch

                Dim findZ

                If phasetype = "V" Then

                    Z = 1
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z -= 0.00001
                        If Z < 0 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                Else

                    Z = 0
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z += 0.00001
                        If Z > 1 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                End If

            End Try

            Dim V = (Z * R * T / P) ' m3/mol

            dadT = Calc_dadT(T, Vz, VKij, Tc, Pc, ai, ci)

            Dim uu, ww As Double
            uu = 2
            ww = -1

            Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(V * 101325 / (R * 298.15))
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V / V0) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            Return Hid + DHres

        End Function

        Public Overrides Function CalcEntropy(ByVal phasetype As String, ByVal T As Double, ByVal P As Double, ByVal Vz As System.Array, ByVal VKij As Object, ByVal VTc As System.Array, ByVal VPc As System.Array, ByVal Vw As System.Array, ByVal Sid As Double, Optional ByVal otherargs As Object = Nothing) As Double

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
            Try

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

                If phasetype = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                ElseIf phasetype = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If
            Catch

                Dim findZ

                If phasetype = "V" Then

                    Z = 1
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z -= 0.00001
                        If Z < 0 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                Else

                    Z = 0
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z += 0.00001
                        If Z > 1 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                End If

            End Try

            Dim V = (Z * R * T / P) ' m3/mol

            Dim aux1 = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 = 0.0#
            Do
                j = 0
                Do
                    aux2 += Vz(i) * Vz(j) * (1 - VKij(i, j)) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadT = aux1 * aux2

            Dim uu, ww As Double
            uu = 2
            ww = -1

            Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(V * 101325 / (R * 298.15))
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V / V0) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            Return Sid + DSres

        End Function

        Public Overrides Function CalcGibbsEnergy(ByVal phasetype As String, ByVal T As Double, ByVal P As Double, ByVal Vz As System.Array, ByVal VKij As Object, ByVal VTc As System.Array, ByVal VPc As System.Array, ByVal Vw As System.Array, ByVal Gid As Double, Optional ByVal otherargs As Object = Nothing) As Double

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
            Try

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

                If phasetype = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                ElseIf phasetype = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If
            Catch

                Dim findZ

                If phasetype = "V" Then

                    Z = 1
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z -= 0.00001
                        If Z < 0 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                Else

                    Z = 0
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z += 0.00001
                        If Z > 1 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                End If

            End Try

            Dim V = (Z * R * T / P) ' m3/mol

            Dim aux1 = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 = 0.0#
            Do
                j = 0
                Do
                    aux2 += Vz(i) * Vz(j) * (1 - VKij(i, j)) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadT = aux1 * aux2

            Dim uu, ww As Double
            uu = 2
            ww = -1

            Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(V * 101325 / (R * 298.15))
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V / V0) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            Return Gid + (DHres - T * DSres)

        End Function

        Public Overrides Function CalcHelmoltzEnergy(ByVal phasetype As String, ByVal T As Double, ByVal P As Double, ByVal Vz As System.Array, ByVal VKij As Object, ByVal VTc As System.Array, ByVal VPc As System.Array, ByVal Vw As System.Array, ByVal Aid As Double, Optional ByVal otherargs As Object = Nothing) As Double

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
            Try

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

                If phasetype = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                ElseIf phasetype = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If
            Catch

                Dim findZ

                If phasetype = "V" Then

                    Z = 1
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z -= 0.00001
                        If Z < 0 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                Else

                    Z = 0
                    Do
                        findZ = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                        Z += 0.00001
                        If Z > 1 Then Throw New Exception(Calculator.GetLocalString("PropPack_ZError"))
                    Loop Until Math.Abs(findZ) < 0.0001

                End If

            End Try

            Dim V = (Z * R * T / P) ' m3/mol

            Dim aux1 = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 = 0.0#
            Do
                j = 0
                Do
                    aux2 += Vz(i) * Vz(j) * (1 - VKij(i, j)) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadT = aux1 * aux2

            Dim uu, ww As Double
            uu = 2
            ww = -1

            Dim DAres = am / (bm * (uu ^ 2 - 4 * ww) ^ 0.5) * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(V * 101325 / (R * 298.15))
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V / V0) - 1 / (8 ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (2 - 8 ^ 0.5)) / (2 * Z + BG1 * (2 + 8 ^ 0.5)))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            Return Aid + DAres

        End Function

        Public Overrides Function CalcP(ByVal V As Double, ByVal T As Double, ByVal Vx As System.Array, ByVal VKij As Object, ByVal VTc As System.Array, ByVal VPc As System.Array, ByVal Vw As System.Array, Optional ByVal otherargs As Object = Nothing) As Object

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
                alpha(i) = Math.Pow(1 + (0.37464 + 1.54226 * Vw(i) - 0.26992 * Math.Pow(Vw(i), 2)) * (1 - Math.Sqrt(T / VTc(i))), 2)
                ai(i) = 0.45724 * alpha(i) * Math.Pow(R * VTc(i), 2) / VPc(i)
                bi(i) = 0.0778 * R * VTc(i) / VPc(i)
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

            Return P

        End Function

        Public Overrides Function CalcLnFugTV(ByVal T As Double, ByVal V As Double, ByVal Vx As System.Array, ByVal VKij As Object, ByVal VTc As System.Array, ByVal VPc As System.Array, ByVal Vw As System.Array, Optional ByVal otherargs As Object = Nothing, Optional ByVal phase As Integer = -1) As Double()

            Dim P As Double = Me.CalcP(V, T, Vx, VKij, VTc, VPc, Vw, otherargs)

            'If P < 0 Then P = -P

            Dim n As Integer, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim criterioOK As Boolean = False
            Dim AG, BG, aml, bml As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n) As Double
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n) As Double
            Dim alpha(n), m(n), Tr(n) As Double

            R = 8.314

            Dim i As Integer

            i = 0
            Do
                alpha(i) = Math.Pow(1 + (0.37464 + 1.54226 * Vw(i) - 0.26992 * Math.Pow(Vw(i), 2)) * (1 - Math.Sqrt(T / VTc(i))), 2)
                ai(i) = 0.45724 * alpha(i) * Math.Pow(R * VTc(i), 2) / VPc(i)
                bi(i) = 0.0778 * R * VTc(i) / VPc(i)
                i = i + 1
            Loop Until i = n + 1

            a = Calc_SUM1(n, ai, VKij)

            Dim tmpa As Double()() = Calc_SUM2(n, Vx, a)

            aml2 = tmpa(0)
            aml = tmpa(1)(0)

            i = 0
            bml = 0.0#
            Do
                bml += Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            AG = aml * P / Math.Pow(R * T, 2)
            BG = bml * P / (R * T)

            Dim Z = P * V / (R * T)

            Dim t1, t2, t3, t4, t5 As Double
            i = 0
            Do
                t1 = bi(i) * (Z - 1.0) / bml
                t2 = -Math.Log(Z - BG)
                t3 = AG * (2.0 * aml2(i) / aml - bi(i) / bml)
                t4 = Math.Log((Z + (1.0 + 1.414213) * BG) / (Z + (1.0 - 1.414213) * BG))
                t5 = 2.0 * 1.414213 * BG
                LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                i = i + 1
            Loop Until i = n + 1

            Return LN_CF

        End Function

        ''' <summary>
        ''' This procedure checks if the compressibility factor is within the allowable region for the specified phase. 
        ''' If not, it generates a pseudo-root cabable of generate properties for the specified phase in order to keep 
        ''' the flash convergence process going forward.
        ''' </summary>
        ''' <param name="Z">The calculated compressibility factor, coming from the EOS</param>
        ''' <param name="a">EOS 'a' mixture parameter</param>
        ''' <param name="b">EOS 'b' mixture parameter</param>
        ''' <param name="P">Pressure in Pa</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="phaselabel">'L' for Liquid, 'V' for Vapor.</param>
        ''' <returns>A vector containing the calculated compressibility factor and pressure, if required. 
        ''' If the given compressibility factor is within the allowable range, it is returned together with 
        ''' the specified pressure (no pseudoroot calculation is required).</returns>
        ''' <remarks>This procedure is based on the paper: 
        ''' Mathias, P. M., Boston, J. F. and Watanasiri, S. (1984), 
        ''' Effective utilization of equations of state for thermodynamic properties in process simulation. 
        ''' AIChE J., 30: 182–186. doi: 10.1002/aic.690300203</remarks>

        Public Shared Function CheckRoot(Z As Double, a As Double, b As Double, P As Double, T As Double, phaselabel As String) As Double()

            If a * b = 0.0# Then Return New Double() {Z, P}

            Dim rho, dPdrho, R, Pnew, P_, rho_, Tmc,
                Zcorr, rhomax, rhomin, rhomc, dPdrholim, C0, C1, rho2 As Double
            Dim i As Integer

            R = 8.314

            If Z < 0.0# Then Z = -Z

            Tmc = 0.20268 * a / (R * b)
            If T > Tmc Then T = Tmc * 0.9

            rho = P / (Z * R * T)

            dPdrholim = 0.1 * R * T

            'find rhomax

            Dim fx, dfdx As Double
            rhomax = rho
            i = 0
            Do
                fx = (1 + rhomax * b - 3 * rhomax ^ 2 * b ^ 2 + rhomax ^ 3 * b ^ 3) / (rhomax * R * T - rhomax ^ 2 * (a - 2 * b * R * T) + rhomax ^ 3 * (a * b - b ^ 2 * R * T))
                dfdx = (2 * a * rhomax * (b * rhomax - 1) ^ 2 * (b * rhomax + 1) - R * T * (-b ^ 2 * rhomax ^ 2 + 2 * b * rhomax + 1) ^ 2) / (rhomax ^ 2 * (a * rhomax * (1 - b * rhomax) + R * T * (b ^ 2 * rhomax ^ 2 - 2 * b * rhomax - 1)) ^ 2)
                rhomax = rhomax - 0.7 * fx / dfdx
                If rhomax < 0 Then rhomax = -rhomax
                i += 1
            Loop Until Math.Abs(fx) < 0.000001 Or i = 100

            'find rhomin

            rhomin = 0.1
            i = 0
            Do
                fx = (rhomin * R * T - rhomin ^ 2 * (a - 2 * b * R * T) + rhomin ^ 3 * (a * b - b ^ 2 * R * T)) / (1 + rhomin * b - 3 * rhomin ^ 2 * b ^ 2 + rhomin ^ 3 * b ^ 3)
                dfdx = (R * T * (-b ^ 2 * rhomin ^ 2 + 2 * b * rhomin + 1) ^ 2 - 2 * a * rhomin * (b * rhomin - 1) ^ 2 * (b * rhomin + 1)) / (b ^ 3 * rhomin ^ 3 - 3 * b ^ 2 * rhomin ^ 2 + b * rhomin + 1) ^ 2
                rhomin = rhomin - 0.7 * fx / dfdx
                If rhomin < 0 Then rhomin = -rhomin
                i += 1
            Loop Until Math.Abs(fx) < 0.000001 Or i = 100

            'find rhomc

            i = 0
            rhomc = (rhomax - rhomin) / 2
            Do
                fx = -(2 * (b * R * T * (b ^ 2 * rhomc ^ 2 - 2 * b * rhomc - 1) ^ 3 - a * (b * rhomc - 1) ^ 3 * (2 * b ^ 3 * rhomc ^ 3 + 3 * b ^ 2 * rhomc ^ 2 + 1))) / (b ^ 3 * rhomc ^ 3 - 3 * b ^ 2 * rhomc ^ 2 + b * rhomc + 1) ^ 3
                dfdx = (6 * b * (b * R * T * (-b ^ 2 * rhomc ^ 2 + 2 * b * rhomc + 1) ^ 4 - 2 * a * (b * rhomc - 1) ^ 4 * (b ^ 4 * rhomc ^ 4 + 2 * b ^ 3 * rhomc ^ 3 + 2 * b * rhomc - 1))) / (b ^ 3 * rhomc ^ 3 - 3 * b ^ 2 * rhomc ^ 2 + b * rhomc + 1) ^ 4
                rhomc = rhomc - 0.7 * fx / dfdx
                i += 1
            Loop Until Math.Abs(fx) < 0.000001 Or i = 100

            dPdrho = (R * T * (-b ^ 2 * rho ^ 2 + 2 * b * rho + 1) ^ 2 - 2 * a * rho * (b * rho - 1) ^ 2 * (b * rho + 1)) / (b ^ 3 * rho ^ 3 - 3 * b ^ 2 * rho ^ 2 + b * rho + 1) ^ 2

            If phaselabel = "L" Then
                If dPdrho > dPdrholim And rho > rhomc Then
                    Return New Double() {Z, P}
                End If
            Else
                If dPdrho > dPdrholim Then
                    Return New Double() {Z, P}
                End If
            End If

            If phaselabel = "L" Then

                'find rho*, P*

                i = 0
                rho_ = rhomc * 1.1
                Do
                    fx = -0.1 * R * T + (R * T * (-b ^ 2 * rho_ ^ 2 + 2 * b * rho_ + 1) ^ 2 - 2 * a * rho_ * (b * rho_ - 1) ^ 2 * (b * rho_ + 1)) / (b ^ 3 * rho_ ^ 3 - 3 * b ^ 2 * rho_ ^ 2 + b * rho_ + 1) ^ 2
                    dfdx = -(2 * (b * R * T * (b ^ 2 * rho_ ^ 2 - 2 * b * rho_ - 1) ^ 3 - a * (b * rho_ - 1) ^ 3 * (2 * b ^ 3 * rho_ ^ 3 + 3 * b ^ 2 * rho_ ^ 2 + 1))) / (b ^ 3 * rho_ ^ 3 - 3 * b ^ 2 * rho_ ^ 2 + b * rho_ + 1) ^ 3
                    rho_ = rho_ - fx / dfdx
                    If rho_ < rhomc Then rho_ = rhomc * 1.02
                    i += 1
                Loop Until Math.Abs(fx) < 0.000001 Or i = 100

                P_ = (rho_ * R * T - rho_ ^ 2 * (a - 2 * b * R * T) + rho_ ^ 3 * (a * b - b ^ 2 * R * T)) / (1 + rho_ * b - 3 * rho_ ^ 2 * b ^ 2 + rho_ ^ 3 * b ^ 3)

                C1 = 0.1 * R * T * (rho_ - 0.7 * rhomc)

                C0 = P_ - C1 * Math.Log(rho_ - 0.7 * rhomc)

                rho = 0.7 * rhomc + Math.Exp((P - C0) / C1)

                Pnew = (rho * R * T - rho ^ 2 * (a - 2 * b * R * T) + rho ^ 3 * (a * b - b ^ 2 * R * T)) / (1 + rho * b - 3 * rho ^ 2 * b ^ 2 + rho ^ 3 * b ^ 3)

                Zcorr = Pnew / (rho * R * T)

                If Double.IsNaN(Zcorr) Or Double.IsNaN(Pnew) Or Double.IsInfinity(Zcorr) Or Double.IsInfinity(Pnew) Then
                    Return New Double() {Z, P}
                Else
                    If Zcorr < 0.0# Or Pnew < 0.0# Then
                        Return New Double() {Z, P}
                    Else
                        Return New Double() {Zcorr, Pnew}
                    End If
                End If

            Else

                'find rho*, P*

                i = 0
                rho_ = rhomc * 0.9
                Do
                    fx = -0.1 * R * T + (R * T * (-b ^ 2 * rho_ ^ 2 + 2 * b * rho_ + 1) ^ 2 - 2 * a * rho_ * (b * rho_ - 1) ^ 2 * (b * rho_ + 1)) / (b ^ 3 * rho_ ^ 3 - 3 * b ^ 2 * rho_ ^ 2 + b * rho_ + 1) ^ 2
                    dfdx = -(2 * (b * R * T * (b ^ 2 * rho_ ^ 2 - 2 * b * rho_ - 1) ^ 3 - a * (b * rho_ - 1) ^ 3 * (2 * b ^ 3 * rho_ ^ 3 + 3 * b ^ 2 * rho_ ^ 2 + 1))) / (b ^ 3 * rho_ ^ 3 - 3 * b ^ 2 * rho_ ^ 2 + b * rho_ + 1) ^ 3
                    rho_ = rho_ - fx / dfdx
                    If rho_ < rhomc Then rho_ = rhomc * 0.98
                    i += 1
                Loop Until Math.Abs(fx) < 0.000001 Or i = 100

                P_ = (rho_ * R * T - rho_ ^ 2 * (a - 2 * b * R * T) + rho_ ^ 3 * (a * b - b ^ 2 * R * T)) / (1 + rho_ * b - 3 * rho_ ^ 2 * b ^ 2 + rho_ ^ 3 * b ^ 3)

                rho2 = (rho_ + rhomc) / 2

                C0 = -2 * rho_ * rho2 ^ 2 / (0.1 * R * T * (rho_ ^ 2 - rho2 ^ 2))

                C1 = 2 * rho_ / (0.1 * R * T * (rho_ ^ 2 - rho2 ^ 2))

                rho = (((1 / P) - C0) / C1) ^ 0.5

                Pnew = (rho * R * T - rho ^ 2 * (a - 2 * b * R * T) + rho ^ 3 * (a * b - b ^ 2 * R * T)) / (1 + rho * b - 3 * rho ^ 2 * b ^ 2 + rho ^ 3 * b ^ 3)

                Zcorr = Pnew / (rho * R * T)

                If Double.IsNaN(Zcorr) Or Double.IsNaN(Pnew) Or Double.IsInfinity(Zcorr) Or Double.IsInfinity(Pnew) Then
                    Return New Double() {Z, P}
                Else
                    If Zcorr < 0.0# Or Pnew < 0.0# Then
                        Return New Double() {Z, P}
                    Else
                        Return New Double() {Zcorr, Pnew}
                    End If
                End If

            End If

            'PR EOS P=f(rho) derivatives
            'P = (rho * R * T - rho ^ 2 * (a - 2 * b * R * T) + rho ^ 3 * (a * b - b ^ 2 * R * T)) / (1 + rho * b - 3 * rho ^ 2 * b ^ 2 + rho ^ 3 * b ^ 3)
            'dPdrho = (R * T * (-b ^ 2 * rho ^ 2 + 2 * b * rho + 1) ^ 2 - 2 * a * rho * (b * rho - 1) ^ 2 * (b * rho + 1)) / (b ^ 3 * rho ^ 3 - 3 * b ^ 2 * rho ^ 2 + b * rho + 1) ^ 2
            'd2Pdrho2 = -(2 * (b * R * T * (b ^ 2 * rho ^ 2 - 2 * b * rho - 1) ^ 3 - a * (b * rho - 1) ^ 3 * (2 * b ^ 3 * rho ^ 3 + 3 * b ^ 2 * rho ^ 2 + 1))) / (b ^ 3 * rho ^ 3 - 3 * b ^ 2 * rho ^ 2 + b * rho + 1) ^ 3
            'd3Pdrho3 = (6 * b * (b * R * T * (-b ^ 2 * rho ^ 2 + 2 * b * rho + 1) ^ 4 - 2 * a * (b * rho - 1) ^ 4 * (b ^ 4 * rho ^ 4 + 2 * b ^ 3 * rho ^ 3 + 2 * b * rho - 1))) / (b ^ 3 * rho ^ 3 - 3 * b ^ 2 * rho ^ 2 + b * rho + 1) ^ 4


        End Function

    End Class

End Namespace

