'    SRK Property Package 
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
Imports DWSIM.Thermodynamics.PropertyPackages.ThermoPlugs.SRK
Imports DWSIM.MathOps.MathEx.PolySolve

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class SRK

        Dim m_pr As New PROPS
        Private _ip As Dictionary(Of String, Dictionary(Of String, PR_IPData))

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, PR_IPData))
            Get
                Return _ip
            End Get
        End Property

        Sub New()
            _ip = New Dictionary(Of String, Dictionary(Of String, PR_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim srkip As PR_IPData
            Dim srkipc() As PR_IPData
            Dim fh1 As New FileHelperEngine(Of PR_IPData)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.srk_ip.dat")
                Using t As New IO.StreamReader(filestr)
                    srkipc = fh1.ReadStream(t)
                End Using
            End Using

            Dim csdb As New ChemSepHelper.ChemSepIDConverter
            For Each srkip In srkipc
                srkip.Owner = Me
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(srkip.ID1)) Then
                    If Me.InteractionParameters(csdb.GetDWSIMName(srkip.ID1)).ContainsKey(csdb.GetDWSIMName(srkip.ID2)) Then
                    Else
                        Me.InteractionParameters(csdb.GetDWSIMName(srkip.ID1)).Add(csdb.GetDWSIMName(srkip.ID2), srkip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(srkip.ID1), New Dictionary(Of String, PR_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(srkip.ID1)).Add(csdb.GetDWSIMName(srkip.ID2), srkip.Clone)
                End If
            Next
            For Each srkip In srkipc
                srkip.Owner = Me
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(srkip.ID1)) Then
                    If Me.InteractionParameters(csdb.GetCSName(srkip.ID1)).ContainsKey(csdb.GetCSName(srkip.ID2)) Then
                    Else
                        Me.InteractionParameters(csdb.GetCSName(srkip.ID1)).Add(csdb.GetCSName(srkip.ID2), srkip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(srkip.ID1), New Dictionary(Of String, PR_IPData))
                    Me.InteractionParameters(csdb.GetCSName(srkip.ID1)).Add(csdb.GetCSName(srkip.ID2), srkip.Clone)
                End If
            Next
            srkip = Nothing
            srkipc = Nothing
            fh1 = Nothing
        End Sub

        Function bi(ByVal omega As Double, ByVal Tc As Double, ByVal Pc As Double) As Double

            Return omega * 8.314 * Tc / Pc

        End Function

        Function Zc1(ByVal w As Double) As Double

            Zc1 = 0.291 - 0.08 * w

        End Function

        Function Z_SRK(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VTc, ByVal VPc, ByVal Vw, ByVal TIPO)

            Calculator.WriteToConsole("SRK cubic equation root finder (Z) for T = " & T & " K, P = " & P & " Pa and Phase = " & TIPO, 3)
            Calculator.WriteToConsole("Mole fractions: " & DirectCast(Vx, Double()).ToArrayString, 3)

            Dim ai(), bi(), aml2(), amv2() As Double
            Dim n, R, coeff(3), tmp() As Double
            Dim Tc(), Pc(), W(), alpha(), Vant(0, 4), m(), a(,), b(,), Tr() As Double

            n = Vx.Length - 1

            ReDim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n)
            ReDim aml2(n), amv2(n)
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
            Dim aml = 0.0#
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
            Dim bml = 0.0#
            Do
                bml = bml + Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            Dim AG = aml * P / (R * T) ^ 2
            Dim BG = bml * P / (R * T)

            coeff(0) = -AG * BG
            coeff(1) = AG - BG - BG ^ 2
            coeff(2) = -1
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

            Z_SRK = 0
            If TIPO = "L" Then
                Z_SRK = temp1(0, 0)
            ElseIf TIPO = "V" Then
                Z_SRK = temp1(2, 0)
            End If

            Calculator.WriteToConsole("Result: Z = " & Z_SRK, 3)

        End Function

        Function H_SRK_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal Tc As Double(), ByVal Pc As Double(), ByVal w As Double(), ByVal VMM As Double(), ByVal Hid As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "H_SRK_MIX", "SRK EOS Enthalpy", "SRK EOS Enthalpy Calculation Routine")

            IObj?.Paragraphs.Add("The SRK equation is a cubic Equation of State (characteristic related to the exponent of the molar volume) 
                                    which relates temperature, pressure And molar volume of a pure component or a mixture of components at equilibrium. The cubic 
                                    equations are, in fact, The simplest equations capable of representing The behavior of liquid And vapor phases simultaneously.
                                    The SRK EOS is written in the following form")
            IObj?.Paragraphs.Add("<math>P=\frac{RT}{(V-b)}-\frac{a(T)}{V(V+b)}<math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>P</math_inline> pressure")
            IObj?.Paragraphs.Add("<math_inline>R</math_inline> ideal gas universal constant")
            IObj?.Paragraphs.Add("<math_inline>v</math_inline> molar volume")
            IObj?.Paragraphs.Add("<math_inline>b</math_inline> parameter related to hard-sphere volume")
            IObj?.Paragraphs.Add("<math_inline>a</math_inline> parameter related to intermolecular forces")
            IObj?.Paragraphs.Add("For pure substances, the a and b parameters are given by:")
            IObj?.Paragraphs.Add("<math>a(T)=[1+(0.48+1.574\omega-0.176\omega^{2})(1-T_{r}^{(1/2)})]^{2}0.42747(R^{2}T_{c}^{2})/P_{c}</math>")
            IObj?.Paragraphs.Add("<math>b=0.08664(RT_{c})/P_{c}</math>")
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
            IObj?.Paragraphs.Add("<math>\ln\dfrac{f_{i}}{x_{i}P}=\frac{b_{i}}{b_{m}}(Z-1)-\ln(Z-B)-\frac{A}{B}(\frac{2\sum_{k}x_{k}a_{ki}}{a_{m}}-\frac{b_{i}}{b_{m}})\ln(\frac{Z+B}{Z}),</math>")
            IObj?.Paragraphs.Add("where Z Is the phase compressibility factor (liquid or vapor) and can be obtained from the equation")
            IObj?.Paragraphs.Add("<math>Z^{3}-Z^{2}+(A-B-B^{2})Z-AB=0,</math>")
            IObj?.Paragraphs.Add("<math>A=\frac{a_{m}P}{R^{2}T^{2}}</math>")
            IObj?.Paragraphs.Add("<math>B=\frac{b_{m}P}{RT}</math>")
            IObj?.Paragraphs.Add("<math>Z=\frac{PV}{RT}</math>")

            IObj?.Paragraphs.Add("For the SRK EOS, Enthalpy is calculated by a departure function, which represents the difference between the current state and the ideal gas state.")

            IObj?.Paragraphs.Add("<math>\frac{H-H^{id}}{RT}=Z-1-\frac{1}{bRT}[a-T\frac{da}{dT}]\ln[1+\frac{b}{V}]</math>")

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

            Dim n As Integer, R As Double
            n = Vz.Length - 1

            Dim ai(n), bi(n), ci(n) As Double
            Dim Vc(n), Zc(n), alpha(n), m(n), a(n, n), b(n, n), Z As Double

            Dim i, j As Integer
            Dim dadt As Double

            R = 8.314

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                ci(i) = 0.48 + 1.574 * w(i) - 0.176 * Math.Pow(w(i), 2)
                alpha(i) = Math.Pow(1 + ci(i) * (1 - Math.Sqrt(T / Tc(i))), 2)
                ai(i) = 0.42748 * alpha(i) * Math.Pow(R * Tc(i), 2) / Pc(i)
                bi(i) = 0.08664 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = Math.Sqrt(ai(i) * ai(j)) * (1 - VKij(i, j))
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

            IObj?.Paragraphs.Add("<math_inline>a_{i}</math_inline>: " & ai.ToMathArrayString)
            IObj?.Paragraphs.Add("<math_inline>b_{i}</math_inline>: " & bi.ToMathArrayString)

            IObj?.Paragraphs.Add("<math_inline>a_{m}</math_inline>: " & am)
            IObj?.Paragraphs.Add("<math_inline>b_{m}</math_inline>: " & bm)

            Dim AG1 = am * P / Math.Pow(R * T, 2)
            Dim BG1 = bm * P / (R * T)

            IObj?.Paragraphs.Add(String.Format("<math_inline>A</math_inline>: {0}", AG1))
            IObj?.Paragraphs.Add(String.Format("<math_inline>B</math_inline>: {0}", BG1))

            Dim coeff(3) As Double

            coeff(0) = -AG1 * BG1
            coeff(1) = AG1 - BG1 - BG1 * BG1
            coeff(2) = -1
            coeff(3) = 1

            Dim _mingz As Double()

            Dim _zarray As List(Of Double) = CalcZ2(AG1, BG1)

            If _zarray.Count = 0 Then
                Throw New Exception("SRK EOS: unable to calculate a valid compressibility factor.")
            End If

            If TIPO <> "" Then
                If TIPO = "L" Then
                    Z = _zarray.Min
                ElseIf TIPO = "V" Then
                    Z = _zarray.Max
                End If
            Else
                _mingz = ZtoMinG(_zarray.ToArray, T, P, Vz, VKij, Tc, Pc, w)
                Z = _zarray(_mingz(0))
            End If

            IObj?.Paragraphs.Add(String.Format("<math_inline>Z</math_inline>: {0}", Z))

            Dim V = (Z * R * T / P) ' m3/mol

            IObj?.Paragraphs.Add(String.Format("<math_inline>V</math_inline>: {0}", V))

            Dim tmp1 = MMm / V / 1000

            Dim aux1, aux2, auxtmp(n) As Double
            aux1 = -R / 2 * Math.Sqrt(0.42748 / T)

            i = 0
            Do
                j = 0
                Do
                    aux2 += Vz(i) * Vz(j) * (1 - VKij(i, j)) * (ci(j) * Math.Sqrt(ai(i) * Tc(j) / Pc(j)) + ci(i) * Math.Sqrt(ai(j) * Tc(i) / Pc(i)))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadt = aux1 * aux2

            Dim uu, ww As Double
            uu = 1
            ww = 0

            Dim DAres = am / (bm * Math.Sqrt(uu ^ 2 - 4 * ww)) * Math.Log((2 * Z + BG1 * (uu - Math.Sqrt(uu ^ 2 - 4 * ww))) / (2 * Z + BG1 * (uu + Math.Sqrt(uu ^ 2 - 4 * ww)))) - R * T * Math.Log((Z - BG1) / Z) - R * T * Math.Log(Z)
            Dim V0 As Double = R * 298.15 / 101325
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / (Math.Sqrt(uu ^ 2 - 4 * ww) * bm) * dadt * Math.Log((2 * Z + BG1 * (uu - Math.Sqrt(uu ^ 2 - 4 * ww))) / (2 * Z + BG1 * (uu + Math.Sqrt(uu ^ 2 - 4 * ww))))
            Dim DHres = DAres + T * (DSres) + R * T * (Z - 1)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Calculated Enthalpy Departure: {0} kJ/kmol", DHres))
            IObj?.Paragraphs.Add(String.Format("Calculated Enthalpy Departure: {0} kJ/kg", DHres / MMm))

            If Vz.Sum = 0.0# Then
                H_SRK_MIX = 0.0#
            Else
                IObj?.Paragraphs.Add(String.Format("Calculated Total Enthalpy (Ideal + Departure): {0} kJ/kg", Hid + DHres / MMm))
                H_SRK_MIX = Hid + DHres / MMm
            End If

            IObj?.Close()

        End Function

        Function S_SRK_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal VMM As Double(), ByVal Sid As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "H_SRK_MIX", "SRK EOS Enthalpy", "SRK EOS Enthalpy Calculation Routine")

            IObj?.Paragraphs.Add("The SRK equation is a cubic Equation of State (characteristic related to the exponent of the molar volume) 
                                    which relates temperature, pressure And molar volume of a pure component or a mixture of components at equilibrium. The cubic 
                                    equations are, in fact, The simplest equations capable of representing The behavior of liquid And vapor phases simultaneously.
                                    The SRK EOS is written in the following form")
            IObj?.Paragraphs.Add("<math>P=\frac{RT}{(V-b)}-\frac{a(T)}{V(V+b)}<math>")
            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<math_inline>P</math_inline> pressure")
            IObj?.Paragraphs.Add("<math_inline>R</math_inline> ideal gas universal constant")
            IObj?.Paragraphs.Add("<math_inline>v</math_inline> molar volume")
            IObj?.Paragraphs.Add("<math_inline>b</math_inline> parameter related to hard-sphere volume")
            IObj?.Paragraphs.Add("<math_inline>a</math_inline> parameter related to intermolecular forces")
            IObj?.Paragraphs.Add("For pure substances, the a and b parameters are given by:")
            IObj?.Paragraphs.Add("<math>a(T)=[1+(0.48+1.574\omega-0.176\omega^{2})(1-T_{r}^{(1/2)})]^{2}0.42747(R^{2}T_{c}^{2})/P_{c}</math>")
            IObj?.Paragraphs.Add("<math>b=0.08664(RT_{c})/P_{c}</math>")
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
            IObj?.Paragraphs.Add("<math>\ln\dfrac{f_{i}}{x_{i}P}=\frac{b_{i}}{b_{m}}(Z-1)-\ln(Z-B)-\frac{A}{B}(\frac{2\sum_{k}x_{k}a_{ki}}{a_{m}}-\frac{b_{i}}{b_{m}})\ln(\frac{Z+B}{Z}),</math>")
            IObj?.Paragraphs.Add("where Z Is the phase compressibility factor (liquid or vapor) and can be obtained from the equation")
            IObj?.Paragraphs.Add("<math>Z^{3}-Z^{2}+(A-B-B^{2})Z-AB=0,</math>")
            IObj?.Paragraphs.Add("<math>A=\frac{a_{m}P}{R^{2}T^{2}}</math>")
            IObj?.Paragraphs.Add("<math>B=\frac{b_{m}P}{RT}</math>")
            IObj?.Paragraphs.Add("<math>Z=\frac{PV}{RT}</math>")

            IObj?.Paragraphs.Add("For the SRK EOS, Entropy is calculated by a departure function, which represents the difference between the current state and the ideal gas state.")

            IObj?.Paragraphs.Add("<math>\frac{S-S^{id}}{RT}=\ln(Z-B)-\ln\frac{P}{P^{0}}-\frac{A}{B}[\frac{T}{a}\frac{da}{dT}]\ln[1+\frac{B}{Z}]</math>")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Ideal Entropy (Ideal Gas State): {0} kJ/kg", Sid))
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
                alpha(i) = (1 + (0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.42748 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.08664 * R * Tc(i) / Pc(i)
                ci(i) = 0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2
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

            coeff(0) = -AG1 * BG1
            coeff(1) = AG1 - BG1 - BG1 ^ 2
            coeff(2) = -1
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

            Dim aux1 = -R / 2 * (0.42748 / T) ^ 0.5
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
            uu = 1
            ww = 0

            Dim V0 As Double = R * 298.15 / 101325
            'Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(V * 101325 / (R * 298.15)) - 1 / ((uu ^ 2 - 4 * ww) ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5)))
            Dim DSres = R * Math.Log((Z - BG1) / Z) + R * Math.Log(Z) - 1 / ((uu ^ 2 - 4 * ww) ^ 0.5 * bm) * dadT * Math.Log((2 * Z + BG1 * (uu - (uu ^ 2 - 4 * ww) ^ 0.5)) / (2 * Z + BG1 * (uu + (uu ^ 2 - 4 * ww) ^ 0.5)))

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Calculated Entropy Departure: {0} kJ/kmol", DSres))
            IObj?.Paragraphs.Add(String.Format("Calculated Entropy Departure: {0} kJ/kg", DSres / MMm))

            If MathEx.Common.Sum(Vz) = 0.0# Then
                S_SRK_MIX = 0.0#
            Else
                IObj?.Paragraphs.Add(String.Format("Calculated Total Entropy (Ideal + Departure): {0} kJ/kg", Sid + DSres / MMm))
                S_SRK_MIX = Sid + DSres / MMm '/ 1000
            End If

            IObj?.Close()

        End Function

        Function G_SRK_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Array, ByVal VKij As Object, ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, ByVal VMM As Array, ByVal Sid As Double, ByVal Hid As Double) As Double

            Dim h As Double = H_SRK_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Hid)
            Dim s As Double = S_SRK_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, Sid)

            Return h - T * s

        End Function

        Function CpCvR(ByVal TIPO, ByVal T, ByVal P, ByVal Vz, ByVal VKij, ByVal Vzmass, ByVal VTc, ByVal VPc, ByVal VCpig, ByVal VMM, ByVal Vw, ByVal VZRa)

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
                alpha(i) = (1 + (0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                ai(i) = 0.42748 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                bi(i) = 0.08664 * R * Tc(i) / Pc(i)
                ci(i) = 0.48 + 1.574 * w(i) - 0.176 * w(i) ^ 2
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

            coeff(0) = -AG1 * BG1
            coeff(1) = AG1 - BG1 - BG1 ^ 2
            coeff(2) = -1
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

                If TIPO = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                ElseIf TIPO = "V" Then
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

                If TIPO = "V" Then

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

            Dim tmp1 = MMm / V / 1000

            Dim aux1 = -R / 2 * (0.42748 / T) ^ 0.5
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
            Dim d2adt2 = R / 4 * (0.42748 / T) ^ 0.5 * (1 / T) * aux2
            'Dim d2adt2 = 0.169049 * R / (T ^ (3 / 2))

            Dim dP_dT_V = R / (V - bm) - dadT / (V ^ 2 + V * bm)

            Dim dV_dT_P = dP_dT_V / (R * T / (V - bm) ^ 2 - am * (2 * V + 2 * bm) / (V * (V + bm) + bm * (V - bm)) ^ 2)

            Dim dP_dV_T = -R * T * (V - bm) ^ -2 - am * (V ^ 2 + V * bm) ^ -2 * (2 * V + bm)

            Dim d2P_dT2 = -1 / (V ^ 2 + V * bm) * d2adt2

            Dim var = (bm + V) / (2 ^ 0.5 * bm)

            Dim Int_d2P_dT2_V_dV = -d2adt2 * Math.Log((-(2 ^ 0.5) * bm + bm + V) / ((2 ^ 0.5) * bm + bm + V)) / (8 ^ 0.5 * bm)

            Dim Cpm_ig = 0.0#
            i = 0
            Do
                Cpm_ig += Vzmass(i) * VCpig(i) * MMm
                i += 1
            Loop Until i = n + 1

            Dim Cv = T * Int_d2P_dT2_V_dV + Cpm_ig - 2 * R - T * dP_dT_V ^ 2 / dP_dV_T
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
            'If i = 11 Then GoTo Final2
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

        Function MAX(ByVal Vv As Object, ByVal Vz As Object)

            Dim n = UBound(Vv)
            Dim mx As Double

            If n >= 1 Then
                Dim i As Integer = 1

                'Do
                '    mx = Vv(i - 1)
                '    i += 1
                'Loop Until i = n + 1

                mx = Vv(i - 1)
                i = 0
                Do
                    If Vv(i) > mx And Vz(i) <> 0 Then
                        mx = Vv(i)
                    End If
                    i += 1
                Loop Until i = n + 1

                Return mx
            Else
                Return Vv(0)
            End If

        End Function

        Function MIN(ByVal Vv As Object, ByVal Vz As Object)

            Dim n = UBound(Vv)
            Dim mx As Double

            If n >= 1 Then
                Dim i As Integer = 1

                Do
                    If Vv(i - 1) <> 0 And Vz(i - 1) <> 0 Then
                        mx = Vv(i - 1)
                        Exit Do
                    End If
                    i += 1
                Loop Until i = n + 2

                i = i - 1
                Do
                    If Vv(i) < mx And Vz(i) <> 0 Then
                        mx = Vv(i)
                    End If
                    i += 1
                Loop Until i = n + 1

                Return mx
            Else
                Return Vv(0)
            End If

        End Function

        Function GeneratePseudoRoot(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VTb, ByVal TIPO)

            Dim ai(), bi(), aml2(), amv2() As Double
            Dim n, R, coeff(3), tmp() As Double
            Dim Tc(), Pc(), W(), alpha(), Vant(0, 4), m(), a(,), b(,), Tr() As Double
            Dim beta As Double
            Dim criterioOK As Boolean = False
            Dim hbcIndex, counter As Integer
            Dim soma_x As Double
            Dim ZV As Double
            Dim AG, BG, aml, bml As Double

            n = Vx.Length - 1

            ReDim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n)
            ReDim aml2(n), amv2(n)
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

            counter = 0
            Do

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

                coeff(0) = -AG * BG
                coeff(1) = AG - BG - BG ^ 2
                coeff(2) = -1
                coeff(3) = 1

                Dim temp1 = Poly_Roots(coeff)
                Dim tv = 0.0#
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

                beta = 1 / P * (1 - (2 * AG * BG + ZV * BG + 2 * BG ^ 2 * ZV - AG * ZV) / (ZV * (3 * ZV ^ 2 - 2 * ZV + AG - BG - BG ^ 2)))

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

        Function JT_SRK(ByVal Z As Double, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VMM As Double(), ByVal VZc As Double(), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Cp As Double, ByVal Vw As Double()) As Double

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

            a = 0.42748 * R ^ 2 * Tcm ^ 2 / Pcm
            b = 0.08664 * R * Tcm / Pcm
            c = 0.48 + 1.574 * wm - 0.176 * wm ^ 2

            Dim V As Double = Z * 8.314 * T / P

            Dim dP_dT_V = c * (1 - Math.Sqrt(T / Tcm)) / (Tcm * V * (V + b) * Math.Sqrt(T / Tcm)) + R / (V - b)
            Dim dP_dV_T = (c * (1 - Math.Sqrt(T / Tcm)) ^ 2 + 1) / (V ^ 2 * (V + b)) + (c * (1 - Math.Sqrt(T / Tcm)) ^ 2 + 1) / (V * (V + b) ^ 2) - R * T / (V - b) ^ 2

            Dim JT = -(T * dP_dT_V / dP_dV_T + V) / (Cp * MMm)

            Return JT

        End Function

        Function CalcLnFug(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VTb, ByVal TIPO)

            Dim n, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim beta As Double
            Dim criterioOK As Boolean = False
            Dim ZV As Double
            Dim AG, BG, aml, bml As Double
            Dim t1, t2, t3, t4, t5 As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n)
            Dim aml2(n), amv2(n), LN_CF(n), PHI(n)
            Dim Tc(n), Pc(n), W(n), alpha(n), m(n), Tr(n)
            Dim rho, rho0, rho_mc, Tmc, dPdrho, dPdrho_, Zcalc As Double
            'Dim P_lim, rho_lim, Pcalc, rho_calc, rho_x As Double

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
                    a(i, j) = Math.Sqrt(ai(i) * ai(j)) * (1 - VKij(i, j))
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

            coeff(0) = -AG * BG
            coeff(1) = AG - BG - BG ^ 2
            coeff(2) = -1
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0.0#
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

            beta = 1 / P * (1 - (2 * AG * BG + ZV * BG + 2 * BG ^ 2 * ZV - AG * ZV) / (ZV * (3 * ZV ^ 2 - 2 * ZV + AG - BG - BG ^ 2)))

            rho0 = 1 / bml
            rho_mc = 0.2599 / bml
            Tmc = 0.20268 * aml / (R * bml)
            rho = P / (ZV * R * T)
            dPdrho_ = 0.1 * R * T
            dPdrho = bml * rho * R * T * (1 - bml * rho) ^ -2 + R * T * (1 - bml * rho) ^ -1 +
                    aml * rho ^ 2 * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -2 * (2 * bml - 2 * bml ^ 2 * rho) +
                    2 * aml * rho * (1 + 2 * bml * rho - (bml * rho) ^ 2) ^ -1

            If TIPO = "L" Then
                'Dim C0, C1 As Double
                'rho_lim = Me.ESTIMAR_RhoLim(aml, bml, T, P)
                'P_lim = R * T * rho_lim / (1 - rho_lim * bml) - aml * rho_lim ^ 2 / (1 + 2 * bml * rho_lim - (rho_lim * bml) ^ 2)
                'C1 = (rho - 0.7 * rho_mc) * dPdrho
                'C0 = P_lim - C1 * Math.Log(rho_lim - 0.7 * rho_mc)
                'rho_calc = Math.Exp((P - C0) / C1) + 0.7 * rho_mc
                'Pcalc = R * T * rho_calc / (1 - rho_calc * bml) - aml * rho_calc ^ 2 / (1 + 2 * bml * rho_calc - (rho_calc * bml) ^ 2)
                'Zcalc = P / (rho_calc * R * T)
                Zcalc = ZV
                ' CALCULO DO COEFICIENTE DE FUGACIDADE DA Phase LIQUIDA
                i = 0
                Do
                    t1 = bi(i) * (Zcalc - 1) / bml
                    t2 = -Math.Log(Zcalc - BG)
                    t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                    t4 = Math.Log((Zcalc + BG) / Zcalc)
                    t5 = BG
                    LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                    i = i + 1
                Loop Until i = n + 1
                Return LN_CF
            Else
                'Dim aa, bb As Double
                'rho_lim = Me.ESTIMAR_RhoLim(aml, bml, T, P)
                'P_lim = R * T * rho_lim / (1 - rho_lim * bml) - aml * rho_lim ^ 2 / (1 + 2 * bml * rho_lim - (rho_lim * bml) ^ 2)
                'rho_x = (rho_lim + rho_mc) / 2
                'bb = 1 / P_lim * (1 / (rho_lim * (1 - rho_lim / rho_x)))
                'aa = -bb / rho_x
                'rho_calc = (1 / P + bb) / aa
                'Pcalc = R * T * rho_calc / (1 - rho_calc * bml) - aml * rho_calc ^ 2 / (1 + 2 * bml * rho_calc - (rho_calc * bml) ^ 2)
                'Zcalc = P / (rho_calc * R * T)
                Zcalc = ZV
                ' CALCULO DO COEFICIENTE DE FUGACIDADE DA Phase VAPOR
                i = 0
                Do
                    t1 = bi(i) * (Zcalc - 1) / bml
                    t2 = -Math.Log(Zcalc - BG)
                    t3 = AG * (2 * aml2(i) / aml - bi(i) / bml)
                    t4 = Math.Log((Zcalc + BG) / Zcalc)
                    t5 = BG
                    LN_CF(i) = t1 + t2 - (t3 * t4 / t5)
                    i = i + 1
                Loop Until i = n + 1
                Return LN_CF
            End If

        End Function

        Function CalcPartialVolume(ByVal T, ByVal P, ByVal Vx, ByVal VKij, ByVal VTc, ByVal VPc, ByVal Vw, ByVal VTb, ByVal TIPO, ByVal deltaP)

            Dim lnfug1, lnfug2 As Object
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

    End Class



End Namespace


