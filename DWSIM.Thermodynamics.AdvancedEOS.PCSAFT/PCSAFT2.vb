Imports System.Math
Imports System.Linq
Imports DWSIM.ExtensionMethods


Namespace DWSIM.Thermodynamics.AdvancedEOS

    Public Class PCSAFTResult

        Public Property Z As Double

        Public Property Ar As Double

        Public Property Hr As Double

        Public Property Sr As Double

        Public Property Cp As Double

        Public Property Cv As Double

        Public Property W As Double

        Public Property JT As Double

    End Class

    Public Class mixture

        Public Property comp As New List(Of pccompound)

        Public Property x As Double()

        Public Property k1 As Double(,)

        Public Property numC As Integer

        Public Property MW As Double

    End Class

    Public Class pccompound

        Public Property EosParam() As Object

    End Class

    Public Class PCSAFT2

        Public Property mix As mixture

        Private numC As Integer

        Public Sub New(pp As PCSAFT2PropertyPackage, molefractions() As Double)


            'InitTests()

            InitPP(pp, molefractions)

        End Sub

        Private Sub InitTests()

            numC = 2 'compounds.Count

            Dim CO2 As New pccompound
            CO2.EosParam = New List(Of Object)
            CO2.EosParam.Add(0.0)
            CO2.EosParam.Add(2.0729) 'm
            CO2.EosParam.Add(2.7852) 'sigma
            CO2.EosParam.Add(169.21) 'epsilon/k
            CO2.EosParam.Add(0) 'NumAss
            CO2.EosParam.Add(New Double(,) {})
            CO2.EosParam.Add(New Double(,) {})

            Dim H2O As New pccompound
            H2O.EosParam = New List(Of Object)
            H2O.EosParam.Add(0.0)
            H2O.EosParam.Add(1.09528) 'm
            H2O.EosParam.Add(2.8898) 'sigma
            H2O.EosParam.Add(365.956) 'epsilon/k
            H2O.EosParam.Add(2) 'NumAss
            H2O.EosParam.Add(New Double(,) {{0.0, 0.0, 0.0}, {0.0, 0.0, 0.03487}, {0.0, 0.03487, 0.0}})
            H2O.EosParam.Add(New Double(,) {{0.0, 0.0, 0.0}, {0.0, 0.0, 2515.7}, {0.0, 2515.7, 0.0}})

            mix = New mixture
            mix.numC = 2
            mix.comp.Add(Nothing)
            mix.comp.Add(CO2)
            mix.comp.Add(H2O)
            mix.x = New Double() {0.0, 0.014971914105686482, 0.98502808589431357}

            mix.MW = 18

            mix.k1 = New Double(,) {{0.0, 0.0, 0.0}, {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0}}

        End Sub

        Private Sub InitPP(pp As PCSAFT2PropertyPackage, molefractions() As Double)

            Dim i, j As Integer

            Dim compounds = pp.DW_GetConstantProperties

            numC = compounds.Count

            mix = New mixture
            mix.numC = compounds.Count
            mix.comp.Add(Nothing)
            mix.x = zeros(molefractions.Length)
            molefractions.CopyTo(mix.x, 1)

            mix.MW = pp.AUX_MMM(molefractions)

            Dim nk As Double = numC

            Dim kvec(nk, nk) As Double

            For i = 1 To nk
                For j = 1 To nk
                    If pp.InteractionParameters.ContainsKey(compounds(i - 1).CAS_Number) Then
                        If pp.InteractionParameters(compounds(i - 1).CAS_Number).ContainsKey(compounds(j - 1).CAS_Number) Then
                            kvec(i, j) = pp.InteractionParameters(compounds(i - 1).CAS_Number)(compounds(j - 1).CAS_Number).kij
                            kvec(j, i) = kvec(i, j)
                        End If
                    ElseIf pp.InteractionParameters.ContainsKey(compounds(j - 1).CAS_Number) Then
                        If pp.InteractionParameters(compounds(j - 1).CAS_Number).ContainsKey(compounds(i - 1).CAS_Number) Then
                            kvec(i, j) = pp.InteractionParameters(compounds(j - 1).CAS_Number)(compounds(i - 1).CAS_Number).kij
                            kvec(j, i) = kvec(i, j)
                        End If
                    End If
                Next
            Next

            mix.k1 = kvec

            Dim assocparam, assocparaml(), vm, em As String
            Dim na As Integer

            For Each c In compounds

                Dim cproxy As New pccompound

                cproxy.EosParam = New List(Of Object)
                cproxy.EosParam.Add(0.0)
                cproxy.EosParam.Add(pp.CompoundParameters(c.CAS_Number).m) 'm
                cproxy.EosParam.Add(pp.CompoundParameters(c.CAS_Number).sigma) 'sigma
                cproxy.EosParam.Add(pp.CompoundParameters(c.CAS_Number).epsilon) 'epsilon/k

                assocparam = pp.CompoundParameters(c.CAS_Number).associationparams

                If assocparam <> "" Then

                    assocparaml = assocparam.Split(vbCrLf)
                    na = Integer.Parse(assocparam(0))
                    vm = assocparaml(1).Trim().Trim(vbLf).Trim("[", "]")
                    em = assocparaml(2).Trim().Trim(vbLf).Trim("[", "]")

                    cproxy.EosParam.Add(na) 'NumAss

                    Dim vmvec(na, na), emvec(na, na) As Double

                    i = 1
                    For Each line In vm.Split(";")
                        j = 1
                        For Each value In line.Trim().Split(" ")
                            vmvec(i, j) = value.ToDoubleFromInvariant
                            j += 1
                        Next
                        i += 1
                    Next

                    cproxy.EosParam.Add(vmvec)

                    i = 1
                    For Each line In em.Split(";")
                        j = 1
                        For Each value In line.Trim().Split(" ")
                            emvec(i, j) = value.ToDoubleFromInvariant
                            j += 1
                        Next
                        i += 1
                    Next

                    cproxy.EosParam.Add(emvec)

                    If sum2(vmvec) + sum2(emvec) = 0.0 Then
                        cproxy.EosParam(4) = 0
                    End If

                Else

                    cproxy.EosParam.Add(0) 'NumAss
                    cproxy.EosParam.Add(New Double(,) {})
                    cproxy.EosParam.Add(New Double(,) {})

                End If

                mix.comp.Add(cproxy)

            Next

        End Sub

        Public Function CalcFugCoeff(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double()

            Return FugF(T, P, mix, liq_or_gas, Zestimate)

        End Function

        Public Function CalcZ(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim Z = compr(T, P, mix, liq_or_gas, Zestimate)

            Return Z

        End Function

        Public Function CalcCp(T As Double, P As Double, liq_or_gas As String, Zestimate As Double, HidFunc As Func(Of Double, Double)) As Double

            Dim h = 0.1

            Dim h1, h2 As Double
            Dim t1, t2 As Task

            t1 = New Task(Sub() h1 = CalcHr(T, P, liq_or_gas, Zestimate) + HidFunc.Invoke(T) * mix.MW)
            t2 = New Task(Sub() h2 = CalcHr(T + h, P, liq_or_gas, Zestimate) + HidFunc.Invoke(T + h) * mix.MW)

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim cp = (h2 - h1) / h

            Return cp / mix.MW

        End Function

        Public Function CalcCv2(T As Double, P As Double, liq_or_gas As String, Zestimate As Double, Cp As Double) As Double

            Dim R = 8.314

            Dim timespans As New List(Of TimeSpan)

            Dim d1 As Date = Date.Now

            Dim Z = compr(T, P, mix, liq_or_gas, Zestimate)

            timespans.Add(Date.Now - d1)
            d1 = Date.Now

            Dim V = Z * R * T / P

            Dim dFdV, d2FdV2, dPdV, dPdT As Double

            dFdV = CalcdFdV(T, P, liq_or_gas, Z)

            Dim Pcheck = -R * T * dFdV + R * T / V

            Dim dFdVcheck = (P - R * T / V) / (-R * T)

            d2FdV2 = CalcdF2dV2(T, P, liq_or_gas, Z)

            dPdV = -R * T * d2FdV2 - R * T / V ^ 2

            dPdT = -R * T * Calcd2FdTdV(T, P, liq_or_gas, Z) + P / T

            Dim Cv = Cp + T * dPdT ^ 2 / dPdV / mix.MW

            Return Cv

        End Function

        Public Function CalcCv(T As Double, P As Double, liq_or_gas As String, Zestimate As Double, SidFunc As Func(Of Double, Double, Double)) As Double

            Dim epsilon = If(liq_or_gas = "gas", 0.1, 0.0001)

            Dim R = 8.314

            Dim Z = compr(T, P, mix, liq_or_gas, Zestimate)

            Dim P2, Z2, Z2_ant, P2_ant, fP As Double

            Dim V = Z * R * T / P

            Dim nloops As Integer = 0

            P2 = P
            Z2 = Z

            If liq_or_gas = "gas" Then

                Do

                    Z2_ant = Z2
                    Z2 = compr(T + epsilon, P2, mix, liq_or_gas, Z2_ant)

                    fP = (Z2 - Z2_ant)

                    If Abs(fP) < 0.0000000001 Then Exit Do

                    P2_ant = P2
                    P2 = (Z2 * R * (T + epsilon) / V)

                    nloops += 1

                Loop

            Else

                P2 = P
                Z2 = compr(T + epsilon, P2, mix, liq_or_gas, Z)

            End If

            Dim t1, t2 As Task
            Dim s1, s2 As Double

            t1 = New Task(Sub() s1 = CalcSr(T, P, liq_or_gas, Z) + SidFunc.Invoke(T, P) * mix.MW)
            t2 = New Task(Sub() s2 = CalcSr(T + epsilon, P2, liq_or_gas, Z2) + SidFunc.Invoke(T + epsilon, P2) * mix.MW)

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim cv = (s2 - s1) * T / epsilon

            Return cv / mix.MW

        End Function

        Public Function CalcHr(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim t1, t2, t3, t4 As Task

            Dim R = 8.314

            Dim epsilon = 0.01

            Dim Ar, Ar2, Z, Z2 As Double

            t1 = New Task(Sub() Z = compr(T, P, mix, liq_or_gas, Zestimate))

            t2 = New Task(Sub() Z2 = compr(T + epsilon, P, mix, liq_or_gas, Zestimate))

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            t3 = New Task(Sub() Ar = Helmholtz(T, P, mix, liq_or_gas, Z))

            t4 = New Task(Sub() Ar2 = Helmholtz(T + epsilon, P, mix, liq_or_gas, Z2))

            t3.Start()
            t4.Start()

            Task.WaitAll(t3, t4)

            Dim dArdT = (Ar2 - Ar) / epsilon

            Return R * T * (-T * dArdT + (Z - 1)) 'kJ/kmol

        End Function

        Public Function CalcSr(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim R = 8.314

            Dim Z As Double = compr(T, P, mix, liq_or_gas, Zestimate)

            Dim Ar = Helmholtz(T, P, mix, liq_or_gas, Z)

            Dim Gr_RT = Ar + (Z - 1) - Log(Z)

            Dim Hr_RT = CalcHr(T, P, liq_or_gas, Zestimate) / (R * T)

            Dim Sr = R * (Hr_RT - Gr_RT)

            Return Sr

        End Function

        Private Function Calcd2FdT2(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim epsilon = 1.0

            Dim R = 8.314

            Dim Z = Zestimate

            Dim t1, t2 As Task

            Dim Ar, Ar2, P2, Z2, Z2_ant As Double

            Dim V = Z * R * T / P

            Dim nloops As Integer = 0

            If liq_or_gas = "gas" Then

                P2 = P
                Z2 = Z
                Do
                    Z2_ant = Z2
                    Z2 = compr(T + epsilon, P2, mix, liq_or_gas, Z2_ant)
                    P2 = (Z2 * R * (T + epsilon) / V) * 0.5 + P2 * 0.5
                    nloops += 1
                Loop Until Abs((Z2 - Z2_ant) / Z2) < 0.00001

            Else

                P2 = P
                Z2 = compr(T + epsilon, P2, mix, liq_or_gas, Z2_ant)

            End If

            t1 = New Task(Sub() Ar = CalcdFdT(T, P, liq_or_gas, Z))
            t2 = New Task(Sub() Ar2 = CalcdFdT(T + epsilon, P2, liq_or_gas, Z2))

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim dF = (Ar2 - Ar) / epsilon

            Return dF

        End Function

        Public Function CalcJT(T As Double, P As Double, liq_or_gas As String, Zestimate As Double, Cp As Double) As PCSAFTResult

            Dim R = 8.314

            Dim epsilon = 0.001

            Dim Z = compr(T, P, mix, liq_or_gas, Zestimate)

            Dim V = Z * R * T / P

            Dim d2FdV2, dPdV, dPdT As Double

            Dim t1, t2 As Task

            t1 = New Task(Sub()
                              d2FdV2 = CalcdF2dV2(T, P, liq_or_gas, Z)
                              dPdV = -R * T * d2FdV2 - R * T / V ^ 2
                          End Sub)

            t2 = New Task(Sub() dPdT = -R * T * Calcd2FdTdV(T, P, liq_or_gas, Z) + P / T)

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim results As New PCSAFTResult

            With results

                .Z = Z
                .JT = -1 / (Cp * mix.MW) * (V + T * dPdT / dPdV)

            End With

            Return results

        End Function

        Private Function CalcdFdV(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim R = 8.314

            Dim Z = Zestimate

            Dim V = Z * R * T / P

            Dim delta = If(liq_or_gas = "gas", 0.01, 0.001)

            Dim epsilon = V * delta

            Dim t1, t2 As Task

            Dim Ar, Ar2, P2, P2_ant, P2_ant2, Z2, Z2_ant, fP_ant, fP_ant2, fP As Double

            Dim nloops As Integer = 0

            Z2 = P * (V + epsilon) / (R * T)
            P2 = P

            If liq_or_gas = "gas" Then

                Do

                    Z2_ant = Z2
                    Z2 = compr(T, P2, mix, liq_or_gas, Z2_ant)

                    P2_ant2 = P2_ant
                    P2_ant = P2

                    If nloops > 3 Then
                        P2 = P2 - fP * (P2 - P2_ant2) / (fP - fP_ant2)
                    Else
                        P2 = (Z2 * R * T / (V + epsilon))
                    End If

                    fP_ant2 = fP_ant
                    fP_ant = fP
                    fP = (P2 - P2_ant)

                    nloops += 1

                Loop Until Abs(fP) < 0.0000000001 And nloops > 3

            End If

            t1 = New Task(Sub() Ar = (Helmholtz(T, P, mix, liq_or_gas, Z) + R * T * Log(Z)) / (R * T))

            t2 = New Task(Sub() Ar2 = (Helmholtz(T, P2, mix, liq_or_gas, Z2) + R * T * Log(Z2)) / (R * T))

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim dF = (Ar2 - Ar) / epsilon

            Return dF

        End Function

        Private Function CalcdFdT(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim R = 8.314

            Dim epsilon = If(liq_or_gas = "gas", 1.0, -0.001)

            Dim Z = Zestimate

            Dim Ar, Ar2, P2, Z2, Z2_ant, P2_ant, P2_ant2, fP, fP_ant2, fP_ant As Double

            Dim V = Z * R * T / P

            Dim nloops As Integer = 0

            If liq_or_gas = "gas" Then

                P2 = P
                Z2 = Z

                Do

                    Z2_ant = Z2
                    Z2 = compr(T + epsilon, P2, mix, liq_or_gas, Z2_ant)

                    P2_ant2 = P2_ant
                    P2_ant = P2

                    If nloops > 3 Then
                        P2 = P2 - fP * (P2 - P2_ant2) / (fP - fP_ant2)
                    Else
                        P2 = (Z2 * R * (T + epsilon) / V)
                    End If

                    fP_ant2 = fP_ant
                    fP_ant = fP
                    fP = (P2 - P2_ant)

                    nloops += 1

                Loop Until Abs(fP) < 0.0000000001 And nloops > 3

            Else

                P2 = P
                Z2 = compr(T + epsilon, P2, mix, liq_or_gas, Z)

            End If

            Dim t1, t2 As Task

            t1 = New Task(Sub() Ar = (Helmholtz(T, P, mix, liq_or_gas, Z) + R * T * Log(Z)) / (R * T))

            t2 = New Task(Sub() Ar2 = (Helmholtz(T + epsilon, P2, mix, liq_or_gas, Z2) + R * (T + epsilon) * Log(Z2)) / (R * (T + epsilon)))

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim dF = (Ar2 - Ar) / epsilon

            Return dF

        End Function

        Private Function CalcdF2dV2(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim R = 8.314

            Dim Z = Zestimate

            Dim V = Z * R * T / P

            Dim epsilon = V * 0.01

            Dim t1, t2 As Task

            Dim Ar, Ar2, P2, P2_ant, P2_ant2, Z2, Z2_ant, fP_ant, fP_ant2, fP As Double

            Dim nloops As Integer = 0

            Z2 = P * (V + epsilon) / (R * T)
            P2 = P

            If liq_or_gas = "gas" Then

                Do

                    Z2_ant = Z2
                    Z2 = compr(T, P2, mix, liq_or_gas, Z2_ant)

                    P2_ant2 = P2_ant
                    P2_ant = P2

                    If nloops > 3 Then
                        P2 = P2 - fP * (P2 - P2_ant2) / (fP - fP_ant2)
                    Else
                        P2 = (Z2 * R * T / (V + epsilon))
                    End If

                    fP_ant2 = fP_ant
                    fP_ant = fP
                    fP = (P2 - P2_ant)

                    nloops += 1

                Loop Until Abs(fP) < 0.0000000001 And nloops > 3

            End If

            t1 = New Task(Sub() Ar = (Helmholtz(T, P, mix, liq_or_gas, Z) + R * T * Log(Z)) / (R * T))
            t2 = New Task(Sub() Ar2 = (Helmholtz(T, P2, mix, liq_or_gas, Z2) + R * T * Log(Z2)) / (R * T))

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim dF = (Ar2 - Ar) / epsilon

            Return dF

        End Function

        Private Function Calcd2FdTdV(T As Double, P As Double, liq_or_gas As String, Zestimate As Double) As Double

            Dim R = 8.314

            Dim Z = Zestimate

            Dim V = Z * R * T / P

            Dim epsilon = V * 0.01

            Dim t1, t2 As Task

            Dim Ar, Ar2, P2, P2_ant, P2_ant2, Z2, Z2_ant, fP_ant, fP_ant2, fP As Double

            Dim nloops As Integer = 0

            Z2 = P * (V + epsilon) / (R * T)
            P2 = P

            If liq_or_gas = "gas" Then

                Do

                    Z2_ant = Z2
                    Z2 = compr(T, P2, mix, liq_or_gas, Z2_ant)

                    P2_ant2 = P2_ant
                    P2_ant = P2

                    If nloops > 3 Then
                        P2 = P2 - fP * (P2 - P2_ant2) / (fP - fP_ant2)
                    Else
                        P2 = (Z2 * R * T / (V + epsilon))
                    End If

                    fP_ant2 = fP_ant
                    fP_ant = fP
                    fP = (P2 - P2_ant)

                    nloops += 1

                Loop Until Abs(fP) < 0.0000000001 And nloops > 3

            End If

            t1 = New Task(Sub() Ar = CalcdFdT(T, P, liq_or_gas, Z))

            t2 = New Task(Sub() Ar2 = CalcdFdT(T, P2, liq_or_gas, Z2))

            t1.Start()
            t2.Start()

            Task.WaitAll(t1, t2)

            Dim dF = (Ar2 - Ar) / epsilon

            Return dF

        End Function

        Friend Function zeros(n, m) As Double(,)

            Dim matrix(n, m) As Double

            Return matrix

        End Function

        Friend Function zeros(n, m, o) As Double(,,)

            Dim matrix(n, m, o) As Double

            Return matrix

        End Function

        Friend Function zeros(n) As Double()

            Dim vector(n) As Double

            Return vector

        End Function

        Friend Function sum(v() As Double) As Double

            Return v.Sum

        End Function

        Public Function max(v(,) As Double) As Double

            Dim maxval As Double = Double.MinValue

            For i = 0 To v.GetUpperBound(0) - 1
                For j = 0 To v.GetUpperBound(0) - 1
                    If v(i, j) > maxval Then maxval = v(i, j)
                Next
            Next

            Return maxval

        End Function

        Public Function sum2(v(,) As Double) As Double

            Dim s As Double = 0

            For i = 0 To v.GetUpperBound(0)
                For j = 0 To v.GetUpperBound(0)
                    s += v(i, j)
                Next
            Next

            Return s

        End Function

        Friend Function FugF(T, P, mix, phase, Zestimate)

            'Calculates the fugacity And compresibility coefficient of mixture mix at temperature T
            'And pressure P using PC-SAFT EoS
            '
            'Parameters:
            'EoS Equation of state used for calculations
            'T: Temperature(K)
            'P: Pressure(K)
            'mix: cMixture Object
            'phase: set phase = 'liq' to calculate the fugacity of a liquid phase or
            '   phase = 'gas' to calculate the fugacity of a gas phase
            '
            'Optional parameters (set [] to keep default value)
            'Z_ini: Initial guess for the compressibility coefficient
            '   If Not defined, the program uses an initial guess Z_ini = 0.8 for gas
            '   phase And a Z_ini corresponding to a liquid density of 800 kg/m3 for
            '   the liquid phase
            'options: parameters of the fsolve numerical resolution method (structure
            '   generated with "optimset")
            '
            'Results:
            'f fugacity coefficient
            'Z: compresibility coefficient
            'EoS: returns EoS used for calculations
            '
            'Reference: Gross And Sadowski, Ind.Eng.Chem.Res. 40 (2001) 1244-1260
            'Reference 2: Chapman et al., Ind.Eng.Chem.Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            '**************************************************************************
            'Calculates the compresibility coefficient
            '**************************************************************************
            'Constants

            Dim kb, Z, muHC(), muDisp(), NumAss(), muAss(), dens_num, logfi, f() As Double

            kb = 1.3806504E-23 'Boltzmann K (J/K)

            Z = compr(T, P, mix, phase, Zestimate)

            dens_num = P / (Z * kb * T) * 1 / (10000000000.0) ^ 3

            '**************************************************************************
            'Calculates the contributions to the chemical potential
            '**************************************************************************

            Dim t1, t2, t3 As Task

            'Hard chain contribution
            t1 = New Task(Sub() muHC = mu_HC(T, dens_num, mix))

            'Dispersive contribution
            t2 = New Task(Sub() muDisp = mu_Disp(T, dens_num, mix))

            'Association contribution
            t3 = New Task(Sub()
                              NumAss = zeros(mix.numC)

                              For i = 1 To mix.numC
                                  NumAss(i) = mix.comp(i).EoSParam(4)
                              Next

                              If sum(NumAss) > 0 Then
                                  muAss = mu_Ass(T, dens_num, mix)
                              Else
                                  muAss = zeros(mix.numC)
                              End If
                          End Sub)

            t1.Start()
            t2.Start()
            t3.Start()

            Task.WaitAll(t1, t2, t3)

            '**************************************************************************
            'Calculates the fugacity coefficient
            '**************************************************************************

            f = zeros(mix.numC - 1)
            For i = 1 To mix.numC
                logfi = muHC(i) + muDisp(i) + muAss(i) - Log(Z) 'Eq. A32 Of reference
                f(i - 1) = Exp(logfi)
            Next

            Return f

        End Function

        Friend Function HardSphereDiameter(T, m, sigma, epsilon)

            'Hard Sphere Diameter with PC-SAFT EoS
            'Auxiliary function, Not to be used directly
            '
            'Reference: Gross And Sadowski, Ind.Eng.Chem.Res. 40 (2001) 1244-1260

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            Return sigma * (1 - 0.12 * Exp(-3 * epsilon / T)) 'Eq. 3 Of reference

        End Function

        Friend Function Helmholtz(T, P, mix, phase, Z)

            'Calculates the residual Helmholtz energy And compresibility coefficient of mixture mix at temperature T
            'And pressure P using PC-SAFT EoS
            '
            'Parameters:
            'EoS Equation of state used for calculations
            'T: Temperature(K)
            'P: Pressure(K)
            'mix: cMixture Object
            'phase: set phase = 'liq' to calculate the fugacity of a liquid phase or
            '   phase = 'gas' to calculate the fugacity of a gas phase
            '
            'Optional parameters (set [] to keep default value)
            'Z_ini: Initial guess for the compressibility coefficient
            '   If Not defined, the program uses an initial guess Z_ini = 0.8 for gas
            '   phase And a Z_ini corresponding to a liquid density of 800 kg/m3 for
            '   the liquid phase
            'options: parameters of the fsolve numerical resolution method (structure
            '   generated with "optimset")
            '
            'Results:
            'Ares residual Helmholtz energy
            'Z: compresibility coefficient
            'EoS: returns EoS used for calculations
            '
            'Reference: Gross And Sadowski, Ind.Eng.Chem.Res. 40 (2001) 1244-1260
            'Reference 2: Chapman et al., Ind.Eng.Chem.Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            '**************************************************************************
            'Calculates the compresibility coefficient 
            '**************************************************************************

            'Constants

            Dim kb, dens_num, ahc, adisp, aass, Ares As Double

            kb = 1.3806504E-23 'Boltzmann K (J/K)

            'Z = compr(T, P, mix, phase, Zestimate)
            dens_num = P / (Z * kb * T) * 1 / (10000000000.0) ^ 3

            '**************************************************************************
            'Hard-Chain Reference Contribution
            '**************************************************************************

            ahc = HelmholtzHC(T, dens_num, mix)

            '**************************************************************************
            'Dispersion Contribution
            '**************************************************************************

            adisp = HelmholtzDisp(T, dens_num, mix)

            '**************************************************************************
            'Association Contribution
            '**************************************************************************

            Dim NumAss() As Double

            NumAss = zeros(mix.numC)
            For i = 1 To mix.numC
                NumAss(i) = mix.comp(i).EoSParam(4)
            Next

            If sum(NumAss) > 0 Then
                aass = HelmholtzAss(T, dens_num, mix)
            Else
                aass = 0
            End If

            '**************************************************************************
            'Residual Helmholtz energy
            '**************************************************************************
            Ares = ahc + adisp + aass

            Return Ares

        End Function

        Friend Function HelmholtzDisp(T, dens_num, mix)

            'Calculates the dispersion contribution to the residual Helmholtz energy 
            'of mixture mix at temperature T And pressure P using PC-SAFT EoS
            '
            'Parameters:
            'EoS Equation of state used for calculations
            'T: Temperature(K)
            'dens_num: Number density(molecule / Angstrom ^ 3)
            'mix: cMixture Object
            '
            'Results:
            'Ahc residual Helmholtz energy, association contribution
            'EoS: returns EoS used for calculations
            '
            'Reference: Gross And Sadowski, Ind.Eng.Chem.Res. 40 (2001) 1244-1260

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            Dim a0(8), a1(8), a2(8), b0(8), b1(8), b2(8) As Double

            'Equation constants
            a0(1) = 0.9105631445
            a0(2) = 0.6361281449
            a0(3) = 2.6861347891
            a0(4) = -26.547362491
            a0(5) = 97.759208784
            a0(6) = -159.59154087
            a0(7) = 91.297774084
            a1(1) = -0.3084016918
            a1(2) = 0.1860531159
            a1(3) = -2.5030047259
            a1(4) = 21.419793629
            a1(5) = -65.25588533
            a1(6) = 83.318680481
            a1(7) = -33.74692293
            a2(1) = -0.0906148351
            a2(2) = 0.4527842806
            a2(3) = 0.5962700728
            a2(4) = -1.7241829131
            a2(5) = -4.1302112531
            a2(6) = 13.77663187
            a2(7) = -8.6728470368
            b0(1) = 0.7240946941
            b0(2) = 2.2382791861
            b0(3) = -4.0025849485
            b0(4) = -21.003576815
            b0(5) = 26.855641363
            b0(6) = 206.55133841
            b0(7) = -355.60235612
            b1(1) = -0.5755498075
            b1(2) = 0.6995095521
            b1(3) = 3.892567339
            b1(4) = -17.215471648
            b1(5) = 192.67226447
            b1(6) = -161.82646165
            b1(7) = -165.20769346
            b2(1) = 0.0976883116
            b2(2) = -0.2557574982
            b2(3) = -9.155856153
            b2(4) = 20.642075974
            b2(5) = -38.804430052
            b2(6) = 93.626774077
            b2(7) = -29.666905585

            Dim x, m, sigma, epsilon, d As Double()
            Dim k1(,) As Double

            'Reads pure-component properties
            numC = mix.numC
            x = mix.x
            m = zeros(numC)
            sigma = zeros(numC)
            epsilon = zeros(numC)
            For i = 1 To numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
            Next
            k1 = mix.k1

            'Calculates the temperature-depNextant segment diameter
            d = zeros(numC)
            For i = 1 To numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim m_prom As Double

            'mean segment number
            m_prom = 0
            For i = 1 To numC
                m_prom = m_prom + m(i) * x(i) 'Eq. 6 Of reference
            Next

            Dim a, b As Double()

            'Calculates the a And b parameters
            a = zeros(7)
            b = zeros(7)
            For j = 1 To 7
                a(j) = a0(j) + (m_prom - 1) / m_prom * a1(j) + (m_prom - 1) / m_prom * (m_prom - 2) / m_prom * a2(j) 'Eq. 18 Of reference
                b(j) = b0(j) + (m_prom - 1) / m_prom * b1(j) + (m_prom - 1) / m_prom * (m_prom - 2) / m_prom * b2(j) 'Eq. 19 Of reference
            Next

            Dim dens_red, sigmaij(,), epsilonij(,) As Double

            'Reduced density
            dens_red = 0
            For i = 1 To numC
                dens_red = dens_red + x(i) * m(i) * d(i) ^ 3
            Next
            dens_red = dens_red * PI / 6 * dens_num 'Eq. 9 Of reference

            'Mixing rules
            sigmaij = zeros(numC, numC)
            epsilonij = zeros(numC, numC)
            For i = 1 To numC
                For j = 1 To numC
                    sigmaij(i, j) = 0.5 * (sigma(i) + sigma(j)) 'Eq. A14 of reference
                    epsilonij(i, j) = Sqrt(epsilon(i) * epsilon(j)) * (1 - k1(i, j)) 'Eq A15 of reference            
                Next
            Next

            Dim term1, term2, C1, prom1, prom2 As Double

            'Dispersion Contribution
            term1 = (m_prom) * (8 * dens_red - 2 * dens_red ^ 2) / (1 - dens_red) ^ 4
            term2 = (1 - m_prom) * (20 * dens_red - 27 * dens_red ^ 2 + 12 * dens_red ^ 3 - 2 * dens_red ^ 4) / ((1 - dens_red) * (2 - dens_red)) ^ 2
            C1 = (1 + term1 + term2) ^ -1 'Eq. A11 of reference

            prom1 = 0
            prom2 = 0
            For i = 1 To numC
                For j = 1 To numC
                    prom1 = prom1 + x(i) * x(j) * m(i) * m(j) * epsilonij(i, j) / T * sigmaij(i, j) ^ 3 'Eq. A12 of reference
                    prom2 = prom2 + x(i) * x(j) * m(i) * m(j) * (epsilonij(i, j) / T) ^ 2 * sigmaij(i, j) ^ 3 'Eq. A13 of reference
                Next
            Next

            Dim I1, I2 As Double, Adisp

            I1 = 0
            I2 = 0
            For j = 1 To 7
                I1 = I1 + a(j) * dens_red ^ (j - 1) 'Eq. A16 of reference
                I2 = I2 + b(j) * dens_red ^ (j - 1) 'Eq. A17 of reference
            Next

            term1 = -2 * PI * dens_num * I1 * prom1
            term2 = -PI * dens_num * m_prom * C1 * I2 * prom2

            Adisp = term1 + term2 'Eq. A10 of reference

            Return Adisp

        End Function

        Friend Function HelmholtzHC(T, dens_num, mix)

            'Calculates the Hard Chain contribution to the residual Helmholtz energy 
            'of mixture mix at temperature T and pressure P using PC-SAFT EoS
            '
            'Parameters:
            'EoS: Equation of state used for calculations
            'T: Temperature(K)
            'dens_num: Number density (molecule/Angstrom^3)
            'mix: cMixture object
            '
            'Results:
            'Ahc: residual Helmholtz energy, association contribution
            'EoS: returns EoS used for calculations
            '
            'Reference: Gross and Sadowski, Ind. Eng. Chem. Res. 40 (2001) 1244-1260

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program is free software: you can redistribute it and/or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, or
            '(at your option) any later version.
            'This program is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If not, see <http://www.gnu.org/licenses/>.

            Dim x, m, sigma, epsilon, d As Double()

            'Reads pure-component properties
            numC = mix.numC
            x = mix.x
            m = zeros(numC)
            sigma = zeros(numC)
            epsilon = zeros(numC)
            For i = 1 To numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(numC)
            For i = 1 To numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim m_prom As Double

            'mean segment number
            m_prom = 0
            For i = 1 To numC
                m_prom = m_prom + m(i) * x(i) 'Eq. 6 of reference
            Next

            Dim auxil(), ghs(,), term1, term2, term3, a_hs, sum1, Ahc As Double

            'auxiliary functions
            auxil = zeros(4)
            For j = 1 To 4
                For i = 1 To numC
                    auxil(j) = auxil(j) + x(i) * m(i) * d(i) ^ (j - 1)
                Next
                auxil(j) = auxil(j) * PI / 6 * dens_num 'Eq. 9 of reference
            Next

            'radial distribution function
            ghs = zeros(numC, numC)
            For i = 1 To numC
                For j = 1 To numC
                    term1 = 1 / (1 - auxil(4))
                    term2 = d(i) * d(j) / (d(i) + d(j)) * 3 * auxil(3) / (1 - auxil(4)) ^ 2
                    term3 = (d(i) * d(j) / (d(i) + d(j))) ^ 2 * 2 * auxil(3) ^ 2 / (1 - auxil(4)) ^ 3
                    ghs(i, j) = term1 + term2 + term3 'Eq. 8 of reference
                Next
            Next

            'Helmholtz energy
            term1 = 3 * auxil(2) * auxil(3) / (1 - auxil(4))
            term2 = auxil(3) ^ 3 / (auxil(4) * (1 - auxil(4)) ^ 2)
            term3 = (auxil(3) ^ 3 / auxil(4) ^ 2 - auxil(1)) * Log(1 - auxil(4))
            a_hs = (1 / auxil(1)) * (term1 + term2 + term3)

            sum1 = 0
            For i = 1 To numC
                sum1 = sum1 + x(i) * (m(i) - 1) * Log(ghs(i, i))
            Next

            Ahc = m_prom * a_hs - sum1 'Eq. A4 of reference

            Return Ahc

        End Function

        Friend Function mu_Disp(T, dens_num, mix)

            'Calculates the dispersion contribution to the residual chemical potential 
            'of mixture mix at temperature T and pressure P using PC-SAFT EoS
            '
            'Parameters:
            'EoS: Equation of state used for calculations
            'T: Temperature(K)
            'P: Pressure (K)
            'dens_num: Number density (molecule/Angstrom^3)
            'mix: cMixture object
            '
            'Results:
            'muass: residual chemical potential, association contribution
            'EoS: returns EoS used for calculations
            '
            'Reference: Gross and Sadowski, Ind. Eng. Chem. Res. 40 (2001) 1244-1260

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program is free software: you can redistribute it and/or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, or
            '(at your option) any later version.
            'This program is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If not, see <http://www.gnu.org/licenses/>.

            Dim a0(8), a1(8), a2(8), b0(8), b1(8), b2(8) As Double

            'Equation constants
            a0(1) = 0.9105631445
            a0(2) = 0.6361281449
            a0(3) = 2.6861347891
            a0(4) = -26.547362491
            a0(5) = 97.759208784
            a0(6) = -159.59154087
            a0(7) = 91.297774084
            a1(1) = -0.3084016918
            a1(2) = 0.1860531159
            a1(3) = -2.5030047259
            a1(4) = 21.419793629
            a1(5) = -65.25588533
            a1(6) = 83.318680481
            a1(7) = -33.74692293
            a2(1) = -0.0906148351
            a2(2) = 0.4527842806
            a2(3) = 0.5962700728
            a2(4) = -1.7241829131
            a2(5) = -4.1302112531
            a2(6) = 13.77663187
            a2(7) = -8.6728470368
            b0(1) = 0.7240946941
            b0(2) = 2.2382791861
            b0(3) = -4.0025849485
            b0(4) = -21.003576815
            b0(5) = 26.855641363
            b0(6) = 206.55133841
            b0(7) = -355.60235612
            b1(1) = -0.5755498075
            b1(2) = 0.6995095521
            b1(3) = 3.892567339
            b1(4) = -17.215471648
            b1(5) = 192.67226447
            b1(6) = -161.82646165
            b1(7) = -165.20769346
            b2(1) = 0.0976883116
            b2(2) = -0.2557574982
            b2(3) = -9.155856153
            b2(4) = 20.642075974
            b2(5) = -38.804430052
            b2(6) = 93.626774077
            b2(7) = -29.666905585

            Dim m, sigma, epsilon, d As Double()

            'Reads pure-component properties
            m = zeros(mix.numC)
            sigma = zeros(mix.numC)
            epsilon = zeros(mix.numC)
            For i = 1 To mix.numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(mix.numC)
            For i = 1 To mix.numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim m_prom As Double

            'mean segment number
            m_prom = 0
            For i = 1 To mix.numC
                m_prom = m_prom + m(i) * mix.x(i) 'Eq. 6 of reference
            Next

            Dim a(), b(), dens_red, sigmaij(,), epsilonij(,) As Double

            'Calculates the a and b parameters
            a = zeros(7)
            b = zeros(7)
            For j = 1 To 7
                a(j) = a0(j) + (m_prom - 1) / m_prom * a1(j) + (m_prom - 1) / m_prom * (m_prom - 2) / m_prom * a2(j) 'Eq. 18 of reference
                b(j) = b0(j) + (m_prom - 1) / m_prom * b1(j) + (m_prom - 1) / m_prom * (m_prom - 2) / m_prom * b2(j) 'Eq. 19 of reference
            Next

            'Reduced density
            dens_red = 0
            For i = 1 To mix.numC
                dens_red = dens_red + mix.x(i) * m(i) * d(i) ^ 3
            Next
            dens_red = dens_red * PI / 6 * dens_num 'Eq. 9 of reference

            'Mixing rules
            sigmaij = zeros(mix.numC, mix.numC)
            epsilonij = zeros(mix.numC, mix.numC)
            For i = 1 To mix.numC
                For j = 1 To mix.numC
                    sigmaij(i, j) = 0.5 * (sigma(i) + sigma(j)) 'Eq. A14 of reference
                    epsilonij(i, j) = Sqrt(epsilon(i) * epsilon(j)) * (1 - mix.k1(i, j)) 'Eq A15 of reference           
                Next
            Next

            Dim Zdisp, Adisp, dauxil_dxk(,), prom1, prom2, der_prom1(), der_prom2(), sum1, sum2 As Double

            'Compressibility coefficient
            Zdisp = Z_disp(T, dens_num, mix)

            'Helmholtz energy
            Adisp = HelmholtzDisp(T, dens_num, mix)

            'Dispersion contribution
            dauxil_dxk = zeros(4, mix.numC)
            For j = 1 To 4
                For i = 1 To mix.numC
                    dauxil_dxk(j, i) = PI / 6 * dens_num * m(i) * d(i) ^ (j - 1) 'Eq. A34 of reference
                Next
            Next

            prom1 = 0
            prom2 = 0
            For i = 1 To mix.numC
                For j = 1 To mix.numC
                    prom1 = prom1 + mix.x(i) * mix.x(j) * m(i) * m(j) * epsilonij(i, j) / T * sigmaij(i, j) ^ 3 'Eq. A12 of reference
                    prom2 = prom2 + mix.x(i) * mix.x(j) * m(i) * m(j) * (epsilonij(i, j) / T) ^ 2 * sigmaij(i, j) ^ 3 'Eq. A13 of reference
                Next
            Next

            der_prom1 = zeros(mix.numC)
            der_prom2 = zeros(mix.numC)
            For i = 1 To mix.numC
                sum1 = 0
                sum2 = 0
                For j = 1 To mix.numC
                    sum1 = sum1 + mix.x(j) * m(j) * (epsilonij(i, j) / T) * sigmaij(i, j) ^ 3
                    sum2 = sum2 + mix.x(j) * m(j) * (epsilonij(i, j) / T) ^ 2 * sigmaij(i, j) ^ 3
                Next
                der_prom1(i) = 2 * m(i) * sum1 'Eq. A39 of reference
                der_prom2(i) = 2 * m(i) * sum2 'Eq. A40 of reference
            Next

            Dim I1, I2, term1, term2, C1, C2, der_C1(), der_a(,), der_b(,) As Double

            I1 = 0
            I2 = 0
            For j = 1 To 7
                I1 = I1 + a(j) * dens_red ^ (j - 1) 'Eq. A16 of reference
                I2 = I2 + b(j) * dens_red ^ (j - 1) 'Eq. A17 of reference
            Next

            term1 = (m_prom) * (8 * dens_red - 2 * dens_red ^ 2) / (1 - dens_red) ^ 4
            term2 = (1 - m_prom) * (20 * dens_red - 27 * dens_red ^ 2 + 12 * dens_red ^ 3 - 2 * dens_red ^ 4) / ((1 - dens_red) * (2 - dens_red)) ^ 2
            C1 = (1 + term1 + term2) ^ -1 'Eq. A11 of reference

            term1 = m_prom * (-4 * dens_red ^ 2 + 20 * dens_red + 8) / (1 - dens_red) ^ 5
            term2 = (1 - m_prom) * (2 * dens_red ^ 3 + 12 * dens_red ^ 2 - 48 * dens_red + 40) / ((1 - dens_red) * (2 - dens_red)) ^ 3
            C2 = -C1 ^ 2 * (term1 + term2) 'Eq. A31 of reference

            der_C1 = zeros(mix.numC)
            For i = 1 To mix.numC
                term1 = m(i) * (8 * dens_red - 2 * dens_red ^ 2) / (1 - dens_red) ^ 4
                term2 = m(i) * (20 * dens_red - 27 * dens_red ^ 2 + 12 * dens_red ^ 3 - 2 * dens_red ^ 4) / ((1 - dens_red) * (2 - dens_red)) ^ 2

                der_C1(i) = C2 * dauxil_dxk(4, i) - C1 ^ 2 * (term1 - term2) 'Eq. A41 of reference
            Next

            der_a = zeros(7, mix.numC)
            der_b = zeros(7, mix.numC)
            For i = 1 To 7
                For j = 1 To mix.numC
                    der_a(i, j) = m(j) / m_prom ^ 2 * a1(i) + m(j) / m_prom ^ 2 * (3 - 4 / m_prom) * a2(i) 'Eq. A44 of reference
                    der_b(i, j) = m(j) / m_prom ^ 2 * b1(i) + m(j) / m_prom ^ 2 * (3 - 4 / m_prom) * b2(i) 'Eq. A45 of reference
                Next
            Next

            Dim der_I1(), der_I2(), dadisp_dxk(), muDisp() As Double

            der_I1 = zeros(mix.numC)
            der_I2 = zeros(mix.numC)
            For i = 1 To mix.numC
                sum1 = 0
                sum2 = 0
                For j = 1 To 7
                    sum1 = sum1 + a(j) * (j - 1) * dauxil_dxk(4, i) * dens_red ^ (j - 2) + der_a(j, i) * dens_red ^ (j - 1) 'Eq. A42 of reference
                    sum2 = sum2 + b(j) * (j - 1) * dauxil_dxk(4, i) * dens_red ^ (j - 2) + der_b(j, i) * dens_red ^ (j - 1) 'Eq. A43 of reference
                Next
                der_I1(i) = sum1
                der_I2(i) = sum2
            Next

            dadisp_dxk = zeros(mix.numC)
            For i = 1 To mix.numC
                term1 = -2 * PI * dens_num * (der_I1(i) * prom1 + I1 * der_prom1(i))
                term2 = -PI * dens_num * ((m(i) * C1 * I2 + m_prom * der_C1(i) * I2 + m_prom * C1 * der_I2(i)) * prom2 + m_prom * C1 * I2 * der_prom2(i))
                dadisp_dxk(i) = term1 + term2 'Eq. A38 of reference
            Next

            'Chemical potential
            sum1 = 0
            For i = 1 To mix.numC
                sum1 = sum1 + mix.x(i) * dadisp_dxk(i)
            Next

            muDisp = zeros(mix.numC)
            For i = 1 To mix.numC
                muDisp(i) = Adisp + Zdisp + dadisp_dxk(i) - sum1  'Eq. A33 of reference
            Next

            Return muDisp

        End Function

        Friend Function mu_HC(T, dens_num, mix)

            'Calculates the hard chain contribution to the residual chemical potential 
            'of mixture mix at temperature T and pressure P using PC-SAFT EoS
            '
            'Parameters:
            'EoS: Equation of state used for calculations
            'T: Temperature(K)
            'P: Pressure (K)
            'dens_num: Number density (molecule/Angstrom^3)
            'mix: cMixture object
            '
            'Results:
            'muass: residual chemical potential, association contribution
            'EoS: returns EoS used for calculations
            '
            'Reference: Gross and Sadowski, Ind. Eng. Chem. Res. 40 (2001) 1244-1260

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program is free software: you can redistribute it and/or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, or
            '(at your option) any later version.
            'This program is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If not, see <http://www.gnu.org/licenses/>.

            Dim m, sigma, epsilon, d As Double()

            'Reads pure-component properties
            m = zeros(mix.numC)
            sigma = zeros(mix.numC)
            epsilon = zeros(mix.numC)
            For i = 1 To mix.numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(mix.numC)
            For i = 1 To mix.numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim m_prom, auxil(), ghs(,), term1, term2, term3 As Double

            'mean segment number
            m_prom = 0
            For i = 1 To mix.numC
                m_prom = m_prom + m(i) * mix.x(i) 'Eq. 6 of reference
            Next

            'auxiliary functions
            auxil = zeros(4)
            For j = 1 To 4
                For i = 1 To mix.numC
                    auxil(j) = auxil(j) + mix.x(i) * m(i) * d(i) ^ (j - 1)
                Next
                auxil(j) = auxil(j) * PI / 6 * dens_num 'Eq. 9 of reference
            Next

            'radial distribution function
            ghs = zeros(mix.numC, mix.numC)
            For i = 1 To mix.numC
                For j = 1 To mix.numC
                    term1 = 1 / (1 - auxil(4))
                    term2 = d(i) * d(j) / (d(i) + d(j)) * 3 * auxil(3) / (1 - auxil(4)) ^ 2
                    term3 = (d(i) * d(j) / (d(i) + d(j))) ^ 2 * 2 * auxil(3) ^ 2 / (1 - auxil(4)) ^ 3
                    ghs(i, j) = term1 + term2 + term3 'Eq. 8 of reference
                Next
            Next

            Dim Zhc, a_hs, sum1, Ahc, dauxil_dxk(,), dahs_dxk(), term4, term5, term6, term7 As Double

            'Compressibility coefficient
            Zhc = Z_hc(T, dens_num, mix)

            'Helmholtz energy
            term1 = 3 * auxil(2) * auxil(3) / (1 - auxil(4))
            term2 = auxil(3) ^ 3 / (auxil(4) * (1 - auxil(4)) ^ 2)
            term3 = (auxil(3) ^ 3 / auxil(4) ^ 2 - auxil(1)) * Log(1 - auxil(4))
            a_hs = (1 / auxil(1)) * (term1 + term2 + term3) 'Eq. A6 of reference
            sum1 = 0
            For i = 1 To mix.numC
                sum1 = sum1 + mix.x(i) * (m(i) - 1) * Log(ghs(i, i))
            Next
            Ahc = m_prom * a_hs - sum1 'Eq. A4 of reference

            'Chemical potential
            dauxil_dxk = zeros(4, mix.numC)
            For j = 1 To 4
                For i = 1 To mix.numC
                    dauxil_dxk(j, i) = PI / 6 * dens_num * m(i) * d(i) ^ (j - 1) 'Eq. A34 of reference
                Next
            Next

            dahs_dxk = zeros(mix.numC)
            For i = 1 To mix.numC
                term1 = -dauxil_dxk(1, i) / auxil(1) * a_hs
                term2 = 3 * (dauxil_dxk(2, i) * auxil(3) + auxil(2) * dauxil_dxk(3, i)) / (1 - auxil(4))
                term3 = 3 * auxil(2) * auxil(3) * dauxil_dxk(4, i) / (1 - auxil(4)) ^ 2
                term4 = 3 * auxil(3) ^ 2 * dauxil_dxk(3, i) / (auxil(4) * (1 - auxil(4)) ^ 2)
                term5 = auxil(3) ^ 3 * dauxil_dxk(4, i) * (3 * auxil(4) - 1) / (auxil(4) ^ 2 * (1 - auxil(4)) ^ 3)
                term6 = ((3 * auxil(3) ^ 2 * dauxil_dxk(3, i) * auxil(4) - 2 * auxil(3) ^ 3 * dauxil_dxk(4, i)) / auxil(4) ^ 3 - dauxil_dxk(1, i)) * Log(1 - auxil(4))
                term7 = (auxil(1) - auxil(3) ^ 3 / auxil(4) ^ 2) * dauxil_dxk(4, i) / (1 - auxil(4))

                dahs_dxk(i) = term1 + 1 / auxil(1) * (term2 + term3 + term4 + term5 + term6 + term7) 'Eq. A36 of reference
            Next

            Dim dgij_dxk(,,), dahc_dxk(), muHC() As Double

            dgij_dxk = zeros(mix.numC, mix.numC, mix.numC)
            For i = 1 To mix.numC
                For j = 1 To mix.numC
                    For k = 1 To mix.numC
                        term1 = dauxil_dxk(4, k) / (1 - auxil(4)) ^ 2
                        term2 = (d(i) * d(j) / (d(i) + d(j))) * (3 * dauxil_dxk(3, k) / (1 - auxil(4)) ^ 2 + 6 * auxil(3) * dauxil_dxk(4, k) / (1 - auxil(4)) ^ 3)
                        term3 = (d(i) * d(j) / (d(i) + d(j))) ^ 2 * (4 * auxil(3) * dauxil_dxk(3, k) / (1 - auxil(4)) ^ 3 + 6 * auxil(3) ^ 2 * dauxil_dxk(4, k) / (1 - auxil(4)) ^ 4)
                        dgij_dxk(i, j, k) = term1 + term2 + term3 'Eq. A37 of reference
                    Next
                Next
            Next

            dahc_dxk = zeros(mix.numC)
            For i = 1 To mix.numC
                sum1 = 0
                For j = 1 To mix.numC
                    sum1 = sum1 + mix.x(j) * (m(j) - 1) / ghs(j, j) * dgij_dxk(j, j, i)
                Next
                dahc_dxk(i) = m(i) * a_hs + m_prom * dahs_dxk(i) - sum1 + (1 - m(i)) * Log(ghs(i, i)) 'Eq. A35 of reference
            Next

            'Chemical potential
            sum1 = 0
            For i = 1 To mix.numC
                sum1 = sum1 + mix.x(i) * dahc_dxk(i)
            Next

            muHC = zeros(mix.numC)
            For i = 1 To mix.numC
                muHC(i) = Ahc + Zhc + dahc_dxk(i) - sum1  'Eq. A33 of reference
            Next

            Return muHC

        End Function

        Friend Function obj_SAFT(dens_red, T, P, mix)

            'Objective function for the calculation of Z with PC-SAFT EoS
            'Auxiliary function, not to be used directly
            '
            'Reference: Gross and Sadowski, Ind. Eng. Chem. Res. 40 (2001) 1244-1260
            'Reference 2: Chapman et al., Ind. Eng. Chem. Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program is free software: you can redistribute it and/or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, or
            '(at your option) any later version.
            'This program is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If not, see <http://www.gnu.org/licenses/>.

            Dim x, m, sigma, epsilon, d As Double()

            'Reads pure-component properties
            numC = mix.numC
            x = mix.x
            m = zeros(numC)
            sigma = zeros(numC)
            epsilon = zeros(numC)
            For i = 1 To numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(numC)
            For i = 1 To numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim sum1, dens_Num, ZHc, Zdisp, Zass, kb, Zcalc, Pcalc, result As Double

            'Calculates density according to the iterated dens_red
            sum1 = 0
            For i = 1 To numC
                sum1 = sum1 + x(i) * m(i) * d(i) ^ 3
            Next
            dens_Num = 6 / PI * dens_red * sum1 ^ -1 'Eq. 9 of reference

            '**************************************************************************
            'Compressibility coefficient 
            '**************************************************************************
            ZHc = Z_hc(T, dens_Num, mix)

            Zdisp = Z_disp(T, dens_Num, mix)

            Zass = Z_ass(T, dens_Num, mix)

            '**************************************************************************
            'Ojective function
            '**************************************************************************
            kb = 1.3806504E-23 'Boltzmann K (J/K)

            Zcalc = 1 + ZHc + Zdisp + Zass

            Pcalc = Zcalc * kb * T * dens_Num * (10000000000.0) ^ 3

            'Dim fac As Decimal = dens_Num * (10000000000.0) ^ 3

            'Dim Zcalc2 = P / (kb * T * fac)

            result = P - Pcalc

            'result = Zcalc2 - Zcalc

            Return New Double() {result, Zcalc}

        End Function

        Friend Function Z_disp(T, dens_num, mix)

            'Dispersive contribution to the compressibility coefficient with PC-SAFT EoS
            'Auxiliary function, Not to be used directly
            '
            'Reference: Gross And Sadowski, Ind.Eng.Chem.Res. 40 (2001) 1244-1260

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            Dim a0(7), a1(7), a2(7), b0(7), b1(7), b2(7) As Double

            'Equation constants
            a0(1) = 0.9105631445
            a0(2) = 0.6361281449
            a0(3) = 2.6861347891
            a0(4) = -26.547362491
            a0(5) = 97.759208784
            a0(6) = -159.59154087
            a0(7) = 91.297774084
            a1(1) = -0.3084016918
            a1(2) = 0.1860531159
            a1(3) = -2.5030047259
            a1(4) = 21.419793629
            a1(5) = -65.25588533
            a1(6) = 83.318680481
            a1(7) = -33.74692293
            a2(1) = -0.0906148351
            a2(2) = 0.4527842806
            a2(3) = 0.5962700728
            a2(4) = -1.7241829131
            a2(5) = -4.1302112531
            a2(6) = 13.77663187
            a2(7) = -8.6728470368
            b0(1) = 0.7240946941
            b0(2) = 2.2382791861
            b0(3) = -4.0025849485
            b0(4) = -21.003576815
            b0(5) = 26.855641363
            b0(6) = 206.55133841
            b0(7) = -355.60235612
            b1(1) = -0.5755498075
            b1(2) = 0.6995095521
            b1(3) = 3.892567339
            b1(4) = -17.215471648
            b1(5) = 192.67226447
            b1(6) = -161.82646165
            b1(7) = -165.20769346
            b2(1) = 0.0976883116
            b2(2) = -0.2557574982
            b2(3) = -9.155856153
            b2(4) = 20.642075974
            b2(5) = -38.804430052
            b2(6) = 93.626774077
            b2(7) = -29.666905585

            Dim x, m, sigma, epsilon, d As Double()
            Dim k1(,) As Double

            'Reads pure-component properties

            numC = mix.numC
            x = mix.x
            k1 = mix.k1
            m = zeros(numC)
            sigma = zeros(numC)
            epsilon = zeros(numC)
            For i = 1 To numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(numC)
            For i = 1 To numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim m_prom, a(), b(), dens_red, sigmaij(,), epsilonij(,) As Double

            'mean segment number
            m_prom = 0
            For i = 1 To numC
                m_prom = m_prom + m(i) * x(i) 'Eq. 6 Of reference
            Next

            'Calculates the a And b parameters
            a = zeros(7)
            b = zeros(7)
            For j = 1 To 7
                a(j) = a0(j) + (m_prom - 1) / m_prom * a1(j) + (m_prom - 1) / m_prom * (m_prom - 2) / m_prom * a2(j) 'Eq. 18 Of reference
                b(j) = b0(j) + (m_prom - 1) / m_prom * b1(j) + (m_prom - 1) / m_prom * (m_prom - 2) / m_prom * b2(j) 'Eq. 19 Of reference
            Next

            'Reduced density
            dens_red = 0
            For i = 1 To numC
                dens_red = dens_red + x(i) * m(i) * d(i) ^ 3
            Next
            dens_red = dens_red * PI / 6 * dens_num 'Eq. 9 Of reference

            '**************************************************************************
            'Mixing rules
            '**************************************************************************
            sigmaij = zeros(numC, numC)
            epsilonij = zeros(numC, numC)
            For i = 1 To numC
                For j = 1 To numC
                    sigmaij(i, j) = 0.5 * (sigma(i) + sigma(j)) 'Eq. A14 Of reference
                    epsilonij(i, j) = Sqrt(epsilon(i) * epsilon(j)) * (1 - k1(i, j)) 'Eq A15 Of reference     	
                Next
            Next

            '**************************************************************************
            'Zdisp
            '**************************************************************************

            Dim dnuI1_dnu, dnuI2_dnu, term1, term2, C1, C2, prom1, prom2, I2, Zdisp As Double

            dnuI1_dnu = 0
            dnuI2_dnu = 0

            For j = 1 To 7
                dnuI1_dnu = dnuI1_dnu + a(j) * (j) * dens_red ^ (j - 1) 'Eq. A29 Of reference
                dnuI2_dnu = dnuI2_dnu + b(j) * (j) * dens_red ^ (j - 1) 'Eq. A30 Of reference
            Next

            term1 = (m_prom) * (8 * dens_red - 2 * dens_red ^ 2) / (1 - dens_red) ^ 4
            term2 = (1 - m_prom) * (20 * dens_red - 27 * dens_red ^ 2 + 12 * dens_red ^ 3 - 2 * dens_red ^ 4) / ((1 - dens_red) * (2 - dens_red)) ^ 2
            C1 = (1 + term1 + term2) ^ -1 'Eq. A11 Of reference

            term1 = m_prom * (-4 * dens_red ^ 2 + 20 * dens_red + 8) / (1 - dens_red) ^ 5
            term2 = (1 - m_prom) * (2 * dens_red ^ 3 + 12 * dens_red ^ 2 - 48 * dens_red + 40) / ((1 - dens_red) * (2 - dens_red)) ^ 3
            C2 = -C1 ^ 2 * (term1 + term2) 'Eq. A31 Of reference

            prom1 = 0
            prom2 = 0
            For i = 1 To numC
                For j = 1 To numC
                    prom1 = prom1 + x(i) * x(j) * m(i) * m(j) * epsilonij(i, j) / T * sigmaij(i, j) ^ 3 'Eq. A12 Of reference
                    prom2 = prom2 + x(i) * x(j) * m(i) * m(j) * (epsilonij(i, j) / T) ^ 2 * sigmaij(i, j) ^ 3 'Eq. A13 Of reference
                Next
            Next

            I2 = 0
            For j = 1 To 7
                I2 = I2 + b(j) * dens_red ^ (j - 1) 'Eq. A17 Of reference
            Next

            term1 = -2 * PI * dens_num * dnuI1_dnu * prom1
            term2 = -PI * dens_num * m_prom * (C1 * dnuI2_dnu + C2 * dens_red * I2) * prom2

            Zdisp = term1 + term2 'Eq. A28 Of reference

            Return Zdisp

        End Function

        Friend Function Z_hc(T, dens_num, mix)

            'Hard-chain contribution to the compressibility coefficient with PC-SAFT EoS
            'Auxiliary function, not to be used directly
            '
            'Reference: Gross and Sadowski, Ind. Eng. Chem. Res. 40 (2001) 1244-1260

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program is free software: you can redistribute it and/or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, or
            '(at your option) any later version.
            'This program is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If not, see <http://www.gnu.org/licenses/>.

            Dim x, m, sigma, epsilon, d As Double()

            'Reads pure-component properties
            numC = mix.numC
            x = mix.x
            m = zeros(numC)
            sigma = zeros(numC)
            epsilon = zeros(numC)
            For i = 1 To numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(numC)
            For i = 1 To numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim m_prom, auxil() As Double

            'mean segment number
            m_prom = 0
            For i = 1 To numC
                m_prom = m_prom + m(i) * x(i) 'Eq. 6 of reference
            Next

            'auxiliary functions
            auxil = zeros(4)
            For j = 1 To 4
                For i = 1 To numC
                    auxil(j) = auxil(j) + x(i) * m(i) * d(i) ^ (j - 1)
                Next
                auxil(j) = auxil(j) * PI / 6 * dens_num 'Eq. 9 of reference
            Next

            Dim ghs(,), term1, term2, term3, Zhs, dens_dg_ddens(,) As Double

            'radial distribution function
            ghs = zeros(numC, numC)
            For i = 1 To numC
                For j = 1 To numC
                    term1 = 1 / (1 - auxil(4))
                    term2 = d(i) * d(j) / (d(i) + d(j)) * 3 * auxil(3) / (1 - auxil(4)) ^ 2
                    term3 = (d(i) * d(j) / (d(i) + d(j))) ^ 2 * 2 * auxil(3) ^ 2 / (1 - auxil(4)) ^ 3
                    ghs(i, j) = term1 + term2 + term3 'Eq. 8 of reference
                Next
            Next

            '**************************************************************************
            'Zhc
            '**************************************************************************
            term1 = auxil(4) / (1 - auxil(4))
            term2 = 3 * auxil(2) * auxil(3) / (auxil(1) * (1 - auxil(4)) ^ 2)
            term3 = (3 * auxil(3) ^ 3 - auxil(4) * auxil(3) ^ 3) / (auxil(1) * (1 - auxil(4)) ^ 3)
            Zhs = term1 + term2 + term3 'Eq. A26 of reference

            dens_dg_ddens = zeros(mix.numC, mix.numC)
            For i = 1 To numC
                For j = 1 To numC
                    term1 = auxil(4) / (1 - auxil(4)) ^ 2
                    term2 = (d(i) * d(j)) / (d(i) + d(j)) * (3 * auxil(3) / (1 - auxil(4)) ^ 2 + 6 * auxil(3) * auxil(4) / (1 - auxil(4)) ^ 3)
                    term3 = (d(i) * d(j) / (d(i) + d(j))) ^ 2 * (4 * auxil(3) ^ 2 / (1 - auxil(4)) ^ 3 + 6 * auxil(3) ^ 2 * auxil(4) / (1 - auxil(4)) ^ 4)
                    dens_dg_ddens(i, j) = term1 + term2 + term3 'Eq. A27 of reference
                Next
            Next

            Dim sum1, Zhc As Double

            sum1 = 0
            For i = 1 To numC
                sum1 = sum1 + x(i) * (m(i) - 1) * ghs(i, i) ^ (-1) * dens_dg_ddens(i, i)
            Next

            Zhc = m_prom * Zhs - sum1 'Eq. A25 of reference

            Return Zhc

        End Function

        Friend Function mu_Ass(T, dens_num, mix)

            'Calculates the association contribution to the residual chemical potential 
            'of mixture mix at temperature T And pressure P using SAFT EoS
            '
            'Parameters:
            'EoS Equation of state used for calculations
            'T: Temperature(K)
            'P: Pressure(K)
            'dens_num: Number density(molecule / Angstrom ^ 3)
            'mix: cMixture Object
            '
            'Results:
            'muass residual chemical potential, association contribution
            'EoS: returns EoS used for calculations
            '
            'Reference: Chapman et al., Ind.Eng.Chem.Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            Dim numC, x(), indx1, indx2, kappa(,), epsilon(,), kappa_, epsilon_, kappa1, kappa2, epsilon1, epsilon2 As Double
            Dim m, sigma, epsilon0, d As Double()
            Dim NumAss() As Double

            'Reads pure-component properties
            numC = mix.numC
            x = mix.x
            m = zeros(numC)
            sigma = zeros(numC)
            epsilon0 = zeros(numC)
            NumAss = zeros(numC)
            For i = 1 To numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon0(i) = mix.comp(i).EoSParam(3)
                NumAss(i) = mix.comp(i).EoSParam(4)
            Next

            Dim kappa_v, epsilon_v As New List(Of Double(,))

            kappa_v.Add(New Double(,) {})
            epsilon_v.Add(New Double(,) {})

            numC = mix.numC
            x = mix.x
            For i = 1 To numC
                kappa_v.Add(mix.comp(i).EoSParam(5))
                epsilon_v.Add(mix.comp(i).EoSParam(6))
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(mix.numC)
            For i = 1 To numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon0(i))
            Next

            Dim auxil(), ghs(,), term1, term2, term3 As Double

            'auxiliary functions
            auxil = zeros(4)
            For j = 1 To 4
                For i = 1 To numC
                    auxil(j) = auxil(j) + x(i) * m(i) * d(i) ^ (j - 1)
                Next
                auxil(j) = auxil(j) * PI / 6 * dens_num 'Eq. 27 Of reference
            Next

            'radial distribution function
            ghs = zeros(numC, numC)
            For i = 1 To mix.numC
                For j = 1 To mix.numC
                    term1 = 1 / (1 - auxil(4))
                    term2 = d(i) * d(j) / (d(i) + d(j)) * 3 * auxil(3) / (1 - auxil(4)) ^ 2
                    term3 = (d(i) * d(j) / (d(i) + d(j))) ^ 2 * 2 * auxil(3) ^ 2 / (1 - auxil(4)) ^ 3
                    ghs(i, j) = term1 + term2 + term3 'Eq. 25 Of reference
                Next
            Next

            'Calculates the molar fraction of molecules Not bonded at association

            Dim iniL = New List(Of Double)

            For i = 0 To NumAss.Sum
                iniL.Add(0.001)
            Next

            Dim ini = iniL.ToArray

            Dim ovars As New List(Of DotNumerics.Optimization.OptSimplexVariable)
            For Each item In ini
                ovars.Add(New DotNumerics.Optimization.OptSimplexVariable(item))
            Next

            Dim opt As New DotNumerics.Optimization.Simplex()
            opt.MaxFunEvaluations = 10000
            opt.Tolerance = 1.0E-20
            Dim result = opt.ComputeMin(Function(myv() As Double)
                                            Return obj_HelmholtzAss(myv, mix, T, NumAss, sigma, d, ghs, dens_num)
                                        End Function, ovars.ToArray)

            Dim Xa = result

            Dim dgij_drok(,,), term4, term5, term6, term7 As Double

            'Derivatives for calculation of chemical potential

            dgij_drok = zeros(numC, numC, numC)

            For i = 1 To numC
                For j = 1 To numC
                    For k = 1 To numC
                        term1 = d(i) ^ 3 / (1 - auxil(4)) ^ 2
                        term2 = 3 * d(j) * d(k) / (d(j) + d(k))
                        term3 = d(i) ^ 2 / (1 - auxil(4)) ^ 2
                        term4 = 2 * d(i) ^ 3 * auxil(3) / (1 - auxil(4)) ^ 3
                        term5 = 2 * (d(j) * d(k) / (d(j) + d(k))) ^ 2
                        term6 = 2 * d(i) ^ 2 * auxil(3) / (1 - auxil(4)) ^ 3
                        term7 = 3 * d(i) ^ 3 * auxil(3) ^ 2 / (1 - auxil(4)) ^ 4
                        dgij_drok(j, k, i) = PI / 6 * m(i) * (term1 + term2 * (term3 + term4) + term5 * (term6 + term7)) 'Eq. A5 Of reference
                    Next
                Next
            Next

            Dim ddeltaAB_droi(,,) As Double

            ddeltaAB_droi = zeros((numC) * NumAss.Max, (numC) * NumAss.Max, numC)

            For i2 = 1 To numC
                indx1 = 0
                For i = 1 To numC
                    For j = 1 To NumAss(i)
                        indx1 = indx1 + 1
                        indx2 = 0
                        For k = 1 To numC
                            For l = 1 To NumAss(k)
                                indx2 = indx2 + 1
                                If i = k Then 'retrieves value from component matrix
                                    kappa = mix.comp(i).EoSParam(5)
                                    kappa_ = kappa(j, l)
                                    epsilon = mix.comp(i).EoSParam(6)
                                    epsilon_ = epsilon(j, l)
                                Else 'applies mixing rules
                                    kappa1 = max(mix.comp(i).EoSParam(5))
                                    epsilon1 = max(mix.comp(i).EoSParam(6))
                                    kappa2 = max(mix.comp(k).EoSParam(5))
                                    epsilon2 = max(mix.comp(k).EoSParam(6))
                                    kappa_ = Sqrt(kappa1 * kappa2) * (Sqrt(sigma(i) * sigma(k)) / (0.5 * (sigma(i) + sigma(k)))) ^ 3
                                    epsilon_ = 0.5 * (epsilon1 + epsilon2)
                                End If
                                ddeltaAB_droi(indx1, indx2, i2) = ((d(i) + d(k)) / 2) ^ 3 * dgij_drok(i, k, i2) * (Exp(epsilon_ / T) - 1) * kappa_
                            Next
                        Next
                    Next
                Next
            Next

            Dim dXaj_droi(,), dXaj_droi_v() As Double

            dXaj_droi_v = obj_muAss(mix, Xa, ddeltaAB_droi, T, NumAss, sigma, d, ghs, dens_num) 'Eq. A3 Of reference

            dXaj_droi = zeros(numC * NumAss.Max, numC)

            'Transforms column-vector parameter dXaj_droi into a matrix
            For i2 = 1 To numC
                indx1 = 0
                For i = 1 To numC
                    For j = 1 To NumAss(i)
                        indx1 = indx1 + 1
                        dXaj_droi(indx1, i2) = dXaj_droi_v((i2 - 1) * sum(NumAss) + indx1)
                    Next
                Next
            Next

            Dim sum1, term1_(), term2_(), muass() As Double

            term1_ = zeros(numC)
            term2_ = zeros(numC)

            'Association contribution to the chemical potential
            indx1 = 0
            For i = 1 To numC
                sum1 = 0
                For j = 1 To NumAss(i)
                    indx1 = indx1 + 1
                    sum1 = sum1 + Log(Xa(indx1)) - Xa(indx1) / 2
                Next
                term1_(i) = sum1 + 0.5 * NumAss(i)
            Next

            For i = 1 To numC
                indx1 = 0
                sum1 = 0
                For j = 1 To numC
                    For k = 1 To NumAss(j)
                        indx1 = indx1 + 1
                        sum1 = sum1 + dens_num * x(j) * (dXaj_droi(indx1, i) * (1 / Xa(indx1) - 0.5))
                    Next
                Next
                term2_(i) = sum1
            Next

            muass = zeros(numC)
            For i = 1 To numC
                muass(i) = term1_(i) + term2_(i) 'Eq A2 Of reference
            Next

            Return muass

        End Function

        Friend Function obj_muAss(mix, Xa, ddeltaAB_droi, T, NumAss, sigma, d, ghs, dens_num)

            'Auxiliary function for the calculation of associaton chemical potential
            'with SAFT (calculates eq. A3 of reference)
            '
            'Reference: Chapman et al., Ind.Eng.Chem.Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            numC = mix.numC

            Dim A(,), B(), indx1, indx2, indx3, sum1, kappa, epsilon, kappa_(,), epsilon_(,) As Double
            Dim epsilon1, epsilon2, kappa1, kappa2 As Double
            Dim delta(,), delta_ As Double
            Dim sum2 As Double

            A = zeros(sum(NumAss) * numC, sum(NumAss) * numC)
            B = zeros(sum(NumAss) * numC)

            delta = zeros(numC * numC, numC * DirectCast(NumAss, Double()).Max)

            indx3 = 0
            For i2 = 1 To numC
                indx1 = 0
                For i = 1 To numC
                    For j = 1 To NumAss(i)
                        indx1 = indx1 + 1
                        indx3 = indx3 + 1
                        indx2 = 0
                        sum1 = 0
                        For k = 1 To numC
                            For l = 1 To NumAss(k)
                                indx2 = indx2 + 1
                                If i = k Then 'retrieves value from component matrix
                                    kappa_ = mix.comp(i).EoSParam(5)
                                    kappa = kappa_(j, l)
                                    epsilon_ = mix.comp(i).EoSParam(6)
                                    epsilon = epsilon_(j, l)
                                Else 'applies mixing rules
                                    kappa1 = max(mix.comp(i).EoSParam(5))
                                    epsilon1 = max(mix.comp(i).EoSParam(6))
                                    kappa2 = max(mix.comp(k).EoSParam(5))
                                    epsilon2 = max(mix.comp(k).EoSParam(6))
                                    kappa = Sqrt(kappa1 * kappa2) * (Sqrt(sigma(i) * sigma(k)) / (0.5 * (sigma(i) + sigma(k)))) ^ 3
                                    epsilon = 0.5 * (epsilon1 + epsilon2)
                                End If
                                delta(indx1, indx2) = ((d(i) + d(k)) / 2) ^ 3 * ghs(i, k) * kappa * (Exp(epsilon / T) - 1)
                                sum1 = sum1 + dens_num * mix.x(k) * (Xa(indx2) * ddeltaAB_droi(indx1, indx2, i2))
                                A(indx1 + (i2 - 1) * sum(NumAss), indx2 + (i2 - 1) * sum(NumAss)) = A(indx1 + (i2 - 1) * sum(NumAss), indx2 + (i2 - 1) * sum(NumAss)) + Xa(indx1) ^ 2 * dens_num * mix.x(k) * delta(indx1, indx2)
                            Next
                        Next

                        sum2 = 0
                        For k = 1 To NumAss(i2)
                            If i = i2 Then 'retrieves value from component matrix
                                kappa_ = mix.comp(i).EoSParam(5)
                                kappa = kappa_(j, k)
                                epsilon_ = mix.comp(i).EoSParam(6)
                                epsilon = epsilon_(j, k)
                            Else 'applies mixing rules
                                kappa1 = max(mix.comp(i).EoSParam(5))
                                epsilon1 = max(mix.comp(i).EoSParam(6))
                                kappa2 = max(mix.comp(i2).EoSParam(5))
                                epsilon2 = max(mix.comp(i2).EoSParam(6))
                                kappa = Sqrt(kappa1 * kappa2) * (Sqrt(sigma(i) * sigma(i2)) / (0.5 * (sigma(i) + sigma(i2)))) ^ 3
                                epsilon = 0.5 * (epsilon1 + epsilon2)
                            End If
                            delta_ = ((d(i) + d(i2)) / 2) ^ 3 * ghs(i, i2) * kappa * (Exp(epsilon / T) - 1)
                            sum2 = sum2 + Xa(DirectCast(NumAss, Double()).Take(i2 - 1).Sum + k) * delta_
                        Next
                        A(indx3, indx3) = A(indx3, indx3) + 1
                        B(indx3) = -(Xa(indx1)) ^ 2 * (sum1 + sum2)
                    Next
                Next
            Next

            'Solves linear system of equations

            Dim A2 = zeros(sum(NumAss) * numC - 1, sum(NumAss) * numC - 1)
            Dim B2 = zeros(sum(NumAss) * numC - 1)

            Dim solution = zeros(sum(NumAss) * numC)
            Dim solution2 = zeros(sum(NumAss) * numC - 1)

            For i = 0 To A.GetLength(0) - 2
                For j = 0 To A.GetLength(1) - 2
                    A2(i, j) = A(i + 1, j + 1)
                Next
                B2(i) = B(i + 1)
            Next

            Dim result = DWSIM.MathOps.MathEx.SysLin.rsolve.rmatrixsolve(A2, B2, B2.Length, solution2)

            solution2.Take(solution2.Length - 1).ToArray.CopyTo(solution, 1)

            Return solution

        End Function

        Friend Function Z_ass(T, dens_num, mix)

            'Associating contribution to the compressibility coefficient with SAFT EoS
            'Auxiliary function, Not to be used directly
            '
            'Reference: Chapman et al., Ind.Eng.Chem.Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            Dim x As Double()

            'Reads pure-component properties
            numC = mix.numC
            x = mix.x

            Dim NumAss(), muass(), Aass, sum1, Zass As Double

            NumAss = zeros(numC)
            For i = 1 To numC
                NumAss(i) = mix.comp(i).EoSParam(4)
            Next

            If sum(NumAss) > 0 Then

                muass = mu_Ass(T, dens_num, mix)
                Aass = HelmholtzAss(T, dens_num, mix)

                sum1 = 0
                For i = 1 To numC
                    sum1 = sum1 + x(i) * muass(i)
                Next

                Zass = sum1 - Aass 'Eq. A10 Of reference

            Else

                Zass = 0

            End If

            Return Zass

        End Function

        Friend Function HelmholtzAss(T, dens_num, mix)

            'Calculates the association contribution to the residual Helmholtz energy 
            'of mixture mix at temperature T And pressure P using SAFT EoS
            '
            'Parameters:
            'EoS Equation of state used for calculations
            'T: Temperature(K)
            'dens_num: Number density(molecule / Angstrom ^ 3)
            'mix: cMixture Object
            '
            'Results:
            'Aass residual Helmholtz energy, association contribution
            'Xa: Fraction of associated sites
            'EoS: returns EoS used for calculations
            '
            'Reference Chapman et al., Ind. Eng. Chem. Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            Dim x, m, sigma, epsilon, d As Double()
            Dim NumAss() As Double

            'Reads pure-component properties
            numC = mix.numC
            x = mix.x
            m = zeros(numC)
            sigma = zeros(numC)
            epsilon = zeros(numC)
            NumAss = zeros(numC)
            For i = 1 To numC
                m(i) = mix.comp(i).EoSParam(1)
                sigma(i) = mix.comp(i).EoSParam(2)
                epsilon(i) = mix.comp(i).EoSParam(3)
                NumAss(i) = mix.comp(i).EoSParam(4)
            Next

            'Calculates the temperature-depNextant segment diameter
            d = zeros(numC)
            For i = 1 To numC
                d(i) = HardSphereDiameter(T, m(i), sigma(i), epsilon(i))
            Next

            Dim auxil(), ghs(,), term1, term2, term3 As Double

            'auxiliary functions
            auxil = zeros(4)
            For j = 1 To 4
                For i = 1 To numC
                    auxil(j) = auxil(j) + x(i) * m(i) * d(i) ^ (j - 1)
                Next
                auxil(j) = auxil(j) * PI / 6 * dens_num 'Eq. 27 Of reference
            Next

            'radial distribution function
            ghs = zeros(numC, numC)
            For i = 1 To numC
                For j = 1 To numC
                    term1 = 1 / (1 - auxil(4))
                    term2 = d(i) * d(j) / (d(i) + d(j)) * 3 * auxil(3) / (1 - auxil(4)) ^ 2
                    term3 = (d(i) * d(j) / (d(i) + d(j))) ^ 2 * 2 * auxil(3) ^ 2 / (1 - auxil(4)) ^ 3
                    ghs(i, j) = term1 + term2 + term3 'Eq. 25 Of reference
                Next
            Next

            Dim Aass, indx1, sum1 As Double

            'Calculates the molar fraction of molecules Not bonded at association

            Dim iniL = New List(Of Double)

            For i = 0 To NumAss.Sum
                iniL.Add(0.001)
            Next

            Dim ini = iniL.ToArray

            Dim ovars As New List(Of DotNumerics.Optimization.OptSimplexVariable)
            For Each item In ini
                ovars.Add(New DotNumerics.Optimization.OptSimplexVariable(item))
            Next

            Dim opt As New DotNumerics.Optimization.Simplex
            opt.MaxFunEvaluations = 10000
            opt.Tolerance = 1.0E-20
            Dim result = opt.ComputeMin(Function(myv() As Double)
                                            Return obj_HelmholtzAss(myv, mix, T, NumAss, sigma, d, ghs, dens_num)
                                        End Function,
                                    ovars.ToArray)

            Dim Xa = result

            'Association contribution to Helmholtz energy
            Aass = 0
            indx1 = 0
            For i = 1 To numC
                sum1 = 0
                For j = 1 To NumAss(i)
                    indx1 = indx1 + 1
                    sum1 = sum1 + Log(Xa(indx1)) - Xa(indx1) / 2
                Next
                Aass = Aass + x(i) * (sum1 + 0.5 * NumAss(i)) 'Eq. 21 Of reference
            Next

            Return Aass

        End Function

        Friend Function compr(T, P, mix, phase, Zestimate)

            'Calculates the compressibility coefficient of mixture mix at temperature T
            'And pressure P using SAFT EoS
            '
            'Parameters:
            'EoS Equation of state used for calculations
            'T: Temperature(K)
            'P: Pressure(Pa)
            'mix: cMixture Object
            'phase: set  phase = 'liq' to get the coefficient of the liquid phase, phase = 'gas'  
            '   to get the coefficient of the gas phase 
            '
            'Optional parameters (set [] to keep default value)
            'Z_ini: Initial guess for the compressibility coefficient
            '   If Not defined, the program uses an initial guess Z_ini = 0.8 for gas
            '   phase And a Z_ini corresponding to a liquid density of 800 kg/m3 for
            '   the liquid phase
            'options: parameters of the fsolve numerical resolution method (structure
            '   generated with "optimset")
            '
            'Results:
            'Z compresibility coefficient
            'EoS: returns EoS used for calculations

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            '**************************************************************************
            'Initial guess
            '**************************************************************************

            Dim ro, sumat, ini, Zini As Double

            If Zestimate = -1 Then
                If phase = "gas" Then
                    ro = P / (0.8 * 8.31 * T) * 1.0E-30 * 6.022E+23 'molecule/A3
                    sumat = 0
                    For i = 1 To mix.numC
                        sumat = sumat + mix.x(i) * mix.comp(i).EoSParam(1) * (mix.comp(i).EoSParam(2)) ^ 3
                    Next
                    ini = PI / 6 * ro * sumat
                ElseIf phase = "liq" Then
                    ro = 800 * 1000 / mix.MW * 1.0E-30 * 6.022E+23 'molecule/A3
                    sumat = 0
                    For i = 1 To mix.numC
                        sumat = sumat + mix.x(i) * mix.comp(i).EoSParam(1) * (mix.comp(i).EoSParam(2)) ^ 3
                    Next
                    ini = PI / 6 * ro * sumat
                Else
                    Throw New Exception("Undefined phase type, must be liq or gas")
                End If
            Else
                Zini = Zestimate
                ro = P / (Zini * 8.31 * T) * 1.0E-30 * 6.022E+23 'molecule/A3
                sumat = 0
                For i = 1 To mix.numC
                    sumat = sumat + mix.x(i) * mix.comp(i).EoSParam(1) * (mix.comp(i).EoSParam(2)) ^ 3
                Next
                ini = PI / 6 * ro * sumat
            End If

            '**************************************************************************
            'Calculates Z And mu with SAFT
            '**************************************************************************

            'If phase = "liq" Then

            '    Dim intervals As New List(Of Tuple(Of Double, Double))

            '    Dim minval, maxval As Double

            '    ro = 1400 * 1000 / mix.MW * 1.0E-30 * 6.022E+23 'molecule/A3
            '    sumat = 0
            '    For i = 1 To mix.numC
            '        sumat = sumat + mix.x(i) * mix.comp(i).EoSParam(1) * (mix.comp(i).EoSParam(2)) ^ 3
            '    Next
            '    maxval = PI / 6 * ro * sumat

            '    ro = 300 * 1000 / mix.MW * 1.0E-30 * 6.022E+23 'molecule/A3
            '    sumat = 0
            '    For i = 1 To mix.numC
            '        sumat = sumat + mix.x(i) * mix.comp(i).EoSParam(1) * (mix.comp(i).EoSParam(2)) ^ 3
            '    Next
            '    minval = PI / 6 * ro * sumat

            '    Dim f1, f2, x1, delta, dens As Double
            '    delta = (maxval - minval) / 20
            '    x1 = minval
            '    While x1 <= maxval
            '        Do
            '            f1 = obj_SAFT(x1, T, P, mix)(0)
            '            f2 = obj_SAFT(x1 + delta, T, P, mix)(0)
            '            x1 += delta
            '        Loop Until f1 * f2 < 0.0 Or x1 >= maxval
            '        If x1 < maxval Then intervals.Add(New Tuple(Of Double, Double)(x1 - delta, x1))
            '    End While

            '    Dim brent As New DWSIM.MathOps.MathEx.BrentOpt.Brent()
            '    brent.DefineFuncDelegate(Function(x, otherargs) obj_SAFT(x, T, P, mix)(0))
            '    Dim Zvec As New List(Of Double)
            '    For Each interval In intervals
            '        dens = brent.BrentOpt(interval.Item1, interval.Item2, 10, 0.0001, 1000, Nothing)
            '        Zvec.Add(obj_SAFT(dens, T, P, mix)(1))
            '    Next

            '    Return Zvec.Min

            'Else

            Dim ovars As New List(Of DotNumerics.Optimization.OptSimplexBoundVariable)
            ovars.Add(New DotNumerics.Optimization.OptSimplexBoundVariable(ini, ini * 0.001, ini * 200))

            Dim opt As New DotNumerics.Optimization.Simplex()
            opt.MaxFunEvaluations = 100000
            opt.Tolerance = 1.0E-20

            Dim result = opt.ComputeMin(Function(myv() As Double)
                                            Return obj_SAFT(myv(0), T, P, mix)(0) ^ 2
                                        End Function,
                                        ovars.ToArray)

            Return obj_SAFT(result(0), T, P, mix)(1)

            'End If

        End Function

        Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
            Return True
        End Function

        Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(),
         ByRef jCol As Integer(), ByRef values As Double()) As Boolean
            Return False
        End Function

        Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(),
         ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean
            Return False
        End Function

        Friend Function obj_HelmholtzAss(Xa, mix, T, NumAss, sigma, d, ghs, dens_num)

            'Calculates the fraction for association site in the PC-SAFT EoS
            'Auxiliary function, Not to be used directly
            '
            'Reference: Chapman et al., Ind.Eng.Chem.Res. 29 (1990) 1709-1721

            'Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
            'This program Is free software: you can redistribute it And/Or modify
            'it under the terms of the GNU Lesser General Public License as published by
            'the Free Software Foundation, either version 3 of the License, Or
            '(at your option) any later version.
            'This program Is distributed in the hope that it will be useful,
            'but WITHOUT ANY WARRANTY without even the implied warranty of
            'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
            'GNU Lesser General Public License for more details.
            'You should have received a copy of the GNU Lesser General Public License
            'along with this program.  If Not, see <http://www.gnu.org/licenses/>.

            Dim numC, x(), indx1, indx2, kappa(,), epsilon(,), kappa_, epsilon_, kappa1, kappa2, epsilon1, epsilon2 As Double

            Dim kappa_v, epsilon_v As New List(Of Double(,))

            kappa_v.Add(New Double(,) {})
            epsilon_v.Add(New Double(,) {})

            numC = mix.numC
            x = mix.x
            For i = 1 To numC
                kappa_v.Add(mix.comp(i).EoSParam(5))
                epsilon_v.Add(mix.comp(i).EoSParam(6))
            Next

            Dim delta = zeros(sum(NumAss), sum(NumAss))

            indx1 = 0

            For i = 1 To numC
                For j = 1 To NumAss(i)
                    indx1 = indx1 + 1
                    indx2 = 0
                    For k = 1 To numC
                        For l = 1 To NumAss(k)
                            indx2 = indx2 + 1
                            If i = k Then 'retrieves value from component matrix
                                kappa = kappa_v(i)
                                kappa_ = kappa(j, l)
                                epsilon = epsilon_v(i)
                                epsilon_ = epsilon(j, l)
                            Else 'applies mixing rules
                                kappa1 = max(kappa_v(i))
                                epsilon1 = max(epsilon_v(i))
                                kappa2 = max(kappa_v(k))
                                epsilon2 = max(epsilon_v(k))
                                kappa_ = Sqrt(kappa1 * kappa2) * (Sqrt(sigma(i) * sigma(k)) / (0.5 * (sigma(i) + sigma(k)))) ^ 3
                                epsilon_ = 0.5 * (epsilon1 + epsilon2)
                            End If
                            delta(indx1, indx2) = ((d(i) + d(k)) / 2) ^ 3 * ghs(i, k) * kappa_ * (Exp(epsilon_ / T) - 1)
                        Next
                    Next
                Next
            Next

            Dim sum1 As Double

            'Calculates the equations
            Dim Res = zeros(sum(NumAss))
            indx1 = 0
            For i = 1 To numC
                For j = 1 To NumAss(i)
                    indx1 = indx1 + 1
                    indx2 = 0
                    sum1 = 0
                    For k = 1 To numC
                        For l = 1 To NumAss(k)
                            indx2 = indx2 + 1
                            sum1 = sum1 + x(k) * dens_num * Xa(indx2) * delta(indx1, indx2)
                        Next
                    Next
                    Res(indx1) = Xa(indx1) - (1 + sum1) ^ -1
                Next
            Next

            'Calculates the jacobian
            'indx1 = 0
            'Dim jv = zeros(sum(NumAss), sum(NumAss))

            'For i = 1 To numC
            '    For j = 1 To NumAss(i)
            '        indx1 = indx1 + 1
            '        indx2 = 0
            '        sum1 = 0
            '        For k = 1 To numC
            '            For l = 1 To NumAss(k)
            '                indx2 = indx2 + 1
            '                sum1 = sum1 + x(k) * dens_num * Xa(indx2) * delta(indx1, indx2)
            '            Next
            '        Next
            '        indx2 = 0
            '        For k = 1 To numC
            '            For l = 1 To NumAss(k)
            '                indx2 = indx2 + 1
            '                jv(indx1, indx2) = jv(indx1, indx2) + x(k) * dens_num * delta(indx1, indx2) / (1 + sum1) ^ 2
            '            Next
            '        Next
            '        jv(indx1, indx1) = jv(indx1, indx1) + 1
            '    Next
            'Next

            Return Res.AbsSqrSumY

        End Function

    End Class

End Namespace