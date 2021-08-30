Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports System.Collections.Generic

Public Class WilsonModel

    Implements DWSIM.Thermodynamics.PropertyPackages.Auxiliary.IActivityCoefficientBase

    ''' <summary>
    ''' Wilson Binary Interaction Parameters.
    ''' First key is Compound 1 CAS ID
    ''' Second key is Compound 2 CAS ID
    ''' Value is a double array containing the A12 and A21 parameters in cal/mol
    ''' </summary>
    ''' <returns></returns>
    Public Property BIPs As Dictionary(Of String, Dictionary(Of String, Double()))

    Sub New()

        BIPs = New Dictionary(Of String, Dictionary(Of String, Double()))()

        Dim lines As New List(Of String)
        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.wilson_bips.csv")
            Using t As New IO.StreamReader(filestr)
                While Not t.EndOfStream
                    lines.Add(t.ReadLine)
                End While
            End Using
        End Using

        For Each line In lines
            Dim cas1 = line.Split(";")(0)
            Dim cas2 = line.Split(";")(1)
            Dim A12 = Double.Parse(line.Split(";")(2), System.Globalization.CultureInfo.InvariantCulture)
            Dim A21 = Double.Parse(line.Split(";")(3), System.Globalization.CultureInfo.InvariantCulture)
            If BIPs.ContainsKey(cas1) Then
                If BIPs(cas1).ContainsKey(cas2) Then
                    BIPs(cas1)(cas2) = New Double() {A12, A21}
                Else
                    BIPs(cas1).Add(cas2, New Double() {A12, A21})
                End If
            Else
                BIPs.Add(cas1, New Dictionary(Of String, Double()))
                BIPs(cas1).Add(cas2, New Double() {A12, A21})
            End If
        Next

    End Sub

    Public Function GetBIPs(cas1 As String, cas2 As String) As Double

        If BIPs.ContainsKey(cas1) Then
            If BIPs(cas1).ContainsKey(cas2) Then
                'return A12
                Return BIPs(cas1)(cas2)(0)
            Else
                If BIPs.ContainsKey(cas2) Then
                    If BIPs(cas2).ContainsKey(cas1) Then
                        'return A21
                        Return BIPs(cas2)(cas1)(1)
                    Else
                        Return 0.0
                    End If
                Else
                    Return 0.0
                End If
            End If
        ElseIf BIPs.ContainsKey(cas2) Then
            If BIPs(cas2).ContainsKey(cas1) Then
                'return A21
                Return BIPs(cas2)(cas1)(1)
            Else
                Return 0.0
            End If
        Else
            Return 0.0
        End If

    End Function

    Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

        'T = Temperature in K
        'Vx = liquid phase molar composition
        'otherargs = first element = CAS IDs, second element = molar volumes

        Dim CASIDs As String() = otherargs(0)
        Dim MolarVolumes As Double() = otherargs(1)

        Dim n = Vx.Length - 1

        Dim Lambda_ij(n, n), BIP As Double

        Dim i, j As Integer

        For i = 0 To n
            For j = 0 To n
                BIP = GetBIPs(CASIDs(i), CASIDs(j))
                'BIP (A12 or A21) is in cal/mol
                Lambda_ij(i, j) = MolarVolumes(j) / MolarVolumes(i) * Math.Exp(-BIP / (1.9872 * T))
            Next
        Next

        Dim sum1(n) As Double

        For i = 0 To n
            sum1(i) = 0.0
            For j = 0 To n
                sum1(i) += Vx(j) * Lambda_ij(i, j)
            Next
        Next

        Dim sum2(n) As Double

        Dim k As Integer

        For i = 0 To n
            sum2(i) = 0.0
            For k = 0 To n
                sum2(i) += Vx(k) * Lambda_ij(k, i) / sum1(k)
            Next
        Next

        Dim lnactcoeff(n), actcoeff(n) As Double

        For i = 0 To n
            lnactcoeff(i) = -Math.Log(sum1(i)) + 1 - sum2(i)
            actcoeff(i) = Math.Exp(lnactcoeff(i))
        Next

        Return actcoeff

    End Function

    Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

        Dim gamma1 = CalcActivityCoefficients(T - 0.01, Vx, otherargs)
        Dim gamma2 = CalcActivityCoefficients(T + 0.01, Vx, otherargs)

        Dim sum As Double = 0
        For i As Integer = 0 To Vx.Length - 1
            sum += Vx(i) * (gamma2(i) - gamma1(i)) / 0.02
        Next

        Dim Hex = -8.314 * T ^ 2 * sum

        Return Hex 'kJ/kmol

    End Function

    Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

        Dim hex1, hex2, cpex As Double

        Dim epsilon As Double = 0.001

        hex1 = CalcExcessEnthalpy(T - epsilon, Vx, otherargs)
        hex2 = CalcExcessEnthalpy(T + epsilon, Vx, otherargs)

        cpex = (hex2 - hex1) / (2 * epsilon)

        Return cpex 'kJ/kmol.K

    End Function

End Class
