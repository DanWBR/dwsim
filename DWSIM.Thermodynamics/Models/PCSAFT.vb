'    PC-SAFT (without association term) Property Package 
'    Copyright 2011 Daniel Wagner O. de Medeiros
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

Imports System.Collections.Generic
Imports FileHelpers
Imports System.Runtime.InteropServices

Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> Public Class PCSParam

        Public compound As String = ""
        Public casno As String = ""
        Public mw As Double = 0.0#
        Public m As Double = 0.0#
        Public sigma As Double = 0.0#
        Public epsilon As Double = 0.0#
        <FieldNullValue(0.0#)> Public kAiBi As Double = 0.0#
        <FieldNullValue(0.0#)> Public epsilon2 As Double = 0.0#

    End Class

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> Public Class PCSIP

        Implements ICloneable

        Public compound1 As String = ""
        Public casno1 As String = ""
        Public compound2 As String = ""
        Public casno2 As String = ""
        Public kij As Double = 0.0#

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New PCSIP
            With newclass
                .compound1 = Me.compound1
                .compound2 = Me.compound2
                .casno1 = Me.casno1
                .casno2 = Me.casno2
                .kij = Me.kij
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class PCSAFT

        Public Declare Sub PHIEOS Lib "pc_saft_prop.dll" Alias "phieos" (<[In](), Out()> ByVal phi As Double(), ByRef h_res As Double, ByRef s_res As Double, ByRef g_res As Double, ByRef zges As Double, <[In]()> ByVal x As Double(), ByRef t As Double, ByRef p As Double, <[In](), Out()> ByVal parame As Double(,), <[In](), Out()> ByVal kij As Double(,), ByRef ncomp As Integer, ByRef densta As Double, ByRef dense As Double)

        Private _data As Dictionary(Of String, PCSParam)

        Public ReadOnly Property Data() As Dictionary(Of String, PCSParam)
            Get
                Return _data
            End Get
        End Property

        Private _ip As Dictionary(Of String, Dictionary(Of String, PCSIP))

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, PCSIP))
            Get
                Return _ip
            End Get
        End Property

        Sub New()

            _data = New Dictionary(Of String, PCSParam)
            _ip = New Dictionary(Of String, Dictionary(Of String, PCSIP))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim pcsaftdata As PCSParam
            Dim pcsaftdatac() As PCSParam
            Dim fh1 As New FileHelperEngine(Of PCSParam)
            With fh1
                pcsaftdatac = .ReadFile(My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "pcsaft.dat")
            End With

            For Each pcsaftdata In pcsaftdatac
                If Not _data.ContainsKey(pcsaftdata.casno) Then _data.Add(pcsaftdata.casno, pcsaftdata)
            Next

            fh1 = Nothing

            Dim prip As PCSIP
            Dim pripc() As PCSIP
            Dim fh2 As New FileHelperEngine(Of PCSIP)
            pripc = fh2.ReadFile(My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "pcsaft_ip.dat")

            For Each prip In pripc
                If Me.InteractionParameters.ContainsKey(prip.casno1) Then
                    If Me.InteractionParameters(prip.casno1).ContainsKey(prip.casno2) Then
                    Else
                        Me.InteractionParameters(prip.casno1).Add(prip.casno2, prip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(prip.casno1, New Dictionary(Of String, PCSIP))
                    Me.InteractionParameters(prip.casno1).Add(prip.casno2, prip.Clone)
                End If
            Next
            For Each prip In pripc
                If Me.InteractionParameters.ContainsKey(prip.casno1) Then
                    If Me.InteractionParameters(prip.casno1).ContainsKey(prip.casno2) Then
                    Else
                        Me.InteractionParameters(prip.casno1).Add(prip.casno2, prip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(prip.casno1, New Dictionary(Of String, PCSIP))
                    Me.InteractionParameters(prip.casno1).Add(prip.casno2, prip.Clone)
                End If
            Next

            prip = Nothing
            pripc = Nothing
            fh2 = Nothing

        End Sub

        Function CalcZ(ByVal Vx As Double(), ByVal VKij As Double(,), ByVal T As Double, ByVal P As Double, ByVal casnos() As String, ByVal state As String) As Double

            Dim ncomp As Integer = UBound(Vx)
            Dim phi(ncomp) As Double, h_res, s_res, g_res, zges, parame(ncomp, 2), kij(ncomp, ncomp), densta, dense As Double
            
            Dim i, j As Integer

            For i = 0 To ncomp
                If _data.ContainsKey(casnos(i)) Then
                    parame(i, 0) = _data(casnos(i)).m
                    parame(i, 1) = _data(casnos(i)).sigma
                    parame(i, 2) = _data(casnos(i)).epsilon
                End If
            Next

            If state = "L" Then
                densta = 0.4
                dense = 0.0#
            Else
                densta = 0.0000000001
                dense = 0.0#
            End If

            For i = 0 To ncomp
                For j = 0 To ncomp
                    kij(i, j) = VKij(i, j)
                Next
            Next

            PHIEOS(phi, h_res, s_res, g_res, zges, Vx, T, P, parame, kij, ncomp + 1, densta, dense)

            Return zges

        End Function

        Function CalcLnFugCoeff(ByVal Vx As Double(), ByVal VKij As Double(,), ByVal T As Double, ByVal P As Double, ByVal casnos() As String, ByVal state As String) As Double()

            Dim ncomp As Integer = UBound(Vx)
            Dim phi(ncomp) As Double, h_res, s_res, g_res, zges, parame(ncomp, 2), kij(ncomp, ncomp), densta, dense As Double

            Dim i, j As Integer

            For i = 0 To ncomp
                If _data.ContainsKey(casnos(i)) Then
                    parame(i, 0) = _data(casnos(i)).m
                    parame(i, 1) = _data(casnos(i)).sigma
                    parame(i, 2) = _data(casnos(i)).epsilon
                End If
            Next

            If state = "L" Then
                densta = 0.4
                dense = 0.0#
            Else
                densta = 0.0000000001
                dense = 0.0#
            End If

            For i = 0 To ncomp
                For j = 0 To ncomp
                    kij(i, j) = VKij(i, j)
                Next
            Next

            PHIEOS(phi, h_res, s_res, g_res, zges, Vx, T, P, parame, kij, ncomp + 1, densta, dense)

            Dim lnfug(ncomp) As Double

            For i = 0 To ncomp
                lnfug(i) = Math.Log(phi(i))
            Next

            Return lnfug

        End Function

        Function CalcHRES(ByVal state As String, ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VMM As Double(), ByVal casnos() As String, ByVal Hid As Double) As Double

            Dim ncomp As Integer = UBound(Vx)
            Dim phi(ncomp) As Double, h_res, s_res, g_res, zges, parame(ncomp, 2), kij(ncomp, ncomp), densta, dense As Double

            Dim i, j As Integer

            For i = 0 To ncomp
                If _data.ContainsKey(casnos(i)) Then
                    parame(i, 0) = _data(casnos(i)).m
                    parame(i, 1) = _data(casnos(i)).sigma
                    parame(i, 2) = _data(casnos(i)).epsilon
                End If
            Next

            If state = "L" Then
                densta = 0.4
                dense = 0.0#
            Else
                densta = 0.0000000001
                dense = 0.0#
            End If

            For i = 0 To ncomp
                For j = 0 To ncomp
                    kij(i, j) = VKij(i, j)
                Next
            Next

            PHIEOS(phi, h_res, s_res, g_res, zges, Vx, T, P, parame, kij, ncomp + 1, densta, dense)

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vx(i) * VMM(i)
                i += 1
            Loop Until i = ncomp + 1

            Return h_res / MMm + Hid

        End Function

        Function CalcSRES(ByVal state As String, ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VMM As Double(), ByVal casnos() As String, ByVal Sid As Double) As Double

            Dim ncomp As Integer = UBound(Vx)
            Dim phi(ncomp) As Double, h_res, s_res, g_res, zges, parame(ncomp, 2), kij(ncomp, ncomp), densta, dense As Double

            Dim i, j As Integer

            For i = 0 To ncomp
                If _data.ContainsKey(casnos(i)) Then
                    parame(i, 0) = _data(casnos(i)).m
                    parame(i, 1) = _data(casnos(i)).sigma
                    parame(i, 2) = _data(casnos(i)).epsilon
                End If
            Next

            If state = "L" Then
                densta = 0.4
                dense = 0.0#
            Else
                densta = 0.0000000001
                dense = 0.0#
            End If

            For i = 0 To ncomp
                For j = 0 To ncomp
                    kij(i, j) = VKij(i, j)
                Next
            Next

            PHIEOS(phi, h_res, s_res, g_res, zges, Vx, T, P, parame, kij, ncomp + 1, densta, dense)

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vx(i) * VMM(i)
                i += 1
            Loop Until i = ncomp + 1

            Return s_res / MMm + Sid

        End Function

        Function CalcPartialVolume(ByVal Vx As Double(), ByVal VKij As Double(,), ByVal T As Double, ByVal P As Double, ByVal casnos() As String, ByVal state As String) As Double()

            Dim lnfug1, lnfug2 As Object
            Dim P1, P2 As Double
            P1 = P
            P2 = P + 1

            lnfug1 = Me.CalcLnFugCoeff(Vx, VKij, T, P1, casnos, state)
            lnfug2 = Me.CalcLnFugCoeff(Vx, VKij, T, P2, casnos, state)

            Dim i As Integer
            Dim n As Integer = UBound(lnfug1)

            Dim partvol(n) As Double

            i = 0
            For i = 0 To n
                partvol(i) = (Math.Log(Math.Exp(lnfug2(i)) * Vx(i) * P2) - Math.Log(Math.Exp(lnfug1(i)) * Vx(i) * P1)) / 1 * (8.314 * T) 'm3/mol
                If Double.IsNaN(partvol(i)) Then partvol(i) = 0
            Next

            Return partvol

        End Function

        Function CalcCp(ByVal state As String, ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VMM As Double(), ByVal casnos() As String, ByVal Cpid As Double) As Double

            Dim ncomp As Integer = UBound(Vx)
            Dim phi(ncomp) As Double, h_res1, h_res2, s_res, g_res, zges, parame(ncomp, 2), kij(ncomp, ncomp), densta, dense, t2 As Double

            Dim i, j As Integer

            For i = 0 To ncomp
                If _data.ContainsKey(casnos(i)) Then
                    parame(i, 0) = _data(casnos(i)).m
                    parame(i, 1) = _data(casnos(i)).sigma
                    parame(i, 2) = _data(casnos(i)).epsilon
                End If
            Next

            If state = "L" Then
                densta = 0.4
                dense = 0.0#
            Else
                densta = 0.0000000001
                dense = 0.0#
            End If

            For i = 0 To ncomp
                For j = 0 To ncomp
                    kij(i, j) = VKij(i, j)
                Next
            Next

            t2 = T + 0.01

            PHIEOS(phi, h_res1, s_res, g_res, zges, Vx, T, P, parame, kij, ncomp + 1, densta, dense)
            PHIEOS(phi, h_res2, s_res, g_res, zges, Vx, t2, P, parame, kij, ncomp + 1, densta, dense)

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vx(i) * VMM(i)
                i += 1
            Loop Until i = ncomp + 1

            Dim res As Double = -(h_res1 - h_res2) / MMm / 0.01 + Cpid
            Return res

        End Function

        Function CalcCv(ByVal state As String, ByVal T As Double, ByVal P As Double, ByVal Vx As Double(), ByVal VKij As Double(,), ByVal VMM As Double(), ByVal casnos() As String, ByVal Cpid As Double) As Double

            Dim ncomp As Integer = UBound(Vx)
            Dim phi(ncomp) As Double, h_res1, h_res2, s_res, g_res, zges, parame(ncomp, 2), kij(ncomp, ncomp), densta, dense, t2 As Double

            Dim i, j As Integer

            For i = 0 To ncomp
                If _data.ContainsKey(casnos(i)) Then
                    parame(i, 0) = _data(casnos(i)).m
                    parame(i, 1) = _data(casnos(i)).sigma
                    parame(i, 2) = _data(casnos(i)).epsilon
                End If
            Next

            If state = "L" Then
                densta = 0.4
                dense = 0.0#
            Else
                densta = 0.0000000001
                dense = 0.0#
            End If

            For i = 0 To ncomp
                For j = 0 To ncomp
                    kij(i, j) = VKij(i, j)
                Next
            Next

            t2 = T + 0.01

            PHIEOS(phi, h_res1, s_res, g_res, zges, Vx, T, P, parame, kij, ncomp + 1, densta, dense)
            PHIEOS(phi, h_res2, s_res, g_res, zges, Vx, t2, P, parame, kij, ncomp + 1, densta, dense)

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vx(i) * VMM(i)
                i += 1
            Loop Until i = ncomp + 1

            Dim res As Double = -((h_res1 - h_res2) / 0.01 + 8.314) / MMm + Cpid
            Return res

        End Function

    End Class

End Namespace
