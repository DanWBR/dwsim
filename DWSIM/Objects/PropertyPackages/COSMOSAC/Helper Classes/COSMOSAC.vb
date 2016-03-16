'    COSMO-SAC Property Package 
'    Copyright 2010 Daniel Wagner O. de Medeiros
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
Imports br.ufrgs.enq.jcosmo

Namespace DWSIM.SimulationObjects.PropertyPackages.Auxiliary

    <System.Serializable()> Public Class COSMO_SAC

        Implements Auxiliary.IActivityCoefficientBase

        <System.NonSerialized()> Dim db As COSMOSACDataBase
        Dim casids As Dictionary(Of String, String)

        Sub New()

        End Sub

        Sub LoadDB()
            Try
                db = COSMOSACDataBase.getInstance()
            Catch ex As TypeLoadException
                MsgBox("Error loading the COSMO-SAC database. You need to install .NET Framework 3.5 to use the COSMO-SAC Property Package.")
            Catch ex As Exception
                MsgBox("Error loading COSMO-SAC database. Details:" & vbCrLf & ex.ToString)
            End Try
        End Sub

        Sub LoadCAS()

            casids = New Dictionary(Of String, String)
            Dim casid, name As String

            Dim text() As String = IO.File.ReadAllLines(My.Application.Info.DirectoryPath & IO.Path.DirectorySeparatorChar & "data" & IO.Path.DirectorySeparatorChar & "cosmo.txt")
            Dim i As Integer = 0
            For Each l As String In text
                If i > 0 Then
                    casid = l.Split(vbTab)(3)
                    name = l.Split(vbTab)(2)
                    If Not casids.ContainsKey(casid) Then
                        casids.Add(casid, name)
                    End If
                End If
                i += 1
            Next

        End Sub

        Function GAMMA(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal index As Integer) As Double

            If db Is Nothing Then LoadDB()
            If casids Is Nothing Then LoadCAS()

            Dim i As Integer
            Dim n As Integer = UBound(Vx)

            Dim ccomps(n) As COSMOSACCompound
            Dim cavityVolume(n) As Double
            Dim sigma(n)() As Double
            Dim cname As String
            i = 0
            For Each c As String In Vids
                If c.Contains("-") Then
                    'cas id provided
                    If casids.ContainsKey(c) Then
                        cname = casids(c)
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                Else
                    'db name or nothing
                    If c <> "" Then
                        cname = c
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                End If
                ccomps(i) = db.getComp(cname)
                cavityVolume(i) = ccomps(i).Vcosmo
                sigma(i) = ccomps(i).sigma
                i += 1
            Next

            Dim cosmosac As New COSMOSAC()
            cosmosac.setParameters(cavityVolume, ccomps(0).charge, sigma)
            cosmosac.setTemperature(T) ' // T em Kelvin

            cosmosac.setComposition(Vx)

            Dim lngamma(n), gammav(n) As Double

            cosmosac.activityCoefficient(lnGamma)

            For i = 0 To n
                gammav(i) = Math.Exp(lngamma(i))
            Next

            Return gammav(index)

        End Function

        Function GAMMA_DINF(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal index As Integer) As Double

            If db Is Nothing Then LoadDB()
            If casids Is Nothing Then LoadCAS()

            Dim n As Integer = UBound(Vx)

            Dim Vx2(n) As Double

            Dim i As Integer

            i = 0
            Dim Vxid_ant = Vx(index)
            Do
                If i <> index Then
                    Vx2(n) = Vx(i) + Vxid_ant / (n - 1)
                End If
                Vx2(index) = 1.0E-20
                i = i + 1
            Loop Until i = n + 1

            Dim ccomps(n) As COSMOSACCompound
            Dim cavityVolume(n) As Double
            Dim sigma(n)() As Double
            Dim cname As String
            i = 0
            For Each c As String In Vids
                If c.Contains("-") Then
                    'cas id provided
                    If casids.ContainsKey(c) Then
                        cname = casids(c)
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                Else
                    'db name or nothing
                    If c <> "" Then
                        cname = c
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                End If
                ccomps(i) = db.getComp(cname)
                cavityVolume(i) = ccomps(i).Vcosmo
                sigma(i) = ccomps(i).sigma
                i += 1
            Next

            Dim cosmosac As New COSMOSAC()
            cosmosac.setParameters(cavityVolume, ccomps(0).charge, sigma)
            cosmosac.setTemperature(T) ' // T em Kelvin

            cosmosac.setComposition(Vx2)

            Dim lngamma(n), gammav(n) As Double

            cosmosac.activityCoefficient(lngamma)

            For i = 0 To n
                gammav(i) = Math.Exp(lngamma(i))
            Next

            Return gammav(index)


        End Function

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Array

            If db Is Nothing Then LoadDB()
            If casids Is Nothing Then LoadCAS()

            Dim i As Integer
            Dim n As Integer = UBound(Vx)

            Dim ccomps(n) As COSMOSACCompound
            Dim cavityVolume(n) As Double
            Dim sigma(n)() As Double
            Dim cname As String
            i = 0
            For Each c As String In Vids
                If c.Contains("-") Then
                    'cas id provided
                    If casids.ContainsKey(c) Then
                        cname = casids(c)
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                Else
                    'db name or nothing
                    If c <> "" Then
                        cname = c
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                End If
                ccomps(i) = db.getComp(cname)
                cavityVolume(i) = ccomps(i).Vcosmo
                sigma(i) = ccomps(i).sigma
                i += 1
            Next

            Dim cosmosac As New COSMOSAC()
            cosmosac.setParameters(cavityVolume, ccomps(0).charge, sigma)
            cosmosac.setTemperature(T) ' // T em Kelvin

            cosmosac.setComposition(Vx)

            Dim lngamma(n), gammav(n) As Double

            cosmosac.activityCoefficient(lngamma)

            For i = 0 To n
                gammav(i) = Math.Exp(lngamma(i))
            Next

            Return gammav

        End Function

        Function GAMMA_DINF_MR(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal index As Integer) As Array

            If db Is Nothing Then LoadDB()
            If casids Is Nothing Then LoadCAS()

            Dim n As Integer = UBound(Vx)

            Dim Vx2(n) As Double

            Dim i As Integer

            i = 0
            Dim Vxid_ant = Vx(index)
            Do
                If i <> index Then
                    Vx2(n) = Vx(i) + Vxid_ant / (n - 1)
                End If
                Vx2(index) = 1.0E-20
                i = i + 1
            Loop Until i = n + 1

            Dim ccomps(n) As COSMOSACCompound
            Dim cavityVolume(n) As Double
            Dim sigma(n)() As Double
            Dim cname As String
            i = 0
            For Each c As String In Vids
                If c.Contains("-") Then
                    'cas id provided
                    If casids.ContainsKey(c) Then
                        cname = casids(c)
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                Else
                    'db name or nothing
                    If c <> "" Then
                        cname = c
                    Else
                        Throw New Exception("COSMO-SAC error: no data available for one or more components in the mixture.")
                    End If
                End If
                ccomps(i) = db.getComp(cname)
                cavityVolume(i) = ccomps(i).Vcosmo
                sigma(i) = ccomps(i).sigma
                i += 1
            Next

            Dim cosmosac As New COSMOSAC()
            cosmosac.setParameters(cavityVolume, ccomps(0).charge, sigma)
            cosmosac.setTemperature(T) ' // T em Kelvin

            cosmosac.setComposition(Vx2)

            Dim lngamma(n), gammav(n) As Double

            cosmosac.activityCoefficient(lngamma)

            For i = 0 To n
                gammav(i) = Math.Exp(lngamma(i))
            Next

            Return gammav

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, Vids)
            gamma2 = GAMMA_MR(T + epsilon, Vx, Vids)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, Vids)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, Vids)
            hex2 = HEX_MIX(T + epsilon, Vx, Vids)

            cpex = (hex2 - hex1) / epsilon

            Return cpex 'kJ/kmol.K

        End Function

        Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

            Return GAMMA_MR(T, Vx, otherargs)

        End Function

        Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

            Return HEX_MIX(T, Vx, otherargs)

        End Function

        Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

            Return CPEX_MIX(T, Vx, otherargs)

        End Function


    End Class

End Namespace

