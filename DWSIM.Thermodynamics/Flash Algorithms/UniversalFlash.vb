'    DWSIM Universal Flash Algorithm
'    Copyright 2020 Daniel Wagner O. de Medeiros
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

Imports System.Math
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    Public Class UniversalFlash

        Inherits FlashAlgorithm

        Public Sub New()
            MyBase.New
            Order = 0
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Universal
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "Universal Equilibrium Flash Algorithm"
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Universal Flash"
            End Get
        End Property

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function Flash_PT(Vz() As Double, P As Double, T As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim names = PP.RET_VNAMES

            Dim n = Vz.Length - 1

            Dim errflag As Boolean

            Dim result As Object = Nothing

            Select Case FlashSettings(FlashSetting.ForceEquilibriumCalculationType)
                Case "VLE"
                    'VLE
                    Try
                        Dim nl As New NestedLoops With {.FlashSettings = FlashSettings}
                        result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                    Catch ex As Exception
                        errflag = True
                    End Try
                Case "VLLE"
                    'VLLE
                    If Settings.ExcelMode Then
                        Try
                            Dim nl As New NestedLoops3PV3 With {.FlashSettings = FlashSettings}
                            result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    Else
                        Dim gmin As New GibbsMinimization3P With {.FlashSettings = FlashSettings}
                        Try
                            result = gmin.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    End If
                Case "SVLLE"
                    'SVLLE
                    If Settings.ExcelMode Then
                        Try
                            Dim nl As New NestedLoopsSVLLE With {.FlashSettings = FlashSettings}
                            result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    Else
                        Dim gmin As New GibbsMinimizationMulti With {.FlashSettings = FlashSettings}
                        Try
                            result = gmin.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    End If
                Case "SVLE"
                    'SVLE
                    Try
                        Dim nl As New NestedLoopsSLE With {.FlashSettings = FlashSettings, .SolidSolution = False}
                        result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                    Catch ex As Exception
                        errflag = True
                    End Try
                Case Else
                    Dim hres = PerformHeuristicsTest(Vz, T, P, PP)
                    If hres.LiquidPhaseSplit And hres.SolidPhase Then
                        'SVLLE
                        If Settings.ExcelMode Then
                            Try
                                Dim nl As New NestedLoopsSVLLE With {.FlashSettings = FlashSettings}
                                result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                            Catch ex As Exception
                                errflag = True
                            End Try
                        Else
                            Dim gmin As New GibbsMinimizationMulti With {.FlashSettings = FlashSettings}
                            Try
                                result = gmin.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                            Catch ex As Exception
                                errflag = True
                            End Try
                        End If
                    ElseIf hres.LiquidPhaseSplit And Not hres.SolidPhase Then
                        'VLLE
                        If Settings.ExcelMode Then
                            Try
                                Dim nl As New NestedLoops3PV3 With {.FlashSettings = FlashSettings}
                                result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                            Catch ex As Exception
                                errflag = True
                            End Try
                        Else
                            Dim gmin As New GibbsMinimization3P With {.FlashSettings = FlashSettings}
                            Try
                                result = gmin.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                            Catch ex As Exception
                                errflag = True
                            End Try
                        End If
                    ElseIf Not hres.LiquidPhaseSplit And hres.SolidPhase Then
                        'SVLE
                        Try
                            Dim nl As New NestedLoopsSLE With {.FlashSettings = FlashSettings, .SolidSolution = False}
                            result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    Else
                        'VLE
                        Try
                            Dim nl As New NestedLoops With {.FlashSettings = FlashSettings}
                            result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    End If
            End Select

            If errflag Then
                Dim nl As New NestedLoops With {.FlashSettings = FlashSettings}
                result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                Return result
            Else
                Return result
            End If

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT

            Return nl.Flash_PH(Vz, P, H, Tref, PP, ReuseKI, PrevKi)

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT

            Return nl.Flash_PS(Vz, P, S, Tref, PP, ReuseKI, PrevKi)

        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, V As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim nl = New NestedLoopsSVLLE
            nl.FlashSettings = FlashSettings

            Return nl.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)

        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim nl = New NestedLoopsSVLLE
            nl.FlashSettings = FlashSettings

            Return nl.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)

        End Function

    End Class

End Namespace



