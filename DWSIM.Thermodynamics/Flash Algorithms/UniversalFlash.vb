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
                    If Not FlashSettings(FlashSetting.ImmiscibleWaterOption) = True Then
                        Try
                            Dim nl As New NestedLoops3PV3 With {.FlashSettings = FlashSettings}
                            result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    Else
                        Dim imm As New NestedLoopsImmiscible With {.FlashSettings = FlashSettings}
                        Try
                            result = imm.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    End If
                Case "SVLLE"
                    'SVLLE
                    Try
                        Dim nl As New NestedLoopsSVLLE With {.FlashSettings = FlashSettings}
                        result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                    Catch ex As Exception
                        errflag = True
                    End Try
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
                        'If Settings.ExcelMode Then
                        Try
                            Dim nl As New NestedLoopsSVLLE With {.FlashSettings = FlashSettings}
                            result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                        'Else
                        '    Dim gmin As New GibbsMinimizationMulti With {.FlashSettings = FlashSettings}
                        '    Try
                        '        result = gmin.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        '    Catch ex As Exception
                        '        errflag = True
                        '    End Try
                        'End If
                    ElseIf hres.LiquidPhaseSplit And Not hres.SolidPhase Then
                        'VLLE
                        Try
                            Dim nl As New NestedLoops3PV3 With {.FlashSettings = FlashSettings}
                            result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        Catch ex As Exception
                            errflag = True
                        End Try
                    ElseIf Not hres.LiquidPhaseSplit And hres.SolidPhase Then
                        'SVLE
                        'Try
                        '    Dim nl As New NestedLoopsSLE With {.FlashSettings = FlashSettings, .SolidSolution = False}
                        '    result = nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                        'Catch ex As Exception
                        '    errflag = True
                        'End Try
                        Try
                            Dim nl As New NestedLoopsSVLLE With {.FlashSettings = FlashSettings}
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
                        If errflag Then
                            Dim nl4 As New NestedLoopsSVLLE With {.FlashSettings = FlashSettings}
                            Try
                                result = nl4.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                                errflag = False
                            Catch ex As Exception
                                errflag = True
                            End Try
                        End If
                    End If
            End Select

            If errflag Then
                Throw New Exception("Flash calculation error")
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

            Dim T0 As Double = Tref
            If T0 = 0.0 Then T0 = 298.15

            Dim names = PP.RET_VNAMES

            Dim hres = PerformHeuristicsTest(Vz, T0, P, PP)

            Dim result As Object = Nothing

            Dim T, L1, L2, S, Vy(), Vx1(), Vx2(), Vs(), VxM(), Lm As Double

            If PP.ForcedSolids.Count > 0 Then

                'we have forced solids

                Dim Vzns As Double() = Vz.Clone
                Vs = PP.RET_NullVector
                For Each item In PP.ForcedSolids
                    Dim index = names.ToList.IndexOf(item)
                    Vs(index) = Vz(index)
                    Vzns(index) = 0.0
                Next
                S = Vs.Sum
                Vzns = Vzns.NormalizeY
                Vs = Vs.NormalizeY

                If hres.LiquidPhaseSplit Then
                    Dim nl3 = New NestedLoops3PV3
                    nl3.FlashSettings = FlashSettings
                    result = nl3.Flash_PV(Vzns, P, V, Tref, PP, ReuseKI, PrevKi)
                Else
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PV(Vzns, P, V, Tref, PP, ReuseKI, PrevKi)
                End If

                'Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                L1 = result(0) * (1 - S)
                V = result(1) * (1 - S)
                Vx1 = result(2)
                Vy = result(3)
                T = result(4)
                L2 = result(7) * (1 - S)
                Vx2 = result(8)

                Return New Object() {L1, V, Vx1, Vy, T, result(5), result(6), L2 * (1 - S), Vx2, S, Vs}

            Else

                If hres.LiquidPhaseSplit Then
                    Dim nl3 = New NestedLoops3PV3
                    nl3.FlashSettings = FlashSettings
                    result = nl3.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
                Else
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
                End If

                'Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                L1 = result(0)
                V = result(1)
                Vx1 = result(2)
                Vy = result(3)
                T = result(4)
                L2 = result(7)
                Vx2 = result(8)

                If hres.SolidPhase And L1 > 0 Then

                    Dim nls = New NestedLoopsSLE
                    nls.FlashSettings = FlashSettings

                    Lm = L1 + L2

                    VxM = Vx1.MultiplyConstY(L1).AddY(Vx2.MultiplyConstY(L2)).MultiplyConstY(1 / (L1 + L2))

                    Dim resultS = nls.Flash_SL(VxM, P, T, PP)

                    'Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}

                    S = result(1) * Lm
                    Lm = result(0) * Lm

                    VxM = result(3)
                    Vs = result(4)

                    Dim nll = New SimpleLLE()

                    Dim resultL = nll.Flash_PT(VxM, P, T, PP)

                    'Return New Object() {L1, V, Vx1, PP.RET_NullVector, ecount, L2, Vx2, 0.0#, PP.RET_NullVector, gamma1, gamma2}

                    L1 = result(0) * Lm
                    Vx1 = result(2)
                    L2 = result(7) * Lm
                    Vx2 = result(8)

                    Return New Object() {L1, V, Vx1, Vy, T, result(5), result(6), L2, Vx2, S, Vs}

                Else

                    Return New Object() {L1, V, Vx1, Vy, T, result(5), result(6), L2, Vx2, 0.0, PP.RET_NullVector()}

                End If

            End If

            Return result

        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim P0 As Double = Pref
            If P0 = 0.0 Then P0 = 101325

            Dim names = PP.RET_VNAMES

            Dim hres = PerformHeuristicsTest(Vz, T, P0, PP)

            Dim result As Object = Nothing

            Dim P, L1, L2, S, Vy(), Vx1(), Vx2(), Vs(), VxM(), Lm As Double

            If PP.ForcedSolids.Count > 0 Then

                'we have forced solids

                Dim Vzns As Double() = Vz.Clone
                Vs = PP.RET_NullVector
                For Each item In PP.ForcedSolids
                    Dim index = names.ToList.IndexOf(item)
                    Vs(index) = Vz(index)
                    Vzns(index) = 0.0
                Next
                S = Vs.Sum
                Vzns = Vzns.NormalizeY
                Vs = Vs.NormalizeY

                If hres.LiquidPhaseSplit Then
                    Dim nl3 = New NestedLoops3PV3
                    nl3.FlashSettings = FlashSettings
                    result = nl3.Flash_TV(Vzns, T, V, Pref, PP, ReuseKI, PrevKi)
                Else
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PV(Vzns, T, V, Pref, PP, ReuseKI, PrevKi)
                End If

                'Return New Object() {L, V, Vx, Vy, P, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                L1 = result(0) * (1 - S)
                V = result(1) * (1 - S)
                Vx1 = result(2)
                Vy = result(3)
                P = result(4)
                L2 = result(7) * (1 - S)
                Vx2 = result(8)

                Return New Object() {L1, V, Vx1, Vy, P, result(5), result(6), L2 * (1 - S), Vx2, S, Vs}

            Else

                If hres.LiquidPhaseSplit Then
                    Dim nl3 = New NestedLoops3PV3
                    nl3.FlashSettings = FlashSettings
                    result = nl3.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
                Else
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
                End If

                'Return New Object() {L, V, Vx, Vy, P, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                L1 = result(0)
                V = result(1)
                Vx1 = result(2)
                Vy = result(3)
                P = result(4)
                L2 = result(7)
                Vx2 = result(8)

                If hres.SolidPhase And L1 > 0 Then

                    Dim nls = New NestedLoopsSLE
                    nls.FlashSettings = FlashSettings

                    Lm = L1 + L2

                    VxM = Vx1.MultiplyConstY(L1).AddY(Vx2.MultiplyConstY(L2)).MultiplyConstY(1 / (L1 + L2))

                    Dim resultS = nls.Flash_SL(VxM, P, T, PP)

                    'Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}

                    S = result(1) * Lm
                    Lm = result(0) * Lm

                    VxM = result(3)
                    Vs = result(4)

                    Dim nll = New SimpleLLE()

                    Dim resultL = nll.Flash_PT(VxM, P, T, PP)

                    'Return New Object() {L1, V, Vx1, PP.RET_NullVector, ecount, L2, Vx2, 0.0#, PP.RET_NullVector, gamma1, gamma2}

                    L1 = result(0) * Lm
                    Vx1 = result(2)
                    L2 = result(7) * Lm
                    Vx2 = result(8)

                    Return New Object() {L1, V, Vx1, Vy, P, result(5), result(6), L2, Vx2, S, Vs}

                Else

                    Return New Object() {L1, V, Vx1, Vy, P, result(5), result(6), L2, Vx2, 0.0, PP.RET_NullVector()}

                End If

            End If

            Return result

        End Function

    End Class

End Namespace



