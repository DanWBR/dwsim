'    DWSIM Universal Flash Algorithm
'    Copyright 2020 Daniel Wagner O. de Medeiros
'              2021 Gregor Reichert
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

            Dim Flashtype As String = FlashSettings(FlashSetting.ForceEquilibriumCalculationType)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT", Name & " (PT Flash)", "Pressure-Temperature Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Components: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Flash Settings: {0}", Flashtype))

            If ReuseKI Then
                IObj?.Paragraphs.Add("Reuse Ki's: true")
                IObj?.Paragraphs.Add(String.Format("Previous Ki's: {0}", PrevKi.ToMathArrayString))
            End If

            If Flashtype = "Default" Then
                IObj?.Paragraphs.Add("<hr><b>Perform Phase Heuristics Test to decide on suitable algorithm.</b>")
                Dim hres = PerformHeuristicsTest(Vz, T, P, PP)

                'chech possible phases to decide on suitable flash algorithm
                If PP.ForcedSolids.Count > 0 Then
                    Flashtype = "SVLLE"
                    IObj?.Paragraphs.Add("Heuristics Result: Solid + Liquid Phase Split")
                ElseIf hres.SolidPhase Then
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "SVLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid + Liquid Phase Split")
                    Else
                        Flashtype = "SVLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid")
                    End If
                Else
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "VLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Liquid Phase Split")
                    Else
                        Flashtype = "VLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Neither Solid nor Liquid Phase Split")
                    End If
                End If
            End If

            Dim result As Object = Nothing

            Select Case Flashtype
                Case "VLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLE")
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PT(Vz, P, T, PP, False, Nothing)
                Case "VLLE"
                    If Not FlashSettings(FlashSetting.ImmiscibleWaterOption) = True Then
                        IObj?.Paragraphs.Add("Selected Flash Algorithm: VLLE")
                        Dim nl = New NestedLoops3PV3
                        nl.FlashSettings = FlashSettings
                        result = nl.Flash_PT(Vz, P, T, PP, False, Nothing)
                    Else
                        IObj?.Paragraphs.Add("Selected Flash Algorithm: VLLE - Immiscible Water")
                        Dim imm As New NestedLoopsImmiscible With {.FlashSettings = FlashSettings}
                        result = imm.Flash_PT(Vz, P, T, PP, False, Nothing)
                    End If
                Case "SVLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: SVLE")
                    Dim nl As New NestedLoopsSLE
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PT(Vz, P, T, PP, False, Nothing)
                Case "SVLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: SVLLE")
                    Dim nl As New NestedLoopsSVLLE
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PT(Vz, P, T, PP, False, Nothing)
            End Select

            IObj?.Close()

            Return result

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Flashtype As String = FlashSettings(FlashSetting.ForceEquilibriumCalculationType)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", Name & " (PH Flash)", "Pressure-Enthalpy Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} KJ/Kg", H))
            IObj?.Paragraphs.Add(String.Format("Components: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Flash Settings: {0}", Flashtype))

            If Flashtype = "Default" Then
                IObj?.Paragraphs.Add("<hr><b>Perform Phase Heuristics Test to decide on suitable algorithm.</b>")
                Dim hres = PerformHeuristicsTest(Vz, Tref, P, PP)

                'chech possible phases to decide on suitable flash algorithm
                If PP.ForcedSolids.Count > 0 Then
                    Flashtype = "SVLLE"
                    IObj?.Paragraphs.Add("Heuristics Result: Solid + Liquid Phase Split")
                ElseIf hres.SolidPhase Then
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "SVLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid + Liquid Phase Split")
                    Else
                        Flashtype = "SVLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid")
                    End If
                Else
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "VLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Liquid Phase Split")
                    Else
                        Flashtype = "VLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Neither Solid nor Liquid Phase Split")
                    End If
                End If
            End If

            Dim result As Object = Nothing

            Select Case Flashtype
                Case "VLE", "SVLE", "SVLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLE")
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    nl.PTFlashFunction = AddressOf Flash_PT
                    result = nl.Flash_PH(Vz, P, H, Tref, PP, False, Nothing)
                Case "VLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLLE")
                    Dim nl = New NestedLoops3PV3
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PH(Vz, P, H, Tref, PP, False, Nothing)
            End Select

            IObj?.Close()

            Return result

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Flashtype As String = FlashSettings(FlashSetting.ForceEquilibriumCalculationType)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", Name & " (PS Flash)", "Pressure-Entropy Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Entropy: {0} kJ/[kg.K]", S))
            IObj?.Paragraphs.Add(String.Format("Components: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Flash Settings: {0}", Flashtype))

            If Flashtype = "Default" Then
                IObj?.Paragraphs.Add("<hr><b>Perform Phase Heuristics Test to decide on suitable algorithm.</b>")
                Dim hres = PerformHeuristicsTest(Vz, Tref, P, PP)

                'chech possible phases to decide on suitable flash algorithm
                If hres.SolidPhase Or PP.ForcedSolids.Count > 0 Then
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "SVLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid + Liquid Phase Split")
                    Else
                        Flashtype = "SVLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid")
                    End If
                Else
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "VLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Liquid Phase Split")
                    Else
                        Flashtype = "VLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Neither Solid nor Liquid Phase Split")
                    End If
                End If
            End If

            Dim result As Object = Nothing

            Select Case Flashtype
                Case "VLE", "SVLE", "SVLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLE")
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    nl.PTFlashFunction = AddressOf Flash_PT
                    result = nl.Flash_PS(Vz, P, S, Tref, PP, False, Nothing)
                Case "VLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLLE")
                    Dim nl = New NestedLoops3PV3
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PS(Vz, P, S, Tref, PP, False, Nothing)
            End Select

            IObj?.Close()

            Return result

        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, V As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim Flashtype As String = FlashSettings(FlashSetting.ForceEquilibriumCalculationType)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PV", Name & " (PV Flash)", "Pressure - Vapor Fraction Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Vapor Fraction: {0}", V))
            IObj?.Paragraphs.Add(String.Format("Components: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Flash Settings: {0}", Flashtype))

            If Flashtype = "Default" Then
                IObj?.Paragraphs.Add("<hr><b>Perform Phase Heuristics Test to decide on suitable algorithm.</b>")
                Dim hres = PerformHeuristicsTest(Vz, Tref, P, PP)

                'chech possible phases to decide on suitable flash algorithm
                If hres.SolidPhase Or PP.ForcedSolids.Count > 0 Then
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "SVLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid + Liquid Phase Split")
                    Else
                        Flashtype = "SVLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid")
                    End If
                Else
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "VLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Liquid Phase Split")
                    Else
                        Flashtype = "VLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Neither Solid nor Liquid Phase Split")
                    End If
                End If
            End If

            Dim result As Object = Nothing

            Select Case Flashtype
                Case "VLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLE")
                    Dim nl = New NestedLoops
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
                Case "VLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLLE")
                    Dim nl3 As New NestedLoops3PV3
                    nl3.FlashSettings = FlashSettings
                    result = nl3.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
                Case "SVLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: SVLE")
                    Dim nls As New NestedLoopsSLE
                    nls.FlashSettings = FlashSettings
                    result = nls.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
                Case "SVLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: SVLLE")
                    Dim nlsv As New NestedLoopsSVLLE
                    nlsv.FlashSettings = FlashSettings
                    result = nlsv.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
            End Select

            IObj?.Close()

            Return result

        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim Flashtype As String = FlashSettings(FlashSetting.ForceEquilibriumCalculationType)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "Flash_TV", Name & " (TV Flash)", "Temperature - Vapor Fraction Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Vapor Fraction: {0}", V))
            IObj?.Paragraphs.Add(String.Format("Components: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Flash Settings: {0}", Flashtype))

            If Flashtype = "Default" Then
                Dim hres = PerformHeuristicsTest(Vz, T, Pref, PP)

                'chech possible phases to decide on suitable flash algorithm
                If hres.SolidPhase Or PP.ForcedSolids.Count > 0 Then
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "SVLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid + Liquid Phase Split")
                    Else
                        Flashtype = "SVLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Solid")
                    End If
                Else
                    If hres.LiquidPhaseSplit Then
                        Flashtype = "VLLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Liquid Phase Split")
                    Else
                        Flashtype = "VLE"
                        IObj?.Paragraphs.Add("Heuristics Result: Neither Solid nor Liquid Phase Split")
                    End If
                End If
            End If

            Dim result As Object = Nothing

            Select Case Flashtype
                Case "VLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLE")
                    Dim nl As New NestedLoops
                    nl.FlashSettings = FlashSettings
                    result = nl.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
                Case "VLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: VLLE")
                    Dim nl3 As New NestedLoops3PV3
                    nl3.FlashSettings = FlashSettings
                    result = nl3.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
                Case "SVLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: SVLE")
                    Dim nls As New NestedLoopsSLE
                    nls.FlashSettings = FlashSettings
                    result = nls.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
                Case "SVLLE"
                    IObj?.Paragraphs.Add("Selected Flash Algorithm: SVLLE")
                    Dim nlsv As New NestedLoopsSVLLE
                    nlsv.FlashSettings = FlashSettings
                    result = nlsv.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
            End Select

            IObj?.Close()

            Return result

        End Function

    End Class

End Namespace



