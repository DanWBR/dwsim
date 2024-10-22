'    CAPE-OPEN Property Package Wrapper
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


Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Math
Imports CapeOpen
Imports Microsoft.Win32
Imports System.Linq
Imports System.Runtime.InteropServices
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Interfaces2
Imports DWSIM.Interfaces
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Serializable()> Public Class CAPEOPENPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        <System.NonSerialized()> Public _copp, _pptpl As Object 'CAPE-OPEN Property Package & Manager

        Public _selts As CapeOpenObjInfo
        Public _istrpp, _istrts As ComIStreamWrapper
        Public _ppname As String = ""

        Public _coversion As String = "1.0"

        Private m_props As New PropertyPackages.Auxiliary.PROPS

        Public _mappings As New Dictionary(Of String, String)

#Region "    IDisposable Support "

        Private disposedValue As Boolean = False        ' To detect redundant calls

        ' IDisposable
        Protected Overrides Sub Dispose(ByVal disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                    ' TODO: free other state (managed objects).
                End If

                If Not _copp Is Nothing Then
                    If Marshal.IsComObject(_copp) Then Marshal.ReleaseComObject(_copp)
                End If
                If Not _pptpl Is Nothing Then
                    If Marshal.IsComObject(_pptpl) Then Marshal.ReleaseComObject(_pptpl)
                End If
                ' TODO: set large fields to null.
            End If
            Me.disposedValue = True
        End Sub

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Overrides Sub Dispose()
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub

#End Region

#Region "    DWSIM Methods and Procedures"

        Public Sub New()

            CreatePhaseMappings()

            Me._packagetype = PropertyPackages.PackageType.CAPEOPEN
            Me.IsConfigurable = True

        End Sub

        Public Overrides Sub DisplayEditingForm()

            If Me._phasemappings Is Nothing Then CreatePhaseMappings()

            Dim f As New FormConfigCAPEOPENPPSocket With {.pp = Me, ._form = Flowsheet}
            f.Show()

        End Sub

        Public Overrides Function GetEditingForm() As Form

            Return New FormConfigCAPEOPENPPSocket() With {.pp = Me, ._form = Flowsheet}

        End Function

        Public Overrides Function Clone() As PropertyPackage

            Dim pp = MemberwiseClone()

            pp.FlashSettings = New Dictionary(Of FlashSetting, String)(FlashSettings)
            pp.ForcedSolids = New List(Of String)(ForcedSolids)

            Return pp

        End Function

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)
            'do nothing
        End Sub

        Public Overrides Function DW_CalcBubP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0.0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim res As Object
            res = Me.DW_CalcEquilibrio_ISOL(Vx, FlashSpec.T, FlashSpec.VAP, T, 0, 0)
            Return New Object() {res(0), res(1), res(8), res(9), res(3), 0, res(10)}
        End Function

        Public Overrides Function DW_CalcBubT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0.0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim res As Object
            res = Me.DW_CalcEquilibrio_ISOL(Vx, FlashSpec.P, FlashSpec.VAP, P, 0, 0)
            Return New Object() {res(0), res(1), res(8), res(9), res(2), 0, res(10)}
        End Function

        Public Overrides Function DW_CalcDewP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0.0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim res As Object
            res = Me.DW_CalcEquilibrio_ISOL(Vx, FlashSpec.T, FlashSpec.VAP, T, 1, 0)
            Return New Object() {res(0), res(1), res(8), res(9), res(3), 0, res(10)}
        End Function

        Public Overrides Function DW_CalcDewT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0.0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim res As Object
            res = Me.DW_CalcEquilibrio_ISOL(Vx, FlashSpec.P, FlashSpec.VAP, P, 1, 0)
            Return New Object() {res(0), res(1), res(8), res(9), res(2), 0, res(10)}
        End Function

        Public Overrides Sub DW_CalcEquilibrium(ByVal spec1 As FlashSpec, ByVal spec2 As FlashSpec)

            Me.CurrentMaterialStream.AtEquilibrium = False

            Dim s1 As String() = New String() {}
            Dim s2 As String() = New String() {}
            Dim s11 As String = ""
            Dim s22 As String = ""

            Select Case spec1
                Case FlashSpec.T
                    s1 = New String() {"temperature", Nothing, "Overall"}
                    s11 = "T"
                Case FlashSpec.P
                    s1 = New String() {"pressure", Nothing, "Overall"}
                    s11 = "P"
            End Select

            Select Case spec2
                Case FlashSpec.T
                    s2 = New String() {"temperature", Nothing, "Overall"}
                    s22 = "T"
                Case FlashSpec.P
                    s2 = New String() {"pressure", Nothing, "Overall"}
                    s22 = "P"
                Case FlashSpec.S
                    s2 = New String() {"entropy", Nothing, "Overall"}
                    s22 = "S"
                Case FlashSpec.H
                    s2 = New String() {"enthalpy", Nothing, "Overall"}
                    s22 = "H"
                Case FlashSpec.VAP
                    s2 = New String() {"phaseFraction", "Mole", "Vapor"}
                    s22 = "VF"
            End Select

            Me.DW_ZerarPhaseProps(Phase.Vapor)
            Me.DW_ZerarPhaseProps(Phase.Liquid)
            Me.DW_ZerarPhaseProps(Phase.Liquid1)
            Me.DW_ZerarPhaseProps(Phase.Liquid2)
            Me.DW_ZerarPhaseProps(Phase.Liquid3)
            Me.DW_ZerarPhaseProps(Phase.Aqueous)
            Me.DW_ZerarPhaseProps(Phase.Solid)
            Me.DW_ZerarComposicoes(Phase.Liquid)
            Me.DW_ZerarComposicoes(Phase.Liquid1)
            Me.DW_ZerarComposicoes(Phase.Liquid2)
            Me.DW_ZerarComposicoes(Phase.Liquid3)
            Me.DW_ZerarComposicoes(Phase.Aqueous)
            Me.DW_ZerarComposicoes(Phase.Vapor)
            Me.DW_ZerarComposicoes(Phase.Solid)

            If _coversion = "1.0" Then
                Try
                    Me.CalcEquilibrium(Me.CurrentMaterialStream, s11 + s22, Nothing)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
            Else
                Try
                    Me.SetMaterial(Me.CurrentMaterialStream)
                    Me.CalcEquilibrium1(s1, s2, "Unspecified")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
            End If

            Me.CurrentMaterialStream.AtEquilibrium = True

            Dim summf As Double = 0.0#, sumwf As Double = 0.0#
            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If Not pi.PhaseLabel = "Disabled" Then
                    summf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.GetValueOrDefault
                    sumwf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction.GetValueOrDefault
                End If
            Next
            If Abs(summf - 1) > 0.000001 Then
                For Each pi As PhaseInfo In Me.PhaseMappings.Values
                    If Not pi.PhaseLabel = "Disabled" Then
                        If Not Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.HasValue Then
                            Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction = 1 - summf
                            Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction = 1 - sumwf
                        End If
                    End If
                Next
            End If

            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If Not pi.PhaseLabel = "Disabled" Then
                    Dim subst As Interfaces.ICompound
                    Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molecularWeight = Me.AUX_MMM(pi.DWPhaseID)
                    For Each subst In Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Compounds.Values
                        subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, pi.DWPhaseIndex)
                    Next
                End If
            Next


        End Sub

        Public Overrides Function CalculateEquilibrium(calctype As Enums.FlashCalculationType, val1 As Double, val2 As Double, mixmolefrac() As Double, initialKval() As Double, initialestimate As Double) As IFlashCalculationResult
            Select Case calctype
                Case Interfaces.Enums.FlashCalculationType.PressureTemperature
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.T, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureEnthalpy
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.H, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureEntropy
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.S, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureSolidFraction
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.SF, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureVaporFraction
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.VAP, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureEnthalpy
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.H, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureEntropy
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.S, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureSolidFraction
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.SF, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureVaporFraction
                    Return CalculateEquilibrium_Override(FlashSpec.P, FlashSpec.VAP, val1, val2, mixmolefrac, initialKval, initialestimate)
                Case Else
                    Throw New NotImplementedException
            End Select
        End Function

        Public Function CalculateEquilibrium_Override(spec1 As FlashSpec, spec2 As FlashSpec,
                                           val1 As Double, val2 As Double,
                                           mixmolefrac As Double(),
                                           initialKval As Double(),
                                           initialestimate As Double) As FlashCalculationResult

            Dim pstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream
            Dim tstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream.Clone

            Dim constprops As List(Of Interfaces.ICompoundConstantProperties) = DW_GetConstantProperties()

            Dim calcresult As New FlashCalculationResult(constprops)

            With calcresult
                .MixtureMoleAmounts = New List(Of Double)(mixmolefrac)
                .FlashAlgorithmType = Me.GetType.ToString
                .FlashSpecification1 = spec1
                .FlashSpecification2 = spec2
            End With

            Me.CurrentMaterialStream = tstr

            Me.CurrentMaterialStream.AtEquilibrium = False

            Me.DW_ZerarPhaseProps(Phase.Vapor)
            Me.DW_ZerarPhaseProps(Phase.Liquid)
            Me.DW_ZerarPhaseProps(Phase.Liquid1)
            Me.DW_ZerarPhaseProps(Phase.Liquid2)
            Me.DW_ZerarPhaseProps(Phase.Liquid3)
            Me.DW_ZerarPhaseProps(Phase.Aqueous)
            Me.DW_ZerarPhaseProps(Phase.Solid)

            Dim s1 As String() = New String() {}
            Dim s2 As String() = New String() {}
            Dim s11 As String = ""
            Dim s22 As String = ""

            Select Case spec1
                Case FlashSpec.T
                    s1 = New String() {"temperature", Nothing, "Overall"}
                    s11 = "T"
                    Me.CurrentMaterialStream.Phases(0).Properties.temperature = val1
                Case FlashSpec.P
                    s1 = New String() {"pressure", Nothing, "Overall"}
                    s11 = "P"
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = val1
            End Select

            Select Case spec2
                Case FlashSpec.T
                    s2 = New String() {"temperature", Nothing, "Overall"}
                    s22 = "T"
                    Me.CurrentMaterialStream.Phases(0).Properties.temperature = val2
                Case FlashSpec.P
                    s2 = New String() {"pressure", Nothing, "Overall"}
                    s22 = "P"
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = val2
                Case FlashSpec.S
                    s2 = New String() {"entropy", Nothing, "Overall"}
                    s22 = "S"
                    Me.CurrentMaterialStream.Phases(0).Properties.entropy = val2
                Case FlashSpec.H
                    s2 = New String() {"enthalpy", Nothing, "Overall"}
                    s22 = "H"
                    Me.CurrentMaterialStream.Phases(0).Properties.enthalpy = val2
                Case FlashSpec.VAP
                    s2 = New String() {"phaseFraction", "Mole", "Vapor"}
                    s22 = "VF"
                    Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = val2
            End Select

            Me.CurrentMaterialStream.SetOverallComposition(mixmolefrac)

            If _coversion = "1.0" Then
                Try
                    Me.CalcEquilibrium(Me.CurrentMaterialStream, s11 + s22, Nothing)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
            Else
                Try
                    Me.SetMaterial(Me.CurrentMaterialStream)
                    Me.CalcEquilibrium1(s1, s2, "Unspecified")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
            End If

            Me.CurrentMaterialStream.AtEquilibrium = True

            Dim summf As Double = 0.0#, sumwf As Double = 0.0#
            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If Not pi.PhaseLabel = "Disabled" Then
                    summf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.GetValueOrDefault
                    sumwf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction.GetValueOrDefault
                End If
            Next
            If Abs(summf - 1) > 0.000001 Then
                For Each pi As PhaseInfo In Me.PhaseMappings.Values
                    If Not pi.PhaseLabel = "Disabled" And Not Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.HasValue Then
                        Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction = 1 - summf
                        Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction = 1 - sumwf
                    End If
                Next
            End If

            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If Not pi.PhaseLabel = "Disabled" Then
                    Dim mw = Me.AUX_MMM(pi.DWPhaseID)
                    Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molecularWeight = mw
                    If mw > 0.0 Then DW_CalcPhaseProps(pi.DWPhaseID)
                End If
            Next

            DW_CalcPhaseProps(Phase.Liquid)
            DW_CalcPhaseProps(Phase.Mixture)

            With calcresult
                .BaseMoleAmount = 1.0
                .VaporPhaseMoleAmounts = Me.RET_VMOL(Phase.Vapor).ToList
                .LiquidPhase1MoleAmounts = Me.RET_VMOL(Phase.Liquid1).ToList
                .LiquidPhase2MoleAmounts = Me.RET_VMOL(Phase.Liquid2).ToList
                .SolidPhaseMoleAmounts = Me.RET_VMOL(Phase.Solid).ToList
                .CalculatedTemperature = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                .CalculatedPressure = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
                .CalculatedEnthalpy = Me.CurrentMaterialStream.Phases(0).Properties.enthalpy.GetValueOrDefault
                .CalculatedEntropy = Me.CurrentMaterialStream.Phases(0).Properties.entropy.GetValueOrDefault
                .Kvalues = New List(Of Double)(.VaporPhaseMoleAmounts.ToArray.DivideY(.LiquidPhase1MoleAmounts.ToArray))
            End With

            Me.CurrentMaterialStream = pstr

            tstr = Nothing

            Return calcresult

        End Function

        Public Overloads Function DW_CalcEquilibrio_ISOL(ByVal Vz As Array, ByVal spec1 As FlashSpec, ByVal spec2 As FlashSpec, ByVal val1 As Double, ByVal val2 As Double, ByVal estimate As Double) As Object

            Dim pstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream
            Dim tstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream.Clone

            Me.CurrentMaterialStream = tstr

            Me.CurrentMaterialStream.AtEquilibrium = False

            Dim s1 As String() = New String() {}
            Dim s2 As String() = New String() {}
            Dim s11 As String = ""
            Dim s22 As String = ""

            Select Case spec1
                Case FlashSpec.T
                    s1 = New String() {"temperature", Nothing, "Overall"}
                    s11 = "T"
                    Me.CurrentMaterialStream.Phases(0).Properties.temperature = val1
                Case FlashSpec.P
                    s1 = New String() {"pressure", Nothing, "Overall"}
                    s11 = "P"
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = val1
            End Select

            Select Case spec2
                Case FlashSpec.T
                    s2 = New String() {"temperature", Nothing, "Overall"}
                    s22 = "T"
                    Me.CurrentMaterialStream.Phases(0).Properties.temperature = val2
                Case FlashSpec.P
                    s2 = New String() {"pressure", Nothing, "Overall"}
                    s22 = "P"
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = val2
                Case FlashSpec.S
                    s2 = New String() {"entropy", Nothing, "Overall"}
                    s22 = "S"
                    Me.CurrentMaterialStream.Phases(0).Properties.entropy = val2
                Case FlashSpec.H
                    s2 = New String() {"enthalpy", Nothing, "Overall"}
                    s22 = "H"
                    Me.CurrentMaterialStream.Phases(0).Properties.enthalpy = val2
                Case FlashSpec.VAP
                    s2 = New String() {"phaseFraction", "Mole", "Vapor"}
                    s22 = "VF"
                    Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = val2
            End Select

            Dim i As Integer = 0
            For Each c As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                c.MoleFraction = Vz(i)
                i += 1
            Next

            Me.DW_ZerarPhaseProps(Phase.Vapor)
            Me.DW_ZerarPhaseProps(Phase.Liquid)
            Me.DW_ZerarPhaseProps(Phase.Liquid1)
            Me.DW_ZerarPhaseProps(Phase.Liquid2)
            Me.DW_ZerarPhaseProps(Phase.Liquid3)
            Me.DW_ZerarPhaseProps(Phase.Aqueous)
            Me.DW_ZerarPhaseProps(Phase.Solid)

            If _coversion = "1.0" Then
                Try
                    CType(_copp, ICapeThermoPropertyPackage).CalcEquilibrium(Me.CurrentMaterialStream, s11 + s22, Nothing)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
            Else
                Try
                    CType(_copp, ICapeThermoMaterialContext).SetMaterial(Me.CurrentMaterialStream)
                    Dim ok As Boolean = CType(_copp, ICapeThermoEquilibriumRoutine).CheckEquilibriumSpec(s1, s2, "Unspecified")
                    CType(_copp, ICapeThermoEquilibriumRoutine).CalcEquilibrium(s1, s2, "Unspecified")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
            End If

            Me.CurrentMaterialStream.AtEquilibrium = True

            Dim summf As Double = 0, sumwf As Double = 0
            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If Not pi.PhaseLabel = "Disabled" Then
                    summf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.GetValueOrDefault
                    sumwf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction.GetValueOrDefault
                End If
            Next
            If Abs(summf - 1) > 0.000001 Then
                For Each pi As PhaseInfo In Me.PhaseMappings.Values
                    If Not pi.PhaseLabel = "Disabled" And Not Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.HasValue Then
                        Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction = 1 - summf
                        Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction = 1 - sumwf
                    End If
                Next
            End If

            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If Not pi.PhaseLabel = "Disabled" Then
                    Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molecularWeight = Me.AUX_MMM(pi.DWPhaseID)
                    DW_CalcPhaseProps(pi.DWPhaseID)
                End If
            Next

            DW_CalcPhaseProps(Phase.Liquid)
            DW_CalcPhaseProps(Phase.Mixture)

            Dim T, P, H, S, xl, xv As Double
            Dim Ki(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1), Vx(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1), Vy(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                Vx(i) = su.MoleFraction.GetValueOrDefault
                i += 1
            Next
            i = 0
            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                Vy(i) = su.MoleFraction.GetValueOrDefault
                i += 1
            Next
            i = 0
            For i = 0 To Vx.Length - 1
                Ki(i) = Vy(i) / Vx(i)
            Next

            xl = Me.CurrentMaterialStream.Phases(1).Properties.molarfraction.GetValueOrDefault
            xv = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            H = Me.CurrentMaterialStream.Phases(0).Properties.enthalpy.GetValueOrDefault
            S = Me.CurrentMaterialStream.Phases(0).Properties.entropy.GetValueOrDefault

            Me.CurrentMaterialStream = pstr
            tstr = Nothing

            Return New Object() {xl, xv, T, P, H, S, 1, 1, Vx, Vy, Ki}

        End Function

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim res As Double = 0.0#, myphase As String = "", tant As Double, pant As Double

            Select Case Phase1
                Case Phase.Vapor
                    myphase = "Vapor"
                Case Phase.Liquid
                    myphase = "Liquid"
            End Select

            tant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            pant = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                CType(_copp, ICapeThermoCalculationRoutine).CalcProp(Me.CurrentMaterialStream, New String() {"heatCapacity"}, New String() {myphase}, "Mixture")
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.heatCapacityCp.GetValueOrDefault
            Else
                CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"heatCapacity"}, myphase)
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.heatCapacityCp.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = tant
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = pant

        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            'do nothing

        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim res As Double = 0.0#, myphase As String = "", tant As Double, pant As Double

            Select Case Phase1
                Case Phase.Vapor
                    myphase = "Vapor"
                Case Phase.Liquid
                    myphase = "Liquid"
            End Select

            tant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            pant = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                CType(_copp, ICapeThermoCalculationRoutine).CalcProp(Me.CurrentMaterialStream, New String() {"thermalConductivity"}, New String() {myphase}, "Mixture")
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.thermalConductivity.GetValueOrDefault
            Else
                CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"thermalConductivity"}, myphase)
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.thermalConductivity.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = tant
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = pant

        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double, Optional ByVal pvp As Double = 0) As Double

            Dim res As Double = 0.0#, myphase As String = "", tant As Double, pant As Double

            Select Case Phase1
                Case Phase.Vapor
                    myphase = "Vapor"
                Case Phase.Liquid
                    myphase = "Liquid"
            End Select

            tant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            pant = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                CType(_copp, ICapeThermoCalculationRoutine).CalcProp(Me.CurrentMaterialStream, New String() {"density"}, New String() {myphase}, "Mixture")
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.density.GetValueOrDefault
            Else
                CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"density"}, myphase)
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.density.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = tant
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = pant

        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_MMM(Phase1)
        End Function

        Public Overrides Sub DW_CalcOverallProps()
            MyBase.DW_CalcOverallProps()
        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal myphase As PropertyPackages.Phase)

            Dim phase As String = ""
            Dim result As Double
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing
            Dim i As Integer
            Dim phaseID As Integer

            Select Case myphase
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
            End Select

            If phaseID > 0 Then
                overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
                result = result * Me.AUX_MMM(myphase) / 1000
                If Double.IsNaN(result) Then result = 0.0
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(myphase) / 1000 / Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault
                If Double.IsNaN(result) Then result = 0.0
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
                Me.DW_CalcCompVolFlow(phaseID)
            End If

            Select Case myphase
                Case PropertyPackages.Phase.Mixture
                    phase = "Overall"
                Case Else
                    For Each pin As PhaseInfo In Me.PhaseMappings.Values
                        If pin.DWPhaseID = myphase Then
                            phase = pin.PhaseLabel
                            Exit For
                        End If
                    Next
            End Select
            Dim proplist As String()

            If _coversion = "1.0" Then
                If myphase <> PropertyPackages.Phase.Liquid Then
                    proplist = Me.GetPropList
                    For i = 0 To UBound(proplist) - 1
                        If Not proplist(i).ToLower.Contains(".d") Then
                            Try
                                Me.CalcProp(Me.CurrentMaterialStream, New String() {proplist(i)}, New String() {myphase}, "Mixture")
                            Catch ex As Exception
                            End Try
                        End If
                    Next
                End If
            Else
                If phase <> "Overall" And phase <> "" Then
                    proplist = Me.GetSinglePhasePropList
                    For i = 0 To UBound(proplist) - 1
                        If Not proplist(i).Contains(".D") Then
                            Try
                                Me.CalcSinglePhaseProp(New String() {proplist(i)}, phase)
                            Catch ex As Exception
                            End Try
                        End If
                    Next
                    result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(myphase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                    If Double.IsNaN(result) Then result = 0.0
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
                End If
            End If

            If phase = "Overall" Then

                Me.DW_CalcOverallProps()

            ElseIf phase = "Liquid" Then

                Me.DW_CalcLiqMixtureProps()

            End If

        End Sub

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Auxiliary.PROPS.Pvp_leekesler(T, Me.RET_VTC(Phase.Liquid), Me.RET_VPC(Phase.Liquid), Me.RET_VW(Phase.Liquid))
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim res As Double = 0.0#, phase As String = "", tant As Double, pant As Double

            tant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            pant = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                CType(_copp, ICapeThermoCalculationRoutine).CalcProp(Me.CurrentMaterialStream, New String() {"surfaceTension"}, New String() {phase}, "Mixture")
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.surfaceTension.GetValueOrDefault
            Else
                CType(_copp, ICapeThermoPropertyRoutine).CalcTwoPhaseProp(New String() {"surfaceTension"}, New String() {"VaporLiquid"})
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.surfaceTension.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = tant
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = pant

        End Function

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal Phase1 As PropertyPackages.Phase, ByVal Phase2 As PropertyPackages.Phase)

        End Sub

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim res As Double = 0.0#, myphase As String = "", tant As Double, pant As Double

            Select Case Phase1
                Case Phase.Vapor
                    myphase = Me.PhaseMappings("Vapor").PhaseLabel
                Case Phase.Liquid
                    myphase = Me.PhaseMappings("Liquid1").PhaseLabel
            End Select

            tant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            pant = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                CType(_copp, ICapeThermoCalculationRoutine).CalcProp(Me.CurrentMaterialStream, New String() {"viscosity"}, New String() {myphase}, "Mixture")
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.viscosity.GetValueOrDefault
            Else
                CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"viscosity"}, myphase)
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.viscosity.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = tant
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = pant

        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim res As Double = 0.0#, phase As String = "", pid As Integer = 0
            Dim pstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream
            Dim tstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream.Clone

            Me.CurrentMaterialStream = tstr

            Select Case st
                Case State.Vapor
                    phase = Me.PhaseMappings("Vapor").PhaseLabel
                    pid = Me.PhaseMappings("Vapor").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Vapor").DWPhaseID)
                Case State.Liquid
                    phase = Me.PhaseMappings("Liquid1").PhaseLabel
                    pid = Me.PhaseMappings("Liquid1").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Liquid1").DWPhaseID)
            End Select

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                Try
                    CType(_copp, ICapeThermoPropertyPackage).CalcProp(Me.CurrentMaterialStream, New String() {"enthalpy"}, New String() {phase}, "Mixture")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.enthalpy.GetValueOrDefault
            Else
                Try
                    CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"enthalpy"}, phase)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.enthalpy.GetValueOrDefault
            End If

            Me.CurrentMaterialStream = pstr
            tstr = Nothing

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim res As Double = 0.0#, phase As String = "", pid As Integer = 0
            Dim pstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream
            Dim tstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream.Clone

            Me.CurrentMaterialStream = tstr

            Select Case st
                Case State.Vapor
                    phase = Me.PhaseMappings("Vapor").PhaseLabel
                    pid = Me.PhaseMappings("Vapor").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Vapor").DWPhaseID)
                Case State.Liquid
                    phase = Me.PhaseMappings("Liquid1").PhaseLabel
                    pid = Me.PhaseMappings("Liquid1").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Liquid1").DWPhaseID)
            End Select

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                Try
                    CType(_copp, ICapeThermoPropertyPackage).CalcProp(Me.CurrentMaterialStream, New String() {"excessEnthalpy"}, New String() {phase}, "Mixture")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.excessEnthalpy.GetValueOrDefault
            Else
                Try
                    CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"excessEnthalpy"}, phase)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.excessEnthalpy.GetValueOrDefault
            End If

            Me.CurrentMaterialStream = pstr
            tstr = Nothing

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim res As Double = 0.0#, phase As String = "", pid As Integer = 0
            Dim pstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream
            Dim tstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream.Clone

            Me.CurrentMaterialStream = tstr

            Select Case st
                Case State.Vapor
                    phase = Me.PhaseMappings("Vapor").PhaseLabel
                    pid = Me.PhaseMappings("Vapor").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Vapor").DWPhaseID)
                Case State.Liquid
                    phase = Me.PhaseMappings("Liquid1").PhaseLabel
                    pid = Me.PhaseMappings("Liquid1").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Liquid1").DWPhaseID)
            End Select

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P
            If _coversion = "1.0" Then
                Try
                    CType(_copp, ICapeThermoPropertyPackage).CalcProp(Me.CurrentMaterialStream, New String() {"entropy"}, New String() {phase}, "Mixture")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.entropy.GetValueOrDefault
            Else
                Try
                    CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"entropy"}, phase)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.entropy.GetValueOrDefault
            End If

            Me.CurrentMaterialStream = pstr
            tstr = Nothing

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim res As Double = 0.0#, phase As String = "", pid As Integer = 0
            Dim pstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream
            Dim tstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream.Clone

            Me.CurrentMaterialStream = tstr

            Select Case st
                Case State.Vapor
                    phase = Me.PhaseMappings("Vapor").PhaseLabel
                    pid = Me.PhaseMappings("Vapor").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Vapor").DWPhaseID)
                Case State.Liquid
                    phase = Me.PhaseMappings("Liquid1").PhaseLabel
                    pid = Me.PhaseMappings("Liquid1").DWPhaseIndex
                    Me.CurrentMaterialStream.SetOverallComposition(Vx)
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Liquid1").DWPhaseID)
            End Select

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                Try
                    CType(_copp, ICapeThermoPropertyPackage).CalcProp(Me.CurrentMaterialStream, New String() {"excessEntropy"}, New String() {phase}, "Mixture")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.excessEntropy.GetValueOrDefault
            Else
                Try
                    CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"excessEntropy"}, phase)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Return Me.CurrentMaterialStream.Phases(pid).Properties.excessEntropy.GetValueOrDefault
            End If

            Me.CurrentMaterialStream = pstr
            tstr = Nothing

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim res As Double = 0.0#, myphase As String = "", tant As Double, pant As Double

            Select Case Phase1
                Case Phase.Vapor
                    myphase = "Vapor"
                Case Phase.Liquid
                    myphase = "Liquid"
            End Select

            tant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            pant = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                CType(_copp, ICapeThermoCalculationRoutine).CalcProp(Me.CurrentMaterialStream, New String() {"heatCapacityCv"}, New String() {myphase}, "Mixture")
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.heatCapacityCp.GetValueOrDefault
            Else
                CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(New String() {"heatCapacityCv"}, myphase)
                Return Me.CurrentMaterialStream.Phases(Phase1).Properties.heatCapacityCp.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = tant
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = pant

        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)



        End Sub

        Public Overrides Function AUX_MMM(Vz() As Double, Optional ByVal state As String = "") As Double

            Dim complist As Object = Nothing
            Dim mw As Object = Nothing
            Dim mw2 As New Dictionary(Of String, Double)
            Me.GetCompoundList(complist, Nothing, Nothing, Nothing, mw, Nothing)
            Dim val As Double = 0.0#
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0
            For i = 0 To Vz.Length - 1
                mw2.Add(complist(i), mw(i))
            Next
            i = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vz(i) * mw2(_mappings(subst.Name))
                i += 1
            Next

            Return val

        End Function

        Public Overrides Function AUX_MMM(Phase As Phase) As Double

            Dim complist As Object = Nothing
            Dim mw As Object = Nothing
            Dim mw2 As New Dictionary(Of String, Double)
            Me.GetCompoundList(complist, Nothing, Nothing, Nothing, mw, Nothing)

            Dim mwt As Double = 0.0#
            Dim i As Integer = 0
            For i = 0 To complist.Length - 1
                mw2.Add(complist(i), mw(i))
            Next
            For Each c As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                mwt += c.MoleFraction.GetValueOrDefault * mw2(_mappings(c.Name))
                i += 1
            Next

            Return mwt

        End Function

        Public Overrides Function AUX_CONVERT_MOL_TO_MASS(ByVal subst As String, ByVal phasenumber As Integer) As Double

            Dim complist As Object = Nothing
            Dim mw As Object = Nothing
            Me.GetCompoundList(complist, Nothing, Nothing, Nothing, mw, Nothing)

            Dim mol_x_mm As Double
            Dim sub1 As Interfaces.ICompound
            Dim i As Integer = 0
            Dim j As Integer = 0
            For Each sub1 In Me.CurrentMaterialStream.Phases(phasenumber).Compounds.Values
                mol_x_mm += sub1.MoleFraction.GetValueOrDefault * mw(i)
                If subst = sub1.Name Then j = i
                i += 1
            Next

            sub1 = Me.CurrentMaterialStream.Phases(phasenumber).Compounds(subst)
            If mol_x_mm <> 0.0# Then
                Return sub1.MoleFraction.GetValueOrDefault * mw(j) / mol_x_mm
            Else
                Return 0.0#
            End If

        End Function

        Public Overrides Function AUX_CONVERT_MASS_TO_MOL(ByVal subst As String, ByVal phasenumber As Integer) As Double

            Dim complist As Object = Nothing
            Dim mw As Object = Nothing
            Me.GetCompoundList(complist, Nothing, Nothing, Nothing, mw, Nothing)

            Dim mass_div_mm As Double
            Dim sub1 As Interfaces.ICompound
            Dim i As Integer = 0
            Dim j As Integer = 0
            For Each sub1 In Me.CurrentMaterialStream.Phases(phasenumber).Compounds.Values
                mass_div_mm += sub1.MassFraction.GetValueOrDefault / mw(i)
                If subst = sub1.Name Then j = i
                i += 1
            Next

            sub1 = Me.CurrentMaterialStream.Phases(phasenumber).Compounds(subst)
            Return sub1.MassFraction.GetValueOrDefault / mw(j) / mass_div_mm

        End Function

        Public Overrides Function AUX_CONVERT_MOL_TO_MASS(ByVal Vz As Double()) As Double()

            Dim complist As Object = Nothing
            Dim mw As Object = Nothing
            Me.GetCompoundList(complist, Nothing, Nothing, Nothing, mw, Nothing)

            Dim Vwe(Vz.Length - 1) As Double
            Dim mol_x_mm As Double = 0.0#
            Dim i As Integer = 0
            For i = 0 To Vz.Length - 1
                mol_x_mm += Vz(i) * mw(i)
            Next

            For i = 0 To Vz.Length - 1
                If mol_x_mm <> 0 Then
                    Vwe(i) = Vz(i) * mw(i) / mol_x_mm
                Else
                    Vwe(i) = 0.0#
                End If
            Next

            Return Vwe

        End Function

        Public Overrides Function AUX_CONVERT_MASS_TO_MOL(ByVal Vz As Double()) As Double()

            Dim complist As Object = Nothing
            Me.GetCompoundList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
            Dim mw = DirectCast(Me.CurrentMaterialStream, ICapeThermoCompounds).GetCompoundConstant(New String() {"molecularWeight"}, complist)

            Dim Vw(Vz.Length - 1) As Double
            Dim mass_div_mm As Double
            Dim i As Integer = 0
            For i = 0 To Vz.Length - 1
                mass_div_mm += Vz(i) / mw(i)
            Next

            For i = 0 To Vz.Length - 1
                Vw(i) = Vz(i) / mw(i) / mass_div_mm
            Next

            Return Vw

        End Function

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim res As Double = 0.0#, phase As String = "Vapor", tant As Double, pant As Double

            tant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            pant = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            If _coversion = "1.0" Then
                Try
                    Me.CalcProp(Me.CurrentMaterialStream, New String() {"density"}, New String() {"Vapor"}, "Mixture")
                Catch ex As Exception
                    Me.CalcProp(Me.CurrentMaterialStream, New String() {"volume"}, New String() {"Vapor"}, "Mixture")
                End Try
                Return Me.CurrentMaterialStream.Phases(2).Properties.density.GetValueOrDefault
            Else
                If CType(_copp, ICapeThermoPropertyRoutine).CheckSinglePhasePropSpec("density", "Vapor") Then
                    Me.CalcSinglePhaseProp(New String() {"density"}, "Vapor")
                Else
                    Me.CalcSinglePhaseProp(New String() {"volume"}, "Vapor")
                End If
                Return Me.CurrentMaterialStream.Phases(2).Properties.density.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = tant
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = pant

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Dim res As Double = 0.0#, phase As String = "", pid As Integer = 0
            Dim pstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream
            Dim tstr As Interfaces.IMaterialStream = Me.CurrentMaterialStream.Clone

            Me.CurrentMaterialStream = tstr

            Select Case st
                Case State.Vapor
                    phase = Me.PhaseMappings("Vapor").PhaseLabel
                    pid = Me.PhaseMappings("Vapor").DWPhaseIndex
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Vapor").DWPhaseID)
                Case State.Liquid
                    phase = Me.PhaseMappings("Liquid1").PhaseLabel
                    pid = Me.PhaseMappings("Liquid1").DWPhaseIndex
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, Me.PhaseMappings("Liquid1").DWPhaseID)
            End Select

            Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
            Me.CurrentMaterialStream.Phases(0).Properties.pressure = P

            Dim lnphi As Object = Nothing
            Dim lnphidt As Object = Nothing
            Dim lnphidp As Object = Nothing
            Dim lnphidn As Object = Nothing

            If _coversion = "1.0" Then
                Try
                    CType(_copp, ICapeThermoPropertyPackage).CalcProp(Me.CurrentMaterialStream, New String() {"fugacityCoefficient"}, New String() {phase}, "Mixture")
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Dim n As Integer = Me.CurrentMaterialStream.Phases(pid).Compounds.Count - 1
                Dim i As Integer = 0
                Dim fugcoeff(n) As Double
                For Each c As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(pid).Compounds.Values
                    fugcoeff(i) = c.FugacityCoeff.GetValueOrDefault
                    i += 1
                Next
                Return fugcoeff
            Else
                Try
                    Me.CalcAndGetLnPhi(phase, T, P, Vx, 1, lnphi, lnphidt, lnphidp, lnphidn)
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = _copp
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
                Dim n As Integer = UBound(lnphi)
                Dim i As Integer
                Dim fugcoeff(n) As Double
                For i = 0 To n
                    fugcoeff(i) = Exp(lnphi(i))
                Next
                Return fugcoeff
            End If

            Me.CurrentMaterialStream = pstr
            tstr = Nothing

        End Function

#End Region

#Region "    CAPE-OPEN 1.0 Methods and Properties"

        Public Overrides Sub CalcEquilibrium(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object)
            CType(_copp, ICapeThermoPropertyPackage).CalcEquilibrium(materialObject, flashType, props)
        End Sub

        Public Overrides Sub CalcProp(ByVal materialObject As Object, ByVal props As Object, ByVal phases As Object, ByVal calcType As String)
            CType(_copp, ICapeThermoPropertyPackage).CalcProp(materialObject, props, phases, calcType)
        End Sub

        Public Overrides Function GetComponentConstant(ByVal materialObject As Object, ByVal props As Object) As Object
            Return CType(_copp, ICapeThermoPropertyPackage).GetComponentConstant(materialObject, props)
        End Function

        Public Overrides Sub GetComponentList(ByRef compIds As Object, ByRef formulae As Object, ByRef names As Object, ByRef boilTemps As Object, ByRef molWt As Object, ByRef casNo As Object)
            CType(_copp, ICapeThermoPropertyPackage).GetComponentList(compIds, formulae, names, boilTemps, molWt, casNo)
        End Sub

        Public Overrides Function GetPhaseList() As Object
            Return CType(_copp, ICapeThermoPropertyPackage).GetPhaseList()
        End Function

        Public Overrides Function GetPropList() As Object
            Return CType(_copp, ICapeThermoPropertyPackage).GetPropList()
        End Function

        Public Overrides Function GetUniversalConstant(ByVal materialObject As Object, ByVal props As Object) As Object
            Return CType(_copp, ICapeThermoPropertyPackage).GetUniversalConstant(materialObject, props)
        End Function

        Public Overrides Function PropCheck(ByVal materialObject As Object, ByVal props As Object) As Object
            Return CType(_copp, ICapeThermoPropertyPackage).PropCheck(materialObject, props)
        End Function

        Public Overrides Function ValidityCheck(ByVal materialObject As Object, ByVal props As Object) As Object
            Return CType(_copp, ICapeThermoPropertyPackage).ValidityCheck(materialObject, props)
        End Function

        Public Overrides Sub Edit()
            CType(_copp, ICapeUtilities).Edit()
        End Sub

        Public Overrides Sub Initialize()
            If Not _copp Is Nothing Then CType(_copp, ICapeUtilities).Initialize()
        End Sub

        Public Overrides ReadOnly Property parameters1() As Object
            Get
                Return CType(_copp, ICapeUtilities).parameters()
            End Get
        End Property

        Public Overrides WriteOnly Property simulationContext() As Object
            Set(ByVal value As Object)
                CType(_copp, ICapeUtilities).simulationContext = value
            End Set
        End Property

        Public Overrides Sub Terminate()
            CType(_copp, ICapeUtilities).Terminate()
        End Sub

        Public Overrides Sub CalcEquilibrium2(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object)
            CType(_copp, ICapeThermoPropertyPackage).CalcEquilibrium(materialObject, flashType, props)
        End Sub

        Public Overrides Sub PropCheck1(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object, ByRef valid As Object)
            CType(_copp, ICapeThermoEquilibriumServer).PropCheck(materialObject, flashType, props, valid)
        End Sub

        Public Overrides Sub PropList(ByRef flashType As Object, ByRef props As Object, ByRef phases As Object, ByRef calcType As Object)
            CType(_copp, ICapeThermoEquilibriumServer).PropList(flashType, props, phases, calcType)
        End Sub

        Public Overrides Sub ValidityCheck1(ByVal materialObject As Object, ByVal props As Object, ByRef relList As Object)
            CType(_copp, ICapeThermoEquilibriumServer).ValidityCheck(materialObject, props, relList)
        End Sub

        Public Overrides Sub CalcProp1(ByVal materialObject As Object, ByVal props As Object, ByVal phases As Object, ByVal calcType As String)
            CType(_copp, ICapeThermoCalculationRoutine).CalcProp(materialObject, props, phases, calcType)
        End Sub

        Public Overrides Function GetPropList1() As Object
            Return CType(_copp, ICapeThermoCalculationRoutine).GetPropList()
        End Function

        Public Overrides Function PropCheck2(ByVal materialObject As Object, ByVal props As Object) As Object
            Return CType(_copp, ICapeThermoCalculationRoutine).PropCheck(materialObject, props)
        End Function

        Public Overrides Function ValidityCheck2(ByVal materialObject As Object, ByVal props As Object) As Object
            Return CType(_copp, ICapeThermoCalculationRoutine).ValidityCheck(materialObject, props)
        End Function

#End Region

#Region "    CAPE-OPEN 1.1 Thermo & Physical Properties"

        Public Overrides Function GetCompoundConstant(ByVal props As Object, ByVal compIds As Object) As Object
            'Me.SetMaterial(Me.CurrentMaterialStream)
            Return CType(_copp, ICapeThermoCompounds).GetCompoundConstant(props, compIds)
        End Function

        Public Overrides Sub GetCompoundList(ByRef compIds As Object, ByRef formulae As Object, ByRef names As Object, ByRef boilTemps As Object, ByRef molwts As Object, ByRef casnos As Object)
            'Me.SetMaterial(Me.CurrentMaterialStream)
            CType(_copp, ICapeThermoCompounds).GetCompoundList(compIds, formulae, names, boilTemps, molwts, casnos)
        End Sub

        Public Overrides Function GetConstPropList() As Object
            'Me.SetMaterial(Me.CurrentMaterialStream)
            Return CType(_copp, ICapeThermoCompounds).GetConstPropList()
        End Function

        Public Overrides Function GetNumCompounds() As Integer
            'Me.SetMaterial(Me.CurrentMaterialStream)
            Return CType(_copp, ICapeThermoCompounds).GetNumCompounds()
        End Function

        Public Overrides Sub GetPDependentProperty(ByVal props As Object, ByVal pressure As Double, ByVal compIds As Object, ByRef propVals As Object)
            'Me.SetMaterial(Me.CurrentMaterialStream)
            CType(_copp, ICapeThermoCompounds).GetPDependentProperty(props, pressure, compIds, propVals)
        End Sub

        Public Overrides Function GetPDependentPropList() As Object
            'Me.SetMaterial(Me.CurrentMaterialStream)
            Return CType(_copp, ICapeThermoCompounds).GetPDependentPropList
        End Function

        Public Overrides Sub GetTDependentProperty(ByVal props As Object, ByVal temperature As Double, ByVal compIds As Object, ByRef propVals As Object)
            'Me.SetMaterial(Me.CurrentMaterialStream)
            CType(_copp, ICapeThermoCompounds).GetTDependentProperty(props, temperature, compIds, propVals)
        End Sub

        Public Overrides Function GetTDependentPropList() As Object
            'Me.SetMaterial(Me.CurrentMaterialStream)
            Return CType(_copp, ICapeThermoCompounds).GetTDependentPropList
        End Function

        Public Overrides Function GetNumPhases() As Integer
            'Me.SetMaterial(Me.CurrentMaterialStream)
            Return CType(_copp, ICapeThermoPhases).GetNumPhases
        End Function

        Public Overrides Function GetPhaseInfo(ByVal phaseLabel As String, ByVal phaseAttribute As String) As Object
            'Me.SetMaterial(Me.CurrentMaterialStream)
            Return CType(_copp, ICapeThermoPhases).GetPhaseInfo(phaseLabel, phaseAttribute)
        End Function

        Public Overrides Sub GetPhaseList1(ByRef phaseLabels As Object, ByRef stateOfAggregation As Object, ByRef keyCompoundId As Object)
            'Me.SetMaterial(Me.CurrentMaterialStream)
            CType(_copp, ICapeThermoPhases).GetPhaseList(phaseLabels, stateOfAggregation, keyCompoundId)
        End Sub

        Public Overrides Sub CalcAndGetLnPhi(ByVal phaseLabel As String, ByVal temperature As Double, ByVal pressure As Double, ByVal moleNumbers As Object, ByVal fFlags As Integer, ByRef lnPhi As Object, ByRef lnPhiDT As Object, ByRef lnPhiDP As Object, ByRef lnPhiDn As Object)
            'Me.SetMaterial(Me.CurrentMaterialStream)
            CType(_copp, ICapeThermoPropertyRoutine).CalcAndGetLnPhi(phaseLabel, temperature, pressure, moleNumbers, fFlags, lnPhi, lnPhiDT, lnPhiDP, lnPhiDn)
        End Sub

        Public Overrides Sub CalcSinglePhaseProp(ByVal props As Object, ByVal phaseLabel As String)
            'Me.SetMaterial(Me.CurrentMaterialStream)
            CType(_copp, ICapeThermoPropertyRoutine).CalcSinglePhaseProp(props, phaseLabel)
        End Sub

        Public Overrides Sub CalcTwoPhaseProp(ByVal props As Object, ByVal phaseLabels As Object)
            'Me.SetMaterial(Me.CurrentMaterialStream)
            CType(_copp, ICapeThermoPropertyRoutine).CalcTwoPhaseProp(props, phaseLabels)
        End Sub

        Public Overrides Function CheckSinglePhasePropSpec(ByVal [property] As String, ByVal phaseLabel As String) As Boolean
            Return CType(_copp, ICapeThermoPropertyRoutine).CheckSinglePhasePropSpec([property], phaseLabel)
        End Function

        Public Overrides Function CheckTwoPhasePropSpec(ByVal [property] As String, ByVal phaseLabels As Object) As Boolean
            Return CType(_copp, ICapeThermoPropertyRoutine).CheckTwoPhasePropSpec([property], phaseLabels)
        End Function

        Public Overrides Function GetSinglePhasePropList() As Object
            Return CType(_copp, ICapeThermoPropertyRoutine).GetSinglePhasePropList()
        End Function

        Public Overrides Function GetTwoPhasePropList() As Object
            Return CType(_copp, ICapeThermoPropertyRoutine).GetTwoPhasePropList()
        End Function

        Public Overrides Function GetUniversalConstant1(ByVal constantId As String) As Object
            Return CType(_copp, ICapeThermoUniversalConstant).GetUniversalConstant(constantId)
        End Function

        Public Overrides Function GetUniversalConstantList() As Object
            Return CType(_copp, ICapeThermoUniversalConstant).GetUniversalConstantList()
        End Function

        Public Overrides Sub CalcEquilibrium1(ByVal specification1 As Object, ByVal specification2 As Object, ByVal solutionType As String)
            'Me.SetMaterial(Me.CurrentMaterialStream)

            'Me.DW_ZerarPhaseProps(Phase.Vapor)
            'Me.DW_ZerarPhaseProps(Phase.Liquid)
            'Me.DW_ZerarPhaseProps(Phase.Liquid1)
            'Me.DW_ZerarPhaseProps(Phase.Liquid2)
            'Me.DW_ZerarPhaseProps(Phase.Liquid3)
            'Me.DW_ZerarPhaseProps(Phase.Aqueous)
            'Me.DW_ZerarPhaseProps(Phase.Solid)

            Me.CurrentMaterialStream.AtEquilibrium = False

            CType(_copp, ICapeThermoEquilibriumRoutine).CalcEquilibrium(specification1, specification2, solutionType)

            Me.CurrentMaterialStream.AtEquilibrium = True

        End Sub

        Public Overrides Function CheckEquilibriumSpec(ByVal specification1 As Object, ByVal specification2 As Object, ByVal solutionType As String) As Boolean
            CType(_copp, ICapeThermoEquilibriumRoutine).CheckEquilibriumSpec(specification1, specification2, solutionType)
        End Function

        Public Overrides Sub SetMaterial(ByVal material As Object)
            CType(_copp, ICapeThermoMaterialContext).SetMaterial(material)
            Dim mcompounds As Integer = CType(material, ICapeThermoCompounds).GetNumCompounds
            Dim pcompounds As Integer = CType(_copp, ICapeThermoCompounds).GetNumCompounds
            If mcompounds <> pcompounds Then
                Flowsheet.ShowMessage("The compounds in DWSIM and CAPE-OPEN Property Package don't match. Please check the compound associations in the Property Package Settings in DWSIM.", IFlowsheet.MessageType.GeneralError)
                Throw New Exception("The compounds in DWSIM and CAPE-OPEN Property Package don't match. Please check the compound associations in the Property Package Settings in DWSIM.")
            End If
        End Sub

        Public Overrides Sub UnsetMaterial()
            CType(_copp, ICapeThermoMaterialContext).UnsetMaterial()
        End Sub

#End Region

#Region "    Auxiliary Functions"

        <OnDeserialized()> Sub PersistLoad(ByVal context As System.Runtime.Serialization.StreamingContext)

            If _selts IsNot Nothing Then

                Dim contains As Boolean = False
                Dim t As Type = Nothing

                Try
                    t = Type.GetTypeFromProgID(_selts.TypeName)
                Catch ex As Exception
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage("Error creating CAPE-OPEN Thermo Server / Property Package Manager instance." & vbCrLf & ex.ToString, IFlowsheet.MessageType.GeneralError)
                End Try

                Try
                    _pptpl = Activator.CreateInstance(t)
                Catch ex As Exception
                    Me.CurrentMaterialStream.Flowsheet.ShowMessage("Error creating CAPE-OPEN Property Package instance." & vbCrLf & ex.ToString, IFlowsheet.MessageType.GeneralError)
                End Try

                If _istrts IsNot Nothing Then
                    Dim myuo As Interfaces2.IPersistStreamInit = TryCast(_pptpl, Interfaces2.IPersistStreamInit)
                    If Not myuo Is Nothing Then
                        Try
                            _istrts.baseStream.Position = 0
                            myuo.Load(_istrts)
                        Catch ex As Exception
                        End Try
                    Else
                        Dim myuo2 As Interfaces2.IPersistStream = TryCast(_pptpl, Interfaces2.IPersistStream)
                        If myuo2 IsNot Nothing Then
                            Try
                                _istrts.baseStream.Position = 0
                                myuo2.Load(_istrts)
                            Catch ex As Exception
                            End Try
                        End If
                    End If
                End If

                If Not _pptpl Is Nothing Then

                    Dim myppm As CapeOpen.ICapeUtilities = TryCast(_pptpl, CapeOpen.ICapeUtilities)
                    If Not myppm Is Nothing Then
                        Try
                            myppm.Initialize()
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = _pptpl
                            Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName + ": error initializing CAPE-OPEN Property Package - " + ex.Message.ToString(), IFlowsheet.MessageType.GeneralError)
                            Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, IFlowsheet.MessageType.GeneralError)
                        End Try
                    End If

                    Dim pplist As String()

                    If _coversion = "1.0" Then
                        pplist = CType(_pptpl, ICapeThermoSystem).GetPropertyPackages
                    Else
                        pplist = CType(_pptpl, ICapeThermoPropertyPackageManager).GetPropertyPackageList
                    End If

                    For Each pp As String In pplist
                        If pp = _ppname Then
                            contains = True
                            Exit For
                        End If
                    Next

                End If

                If _coversion = "1.0" Then
                    _copp = CType(_pptpl, ICapeThermoSystem).ResolvePropertyPackage(_ppname)
                Else
                    _copp = CType(_pptpl, ICapeThermoPropertyPackageManager).GetPropertyPackage(_ppname)
                End If

                If _istrpp IsNot Nothing Then
                    Dim myuo As IPersistStreamInit = TryCast(_copp, Interfaces2.IPersistStreamInit)
                    If Not myuo Is Nothing Then
                        Try
                            _istrpp.baseStream.Position = 0
                            myuo.Load(_istrpp)
                        Catch ex As Exception
                            Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName + ": error restoring persisted data from CAPE-OPEN Object - " + ex.Message.ToString(), IFlowsheet.MessageType.GeneralError)
                        End Try
                    Else
                        Dim myuo2 As Interfaces2.IPersistStream = TryCast(_copp, Interfaces2.IPersistStream)
                        If myuo2 IsNot Nothing Then
                            Try
                                _istrpp.baseStream.Position = 0
                                myuo2.Load(_istrpp)
                            Catch ex As Exception
                                Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName + ": error restoring persisted data from CAPE-OPEN Object - " + ex.Message.ToString(), IFlowsheet.MessageType.GeneralError)
                            End Try
                        End If
                    End If
                End If

                Dim myuu As CapeOpen.ICapeUtilities = TryCast(_copp, CapeOpen.ICapeUtilities)
                If Not myuu Is Nothing Then
                    Try
                        myuu.Initialize()
                    Catch ex As Exception
                        Dim ecu As CapeOpen.ECapeUser = _copp
                        Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName + ": error initializing CAPE-OPEN Property Package - " + ex.Message.ToString(), IFlowsheet.MessageType.GeneralError)
                        Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, IFlowsheet.MessageType.GeneralError)
                    End Try
                End If

            End If

        End Sub

        <OnSerializing()> Sub PersistSave(ByVal context As System.Runtime.Serialization.StreamingContext)

            'If the CAPE-OPEN Property Package doesn't implement any of the IPersist interfaces, the _istrpp variable will be null.

            If Not _pptpl Is Nothing Then
                Dim myuo As Interfaces2.IPersistStream = TryCast(_pptpl, Interfaces2.IPersistStream)
                If myuo IsNot Nothing Then
                    _istrts = New ComIStreamWrapper(New MemoryStream())
                    myuo.Save(_istrts, True)
                Else
                    Dim myuo2 As Interfaces2.IPersistStreamInit = TryCast(_pptpl, Interfaces2.IPersistStreamInit)
                    If myuo2 IsNot Nothing Then
                        _istrts = New ComIStreamWrapper(New MemoryStream())
                        myuo2.Save(_istrts, True)
                    End If
                End If
            End If

            If Not _copp Is Nothing Then
                Dim myuo As Interfaces2.IPersistStream = TryCast(_copp, Interfaces2.IPersistStream)
                If myuo IsNot Nothing Then
                    _istrpp = New ComIStreamWrapper(New MemoryStream())
                    myuo.Save(_istrpp, True)
                Else
                    Dim myuo2 As Interfaces2.IPersistStreamInit = TryCast(_copp, Interfaces2.IPersistStreamInit)
                    If myuo2 IsNot Nothing Then
                        _istrpp = New ComIStreamWrapper(New MemoryStream())
                        myuo2.Save(_istrpp, True)
                    End If
                End If
            End If

        End Sub

#End Region

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Me.ComponentName = (From el As XElement In data Select el Where el.Name = "ComponentName").SingleOrDefault.Value
            Me.ComponentDescription = (From el As XElement In data Select el Where el.Name = "ComponentDescription").SingleOrDefault.Value
            Me.Tag = (From el As XElement In data Select el Where el.Name = "Tag").SingleOrDefault.Value
            Me._coversion = (From el As XElement In data Select el Where el.Name = "CAPEOPEN_Version").SingleOrDefault.Value
            Me._ppname = (From el As XElement In data Select el Where el.Name = "CAPEOPEN_PropertyPackageName").SingleOrDefault.Value

            _mappings.Clear()
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "CompoundMappings").Elements
                _mappings.Add(xel2.@From, xel2.@To)
            Next

            _phasemappings.Clear()
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "PhaseMappings").Elements
                _phasemappings.Add(xel2.@From, New PhaseInfo(xel2.@PhaseLabel, xel2.@DWPhaseIndex, [Enum].Parse(Type.GetType("DWSIM.Thermodynamics.PropertyPackages.Phase"), xel2.@DWPhaseID)))
            Next

            Dim pdata1 As XElement = (From el As XElement In data Select el Where el.Name = "PersistedData1").SingleOrDefault
            If Not pdata1 Is Nothing Then
                _istrts = New ComIStreamWrapper(New MemoryStream(Convert.FromBase64String(pdata1.Value)))
            End If

            Dim pdata2 As XElement = (From el As XElement In data Select el Where el.Name = "PersistedData2").SingleOrDefault
            If Not pdata2 Is Nothing Then
                _istrpp = New ComIStreamWrapper(New MemoryStream(Convert.FromBase64String(pdata2.Value)))
            End If

            Dim info As XElement = (From el As XElement In data Select el Where el.Name = "CAPEOPEN_Object_Info").SingleOrDefault
            Try
                _selts = New CapeOpenObjInfo
                _selts.LoadData(info.Elements.ToList)
            Catch ex As Exception
            End Try

            PersistLoad(Nothing)

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As New System.Collections.Generic.List(Of System.Xml.Linq.XElement)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("Type", Me.GetType.ToString))
                .Add(New XElement("ComponentName", ComponentName))
                .Add(New XElement("ComponentDescription", ComponentDescription))
                .Add(New XElement("Tag", Tag))
                .Add(New XElement("CAPEOPEN_Version", _coversion))
                .Add(New XElement("CAPEOPEN_PropertyPackageName", _ppname))
                If _selts IsNot Nothing Then
                    .Add(New XElement("CAPEOPEN_Object_Info", _selts.SaveData().ToArray))
                End If
                .Add(New XElement("CompoundMappings"))
                For Each kvp As KeyValuePair(Of String, String) In _mappings
                    .Item(.Count - 1).Add(New XElement("CompoundMapping", New XAttribute("From", kvp.Key), New XAttribute("To", kvp.Value)))
                Next
                .Add(New XElement("PhaseMappings"))
                For Each kvp As KeyValuePair(Of String, PhaseInfo) In _phasemappings
                    .Item(.Count - 1).Add(New XElement("PhaseMapping", New XAttribute("From", kvp.Key),
                                                                        New XAttribute("DWPhaseID", kvp.Value.DWPhaseID),
                                                                        New XAttribute("DWPhaseIndex", kvp.Value.DWPhaseIndex),
                                                                        New XAttribute("PhaseLabel", kvp.Value.PhaseLabel)))
                Next

                If Not _pptpl Is Nothing Then
                    Dim myuo As Interfaces2.IPersistStream = TryCast(_pptpl, Interfaces2.IPersistStream)
                    If myuo IsNot Nothing Then
                        Dim mbs As New ComIStreamWrapper(New MemoryStream)
                        myuo.Save(mbs, True)
                        .Add(New XElement("PersistedData1", Convert.ToBase64String(CType(mbs.baseStream, MemoryStream).ToArray())))
                    Else
                        Dim myuo2 As Interfaces2.IPersistStreamInit = TryCast(_pptpl, Interfaces2.IPersistStreamInit)
                        If myuo2 IsNot Nothing Then
                            Dim mbs As New ComIStreamWrapper(New MemoryStream)
                            myuo2.Save(mbs, True)
                            .Add(New XElement("PersistedData1", Convert.ToBase64String(CType(mbs.baseStream, MemoryStream).ToArray())))
                        End If
                    End If
                End If

                If Not _copp Is Nothing Then
                    Dim myuo As Interfaces2.IPersistStream = TryCast(_copp, Interfaces2.IPersistStream)
                    If myuo IsNot Nothing Then
                        Dim mbs As New ComIStreamWrapper(New MemoryStream)
                        myuo.Save(mbs, True)
                        .Add(New XElement("PersistedData2", Convert.ToBase64String(CType(mbs.baseStream, MemoryStream).ToArray())))
                    Else
                        Dim myuo2 As Interfaces2.IPersistStreamInit = TryCast(_copp, Interfaces2.IPersistStreamInit)
                        If myuo2 IsNot Nothing Then
                            Dim mbs As New ComIStreamWrapper(New MemoryStream)
                            myuo2.Save(mbs, True)
                            .Add(New XElement("PersistedData2", Convert.ToBase64String(CType(mbs.baseStream, MemoryStream).ToArray())))
                        End If
                    End If
                End If

            End With

            Return elements

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Return 0.0

        End Function

    End Class

End Namespace
