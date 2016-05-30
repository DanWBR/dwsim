'    DWSIM CAPE-OPEN Equilibrium Server Socket
'    Copyright 2016 Daniel Wagner O. de Medeiros 
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

Imports System.Threading.Tasks
Imports System.Linq
Imports DWSIM.SharedClasses
Imports CapeOpen

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class CAPEOPEN_Equilibrium_Server

        Inherits FlashAlgorithm

        <System.NonSerialized()> Private _coes As Object 'CAPE-OPEN Thermo 1.1 Equilibrium Server

        Private _selts As CapeOpenObjInfo
        Private _istr As ComIStreamWrapper

        Private initialized As Boolean = False

        Private T, P, He, Se, VF, L1, L2, V, S As Double, Vx1, Vx2, Vy, Vs, Vxs As Double()

        Sub New()
            MyBase.New()
        End Sub

        Public Overrides ReadOnly Property InternalUseOnly As Boolean
            Get
                Return False
            End Get
        End Property

        Public Sub Initialize()
            If Not _coes Is Nothing Then CType(_coes, ICapeUtilities).Initialize()
        End Sub

        Public WriteOnly Property simulationContext() As Object
            Set(ByVal value As Object)
                CType(_coes, ICapeUtilities).simulationContext = value
            End Set
        End Property

        Public Sub Terminate()
            CType(_coes, ICapeUtilities).Terminate()
        End Sub

        Public Sub SetMaterial(ByVal material As Object)
            CType(_coes, ICapeThermoMaterialContext).SetMaterial(material)
        End Sub

        Public Sub UnsetMaterial()
            CType(_coes, ICapeThermoMaterialContext).UnsetMaterial()
        End Sub

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal Pspec As Double, ByVal Tspec As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim mo = New Streams.MaterialStream("", "")
            PP.Flowsheet.AddCompoundsToMaterialStream(mo)

            Dim tmppp = PP.Clone

            With mo

                .SetOverallComposition(Vz)
                .Phases(0).Properties.temperature = Tspec
                .Phases(0).Properties.pressure = Pspec

            End With

            SetMaterial(mo)

            Dim s1 As String() = New String() {"temperature", Nothing, "Overall"}
            Dim s2 As String() = New String() {"pressure", Nothing, "Overall"}

            Try
                SetMaterial(mo)
                CType(_coes, ICapeThermoEquilibriumRoutine).CalcEquilibrium(s1, s2, "Unspecified")
                UnsetMaterial()
                With mo
                    L1 = .Phases(3).Properties.molarfraction.GetValueOrDefault
                    L2 = .Phases(4).Properties.molarfraction.GetValueOrDefault
                    V = .Phases(2).Properties.molarfraction.GetValueOrDefault
                    S = .Phases(7).Properties.molarfraction.GetValueOrDefault
                    Vx1 = tmppp.RET_VMOL(Phase.Liquid1)
                    Vx2 = tmppp.RET_VMOL(Phase.Liquid2)
                    Vy = tmppp.RET_VMOL(Phase.Vapor)
                    Vs = tmppp.RET_VMOL(Phase.Solid)
                End With
                mo = Nothing
                tmppp.Dispose()
                tmppp = Nothing
            Catch ex As Exception
                Dim ecu As CapeOpen.ECapeUser = _coes
                PP.CurrentMaterialStream.Flowsheet.ShowMessage(_coes.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                Throw ex
            End Try

            Return New Object() {L1, V, Vx1, Vy, 1, L2, Vx2, S, Vxs}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal Pspec As Double, ByVal Hspec As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim mo = New Streams.MaterialStream("", "")
            PP.Flowsheet.AddCompoundsToMaterialStream(mo)

            Dim tmppp = PP.Clone

            With mo

                .SetOverallComposition(Vz)
                .Phases(0).Properties.enthalpy = Hspec
                .Phases(0).Properties.pressure = Pspec

            End With

            SetMaterial(mo)

            Dim s1 As String() = New String() {"temperature", Nothing, "Overall"}
            Dim s2 As String() = New String() {"enthalpy", Nothing, "Overall"}

            Try
                SetMaterial(mo)
                CType(_coes, ICapeThermoEquilibriumRoutine).CalcEquilibrium(s1, s2, "Unspecified")
                UnsetMaterial()
                With mo
                    T = .Phases(0).Properties.temperature.GetValueOrDefault
                    L1 = .Phases(3).Properties.molarfraction.GetValueOrDefault
                    L2 = .Phases(4).Properties.molarfraction.GetValueOrDefault
                    V = .Phases(2).Properties.molarfraction.GetValueOrDefault
                    S = .Phases(7).Properties.molarfraction.GetValueOrDefault
                    Vx1 = tmppp.RET_VMOL(Phase.Liquid1)
                    Vx2 = tmppp.RET_VMOL(Phase.Liquid2)
                    Vy = tmppp.RET_VMOL(Phase.Vapor)
                    Vs = tmppp.RET_VMOL(Phase.Solid)
                End With
                mo = Nothing
                tmppp.Dispose()
                tmppp = Nothing
            Catch ex As Exception
                Dim ecu As CapeOpen.ECapeUser = _coes
                PP.CurrentMaterialStream.Flowsheet.ShowMessage(_coes.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                Throw ex
            End Try

            Return New Object() {L1, V, Vx1, Vy, T, 0, Vy.DivideY(Vx1), L2, Vx2, S, Vxs}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal Pspec As Double, ByVal Sspec As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim mo = New Streams.MaterialStream("", "")
            PP.Flowsheet.AddCompoundsToMaterialStream(mo)

            Dim tmppp = PP.Clone

            With mo

                .SetOverallComposition(Vz)
                .Phases(0).Properties.entropy = Sspec
                .Phases(0).Properties.pressure = Pspec

            End With

            SetMaterial(mo)

            Dim s1 As String() = New String() {"temperature", Nothing, "Overall"}
            Dim s2 As String() = New String() {"entropy", Nothing, "Overall"}

            Try
                SetMaterial(mo)
                CType(_coes, ICapeThermoEquilibriumRoutine).CalcEquilibrium(s1, s2, "Unspecified")
                UnsetMaterial()
                With mo
                    T = .Phases(0).Properties.temperature.GetValueOrDefault
                    L1 = .Phases(3).Properties.molarfraction.GetValueOrDefault
                    L2 = .Phases(4).Properties.molarfraction.GetValueOrDefault
                    V = .Phases(2).Properties.molarfraction.GetValueOrDefault
                    S = .Phases(7).Properties.molarfraction.GetValueOrDefault
                    Vx1 = tmppp.RET_VMOL(Phase.Liquid1)
                    Vx2 = tmppp.RET_VMOL(Phase.Liquid2)
                    Vy = tmppp.RET_VMOL(Phase.Vapor)
                    Vs = tmppp.RET_VMOL(Phase.Solid)
                End With
                mo = Nothing
                tmppp.Dispose()
                tmppp = Nothing
            Catch ex As Exception
                Dim ecu As CapeOpen.ECapeUser = _coes
                PP.CurrentMaterialStream.Flowsheet.ShowMessage(_coes.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                Throw ex
            End Try

            Return New Object() {L1, V, Vx1, Vy, T, 0, Vy.DivideY(Vx1), L2, Vx2, S, Vxs}

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal Tspec As Double, ByVal Vspec As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim mo = New Streams.MaterialStream("", "")
            PP.Flowsheet.AddCompoundsToMaterialStream(mo)

            Dim tmppp = PP.Clone

            With mo

                .SetOverallComposition(Vz)
                .Phases(2).Properties.molarfraction = Vspec
                .Phases(0).Properties.temperature = Tspec

            End With

            SetMaterial(mo)

            Dim s1 As String() = New String() {"temperature", Nothing, "Overall"}
            Dim s2 As String() = New String() {"phaseFraction", "Mole", "Vapor"}

            Try
                SetMaterial(mo)
                CType(_coes, ICapeThermoEquilibriumRoutine).CalcEquilibrium(s1, s2, "Unspecified")
                UnsetMaterial()
                With mo
                    P = .Phases(0).Properties.pressure.GetValueOrDefault
                    L1 = .Phases(3).Properties.molarfraction.GetValueOrDefault
                    L2 = .Phases(4).Properties.molarfraction.GetValueOrDefault
                    V = .Phases(2).Properties.molarfraction.GetValueOrDefault
                    S = .Phases(7).Properties.molarfraction.GetValueOrDefault
                    Vx1 = tmppp.RET_VMOL(Phase.Liquid1)
                    Vx2 = tmppp.RET_VMOL(Phase.Liquid2)
                    Vy = tmppp.RET_VMOL(Phase.Vapor)
                    Vs = tmppp.RET_VMOL(Phase.Solid)
                End With
                mo = Nothing
                tmppp.Dispose()
                tmppp = Nothing
            Catch ex As Exception
                Dim ecu As CapeOpen.ECapeUser = _coes
                PP.CurrentMaterialStream.Flowsheet.ShowMessage(_coes.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                Throw ex
            End Try

            Return New Object() {L1, V, Vx1, Vy, P, 0, Vy.DivideY(Vx1), L2, Vx2, S, Vxs}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal Pspec As Double, ByVal Vspec As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim mo = New Streams.MaterialStream("", "")
            PP.Flowsheet.AddCompoundsToMaterialStream(mo)

            Dim tmppp = PP.Clone

            With mo

                .SetOverallComposition(Vz)
                .Phases(2).Properties.molarfraction = Vspec
                .Phases(0).Properties.pressure = Pspec

            End With

            SetMaterial(mo)

            Dim s1 As String() = New String() {"pressure", Nothing, "Overall"}
            Dim s2 As String() = New String() {"phaseFraction", "Mole", "Vapor"}

            Try
                SetMaterial(mo)
                CType(_coes, ICapeThermoEquilibriumRoutine).CalcEquilibrium(s1, s2, "Unspecified")
                UnsetMaterial()
                With mo
                    T = .Phases(0).Properties.temperature.GetValueOrDefault
                    L1 = .Phases(3).Properties.molarfraction.GetValueOrDefault
                    L2 = .Phases(4).Properties.molarfraction.GetValueOrDefault
                    V = .Phases(2).Properties.molarfraction.GetValueOrDefault
                    S = .Phases(7).Properties.molarfraction.GetValueOrDefault
                    Vx1 = tmppp.RET_VMOL(Phase.Liquid1)
                    Vx2 = tmppp.RET_VMOL(Phase.Liquid2)
                    Vy = tmppp.RET_VMOL(Phase.Vapor)
                    Vs = tmppp.RET_VMOL(Phase.Solid)
                End With
                mo = Nothing
                tmppp.Dispose()
                tmppp = Nothing
            Catch ex As Exception
                Dim ecu As CapeOpen.ECapeUser = _coes
                PP.CurrentMaterialStream.Flowsheet.ShowMessage(_coes.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Interfaces.IFlowsheet.MessageType.GeneralError)
                Throw ex
            End Try

            Return New Object() {L1, V, Vx1, Vy, T, 0, Vy.DivideY(Vx1), L2, Vx2, S, Vxs}

        End Function

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod

            Get
                Return Interfaces.Enums.FlashMethod.CAPE_OPEN_Equilibrium_Server
            End Get

        End Property

        Public Overrides ReadOnly Property Description As String

            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Permite a utilização de um Servidor de Equilíbrio CAPE-OPEN externo"
                Else
                    Return "Wrapper for external CAPE-OPEN Thermo 1.1 Equilibrium Servers"
                End If
            End Get

        End Property

        Public Overrides ReadOnly Property Name As String

            Get
                Return "CAPE-OPEN ES Socket"
            End Get

        End Property

    End Class

End Namespace
