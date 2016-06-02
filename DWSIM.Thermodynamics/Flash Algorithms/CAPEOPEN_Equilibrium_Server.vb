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
Imports System.IO
Imports DWSIM.Interfaces

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class CAPEOPEN_Equilibrium_Server

        Inherits FlashAlgorithm

        Public _selppm As CapeOpenObjInfo
        <System.NonSerialized()> Private _istres, _istrppm As ComIStreamWrapper

        <System.NonSerialized()> Public _coes, _coppm As Object 'CAPE-OPEN ES & Manager
        Public _esname As String = ""

        Public _phasemappings As New Dictionary(Of String, PhaseInfo)
        Public _mappings As New Dictionary(Of String, String)

        Private initialized As Boolean = False

        Private T, P, He, Se, VF, L1, L2, V, S As Double, Vx1, Vx2, Vy, Vxs As Double()

        Sub New()
            MyBase.New()
            CreatePhaseMappings()
        End Sub

        Public Overrides ReadOnly Property InternalUseOnly As Boolean
            Get
                Return False
            End Get
        End Property

#Region "CAPE-OPEN"

        Public Sub CreatePhaseMappings()
            Me._phasemappings = New Dictionary(Of String, PhaseInfo)
            With Me._phasemappings
                .Add("Vapor", New PhaseInfo("", 2, Phase.Vapor))
                .Add("Liquid1", New PhaseInfo("", 3, Phase.Liquid1))
                .Add("Liquid2", New PhaseInfo("", 4, Phase.Liquid2))
                .Add("Solid", New PhaseInfo("", 7, Phase.Solid))
            End With
        End Sub

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

#End Region

#Region "XML Load/Save"

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean

            MyBase.LoadData(data)

            Me._esname = (From el As XElement In data Select el Where el.Name = "CAPEOPEN_EquilibriumServerName").SingleOrDefault.Value

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
                _istrppm = New ComIStreamWrapper(New MemoryStream(Convert.FromBase64String(pdata1.Value)))
            End If

            Dim pdata2 As XElement = (From el As XElement In data Select el Where el.Name = "PersistedData2").SingleOrDefault
            If Not pdata2 Is Nothing Then
                _istres = New ComIStreamWrapper(New MemoryStream(Convert.FromBase64String(pdata2.Value)))
            End If

            Dim info As XElement = (From el As XElement In data Select el Where el.Name = "CAPEOPEN_Object_Info").SingleOrDefault
            _selppm = New CapeOpenObjInfo
            _selppm.LoadData(info.Elements.ToList)

            PersistLoad(Nothing)

        End Function

        Public Overrides Function SaveData() As List(Of XElement)

            Dim elements = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("CAPEOPEN_EquilibriumServerName", _esname))
                .Add(New XElement("CAPEOPEN_Object_Info", _selppm.SaveData().ToArray))
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

                If Not _coppm Is Nothing Then
                    Dim myuo As Interfaces2.IPersistStream = TryCast(_coppm, Interfaces2.IPersistStream)
                    If myuo IsNot Nothing Then
                        Dim mbs As New ComIStreamWrapper(New MemoryStream)
                        myuo.Save(mbs, True)
                        .Add(New XElement("PersistedData1", Convert.ToBase64String(CType(mbs.baseStream, MemoryStream).ToArray())))
                    Else
                        Dim myuo2 As Interfaces2.IPersistStreamInit = TryCast(_coppm, Interfaces2.IPersistStreamInit)
                        If myuo2 IsNot Nothing Then
                            Dim mbs As New ComIStreamWrapper(New MemoryStream)
                            myuo2.Save(mbs, True)
                            .Add(New XElement("PersistedData1", Convert.ToBase64String(CType(mbs.baseStream, MemoryStream).ToArray())))
                        End If
                    End If
                End If

                If Not _coes Is Nothing Then
                    Dim myuo As Interfaces2.IPersistStream = TryCast(_coes, Interfaces2.IPersistStream)
                    If myuo IsNot Nothing Then
                        Dim mbs As New ComIStreamWrapper(New MemoryStream)
                        myuo.Save(mbs, True)
                        .Add(New XElement("PersistedData2", Convert.ToBase64String(CType(mbs.baseStream, MemoryStream).ToArray())))
                    Else
                        Dim myuo2 As Interfaces2.IPersistStreamInit = TryCast(_coes, Interfaces2.IPersistStreamInit)
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

        Sub PersistLoad(ByVal context As System.Runtime.Serialization.StreamingContext)

            If _selppm IsNot Nothing Then

                Dim contains As Boolean = False
                Dim t As Type = Nothing

                Try
                    t = Type.GetTypeFromProgID(_selppm.TypeName)
                Catch ex As Exception
                    Me.WriteDebugInfo("Error creating CAPE-OPEN Thermo Server / Property Package Manager instance." & vbCrLf & ex.ToString)
                End Try

                Try
                    _coppm = Activator.CreateInstance(t)
                Catch ex As Exception
                    Me.WriteDebugInfo("Error creating Equilibrium Server instance." & vbCrLf & ex.ToString)
                End Try

                If _istrppm IsNot Nothing Then
                    Dim myuo As Interfaces2.IPersistStreamInit = TryCast(_coppm, Interfaces2.IPersistStreamInit)
                    If Not myuo Is Nothing Then
                        Try
                            _istrppm.baseStream.Position = 0
                            myuo.Load(_istrppm)
                        Catch ex As Exception
                        End Try
                    Else
                        Dim myuo2 As Interfaces2.IPersistStream = TryCast(_coppm, Interfaces2.IPersistStream)
                        If myuo2 IsNot Nothing Then
                            Try
                                _istrppm.baseStream.Position = 0
                                myuo2.Load(_istrppm)
                            Catch ex As Exception
                            End Try
                        End If
                    End If
                End If

                If Not _coppm Is Nothing Then

                    Dim myppm As CapeOpen.ICapeUtilities = TryCast(_coppm, CapeOpen.ICapeUtilities)
                    If Not myppm Is Nothing Then
                        Try
                            myppm.Initialize()
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = _coppm
                            Me.WriteDebugInfo("Error initializing CAPE-OPEN Property Package Manager - " + ex.Message.ToString())
                            Me.WriteDebugInfo("CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description)
                        End Try
                    End If

                    Dim pplist As String()

                    pplist = CType(_coppm, ICapeThermoPropertyPackageManager).GetPropertyPackageList

                    For Each pp As String In pplist
                        If pp = _esname Then
                            contains = True
                            Exit For
                        End If
                    Next

                End If

                _coes = CType(_coppm, ICapeThermoPropertyPackageManager).GetPropertyPackage(_esname)
                
                If _istres IsNot Nothing Then
                    Dim myuo As Interfaces2.IPersistStreamInit = TryCast(_coes, Interfaces2.IPersistStreamInit)
                    If Not myuo Is Nothing Then
                        Try
                            _istres.baseStream.Position = 0
                            myuo.Load(_istres)
                        Catch ex As Exception
                            Me.WriteDebugInfo("Error restoring persisted data from CAPE-OPEN Object - " + ex.Message.ToString())
                        End Try
                    Else
                        Dim myuo2 As Interfaces2.IPersistStream = TryCast(_coes, Interfaces2.IPersistStream)
                        If myuo2 IsNot Nothing Then
                            Try
                                _istres.baseStream.Position = 0
                                myuo2.Load(_istres)
                            Catch ex As Exception
                                Me.WriteDebugInfo("Error restoring persisted data from CAPE-OPEN Object - " + ex.Message.ToString())
                            End Try
                        End If
                    End If
                End If

                Dim myuu As CapeOpen.ICapeUtilities = TryCast(_coes, CapeOpen.ICapeUtilities)
                If Not myuu Is Nothing Then
                    Try
                        myuu.Initialize()
                    Catch ex As Exception
                        Dim ecu As CapeOpen.ECapeUser = _coes
                        Me.WriteDebugInfo("Error initializing CAPE-OPEN Property Package / Equilibrium Server - " + ex.Message.ToString())
                        Me.WriteDebugInfo("CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description)
                    End Try
                End If

            End If

        End Sub

#End Region

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal Pspec As Double, ByVal Tspec As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim mo = New Streams.MaterialStream("", "")
            PP.Flowsheet.AddCompoundsToMaterialStream(mo)

            Dim tmppp = PP.Clone

            With tmppp
                ._phasemappings = _phasemappings
            End With

            With mo

                .PropertyPackage = tmppp
                .SetOverallComposition(Vz)
                .Phases(0).Properties.temperature = Tspec
                .Phases(0).Properties.pressure = Pspec

            End With

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
                    Vxs = tmppp.RET_VMOL(Phase.Solid)
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

            With tmppp
                ._phasemappings = _phasemappings
            End With

            With mo

                .PropertyPackage = tmppp
                .SetOverallComposition(Vz)
                .Phases(0).Properties.enthalpy = Hspec
                .Phases(0).Properties.pressure = Pspec

            End With

            Dim s1 As String() = New String() {"pressure", Nothing, "Overall"}
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
                    Vxs = tmppp.RET_VMOL(Phase.Solid)
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

            With tmppp
                ._phasemappings = _phasemappings
            End With

            With mo

                .PropertyPackage = tmppp
                .SetOverallComposition(Vz)
                .Phases(0).Properties.entropy = Sspec
                .Phases(0).Properties.pressure = Pspec

            End With

            Dim s1 As String() = New String() {"pressure", Nothing, "Overall"}
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
                    Vxs = tmppp.RET_VMOL(Phase.Solid)
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

            With tmppp
                ._phasemappings = _phasemappings
            End With

            With mo

                .PropertyPackage = tmppp
                .SetOverallComposition(Vz)
                .Phases(2).Properties.molarfraction = Vspec
                .Phases(0).Properties.temperature = Tspec

            End With

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
                    Vxs = tmppp.RET_VMOL(Phase.Solid)
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

            With tmppp
                ._phasemappings = _phasemappings
            End With

            With mo

                .PropertyPackage = tmppp
                .SetOverallComposition(Vz)
                .Phases(2).Properties.molarfraction = Vspec
                .Phases(0).Properties.pressure = Pspec

            End With

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
                    Vxs = tmppp.RET_VMOL(Phase.Solid)
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
