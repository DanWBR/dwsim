'    CAPE-OPEN Unit Operation Wrapper Class
'    Copyright 2011-2014 Daniel Wagner O. de Medeiros
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

Imports Microsoft.Msdn.Samples.GraphicObjects
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports Microsoft.Scripting.Hosting
Imports System.IO
Imports CapeOpen
Imports System.Runtime.InteropServices.ComTypes
Imports System.Runtime.Serialization
Imports STATSTG = System.Runtime.InteropServices.ComTypes.STATSTG
Imports System.Runtime.InteropServices
Imports System.Linq
Imports System.Xml.Linq
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Reflection
Imports System.Runtime.Serialization.Formatters.Binary
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen
Imports System.Threading

Namespace DWSIM.SimulationObjects.UnitOperations

    <System.Serializable()> Public Class CapeOpenUO

        Inherits DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass

        <System.NonSerialized()> Private _couo As Object
        <System.NonSerialized()> Private _form As FormCapeOpenUnitSelector

        Private m_reactionSetID As String = "DefaultSet"
        Private m_reactionSetName As String = ""

        Private _seluo As DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo
        Private Shadows _ports As List(Of ICapeUnitPort)
        <System.NonSerialized()> Private _params As List(Of ICapeParameter)

        <System.NonSerialized> Private _tempdata As List(Of XElement)

        <System.NonSerialized()> Private _istr As DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.ComIStreamWrapper
        Private _persisteddata As Byte()

        Private _restorefromcollections As Boolean = False
        Private _recalculateoutputstreams As Boolean = True

        Public Sub New()
            MyBase.New()
            _ports = New List(Of ICapeUnitPort)
            _params = New List(Of ICapeParameter)
        End Sub

        Public Sub New(ByVal name As String, ByVal desc As String)
            Me.New(name, desc, Nothing)
        End Sub

        Public Function Clone2() As CapeOpenUO

            Dim persisteddata = Me.SaveData()

            Dim couo As New CapeOpenUO()

            couo.LoadData(persisteddata)

            Return couo

        End Function

        Public Sub New(ByVal name As String, ByVal description As String, ByVal gobj As GraphicObject)

            Me.New()

            Me.GraphicObject = gobj

            Me.ComponentName = name
            Me.ComponentDescription = description
            Me.FillNodeItems()
            Me.QTFillNodeItems()

            If Not DWSIM.App.IsRunningOnMono Then

                ShowForm()

                If Not _seluo Is Nothing Then
                    Try
                        Dim t As Type = Type.GetTypeFromProgID(_seluo.TypeName)
                        _couo = Activator.CreateInstance(t)
                        InitNew()
                        Init()
                        GetPorts()
                        GetParams()
                        CreateConnectors()
                    Catch ex As Exception
                        Me.FlowSheet.WriteToLog("Error creating CAPE-OPEN Unit Operation: " & ex.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                    End Try
                End If

            Else

                ShowFormMono()

                If Not _seluo Is Nothing Then

                    'Try

                    Dim myt As Type = Nothing

                    If Not File.Exists(_seluo.Location) Then
                        ShowFormMono()
                    End If

                    myt = GetManagedUnitType(_seluo.Location)

                    _couo = Activator.CreateInstance(myt)

                    Try
                        With _seluo
                            .Name = CType(_couo, ICapeIdentification).ComponentName
                            .Description = CType(_couo, ICapeIdentification).ComponentDescription
                            .TypeName = myt.Name
                            .Version = myt.Module.Assembly.GetName.Version.ToString
                        End With
                    Catch ex As Exception

                    End Try

                    InitNew()
                    Init()
                    GetPorts()
                    GetParams()
                    CreateConnectors()

                    'Catch ex As Exception
                    '    Me.FlowSheet.WriteToLog("Error creating CAPE-OPEN Unit Operation: " & ex.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                    'End Try

                End If

            End If



        End Sub

#Region "    CAPE-OPEN Specifics"

        <OnDeserialized()> Sub PersistLoad(ByVal context As System.Runtime.Serialization.StreamingContext)

            If Not DWSIM.App.IsRunningOnMono Then

                If Not _seluo Is Nothing Then
                    Dim t As Type = Type.GetTypeFromProgID(_seluo.TypeName)
                    Try
                        If _couo Is Nothing Then _couo = Activator.CreateInstance(t)
                    Catch ex As Exception
                        MessageBox.Show("Error creating CAPE-OPEN Unit Operation instance." & vbCrLf & ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If

            Else

                If Not _seluo Is Nothing Then
                    Dim myt As Type = Nothing
                    If Not File.Exists(_seluo.Location) Then
                        ShowFormMono()
                    End If
                    myt = GetManagedUnitType(_seluo.Location)
                    Try
                        _couo = Activator.CreateInstance(myt)
                    Catch ex As Exception
                        Dim ecu As CapeOpen.ECapeUser = _couo
                        MessageBox.Show(Me.ComponentName + ": error loading CAPE-OPEN Unit Operation - " + ex.Message.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                        MessageBox.Show(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description)
                    End Try
                End If

            End If

            If _persisteddata IsNot Nothing Then
                _istr = New DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream(_persisteddata))
            End If

            If _istr IsNot Nothing Then
                Dim myuo As Interfaces.IPersistStreamInit = TryCast(_couo, Interfaces.IPersistStreamInit)
                If Not myuo Is Nothing Then
                    Try
                        _istr.baseStream.Position = 0
                        myuo.Load(_istr)
                        _restorefromcollections = False
                    Catch ex As Exception
                        'couldn't restore data from IStream. Will restore using port and parameter collections instead.
                        MessageBox.Show(Me.GraphicObject.Tag + ": Error restoring persisted data from CAPE-OPEN Object - " + ex.Message.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                        _restorefromcollections = True
                    End Try
                Else
                    Dim myuo2 As Interfaces2.IPersistStream = TryCast(_couo, Interfaces2.IPersistStream)
                    If myuo2 IsNot Nothing Then
                        Try
                            _istr.baseStream.Position = 0
                            myuo2.Load(_istr)
                            _restorefromcollections = False
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = _couo
                            MessageBox.Show(Me.ComponentName + ": error loading CAPE-OPEN Unit Operation - " + ex.Message.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                            MessageBox.Show(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description)
                            _restorefromcollections = True
                        End Try
                    End If
                End If
            Else
                'the CAPE-OPEN object doesn't support Persistence, restore parameters and ports info from internal collections.
                _restorefromcollections = True
            End If

            'Init()

            If _restorefromcollections Then
                RestoreParams()
            Else
                GetParams()
            End If

        End Sub

        <OnSerializing()> Sub PersistSave(ByVal context As System.Runtime.Serialization.StreamingContext)

            'If the Unit Operation doesn't implement any of the IPersist interfaces, the _istr variable will be null.
            'The object will have to be restored using the parameters and ports' information only.

            _istr = Nothing

            If Not _couo Is Nothing Then
                Dim myuo As Interfaces.IPersistStreamInit = TryCast(_couo, Interfaces.IPersistStreamInit)
                If Not myuo Is Nothing Then
                    Dim myuo2 As Interfaces.IPersistStreamInit = TryCast(_couo, Interfaces.IPersistStreamInit)
                    If myuo2 IsNot Nothing Then
                        _istr = New DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream())
                        Try
                            _istr.baseStream.Position = 0
                            myuo2.Save(_istr, True)
                            Using ms As New MemoryStream()
                                _istr.baseStream.Position = 0
                                _istr.baseStream.CopyTo(ms)
                                _persisteddata = ms.ToArray()
                            End Using
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = _couo
                            Me.FlowSheet.WriteToLog(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                            Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag + ": Error saving data from CAPE-OPEN Object - " + ex.Message.ToString(), Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                        End Try
                    End If
                Else
                    Dim myuo2 As Interfaces2.IPersistStream = TryCast(_couo, Interfaces2.IPersistStream)
                    If myuo2 IsNot Nothing Then
                        _istr = New DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream())
                        Try
                            _istr.baseStream.Position = 0
                            myuo2.Save(_istr, True)
                            Using ms As New MemoryStream()
                                _istr.baseStream.Position = 0
                                _istr.baseStream.CopyTo(ms)
                                _persisteddata = ms.ToArray()
                            End Using
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = _couo
                            Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                            Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag + ": Error saving data from CAPE-OPEN Object - " + ex.Message.ToString(), Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                        End Try
                    End If
                End If
            End If

        End Sub

        Sub ShowForm()

            _form = New FormCapeOpenUnitSelector
            _form.ShowDialog(Me.FlowSheet)
            Me._seluo = _form._seluo

        End Sub

        Sub ShowFormMono()

            Dim ofd As New OpenFileDialog

            With ofd
                .Title = "Insert Managed CAPE-OPEN Unit Operation"
                .Filter = "Managed Assemblies (*.exe, *.dll)|*.EXE;*.DLL;*.exe;*.dll"
                .Multiselect = False
            End With

            If ofd.ShowDialog = DialogResult.OK Then
                Dim assmpath As String = ofd.FileName
                Me._seluo = New Auxiliary.CapeOpen.CapeOpenUnitOpInfo
                With Me._seluo
                    .Location = assmpath
                End With
            End If

        End Sub

        Overloads Sub InitNew()

            Dim myuo As Interfaces2.IPersistStreamInit = TryCast(_couo, Interfaces2.IPersistStreamInit)
            If Not myuo Is Nothing Then
                Try
                    myuo.InitNew()
                Catch ex As Exception
                End Try
            End If

        End Sub

        Sub Init()

            If DWSIM.App.IsRunningOnMono Then

                If Not _couo Is Nothing Then
                    Dim myuo As CapeOpen.ICapeUtilities = _couo
                    myuo.Initialize()
                    myuo.simulationContext = Me.FlowSheet
                End If

            Else

                If Not _couo Is Nothing Then
                    Dim myuo As CapeOpen.ICapeUtilities = TryCast(_couo, CapeOpen.ICapeUtilities)
                    If Not myuo Is Nothing Then
                        myuo.Initialize()
                        myuo.simulationContext = Me.FlowSheet
                    End If
                    Dim myuo2 As CapeOpen.ICapeIdentification = TryCast(_couo, CapeOpen.ICapeIdentification)
                    If Not myuo2 Is Nothing Then
                        If Not Me.GraphicObject Is Nothing Then myuo2.ComponentName = Me.GraphicObject.Tag
                        If Not Me.GraphicObject Is Nothing Then myuo2.ComponentDescription = Me.GraphicObject.Name
                    End If
                End If

            End If

        End Sub

        Sub Terminate()

            If Not _couo Is Nothing Then
                Dim myuo As CapeOpen.ICapeUtilities = TryCast(_couo, CapeOpen.ICapeUtilities)
                If Not myuo Is Nothing Then myuo.Terminate()
            End If

        End Sub

        Sub GetPorts()
            If Not _couo Is Nothing Then
                Dim myuo As CapeOpen.ICapeUnit = _couo
                Dim myports As ICapeCollection = myuo.ports
                Dim i As Integer = 0
                Dim numports As Integer = myports.Count
                If numports > 0 Then
                    For i = 1 To numports
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        _ports.Add(New UnitPort(id.ComponentName, id.ComponentDescription, myport.direction, myport.portType))
                    Next
                End If
            End If
        End Sub

        Sub GetParams()
            If Not _couo Is Nothing Then
                If _params Is Nothing Then _params = New List(Of ICapeParameter)
                '_params.Clear()
                Dim myuo As CapeOpen.ICapeUtilities = _couo
                Dim myparms As ICapeCollection = myuo.parameters
                Dim paramcount As Integer = myparms.Count
                If paramcount > 0 Then
                    Dim i As Integer = 0
                    For i = 1 To paramcount
                        Dim id As ICapeIdentification = myparms.Item(i)
                        Dim myparam As ICapeParameterSpec = myparms.Item(i)
                        Select Case myparam.Type
                            Case CapeParamType.CAPE_REAL
                                Dim ips As ICapeRealParameterSpec = CType(myparam, ICapeRealParameterSpec)
                                Dim ip As ICapeParameter = CType(myparam, ICapeParameter)
                                Dim p As New RealParameter(id.ComponentName, id.ComponentDescription, ip.value, ips.DefaultValue, ips.LowerBound, ips.UpperBound, ip.Mode, "")
                                _params.Add(p)
                            Case CapeParamType.CAPE_INT
                                Dim ips As ICapeIntegerParameterSpec = CType(myparam, ICapeIntegerParameterSpec)
                                Dim ip As ICapeParameter = CType(myparam, ICapeParameter)
                                Dim p As New IntegerParameter(id.ComponentName, id.ComponentDescription, ip.value, ips.DefaultValue, ips.LowerBound, ips.UpperBound, ip.Mode)
                                _params.Add(p)
                            Case CapeParamType.CAPE_BOOLEAN
                                Dim ips As ICapeBooleanParameterSpec = CType(myparam, ICapeBooleanParameterSpec)
                                Dim ip As ICapeParameter = CType(myparam, ICapeParameter)
                                Dim p As New BooleanParameter(id.ComponentName, id.ComponentDescription, ip.value, ips.DefaultValue, ip.Mode)
                                _params.Add(p)
                            Case CapeParamType.CAPE_OPTION
                                Dim ips As ICapeOptionParameterSpec = CType(myparam, ICapeOptionParameterSpec)
                                Dim ip As ICapeParameter = CType(myparam, ICapeParameter)
                                Dim p As New OptionParameter(id.ComponentName, id.ComponentDescription, ip.value, ips.DefaultValue, ips.OptionList, ips.RestrictedToList, ip.Mode)
                                _params.Add(p)
                            Case CapeParamType.CAPE_ARRAY
                                Dim ips As ICapeArrayParameterSpec = CType(myparam, ICapeArrayParameterSpec)
                                Dim ip As ICapeParameter = CType(myparam, ICapeParameter)
                                Dim p As New CapeArrayParameter(id.ComponentName, id.ComponentDescription, ip.value, ips.ItemsSpecifications, ips.NumDimensions)
                                _params.Add(p)
                        End Select
                    Next
                End If
            End If
        End Sub

        Sub RestorePorts()
            If Not _couo Is Nothing Then
                Dim cnobj As Object = Nothing
                Dim myuo As ICapeUnit = _couo
                Dim myports As ICapeCollection = myuo.ports
                Dim i As Integer = 0
                Dim numports As Integer = myports.Count
                If numports > 0 Then
                    For i = 1 To numports
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        For Each p As UnitPort In _ports
                            If id.ComponentName = p.ComponentName Then
                                Try
                                    cnobj = p.connectedObject
                                Catch ex As Exception
                                    cnobj = Nothing
                                End Try
                                If cnobj IsNot Nothing Then
                                    If Not Me.FlowSheet Is Nothing Then
                                        Dim mystr As DWSIM.SimulationObjects.UnitOperations.BaseClass = Me.FlowSheet.Collections.FlowsheetObjectCollection(CType(cnobj, ICapeIdentification).ComponentDescription)
                                        Try
                                            cnobj = myport.connectedObject
                                        Catch ex As Exception
                                            cnobj = Nothing
                                        End Try
                                        If Not cnobj Is Nothing Then myport.Disconnect()
                                        myport.Connect(mystr)
                                    End If
                                End If
                            End If
                        Next
                    Next
                End If
            End If
        End Sub

        Sub RestoreParams()
            If Not _couo Is Nothing Then
                Dim myuo As CapeOpen.ICapeUtilities = _couo
                Dim myparms As ICapeCollection = myuo.parameters
                Dim paramcount As Integer = myparms.Count
                If paramcount > 0 Then
                    Dim i As Integer = 0
                    For i = 1 To paramcount
                        Dim myparam As ICapeParameterSpec = myparms.Item(i)
                        Dim ip As ICapeParameter = DirectCast(myparam, ICapeParameter)
                        Try
                            If Not ip.Mode = CapeParamMode.CAPE_OUTPUT Then ip.value = _params(i - 1).value
                        Catch ex As Exception
                            'Console.WriteLine(ex.ToString)
                            'Dim ecu As CapeOpen.ECapeUser = myuo
                            'Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": CAPE-OPEN Exception: " & ecu.code & " at " & ecu.interfaceName & ". Reason: " & ecu.description, Color.DarkGray, DWSIM.Flowsheet.MessageType.Warning)
                        End Try
                    Next
                End If
            End If
        End Sub

        Sub UpdateParams()
            If Not _couo Is Nothing Then
                Dim myuo As CapeOpen.ICapeUtilities = _couo
                Dim myparms As ICapeCollection = myuo.parameters
                Dim paramcount As Integer = myparms.Count
                If paramcount > 0 Then
                    If Convert.ToInt32(paramcount) <> _params.Count Then
                        _params = New List(Of ICapeParameter)
                        GetParams()
                    End If
                    Dim i As Integer = 0
                    For i = 1 To paramcount
                        Dim myparam As ICapeParameterSpec = myparms.Item(i)
                        Dim ip As ICapeParameter = CType(myparam, ICapeParameter)
                        If Not myparam.Type = CapeParamType.CAPE_ARRAY Then
                            Try
                                _params(i - 1).value = ip.value
                            Catch ex As Exception
                                Console.WriteLine(ex.ToString)
                            End Try
                        End If
                    Next
                End If
            End If
        End Sub

        Sub CreateConnectors()
            Me.GraphicObject.InputConnectors = New List(Of ConnectionPoint)
            Me.GraphicObject.OutputConnectors = New List(Of ConnectionPoint)
            Dim nip As Integer = 1
            Dim nop As Integer = 1
            Dim objid As String = ""
            Dim i As Integer = 0
            For Each p As UnitPort In _ports
                Select Case p.direction
                    Case CapePortDirection.CAPE_INLET
                        nip += 1
                    Case CapePortDirection.CAPE_OUTLET
                        nop += 1
                End Select
            Next
            For Each p As UnitPort In _ports
                Select Case p.direction
                    Case CapePortDirection.CAPE_INLET
                        Me.GraphicObject.InputConnectors.Add(New ConnectionPoint())
                        With Me.GraphicObject.InputConnectors(Me.GraphicObject.InputConnectors.Count - 1)
                            Select Case p.portType
                                Case CapePortType.CAPE_ENERGY
                                    .Type = ConType.ConEn
                                Case CapePortType.CAPE_MATERIAL
                                    .Type = ConType.ConIn
                            End Select
                            .Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + (Me.GraphicObject.InputConnectors.Count) / (nip - 1) * Me.GraphicObject.Height / 2)
                            .ConnectorName = p.ComponentName
                        End With
                    Case CapePortDirection.CAPE_OUTLET
                        Me.GraphicObject.OutputConnectors.Add(New ConnectionPoint())
                        With Me.GraphicObject.OutputConnectors(Me.GraphicObject.OutputConnectors.Count - 1)
                            Select Case p.portType
                                Case CapePortType.CAPE_ENERGY
                                    .Type = ConType.ConEn
                                Case CapePortType.CAPE_MATERIAL
                                    .Type = ConType.ConOut
                            End Select
                            .Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + +(Me.GraphicObject.InputConnectors.Count) / (nip - 1) * Me.GraphicObject.Height / 2)
                            .ConnectorName = p.ComponentName
                        End With
                End Select
            Next
            UpdateConnectorPositions()
        End Sub

        Sub UpdateConnectors()

            ' disconnect existing connections
            For Each c As ConnectionPoint In Me.GraphicObject.InputConnectors
                If c.IsAttached Then
                    Me.FlowSheet.DisconnectObject(Me.FlowSheet.GetFlowsheetGraphicObject(c.AttachedConnector.AttachedFrom.Tag), Me.GraphicObject, False)
                End If
            Next
            For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                If c.IsAttached Then
                    Me.FlowSheet.DisconnectObject(Me.GraphicObject, Me.FlowSheet.GetFlowsheetGraphicObject(c.AttachedConnector.AttachedTo.Tag), False)
                End If
            Next

            Me.GraphicObject.InputConnectors = New List(Of ConnectionPoint)
            Me.GraphicObject.OutputConnectors = New List(Of ConnectionPoint)

            If Not _couo Is Nothing Then
                Dim cnobj As Object = Nothing
                Dim myuo As CapeOpen.ICapeUnit = _couo
                Dim myports As ICapeCollection = myuo.ports
                Dim nip As Integer = 1
                Dim nop As Integer = 1
                Dim objid As String
                Dim i As Integer = 0
                Dim numports As Integer = myports.Count
                If numports > 0 Then
                    For i = 1 To numports
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        Select Case myport.direction
                            Case CapePortDirection.CAPE_INLET
                                nip += 1
                            Case CapePortDirection.CAPE_OUTLET
                                nop += 1
                        End Select
                    Next
                    For i = 1 To numports
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        Select Case myport.direction
                            Case CapePortDirection.CAPE_INLET
                                Me.GraphicObject.InputConnectors.Add(New ConnectionPoint())
                                With Me.GraphicObject.InputConnectors(Me.GraphicObject.InputConnectors.Count - 1)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_ENERGY
                                            .Type = ConType.ConEn
                                        Case CapePortType.CAPE_MATERIAL
                                            .Type = ConType.ConIn
                                    End Select
                                    .Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + (Me.GraphicObject.InputConnectors.Count) / (nip - 1) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Try
                                    cnobj = myport.connectedObject
                                Catch ex As Exception
                                    cnobj = Nothing
                                End Try
                                If cnobj IsNot Nothing Then
                                    objid = CType(cnobj, ICapeIdentification).ComponentDescription
                                    myport.Disconnect()
                                    Dim gobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByName(objid, Me.FlowSheet.FormSurface.FlowsheetDesignSurface)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_MATERIAL
                                            Me.FlowSheet.ConnectObject(gobj, Me.GraphicObject, 0, Me.GraphicObject.InputConnectors.Count - 1)
                                        Case CapePortType.CAPE_ENERGY
                                            Me.FlowSheet.ConnectObject(gobj, Me.GraphicObject, 0, Me.GraphicObject.InputConnectors.Count - 1)
                                    End Select
                                    myport.Connect(Me.FlowSheet.GetFlowsheetSimulationObject(gobj.Tag))
                                End If
                            Case CapePortDirection.CAPE_OUTLET
                                Me.GraphicObject.OutputConnectors.Add(New ConnectionPoint())
                                With Me.GraphicObject.OutputConnectors(Me.GraphicObject.OutputConnectors.Count - 1)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_ENERGY
                                            .Type = ConType.ConEn
                                        Case CapePortType.CAPE_MATERIAL
                                            .Type = ConType.ConOut
                                    End Select
                                    .Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + (Me.GraphicObject.OutputConnectors.Count) / (nop - 1) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Try
                                    cnobj = myport.connectedObject
                                Catch ex As Exception
                                    cnobj = Nothing
                                End Try
                                If cnobj IsNot Nothing Then
                                    objid = CType(cnobj, ICapeIdentification).ComponentDescription
                                    myport.Disconnect()
                                    Dim gobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByName(objid, Me.FlowSheet.FormSurface.FlowsheetDesignSurface)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_MATERIAL
                                            Me.FlowSheet.ConnectObject(Me.GraphicObject, gobj, Me.GraphicObject.OutputConnectors.Count - 1, 0)
                                        Case CapePortType.CAPE_ENERGY
                                            Me.FlowSheet.ConnectObject(Me.GraphicObject, gobj, Me.GraphicObject.OutputConnectors.Count - 1, 0)
                                    End Select
                                    myport.Connect(Me.FlowSheet.GetFlowsheetSimulationObject(gobj.Tag))
                                End If
                        End Select
                    Next
                End If
                UpdateConnectorPositions()
            End If
        End Sub

        Sub UpdateConnectors2()

            'called only when loading simulation from XML file.

            If Not _couo Is Nothing Then
                Dim myuo As CapeOpen.ICapeUnit = _couo
                Dim myports As ICapeCollection = myuo.ports
                Dim nip As Integer = 0
                Dim nop As Integer = 0
                Dim ic, oc As Integer
                Dim i As Integer = 0
                Dim numports As Integer = myports.Count
                If numports > 0 Then
                    For i = 1 To numports
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        Select Case myport.direction
                            Case CapePortDirection.CAPE_INLET
                                nip += 1
                            Case CapePortDirection.CAPE_OUTLET
                                nop += 1
                        End Select
                    Next
                    ic = 0
                    oc = 0
                    For i = 1 To nip + nop
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        Select Case myport.direction
                            Case CapePortDirection.CAPE_INLET
                                With Me.GraphicObject.InputConnectors(ic)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_ENERGY
                                            .Type = ConType.ConEn
                                        Case CapePortType.CAPE_MATERIAL
                                            .Type = ConType.ConIn
                                    End Select
                                    .Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + (Me.GraphicObject.InputConnectors.Count) / (nip) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Dim gobj As GraphicObject = Me.GraphicObject.InputConnectors(ic).AttachedConnector.AttachedFrom
                                myport.Connect(Me.FlowSheet.GetFlowsheetSimulationObject(gobj.Tag))
                                ic += 1
                            Case CapePortDirection.CAPE_OUTLET
                                With Me.GraphicObject.OutputConnectors(oc)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_ENERGY
                                            .Type = ConType.ConEn
                                        Case CapePortType.CAPE_MATERIAL
                                            .Type = ConType.ConOut
                                    End Select
                                    .Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + (Me.GraphicObject.OutputConnectors.Count) / (nop) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Dim gobj As GraphicObject = Me.GraphicObject.OutputConnectors(oc).AttachedConnector.AttachedTo
                                myport.Connect(Me.FlowSheet.GetFlowsheetSimulationObject(gobj.Tag))
                                oc += 1
                        End Select
                    Next
                End If
                UpdateConnectorPositions()
            End If
        End Sub

        Sub UpdateConnectorPositions()
            Dim i As Integer = 0
            Dim obj1(Me.GraphicObject.InputConnectors.Count), obj2(Me.GraphicObject.InputConnectors.Count) As Double
            Dim obj3(Me.GraphicObject.OutputConnectors.Count), obj4(Me.GraphicObject.OutputConnectors.Count) As Double
            For Each ic As ConnectionPoint In Me.GraphicObject.InputConnectors
                obj1(i) = -Me.GraphicObject.X + ic.Position.X
                obj2(i) = -Me.GraphicObject.Y + ic.Position.Y
                i = i + 1
            Next
            i = 0
            For Each oc As ConnectionPoint In Me.GraphicObject.OutputConnectors
                obj3(i) = -Me.GraphicObject.X + oc.Position.X
                obj4(i) = -Me.GraphicObject.Y + oc.Position.Y
                i = i + 1
            Next
            Me.GraphicObject.AdditionalInfo = New Object() {obj1, obj2, obj3, obj4}
        End Sub

        Sub UpdatePorts()
            If Not _couo Is Nothing Then
                Dim cnobj As Object = Nothing
                Dim myuo As CapeOpen.ICapeUnit = _couo
                Dim myports As ICapeCollection = myuo.ports
                Dim i As Integer = 0
                Dim numports As Integer = myports.Count
                _ports.Clear()
                If numports > 0 Then
                    For i = 1 To numports
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        _ports.Add(New UnitPort(id.ComponentName, id.ComponentDescription, myport.direction, myport.portType))
                        Try
                            cnobj = myport.connectedObject
                        Catch ex As Exception
                            cnobj = Nothing
                        End Try
                        Try
                            If Not cnobj Is Nothing Then
                                _ports(_ports.Count - 1).Connect(Me.FlowSheet.Collections.FlowsheetObjectCollection(CType(cnobj, ICapeIdentification).ComponentDescription))
                            End If
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = myuo
                            Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": CAPE-OPEN Exception: " & ecu.code & " at " & ecu.interfaceName & ". Reason: " & ecu.description, Color.DarkGray, DWSIM.Flowsheet.MessageType.Warning)
                        End Try
                    Next
                End If
            End If
        End Sub

        Sub UpdatePortsFromConnectors()

            Dim cnobj As Object = Nothing
            For Each c As ConnectionPoint In Me.GraphicObject.InputConnectors
                For Each p As UnitPort In _ports
                    Try
                        cnobj = p.connectedObject
                    Catch ex As Exception
                        cnobj = Nothing
                    End Try
                    If c.ConnectorName = p.ComponentName Then
                        If Not c.IsAttached Then
                            p.Disconnect()
                        ElseIf c.IsAttached And cnobj Is Nothing Then
                            p.Connect(Me.FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedFrom.Name))
                        End If
                    End If
                Next
            Next

            For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                For Each p As UnitPort In _ports
                    Try
                        cnobj = p.connectedObject
                    Catch ex As Exception
                        cnobj = Nothing
                    End Try
                    If c.ConnectorName = p.ComponentName Then
                        If Not c.IsAttached Then
                            p.Disconnect()
                        ElseIf c.IsAttached And cnobj Is Nothing Then
                            p.Connect(Me.FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedTo.Name))
                        End If
                    End If
                Next
            Next

            RestorePorts()

        End Sub

        Function Edit(ByVal sender As Object, ByVal e As System.EventArgs) As Object
            If Not _couo Is Nothing Then
                Dim myuo As CapeOpen.ICapeUtilities = _couo
                RestorePorts()
                Try
                    'set reaction set, if supported
                    If Not TryCast(_couo, ICapeKineticReactionContext) Is Nothing Then
                        Me.FlowSheet.Options.ReactionSets(Me.ReactionSetID).simulationContext = Me.FlowSheet
                        Dim myset As Object = CType(Me.FlowSheet.Options.ReactionSets(Me.ReactionSetID), Object)
                        Dim myruo As CAPEOPEN110.ICapeKineticReactionContext = _couo
                        myruo.SetReactionObject(myset)
                    End If
                    myuo.Edit()
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = myuo
                    Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": CAPE-OPEN Exception: " & ecu.code & " at " & ecu.interfaceName & ". Reason: " & ecu.description, Color.DarkGray, DWSIM.Flowsheet.MessageType.Warning)
                    Throw
                End Try
                UpdateParams()
                UpdatePorts()
                UpdateConnectors()
                Me.FlowSheet.FormSurface.UpdateSelectedObject()
            End If
            Return "click to show ->"
        End Function

#End Region

#Region "    DWSIM Specifics"

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Dim info As XElement = (From el As XElement In data Select el Where el.Name = "CAPEOPEN_Object_Info").SingleOrDefault
            _seluo = New Auxiliary.CapeOpen.CapeOpenUnitOpInfo
            _seluo.LoadData(info.Elements.ToList)

            If Not DWSIM.App.IsRunningOnMono Then

                If Not _seluo Is Nothing Then
                    Try

                        Dim t As Type = Type.GetTypeFromProgID(_seluo.TypeName)
                        _couo = Activator.CreateInstance(t)

                        InitNew()
                        Init()

                        Dim pdata As XElement = (From el As XElement In data Select el Where el.Name = "PersistedData").SingleOrDefault

                        If Not pdata Is Nothing Then
                            _istr = New DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream(Convert.FromBase64String(pdata.Value)))
                            PersistLoad(Nothing)
                        End If

                        Dim paramdata As XElement = (From el As XElement In data Select el Where el.Name = "ParameterData").SingleOrDefault
                        Dim b As New BinaryFormatter, m As New MemoryStream()
                        _params = b.Deserialize(New MemoryStream(Convert.FromBase64String(paramdata.Value)))

                        RestoreParams()
                        GetPorts()

                    Catch ex As Exception

                        Me.FlowSheet.WriteToLog("Error creating CAPE-OPEN Unit Operation: " & ex.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)

                    End Try
                End If

            Else

                If Not _seluo Is Nothing Then

                    Dim myt As Type = Nothing

                    If Not File.Exists(_seluo.Location) Then
                        ShowFormMono()
                    End If

                    myt = GetManagedUnitType(_seluo.Location)

                    _couo = Activator.CreateInstance(myt)

                    Try
                        With _seluo
                            .Name = CType(_couo, ICapeIdentification).ComponentName
                            .Description = CType(_couo, ICapeIdentification).ComponentDescription
                            .TypeName = myt.Name
                            .Version = myt.Module.Assembly.GetName.Version.ToString
                        End With
                    Catch ex As Exception

                    End Try

                    InitNew()
                    Init()

                    Dim pdata As XElement = (From el As XElement In data Select el Where el.Name = "PersistedData").SingleOrDefault
                    _istr = New DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream(Convert.FromBase64String(pdata.Value)))
                    PersistLoad(Nothing)

                    Dim paramdata As XElement = (From el As XElement In data Select el Where el.Name = "ParameterData").SingleOrDefault
                    Dim b As New BinaryFormatter, m As New MemoryStream()
                    _params = b.Deserialize(New MemoryStream(Convert.FromBase64String(paramdata.Value)))

                    RestoreParams()
                    GetPorts()

                End If

            End If

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()
            With elements
                .Add(New XElement("CAPEOPEN_Object_Info", _seluo.SaveData().ToArray))
                Me.PersistSave(Nothing)
                If Not _istr Is Nothing Then .Add(New XElement("PersistedData", Convert.ToBase64String(CType(_istr.baseStream, MemoryStream).ToArray())))
                Dim b As New BinaryFormatter, m As New MemoryStream()
                b.Serialize(m, _params)
                .Add(New XElement("ParameterData", Convert.ToBase64String(m.ToArray)))
            End With

            Return elements

        End Function

        ''' <summary>
        ''' Saves the current UO data in a temporary placeholder, so it can be loaded by another thread which will
        ''' reinstantiate the COM object because of interop restrictions.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub SaveTempData()
            _tempdata = SaveData()
        End Sub

        Public Property ReactionSetID() As String
            Get
                Return Me.m_reactionSetID
            End Get
            Set(ByVal value As String)
                Me.m_reactionSetID = value
            End Set
        End Property

        Public Property ReactionSetName() As String
            Get
                Return Me.m_reactionSetName
            End Get
            Set(ByVal value As String)
                Me.m_reactionSetName = value
            End Set
        End Property

        Public Property RecalcOutputStreams() As Boolean
            Get
                Return _recalculateoutputstreams
            End Get
            Set(ByVal value As Boolean)
                _recalculateoutputstreams = value
            End Set
        End Property

        Function isCOUnit(ByVal t As Type)
            Dim interfaceTypes As New List(Of Type)(t.GetInterfaces())
            Return (interfaceTypes.Contains(GetType(CapeOpen.ICapeUnit)))
        End Function

        Function GetManagedUnitType(ByVal filepath As String) As Type

            Dim mya As Assembly = Assembly.LoadFile(filepath)
            Dim availableTypes As New List(Of Type)()
            availableTypes.AddRange(mya.GetTypes())

            Dim tl As List(Of Type) = availableTypes.FindAll(AddressOf isCOUnit)

            Dim myt As Type = Nothing
            For Each t As Type In tl
                If Not t.IsAbstract Then
                    myt = t
                    Exit For
                End If
            Next

            Return myt

        End Function

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            'If Not CreatedWithThreadID = Thread.CurrentThread.ManagedThreadId Then
            '    disposedValue = False
            '    Me.Dispose(True)
            '    'load current configuration from temporary data and re-instantiate the COM object using the current thread.
            '    'this is called only when solving the object with a background thread.
            '    Me.LoadData(_tempdata)
            '    CreatedWithThreadID = Thread.CurrentThread.ManagedThreadId
            'End If

            UpdatePortsFromConnectors()

            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

            If Not _couo Is Nothing Then
                For Each c As ConnectionPoint In Me.GraphicObject.InputConnectors
                    If c.IsAttached And c.Type = ConType.ConIn Then
                        Dim mat As Streams.MaterialStream = Me.FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedFrom.Name)
                        mat.SetFlowsheet(Me.FlowSheet)
                    End If
                Next
                Dim myuo As CapeOpen.ICapeUnit = _couo
                Dim msg As String = ""
                Try
                    'set reaction set, if supported
                    If Not TryCast(_couo, ICapeKineticReactionContext) Is Nothing Then
                        Me.FlowSheet.Options.ReactionSets(Me.ReactionSetID).simulationContext = Me.FlowSheet
                        Dim myset As Object = CType(Me.FlowSheet.Options.ReactionSets(Me.ReactionSetID), Object)
                        Dim myruo As CAPEOPEN110.ICapeKineticReactionContext = _couo
                        myruo.SetReactionObject(myset)
                    End If
                Catch ex As Exception
                    With objargs
                        .Calculated = False
                        .Name = Me.Name
                        .Tag = Me.GraphicObject.Tag
                        .ObjectType = ObjectType.CapeOpenUO
                    End With
                    For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                        If c.Type = ConType.ConEn Then
                            If c.IsAttached Then c.AttachedConnector.AttachedTo.Calculated = False
                        End If
                    Next
                    Dim ecu As CapeOpen.ECapeUser = myuo
                    Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & ":" & ecu.scope & ". Reason: " & ecu.description, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                End Try
                'My.Application.ActiveSimulation = Me.FlowSheet
                myuo.Validate(msg)
                If myuo.ValStatus = CapeValidationStatus.CAPE_VALID Then
                    Try
                        For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.IsAttached And c.Type = ConType.ConOut Then
                                Dim mat As Streams.MaterialStream = Me.FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedTo.Name)
                                mat.ClearAllProps()
                            End If
                        Next
                        RestorePorts()
                        myuo.Calculate()
                        UpdateParams()
                        For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.IsAttached And c.Type = ConType.ConOut Then
                                Dim mat As Streams.MaterialStream = Me.FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedTo.Name)
                                mat.PropertyPackage.CurrentMaterialStream = mat
                                For Each subst As Compound In mat.Phases(0).Compounds.Values
                                    subst.FracaoMassica = mat.PropertyPackage.AUX_CONVERT_MOL_TO_MASS(subst.Name, 0)
                                Next
                            End If
                        Next
                        'Call function to calculate flowsheet
                        With objargs
                            .Calculated = True
                            .Name = Me.Name
                            .Tag = Me.GraphicObject.Tag
                            .ObjectType = ObjectType.CapeOpenUO
                        End With
                        For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConEn And c.IsAttached Then
                                c.AttachedConnector.AttachedTo.Calculated = True
                            End If
                        Next
                        If DWSIM.App.IsMainThread Then
                            Dim ur As CapeOpen.ICapeUnitReport = _couo
                            If Not ur Is Nothing Then
                                Dim reps As String() = ur.reports
                                For Each r As String In reps
                                    ur.selectedReport = r
                                    Dim msg2 As String = ""
                                    ur.ProduceReport(msg2)
                                    Me.FlowSheet.FormCOReports.TextBox1.AppendText(Date.Now.ToString + ", " +
                                                                                    Me.GraphicObject.Tag + " (" + r + "):" +
                                                                                    vbCrLf + vbCrLf + msg2 + vbCrLf + vbCrLf)
                                Next
                            End If
                        End If
                    Catch ex As Exception
                        With objargs
                            .Calculated = False
                            .Name = Me.Name
                            .Tag = Me.GraphicObject.Tag
                            .ObjectType = ObjectType.CapeOpenUO
                        End With
                        For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConEn Then
                                If c.IsAttached Then c.AttachedConnector.AttachedTo.Calculated = False
                            End If
                        Next
                        Dim ecu As CapeOpen.ECapeUser = myuo
                        Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & ":" & ecu.scope & ". Reason: " & ecu.description, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                    End Try
                Else
                    Me.FlowSheet.WriteToLog(Me.GraphicObject.Tag + ": CO Unit not validated. Reason: " + msg, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                    'Call function to calculate flowsheet
                    With objargs
                        .Calculated = False
                        .Name = Me.Name
                        .Tag = Me.GraphicObject.Tag
                        .ObjectType = ObjectType.CapeOpenUO
                    End With
                    For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                        If c.Type = ConType.ConEn Then
                            If c.IsAttached Then c.AttachedConnector.AttachedTo.Calculated = False
                        End If
                    Next
                End If
            End If

            FlowSheet.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            With objargs
                .Calculated = False
                .Name = Me.Name
                .Tag = Me.GraphicObject.Tag
                .ObjectType = ObjectType.CapeOpenUO
            End With

            Try
                FlowSheet.CalculationQueue.Enqueue(objargs)
            Catch ex As Exception

            End Try

        End Function

        Public Overrides Sub Validate()
            MyBase.Validate()
        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

            UpdatePortsFromConnectors()

            Dim Conversor As New DWSIM.SystemsOfUnits.Converter

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                'identify
                .Item.Add("Name", _seluo.Name, True, "1. CAPE-OPEN Object Info", "", True)
                .Item.Add("Description", _seluo.Description, True, "1. CAPE-OPEN Object Info", "", True)
                .Item.Add("ProgID", _seluo.TypeName, True, "1. CAPE-OPEN Object Info", "", True)
                .Item.Add("Version", _seluo.Version, True, "1. CAPE-OPEN Object Info", "", True)
                .Item.Add("CAPE-OPEN Version", _seluo.CapeVersion, True, "1. CAPE-OPEN Object Info", "", True)
                .Item.Add("File Location", _seluo.Location, True, "1. CAPE-OPEN Object Info", "", True)
                .Item.Add("Vendor URL", _seluo.VendorURL, True, "1. CAPE-OPEN Object Info", "", True)
                '.Item.Add("Help URL", _seluo.HelpURL, True, "1. CAPE-OPEN Object Info", "", True)

                'show edit form if available
                .Item.Add("Editing Form", "click to show ->", False, "2. Editing Form", "", True)
                .Item(.Item.Count - 1).OnClick = AddressOf Me.Edit

                Dim cnobj As Object = Nothing

                'populate ports
                For Each p As UnitPort In _ports
                    If p.portType = CapePortType.CAPE_MATERIAL Then
                        Dim tag As String = ""
                        Try
                            cnobj = p.connectedObject
                        Catch ex As Exception
                            cnobj = Nothing
                        End Try
                        Dim conobj As DWSIM.SimulationObjects.Streams.MaterialStream = cnobj
                        If Not conobj Is Nothing Then tag = conobj.GraphicObject.Tag
                        .Item.Add(p.ComponentName + " [" + p.direction.ToString + ", " + p.portType.ToString() + "]", tag, False, "3. Ports", p.ComponentDescription, True)
                        With .Item(.Item.Count - 1)
                            .Tag = _ports.IndexOf(p)
                            If p.direction = CapePortDirection.CAPE_INLET Then
                                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                            Else
                                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                            End If
                        End With
                    ElseIf p.portType = CapePortType.CAPE_ENERGY Then
                        Dim tag As String = ""
                        Try
                            cnobj = p.connectedObject
                        Catch ex As Exception
                            cnobj = Nothing
                        End Try
                        Dim conobj As DWSIM.SimulationObjects.Streams.EnergyStream = cnobj
                        If Not conobj Is Nothing Then tag = conobj.GraphicObject.Tag
                        .Item.Add(p.ComponentName + " [" + p.direction.ToString + ", " + p.portType.ToString() + "]", tag, False, "3. Ports", p.ComponentDescription, True)
                        With .Item(.Item.Count - 1)
                            .Tag = _ports.IndexOf(p)
                            If p.direction = CapePortDirection.CAPE_INLET Then
                                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
                            Else
                                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
                            End If
                        End With
                    End If
                Next

                .Item.Add("Shape Override", Me.GraphicObject, "ShapeOverride", False, "4. Parameters", "Overrides the graphical representation of the object in the Flowsheet.", True)
                .Item.Add("Recalculate Output Streams", Me, "RecalcOutputStreams", False, "4. Parameters", "Recalculate output streams using the selected property package.", True)

                If Not TryCast(_couo, ICapeKineticReactionContext) Is Nothing Then
                    .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), FlowSheet.Options.ReactionSets(Me.ReactionSetID).Name, False, "4. Parameters", DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
                    With .Item(.Item.Count - 1)
                        .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
                        .IsDropdownResizable = True
                    End With
                End If

                'populate parameters
                For Each p As Object In _params
                    Dim id As String = ""
                    Dim desc As String = ""
                    id = CType(p, ICapeIdentification).ComponentName
                    desc = CType(p, ICapeIdentification).ComponentDescription
                    'find parameter type
                    Dim myp As ICapeParameterSpec = TryCast(p, ICapeParameterSpec)
                    Select Case myp.Type
                        Case CapeParamType.CAPE_ARRAY
                            Dim par As CapeArrayParameter = p
                            Dim m2 As New PropertyGridEx.CustomPropertyCollection()
                            Dim i As Integer = 0
                            For Each o In DirectCast(par.value, System.Array)
                                m2.Add(i.ToString, o, True, "", "", True)
                                m2.Item(m2.Count - 1).IsReadOnly = True
                                i += 1
                            Next
                            .Item.Add(id, m2, If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                            With .Item(.Item.Count - 1)
                                .IsReadOnly = True
                                .IsBrowsable = True
                                .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                                .CustomEditor = New System.Drawing.Design.UITypeEditor
                            End With
                        Case CapeParamType.CAPE_BOOLEAN
                            Dim par As BooleanParameter = TryCast(p, BooleanParameter)
                            .Item.Add(id, par, "Value", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                            .Item(.Item.Count - 1).Tag2 = id
                        Case CapeParamType.CAPE_INT
                            Dim par As IntegerParameter = TryCast(p, IntegerParameter)
                            .Item.Add(id, par, "Value", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                            .Item(.Item.Count - 1).Tag2 = id
                        Case CapeParamType.CAPE_OPTION
                            Dim par As OptionParameter = TryCast(p, OptionParameter)
                            .Item.Add(id, par, "Value", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                            .Item(.Item.Count - 1).Tag2 = id
                            With .Item(.Item.Count - 1)
                                If Not par.OptionList Is Nothing Then .Choices = New PropertyGridEx.CustomChoices(par.OptionList, False)
                            End With
                        Case CapeParamType.CAPE_REAL
                            Dim par As RealParameter = TryCast(p, RealParameter)
                            .Item.Add(id, par, "SIValue", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                            .Item(.Item.Count - 1).Tag2 = id
                    End Select
                Next

                If Me.GraphicObject.Calculated = False Then
                    .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), Me, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea5"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                    With .Item(.Item.Count - 1)
                        .DefaultType = GetType(System.String)
                    End With
                End If

            End With

        End Sub

        Public Overrides Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

            MyBase.PropertyValueChanged(s, e)

            If e.ChangedItem.Parent.Label.Contains("Parameters") Then

                RestoreParams()

            ElseIf e.ChangedItem.Parent.Label.Contains("Ports") Then
                Dim index, indexc, i As Integer
                i = 0
                For Each gi As GridItem In e.ChangedItem.Parent.GridItems
                    If gi.Label = e.ChangedItem.Label Then
                        index = i
                        Exit For
                    End If
                    i += 1
                Next
                If e.ChangedItem.Label.Contains("[CAPE_INLET, CAPE_MATERIAL]") Then
                    For Each p As UnitPort In _ports
                        i = 0
                        If e.ChangedItem.Label.Contains(p.ComponentName) Then
                            For Each c As ConnectionPoint In Me.GraphicObject.InputConnectors
                                If p.ComponentName = c.ConnectorName And _
                                p.direction = CapePortDirection.CAPE_INLET And _
                                p.portType = CapePortType.CAPE_MATERIAL Then
                                    indexc = i
                                    Exit For
                                End If
                                i += 1
                            Next
                        End If
                    Next
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                            Dim oguid As String = Me.FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, Me.GraphicObject.X - 40, Me.GraphicObject.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Me.GraphicObject.InputConnectors(indexc).IsAttached Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, Me.GraphicObject)
                            _ports(index).Disconnect()
                        End If
                        Me.FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), Me.GraphicObject, 0, indexc)
                        _ports(index).Connect(Me.FlowSheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                    Else
                        If e.OldValue.ToString <> "" Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, Me.GraphicObject)
                            _ports(index).Disconnect()
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Contains("[CAPE_OUTLET, CAPE_MATERIAL]") Then
                    For Each p As UnitPort In _ports
                        i = 0
                        If e.ChangedItem.Label.Contains(p.ComponentName) Then
                            For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                                If p.ComponentName = c.ConnectorName And _
                                p.direction = CapePortDirection.CAPE_OUTLET And _
                                p.portType = CapePortType.CAPE_MATERIAL Then
                                    indexc = i
                                    Exit For
                                End If
                                i += 1
                            Next
                        End If
                    Next
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                            Dim oguid As String = Me.FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, Me.GraphicObject.X + Me.GraphicObject.Width + 40, Me.GraphicObject.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Me.GraphicObject.OutputConnectors(indexc).IsAttached Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject, Me.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                            _ports(index).Disconnect()
                        End If
                        Me.FlowSheet.ConnectObject(Me.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), indexc, 0)
                        _ports(index).Connect(Me.FlowSheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                    Else
                        If e.OldValue.ToString <> "" Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject, Me.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                            _ports(index).Disconnect()
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Contains("[CAPE_INLET, CAPE_ENERGY]") Then
                    For Each p As UnitPort In _ports
                        i = 0
                        If e.ChangedItem.Label.Contains(p.ComponentName) Then
                            For Each c As ConnectionPoint In Me.GraphicObject.InputConnectors
                                If p.ComponentName = c.ConnectorName And _
                                p.direction = CapePortDirection.CAPE_INLET And _
                                p.portType = CapePortType.CAPE_ENERGY Then
                                    indexc = i
                                    Exit For
                                End If
                                i += 1
                            Next
                        End If
                    Next
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                            Dim oguid As String = Me.FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, Me.GraphicObject.X - 40, Me.GraphicObject.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Me.GraphicObject.InputConnectors(indexc).IsAttached Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, Me.GraphicObject)
                            _ports(index).Disconnect()
                        End If
                        Me.FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), Me.GraphicObject, 0, indexc)
                        _ports(index).Connect(Me.FlowSheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                    Else
                        If e.OldValue.ToString <> "" Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, Me.GraphicObject)
                            _ports(index).Disconnect()
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Contains("[CAPE_OUTLET, CAPE_ENERGY]") Then
                    For Each p As UnitPort In _ports
                        i = 0
                        If e.ChangedItem.Label.Contains(p.ComponentName) Then
                            For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                                If p.ComponentName = c.ConnectorName And _
                                p.direction = CapePortDirection.CAPE_OUTLET And _
                                p.portType = CapePortType.CAPE_ENERGY Then
                                    indexc = i
                                    Exit For
                                End If
                                i += 1
                            Next
                        End If
                    Next
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                            Dim oguid As String = Me.FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, Me.GraphicObject.X + Me.GraphicObject.Width + 40, Me.GraphicObject.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Me.GraphicObject.OutputConnectors(indexc).IsAttached Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject, Me.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                            _ports(index).Disconnect()
                        End If
                        Me.FlowSheet.ConnectObject(Me.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Me.FlowSheet.FormSurface.FlowsheetDesignSurface), indexc, 0)
                        _ports(index).Connect(Me.FlowSheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                    Else
                        If e.OldValue.ToString <> "" Then
                            Me.FlowSheet.DisconnectObject(Me.GraphicObject, Me.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                            _ports(index).Disconnect()
                        End If
                    End If
                End If

                UpdateConnectorPositions()

            End If

        End Sub

        Public Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
            If Not Me._params Is Nothing Then
                Dim props As New ArrayList
                For Each p As ICapeIdentification In Me._params
                    props.Add(p.ComponentName)
                Next
                Return props.ToArray(Type.GetType("System.String"))
            Else
                Return New String() {Nothing}
            End If
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            Return ""
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If Not Me._params Is Nothing Then
                For Each p As ICapeIdentification In Me._params
                    If p.ComponentName = prop Then
                        Return CType(p, ICapeParameter).value
                        Exit Function
                    End If
                Next
                Return Nothing
            Else
                Return Nothing
            End If
        End Function

        Public Overrides Sub QTFillNodeItems()
            Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Extras.NodeItem)
            If Not _seluo Is Nothing Then
                With _seluo
                    Me.QTNodeTableItems.Add(0, New Extras.NodeItem("Name", .Name, "", 0, 0, Nothing))
                    Me.QTNodeTableItems.Add(1, New Extras.NodeItem("Type Name", .TypeName, "", 1, 0, Nothing))
                    Me.QTNodeTableItems.Add(2, New Extras.NodeItem("Version", .Version, "", 2, 0, Nothing))
                End With
            End If
        End Sub

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            For Each p As ICapeIdentification In Me._params
                If p.ComponentName = prop Then
                    CType(p, ICapeParameter).value = propval
                    RestoreParams()
                    Return 1
                End If
            Next
            Return 0
        End Function

        Public Overrides Sub UpdatePropertyNodes(ByVal su As SystemsOfUnits.Units, ByVal nf As String)

            Me.QTFillNodeItems()

        End Sub

#End Region

#Region "    IDisposable Overload"

        Protected Overrides Sub Dispose(ByVal disposing As Boolean)
            ' Check to see if Dispose has already been called.
            If Not Me.disposedValue Then
                ' If disposing equals true, dispose all managed 
                ' and unmanaged resources.
                If disposing Then
                    If _ports IsNot Nothing Then _ports.Clear()
                    If _params IsNot Nothing Then _params.Clear()
                    If _parameters IsNot Nothing Then _parameters.Clear()
                End If

                ' Call the appropriate methods to clean up 
                ' unmanaged resources here.
                ' If disposing is false, 
                ' only the following code is executed.
                If Not _couo Is Nothing Then
                    Terminate()
                    If Marshal.IsComObject(_couo) Then Marshal.ReleaseComObject(_couo)
                End If

                ' Note disposing has been done.
                disposedValue = True

            End If
        End Sub

#End Region

    End Class

End Namespace

Namespace DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen

    <System.Serializable()> Public Class CapeOpenUnitOpInfo

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public TypeName As String = ""
        Public Version As String = ""
        Public Description As String = ""
        Public HelpURL As String = ""
        Public VendorURL As String = ""
        Public AboutInfo As String = ""
        Public CapeVersion As String = ""
        Public Location As String = ""
        Public Name As String = ""
        Public ImplementedCategory As String = ""

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

    ' System.Drawing.ComIStreamWrapper.cs
    '
    ' Author:
    ' Kornél Pál <http://www.kornelpal.hu/>
    '
    ' Copyright (C) 2005-2008 Kornél Pál
    '

    '
    ' Permission is hereby granted, free of charge, to any person obtaining
    ' a copy of this software and associated documentation files (the
    ' "Software"), to deal in the Software without restriction, including
    ' without limitation the rights to use, copy, modify, merge, publish,
    ' distribute, sublicense, and/or sell copies of the Software, and to
    ' permit persons to whom the Software is furnished to do so, subject to
    ' the following conditions:
    '
    ' The above copyright notice and this permission notice shall be
    ' included in all copies or substantial portions of the Software.
    '
    ' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    ' EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    ' MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    ' NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    ' LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    ' OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    ' WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
    '
    <System.Serializable()> Friend NotInheritable Class ComIStreamWrapper

        Implements IStream

        Private Const STG_E_INVALIDFUNCTION As Integer = &H80030001

        Public ReadOnly baseStream As Stream
        Private position As Long = -1

        Friend Sub New(ByVal stream As Stream)
            baseStream = stream
        End Sub

        Private Sub SetSizeToPosition()
            If position <> -1 Then
                If position > baseStream.Length Then
                    baseStream.SetLength(position)
                End If
                baseStream.Position = position
                position = -1
            End If
        End Sub

        Public Sub Read(ByVal pv As Byte(), ByVal cb As Integer, ByVal pcbRead As IntPtr) Implements IStream.Read
            Dim read__1 As Integer = 0

            If cb <> 0 Then
                SetSizeToPosition()
                read__1 = baseStream.Read(pv, 0, cb)
            End If

            If pcbRead <> IntPtr.Zero Then
                Marshal.WriteInt32(pcbRead, read__1)
            End If
        End Sub

        Public Sub Write(ByVal pv As Byte(), ByVal cb As Integer, ByVal pcbWritten As IntPtr) Implements IStream.Write
            If cb <> 0 Then
                SetSizeToPosition()
                baseStream.Write(pv, 0, cb)
            End If

            If pcbWritten <> IntPtr.Zero Then
                Marshal.WriteInt32(pcbWritten, cb)
            End If
        End Sub

        Public Sub Seek(ByVal dlibMove As Long, ByVal dwOrigin As Integer, ByVal plibNewPosition As IntPtr) Implements IStream.Seek
            Dim length As Long = baseStream.Length
            Dim newPosition As Long

            Select Case CType(dwOrigin, SeekOrigin)
                Case SeekOrigin.Begin
                    newPosition = dlibMove
                    Exit Select
                Case SeekOrigin.Current
                    If position = -1 Then
                        newPosition = baseStream.Position + dlibMove
                    Else
                        newPosition = position + dlibMove
                    End If
                    Exit Select
                Case SeekOrigin.[End]
                    newPosition = length + dlibMove
                    Exit Select
                Case Else
                    Throw New ExternalException(Nothing, STG_E_INVALIDFUNCTION)
            End Select

            If newPosition > length Then
                position = newPosition
            Else
                baseStream.Position = newPosition
                position = -1
            End If

            If plibNewPosition <> IntPtr.Zero Then
                Marshal.WriteInt64(plibNewPosition, newPosition)
            End If
        End Sub

        Public Sub SetSize(ByVal libNewSize As Long) Implements IStream.SetSize
            baseStream.SetLength(libNewSize)
        End Sub

        Public Sub CopyTo(ByVal pstm As IStream, ByVal cb As Long, ByVal pcbRead As IntPtr, ByVal pcbWritten As IntPtr) Implements IStream.CopyTo
            Dim buffer As Byte()
            Dim written As Long = 0
            Dim read As Integer
            Dim count As Integer

            If cb <> 0 Then
                If cb < 4096 Then
                    count = Convert.ToInt32(cb)
                Else
                    count = 4096
                End If
                buffer = New Byte(count - 1) {}
                SetSizeToPosition()
                While True
                    If (InlineAssignHelper(read, baseStream.Read(buffer, 0, count))) = 0 Then
                        Exit While
                    End If
                    pstm.Write(buffer, read, IntPtr.Zero)
                    written += read
                    If written >= cb Then
                        Exit While
                    End If
                    If cb - written < 4096 Then
                        count = Convert.ToInt32(cb - written)
                    End If
                End While
            End If

            If pcbRead <> IntPtr.Zero Then
                Marshal.WriteInt64(pcbRead, written)
            End If
            If pcbWritten <> IntPtr.Zero Then
                Marshal.WriteInt64(pcbWritten, written)
            End If
        End Sub

        Public Sub Commit(ByVal grfCommitFlags As Integer) Implements IStream.Commit
            baseStream.Flush()
            SetSizeToPosition()
        End Sub

        Public Sub Revert() Implements IStream.Revert
            Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
        End Sub

        Public Sub LockRegion(ByVal libOffset As Long, ByVal cb As Long, ByVal dwLockType As Integer) Implements IStream.LockRegion
            Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
        End Sub

        Public Sub UnlockRegion(ByVal libOffset As Long, ByVal cb As Long, ByVal dwLockType As Integer) Implements IStream.UnlockRegion
            Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
        End Sub

        Public Sub Stat(ByRef pstatstg As System.Runtime.InteropServices.ComTypes.STATSTG, ByVal grfStatFlag As Integer) Implements IStream.Stat
            pstatstg = New System.Runtime.InteropServices.ComTypes.STATSTG()
            pstatstg.cbSize = baseStream.Length
        End Sub

        Public Sub Clone(ByRef ppstm As IStream) Implements IStream.Clone
            ppstm = Nothing
            Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
        End Sub

        Private Shared Function InlineAssignHelper(Of T)(ByRef target As T, ByVal value As T) As T
            target = value
            Return value
        End Function

    End Class

    <System.Serializable()> Public Class CCapeCollection

        Implements ICapeCollection

        Public _icol As List(Of ICapeIdentification)

        Sub New()
            _icol = New List(Of ICapeIdentification)
        End Sub

        Public Function Count() As Integer Implements Global.CapeOpen.ICapeCollection.Count
            Return _icol.Count
        End Function

        Public Function Item(ByVal index As Object) As Object Implements Global.CapeOpen.ICapeCollection.Item
            If Integer.TryParse(index, New Integer) Then
                Return _icol(index - 1)
            Else
                For Each p As ICapeIdentification In _icol
                    If p.ComponentName = index Then Return p Else Return Nothing
                Next
                Return Nothing
            End If
        End Function
    End Class

End Namespace
