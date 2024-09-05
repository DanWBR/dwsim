'    CAPE-OPEN Unit Operation Wrapper Class
'    Copyright 2011-2019 Daniel Wagner O. de Medeiros
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


Imports System.IO
Imports CapeOpen
Imports System.Runtime.InteropServices.ComTypes
Imports System.Runtime.InteropServices
Imports System.Reflection
Imports System.Runtime.Serialization.Formatters.Binary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Interfaces2
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects

Namespace UnitOperations

    <System.Serializable()> Public Class CapeOpenUO

        Inherits UnitOperations.UnitOpBaseClass

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_CAPEOPENUO

        <System.NonSerialized()> Private _couo As Object
        <System.NonSerialized()> Private _form As Form_CapeOpenSelector

        Private m_reactionSetID As String = "DefaultSet"
        Private m_reactionSetName As String = ""

        Public _seluo As Auxiliary.CapeOpen.CapeOpenUnitOpInfo
        Public Shadows _ports As List(Of ICapeUnitPort)
        <System.NonSerialized()> Public _params As List(Of ICapeParameter)

        <System.NonSerialized> Private _tempdata As List(Of XElement)

        <System.NonSerialized()> Private _istr As Auxiliary.CapeOpen.ComIStreamWrapper
        Private _persisteddata As Byte()

        Private _restorefromcollections As Boolean = False
        Private _recalculateoutputstreams As Boolean = True

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.CAPEOPEN

        Public Property EmbeddedImageData As String = ""

        Public Property UseEmbeddedImage As Boolean = False

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

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

        Public Sub New(ByVal name As String, ByVal description As String, ByVal gobj As IGraphicObject, Optional ByVal chemsep As Boolean = False)

            Me.New()

            Me.GraphicObject = gobj

            Me.ComponentName = name
            Me.ComponentDescription = description

            If Type.GetType("Mono.Runtime") Is Nothing Then

                If Not chemsep Then

                    ShowForm()
                    Instantiate(False)

                Else

                    Dim frmwait As New FormLS

                    frmwait.Text = "Add ChemSep Column"
                    frmwait.Label1.Text = "Scanning Registry for ChemSep's Location..."
                    frmwait.StartPosition = FormStartPosition.CenterScreen
                    frmwait.Opacity = 1.0#

                    Application.DoEvents()

                    frmwait.Show()

                    Task.Factory.StartNew(Sub()
                                              Dim colist = Form_CapeOpenSelector.SearchCOUOS(True)
                                              Dim cs = colist.Where(Function(x) x.Name.ToLower.Contains("chemsep")).SingleOrDefault
                                              If Not cs Is Nothing Then
                                                  _seluo = cs
                                              Else
                                                  Me.FlowSheet.ShowMessage("Error creating ChemSep column: ChemSep is not installed or cannot be accessed by DWSIM.", IFlowsheet.MessageType.GeneralError)
                                              End If
                                          End Sub).ContinueWith(Sub()
                                                                    frmwait.UIThreadInvoke(Sub()
                                                                                               frmwait.Close()
                                                                                               Instantiate(True)
                                                                                           End Sub)
                                                                End Sub)
                End If

            Else

                FlowSheet.ShowMessage("CAPE-OPEN Unit Operations are not supported on macOS and Linux. They will run in read-only bypass mode on these systems.", IFlowsheet.MessageType.Warning)

            End If

        End Sub

        Private Sub Instantiate(chemsep As Boolean)
            If Not _seluo Is Nothing Then
                Try
                    Dim t As Type = Type.GetTypeFromProgID(_seluo.TypeName)
                    _couo = Activator.CreateInstance(t)
                    InitNew()
                    Init()
                    GetPorts()
                    GetParams()
                    CreateConnectors()
                    If chemsep Then Edit()
                Catch ex As Exception
                    Me.FlowSheet.ShowMessage("Error creating CAPE-OPEN Unit Operation: " & ex.ToString, IFlowsheet.MessageType.GeneralError)
                End Try
            End If
        End Sub

        Public Function GetCAPEOPENObject() As Object
            Return _couo
        End Function

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New CapeOpenUO()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of CapeOpenUO)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

#Region "    CAPE-OPEN Specifics"

        Sub PersistLoad(ByVal context As System.Runtime.Serialization.StreamingContext)

            If Type.GetType("Mono.Runtime") Is Nothing Then

                If Not _seluo Is Nothing Then
                    Dim t As Type = Type.GetTypeFromProgID(_seluo.TypeName)
                    Try
                        If _couo Is Nothing Then _couo = Activator.CreateInstance(t)
                    Catch ex As Exception
                        MessageBox.Show("Error creating CAPE-OPEN Unit Operation instance." & vbCrLf & ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If

            Else

                FlowSheet.ShowMessage("CAPE-OPEN Unit Operations are not supported on macOS and Linux. They will run in read-only bypass mode on these systems.", IFlowsheet.MessageType.Warning)

            End If

            If _persisteddata IsNot Nothing Then
                _istr = New Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream(_persisteddata))
            End If

            If _istr IsNot Nothing Then
                Dim myuo As IPersistStreamInit = TryCast(_couo, IPersistStreamInit)
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

        Sub PersistSave(ByVal context As System.Runtime.Serialization.StreamingContext)

            'If the Unit Operation doesn't implement any of the IPersist interfaces, the _istr variable will be null.
            'The object will have to be restored using the parameters and ports' information only.

            _istr = Nothing

            If Not _couo Is Nothing Then
                Dim myuo As IPersistStreamInit = TryCast(_couo, IPersistStreamInit)
                If Not myuo Is Nothing Then
                    Dim myuo2 As IPersistStreamInit = TryCast(_couo, IPersistStreamInit)
                    If myuo2 IsNot Nothing Then
                        _istr = New Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream())
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
                            Me.FlowSheet.ShowMessage(Me.ComponentName & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, IFlowsheet.MessageType.GeneralError)
                            Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag + ": Error saving data from CAPE-OPEN Object - " + ex.Message.ToString(), IFlowsheet.MessageType.GeneralError)
                        End Try
                    End If
                Else
                    Dim myuo2 As Interfaces2.IPersistStream = TryCast(_couo, Interfaces2.IPersistStream)
                    If myuo2 IsNot Nothing Then
                        _istr = New Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream())
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
                            Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description, IFlowsheet.MessageType.GeneralError)
                            Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag + ": Error saving data from CAPE-OPEN Object - " + ex.Message.ToString(), IFlowsheet.MessageType.GeneralError)
                        End Try
                    End If
                End If
            End If

        End Sub


        Sub ShowForm()

            _form = New Form_CapeOpenSelector
            _form.ShowDialog(Me.FlowSheet)
            Me._seluo = _form._seluo
            If _seluo.Name.ToLower.Contains("chemsep") Then
                GraphicObject.ChemSep = True
                GraphicObject.Width = 144
                GraphicObject.Height = 180
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

            If Calculator.IsRunningOnMono Then

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
                                Dim p As New Auxiliary.CapeOpen.CapeArrayParameter(id.ComponentName, id.ComponentDescription, ip.value, ips.ItemsSpecifications, ips.NumDimensions)
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
                                        Dim mystr As Object = FlowSheet.SimulationObjects(CType(cnobj, ICapeIdentification).ComponentName)
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
                            'Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": CAPE-OPEN Exception: " & ecu.code & " at " & ecu.interfaceName & ". Reason: " & ecu.description, Color.DarkGray, DWSIM.Flowsheet.MessageType.Warning)
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
            Me.GraphicObject.InputConnectors = New List(Of Interfaces.IConnectionPoint)
            Me.GraphicObject.OutputConnectors = New List(Of Interfaces.IConnectionPoint)
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
                            .Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + (Me.GraphicObject.InputConnectors.Count) / (nip - 1) * Me.GraphicObject.Height / 2)
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
                            .Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + +(Me.GraphicObject.InputConnectors.Count) / (nip - 1) * Me.GraphicObject.Height / 2)
                            .ConnectorName = p.ComponentName
                        End With
                End Select
            Next
            UpdateConnectorPositions()
        End Sub

        Sub UpdateConnectors()

            ' disconnect existing connections
            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
                If c.IsAttached Then
                    Me.FlowSheet.DisconnectObjects(FlowSheet.GraphicObjects(c.AttachedConnector.AttachedFrom.Name), Me.GraphicObject)
                End If
            Next
            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                If c.IsAttached Then
                    Me.FlowSheet.DisconnectObjects(Me.GraphicObject, FlowSheet.GraphicObjects(c.AttachedConnector.AttachedTo.Name))
                End If
            Next

            Me.GraphicObject.InputConnectors = New List(Of Interfaces.IConnectionPoint)
            Me.GraphicObject.OutputConnectors = New List(Of Interfaces.IConnectionPoint)

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
                                    .Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + (Me.GraphicObject.InputConnectors.Count) / (nip - 1) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Try
                                    cnobj = myport.connectedObject
                                Catch ex As Exception
                                    cnobj = Nothing
                                End Try
                                If cnobj IsNot Nothing Then
                                    objid = CType(cnobj, ICapeIdentification).ComponentName
                                    myport.Disconnect()
                                    Dim gobj As IGraphicObject = FlowSheet.GraphicObjects(objid)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_MATERIAL
                                            Me.FlowSheet.ConnectObjects(gobj, Me.GraphicObject, 0, Me.GraphicObject.InputConnectors.Count - 1)
                                        Case CapePortType.CAPE_ENERGY
                                            Me.FlowSheet.ConnectObjects(gobj, Me.GraphicObject, 0, Me.GraphicObject.InputConnectors.Count - 1)
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
                                    .Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + (Me.GraphicObject.OutputConnectors.Count) / (nop - 1) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Try
                                    cnobj = myport.connectedObject
                                Catch ex As Exception
                                    cnobj = Nothing
                                End Try
                                If cnobj IsNot Nothing Then
                                    objid = CType(cnobj, ICapeIdentification).ComponentName
                                    myport.Disconnect()
                                    Dim gobj As IGraphicObject = FlowSheet.GraphicObjects(objid)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_MATERIAL
                                            Me.FlowSheet.ConnectObjects(Me.GraphicObject, gobj, Me.GraphicObject.OutputConnectors.Count - 1, 0)
                                        Case CapePortType.CAPE_ENERGY
                                            Me.FlowSheet.ConnectObjects(Me.GraphicObject, gobj, Me.GraphicObject.OutputConnectors.Count - 1, 0)
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
                                    .Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + (Me.GraphicObject.InputConnectors.Count) / (nip) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Try
                                    Dim gobj As IGraphicObject = Me.GraphicObject.InputConnectors(ic).AttachedConnector.AttachedFrom
                                    myport.Connect(FlowSheet.GetFlowsheetSimulationObject(gobj.Tag))
                                Catch ex As Exception
                                End Try
                                ic += 1
                            Case CapePortDirection.CAPE_OUTLET
                                With Me.GraphicObject.OutputConnectors(oc)
                                    Select Case myport.portType
                                        Case CapePortType.CAPE_ENERGY
                                            .Type = ConType.ConEn
                                        Case CapePortType.CAPE_MATERIAL
                                            .Type = ConType.ConOut
                                    End Select
                                    .Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + (Me.GraphicObject.OutputConnectors.Count) / (nop) * Me.GraphicObject.Height / 2)
                                    .ConnectorName = id.ComponentName
                                End With
                                Try
                                    Dim gobj As IGraphicObject = Me.GraphicObject.OutputConnectors(oc).AttachedConnector.AttachedTo
                                    myport.Connect(Me.FlowSheet.GetFlowsheetSimulationObject(gobj.Tag))
                                Catch ex As Exception
                                End Try
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
            For Each ic As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
                obj1(i) = -Me.GraphicObject.X + ic.Position.X
                obj2(i) = -Me.GraphicObject.Y + ic.Position.Y
                i = i + 1
            Next
            i = 0
            For Each oc As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
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
                                _ports(_ports.Count - 1).Connect(FlowSheet.SimulationObjects(CType(cnobj, ICapeIdentification).ComponentName))
                            End If
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = myuo
                            Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": CAPE-OPEN Exception: " & ecu.code & " at " & ecu.interfaceName & ". Reason: " & ecu.description, IFlowsheet.MessageType.Warning)
                        End Try
                    Next
                End If
            End If
        End Sub

        Sub UpdatePortsFromConnectors()

            Dim cnobj As Object = Nothing
            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
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
                            p.Connect(FlowSheet.SimulationObjects(c.AttachedConnector.AttachedFrom.Name))
                        End If
                    End If
                Next
            Next

            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
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
                            p.Connect(FlowSheet.SimulationObjects(c.AttachedConnector.AttachedTo.Name))
                        End If
                    End If
                Next
            Next

            RestorePorts()

        End Sub

        Sub DisconnectPorts()
            If Not _couo Is Nothing Then
                Dim cnobj As Object = Nothing
                Dim myuo As CapeOpen.ICapeUnit = _couo
                Dim myports As ICapeCollection = myuo.ports
                Dim i As Integer = 0
                Dim numports As Integer = myports.Count
                If numports > 0 Then
                    For i = 1 To numports
                        Dim id As ICapeIdentification = myports.Item(i)
                        Dim myport As ICapeUnitPort = myports.Item(i)
                        Try
                            cnobj = myport.connectedObject
                        Catch ex As Exception
                            cnobj = Nothing
                        End Try
                        Try
                            If Not cnobj Is Nothing Then myport.Disconnect()
                        Catch ex As Exception
                            Dim ecu As CapeOpen.ECapeUser = myuo
                            Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": CAPE-OPEN Exception: " & ecu.code & " at " & ecu.interfaceName & ". Reason: " & ecu.description, IFlowsheet.MessageType.Warning)
                        End Try
                    Next
                End If
            End If
        End Sub


        Sub Edit()
            If Not _couo Is Nothing Then
                Dim myuo As CapeOpen.ICapeUtilities = _couo
                RestorePorts()
                Try
                    myuo.Edit()
                Catch ex As Exception
                    Dim ecu As CapeOpen.ECapeUser = myuo
                    Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": CAPE-OPEN Exception: " & ecu.code & " at " & ecu.interfaceName & ". Reason: " & ecu.description, IFlowsheet.MessageType.Warning)
                End Try
                UpdateParams()
                UpdatePorts()
                UpdateConnectors()
            End If
        End Sub

#End Region

#Region "    DWSIM Specifics"

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Dim info As XElement = (From el As XElement In data Select el Where el.Name = "CAPEOPEN_Object_Info").SingleOrDefault
            _seluo = New Auxiliary.CapeOpen.CapeOpenUnitOpInfo
            _seluo.LoadData(info.Elements.ToList)

            If Not Calculator.IsRunningOnMono Then

                If Not _seluo Is Nothing Then
                    Try

                        Dim t As Type = Type.GetTypeFromProgID(_seluo.TypeName)
                        _couo = Activator.CreateInstance(t)

                        InitNew()
                        Init()

                        Dim pdata As XElement = (From el As XElement In data Select el Where el.Name = "PersistedData").SingleOrDefault

                        If Not pdata Is Nothing Then
                            _istr = New Auxiliary.CapeOpen.ComIStreamWrapper(New MemoryStream(Convert.FromBase64String(pdata.Value)))
                            PersistLoad(Nothing)
                        End If

                        'Dim paramdata As XElement = (From el As XElement In data Select el Where el.Name = "ParameterData").SingleOrDefault
                        'Dim b As New BinaryFormatter, m As New MemoryStream()
                        '_params = b.Deserialize(New MemoryStream(Convert.FromBase64String(paramdata.Value)))

                        'RestoreParams()
                        GetPorts()

                    Catch ex As Exception

                        Me.FlowSheet.ShowMessage("Error creating CAPE-OPEN Unit Operation: " & ex.ToString, IFlowsheet.MessageType.GeneralError)

                    End Try
                End If

            Else

                If Not _seluo Is Nothing Then

                    FlowSheet.ShowMessage("CAPE-OPEN Unit Operations are not supported on macOS and Linux. They will run in read-only bypass mode on these systems.", IFlowsheet.MessageType.Warning)

                End If

            End If

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()
            With elements
                If Not _seluo Is Nothing Then
                    .Add(New XElement("CAPEOPEN_Object_Info", _seluo.SaveData().ToArray))
                    Me.PersistSave(Nothing)
                    If Not _istr Is Nothing Then .Add(New XElement("PersistedData", Convert.ToBase64String(CType(_istr.baseStream, MemoryStream).ToArray())))
                    'Dim b As New BinaryFormatter, m As New MemoryStream()
                    'b.Serialize(m, _params)
                    '.Add(New XElement("ParameterData", Convert.ToBase64String(m.ToArray)))
                End If
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

        Public Overrides Sub RunDynamicModel()

            Calculate()

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If Not Calculator.IsRunningOnMono Then

                Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

                IObj?.SetCurrent()

                'If Not CreatedWithThreadID = Thread.CurrentThread.ManagedThreadId Then
                '    disposedValue = False
                '    Me.Dispose(True)
                '    'load current configuration from temporary data and re-instantiate the COM object using the current thread.
                '    'this is called only when solving the object with a background thread.
                '    Me.LoadData(_tempdata)
                '    CreatedWithThreadID = Thread.CurrentThread.ManagedThreadId
                'End If

                UpdatePortsFromConnectors()

                If Not _couo Is Nothing Then
                    For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
                        If c.IsAttached And c.Type = ConType.ConIn Then
                            Dim mat As MaterialStream = FlowSheet.SimulationObjects(c.AttachedConnector.AttachedFrom.Name)
                            mat.SetFlowsheet(Me.FlowSheet)
                        End If
                    Next
                    Dim myuo As CapeOpen.ICapeUnit = _couo
                    Dim msg As String = ""
                    Try
                        'set reaction set, if supported
                        If Not TryCast(_couo, CAPEOPEN110.ICapeKineticReactionContext) Is Nothing Then
                            Me.FlowSheet.ReactionSets(Me.ReactionSetID).simulationContext = Me.FlowSheet
                            Dim myset = DirectCast(Me.FlowSheet.ReactionSets(Me.ReactionSetID), ReactionSet)
                            Dim myruo As CAPEOPEN110.ICapeKineticReactionContext = _couo
                            myruo.SetReactionObject(myset)
                        End If
                    Catch ex As Exception
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConEn Then
                                If c.IsAttached Then c.AttachedConnector.AttachedTo.Calculated = False
                            End If
                        Next
                        Dim ecu As CapeOpen.ECapeUser = myuo
                        Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & ":" & ecu.scope & ". Reason: " & ecu.description, IFlowsheet.MessageType.GeneralError)
                    End Try

                    For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                        If c.IsAttached And c.Type = ConType.ConOut Then
                            Dim mat As MaterialStream = FlowSheet.SimulationObjects(c.AttachedConnector.AttachedTo.Name)
                            mat.ClearAllProps()
                        End If
                    Next

                    RestorePorts()

                    Try
                        myuo.Validate(msg)
                        If Not myuo.ValStatus = CapeValidationStatus.CAPE_VALID Then
                            Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag + ": CAPE-OPEN Unit Operation not validated. Reason: " + msg, IFlowsheet.MessageType.GeneralError)
                            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                                If c.Type = ConType.ConEn Then
                                    If c.IsAttached Then c.AttachedConnector.AttachedTo.Calculated = False
                                End If
                            Next
                            Throw New Exception("CAPE-OPEN Unit Operation not validated. Reason: " + msg)
                        End If
                        myuo.Calculate()
                        UpdateParams()
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.IsAttached And c.Type = ConType.ConOut Then
                                Dim mat As MaterialStream = FlowSheet.SimulationObjects(c.AttachedConnector.AttachedTo.Name)
                                mat.PropertyPackage.CurrentMaterialStream = mat
                                For Each subst As Compound In mat.Phases(0).Compounds.Values
                                    subst.MassFraction = mat.PropertyPackage.AUX_CONVERT_MOL_TO_MASS(subst.Name, 0)
                                Next
                            End If
                        Next
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConEn And c.IsAttached Then
                                c.AttachedConnector.AttachedTo.Calculated = True
                            End If
                        Next
                        If IObj IsNot Nothing Then
                            Dim ur As CapeOpen.ICapeUnitReport = _couo
                            If Not ur Is Nothing Then
                                Dim reps As String() = ur.reports
                                For Each r As String In reps
                                    ur.selectedReport = r
                                    Dim msg2 As String = ""
                                    ur.ProduceReport(msg2)
                                    IObj.Paragraphs.Add("</p><pre>" + msg2 + "</pre><p>")
                                Next
                            End If
                        End If
                    Catch ex As Exception
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConEn Then
                                If c.IsAttached Then c.AttachedConnector.AttachedTo.Calculated = False
                            End If
                        Next
                        Dim ecu As CapeOpen.ECapeUser = myuo
                        Me.FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & ":" & ecu.scope & ". Reason: " & ecu.description, IFlowsheet.MessageType.GeneralError)
                        Throw ex
                    End Try
                End If

                IObj?.Close()

            End If

        End Sub

        Public Overrides Sub DeCalculate()


        End Sub

        Public Overrides Sub Validate()
            MyBase.Validate()
        End Sub

        Public Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            If Not Me._params Is Nothing Then
                For Each p As ICapeIdentification In Me._params
                    proplist.Add(p.ComponentName)
                Next
                Return proplist.ToArray(Type.GetType("System.String"))
            Else
                Return New String() {Nothing}
            End If
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Return ""
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then

                Return val0

            Else

                If Not Me._params Is Nothing Then
                    For Each p As ICapeIdentification In Me._params
                        If p.ComponentName = prop Then
                            Dim val As Object = Nothing
                            If TypeOf p Is Auxiliary.CapeOpen.CapeArrayParameter Then
                                val = DirectCast(p, Auxiliary.CapeOpen.CapeArrayParameter).value
                            Else
                                val = DirectCast(p, ICapeParameter).value
                            End If
                            If Not val Is Nothing Then
                                If TypeOf p Is Auxiliary.CapeOpen.CapeArrayParameter Then
                                    If TryCast(val, Object()) IsNot Nothing Then
                                        Return DirectCast(val, Object()).ToArrayString
                                    Else
                                        Return val.ToString
                                    End If
                                Else
                                    Return CType(p, ICapeParameter).value.ToString
                                End If
                            Else
                                Return ""
                            End If
                        End If
                    Next
                    Return ""
                Else
                    Return ""
                End If

            End If

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            For Each p As ICapeIdentification In Me._params
                If p.ComponentName = prop Then
                    If TypeOf p Is Auxiliary.CapeOpen.CapeArrayParameter Then
                        DirectCast(p, Auxiliary.CapeOpen.CapeArrayParameter).value = propval
                    Else
                        DirectCast(p, ICapeParameter).value = propval
                    End If
                    RestoreParams()
                    Return 1
                End If
            Next
            Return 0
        End Function

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

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_CAPEOPENUO With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_CAPEOPENUO With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.uo_co_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("COUO_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("COUO_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property


        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim reports As New Text.StringBuilder

            Dim ur As CapeOpen.ICapeUnitReport = _couo
            If Not ur Is Nothing Then
                Dim reps As String() = ur.reports
                For Each r As String In reps
                    ur.selectedReport = r
                    Dim msg2 As String = ""
                    ur.ProduceReport(msg2)
                    reports.AppendLine(msg2)
                Next
            End If

            Return reports.ToString

        End Function

        Public Overrides Sub PerformPostCalcValidation()


        End Sub

#Region "    IProductInformation"

        Public Overrides ReadOnly Property ProductName As String
            Get
                If _seluo IsNot Nothing Then
                    Return _seluo.Name
                Else
                    Return ComponentName
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property ProductDescription As String
            Get
                If _seluo IsNot Nothing Then
                    Return _seluo.Description
                Else
                    Return ComponentDescription
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property ProductAuthor As String
            Get
                If _seluo IsNot Nothing Then
                    Return _seluo.AboutInfo
                Else
                    Return "Daniel Wagner"
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property ProductContactInfo As String
            Get
                If _seluo IsNot Nothing Then
                    Return _seluo.HelpURL
                Else
                    Return "https://dwsim.inforside.com.br"
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property ProductPage As String
            Get
                If _seluo IsNot Nothing Then
                    Return _seluo.VendorURL
                Else
                    Return "https://dwsim.inforside.com.br"
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property ProductVersion As String
            Get
                If _seluo IsNot Nothing Then
                    Return _seluo.Version
                Else
                    Return Assembly.GetExecutingAssembly().GetName().Version.ToString()
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property ProductAssembly As String
            Get
                If _seluo IsNot Nothing Then
                    Return _seluo.Location
                Else
                    Return Assembly.GetExecutingAssembly().GetName().Name
                End If
            End Get
        End Property

#End Region

    End Class

End Namespace

Namespace UnitOperations.Auxiliary.CapeOpen

    <System.Serializable()> Public Class CapeOpenUnitOpInfo

        Implements Interfaces.ICustomXMLSerialization

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

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
            Return True
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

    ' System.Drawing.ComIStreamWrapper.cs
    '
    ' Author:
    ' Kornel Pal <http://www.kornelpal.hu/>
    '
    ' Copyright (C) 2005-2008 Kornel Pal
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

Namespace UnitOperations.Auxiliary.CapeOpen

    ''' <summary>
    ''' This class if for legacy compatibility only. It should NOT be used. Use CapeOpen.RealParameter instead if necessary.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class CRealParameter
        Implements ICapeIdentification, ICapeParameter, ICapeParameterSpec, ICapeRealParameterSpec
        Dim _par As Global.CapeOpen.RealParameter
        Public Event OnParameterValueChanged(ByVal sender As Object, ByVal args As System.EventArgs)
        Sub New(ByVal name As String, ByVal value As Double, ByVal defaultvalue As Double, ByVal unit As String)
            _par = New Global.CapeOpen.RealParameter(name, value, defaultvalue, unit)
        End Sub
        Public Property ComponentDescription() As String Implements Global.CapeOpen.ICapeIdentification.ComponentDescription
            Get
                Return _par.ComponentDescription
            End Get
            Set(ByVal value As String)
                _par.ComponentDescription = value
            End Set
        End Property
        Public Property ComponentName() As String Implements Global.CapeOpen.ICapeIdentification.ComponentName
            Get
                Return _par.ComponentName
            End Get
            Set(ByVal value As String)
                _par.ComponentName = value
            End Set
        End Property
        Public Property Mode() As Global.CapeOpen.CapeParamMode Implements Global.CapeOpen.ICapeParameter.Mode
            Get
                Return _par.Mode
            End Get
            Set(ByVal value As Global.CapeOpen.CapeParamMode)
                _par.Mode = value
            End Set
        End Property
        Public Sub Reset() Implements Global.CapeOpen.ICapeParameter.Reset
            _par.Reset()
        End Sub
        Public ReadOnly Property Specification() As Object Implements Global.CapeOpen.ICapeParameter.Specification
            Get
                Return Me
            End Get
        End Property
        Public Function Validate(ByRef message As String) As Boolean Implements Global.CapeOpen.ICapeParameter.Validate
            Return _par.Validate(message)
        End Function
        Public ReadOnly Property ValStatus() As Global.CapeOpen.CapeValidationStatus Implements Global.CapeOpen.ICapeParameter.ValStatus
            Get
                Return _par.ValStatus
            End Get
        End Property
        Public Property value() As Object Implements Global.CapeOpen.ICapeParameter.value
            Get
                Return _par.SIValue
            End Get
            Set(ByVal value As Object)
                _par.SIValue = value
                RaiseEvent OnParameterValueChanged(Me, New System.EventArgs())
            End Set
        End Property
        Public ReadOnly Property Dimensionality() As Object Implements Global.CapeOpen.ICapeParameterSpec.Dimensionality
            Get
                Dim myd As ICapeParameterSpec = _par
                Return New Double() {myd.Dimensionality(0), myd.Dimensionality(1), myd.Dimensionality(2), myd.Dimensionality(3), myd.Dimensionality(4), myd.Dimensionality(5), myd.Dimensionality(6), myd.Dimensionality(7)}
            End Get
        End Property
        Public ReadOnly Property Type() As Global.CapeOpen.CapeParamType Implements Global.CapeOpen.ICapeParameterSpec.Type
            Get
                Return _par.Type
            End Get
        End Property
        Public ReadOnly Property DefaultValue() As Double Implements Global.CapeOpen.ICapeRealParameterSpec.DefaultValue
            Get
                Return _par.DefaultValue
            End Get
        End Property
        Public ReadOnly Property LowerBound() As Double Implements Global.CapeOpen.ICapeRealParameterSpec.LowerBound
            Get
                Return _par.LowerBound
            End Get
        End Property
        Public ReadOnly Property UpperBound() As Double Implements Global.CapeOpen.ICapeRealParameterSpec.UpperBound
            Get
                Return _par.UpperBound
            End Get
        End Property
        Public Function Validate1(ByVal value As Double, ByRef message As String) As Boolean Implements Global.CapeOpen.ICapeRealParameterSpec.Validate
            Return _par.Validate(value, message)
        End Function
    End Class

    <System.Serializable> Public Class CapeArrayParameter

        Inherits Global.CapeOpen.CapeParameter

        Implements Global.CapeOpen.ICapeArrayParameterSpec

        Public Property value As Object
        <NonSerialized> Private ispecs As Object
        Private numdim As Integer

        Sub New(ByVal name As String, description As String, ByVal value As Object, ispecs As Object, numdim As Integer)
            MyBase.New(name, description)
            Me.value = value
            Me.ispecs = ispecs
            Me.numdim = numdim
        End Sub

        Public ReadOnly Property ItemsSpecifications As Object Implements ICapeArrayParameterSpec.ItemsSpecifications
            Get
                Return ispecs
            End Get
        End Property

        Public ReadOnly Property NumDimensions As Integer Implements ICapeArrayParameterSpec.NumDimensions
            Get
                Return 1
            End Get
        End Property

        Public ReadOnly Property Size As Object Implements ICapeArrayParameterSpec.Size
            Get
                Return value.Length
            End Get
        End Property

        Public Overrides ReadOnly Property Type As CapeParamType
            Get
                Return CapeParamType.CAPE_ARRAY
            End Get
        End Property

        Public Overrides Sub Reset()
            value = Nothing
        End Sub

        Public Function Validate1(inputArray As Object, ByRef value As Object) As Object Implements ICapeArrayParameterSpec.Validate
            Return TypeOf inputArray Is System.Array
        End Function

        Public Overrides Function Validate(ByRef message As String) As Boolean
            Return True
        End Function

    End Class

End Namespace
