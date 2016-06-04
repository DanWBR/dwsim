'    Custom (Scripting) Unit Operation Calculation Routines 
'    Copyright 2010-2011 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.DrawingTools.GraphicsSurface
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums
Imports System.IO
Imports System.Runtime.InteropServices
Imports CapeOpen
Imports System.Runtime.Serialization.Formatters
Imports System.Linq
Imports System.ComponentModel
Imports System.Drawing.Design
Imports Microsoft.Scripting.Hosting

Namespace UnitOperations

    <Guid(CustomUO.ClassId)> <System.Serializable()> <ComVisible(True)> Public Class CustomUO

        Inherits SharedClasses.UnitOperations.UnitOpBaseClass

        <NonSerialized> <Xml.Serialization.XmlIgnore> Dim f As EditingForm_CustomUO

        Private _scripttext As String = ""
        Private _includes() As String
        Private _fontname As String = "Courier New"
        Private _fontsize As Integer = 10

        Public Property HighlightSpaces As Boolean = False
        Public Property HighlightTabs As Boolean = False

        Public Shadows Const ClassId As String = "1FD2DC53-DC7B-4c4d-BBEE-F37F4E5ADDFB"

#Region "   DWSIM Methods"

        Public Property InputVariables As Dictionary(Of String, Double)
        Public Property OutputVariables As Dictionary(Of String, Double)

        Public Property FontName() As String
            Get
                Return _fontname
            End Get
            Set(ByVal value As String)
                _fontname = value
            End Set
        End Property

        Public Property FontSize() As Integer
            Get
                Return _fontsize
            End Get
            Set(ByVal value As Integer)
                _fontsize = value
            End Set
        End Property

        Public Property Includes() As String()
            Get
                Return _includes
            End Get
            Set(ByVal value As String())
                _includes = value
            End Set
        End Property

        Public Property ScriptText() As String
            Get
                Return _scripttext
            End Get
            Set(ByVal value As String)
                _scripttext = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
            InputVariables = New Dictionary(Of String, Double)
            OutputVariables = New Dictionary(Of String, Double)
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)
            MyBase.CreateNew()
            InputVariables = New Dictionary(Of String, Double)
            OutputVariables = New Dictionary(Of String, Double)
            Me.ComponentName = name
            Me.ComponentDescription = description
        End Sub

        Private Property engine As ScriptEngine

        Private Property scope As Object

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim ims1, ims2, ims3, ims4, ims5, ims6, oms1, oms2, oms3, oms4, oms5, oms6 As MaterialStream
            If Me.GraphicObject.InputConnectors(0).IsAttached Then
                ims1 = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            Else
                ims1 = Nothing
            End If
            If Me.GraphicObject.InputConnectors(1).IsAttached Then
                ims2 = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
            Else
                ims2 = Nothing
            End If
            If Me.GraphicObject.InputConnectors(2).IsAttached Then
                ims3 = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Name)
            Else
                ims3 = Nothing
            End If
            If Me.GraphicObject.InputConnectors(4).IsAttached Then
                ims4 = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Name)
            Else
                ims4 = Nothing
            End If
            If Me.GraphicObject.InputConnectors(5).IsAttached Then
                ims5 = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Name)
            Else
                ims5 = Nothing
            End If
            If Me.GraphicObject.InputConnectors(6).IsAttached Then
                ims6 = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom.Name)
            Else
                ims6 = Nothing
            End If
            If Me.GraphicObject.OutputConnectors(0).IsAttached Then
                oms1 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
            Else
                oms1 = Nothing
            End If
            If Me.GraphicObject.OutputConnectors(1).IsAttached Then
                oms2 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)
            Else
                oms2 = Nothing
            End If
            If Me.GraphicObject.OutputConnectors(2).IsAttached Then
                oms3 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Name)
            Else
                oms3 = Nothing
            End If
            If Me.GraphicObject.OutputConnectors(4).IsAttached Then
                oms4 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(4).AttachedConnector.AttachedTo.Name)
            Else
                oms4 = Nothing
            End If
            If Me.GraphicObject.OutputConnectors(5).IsAttached Then
                oms5 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(5).AttachedConnector.AttachedTo.Name)
            Else
                oms5 = Nothing
            End If
            If Me.GraphicObject.OutputConnectors(6).IsAttached Then
                oms6 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(6).AttachedConnector.AttachedTo.Name)
            Else
                oms6 = Nothing
            End If

            Dim ies1, oes1 As Streams.EnergyStream
            If Me.GraphicObject.InputConnectors(3).IsAttached Then
                ies1 = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Name)
            Else
                ies1 = Nothing
            End If
            If Me.GraphicObject.OutputConnectors(3).IsAttached Then
                oes1 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Name)
            Else
                oes1 = Nothing
            End If

            engine = IronPython.Hosting.Python.CreateEngine()
            engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
            engine.Runtime.LoadAssembly(GetType(BaseClasses.ConstantProperties).Assembly)
            engine.Runtime.LoadAssembly(GetType(GraphicObject).Assembly)
            engine.Runtime.LoadAssembly(GetType(GraphicsSurface).Assembly)
            scope = engine.CreateScope()
            scope.SetVariable("Flowsheet", FlowSheet)
            'scope.SetVariable("Plugins", My.Application.UtilityPlugins)
            scope.SetVariable("Me", Me)
            'scope.SetVariable("AbortScript", My.Application.CalculatorStopRequested)
            For Each variable In InputVariables
                scope.SetVariable(variable.Key, variable.Value)
            Next
            scope.SetVariable("ims1", ims1)
            scope.SetVariable("ims2", ims2)
            scope.SetVariable("ims3", ims3)
            scope.SetVariable("ims4", ims4)
            scope.SetVariable("ims5", ims5)
            scope.SetVariable("ims6", ims6)
            scope.SetVariable("oms1", oms1)
            scope.SetVariable("oms2", oms2)
            scope.SetVariable("oms3", oms3)
            scope.SetVariable("oms4", oms4)
            scope.SetVariable("oms5", oms5)
            scope.SetVariable("oms6", oms6)
            scope.SetVariable("ies1", ies1)
            scope.SetVariable("oes1", oes1)
            'Dim Solver As New DWSIM.Flowshee
            'scope.SetVariable("Solver", Solver)
            Dim txtcode As String = ""
            If Not Includes Is Nothing Then
                For Each fname As String In Me.Includes
                    txtcode += File.ReadAllText(fname) + vbCrLf
                Next
            End If
            txtcode += Me.ScriptText
            Dim source As Microsoft.Scripting.Hosting.ScriptSource = Me.engine.CreateScriptSourceFromString(txtcode, Microsoft.Scripting.SourceCodeKind.Statements)
            Try
                Me.ErrorMessage = ""
                source.Execute(Me.scope)
                OutputVariables.Clear()
                For Each variable In scope.GetVariableNames
                    If TypeOf scope.GetVariable(variable) Is Double Or TypeOf scope.GetVariable(variable) Is Integer Then OutputVariables.Add(variable, scope.GetVariable(variable))
                Next
            Catch ex As Exception
                Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
                Me.ErrorMessage = ops.FormatException(ex).ToString
                Me.DeCalculate()
                engine = Nothing
                scope = Nothing
                source = Nothing
                Throw New Exception(Me.ErrorMessage, ex)
            Finally
                engine = Nothing
                scope = Nothing
                source = Nothing
            End Try

            If Not oes1 Is Nothing Then
                oes1.GraphicObject.Calculated = True
            End If

        End Sub

        Public Overrides Sub DeCalculate()


        End Sub

        Public Overrides Sub Validate()
            MyBase.Validate()
        End Sub

        Public Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Select Case proptype
                Case PropertyType.ALL
                    Return Me.OutputVariables.Keys.ToArray.Union(Me.InputVariables.Keys.ToArray).ToArray
                Case PropertyType.RO
                    Return Me.OutputVariables.Keys.ToArray
                Case PropertyType.WR
                    Return Me.InputVariables.Keys.ToArray
                Case Else
                    Return New String() {}
            End Select
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Return ""
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            If Me.OutputVariables.ContainsKey(prop) Then Return Me.OutputVariables(prop)
            If Me.InputVariables.ContainsKey(prop) Then Return Me.InputVariables(prop)
            Return Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If Me.InputVariables.ContainsKey(prop) Then Me.InputVariables(prop) = propval
            Return Nothing
        End Function

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean

            If InputVariables Is Nothing Then InputVariables = New Dictionary(Of String, Double)
            If OutputVariables Is Nothing Then OutputVariables = New Dictionary(Of String, Double)

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Me.InputVariables.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InputVariables").Elements.ToList
                Me.InputVariables.Add(xel.@Key, Double.Parse(xel.@Value, ci))
            Next

            Me.OutputVariables.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "OutputVariables").Elements.ToList
                Me.OutputVariables.Add(xel.@Key, Double.Parse(xel.@Value, ci))
            Next
            Return True
        End Function

        Public Overrides Function SaveData() As List(Of XElement)

            Dim elements As List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("InputVariables"))
                For Each p In InputVariables
                    .Item(.Count - 1).Add(New XElement("Variable", New XAttribute("Key", p.Key), New XAttribute("Value", p.Value.ToString(ci))))
                Next
                .Add(New XElement("OutputVariables"))
                For Each p In OutputVariables
                    .Item(.Count - 1).Add(New XElement("Variable", New XAttribute("Key", p.Key), New XAttribute("Value", p.Value.ToString(ci))))
                Next
            End With

            Return elements

        End Function

#End Region

#Region "   CAPE-OPEN Methods"

        Protected WithEvents m_sl As OptionParameter

        Public Overrides Sub Initialize()

            My.Application.ChangeUICulture("en")

            'set CAPE-OPEN Mode 
            _capeopenmode = True

            'create port collection
            _ports = New PortCollection()

            ' create ports
            With _ports
                .Add(New UnitPort("Inlet_Port_1", "Material Object Inlet Port 1", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_2", "Material Object Inlet Port 2", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_3", "Material Object Inlet Port 3", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_4", "Material Object Inlet Port 4", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_5", "Material Object Inlet Port 5", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_6", "Material Object Inlet Port 6", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_7", "Material Object Inlet Port 7", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_8", "Material Object Inlet Port 8", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_9", "Material Object Inlet Port 9", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_10", "Material Object Inlet Port 10", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_1", "Material Object Outlet Port 1", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_2", "Material Object Outlet Port 2", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_3", "Material Object Outlet Port 3", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_4", "Material Object Outlet Port 4", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_5", "Material Object Outlet Port 5", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_6", "Material Object Outlet Port 6", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_7", "Material Object Outlet Port 7", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_8", "Material Object Outlet Port 8", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_9", "Material Object Outlet Port 9", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_10", "Material Object Outlet Port 10", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Energy_Inlet_Port_1", "Energy Stream Inlet Port", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                .Add(New UnitPort("Energy_Outlet_Port_1", "Energy Stream Outlet Port", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
            End With

            _parameters = New ParameterCollection()

        End Sub

        Public Overrides Sub Edit1()

            Dim edform As New EditingForm_CustomUO_ScriptEditor
            With edform
                .fontname = Me.FontName
                .fontsize = Me.FontSize
                .includes = Me.Includes
                .txtScript.Text = Me.ScriptText
                .ScriptUO = Me
                .ShowDialog()
                Me.FontName = .fontname
                Me.FontSize = .fontsize
                Me.Includes = .includes
                Me.ScriptText = .txtScript.Text
            End With
            edform.Dispose()
            edform = Nothing

        End Sub

        Public Overrides ReadOnly Property ValStatus() As CapeOpen.CapeValidationStatus
            Get
                _valres = "Unit validated successfully."
                Return CapeOpen.CapeValidationStatus.CAPE_VALID
            End Get
        End Property

        Public Overrides Sub Calculate1()

            Dim ims1, ims2, ims3, ims4, ims5, ims6, ims7, ims8, ims9, ims10,
                oms1, oms2, oms3, oms4, oms5, oms6, oms7, oms8, oms9, oms10 As ICapeThermoMaterialObject

            ims1 = TryCast(Me._ports(0).connectedObject, ICapeThermoMaterialObject)
            ims2 = TryCast(Me._ports(1).connectedObject, ICapeThermoMaterialObject)
            ims3 = TryCast(Me._ports(2).connectedObject, ICapeThermoMaterialObject)
            ims4 = TryCast(Me._ports(3).connectedObject, ICapeThermoMaterialObject)
            ims5 = TryCast(Me._ports(4).connectedObject, ICapeThermoMaterialObject)
            ims6 = TryCast(Me._ports(5).connectedObject, ICapeThermoMaterialObject)
            ims7 = TryCast(Me._ports(6).connectedObject, ICapeThermoMaterialObject)
            ims8 = TryCast(Me._ports(7).connectedObject, ICapeThermoMaterialObject)
            ims9 = TryCast(Me._ports(8).connectedObject, ICapeThermoMaterialObject)
            ims10 = TryCast(Me._ports(9).connectedObject, ICapeThermoMaterialObject)
            oms1 = TryCast(Me._ports(10).connectedObject, ICapeThermoMaterialObject)
            oms2 = TryCast(Me._ports(11).connectedObject, ICapeThermoMaterialObject)
            oms3 = TryCast(Me._ports(12).connectedObject, ICapeThermoMaterialObject)
            oms4 = TryCast(Me._ports(13).connectedObject, ICapeThermoMaterialObject)
            oms5 = TryCast(Me._ports(14).connectedObject, ICapeThermoMaterialObject)
            oms6 = TryCast(Me._ports(15).connectedObject, ICapeThermoMaterialObject)
            oms7 = TryCast(Me._ports(16).connectedObject, ICapeThermoMaterialObject)
            oms8 = TryCast(Me._ports(17).connectedObject, ICapeThermoMaterialObject)
            oms9 = TryCast(Me._ports(18).connectedObject, ICapeThermoMaterialObject)
            oms10 = TryCast(Me._ports(19).connectedObject, ICapeThermoMaterialObject)

            Dim ies1, oes1 As ICapeCollection

            ies1 = TryCast(Me._ports(20).connectedObject, ICapeCollection)
            oes1 = TryCast(Me._ports(21).connectedObject, ICapeCollection)

            Dim source As Microsoft.Scripting.Hosting.ScriptSource
            Try
                engine = IronPython.Hosting.Python.CreateEngine()
                engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
                engine.Runtime.LoadAssembly(GetType(ICapeIdentification).Assembly)
                engine.Runtime.LoadAssembly(GetType(CapeOpen.ICapeIdentification).Assembly)
                engine.Runtime.LoadAssembly(GetType(BaseClasses.ConstantProperties).Assembly)
                scope = engine.CreateScope()
                scope.SetVariable("pme", Me._simcontext)
                scope.SetVariable("this", Me)
                scope.SetVariable("ims1", ims1)
                scope.SetVariable("ims2", ims2)
                scope.SetVariable("ims3", ims3)
                scope.SetVariable("ims4", ims4)
                scope.SetVariable("ims5", ims5)
                scope.SetVariable("ims6", ims6)
                scope.SetVariable("ims7", ims7)
                scope.SetVariable("ims8", ims8)
                scope.SetVariable("ims9", ims9)
                scope.SetVariable("ims10", ims10)
                scope.SetVariable("oms1", oms1)
                scope.SetVariable("oms2", oms2)
                scope.SetVariable("oms3", oms3)
                scope.SetVariable("oms4", oms4)
                scope.SetVariable("oms5", oms5)
                scope.SetVariable("oms6", oms6)
                scope.SetVariable("oms7", oms7)
                scope.SetVariable("oms8", oms8)
                scope.SetVariable("oms9", oms9)
                scope.SetVariable("oms10", oms10)
                scope.SetVariable("ies1", ies1)
                scope.SetVariable("oes1", oes1)
                Dim txtcode As String = ""
                If Not Includes Is Nothing Then
                    For Each fname As String In Me.Includes
                        txtcode += File.ReadAllText(fname) + vbCrLf
                    Next
                End If
                txtcode += Me.ScriptText
                source = Me.engine.CreateScriptSourceFromString(txtcode, Microsoft.Scripting.SourceCodeKind.Statements)
                Me.ErrorMessage = ""
                source.Execute(Me.scope)
                _lastrun = "script executed succesfully."
            Catch ex As Exception
                Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
                Me.ErrorMessage = ops.FormatException(ex).ToString
                CType(Me._simcontext, ICapeDiagnostic).LogMessage(Me.ErrorMessage)
                engine = Nothing
                scope = Nothing
                source = Nothing
                Throw ex
            Finally
                engine = Nothing
                scope = Nothing
                source = Nothing
                Me._calclog = Me.ErrorMessage
                _lastrun = "error executing script: " & _calclog
            End Try

        End Sub

        Public Overrides Sub Terminate1()
            _ports.Clear()
            _parameters.Clear()
            MyBase.Terminate1()
        End Sub

        Public Overrides Sub Load(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream)

            ' Read the length of the string  
            Dim arrLen As Byte() = New [Byte](3) {}
            pStm.Read(arrLen, arrLen.Length, IntPtr.Zero)

            ' Calculate the length  
            Dim cb As Integer = BitConverter.ToInt32(arrLen, 0)

            ' Read the stream to get the string    
            Dim bytes As Byte() = New Byte(cb - 1) {}
            Dim pcb As New IntPtr()
            pStm.Read(bytes, bytes.Length, pcb)
            If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            ' Deserialize byte array    

            Dim memoryStream As New System.IO.MemoryStream(bytes)

            Try

                Dim domain As AppDomain = AppDomain.CurrentDomain
                AddHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

                Dim myarr As ArrayList

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                myarr = mySerializer.Deserialize(memoryStream)

                Me.ScriptText = myarr(0)
                Me.FontName = myarr(1)
                Me.FontSize = myarr(2)

                myarr = Nothing
                mySerializer = Nothing

                RemoveHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            memoryStream.Close()

        End Sub

        Public Overrides Sub Save(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream, ByVal fClearDirty As Boolean)

            Dim props As New ArrayList

            With props

                .Add(Me.ScriptText)
                .Add(Me.FontName)
                .Add(Me.FontSize)

            End With

            Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
            Dim mstr As New MemoryStream
            mySerializer.Serialize(mstr, props)
            Dim bytes As Byte() = mstr.ToArray()
            mstr.Close()

            ' construct length (separate into two separate bytes)    

            Dim arrLen As Byte() = BitConverter.GetBytes(bytes.Length)
            Try

                ' Save the array in the stream    
                pStm.Write(arrLen, arrLen.Length, IntPtr.Zero)
                pStm.Write(bytes, bytes.Length, IntPtr.Zero)
                If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            If fClearDirty Then
                m_dirty = False
            End If

        End Sub

#End Region

#Region "   Register/Unregister Procedures"

        <System.Runtime.InteropServices.ComRegisterFunction()> _
        Private Shared Sub RegisterFunction(ByVal t As Type)

            Dim keyname As String = String.Concat("CLSID\\{", t.GUID.ToString, "}\\Implemented Categories")
            Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.ClassesRoot.OpenSubKey(keyname, True)
            If key Is Nothing Then
                key = Microsoft.Win32.Registry.ClassesRoot.CreateSubKey(keyname)
            End If
            key.CreateSubKey("{678C09A5-7D66-11D2-A67D-00105A42887F}") ' CAPE-OPEN Unit Operation
            key.CreateSubKey("{678C09A1-7D66-11D2-A67D-00105A42887F}") ' CAPE-OPEN Object 
            keyname = String.Concat("CLSID\\{", t.GUID.ToString, "}\\CapeDescription")
            key = Microsoft.Win32.Registry.ClassesRoot.CreateSubKey(keyname)
            key.SetValue("Name", "IronPython Scripting Unit Operation")
            key.SetValue("Description", "DWSIM IronPython Scripting Unit Operation CAPE-OPEN Wrapper")
            key.SetValue("CapeVersion", "1.0")
            key.SetValue("ComponentVersion", My.Application.Info.Version.ToString)
            key.SetValue("VendorURL", "http://dwsim.inforside.com.br")
            key.SetValue("HelpURL", "http://dwsim.inforside.com.br")
            key.SetValue("About", "DWSIM is open-source software, released under the GPL v3 license. (c) 2011-2016 Daniel Medeiros.")
            key.Close()

        End Sub

        <System.Runtime.InteropServices.ComUnregisterFunction()> _
        Private Shared Sub UnregisterFunction(ByVal t As Type)

            Try

                Dim keyname As String = String.Concat("CLSID\\{", t.GUID.ToString, "}")
                Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.ClassesRoot.OpenSubKey(keyname, True)
                Dim keyNames() As String = key.GetSubKeyNames
                For Each kn As String In keyNames
                    key.DeleteSubKeyTree(kn)
                Next
                Dim valueNames() As String = key.GetValueNames
                For Each valueName As String In valueNames
                    key.DeleteValue(valueName)
                Next

            Catch ex As Exception

            End Try

        End Sub

#End Region

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_CustomUO With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_CustomUO With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UpdateInfo()
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.uo_custom_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                Return "Operação unitária definida por um script IronPython"
            Else
                Return "IronPython script-defined Unit Operation"
            End If
        End Function

        Public Overrides Function GetDisplayName() As String
            If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                Return "Script IronPython"
            Else
                Return "IronPython Script"
            End If
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

    End Class

End Namespace
