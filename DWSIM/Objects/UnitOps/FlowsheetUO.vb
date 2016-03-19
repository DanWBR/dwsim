'    Flowsheet Unit Operation
'    Copyright 2015 Daniel Wagner O. de Medeiros
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
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports DWSIM.DWSIM.SimulationObjects.UnitOperations.Auxiliary
Imports System.Globalization
Imports System.Linq
Imports System.Xml.Linq
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports DWSIM.DrawingTools
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.Extras
Imports System.IO
Imports System.Threading.Tasks
Imports DWSIM.DWSIM.DrawingTools.GraphicObjects2

Namespace DWSIM.SimulationObjects.UnitOperations.Auxiliary

    <System.Serializable()> Public Class FlowsheetUOParameter
        Implements XMLSerializer.Interfaces.ICustomXMLSerialization
        Public Property ID As String = ""
        Public Property ObjectID As String = ""
        Public Property ObjectProperty As String = ""
        'Public Property Value As Object = Nothing
        'Public Property Unit As String = ""
        Public Function LoadData(data As List(Of XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data)
        End Function
        Public Function SaveData() As List(Of XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me)
        End Function
    End Class

    Public Enum FlowsheetUOMassTransferMode
        CompoundMassFlows = 0
        CompoundMoleFlows = 1
        CompoundMassFractions = 2
        CompoundMoleFractions = 3
    End Enum

End Namespace

Namespace DWSIM.SimulationObjects.UnitOperations
    <System.Serializable()> Public Class Flowsheet

        Inherits DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass

        Public Property SimulationFile As String = ""
        <System.Xml.Serialization.XmlIgnore> Public Property Initialized As Boolean = False
        Public Property InitializeOnLoad As Boolean = False
        Public Property UpdateOnSave As Boolean = False
        Public Property MassTransferMode As FlowsheetUOMassTransferMode = FlowsheetUOMassTransferMode.CompoundMassFlows
        Public Property InputParams As Dictionary(Of String, FlowsheetUOParameter)
        Public Property OutputParams As Dictionary(Of String, FlowsheetUOParameter)
        <System.Xml.Serialization.XmlIgnore> <System.NonSerialized> Public Fsheet As FormFlowsheet = Nothing
        Public Property InputConnections As List(Of String)
        Public Property OutputConnections As List(Of String)
        Public Property MassBalanceError As Double = 0.0#
        Public Property RedirectOutput As Boolean = False
        Public Property CompoundMappings As Dictionary(Of String, String)

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

            CompoundMappings = New Dictionary(Of String, String)

            InputParams = New Dictionary(Of String, FlowsheetUOParameter)
            OutputParams = New Dictionary(Of String, FlowsheetUOParameter)

            InputConnections = New List(Of String) From {"", "", "", "", "", "", "", "", "", ""}
            OutputConnections = New List(Of String) From {"", "", "", "", "", "", "", "", "", ""}




        End Sub

        Public Sub New()

            MyBase.New()

            CompoundMappings = New Dictionary(Of String, String)

            InputParams = New Dictionary(Of String, FlowsheetUOParameter)
            OutputParams = New Dictionary(Of String, FlowsheetUOParameter)

            InputConnections = New List(Of String) From {"", "", "", "", "", "", "", "", "", ""}
            OutputConnections = New List(Of String) From {"", "", "", "", "", "", "", "", "", ""}

        End Sub

        Public Sub InitializeMappings()

            If CompoundMappings Is Nothing Then CompoundMappings = New Dictionary(Of String, String)

            'create mappings
            If CompoundMappings.Count = 0 Then
                For Each c In Me.FlowSheet.Options.SelectedComponents.Values
                    If Me.Fsheet.Options.SelectedComponents.ContainsKey(c.Name) Then CompoundMappings.Add(c.Name, c.Name) Else CompoundMappings.Add(c.Name, "")
                Next
            End If

            If CompoundMappings.Count <> Me.FlowSheet.Options.SelectedComponents.Count Then
                'update mappings
                For Each c In Me.FlowSheet.Options.SelectedComponents.Values
                    If Not CompoundMappings.ContainsKey(c.Name) Then
                        If Me.Fsheet.Options.SelectedComponents.ContainsKey(c.Name) Then CompoundMappings.Add(c.Name, c.Name) Else CompoundMappings.Add(c.Name, "")
                    End If
                Next
            End If

        End Sub

        Public Sub ParseFilePath()

            If Not IO.File.Exists(SimulationFile) Then
                'look at the current simulation location to see if the file is there.
                Dim fname As String = IO.Path.GetFileName(SimulationFile)
                Dim currpath As String = IO.Path.GetDirectoryName(FlowSheet.Options.FilePath)
                Dim newpath As String = IO.Path.Combine(currpath, fname)
                If IO.File.Exists(newpath) Then SimulationFile = newpath
            End If

        End Sub

        Public Shared Function InitializeFlowsheet(path As String) As FormFlowsheet
            Return InitializeFlowsheetInternal(XDocument.Load(path))
        End Function

        Public Shared Function InitializeFlowsheet(compressedstream As MemoryStream) As FormFlowsheet
            Using decompressedstream As New IO.MemoryStream
                compressedstream.Position = 0
                Using gzs As New IO.BufferedStream(New Compression.GZipStream(compressedstream, Compression.CompressionMode.Decompress, True), 64 * 1024)
                    gzs.CopyTo(decompressedstream)
                    gzs.Close()
                    decompressedstream.Position = 0
                    Return InitializeFlowsheetInternal(XDocument.Load(decompressedstream))
                End Using
            End Using
        End Function

        Private Shared Function InitializeFlowsheetInternal(xdoc As XDocument) As FormFlowsheet

            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            Dim excs As New Concurrent.ConcurrentBag(Of Exception)

            Dim form As FormFlowsheet = Nothing

            form = New FormFlowsheet()

            Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

            Try
                form.Options.LoadData(data)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
            End Try

            data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As GraphicObject = Nothing
                    Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                    If Not t Is Nothing Then obj = Activator.CreateInstance(t)
                    If obj Is Nothing Then
                        obj = GraphicObject.ReturnInstance(xel.Element("Type").Value)
                    End If
                    obj.LoadData(xel.Elements.ToList)
                    If Not TypeOf obj Is TableGraphic Then
                        form.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(obj)
                        obj.CreateConnectors(0, 0)
                        form.Collections.GraphicObjectCollection.Add(obj.Name, obj)
                    End If
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
                End Try
            Next

            For Each xel As XElement In data
                Try
                    Dim id As String = xel.Element("Name").Value
                    If id <> "" Then
                        Dim obj As GraphicObject = (From go As GraphicObject In
                                                                form.FormSurface.FlowsheetDesignSurface.drawingObjects Where go.Name = id).SingleOrDefault
                        If Not obj Is Nothing Then
                            Dim i As Integer = 0
                            For Each xel2 As XElement In xel.Element("InputConnectors").Elements
                                If xel2.@IsAttached = True Then
                                    obj.InputConnectors(i).ConnectorName = xel2.@AttachedFromObjID & "|" & xel2.@AttachedFromConnIndex
                                    obj.InputConnectors(i).Type = [Enum].Parse(obj.InputConnectors(i).Type.GetType, xel2.@ConnType)
                                End If
                                i += 1
                            Next
                        End If
                    End If
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
                End Try
            Next

            For Each xel As XElement In data
                Try
                    Dim id As String = xel.Element("Name").Value
                    If id <> "" Then
                        Dim obj As GraphicObject = (From go As GraphicObject In
                                                                form.FormSurface.FlowsheetDesignSurface.drawingObjects Where go.Name = id).SingleOrDefault
                        If Not obj Is Nothing Then
                            For Each xel2 As XElement In xel.Element("OutputConnectors").Elements
                                If xel2.@IsAttached = True Then
                                    Dim objToID = xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        Dim objTo As GraphicObject = (From go As GraphicObject In
                                                                                        form.FormSurface.FlowsheetDesignSurface.drawingObjects Where go.Name = objToID).SingleOrDefault
                                        Dim fromidx As Integer = -1
                                        Dim cp As ConnectionPoint = (From cp2 As ConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = obj.Name).SingleOrDefault
                                        If Not cp Is Nothing Then
                                            fromidx = cp.ConnectorName.Split("|")(1)
                                        End If
                                        form.ConnectObject(obj, objTo, fromidx, xel2.@AttachedToConnIndex)
                                    End If
                                End If
                            Next
                            For Each xel2 As XElement In xel.Element("EnergyConnector").Elements
                                If xel2.@IsAttached = True Then
                                    Dim objToID = xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        Dim objTo As GraphicObject = (From go As GraphicObject In
                                                                                        form.FormSurface.FlowsheetDesignSurface.drawingObjects Where go.Name = objToID).SingleOrDefault
                                        form.ConnectObject(obj, objTo, -1, xel2.@AttachedToConnIndex)
                                    End If
                                End If
                            Next
                        End If
                    End If
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
                End Try
            Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

            Dim complist As New Concurrent.ConcurrentBag(Of ConstantProperties)

            Parallel.ForEach(data, Sub(xel)
                                       Try
                                           Dim obj As New ConstantProperties
                                           obj.LoadData(xel.Elements.ToList)
                                           complist.Add(obj)
                                       Catch ex As Exception
                                           excs.Add(New Exception("Error Loading Compound Information", ex))
                                       End Try
                                   End Sub)

            Dim orderedlist = complist.OrderBy(Function(o) o.Molar_Weight)

            For Each obj In orderedlist
                form.Options.SelectedComponents.Add(obj.Name, obj)
            Next

            complist = Nothing
            orderedlist = Nothing

            data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                    Dim obj As PropertyPackage = Activator.CreateInstance(t)
                    obj.LoadData(xel.Elements.ToList)
                    Dim newID As String = Guid.NewGuid.ToString
                    If form.Options.PropertyPackages.ContainsKey(obj.UniqueID) Then obj.UniqueID = newID
                    form.Options.PropertyPackages.Add(obj.UniqueID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Property Package Information", ex))
                End Try
            Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

            Dim objlist As New Concurrent.ConcurrentBag(Of DWSIM.SimulationObjects.UnitOperations.BaseClass)

            Parallel.ForEach(data, Sub(xel)
                                       Try
                                           Dim id As String = xel.<Name>.Value
                                           Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                                           Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Activator.CreateInstance(t)
                                           Dim gobj As GraphicObject = (From go As GraphicObject In
                                                               form.FormSurface.FlowsheetDesignSurface.drawingObjects Where go.Name = id).SingleOrDefault
                                           obj.GraphicObject = gobj
                                           obj.SetFlowsheet(form)
                                           If Not gobj Is Nothing Then
                                               obj.LoadData(xel.Elements.ToList)
                                               If TypeOf obj Is Streams.MaterialStream Then
                                                   For Each phase As DWSIM.Thermodynamics.BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                                                       For Each c As ConstantProperties In form.Options.SelectedComponents.Values
                                                           phase.Compounds(c.Name).ConstantProperties = c
                                                       Next
                                                   Next
                                               End If
                                           End If
                                           objlist.Add(obj)
                                       Catch ex As Exception
                                           excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                                       End Try
                                   End Sub)

            For Each obj In objlist
                Try
                    Dim id = obj.Name
                    Dim gobj = obj.GraphicObject
                    form.Collections.FlowsheetObjectCollection.Add(id, obj)
                    form.Collections.GraphicObjectCollection.Add(id, gobj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                End Try
            Next

            For Each so As DWSIM.SimulationObjects.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
                Try
                    If TryCast(so, DWSIM.SimulationObjects.SpecialOps.Adjust) IsNot Nothing Then
                        Dim so2 As DWSIM.SimulationObjects.SpecialOps.Adjust = so
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ManipulatedObjectData.m_ID) Then
                            so2.ManipulatedObject = form.Collections.FlowsheetObjectCollection(so2.ManipulatedObjectData.m_ID)
                            DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToMv = so2.ManipulatedObject.GraphicObject
                        End If
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ControlledObjectData.m_ID) Then
                            so2.ControlledObject = form.Collections.FlowsheetObjectCollection(so2.ControlledObjectData.m_ID)
                            DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToCv = so2.ControlledObject.GraphicObject
                        End If
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ReferencedObjectData.m_ID) Then
                            so2.ReferenceObject = form.Collections.FlowsheetObjectCollection(so2.ReferencedObjectData.m_ID)
                            DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToRv = so2.ReferenceObject.GraphicObject
                        End If
                    End If
                    If TryCast(so, DWSIM.SimulationObjects.SpecialOps.Spec) IsNot Nothing Then
                        Dim so2 As DWSIM.SimulationObjects.SpecialOps.Spec = so
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.TargetObjectData.m_ID) Then
                            so2.TargetObject = form.Collections.FlowsheetObjectCollection(so2.TargetObjectData.m_ID)
                            DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToTv = so2.TargetObject.GraphicObject
                        End If
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.SourceObjectData.m_ID) Then
                            so2.SourceObject = form.Collections.FlowsheetObjectCollection(so2.SourceObjectData.m_ID)
                            DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToSv = so2.SourceObject.GraphicObject
                        End If
                    End If
                    If TryCast(so, DWSIM.SimulationObjects.UnitOperations.CapeOpenUO) IsNot Nothing Then
                        DirectCast(so, DWSIM.SimulationObjects.UnitOperations.CapeOpenUO).UpdateConnectors2()
                        DirectCast(so, DWSIM.SimulationObjects.UnitOperations.CapeOpenUO).UpdatePortsFromConnectors()
                    End If
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Unit Operation Connection Information", ex))
                End Try
            Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.<Type>.Value.Equals("DWSIM.DWSIM.GraphicObjects.TableGraphic")).ToList
                Try
                    Dim obj As GraphicObject = Nothing
                    Dim t As Type = Type.GetType(xel2.Element("Type").Value, False)
                    If Not t Is Nothing Then obj = Activator.CreateInstance(t)
                    If obj Is Nothing Then
                        obj = GraphicObject.ReturnInstance(xel2.Element("Type").Value)
                    End If
                    obj.LoadData(xel2.Elements.ToList)
                    DirectCast(obj, TableGraphic).BaseOwner = form.Collections.FlowsheetObjectCollection(xel2.<Owner>.Value)
                    form.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Flowsheet Table Information", ex))
                End Try
            Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets").Elements.ToList

            form.Options.ReactionSets.Clear()

            For Each xel As XElement In data
                Try
                    Dim obj As New ReactionSet()
                    obj.LoadData(xel.Elements.ToList)
                    form.Options.ReactionSets.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Reaction Set Information", ex))
                End Try
            Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New Reaction()
                    obj.LoadData(xel.Elements.ToList)
                    form.Options.Reactions.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Reaction Information", ex))
                End Try
            Next

            form.ScriptCollection = New Dictionary(Of String, Script)

            If xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems") IsNot Nothing Then

                data = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems").Elements.ToList

                Dim i As Integer = 0
                For Each xel As XElement In data
                    Try
                        Dim obj As New DWSIM.Extras.Script()
                        obj.LoadData(xel.Elements.ToList)
                        form.ScriptCollection.Add(obj.ID, obj)
                    Catch ex As Exception
                        excs.Add(New Exception("Error Loading Script Item Information", ex))
                    End Try
                    i += 1
                Next

            End If

            Try
                Dim data1 As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data1").Value
                Dim data2 As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data2").Value
                If data1 <> "" Then form.FormSpreadsheet.CopyDT1FromString(data1)
                If data2 <> "" Then form.FormSpreadsheet.CopyDT2FromString(data2)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Spreadsheet Information", ex))
            End Try

            For Each pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage In form.Options.PropertyPackages.Values
                Try
                    If pp.ConfigForm Is Nothing Then pp.ReconfigureConfigForm()
                Catch ex As Exception
                    excs.Add(New Exception("Error Reconfiguring Property Package", ex))
                End Try
            Next

            form.Options.NotSelectedComponents = New Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ConstantProperties)

            Dim tmpc As DWSIM.Thermodynamics.BaseClasses.ConstantProperties
            For Each tmpc In FormMain.AvailableComponents.Values
                Dim newc As New DWSIM.Thermodynamics.BaseClasses.ConstantProperties
                newc = tmpc
                If Not form.Options.SelectedComponents.ContainsKey(tmpc.Name) Then
                    form.Options.NotSelectedComponents.Add(tmpc.Name, newc)
                End If
            Next

            form.FormProps.DockPanel = Nothing
            form.FormSurface.DockPanel = Nothing

            Try
                form.FormProps.Show(form.dckPanel)
                form.FormSurface.Show(form.dckPanel)
                form.dckPanel.BringToFront()
                form.dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)
            Catch ex As Exception
                excs.Add(New Exception("Error Restoring Window Layout", ex))
            End Try

            If excs.Count > 0 Then Throw New AggregateException(excs).Flatten Else Return form

        End Function

        Public Shared Function UpdateProcessData(form As FormFlowsheet, xdoc As XDocument)

            Dim ci As CultureInfo = CultureInfo.InvariantCulture
            Dim excs As New List(Of Exception)

            Dim data As List(Of XElement)

            'data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

            'For Each xel As XElement In data
            '    Try
            '        Dim id As String = xel.<Name>.Value
            '        If form.Collections.FlowsheetObjectCollection.ContainsKey(id) Then
            '            Dim obj = form.Collections.FlowsheetObjectCollection(id).GraphicObject
            '            obj.LoadData(xel.Elements.ToList)
            '        End If
            '    Catch ex As Exception
            '        excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
            '    End Try
            'Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim id As String = xel.<Name>.Value
                    Dim obj = form.Collections.FlowsheetObjectCollection(id)
                    obj.LoadData(xel.Elements.ToList)
                    If TypeOf obj Is Streams.MaterialStream Then
                        For Each phase As DWSIM.Thermodynamics.BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                            For Each c As ConstantProperties In form.Options.SelectedComponents.Values
                                phase.Compounds(c.Name).ConstantProperties = c
                            Next
                        Next
                    End If
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                End Try
            Next

            If excs.Count > 0 Then Throw New AggregateException(excs).Flatten Else Return form

        End Function

        Public Shared Function ReturnProcessData(Form As FormFlowsheet) As IO.MemoryStream

            Dim xdoc As New XDocument()
            Dim xel As XElement

            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            xdoc.Add(New XElement("DWSIM_Simulation_Data"))

            xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
            xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

            For Each so As DWSIM.SimulationObjects.UnitOperations.BaseClass In Form.Collections.FlowsheetObjectCollection.Values
                xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
            Next

            'xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
            'xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

            'For Each go As DWSIM.DrawingTools.GraphicObjects.GraphicObject In Form.FormSurface.FlowsheetDesignSurface.drawingObjects
            '    If Not go.IsConnector Then xel.Add(New XElement("GraphicObject", go.SaveData().ToArray()))
            'Next

            Dim bytestream As New IO.MemoryStream()
            xdoc.Save(bytestream)

            Return bytestream

        End Function

        Public Sub UpdateProcessData(path As String)

            If Me.Initialized Then

                Try

                    Dim xdoc As XDocument = XDocument.Load(path)

                    Dim xel As XElement

                    xel = xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo")

                    xel.RemoveAll()
                    xel.Add(New XElement("BuildVersion", My.Application.Info.Version.ToString))
                    xel.Add(New XElement("BuildDate", CType("01/01/2000", DateTime).AddDays(My.Application.Info.Version.Build).AddSeconds(My.Application.Info.Version.Revision * 2)))
                    xel.Add(New XElement("OSInfo", My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"))
                    xel.Add(New XElement("SavedOn", Date.Now))

                    xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")
                    xel.RemoveAll()

                    For Each so As DWSIM.SimulationObjects.UnitOperations.BaseClass In Me.Fsheet.Collections.FlowsheetObjectCollection.Values
                        xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
                    Next

                    xdoc.Save(path)

                    FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": " & DWSIM.App.GetLocalString("SubFSUpdateSuccess"), Color.Blue, DWSIM.Flowsheet.MessageType.Information)

                Catch ex As Exception

                    FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": " & DWSIM.App.GetLocalString("SubFSUpdateFailed") & " " & ex.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)

                End Try

            End If

        End Sub

        Public Overrides Function Calculate(Optional args As Object = Nothing) As Integer

            Validate()

            Me.Fsheet.MasterUnitOp = Me

            Calculated = False

            MassBalanceError = 0.0#

            Dim msfrom, msto As Streams.MaterialStream

            Dim win, wout As Double

            win = 0.0#
            For Each c In Me.GraphicObject.InputConnectors

                If c.IsAttached Then

                    msfrom = FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedFrom.Name)
                    msto = Fsheet.Collections.FlowsheetObjectCollection(InputConnections(Me.GraphicObject.InputConnectors.IndexOf(c)))

                    win += msfrom.Phases(0).Properties.massflow.GetValueOrDefault

                    msto.Clear()

                    msto.Phases(0).Properties.temperature = msfrom.Phases(0).Properties.temperature.GetValueOrDefault
                    msto.Phases(0).Properties.pressure = msfrom.Phases(0).Properties.pressure.GetValueOrDefault
                    msto.Phases(0).Properties.enthalpy = msfrom.Phases(0).Properties.enthalpy.GetValueOrDefault
                    msto.SpecType = Interfaces.Enums.StreamSpec.Temperature_and_Pressure

                    Dim wt, mt As Double

                    Select Case MassTransferMode

                        Case FlowsheetUOMassTransferMode.CompoundMassFlows

                            wt = 0.0#
                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    wt += msfrom.Phases(0).Compounds(s.Key).MassFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Phases(0).Properties.massflow = wt

                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    If Not msto.Phases(0).Compounds(s.Value).FracaoMassica.HasValue Then msto.Phases(0).Compounds(s.Value).FracaoMassica = 0.0#
                                    msto.Phases(0).Compounds(s.Value).FracaoMassica += msfrom.Phases(0).Compounds(s.Key).MassFlow.GetValueOrDefault / wt
                                End If
                            Next

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMassFractions

                            msto.Phases(0).Properties.massflow = msfrom.Phases(0).Properties.massflow.GetValueOrDefault

                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    If Not msto.Phases(0).Compounds(s.Value).FracaoMassica.HasValue Then msto.Phases(0).Compounds(s.Value).FracaoMassica = 0.0#
                                    msto.Phases(0).Compounds(s.Value).FracaoMassica += msfrom.Phases(0).Compounds(s.Key).FracaoMassica.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMassComposition()

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFlows

                            mt = 0.0#
                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    mt += msfrom.Phases(0).Compounds(s.Key).MolarFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Phases(0).Properties.molarflow = mt

                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    If Not msto.Phases(0).Compounds(s.Value).FracaoMolar.HasValue Then msto.Phases(0).Compounds(s.Value).FracaoMolar = 0.0#
                                    msto.Phases(0).Compounds(s.Value).FracaoMolar += msfrom.Phases(0).Compounds(s.Key).MolarFlow.GetValueOrDefault / mt
                                End If
                            Next

                            msto.CalcOverallCompMassFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFractions

                            msto.Phases(0).Properties.molarflow = msfrom.Phases(0).Properties.molarflow.GetValueOrDefault

                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    If Not msto.Phases(0).Compounds(s.Value).FracaoMolar.HasValue Then msto.Phases(0).Compounds(s.Value).FracaoMolar = 0.0#
                                    msto.Phases(0).Compounds(s.Value).FracaoMolar += msfrom.Phases(0).Compounds(s.Key).FracaoMolar.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMoleComposition()

                            msto.CalcOverallCompMassFractions()

                    End Select

                End If

            Next

            If Me.RedirectOutput Then
                Fsheet.MasterFlowsheet = Me.FlowSheet
                Fsheet.RedirectMessages = Me.RedirectOutput
            Else
                Fsheet.MasterFlowsheet = Nothing
                Fsheet.RedirectMessages = Me.RedirectOutput
            End If

            Fsheet.Options.CalculatorActivated = True

            Select Case My.Settings.SolverMode
                Case 0, 3, 4
                    DWSIM.Flowsheet.FlowsheetSolver.CalculateAll2(Fsheet, 0, My.Application.TaskCancellationTokenSource)
                Case 1, 2
                    DWSIM.Flowsheet.FlowsheetSolver.CalculateAll2(Fsheet, 1, My.Application.TaskCancellationTokenSource)
            End Select

            wout = 0.0#
            For Each c In Me.GraphicObject.OutputConnectors

                If c.IsAttached Then

                    msto = FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedTo.Name)
                    msfrom = Fsheet.Collections.FlowsheetObjectCollection(OutputConnections(Me.GraphicObject.OutputConnectors.IndexOf(c)))

                    wout += msfrom.Phases(0).Properties.massflow.GetValueOrDefault

                    msto.Clear()

                    msto.Phases(0).Properties.temperature = msfrom.Phases(0).Properties.temperature.GetValueOrDefault
                    msto.Phases(0).Properties.pressure = msfrom.Phases(0).Properties.pressure.GetValueOrDefault
                    msto.Phases(0).Properties.enthalpy = msfrom.Phases(0).Properties.enthalpy.GetValueOrDefault
                    msto.SpecType = Interfaces.Enums.StreamSpec.Temperature_and_Pressure

                    Dim wt, mt As Double

                    Select Case MassTransferMode

                        Case FlowsheetUOMassTransferMode.CompoundMassFlows

                            wt = 0.0#
                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    wt += msfrom.Phases(0).Compounds(s.Name).MassFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Phases(0).Properties.massflow = wt

                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    msto.Phases(0).Compounds(s.Name).FracaoMassica = msfrom.Phases(0).Compounds(s.Name).MassFlow.GetValueOrDefault / wt
                                End If
                            Next

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMassFractions

                            msto.Phases(0).Properties.massflow = msfrom.Phases(0).Properties.massflow.GetValueOrDefault

                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    msto.Phases(0).Compounds(s.Name).FracaoMassica = msfrom.Phases(0).Compounds(s.Name).FracaoMassica.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMassComposition()

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFlows

                            mt = 0.0#
                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    mt += msfrom.Phases(0).Compounds(s.Name).MolarFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Phases(0).Properties.molarflow = mt

                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    msto.Phases(0).Compounds(s.Name).FracaoMolar = msfrom.Phases(0).Compounds(s.Name).MolarFlow.GetValueOrDefault / mt
                                End If
                            Next

                            msto.CalcOverallCompMassFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFractions

                            msto.Phases(0).Properties.molarflow = msfrom.Phases(0).Properties.molarflow.GetValueOrDefault

                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    msto.Phases(0).Compounds(s.Name).FracaoMolar = msfrom.Phases(0).Compounds(s.Name).FracaoMolar.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMoleComposition()

                            msto.CalcOverallCompMassFractions()

                    End Select

                End If

            Next

            MassBalanceError = (wout - win) / win * 100

            Calculated = True

            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

            'Call function to calculate flowsheet
            With objargs
                .Calculated = True
                .Name = Me.Name
                .Tag = Me.GraphicObject.Tag
                .ObjectType = ObjectType.FlowsheetUO
            End With

            FlowSheet.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Sub Validate()

            For Each c In Me.GraphicObject.InputConnectors
                If c.IsAttached Then
                    Dim mstr = FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedFrom.Name)
                    If Not mstr.GraphicObject.Calculated Then
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        'Call function to calculate flowsheet
                        With objargs
                            .Calculated = False
                            .Name = Me.Name
                            .ObjectType = ObjectType.FlowsheetUO
                        End With
                        Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
                    End If
                End If
            Next

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object

            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Try
                If prop.Contains("[I]") Then
                    Return Fsheet.Collections.FlowsheetObjectCollection(InputParams(pkey).ObjectID).GetPropertyValue(InputParams(pkey).ObjectProperty, su)
                Else
                    Return Fsheet.Collections.FlowsheetObjectCollection(OutputParams(pkey).ObjectID).GetPropertyValue(OutputParams(pkey).ObjectProperty, su)
                End If
            Catch ex As Exception
                Return ex.ToString
            End Try

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
            Dim proplist As New ArrayList
            If Initialized Then
                Select Case proptype
                    Case PropertyType.ALL
                        For Each p In InputParams.Values
                            proplist.Add(Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [I][" & p.ID & "]")
                        Next
                        For Each p In OutputParams.Values
                            proplist.Add(Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [O][" & p.ID & "]")
                        Next
                    Case PropertyType.WR
                        For Each p In InputParams.Values
                            proplist.Add(Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [I][" & p.ID & "]")
                        Next
                    Case PropertyType.RO
                        For Each p In OutputParams.Values
                            proplist.Add(Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [O][" & p.ID & "]")
                        Next
                End Select
            End If
            Return proplist.ToArray(GetType(System.String))
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SystemsOfUnits.Units = Nothing) As Object

            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Fsheet.Collections.FlowsheetObjectCollection(InputParams(pkey).ObjectID).SetPropertyValue(InputParams(pkey).ObjectProperty, propval, su)

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object

            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Try
                If prop.Contains("[I]") Then
                    Return Fsheet.Collections.FlowsheetObjectCollection(InputParams(pkey).ObjectID).GetPropertyUnit(InputParams(pkey).ObjectProperty, su)
                Else
                    Return Fsheet.Collections.FlowsheetObjectCollection(OutputParams(pkey).ObjectID).GetPropertyUnit(OutputParams(pkey).ObjectProperty, su)
                End If
            Catch ex As Exception
                Return ex.ToString
            End Try

        End Function

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

            Dim Conversor As New DWSIM.SystemsOfUnits.Converter

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent1, ent2, ent3, ent4, ent5, ent6, ent7, ent8, ent9, ent10, saida1, saida2, saida3, saida4, saida5, saida6, saida7, saida8, saida9, saida10 As String

                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then ent1 = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag Else ent1 = ""
                If Me.GraphicObject.InputConnectors(1).IsAttached = True Then ent2 = Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag Else ent2 = ""
                If Me.GraphicObject.InputConnectors(2).IsAttached = True Then ent3 = Me.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag Else ent3 = ""
                If Me.GraphicObject.InputConnectors(3).IsAttached = True Then ent4 = Me.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag Else ent4 = ""
                If Me.GraphicObject.InputConnectors(4).IsAttached = True Then ent5 = Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag Else ent5 = ""
                If Me.GraphicObject.InputConnectors(5).IsAttached = True Then ent6 = Me.GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Tag Else ent6 = ""
                If Me.GraphicObject.InputConnectors(6).IsAttached = True Then ent7 = Me.GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom.Tag Else ent7 = ""
                If Me.GraphicObject.InputConnectors(7).IsAttached = True Then ent8 = Me.GraphicObject.InputConnectors(7).AttachedConnector.AttachedFrom.Tag Else ent8 = ""
                If Me.GraphicObject.InputConnectors(8).IsAttached = True Then ent9 = Me.GraphicObject.InputConnectors(8).AttachedConnector.AttachedFrom.Tag Else ent9 = ""
                If Me.GraphicObject.InputConnectors(9).IsAttached = True Then ent10 = Me.GraphicObject.InputConnectors(9).AttachedConnector.AttachedFrom.Tag Else ent10 = ""

                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then saida1 = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag Else saida1 = ""
                If Me.GraphicObject.OutputConnectors(1).IsAttached = True Then saida2 = Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag Else saida2 = ""
                If Me.GraphicObject.OutputConnectors(2).IsAttached = True Then saida3 = Me.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag Else saida3 = ""
                If Me.GraphicObject.OutputConnectors(3).IsAttached = True Then saida4 = Me.GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag Else saida4 = ""
                If Me.GraphicObject.OutputConnectors(4).IsAttached = True Then saida5 = Me.GraphicObject.OutputConnectors(4).AttachedConnector.AttachedTo.Tag Else saida5 = ""
                If Me.GraphicObject.OutputConnectors(5).IsAttached = True Then saida6 = Me.GraphicObject.OutputConnectors(5).AttachedConnector.AttachedTo.Tag Else saida6 = ""
                If Me.GraphicObject.OutputConnectors(6).IsAttached = True Then saida7 = Me.GraphicObject.OutputConnectors(6).AttachedConnector.AttachedTo.Tag Else saida7 = ""
                If Me.GraphicObject.OutputConnectors(7).IsAttached = True Then saida8 = Me.GraphicObject.OutputConnectors(7).AttachedConnector.AttachedTo.Tag Else saida8 = ""
                If Me.GraphicObject.OutputConnectors(8).IsAttached = True Then saida9 = Me.GraphicObject.OutputConnectors(8).AttachedConnector.AttachedTo.Tag Else saida9 = ""
                If Me.GraphicObject.OutputConnectors(9).IsAttached = True Then saida10 = Me.GraphicObject.OutputConnectors(9).AttachedConnector.AttachedTo.Tag Else saida10 = ""

                '==== Streams (1) =======================
                '==== Input streams ===
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), ent1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), ent2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada3"), ent3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada4"), ent4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada5"), ent5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada6"), ent6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada7"), ent7, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada8"), ent8, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada9"), ent9, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada10"), ent10, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                '==== Output streams ===
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida3"), saida3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida4"), saida4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida5"), saida5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida6"), saida6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida7"), saida7, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida8"), saida8, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida9"), saida9, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida10"), saida10, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("SimulationFile"), Me, "SimulationFile", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("SimulationFileDesc"), True)
                .Item(.Item.Count - 1).CustomEditor = New PropertyGridEx.UIFilenameEditor

                If IO.File.Exists(SimulationFile) Then
                    .Item.Add(DWSIM.App.GetLocalString("FlowsheetUOEditor"), New DummyClass, False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("FlowsheetUOEditorDesc"), True)
                    .Item(.Item.Count - 1).DefaultValue = Nothing
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.FlowsheetUO.UIFlowsheetUOEditor
                End If

                .Item.Add(DWSIM.App.GetLocalString("InitializeOnLoad"), Me, "InitializeOnLoad", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("InitializeOnLoadDesc"), True)
                .Item(.Item.Count - 1).Tag2 = "InitializeOnLoad"
                .Item.Add(DWSIM.App.GetLocalString("UpdateOnSave"), Me, "UpdateOnSave", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("UpdateOnSaveDesc"), True)
                .Item(.Item.Count - 1).Tag2 = "UpdateOnSave"
                .Item.Add(DWSIM.App.GetLocalString("RedirectOutput"), Me, "RedirectOutput", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("RedirectOutputDesc"), True)
                .Item(.Item.Count - 1).Tag2 = "RedirectOutput"

                If Initialized Then

                    .Item.Add(DWSIM.App.GetLocalString("FlowsheetUOViewer"), New DummyClass, False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("FlowsheetUOViewerDesc"), True)
                    .Item(.Item.Count - 1).DefaultValue = Nothing
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.FlowsheetUO.UIFlowsheetUOViewer

                    For Each p In InputParams.Values
                        If Fsheet.Collections.FlowsheetObjectCollection.ContainsKey(p.ObjectID) Then
                            .Item.Add(Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GraphicObject.Tag & ", " &
                                      DWSIM.App.GetPropertyName(p.ObjectProperty) &
                                      " (" & Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GetPropertyUnit(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem) & ")",
                                      Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GetPropertyValue(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem), False,
                                      DWSIM.App.GetLocalString("LinkedInputParms"), DWSIM.App.GetLocalString(""), True)
                            .Item(.Item.Count - 1).Tag = p.ID
                            .Item(.Item.Count - 1).Tag2 = "[I][" & p.ID & "]"
                        End If
                    Next

                    For Each p In OutputParams.Values
                        If Fsheet.Collections.FlowsheetObjectCollection.ContainsKey(p.ObjectID) Then
                            .Item.Add(Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GraphicObject.Tag & ", " &
                                      DWSIM.App.GetPropertyName(p.ObjectProperty) &
                                      " (" & Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GetPropertyUnit(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem) & ")",
                                      Fsheet.Collections.FlowsheetObjectCollection(p.ObjectID).GetPropertyValue(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem), True,
                                      DWSIM.App.GetLocalString("LinkedOutputParms"), DWSIM.App.GetLocalString(""), True)
                            .Item(.Item.Count - 1).Tag = p.ID
                        End If
                    Next

                End If

                If Calculated Then

                    .Item.Add(DWSIM.App.GetLocalString("MassBalanceError"), Format(Me.MassBalanceError, Me.FlowSheet.Options.NumberFormat), True, "5. " & DWSIM.App.GetLocalString("Resultados"), DWSIM.App.GetLocalString(""), True)

                End If

            End With

        End Sub

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Me.Annotation.annotation = New Object() {"", (From xel As XElement In data Select xel Where xel.Name = "Annotation").SingleOrDefault.Value}

            ParseFilePath()

            If InitializeOnLoad Then
                If IO.File.Exists(SimulationFile) Then
                    Me.Fsheet = InitializeFlowsheet(SimulationFile)
                    Me.Initialized = True
                End If
            End If

            Dim i As Integer

            i = 0
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InputConnections").Elements.ToList
                Me.InputConnections(i) = xel.Value
                i += 1
            Next

            i = 0
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "OutputConnections").Elements.ToList
                Me.OutputConnections(i) = xel.Value
                i += 1
            Next

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InputParameters").Elements.ToList
                Dim fp As New FlowsheetUOParameter()
                fp.LoadData(xel.Elements.ToList)
                Me.InputParams.Add(fp.ID, fp)
            Next

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "OutputParameters").Elements.ToList
                Dim fp As New FlowsheetUOParameter()
                fp.LoadData(xel.Elements.ToList)
                Me.OutputParams.Add(fp.ID, fp)
            Next

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "CompoundMappings").Elements.ToList
                Me.CompoundMappings.Add(xel.Attribute("From").Value, xel.Attribute("To").Value)
            Next

        End Function

        Public Overrides Function SaveData() As List(Of XElement)

            Dim elements As List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("InputConnections"))
                For Each s In InputConnections
                    .Item(.Count - 1).Add(New XElement("InputConnection", s))
                Next
                .Add(New XElement("OutputConnections"))
                For Each s In OutputConnections
                    .Item(.Count - 1).Add(New XElement("OutputConnection", s))
                Next
                .Add(New XElement("InputParameters"))
                For Each p In InputParams.Values
                    .Item(.Count - 1).Add(New XElement("FlowsheetUOParameter", p.SaveData.ToArray))
                Next
                .Add(New XElement("OutputParameters"))
                For Each p In OutputParams.Values
                    .Item(.Count - 1).Add(New XElement("FlowsheetUOParameter", p.SaveData.ToArray))
                Next
                .Add(New XElement("CompoundMappings"))
                For Each p In CompoundMappings
                    Dim xel As New XElement("CompoundMapping")
                    xel.SetAttributeValue("From", p.Key)
                    xel.SetAttributeValue("To", p.Value)
                    .Item(.Count - 1).Add(xel)
                Next
            End With

            If Me.UpdateOnSave Then
                ParseFilePath()
                UpdateProcessData(SimulationFile)
            End If

            Return elements

        End Function

    End Class

End Namespace


