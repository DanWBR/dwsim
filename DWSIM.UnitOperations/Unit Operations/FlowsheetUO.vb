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
Imports System.Globalization
Imports System.Linq
Imports System.Xml.Linq
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.DrawingTools
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports System.Threading.Tasks
Imports DWSIM.SharedClasses.Flowsheet
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.SharedClasses.UnitOperations

Namespace UnitOperations.Auxiliary

    <System.Serializable()> Public Class FlowsheetUOParameter
        Implements XMLSerializer.Interfaces.ICustomXMLSerialization
        Public Property ID As String = ""
        Public Property ObjectID As String = ""
        Public Property ObjectProperty As String = ""
        'Public Property Value As Object = Nothing
        'Public Property Unit As String = ""
        Public Function LoadData(data As List(Of XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Return True
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

        Inherits SharedClasses.UnitOperations.UnitOpBaseClass

        Public Property SimulationFile As String = ""
        <System.Xml.Serialization.XmlIgnore> Public Property Initialized As Boolean = False
        Public Property InitializeOnLoad As Boolean = False
        Public Property UpdateOnSave As Boolean = False
        Public Property MassTransferMode As FlowsheetUOMassTransferMode = FlowsheetUOMassTransferMode.CompoundMassFlows
        Public Property InputParams As Dictionary(Of String, FlowsheetUOParameter)
        Public Property OutputParams As Dictionary(Of String, FlowsheetUOParameter)
        <System.Xml.Serialization.XmlIgnore> <System.NonSerialized> Public Fsheet As SharedClasses.Flowsheet.FlowsheetBag = Nothing
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
                For Each c In Me.FlowSheet.SelectedCompounds.Values
                    If Me.Fsheet.Compounds.ContainsKey(c.Name) Then CompoundMappings.Add(c.Name, c.Name) Else CompoundMappings.Add(c.Name, "")
                Next
            End If

            If CompoundMappings.Count <> Me.FlowSheet.SelectedCompounds.Count Then
                'update mappings
                For Each c In Me.FlowSheet.SelectedCompounds.Values
                    If Not CompoundMappings.ContainsKey(c.Name) Then
                        If Me.Fsheet.Compounds.ContainsKey(c.Name) Then CompoundMappings.Add(c.Name, c.Name) Else CompoundMappings.Add(c.Name, "")
                    End If
                Next
            End If

        End Sub

        Public Sub ParseFilePath()

            If Not IO.File.Exists(SimulationFile) Then
                'look at the current simulation location to see if the file is there.
                Dim fname As String = IO.Path.GetFileName(SimulationFile)
                Dim currpath As String = IO.Path.GetDirectoryName(FlowSheet.FilePath)
                Dim newpath As String = IO.Path.Combine(currpath, fname)
                If IO.File.Exists(newpath) Then SimulationFile = newpath
            End If

        End Sub

        Public Shared Function InitializeFlowsheet(path As String) As IFlowsheetBag
            Return InitializeFlowsheetInternal(XDocument.Load(path))
        End Function

        Public Shared Function InitializeFlowsheet(compressedstream As MemoryStream) As IFlowsheetBag
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

        Private Shared Function InitializeFlowsheetInternal(xdoc As XDocument) As IFlowsheetBag

            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            Dim excs As New Concurrent.ConcurrentBag(Of Exception)

            Dim form As New FlowsheetBag(New Dictionary(Of String, Interfaces.ISimulationObject),
                                         New Dictionary(Of String, Interfaces.IGraphicObject),
                                         New Dictionary(Of String, Interfaces.ICompoundConstantProperties),
                                         New Dictionary(Of String, Interfaces.IPropertyPackage),
                                         New Dictionary(Of String, Interfaces.IReaction),
                                         New Dictionary(Of String, Interfaces.IReactionSet))

            Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As GraphicObject = Nothing
                    Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                    If Not t Is Nothing Then obj = Activator.CreateInstance(t)
                    If obj Is Nothing Then
                        obj = GraphicObjects.GraphicObject.ReturnInstance(xel.Element("Type").Value)
                    End If
                    obj.LoadData(xel.Elements.ToList)
                    obj.CreateConnectors(0, 0)
                    form.GraphicObjects.Add(obj.Name, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
                End Try
            Next

            For Each xel As XElement In data
                Try
                    Dim id As String = xel.Element("Name").Value
                    If id <> "" Then
                        If Not form.GraphicObjects.ContainsKey(id) Then
                            Dim i As Integer = 0
                            For Each xel2 As XElement In xel.Element("InputConnectors").Elements
                                If xel2.@IsAttached = True Then
                                    form.GraphicObjects(id).InputConnectors(i).ConnectorName = xel2.@AttachedFromObjID & "|" & xel2.@AttachedFromConnIndex
                                    form.GraphicObjects(id).InputConnectors(i).Type = [Enum].Parse(form.GraphicObjects(id).InputConnectors(i).Type.GetType, xel2.@ConnType)
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
                        If Not form.GraphicObjects(id) Is Nothing Then
                            For Each xel2 As XElement In xel.Element("OutputConnectors").Elements
                                If xel2.@IsAttached = True Then
                                    Dim objToID = xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        Dim fromidx As Integer = -1
                                        Dim cp As ConnectionPoint = (From cp2 As ConnectionPoint In form.GraphicObjects(objToID).InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = form.GraphicObjects(id).Name).SingleOrDefault
                                        If Not cp Is Nothing Then
                                            fromidx = cp.ConnectorName.Split("|")(1)
                                        End If
                                        'ConnectObject(obj, objTo, fromidx, xel2.@AttachedToConnIndex)
                                    End If
                                End If
                            Next
                            For Each xel2 As XElement In xel.Element("EnergyConnector").Elements
                                If xel2.@IsAttached = True Then
                                    Dim objToID = xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        'ConnectObject(obj, objTo, -1, xel2.@AttachedToConnIndex)
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
                form.Compounds.Add(obj.Name, obj)
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
                    If form.PropertyPackages.ContainsKey(obj.UniqueID) Then obj.UniqueID = newID
                    form.PropertyPackages.Add(obj.UniqueID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Property Package Information", ex))
                End Try
            Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

            Dim objlist As New Concurrent.ConcurrentBag(Of SharedClasses.UnitOperations.BaseClass)

            For Each xel In data
                Try
                    Dim id As String = xel.<Name>.Value
                    Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                    Dim obj As SharedClasses.UnitOperations.BaseClass = Activator.CreateInstance(t)
                    Dim gobj As IGraphicObject = form.GraphicObjects(obj.Name)
                    obj.GraphicObject = gobj
                    'obj.SetFlowsheet(form)
                    If Not gobj Is Nothing Then
                        obj.LoadData(xel.Elements.ToList)
                        If TypeOf obj Is MaterialStream Then
                            For Each phase As BaseClasses.Phase In DirectCast(obj, MaterialStream).Phases.Values
                                For Each c As ICompoundConstantProperties In form.Compounds.Values
                                    phase.Compounds(c.Name).ConstantProperties = c
                                Next
                            Next
                        End If
                    End If
                    objlist.Add(obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                End Try
            Next

            For Each obj In objlist
                Try
                    Dim id = obj.Name
                    Dim gobj = obj.GraphicObject
                    form.SimulationObjects.Add(id, obj)
                    'form.GraphicObjects.Add(id, gobj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                End Try
            Next

            'For Each so In form.SimulationObjects.Values
            '    Try
            '        If TryCast(so, SpecialOps.Adjust) IsNot Nothing Then
            '            Dim so2 As DWSIM.SimulationObjects.SpecialOps.Adjust = so
            '            If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ManipulatedObjectData.m_ID) Then
            '                so2.ManipulatedObject = form.Collections.FlowsheetObjectCollection(so2.ManipulatedObjectData.m_ID)
            '                DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToMv = so2.ManipulatedObject.GraphicObject
            '            End If
            '            If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ControlledObjectData.m_ID) Then
            '                so2.ControlledObject = form.Collections.FlowsheetObjectCollection(so2.ControlledObjectData.m_ID)
            '                DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToCv = so2.ControlledObject.GraphicObject
            '            End If
            '            If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ReferencedObjectData.m_ID) Then
            '                so2.ReferenceObject = form.Collections.FlowsheetObjectCollection(so2.ReferencedObjectData.m_ID)
            '                DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToRv = so2.ReferenceObject.GraphicObject
            '            End If
            '        End If
            '        If TryCast(so, DWSIM.SimulationObjects.SpecialOps.Spec) IsNot Nothing Then
            '            Dim so2 As DWSIM.SimulationObjects.SpecialOps.Spec = so
            '            If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.TargetObjectData.m_ID) Then
            '                so2.TargetObject = form.Collections.FlowsheetObjectCollection(so2.TargetObjectData.m_ID)
            '                DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToTv = so2.TargetObject.GraphicObject
            '            End If
            '            If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.SourceObjectData.m_ID) Then
            '                so2.SourceObject = form.Collections.FlowsheetObjectCollection(so2.SourceObjectData.m_ID)
            '                DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToSv = so2.SourceObject.GraphicObject
            '            End If
            '        End If
            '        If TryCast(so, DWSIM.SimulationObjects.UnitOperations.CapeOpenUO) IsNot Nothing Then
            '            DirectCast(so, DWSIM.SimulationObjects.UnitOperations.CapeOpenUO).UpdateConnectors2()
            '            DirectCast(so, DWSIM.SimulationObjects.UnitOperations.CapeOpenUO).UpdatePortsFromConnectors()
            '        End If
            '    Catch ex As Exception
            '        excs.Add(New Exception("Error Loading Unit Operation Connection Information", ex))
            '    End Try
            'Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New ReactionSet()
                    obj.LoadData(xel.Elements.ToList)
                    form.ReactionSets.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Reaction Set Information", ex))
                End Try
            Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New Reaction()
                    obj.LoadData(xel.Elements.ToList)
                    form.Reactions.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Reaction Information", ex))
                End Try
            Next

            For Each pp As PropertyPackages.PropertyPackage In form.PropertyPackages.Values
                Try
                    'If pp.ConfigForm Is Nothing Then pp.ReconfigureConfigForm()
                Catch ex As Exception
                    excs.Add(New Exception("Error Reconfiguring Property Package", ex))
                End Try
            Next

            If excs.Count > 0 Then Throw New AggregateException(excs).Flatten Else Return form

        End Function

        Public Shared Function UpdateProcessData(form As IFlowsheetBag, xdoc As XDocument)

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
                    Dim obj = form.SimulationObjects(id)
                    DirectCast(obj, XMLSerializer.Interfaces.ICustomXMLSerialization).LoadData(xel.Elements.ToList)
                    If TypeOf obj Is MaterialStream Then
                        For Each phase As BaseClasses.Phase In DirectCast(obj, MaterialStream).Phases.Values
                            For Each c As ConstantProperties In form.Compounds.Values
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

        Public Shared Function ReturnProcessData(Form As IFlowsheetBag) As IO.MemoryStream

            Dim xdoc As New XDocument()
            Dim xel As XElement

            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            xdoc.Add(New XElement("DWSIM_Simulation_Data"))

            xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
            xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

            For Each so As BaseClass In Form.SimulationObjects.Values
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

                    For Each so As BaseClass In Me.Fsheet.SimulationObjects.Values
                        xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
                    Next

                    xdoc.Save(path)

                    FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": " & FlowSheet.GetTranslatedString("SubFSUpdateSuccess"), IFlowsheet.MessageType.Information)

                Catch ex As Exception

                    FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": " & FlowSheet.GetTranslatedString("SubFSUpdateFailed") & " " & ex.ToString, IFlowsheet.MessageType.Information)

                End Try

            End If

        End Sub

        Public Overrides Sub Calculate(Optional args As Object = Nothing)

            Validate()

            'Me.Fsheet.MasterUnitOp = Me

            Calculated = False

            MassBalanceError = 0.0#

            Dim msfrom, msto As MaterialStream

            Dim win, wout As Double

            win = 0.0#
            For Each c In Me.GraphicObject.InputConnectors

                If c.IsAttached Then

                    msfrom = FlowSheet.SimulationObjects(c.AttachedConnector.AttachedFrom.Name)
                    msto = Fsheet.SimulationObjects(InputConnections(Me.GraphicObject.InputConnectors.IndexOf(c)))

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
                                    If Not msto.Phases(0).Compounds(s.Value).MassFraction.HasValue Then msto.Phases(0).Compounds(s.Value).MassFraction = 0.0#
                                    msto.Phases(0).Compounds(s.Value).MassFraction += msfrom.Phases(0).Compounds(s.Key).MassFlow.GetValueOrDefault / wt
                                End If
                            Next

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMassFractions

                            msto.Phases(0).Properties.massflow = msfrom.Phases(0).Properties.massflow.GetValueOrDefault

                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    If Not msto.Phases(0).Compounds(s.Value).MassFraction.HasValue Then msto.Phases(0).Compounds(s.Value).MassFraction = 0.0#
                                    msto.Phases(0).Compounds(s.Value).MassFraction += msfrom.Phases(0).Compounds(s.Key).MassFraction.GetValueOrDefault
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
                                    If Not msto.Phases(0).Compounds(s.Value).MoleFraction.HasValue Then msto.Phases(0).Compounds(s.Value).MoleFraction = 0.0#
                                    msto.Phases(0).Compounds(s.Value).MoleFraction += msfrom.Phases(0).Compounds(s.Key).MolarFlow.GetValueOrDefault / mt
                                End If
                            Next

                            msto.CalcOverallCompMassFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFractions

                            msto.Phases(0).Properties.molarflow = msfrom.Phases(0).Properties.molarflow.GetValueOrDefault

                            For Each s In CompoundMappings
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Key) And msto.Phases(0).Compounds.ContainsKey(s.Value) Then
                                    If Not msto.Phases(0).Compounds(s.Value).MoleFraction.HasValue Then msto.Phases(0).Compounds(s.Value).MoleFraction = 0.0#
                                    msto.Phases(0).Compounds(s.Value).MoleFraction += msfrom.Phases(0).Compounds(s.Key).MoleFraction.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMoleComposition()

                            msto.CalcOverallCompMassFractions()

                    End Select

                End If

            Next

            'Select Case Calculator.SolverMode
            '    Case 0, 3, 4
            '        'DWSIM.Flowsheet.FlowsheetSolver.CalculateAll2(Fsheet, 0, Settings.TaskCancellationTokenSource)
            '    Case 1, 2
            '        'DWSIM.Flowsheet.FlowsheetSolver.CalculateAll2(Fsheet, 1, Settings.TaskCancellationTokenSource)
            'End Select

            wout = 0.0#
            For Each c In Me.GraphicObject.OutputConnectors

                If c.IsAttached Then

                    msto = FlowSheet.SimulationObjects(c.AttachedConnector.AttachedTo.Name)
                    msfrom = Fsheet.SimulationObjects(OutputConnections(Me.GraphicObject.OutputConnectors.IndexOf(c)))

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
                                    msto.Phases(0).Compounds(s.Name).MassFraction = msfrom.Phases(0).Compounds(s.Name).MassFlow.GetValueOrDefault / wt
                                End If
                            Next

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMassFractions

                            msto.Phases(0).Properties.massflow = msfrom.Phases(0).Properties.massflow.GetValueOrDefault

                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    msto.Phases(0).Compounds(s.Name).MassFraction = msfrom.Phases(0).Compounds(s.Name).MassFraction.GetValueOrDefault
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
                                    msto.Phases(0).Compounds(s.Name).MoleFraction = msfrom.Phases(0).Compounds(s.Name).MolarFlow.GetValueOrDefault / mt
                                End If
                            Next

                            msto.CalcOverallCompMassFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFractions

                            msto.Phases(0).Properties.molarflow = msfrom.Phases(0).Properties.molarflow.GetValueOrDefault

                            For Each s In msto.Phases(0).Compounds.Values
                                If msfrom.Phases(0).Compounds.ContainsKey(s.Name) And msto.Phases(0).Compounds.ContainsKey(s.Name) Then
                                    msto.Phases(0).Compounds(s.Name).MoleFraction = msfrom.Phases(0).Compounds(s.Name).MoleFraction.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMoleComposition()

                            msto.CalcOverallCompMassFractions()

                    End Select

                End If

            Next

            MassBalanceError = (wout - win) / win * 100

            Calculated = True

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Try
                If prop.Contains("[I]") Then
                    Return Fsheet.SimulationObjects(InputParams(pkey).ObjectID).GetPropertyValue(InputParams(pkey).ObjectProperty, su)
                Else
                    Return Fsheet.SimulationObjects(OutputParams(pkey).ObjectID).GetPropertyValue(OutputParams(pkey).ObjectProperty, su)
                End If
            Catch ex As Exception
                Return ex.ToString
            End Try

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim proplist As New ArrayList
            If Initialized Then
                Select Case proptype
                    Case Enums.PropertyType.ALL
                        For Each p In InputParams.Values
                            proplist.Add(Fsheet.SimulationObjects(p.ObjectID).GraphicObject.Tag & " / " & FlowSheet.GetTranslatedString(p.ObjectProperty) & " / [I][" & p.ID & "]")
                        Next
                        For Each p In OutputParams.Values
                            proplist.Add(Fsheet.SimulationObjects(p.ObjectID).GraphicObject.Tag & " / " & FlowSheet.GetTranslatedString(p.ObjectProperty) & " / [O][" & p.ID & "]")
                        Next
                    Case Enums.PropertyType.WR
                        For Each p In InputParams.Values
                            proplist.Add(Fsheet.SimulationObjects(p.ObjectID).GraphicObject.Tag & " / " & FlowSheet.GetTranslatedString(p.ObjectProperty) & " / [I][" & p.ID & "]")
                        Next
                    Case Enums.PropertyType.RO
                        For Each p In OutputParams.Values
                            proplist.Add(Fsheet.SimulationObjects(p.ObjectID).GraphicObject.Tag & " / " & FlowSheet.GetTranslatedString(p.ObjectProperty) & " / [O][" & p.ID & "]")
                        Next
                End Select
            End If
            Return proplist.ToArray(GetType(System.String))
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Fsheet.SimulationObjects(InputParams(pkey).ObjectID).SetPropertyValue(InputParams(pkey).ObjectProperty, propval, su)

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String

            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Try
                If prop.Contains("[I]") Then
                    Return Fsheet.SimulationObjects(InputParams(pkey).ObjectID).GetPropertyUnit(InputParams(pkey).ObjectProperty, su)
                Else
                    Return Fsheet.SimulationObjects(OutputParams(pkey).ObjectID).GetPropertyUnit(OutputParams(pkey).ObjectProperty, su)
                End If
            Catch ex As Exception
                Return ex.ToString
            End Try

        End Function

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Me.Annotation = (From xel As XElement In data Select xel Where xel.Name = "Annotation").SingleOrDefault.Value

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
            Return True
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

        Public Overrides Sub DisplayEditForm()

        End Sub
    End Class

End Namespace


