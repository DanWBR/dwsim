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

Imports Microsoft.Msdn.Samples.GraphicObjects
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports DWSIM.DWSIM.SimulationObjects.UnitOps.Auxiliary
Imports System.Globalization
Imports System.Linq
Imports System.Xml.Linq
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports Microsoft.Msdn.Samples
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports DWSIM.DWSIM.Outros
Imports System.IO
Imports System.Threading.Tasks

Namespace DWSIM.SimulationObjects.UnitOps.Auxiliary

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

Namespace DWSIM.SimulationObjects.UnitOps
    <System.Serializable()> Public Class Flowsheet

        Inherits SimulationObjects_UnitOpBaseClass

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

        Public Sub New(ByVal nome As String, ByVal descricao As String)

            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao

            CompoundMappings = New Dictionary(Of String, String)

            InputParams = New Dictionary(Of String, FlowsheetUOParameter)
            OutputParams = New Dictionary(Of String, FlowsheetUOParameter)

            InputConnections = New List(Of String) From {"", "", "", "", "", "", "", "", "", ""}
            OutputConnections = New List(Of String) From {"", "", "", "", "", "", "", "", "", ""}

            Me.FillNodeItems()
            Me.QTFillNodeItems()

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
            If Not DWSIM.App.IsRunningOnMono Then form.FormObjList = New frmObjList

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
                    If Not TypeOf obj Is DWSIM.GraphicObjects.TableGraphic Then
                        form.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(obj)
                        obj.CreateConnectors(0, 0)
                        With form.Collections
                            Select Case obj.TipoObjeto
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Compressor
                                    .CompressorCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Cooler
                                    .CoolerCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.EnergyStream
                                    .EnergyStreamCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Heater
                                    .HeaterCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.MaterialStream
                                    .MaterialStreamCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.NodeEn
                                    .MixerENCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.NodeIn
                                    .MixerCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.NodeOut
                                    .SplitterCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Pipe
                                    .PipeCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Pump
                                    .PumpCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Tank
                                    .TankCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Expander
                                    .TurbineCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Valve
                                    .ValveCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Vessel
                                    .SeparatorCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Expander
                                    .TurbineCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_Ajuste
                                    .AdjustCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_Reciclo
                                    .RecycleCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_Especificacao
                                    .SpecCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_Conversion
                                    .ReactorConversionCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_Equilibrium
                                    .ReactorEquilibriumCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_Gibbs
                                    .ReactorGibbsCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_CSTR
                                    .ReactorCSTRCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_PFR
                                    .ReactorPFRCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.HeatExchanger
                                    .HeatExchangerCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ShortcutColumn
                                    .ShortcutColumnCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.DistillationColumn
                                    obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                                    .DistillationColumnCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.AbsorptionColumn
                                    obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                                    .AbsorptionColumnCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RefluxedAbsorber
                                    obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                                    .RefluxedAbsorberCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ReboiledAbsorber
                                    obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                                    .ReboiledAbsorberCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_EnergyRecycle
                                    .EnergyRecycleCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ComponentSeparator
                                    .ComponentSeparatorCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OrificePlate
                                    .OrificePlateCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.CustomUO
                                    .CustomUOCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ExcelUO
                                    .ExcelUOCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.FlowsheetUO
                                    .FlowsheetUOCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.CapeOpenUO
                                    obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                                    .CapeOpenUOCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.SolidSeparator
                                    .SolidsSeparatorCollection.Add(obj.Name, obj)
                                Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Filter
                                    .FilterCollection.Add(obj.Name, obj)
                            End Select
                            If Not DWSIM.App.IsRunningOnMono Then
                                Select Case obj.TipoObjeto
                                    Case TipoObjeto.NodeIn
                                        form.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.NodeEn
                                        form.FormObjList.TreeViewObj.Nodes("NodeME").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeME").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.NodeOut
                                        form.FormObjList.TreeViewObj.Nodes("NodeSP").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeSP").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Pump
                                        form.FormObjList.TreeViewObj.Nodes("NodePU").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodePU").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Tank
                                        form.FormObjList.TreeViewObj.Nodes("NodeTQ").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeTQ").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Vessel
                                        form.FormObjList.TreeViewObj.Nodes("NodeSE").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeSE").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.TPVessel
                                        form.FormObjList.TreeViewObj.Nodes("NodeTP").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeTP").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.MaterialStream
                                        form.FormObjList.TreeViewObj.Nodes("NodeMS").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeMS").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.EnergyStream
                                        form.FormObjList.TreeViewObj.Nodes("NodeEN").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeEN").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Compressor
                                        form.FormObjList.TreeViewObj.Nodes("NodeCO").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeCO").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Expander
                                        form.FormObjList.TreeViewObj.Nodes("NodeTU").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeTU").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Cooler
                                        form.FormObjList.TreeViewObj.Nodes("NodeCL").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeCL").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Heater
                                        form.FormObjList.TreeViewObj.Nodes("NodeHT").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeHT").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Pipe
                                        form.FormObjList.TreeViewObj.Nodes("NodePI").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodePI").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Valve
                                        form.FormObjList.TreeViewObj.Nodes("NodeVA").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeVA").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.RCT_Conversion
                                        form.FormObjList.TreeViewObj.Nodes("NodeRCONV").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeRCONV").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.RCT_Equilibrium
                                        form.FormObjList.TreeViewObj.Nodes("NodeREQ").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeREQ").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.RCT_Gibbs
                                        form.FormObjList.TreeViewObj.Nodes("NodeRGIB").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeRGIB").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.RCT_CSTR
                                        form.FormObjList.TreeViewObj.Nodes("NodeRCSTR").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeRCSTR").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.RCT_PFR
                                        form.FormObjList.TreeViewObj.Nodes("NodeRPFR").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeRPFR").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.HeatExchanger
                                        form.FormObjList.TreeViewObj.Nodes("NodeHE").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeHE").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.ShortcutColumn
                                        form.FormObjList.TreeViewObj.Nodes("NodeSC").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeSC").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.DistillationColumn
                                        form.FormObjList.TreeViewObj.Nodes("NodeDC").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeDC").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.AbsorptionColumn
                                        form.FormObjList.TreeViewObj.Nodes("NodeAC").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeAC").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.ReboiledAbsorber
                                        form.FormObjList.TreeViewObj.Nodes("NodeRBA").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeRBA").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.RefluxedAbsorber
                                        form.FormObjList.TreeViewObj.Nodes("NodeRFA").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeRFA").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.ComponentSeparator
                                        form.FormObjList.TreeViewObj.Nodes("NodeCSEP").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeCSEP").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.OrificePlate
                                        form.FormObjList.TreeViewObj.Nodes("NodeOPL").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeOPL").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.CustomUO
                                        form.FormObjList.TreeViewObj.Nodes("NodeUO").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeUO").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.ExcelUO
                                        form.FormObjList.TreeViewObj.Nodes("NodeExcel").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeExcel").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.CapeOpenUO
                                        form.FormObjList.TreeViewObj.Nodes("NodeCOUO").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeCOUO").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.SolidSeparator
                                        form.FormObjList.TreeViewObj.Nodes("NodeSS").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeSS").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.Filter
                                        form.FormObjList.TreeViewObj.Nodes("NodeFT").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeFT").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                    Case TipoObjeto.FlowsheetUO
                                        form.FormObjList.TreeViewObj.Nodes("NodeFS").Nodes.Add(obj.Name, obj.Tag).Name = obj.Name
                                        form.FormObjList.TreeViewObj.Nodes("NodeFS").Nodes(obj.Name).ContextMenuStrip = form.FormObjList.ContextMenuStrip1
                                End Select
                            End If
                        End With
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

            Dim objlist As New Concurrent.ConcurrentBag(Of SimulationObjects_BaseClass)

            Parallel.ForEach(data, Sub(xel)
                                       Try
                                           Dim id As String = xel.<Nome>.Value
                                           Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                                           Dim obj As SimulationObjects_BaseClass = Activator.CreateInstance(t)
                                           Dim gobj As GraphicObject = (From go As GraphicObject In
                                                               form.FormSurface.FlowsheetDesignSurface.drawingObjects Where go.Name = id).SingleOrDefault
                                           obj.GraphicObject = gobj
                                           obj.SetFlowsheet(form)
                                           If Not obj.GraphicObject.TipoObjeto = TipoObjeto.FlowsheetUO Then
                                               obj.FillNodeItems(True)
                                               obj.QTFillNodeItems()
                                           End If
                                           If Not gobj Is Nothing Then
                                               obj.LoadData(xel.Elements.ToList)
                                               If TypeOf obj Is Streams.MaterialStream Then
                                                   For Each phase As DWSIM.ClassesBasicasTermodinamica.Fase In DirectCast(obj, Streams.MaterialStream).Fases.Values
                                                       For Each c As ConstantProperties In form.Options.SelectedComponents.Values
                                                           phase.Componentes(c.Name).ConstantProperties = c
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
                    Dim id = obj.Nome
                    Dim gobj = obj.GraphicObject
                    form.Collections.ObjectCollection.Add(id, obj)
                    With form.Collections
                        Select Case obj.GraphicObject.TipoObjeto
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Compressor
                                .CLCS_CompressorCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Cooler
                                .CLCS_CoolerCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.EnergyStream
                                .CLCS_EnergyStreamCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Heater
                                .CLCS_HeaterCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.MaterialStream
                                .CLCS_MaterialStreamCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.NodeEn
                                .CLCS_EnergyMixerCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.NodeIn
                                .CLCS_MixerCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.NodeOut
                                .CLCS_SplitterCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Pipe
                                .CLCS_PipeCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Pump
                                .CLCS_PumpCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Tank
                                .CLCS_TankCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Expander
                                .CLCS_TurbineCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Valve
                                .CLCS_ValveCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Vessel
                                .CLCS_VesselCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.GO_Tabela
                                .ObjectCollection(gobj.Tag).Tabela = gobj
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Expander
                                .CLCS_TurbineCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_Ajuste
                                .CLCS_AdjustCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_Reciclo
                                .CLCS_RecycleCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_Especificacao
                                .CLCS_SpecCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_Conversion
                                .CLCS_ReactorConversionCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_Equilibrium
                                .CLCS_ReactorEquilibriumCollection.Add(id, obj)
                                .ReactorEquilibriumCollection(gobj.Name) = gobj
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_Gibbs
                                .CLCS_ReactorGibbsCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_CSTR
                                .CLCS_ReactorCSTRCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RCT_PFR
                                .CLCS_ReactorPFRCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.HeatExchanger
                                .CLCS_HeatExchangerCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ShortcutColumn
                                .CLCS_ShortcutColumnCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.DistillationColumn
                                .CLCS_DistillationColumnCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.AbsorptionColumn
                                .CLCS_AbsorptionColumnCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.RefluxedAbsorber
                                .CLCS_RefluxedAbsorberCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ReboiledAbsorber
                                .CLCS_ReboiledAbsorberCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OT_EnergyRecycle
                                .CLCS_EnergyRecycleCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.GO_TabelaRapida
                                .ObjectCollection(CType(gobj, DWSIM.GraphicObjects.QuickTableGraphic).BaseOwner.Nome).TabelaRapida = gobj
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ComponentSeparator
                                .CLCS_ComponentSeparatorCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.OrificePlate
                                .CLCS_OrificePlateCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.CustomUO
                                .CLCS_CustomUOCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.ExcelUO
                                .CLCS_ExcelUOCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.CapeOpenUO
                                .CLCS_CapeOpenUOCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.SolidSeparator
                                .CLCS_SolidsSeparatorCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.Filter
                                .CLCS_FilterCollection.Add(id, obj)
                            Case Microsoft.Msdn.Samples.GraphicObjects.TipoObjeto.FlowsheetUO
                                .CLCS_FlowsheetUOCollection.Add(id, obj)
                        End Select
                    End With
                    obj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                End Try
            Next

            For Each so As SimulationObjects_BaseClass In form.Collections.ObjectCollection.Values
                Try
                    If TryCast(so, DWSIM.SimulationObjects.SpecialOps.Adjust) IsNot Nothing Then
                        Dim so2 As DWSIM.SimulationObjects.SpecialOps.Adjust = so
                        If form.Collections.ObjectCollection.ContainsKey(so2.ManipulatedObjectData.m_ID) Then
                            so2.ManipulatedObject = form.Collections.ObjectCollection(so2.ManipulatedObjectData.m_ID)
                            DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToMv = so2.ManipulatedObject.GraphicObject
                        End If
                        If form.Collections.ObjectCollection.ContainsKey(so2.ControlledObjectData.m_ID) Then
                            so2.ControlledObject = form.Collections.ObjectCollection(so2.ControlledObjectData.m_ID)
                            DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToCv = so2.ControlledObject.GraphicObject
                        End If
                        If form.Collections.ObjectCollection.ContainsKey(so2.ReferencedObjectData.m_ID) Then
                            so2.ReferenceObject = form.Collections.ObjectCollection(so2.ReferencedObjectData.m_ID)
                            DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToRv = so2.ReferenceObject.GraphicObject
                        End If
                    End If
                    If TryCast(so, DWSIM.SimulationObjects.SpecialOps.Spec) IsNot Nothing Then
                        Dim so2 As DWSIM.SimulationObjects.SpecialOps.Spec = so
                        If form.Collections.ObjectCollection.ContainsKey(so2.TargetObjectData.m_ID) Then
                            so2.TargetObject = form.Collections.ObjectCollection(so2.TargetObjectData.m_ID)
                            DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToTv = so2.TargetObject.GraphicObject
                        End If
                        If form.Collections.ObjectCollection.ContainsKey(so2.SourceObjectData.m_ID) Then
                            so2.SourceObject = form.Collections.ObjectCollection(so2.SourceObjectData.m_ID)
                            DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToSv = so2.SourceObject.GraphicObject
                        End If
                    End If
                    If TryCast(so, DWSIM.SimulationObjects.UnitOps.CapeOpenUO) IsNot Nothing Then
                        DirectCast(so, DWSIM.SimulationObjects.UnitOps.CapeOpenUO).UpdateConnectors2()
                        DirectCast(so, DWSIM.SimulationObjects.UnitOps.CapeOpenUO).UpdatePortsFromConnectors()
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
                    DirectCast(obj, DWSIM.GraphicObjects.TableGraphic).BaseOwner = form.Collections.ObjectCollection(xel2.<Owner>.Value)
                    form.Collections.ObjectCollection(xel2.<Owner>.Value).Tabela = obj
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
                        Dim obj As New DWSIM.Outros.Script()
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

            form.Options.NotSelectedComponents = New Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ConstantProperties)

            Dim tmpc As DWSIM.ClassesBasicasTermodinamica.ConstantProperties
            For Each tmpc In FormMain.AvailableComponents.Values
                Dim newc As New DWSIM.ClassesBasicasTermodinamica.ConstantProperties
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
            '        If form.Collections.ObjectCollection.ContainsKey(id) Then
            '            Dim obj = form.Collections.ObjectCollection(id).GraphicObject
            '            obj.LoadData(xel.Elements.ToList)
            '        End If
            '    Catch ex As Exception
            '        excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
            '    End Try
            'Next

            data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim id As String = xel.<Nome>.Value
                    Dim obj = form.Collections.ObjectCollection(id)
                    obj.LoadData(xel.Elements.ToList)
                    If TypeOf obj Is Streams.MaterialStream Then
                        For Each phase As DWSIM.ClassesBasicasTermodinamica.Fase In DirectCast(obj, Streams.MaterialStream).Fases.Values
                            For Each c As ConstantProperties In form.Options.SelectedComponents.Values
                                phase.Componentes(c.Name).ConstantProperties = c
                            Next
                        Next
                    End If
                    obj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
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

            For Each so As SimulationObjects_BaseClass In Form.Collections.ObjectCollection.Values
                xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
            Next

            'xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
            'xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

            'For Each go As Microsoft.Msdn.Samples.GraphicObjects.GraphicObject In Form.FormSurface.FlowsheetDesignSurface.drawingObjects
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

                    For Each so As SimulationObjects_BaseClass In Me.Fsheet.Collections.ObjectCollection.Values
                        xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
                    Next

                    xdoc.Save(path)

                    FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": " & DWSIM.App.GetLocalString("SubFSUpdateSuccess"), Color.Blue, FormClasses.TipoAviso.Informacao)

                Catch ex As Exception

                    FlowSheet.WriteToLog(Me.GraphicObject.Tag & ": " & DWSIM.App.GetLocalString("SubFSUpdateFailed") & " " & ex.ToString, Color.Red, FormClasses.TipoAviso.Erro)

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

                    msfrom = FlowSheet.Collections.ObjectCollection(c.AttachedConnector.AttachedFrom.Name)
                    msto = Fsheet.Collections.ObjectCollection(InputConnections(Me.GraphicObject.InputConnectors.IndexOf(c)))

                    win += msfrom.Fases(0).SPMProperties.massflow.GetValueOrDefault

                    msto.Clear()

                    msto.Fases(0).SPMProperties.temperature = msfrom.Fases(0).SPMProperties.temperature.GetValueOrDefault
                    msto.Fases(0).SPMProperties.pressure = msfrom.Fases(0).SPMProperties.pressure.GetValueOrDefault
                    msto.Fases(0).SPMProperties.enthalpy = msfrom.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
                    msto.SpecType = Streams.MaterialStream.Flashspec.Temperature_and_Pressure

                    Dim wt, mt As Double

                    Select Case MassTransferMode

                        Case FlowsheetUOMassTransferMode.CompoundMassFlows

                            wt = 0.0#
                            For Each s In CompoundMappings
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Key) And msto.Fases(0).Componentes.ContainsKey(s.Value) Then
                                    wt += msfrom.Fases(0).Componentes(s.Key).MassFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Fases(0).SPMProperties.massflow = wt

                            For Each s In CompoundMappings
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Key) And msto.Fases(0).Componentes.ContainsKey(s.Value) Then
                                    If Not msto.Fases(0).Componentes(s.Value).FracaoMassica.HasValue Then msto.Fases(0).Componentes(s.Value).FracaoMassica = 0.0#
                                    msto.Fases(0).Componentes(s.Value).FracaoMassica += msfrom.Fases(0).Componentes(s.Key).MassFlow.GetValueOrDefault / wt
                                End If
                            Next

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMassFractions

                            msto.Fases(0).SPMProperties.massflow = msfrom.Fases(0).SPMProperties.massflow.GetValueOrDefault

                            For Each s In CompoundMappings
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Key) And msto.Fases(0).Componentes.ContainsKey(s.Value) Then
                                    If Not msto.Fases(0).Componentes(s.Value).FracaoMassica.HasValue Then msto.Fases(0).Componentes(s.Value).FracaoMassica = 0.0#
                                    msto.Fases(0).Componentes(s.Value).FracaoMassica += msfrom.Fases(0).Componentes(s.Key).FracaoMassica.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMassComposition()

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFlows

                            mt = 0.0#
                            For Each s In CompoundMappings
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Key) And msto.Fases(0).Componentes.ContainsKey(s.Value) Then
                                    mt += msfrom.Fases(0).Componentes(s.Key).MolarFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Fases(0).SPMProperties.molarflow = mt

                            For Each s In CompoundMappings
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Key) And msto.Fases(0).Componentes.ContainsKey(s.Value) Then
                                    If Not msto.Fases(0).Componentes(s.Value).FracaoMolar.HasValue Then msto.Fases(0).Componentes(s.Value).FracaoMolar = 0.0#
                                    msto.Fases(0).Componentes(s.Value).FracaoMolar += msfrom.Fases(0).Componentes(s.Key).MolarFlow.GetValueOrDefault / mt
                                End If
                            Next

                            msto.CalcOverallCompMassFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFractions

                            msto.Fases(0).SPMProperties.molarflow = msfrom.Fases(0).SPMProperties.molarflow.GetValueOrDefault

                            For Each s In CompoundMappings
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Key) And msto.Fases(0).Componentes.ContainsKey(s.Value) Then
                                    If Not msto.Fases(0).Componentes(s.Value).FracaoMolar.HasValue Then msto.Fases(0).Componentes(s.Value).FracaoMolar = 0.0#
                                    msto.Fases(0).Componentes(s.Value).FracaoMolar += msfrom.Fases(0).Componentes(s.Key).FracaoMolar.GetValueOrDefault
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

                    msto = FlowSheet.Collections.ObjectCollection(c.AttachedConnector.AttachedTo.Name)
                    msfrom = Fsheet.Collections.ObjectCollection(OutputConnections(Me.GraphicObject.OutputConnectors.IndexOf(c)))

                    wout += msfrom.Fases(0).SPMProperties.massflow.GetValueOrDefault

                    msto.Clear()

                    msto.Fases(0).SPMProperties.temperature = msfrom.Fases(0).SPMProperties.temperature.GetValueOrDefault
                    msto.Fases(0).SPMProperties.pressure = msfrom.Fases(0).SPMProperties.pressure.GetValueOrDefault
                    msto.Fases(0).SPMProperties.enthalpy = msfrom.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
                    msto.SpecType = Streams.MaterialStream.Flashspec.Temperature_and_Pressure

                    Dim wt, mt As Double

                    Select Case MassTransferMode

                        Case FlowsheetUOMassTransferMode.CompoundMassFlows

                            wt = 0.0#
                            For Each s In msto.Fases(0).Componentes.Values
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Nome) And msto.Fases(0).Componentes.ContainsKey(s.Nome) Then
                                    wt += msfrom.Fases(0).Componentes(s.Nome).MassFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Fases(0).SPMProperties.massflow = wt

                            For Each s In msto.Fases(0).Componentes.Values
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Nome) And msto.Fases(0).Componentes.ContainsKey(s.Nome) Then
                                    msto.Fases(0).Componentes(s.Nome).FracaoMassica = msfrom.Fases(0).Componentes(s.Nome).MassFlow.GetValueOrDefault / wt
                                End If
                            Next

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMassFractions

                            msto.Fases(0).SPMProperties.massflow = msfrom.Fases(0).SPMProperties.massflow.GetValueOrDefault

                            For Each s In msto.Fases(0).Componentes.Values
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Nome) And msto.Fases(0).Componentes.ContainsKey(s.Nome) Then
                                    msto.Fases(0).Componentes(s.Nome).FracaoMassica = msfrom.Fases(0).Componentes(s.Nome).FracaoMassica.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMassComposition()

                            msto.CalcOverallCompMoleFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFlows

                            mt = 0.0#
                            For Each s In msto.Fases(0).Componentes.Values
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Nome) And msto.Fases(0).Componentes.ContainsKey(s.Nome) Then
                                    mt += msfrom.Fases(0).Componentes(s.Nome).MolarFlow.GetValueOrDefault
                                End If
                            Next

                            msto.Fases(0).SPMProperties.molarflow = mt

                            For Each s In msto.Fases(0).Componentes.Values
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Nome) And msto.Fases(0).Componentes.ContainsKey(s.Nome) Then
                                    msto.Fases(0).Componentes(s.Nome).FracaoMolar = msfrom.Fases(0).Componentes(s.Nome).MolarFlow.GetValueOrDefault / mt
                                End If
                            Next

                            msto.CalcOverallCompMassFractions()

                        Case FlowsheetUOMassTransferMode.CompoundMoleFractions

                            msto.Fases(0).SPMProperties.molarflow = msfrom.Fases(0).SPMProperties.molarflow.GetValueOrDefault

                            For Each s In msto.Fases(0).Componentes.Values
                                If msfrom.Fases(0).Componentes.ContainsKey(s.Nome) And msto.Fases(0).Componentes.ContainsKey(s.Nome) Then
                                    msto.Fases(0).Componentes(s.Nome).FracaoMolar = msfrom.Fases(0).Componentes(s.Nome).FracaoMolar.GetValueOrDefault
                                End If
                            Next

                            msto.NormalizeOverallMoleComposition()

                            msto.CalcOverallCompMassFractions()

                    End Select

                End If

            Next

            MassBalanceError = (wout - win) / win * 100

            Calculated = True

            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            'Call function to calculate flowsheet
            With objargs
                .Calculado = True
                .Nome = Me.Nome
                .Tag = Me.GraphicObject.Tag
                .Tipo = TipoObjeto.FlowsheetUO
            End With

            FlowSheet.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Sub Validate()

            For Each c In Me.GraphicObject.InputConnectors
                If c.IsAttached Then
                    Dim mstr = FlowSheet.Collections.ObjectCollection(c.AttachedConnector.AttachedFrom.Name)
                    If Not mstr.GraphicObject.Calculated Then
                        Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                        'Call function to calculate flowsheet
                        With objargs
                            .Calculado = False
                            .Nome = Me.Nome
                            .Tipo = TipoObjeto.FlowsheetUO
                        End With
                        Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
                    End If
                End If
            Next

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object

            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Try
                If prop.Contains("[I]") Then
                    Return Fsheet.Collections.ObjectCollection(InputParams(pkey).ObjectID).GetPropertyValue(InputParams(pkey).ObjectProperty, su)
                Else
                    Return Fsheet.Collections.ObjectCollection(OutputParams(pkey).ObjectID).GetPropertyValue(OutputParams(pkey).ObjectProperty, su)
                End If
            Catch ex As Exception
                Return ex.ToString
            End Try

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim proplist As New ArrayList
            If Initialized Then
                Select Case proptype
                    Case PropertyType.ALL
                        For Each p In InputParams.Values
                            proplist.Add(Fsheet.Collections.ObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [I][" & p.ID & "]")
                        Next
                        For Each p In OutputParams.Values
                            proplist.Add(Fsheet.Collections.ObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [O][" & p.ID & "]")
                        Next
                    Case PropertyType.WR
                        For Each p In InputParams.Values
                            proplist.Add(Fsheet.Collections.ObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [I][" & p.ID & "]")
                        Next
                    Case PropertyType.RO
                        For Each p In OutputParams.Values
                            proplist.Add(Fsheet.Collections.ObjectCollection(p.ObjectID).GraphicObject.Tag & " / " & DWSIM.App.GetPropertyName(p.ObjectProperty) & " / [O][" & p.ID & "]")
                        Next
                End Select
            End If
            Return proplist.ToArray(GetType(System.String))
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SistemasDeUnidades.Unidades = Nothing) As Object

            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Fsheet.Collections.ObjectCollection(InputParams(pkey).ObjectID).SetPropertyValue(InputParams(pkey).ObjectProperty, propval, su)

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object

            Dim pkey As String = prop.Split("][")(1).TrimStart("[").TrimEnd("]")

            Try
                If prop.Contains("[I]") Then
                    Return Fsheet.Collections.ObjectCollection(InputParams(pkey).ObjectID).GetPropertyUnit(InputParams(pkey).ObjectProperty, su)
                Else
                    Return Fsheet.Collections.ObjectCollection(OutputParams(pkey).ObjectID).GetPropertyUnit(OutputParams(pkey).ObjectProperty, su)
                End If
            Catch ex As Exception
                Return ex.ToString
            End Try

        End Function

        Public Overrides Sub QTFillNodeItems()
            If Me.QTNodeTableItems Is Nothing Then Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
            With Me.QTNodeTableItems
                .Clear()
                .Add(0, New DWSIM.Outros.NodeItem(DWSIM.App.GetLocalString("MassBalanceError"), "", "%", 0, 0, ""))
            End With
        End Sub

        Public Overrides Sub UpdatePropertyNodes(su As SistemasDeUnidades.Unidades, nf As String)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor
            If Me.NodeTableItems Is Nothing Then
                Me.NodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.FillNodeItems()
            End If

            For Each nti As Outros.NodeItem In Me.NodeTableItems.Values
                nti.Value = GetPropertyValue(nti.Text, FlowSheet.Options.SelectedUnitSystem)
                nti.Unit = GetPropertyUnit(nti.Text, FlowSheet.Options.SelectedUnitSystem)
            Next

            If Me.QTNodeTableItems Is Nothing Then
                Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.QTFillNodeItems()
            End If

            With Me.QTNodeTableItems

                .Item(0).Value = Me.MassBalanceError
                .Item(0).Unit = "%"

            End With

        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

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
                        If Fsheet.Collections.ObjectCollection.ContainsKey(p.ObjectID) Then
                            .Item.Add(Fsheet.Collections.ObjectCollection(p.ObjectID).GraphicObject.Tag & ", " &
                                      DWSIM.App.GetPropertyName(p.ObjectProperty) &
                                      " (" & Fsheet.Collections.ObjectCollection(p.ObjectID).GetPropertyUnit(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem) & ")",
                                      Fsheet.Collections.ObjectCollection(p.ObjectID).GetPropertyValue(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem), False,
                                      DWSIM.App.GetLocalString("LinkedInputParms"), DWSIM.App.GetLocalString(""), True)
                            .Item(.Item.Count - 1).Tag = p.ID
                            .Item(.Item.Count - 1).Tag2 = "[I][" & p.ID & "]"
                        End If
                    Next

                    For Each p In OutputParams.Values
                        If Fsheet.Collections.ObjectCollection.ContainsKey(p.ObjectID) Then
                            .Item.Add(Fsheet.Collections.ObjectCollection(p.ObjectID).GraphicObject.Tag & ", " &
                                      DWSIM.App.GetPropertyName(p.ObjectProperty) &
                                      " (" & Fsheet.Collections.ObjectCollection(p.ObjectID).GetPropertyUnit(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem) & ")",
                                      Fsheet.Collections.ObjectCollection(p.ObjectID).GetPropertyValue(p.ObjectProperty, Me.FlowSheet.Options.SelectedUnitSystem), True,
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

            m_nodeitems = Nothing
            FillNodeItems(True)
            QTFillNodeItems()

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "NodeItems").Elements
                Dim text As String = xel2.@Text
                Dim ni2 As DWSIM.Outros.NodeItem = (From ni As DWSIM.Outros.NodeItem In m_nodeitems.Values Select ni Where ni.Text = text).SingleOrDefault
                If Not ni2 Is Nothing Then
                    ni2.Checked = True
                End If
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


