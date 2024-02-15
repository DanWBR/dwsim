'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports NetOffice
Imports Excel = NetOffice.ExcelApi
Imports NetOffice.ExcelApi.Enums
Imports System.Text
Imports System.IO
Imports System.Xml.Xsl
Imports AODL.Document
Imports System.Linq
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class FormReportConfig

    Inherits System.Windows.Forms.Form

    Protected frm As FormFlowsheet

    Dim DT As New DataTable
    Dim Conversor As New SystemsOfUnits.Converter
    Dim su As New SystemsOfUnits.Units
    Dim nf As String

    Private Sub FillDataTable()

        su = frm.Options.SelectedUnitSystem
        nf = frm.Options.NumberFormat

        'DT.Columns.Clear()
        If Not DT.Columns.Contains(("Nome")) Then DT.Columns.Add(("Nome"), GetType(System.String))
        If Not DT.Columns.Contains(("Tipo")) Then DT.Columns.Add(("Tipo"), GetType(System.String))
        If Not DT.Columns.Contains(("Propriedade")) Then DT.Columns.Add(("Propriedade"), GetType(System.String))
        If Not DT.Columns.Contains(("Valor")) Then DT.Columns.Add(("Valor"), GetType(System.String))
        If Not DT.Columns.Contains(("Unidade")) Then DT.Columns.Add(("Unidade"), GetType(System.String))
        DT.Rows.Clear()


        Dim baseobj As SharedClasses.UnitOperations.BaseClass
        Dim properties() As String
        Dim description As String
        Dim objtype As ObjectType
        Dim propidx, r1, r2, r3, r4, r5, r6, r7 As Integer
        Dim inclcond, inclcomp, inclmist, inclvap, inclliqm, inclliq1, inclliq2, inclaq, inclsolid As Boolean
        r1 = 5
        r2 = 12
        r3 = 30
        r4 = 48
        r5 = 66
        r6 = 84
        r7 = 131
        inclcond = Me.CheckBox1.Checked
        inclcomp = Me.CheckBox2.Checked
        inclmist = Me.CheckBox3.Checked
        inclvap = Me.CheckBox4.Checked
        inclliqm = Me.CheckBox5.Checked
        inclliq1 = Me.CheckBox6.Checked
        inclliq2 = Me.CheckBox7.Checked
        inclaq = Me.CheckBox8.Checked
        inclsolid = Me.CheckBox9.Checked

        For Each lvi As ListViewItem In Me.ListView1.Items
            baseobj = frm.Collections.FlowsheetObjectCollection(lvi.Tag)
            properties = baseobj.GetProperties(Interfaces.Enums.PropertyType.ALL)
            objtype = baseobj.GraphicObject.ObjectType
            description = DWSIM.App.GetLocalString(baseobj.GraphicObject.Description)
            If objtype = ObjectType.MaterialStream Then
                Dim value As String
                If inclcond Then
                    For propidx = 0 To r1 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
                If inclcomp Then
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetLocalString("FraomolarnaMistura"), "", ""})
                    For Each subst As BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(0).Compounds.Values
                        DT.Rows.Add(New String() {lvi.Text, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If inclmist Then
                    For propidx = r1 To r2 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
                If inclcomp Then
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName("PROP_MS_106"), "", ""})
                    For Each subst As BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(2).Compounds.Values
                        DT.Rows.Add(New String() {lvi.Text, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If inclvap Then
                    For propidx = r2 To r3 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
                If inclcomp Then
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName("PROP_MS_107"), "", ""})
                    For Each subst As BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(1).Compounds.Values
                        DT.Rows.Add(New String() {lvi.Text, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If inclliqm Then
                    For propidx = r3 To r4 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
                If inclcomp Then
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName("PROP_MS_108"), "", ""})
                    For Each subst As BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(3).Compounds.Values
                        DT.Rows.Add(New String() {lvi.Text, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If inclliq1 Then
                    For propidx = r4 To r5 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
                If inclcomp Then
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName("PROP_MS_109"), "", ""})
                    For Each subst As BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(4).Compounds.Values
                        DT.Rows.Add(New String() {lvi.Text, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If inclliq2 Then
                    For propidx = r5 To r6 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
                If inclcomp Then
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName("PROP_MS_110"), "", ""})
                    For Each subst As BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(6).Compounds.Values
                        DT.Rows.Add(New String() {lvi.Text, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If inclaq Then
                    For propidx = r6 To 101
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
                If inclcomp Then
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName("PROP_MS_146"), "", ""})
                    For Each subst As BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(7).Compounds.Values
                        DT.Rows.Add(New String() {lvi.Text, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If inclsolid Then
                    For propidx = r7 To 148
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                End If
            Else
                For Each prop As String In properties
                    Dim val = baseobj.GetPropertyValue(prop, su)
                    Try
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(prop), Format(val, nf), baseobj.GetPropertyUnit(prop, su)})
                    Catch ex As Exception
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(prop), val.ToString, baseobj.GetPropertyUnit(prop, su)})
                    End Try

                Next
            End If
        Next

    End Sub

    Private Sub FormReportConfig_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        frm = My.Application.ActiveSimulation
        Me.Text = DWSIM.App.GetLocalString("RelatoriodaSimulacao")

        Me.ListView1.Items.Clear()

        KButton4.Enabled = Not DWSIM.App.IsRunningOnMono
        KButton5.Enabled = Not DWSIM.App.IsRunningOnMono

        Dim obj As SharedClasses.UnitOperations.BaseClass

        TreeViewObj.Nodes.Add("Other")

        For Each n As TreeNode In Me.TreeViewObj.Nodes
            n.Nodes.Clear()
        Next

        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.MaterialStream)
            Me.TreeViewObj.Nodes(0).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream)
            Me.TreeViewObj.Nodes(1).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.NodeIn)
            Me.TreeViewObj.Nodes(2).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.NodeOut)
            Me.TreeViewObj.Nodes(3).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Pipe)
            Me.TreeViewObj.Nodes(4).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Valve)
            Me.TreeViewObj.Nodes(5).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Pump)
            Me.TreeViewObj.Nodes(6).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Tank)
            Me.TreeViewObj.Nodes(7).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Vessel)
            Me.TreeViewObj.Nodes(8).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Compressor)
            Me.TreeViewObj.Nodes(9).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Heater)
            Me.TreeViewObj.Nodes(10).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Cooler)
            Me.TreeViewObj.Nodes(11).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.Expander)
            Me.TreeViewObj.Nodes(12).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.RCT_Conversion)
            Me.TreeViewObj.Nodes(13).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.RCT_Equilibrium)
            Me.TreeViewObj.Nodes(14).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.RCT_Gibbs)
            Me.TreeViewObj.Nodes(15).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.RCT_CSTR)
            Me.TreeViewObj.Nodes(16).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.RCT_PFR)
            Me.TreeViewObj.Nodes(17).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.HeatExchanger)
            Me.TreeViewObj.Nodes(18).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.ShortcutColumn)
            Me.TreeViewObj.Nodes(19).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.DistillationColumn)
            Me.TreeViewObj.Nodes(20).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.AbsorptionColumn)
            Me.TreeViewObj.Nodes(21).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.ReboiledAbsorber)
            Me.TreeViewObj.Nodes(22).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.RefluxedAbsorber)
            Me.TreeViewObj.Nodes(23).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.ComponentSeparator)
            Me.TreeViewObj.Nodes(24).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.OrificePlate)
            Me.TreeViewObj.Nodes(25).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.External)
            Me.TreeViewObj.Nodes(26).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next

        If FormMain.IsPro Then
            KButton4.Enabled = False
            KButton5.Enabled = False
        End If

    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton5.Click

        Me.FillDataTable()

        Dim MyReport As New NeoDataType.MyNeoReport.Report
        Select Case My.Settings.CultureInfo
            Case "pt-BR"
                Using filestr As Stream = Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.report_pt-BR.mr6")
                    MyReport.LoadFrom(filestr)
                End Using
            Case Else
                Using filestr As Stream = Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.report_en-US.mr6")
                    MyReport.LoadFrom(filestr)
                End Using
        End Select

        Dim ds As NeoDataType.MyNeoReport.TableDataSource = MyReport.Page.Sections(2).DataSource
        ds.Table = DT

        Dim titlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label5")
        Dim comlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label6")
        Dim verlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label4")

        verlbl.Text = "DWSIM " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor

        If Not frm.Options.SimulationName Is Nothing Then titlbl.Text = frm.Options.SimulationName
        If Not frm.Options.SimulationComments Is Nothing Then comlbl.Text = frm.Options.SimulationComments

        Dim nlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageFooter.Items("Label")
        nlbl.Text = ""

        MyReport.ShowPreview(NeoDataType.MyNeoReport.PreviewWindowState.Maximized)

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton4.Click

        Me.FillDataTable()

        Dim MyReport As New NeoDataType.MyNeoReport.Report
        Select Case My.Settings.CultureInfo
            Case "pt-BR"
                Using filestr As Stream = Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.report_pt-BR.mr6")
                    MyReport.LoadFrom(filestr)
                End Using
            Case Else
                Using filestr As Stream = Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.report_en-US.mr6")
                    MyReport.LoadFrom(filestr)
                End Using
        End Select

        Dim ds As NeoDataType.MyNeoReport.TableDataSource = CType(MyReport.Page.Sections(2).DataSource, NeoDataType.MyNeoReport.TableDataSource)
        ds.Table = DT

        Dim nlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageFooter.Items("Label")
        nlbl.Text = ""

        Dim verlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label4")

        verlbl.Text = "DWSIM v." & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor

        Dim titlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label5")
        Dim comlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label6")
        If Not frm.Options.SimulationName Is Nothing Then titlbl.Text = frm.Options.SimulationName
        If Not frm.Options.SimulationComments Is Nothing Then comlbl.Text = frm.Options.SimulationComments

        MyReport.Print()

    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton7.Click


        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        'XML File (.xml)|*.xml|Text File (.txt)|*.txt|Microsoft Excel Spreadsheet (.xlsx)|*.xlsx|Open Document Spreadsheet (.ods)|*.ods|Open Document Text (.odt)|*.odt
        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("XML File", "*.xml"),
            New FilePickerAllowedType("Text File", "*.txt"),
            New FilePickerAllowedType("Excel File", "*.xlsx"),
            New FilePickerAllowedType("ODS File", "*.ods"),
            New FilePickerAllowedType("ODT File", "*.odt")})

        If handler IsNot Nothing Then
            Using stream As New MemoryStream()
                If handler.GetExtension().ToLower() = ".xml" Then
                    Me.FillDataTable()
                    Me.DT.TableName = DWSIM.App.GetLocalString("Resultados")
                    Dim output As String = ""
                    Using sri As New MemoryStream()
                        Me.DT.WriteXml(sri)
                        sri.Position = 0
                        ' xslInput is a string that contains xsl
                        Using filestr As Stream = Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.report_transform.xsl")
                            Using xrt As XmlReader = XmlReader.Create(filestr)
                                Using xri As XmlReader = XmlReader.Create(sri)
                                    Dim xslt As New XslCompiledTransform()
                                    xslt.Load(xrt)
                                    Using sw As New StringWriter()
                                        Using xwo As XmlWriter = XmlWriter.Create(sw, xslt.OutputSettings)
                                            ' use OutputSettings of xsl, so it can be output as HTML
                                            xslt.Transform(xri, xwo)
                                            output = sw.ToString()
                                        End Using
                                    End Using
                                End Using
                            End Using
                        End Using
                    End Using
                    Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                        writer.Write(output)
                        handler.Write(stream)
                    End Using
                ElseIf handler.GetExtension().ToLower() = ".txt" Then
                    Me.FillDataTable()
                    Me.CreateAndSaveCSVFile(stream, handler)
                ElseIf handler.GetExtension().ToLower() = ".xlsx" Then
                    Me.FillDataTable()
                    Me.CreateAndSaveExcelFile(stream, handler)
                ElseIf handler.GetExtension().ToLower() = ".ods" Then
                    Me.FillDataTable()
                    Me.CreateAndSaveODSFile(stream, handler)
                ElseIf handler.GetExtension().ToLower() = ".odt" Then
                    Me.FillDataTable()
                    Me.CreateAndSaveODTFile(stream, handler)
                End If
            End Using
        End If
    End Sub

    Sub CreateAndSaveODTFile(stream As MemoryStream, handler As IVirtualFile)


        Dim fname = AODL.Document.Styles.FontFamilies.Arial
        Dim fsize = "10pt"

        'Create a new text document
        Dim document As New TextDocuments.TextDocument()
        document.[New]()

        'Create a table for a text document using the TableBuilder
        Dim table As Content.Tables.Table = Content.Tables.TableBuilder.CreateTextDocumentTable(document, "table1", "table1", 3, 1, 16.99, False, False)

        Dim paragraph As Content.Text.Paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p0")
        paragraph.ParagraphStyle.TextProperties.Bold = "bold"
        paragraph.ParagraphStyle.TextProperties.FontName = fname
        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
        paragraph.TextContent.Add(New Content.Text.SimpleText(document, "DWSIM Simulation Results Report"))
        table.Rows(0).Cells(0).Content.Add(paragraph)

        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p1")
        paragraph.ParagraphStyle.TextProperties.FontName = fname
        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
        paragraph.TextContent.Add(New Content.Text.SimpleText(document, "Simulation File: " & frm.Options.FilePath))
        table.Rows(1).Cells(0).Content.Add(paragraph)

        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p2")
        paragraph.ParagraphStyle.TextProperties.FontName = fname
        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
        paragraph.TextContent.Add(New Content.Text.SimpleText(document, "Date created: " & Date.Now.ToString))
        table.Rows(2).Cells(0).Content.Add(paragraph)

        'Add table to the document
        document.Content.Add(table)

        Try

            With document

                Dim i As Integer = 0
                Dim j As Integer = 1

                Dim prevmat, actualmat As String

                table = Content.Tables.TableBuilder.CreateTextDocumentTable(document, "table2", "table2", DT.Rows.Count + ListView1.Items.Count * 3, 3, 16.99, False, False)

                Do
                    actualmat = Me.DT.Rows(i).Item(0).ToString
                    prevmat = Me.DT.Rows(i).Item(0).ToString

                    'Create a standard paragraph
                    paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p" + i.ToString() + j.ToString())
                    paragraph.ParagraphStyle.TextProperties.FontName = fname
                    paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                    paragraph.ParagraphStyle.TextProperties.Bold = "bold"

                    'Add some simple text
                    paragraph.TextContent.Add(New Content.Text.SimpleText(document, DWSIM.App.GetLocalString("Objeto") & ": " & prevmat & " (" & Me.DT.Rows(i).Item(1).ToString & ")"))

                    'Insert paragraph into the first cell
                    table.Rows(j).Cells(0).Content.Add(paragraph)

                    j = j + 1
                    Do

                        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p1" + i.ToString() + j.ToString())
                        paragraph.ParagraphStyle.TextProperties.FontName = fname
                        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                        paragraph.TextContent.Add(New Content.Text.SimpleText(document, Me.DT.Rows(i).Item(2)))
                        table.Rows(j + 1).Cells(0).Content.Add(paragraph)

                        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p2" + i.ToString() + j.ToString())
                        paragraph.ParagraphStyle.ParagraphProperties.Alignment = "right"
                        paragraph.ParagraphStyle.TextProperties.FontName = fname
                        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                        paragraph.TextContent.Add(New Content.Text.SimpleText(document, Me.DT.Rows(i).Item(3)))
                        table.Rows(j + 1).Cells(1).Content.Add(paragraph)

                        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p3" + i.ToString() + j.ToString())
                        paragraph.ParagraphStyle.ParagraphProperties.MarginLeft = "0.2cm"
                        paragraph.ParagraphStyle.TextProperties.FontName = fname
                        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                        paragraph.TextContent.Add(New Content.Text.SimpleText(document, Me.DT.Rows(i).Item(4)))
                        table.Rows(j + 1).Cells(2).Content.Add(paragraph)

                        i = i + 1
                        j = j + 1

                        If i < DT.Rows.Count Then actualmat = Me.DT.Rows(i).Item(0).ToString

                    Loop Until actualmat <> prevmat Or i >= DT.Rows.Count

                    j = j + 2

                Loop Until i >= DT.Rows.Count

            End With

            'Add table to the document
            document.Content.Add(table)

            'Save the document
            Using writer = New AODL.IO.InMemoryPackageWriter(stream)
                document.Save("", New Export.OpenDocument.OpenDocumentTextExporter(writer))
                handler.Write(New MemoryStream(stream.ToArray()))
            End Using

            MsgBox(DWSIM.App.GetLocalString("FileSaved"), MsgBoxStyle.Information, "DWSIM")

        Catch ex As Exception

            MsgBox(ex.ToString, MsgBoxStyle.Exclamation, DWSIM.App.GetLocalString("Erro"))

        Finally

        End Try

    End Sub

    Sub CreateAndSaveODSFile(stream As MemoryStream, handler As IVirtualFile)

        Dim sheetdoc As New SpreadsheetDocuments.SpreadsheetDocument()
        sheetdoc.[New]()
        Dim mysheet As New Content.Tables.Table(sheetdoc, "DWSIM_Report", "report")
        mysheet.Rows.Add(New Content.Tables.Row(mysheet))

        Try

            With mysheet

                Dim i As Integer = 0
                Dim j As Integer = 1

                Dim prevmat, actualmat As String

                Do

                    actualmat = Me.DT.Rows(i).Item(0).ToString
                    prevmat = Me.DT.Rows(i).Item(0).ToString
                    Dim cell = mysheet.CreateCell()
                    cell.OfficeValueType = "string"
                    Dim paragraph = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                    paragraph.TextContent.Add(New Content.Text.SimpleText(sheetdoc, DWSIM.App.GetLocalString("Objeto") & ": " & prevmat & " (" & Me.DT.Rows(i).Item(1).ToString & ")"))
                    cell.Content.Add(paragraph)
                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                    mysheet.InsertCellAt(j, 0, cell)

                    j = j + 1

                    Do
                        mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                        Dim cell0 = mysheet.CreateCell()
                        cell0.OfficeValueType = "string"
                        Dim paragraph0 = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                        paragraph0.TextContent.Add(New Content.Text.SimpleText(sheetdoc, Me.DT.Rows(i).Item(2)))
                        cell0.Content.Add(paragraph0)
                        mysheet.InsertCellAt(j + 1, 0, cell0)
                        Dim cell1 = mysheet.CreateCell()
                        cell1.OfficeValueType = "string"
                        Dim paragraph1 = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                        paragraph1.TextContent.Add(New Content.Text.SimpleText(sheetdoc, Me.DT.Rows(i).Item(3)))
                        cell1.Content.Add(paragraph1)
                        mysheet.InsertCellAt(j + 1, 1, cell1)
                        Dim cell2 = mysheet.CreateCell()
                        cell2.OfficeValueType = "string"
                        Dim paragraph2 = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                        paragraph2.TextContent.Add(New Content.Text.SimpleText(sheetdoc, Me.DT.Rows(i).Item(4)))
                        cell2.Content.Add(paragraph2)
                        mysheet.InsertCellAt(j + 1, 2, cell2)
                        i = i + 1
                        j = j + 1
                        If i < DT.Rows.Count Then actualmat = Me.DT.Rows(i).Item(0).ToString
                    Loop Until actualmat <> prevmat Or i >= DT.Rows.Count

                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))

                    j = j + 2

                Loop Until i >= DT.Rows.Count

            End With

            sheetdoc.TableCollection.Add(mysheet)

            Using writer As New AODL.IO.InMemoryPackageWriter(stream)
                sheetdoc.Save(handler.Filename, New Export.OpenDocument.OpenDocumentTextExporter(writer))
                handler.Write(New MemoryStream(stream.ToArray()))
            End Using

            MsgBox(DWSIM.App.GetLocalString("FileSaved"), MsgBoxStyle.Information, "DWSIM")

        Catch ex As Exception

            MsgBox(ex.ToString, MsgBoxStyle.Exclamation, DWSIM.App.GetLocalString("Erro"))

        Finally


        End Try

    End Sub

    Sub CreateAndSaveExcelFile(stream As MemoryStream, handler As IVirtualFile)


        Dim xcl As New OfficeOpenXml.ExcelPackage()
        Dim mybook As OfficeOpenXml.ExcelWorkbook = xcl.Workbook

        Dim mysheet As OfficeOpenXml.ExcelWorksheet = mybook.Worksheets.Add("DWSIM_Report")

        Try
            With mysheet
                .Name = "DWSIM_Report"
                Dim i As Integer = 0
                Dim j As Integer = 1
                Dim prevmat, actualmat As String
                Do
                    actualmat = Me.DT.Rows(i).Item(0).ToString
                    prevmat = Me.DT.Rows(i).Item(0).ToString
                    .Cells(j, 1).Value = DWSIM.App.GetLocalString("Objeto") & ": " & prevmat
                    j = j + 1
                    Do
                        .Cells(j + 1, 1).Value = Me.DT.Rows(i).Item(2)
                        .Cells(j + 1, 2).Value = Me.DT.Rows(i).Item(3)
                        .Cells(j + 1, 3).Value = Me.DT.Rows(i).Item(4)
                        i = i + 1
                        j = j + 1
                        If i < DT.Rows.Count Then actualmat = Me.DT.Rows(i).Item(0).ToString
                    Loop Until actualmat <> prevmat Or i >= DT.Rows.Count
                    j = j + 2
                Loop Until i >= DT.Rows.Count
            End With
            xcl.SaveAs(stream)
            handler.Write(stream)
            MsgBox(DWSIM.App.GetLocalString("XLFileSaved"), MsgBoxStyle.Information, "DWSIM")
        Catch ex As Exception
            MsgBox(ex.ToString, MsgBoxStyle.Exclamation, DWSIM.App.GetLocalString("Erro"))
        Finally
            xcl.Dispose()
        End Try

    End Sub

    Sub CreateAndSaveCSVFile(stream As MemoryStream, handler As IVirtualFile)

        Dim csvtext As New StringBuilder

        Dim i As Integer = 0
        Dim prevmat, actualmat As String
        Do
            actualmat = Me.DT.Rows(i).Item(0).ToString
            prevmat = Me.DT.Rows(i).Item(0).ToString
            csvtext.AppendLine(DWSIM.App.GetLocalString("Objeto") & ": " & prevmat)
            Do
                csvtext.AppendLine(Me.DT.Rows(i).Item(2) & vbTab & Me.DT.Rows(i).Item(3) & vbTab & Me.DT.Rows(i).Item(4))
                i = i + 1
                If i < DT.Rows.Count Then actualmat = Me.DT.Rows(i).Item(0).ToString
            Loop Until actualmat <> prevmat Or i >= DT.Rows.Count
            csvtext.AppendLine()
        Loop Until i >= DT.Rows.Count
        Using writer As New StreamWriter(stream) With {.AutoFlush = True}
            writer.Write(csvtext.ToString())
            handler.Write(stream)
        End Using
    End Sub

    Private Sub TreeViewObj_AfterCheck(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles TreeViewObj.AfterCheck
        If e.Node.Level = 0 Then
            For Each n As TreeNode In e.Node.Nodes
                n.Checked = e.Node.Checked
            Next
        ElseIf e.Node.Level = 1 Then
            If e.Node.Checked Then
                If Not Me.ListView1.Items.ContainsKey(e.Node.Tag) Then Me.ListView1.Items.Add(e.Node.Tag, e.Node.Text, 0).Tag = e.Node.Tag
            Else
                If Me.ListView1.Items.ContainsKey(e.Node.Tag) Then Me.ListView1.Items.RemoveByKey(e.Node.Tag)
            End If
        End If
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim index As Integer = 0
        If Me.ListView1.SelectedItems.Count > 0 Then
            index = Me.ListView1.SelectedItems(0).Index
            If index <> 0 Then
                Dim lvi As ListViewItem = Me.ListView1.SelectedItems(0).Clone
                Me.ListView1.SelectedItems(0).Remove()
                Me.ListView1.Items.Insert(index - 1, lvi)
            End If
        End If
    End Sub

    Private Sub Button2_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim index As Integer = 0
        If Me.ListView1.SelectedItems.Count > 0 Then
            index = Me.ListView1.SelectedItems(0).Index
            If index <> Me.ListView1.Items.Count - 1 Then
                Dim lvi As ListViewItem = Me.ListView1.SelectedItems(0).Clone
                Me.ListView1.SelectedItems(0).Remove()
                Me.ListView1.Items.Insert(index + 1, lvi)
            End If
        End If

    End Sub

    Private Sub FormReportConfig_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub
End Class