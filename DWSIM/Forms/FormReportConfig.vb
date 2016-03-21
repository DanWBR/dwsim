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

Imports DWSIM.DWSIM.SimulationObjects
Imports NetOffice
Imports Excel = NetOffice.ExcelApi
Imports NetOffice.ExcelApi.Enums
Imports System.Text
Imports System.IO
Imports System.Xml.Xsl

Public Class FormReportConfig

    Inherits System.Windows.Forms.Form

    Protected frm As FormFlowsheet

    Dim DT As New DataTable
    Dim filename As String
    Dim Conversor As New DWSIM.SystemsOfUnits.Converter
    Dim su As New DWSIM.SystemsOfUnits.Units
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


        Dim baseobj As DWSIM.SimulationObjects.UnitOperations.BaseClass
        Dim properties() As String
        Dim description As String
        Dim objtype As GraphicObjects.ObjectType
        Dim propidx, r1, r2, r3, r4, r5, r6 As Integer
        Dim inclcond, inclcomp, inclmist, inclvap, inclliqm, inclliq1, inclliq2, inclaq As Boolean
        r1 = 5
        r2 = 12
        r3 = 30
        r4 = 48
        r5 = 66
        r6 = 84
        inclcond = Me.CheckBox1.Checked
        inclcomp = Me.CheckBox2.Checked
        inclmist = Me.CheckBox3.Checked
        inclvap = Me.CheckBox4.Checked
        inclliqm = Me.CheckBox5.Checked
        inclliq1 = Me.CheckBox6.Checked
        inclliq2 = Me.CheckBox7.Checked
        inclaq = Me.CheckBox8.Checked

        For Each lvi As ListViewItem In Me.ListView1.Items
            baseobj = frm.Collections.FlowsheetObjectCollection(lvi.Tag)
            properties = baseobj.GetProperties(DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType.ALL)
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
                If inclmist Then
                    For propidx = r1 To r2 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                    If inclcomp Then
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetLocalString("FraomolarnaMistura"), "", ""})
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(0).Compounds.Values
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetComponentName(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                        Next
                    End If
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
                    If inclcomp Then
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetLocalString("FraomolarnaPhaseVapor"), "", ""})
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(2).Compounds.Values
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetComponentName(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                        Next
                    End If
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
                    If inclcomp Then
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(1).Compounds.Values
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetComponentName(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                        Next
                    End If
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
                    If inclcomp Then
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(3).Compounds.Values
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetComponentName(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                        Next
                    End If
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
                    If inclcomp Then
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(4).Compounds.Values
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetComponentName(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                        Next
                    End If
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
                    If inclcomp Then
                        DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In frm.Collections.FlowsheetObjectCollection(lvi.Tag).Phases(6).Compounds.Values
                            DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetComponentName(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                        Next
                    End If
                End If
            Else
                For Each prop As String In properties
                    DT.Rows.Add(New String() {lvi.Text, description, DWSIM.App.GetPropertyName(prop), Format(baseobj.GetPropertyValue(prop, su), nf), baseobj.GetPropertyUnit(prop, su)})
                Next
            End If
        Next

    End Sub

    Private Sub FormReportConfig_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        e.Cancel = True
        Me.Hide()
    End Sub

    Private Sub FormReportConfig_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        frm = My.Application.ActiveSimulation
        Me.Text = frm.Options.SimNome & " - " & DWSIM.App.GetLocalString("RelatoriodaSimulacao")

        Me.ListView1.Items.Clear()

        KButton4.Enabled = Not DWSIM.App.IsRunningOnMono
        KButton5.Enabled = Not DWSIM.App.IsRunningOnMono

        Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass

        For Each n As TreeNode In Me.TreeViewObj.Nodes
            n.Nodes.Clear()
        Next

        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(0).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(1).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(2).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(3).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(4).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(5).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(6).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(7).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(8).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(9).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(10).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(11).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(12).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(13).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(14).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(15).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(16).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(17).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(18).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(19).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(20).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(21).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(22).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(23).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(24).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
        For Each obj In frm.Collections.FlowsheetObjectCollection.Values
            Me.TreeViewObj.Nodes(25).Nodes.Add(obj.Name, obj.GraphicObject.Tag).Tag = obj.Name
        Next
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton5.Click

        Me.FillDataTable()

        Dim MyReport As New NeoDataType.MyNeoReport.Report
        Select Case My.Settings.CultureInfo
            Case "pt-BR"
                MyReport.LoadFrom(My.Application.Info.DirectoryPath & "\data\report_pt-BR.mr6")
            Case "en"
                MyReport.LoadFrom(My.Application.Info.DirectoryPath & "\data\report_en-US.mr6")
            Case Else
                MyReport.LoadFrom(My.Application.Info.DirectoryPath & "\data\report_en-US.mr6")
        End Select

        Dim ds As NeoDataType.MyNeoReport.TableDataSource = MyReport.Page.Sections(2).DataSource
        ds.Table = DT

        Dim titlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label5")
        Dim comlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label6")
        Dim verlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label4")

        verlbl.Text = "DWSIM " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor

        If Not frm.Options.SimNome Is Nothing Then titlbl.Text = frm.Options.SimNome
        If Not frm.Options.SimComentario Is Nothing Then comlbl.Text = frm.Options.SimComentario

        Dim nlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageFooter.Items("Label")
        nlbl.Text = ""

        MyReport.ShowPreview(NeoDataType.MyNeoReport.PreviewWindowState.Maximized)

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton4.Click

        Me.FillDataTable()

        Dim MyReport As New NeoDataType.MyNeoReport.Report
        Select Case My.Settings.CultureInfo
            Case "pt-BR"
                MyReport.LoadFrom(My.Application.Info.DirectoryPath & "\data\report_pt-BR.mr6")
            Case "en"
                MyReport.LoadFrom(My.Application.Info.DirectoryPath & "\data\report_en-US.mr6")
            Case Else
                MyReport.LoadFrom(My.Application.Info.DirectoryPath & "\data\report_en-US.mr6")
        End Select

        Dim ds As NeoDataType.MyNeoReport.TableDataSource = CType(MyReport.Page.Sections(2).DataSource, NeoDataType.MyNeoReport.TableDataSource)
        ds.Table = DT

        Dim nlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageFooter.Items("Label")
        nlbl.Text = ""

        Dim verlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label4")

        verlbl.Text = "DWSIM v." & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor

        Dim titlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label5")
        Dim comlbl As NeoDataType.MyNeoReport.Label = MyReport.Page.PageHeader.Items("Label6")
        If Not frm.Options.SimNome Is Nothing Then titlbl.Text = frm.Options.SimNome
        If Not frm.Options.SimComentario Is Nothing Then comlbl.Text = frm.Options.SimComentario

        MyReport.Print()

    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton7.Click

        Me.SaveFileDialog1.DefaultExt = True
        If Me.SaveFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            If Me.SaveFileDialog1.FilterIndex = 1 Then
                Me.filename = Me.SaveFileDialog1.FileName
                If Not (Me.filename Is Nothing) Then
                    Me.FillDataTable()
                    Me.DT.TableName = DWSIM.App.GetLocalString("Resultados")
                    Dim output As String = ""
                    Using sri As New MemoryStream()
                        Me.DT.WriteXml(sri)
                        sri.Position = 0
                        ' xslInput is a string that contains xsl
                        Using srt As New StringReader(File.ReadAllText(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "report_transform.xsl"))
                            ' xmlInput is a string that contains xml
                            Using xrt As XmlReader = XmlReader.Create(srt)
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
                    File.WriteAllText(Me.filename, output)
                End If
            ElseIf Me.SaveFileDialog1.FilterIndex = 2 Then
                Me.filename = Me.SaveFileDialog1.FileName
                If Not (Me.filename Is Nothing) Then
                    Me.FillDataTable()
                    Me.CreateAndSaveCSVFile()
                End If
            ElseIf Me.SaveFileDialog1.FilterIndex = 3 Then
                If DWSIM.App.IsRunningOnMono Then
                    MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                Else
                    Me.filename = Me.SaveFileDialog1.FileName
                    If Not (Me.filename Is Nothing) Then
                        Me.FillDataTable()
                        Me.CreateAndSaveExcelFile()
                    End If
                End If
            End If
        End If

    End Sub

    Sub CreateAndSaveExcelFile()

        Dim xcl As New Excel.Application()
        Dim mybook As Excel.Workbook = xcl.Workbooks.Add()
        Dim mysheet As Excel.Worksheet = mybook.Worksheets.Add()

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
            mybook.SaveAs(filename:=Me.filename, fileFormat:=XlFileFormat.xlWorkbookNormal)
            MsgBox(DWSIM.App.GetLocalString("XLFileSaved"), MsgBoxStyle.Information, "DWSIM")
        Catch ex As Exception
            MsgBox(ex.ToString, MsgBoxStyle.Exclamation, DWSIM.App.GetLocalString("Erro"))
        Finally
            mybook.Close(saveChanges:=False)
            xcl.Quit()
            xcl.Dispose()
        End Try

    End Sub

    Sub CreateAndSaveCSVFile()

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

        IO.File.WriteAllText(Me.filename, csvtext.ToString)

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
End Class