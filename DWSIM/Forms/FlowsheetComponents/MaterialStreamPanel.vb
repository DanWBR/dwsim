﻿Imports System.Linq

Public Class MaterialStreamPanel

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Protected Conversor As SystemsOfUnits.Converter
    Protected filename As String = ""
    Protected Flowsheet As FormFlowsheet
    Protected RowsCreated As Boolean = False

    Public Function ReturnForm(ByVal str As String) As WeifenLuo.WinFormsUI.Docking.IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Private Sub UpdateTable()

        Me.Flowsheet = My.Application.ActiveSimulation

        ToolStripLabel1.Text = ""

        If Not Flowsheet Is Nothing Then
            'TABELA DE CORRENTES
            Dim ms As Streams.MaterialStream
            DataGridView1.Columns.Clear()
            RowsCreated = False
            Dim i, n As Integer
            n = Flowsheet.Collections.FlowsheetObjectCollection.Values.Count
            i = 1
            For Each ms In Flowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                ToolStripLabel1.Text = i & "/" & n & "..."
                AddColumn(ms)
                i += 1
            Next
        End If

        ToolStripLabel1.Text = ""

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click

        UpdateTable()

        If DataGridView1.RowCount > 0 Then ToolStripButton2.Enabled = True

    End Sub


    Private Sub frmMatList_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.Flowsheet = My.Application.ActiveSimulation

    End Sub

    Sub AddColumn(ByRef ms As Streams.MaterialStream)

        Me.Flowsheet = My.Application.ActiveSimulation
        Me.Conversor = New SystemsOfUnits.Converter

        Dim su As SystemsOfUnits.Units
        su = Flowsheet.Options.SelectedUnitSystem

        Me.DataGridView1.Columns.Add(ms.Name, ms.GraphicObject.Tag)
        Me.DataGridView1.Columns(ms.Name).SortMode = DataGridViewColumnSortMode.NotSortable

        Dim props As String() = ms.GetProperties(Interfaces.Enums.PropertyType.ALL)
        Dim unit As String = ""

        Me.SuspendLayout()
        Me.DataGridView1.SuspendLayout()

        If Not RowsCreated Then

            'create rows
            For Each prop As String In props
                With Me.DataGridView1.Rows
                    .Add()
                    unit = ms.GetPropertyUnit(prop, su)
                    If unit <> "" Then
                        .Item(.Count - 1).HeaderCell.Value = DWSIM.App.GetPropertyName(prop) & " (" & ms.GetPropertyUnit(prop, su) & ")"
                    Else
                        .Item(.Count - 1).HeaderCell.Value = DWSIM.App.GetPropertyName(prop)
                    End If
                End With
            Next

        End If

        RowsCreated = True

        'populate rows
        Dim col As DataGridViewColumn = Me.DataGridView1.Columns(ms.Name)
        Dim i As Integer = 0
        Dim value As String

        For Each prop As String In props
            value = ms.GetPropertyValue(prop, su)
            If Double.TryParse(value, New Double) Then
                Me.DataGridView1.Rows.Item(i).Cells(col.Index).Value = Format(Double.Parse(value), Flowsheet.Options.NumberFormat)
            Else
                Me.DataGridView1.Rows.Item(i).Cells(col.Index).Value = value
            End If
            i += 1
        Next

        Me.DataGridView1.RowHeadersWidth = 300

        Me.DataGridView1.ResumeLayout()
        Me.ResumeLayout()

        Application.DoEvents()

    End Sub

    Private Sub FloatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FloatToolStripMenuItem.Click, DocumentToolStripMenuItem.Click,
                                                                      DockLeftToolStripMenuItem.Click, DockLeftAutoHideToolStripMenuItem.Click,
                                                                      DockRightAutoHideToolStripMenuItem.Click, DockRightToolStripMenuItem.Click,
                                                                      DockTopAutoHideToolStripMenuItem.Click, DockTopToolStripMenuItem.Click,
                                                                      DockBottomAutoHideToolStripMenuItem.Click, DockBottomToolStripMenuItem.Click

        For Each ts As ToolStripMenuItem In dckMenu.Items
            ts.Checked = False
        Next

        sender.Checked = True

        Select Case sender.Name
            Case "FloatToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
            Case "DocumentToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
            Case "DockLeftToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
            Case "DockLeftAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
            Case "DockRightAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
            Case "DockRightToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
            Case "DockBottomAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottomAutoHide
            Case "DockBottomToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
            Case "DockTopAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTopAutoHide
            Case "DockTopToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
            Case "HiddenToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Hidden
        End Select

    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click
        Clipboard.SetDataObject(Me.DataGridView1.GetClipboardContent)
    End Sub

    Private Sub MaterialStreamPanel_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub
End Class