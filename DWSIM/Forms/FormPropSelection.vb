Imports System.Linq

Public Class FormPropSelection

    Inherits System.Windows.Forms.Form

    Public formC As FormFlowsheet
    Public ssheet As unvell.ReoGrid.ReoGridControl

    Public ssmode As Boolean = True

    Public wi As Extras.WatchItem

    Public mode As Integer

    Private Sub UICVSelectorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        formC = My.Application.ActiveSimulation

        lvObject.Sorting = SortOrder.Ascending

        lvType.Items.Add("All Added Objects")
        lvType.Items.AddRange(FormMain.ObjectList.Keys.OrderBy(Function(x) x).Select(Function(x) New ListViewItem(x)).ToArray())

        lvUnits.Enabled = ssmode
        If ssmode Then btnOK.Enabled = False

    End Sub

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnOK.Click
        Dim separator = ";"
        If lvObject.SelectedItems.Count = 0 Then
            MessageBox.Show("Please select an object.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If
        If lvProp.SelectedItems.Count = 0 Then
            MessageBox.Show("Please select a property.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If
        If ssmode Then
            Dim scell = ssheet.CurrentWorksheet.GetCell(ssheet.CurrentWorksheet.SelectionRange.StartPos)
            If scell Is Nothing Then scell = ssheet.CurrentWorksheet.CreateAndGetCell(ssheet.CurrentWorksheet.SelectionRange.StartPos)
            Dim units As String = ""
            If lvUnits.SelectedItems.Count > 0 Then
                units = lvUnits.SelectedItems(0).Text
            End If
            If mode = 0 Then
                scell.Formula = String.Format("GETPROPVAL({3}{1}{3}{0}{3}{2}{3}{0}{3}{4}{3})",
                                              separator,
                                              lvObject.SelectedItems(0).Tag,
                                              lvProp.SelectedItems(0).Tag,
                                              Chr(34),
                                              units)
            Else
                scell.Formula = String.Format("SETPROPVAL({3}{1}{3}{0}{3}{2}{3}{0}{3}{4}{3}{0}{3}{5}{3})",
                                              separator,
                                              lvObject.SelectedItems(0).Tag,
                                              lvProp.SelectedItems(0).Tag,
                                              Chr(34),
                                              If(scell.Formula = "", scell.Data, scell.Formula),
                                              units)
            End If
        Else
            Dim obj As SharedClasses.UnitOperations.BaseClass = formC.GetFlowsheetSimulationObject(lvObject.SelectedItems(0).Text)
            If obj.GetProperties(Interfaces.Enums.PropertyType.RO).Contains(lvProp.SelectedItems(0).Tag) Then
                Me.wi = New Extras.WatchItem(lvObject.SelectedItems(0).Tag, lvProp.SelectedItems(0).Tag, True)
            Else
                Me.wi = New Extras.WatchItem(lvObject.SelectedItems(0).Tag, lvProp.SelectedItems(0).Tag, False)
            End If
        End If
        Me.Close()
    End Sub

    Private Sub lvType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles lvType.SelectedIndexChanged

        If lvType.SelectedItems.Count > 0 Then
            If lvType.SelectedItems(0).Text = "All Added Objects" Then
                lvObject.Items.Clear()
                For Each obj In formC.SimulationObjects.Values.OrderBy(Function(o) o.GraphicObject.Tag)
                    lvObject.Items.Add(New ListViewItem(obj.GraphicObject.Tag) With {.Tag = obj.Name})
                Next
            Else
                Dim obj = FormMain.ObjectList(lvType.SelectedItems(0).Text)
                Dim objs = formC.SimulationObjects.Values.Where(Function(x) x.GetType().Equals(obj.GetType())).ToList()
                lvObject.Items.Clear()
                For Each obj In objs
                    lvObject.Items.Add(New ListViewItem(obj.GraphicObject.Tag) With {.Tag = obj.Name})
                Next
            End If
        End If

    End Sub

    Private Sub lvObject_SelectedIndexChanged(sender As Object, e As EventArgs) Handles lvObject.SelectedIndexChanged

        If lvObject.SelectedItems.Count > 0 Then
            lvProp.Items.Clear()
            With lvProp.Items
                Dim key As String = lvObject.SelectedItems(0).Tag
                Dim properties As String()
                If mode = 0 Then
                    properties = formC.Collections.FlowsheetObjectCollection(key).GetProperties(Interfaces.Enums.PropertyType.ALL)
                Else
                    properties = formC.Collections.FlowsheetObjectCollection(key).GetProperties(Interfaces.Enums.PropertyType.WR)
                End If
                For Each prop As String In properties
                    .Add(DWSIM.App.GetPropertyName(prop)).Tag = prop
                Next
            End With
        End If

    End Sub

    Private Sub lvProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles lvProp.SelectedIndexChanged

        If lvProp.SelectedItems.Count > 0 Then

            lvUnits.Items.Clear()

            Dim key As String = lvProp.SelectedItems(0).Tag
            Dim obj = formC.GetFlowsheetSimulationObject(lvObject.SelectedItems(0).Text)
            Dim unit = obj.GetPropertyUnit(key)
            Dim su = formC.FlowsheetOptions.SelectedUnitSystem
            Dim units = su.GetUnitSet(su.GetUnitType(unit))

            For Each item In units
                lvUnits.Items.Add(item)
            Next

            If ssmode Then
                If lvProp.SelectedItems.Count > 0 Then btnOK.Enabled = True
            End If

        End If

    End Sub

    Private Sub FormPropSelection_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        ColumnHeader1.Width = lvType.Width - 5
        ColumnHeader2.Width = lvObject.Width - 5
        ColumnHeader3.Width = lvProp.Width - 5
        ColumnHeader4.Width = lvUnits.Width - 5
        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

End Class