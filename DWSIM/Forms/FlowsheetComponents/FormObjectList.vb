Imports OutlookStyleControls

Public Class FormObjectList

    Public Flowsheet As FormFlowsheet

    Private Sub FormObjectList_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Public Sub UpdateData()

        OutlookGrid1.Rows.Clear()

        For Each obj In Flowsheet.SimulationObjects.Values
            OutlookGrid1.Rows.Add(New Object() {obj.Name, obj.GraphicObject.Tag, obj.GetDisplayName(), "", "", ""})
        Next

        OutlookGrid1.GroupTemplate.Column = OutlookGrid1.Columns(2)

        OutlookGrid1.Sort(OutlookGrid1.Columns(2), ComponentModel.ListSortDirection.Ascending)

    End Sub

    Private Sub FormObjectList_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        UpdateData()

    End Sub

    Private Sub OutlookGrid1_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles OutlookGrid1.CellContentClick

        Dim objID = OutlookGrid1.Rows(e.RowIndex).Cells(0).Value.ToString()
        Select Case e.ColumnIndex
            Case 3
                Try
                    Dim obj = Flowsheet.SimulationObjects(objID).GraphicObject
                    Dim center As Point = New Point(Flowsheet.FormSurface.SplitContainerHorizontal.Panel1.Width / 2, Flowsheet.FormSurface.SplitContainerHorizontal.Panel1.Height / 2)
                    Flowsheet.FormSurface.FlowsheetSurface.OffsetAll(center.X / Flowsheet.FormSurface.FlowsheetSurface.Zoom - obj.X, center.Y / Flowsheet.FormSurface.FlowsheetSurface.Zoom - obj.Y)
                    Flowsheet.FormSurface.FlowsheetSurface.SelectedObject = obj
                    Flowsheet.FormSurface.FControl.Invalidate()
                    Flowsheet.FormSurface.FControl.Invalidate()
                Catch ex As Exception
                End Try
            Case 4
                Flowsheet.SimulationObjects(objID).DisplayEditForm()
            Case 5
                Try
                    Dim obj = Flowsheet.SimulationObjects(objID).GraphicObject
                    Flowsheet.FormSurface.FlowsheetSurface.SelectedObject = obj
                    Flowsheet.DeleteSelectedObject(sender, e, obj)
                    UpdateData()
                Catch ex As Exception
                End Try
        End Select

    End Sub

    Private Sub OutlookGrid1_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles OutlookGrid1.CellValueChanged
        If e.RowIndex >= 0 Then
            Try
                Dim objID = OutlookGrid1.Rows(e.RowIndex).Cells(0).Value.ToString()
                Select Case e.ColumnIndex
                    Case 1
                        Dim obj = Flowsheet.SimulationObjects(objID).GraphicObject
                        obj.Tag = OutlookGrid1.Rows(e.RowIndex).Cells(1).Value
                        Flowsheet.UpdateInterface()
                        Flowsheet.SimulationObjects(objID).UpdateEditForm()
                End Select
            Catch ex As Exception
            End Try
        End If
    End Sub
End Class