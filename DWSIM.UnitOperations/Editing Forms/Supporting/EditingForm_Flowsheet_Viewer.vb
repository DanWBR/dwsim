Public Class EditingForm_Flowsheet_Viewer

    Inherits SharedClasses.ObjectEditorForm

    Public fsuo As UnitOperations.Flowsheet

    Private Sub FlowsheetUOViewerForm_Load(sender As Object, e As EventArgs) Handles Me.Load

        fsuo.Fsheet.GetSurface.Flowsheet = fsuo.Fsheet
        fsuo.Fsheet.GetSurface.FControl.FlowsheetObject = fsuo.Fsheet
        Me.Controls.Add(fsuo.Fsheet.GetSurface.FControl)
        fsuo.Fsheet.GetSurface.FlowsheetSurface.ZoomAll(Width, Height)
        fsuo.Fsheet.GetSurface.FlowsheetSurface.ZoomAll(Width, Height)
        fsuo.Fsheet.GetSurface.Invalidate()
        Me.Invalidate()

    End Sub

End Class