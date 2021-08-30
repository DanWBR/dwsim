Public Class EditingForm_Flowsheet_Viewer

    Inherits SharedClasses.ObjectEditorForm

    Public fsuo As UnitOperations.Flowsheet

    Private Sub FlowsheetUOViewerForm_Load(sender As Object, e As EventArgs) Handles Me.Load

        fsuo.Fsheet.GetSurfaceControl().Flowsheet = fsuo.Fsheet
        fsuo.Fsheet.GetSurfaceControl().FControl.FlowsheetObject = fsuo.Fsheet
        Me.Controls.Add(fsuo.Fsheet.GetSurfaceControl().FControl)
        fsuo.Fsheet.GetSurfaceControl().FlowsheetSurface.ZoomAll(Width, Height)
        fsuo.Fsheet.GetSurfaceControl().FlowsheetSurface.ZoomAll(Width, Height)
        fsuo.Fsheet.GetSurfaceControl().Invalidate()
        Me.Invalidate()

    End Sub

End Class