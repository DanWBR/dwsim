Public Class EditingForm_Flowsheet_Viewer

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public fsuo As UnitOperations.Flowsheet

    Private Sub FlowsheetUOViewerForm_Load(sender As Object, e As EventArgs) Handles Me.Load

        fsuo.Fsheet.GetSurface.Flowsheet = fsuo.Fsheet
        Me.Controls.Add(fsuo.Fsheet.GetSurface.FlowsheetDesignSurface)
        fsuo.Fsheet.GetSurface.FlowsheetDesignSurface.Zoom = 1.0#
        fsuo.Fsheet.GetSurface.FlowsheetDesignSurface.Center()
        fsuo.Fsheet.GetSurface.FlowsheetDesignSurface.Invalidate()
        fsuo.Fsheet.GetSurface.Invalidate()
        Me.Invalidate()

    End Sub

End Class