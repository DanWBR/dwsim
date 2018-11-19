Imports DWSIM.Drawing.SkiaSharp
Imports SkiaSharp.Views.Desktop

Public Class FlowsheetSurfaceControl

    Inherits SkiaSharp.Views.Desktop.SKControl

    Public WithEvents FlowsheetSurface As New Drawing.SkiaSharp.GraphicsSurface

    Public FlowsheetObject As FormFlowsheet

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        AllowDrop = True

    End Sub

    Protected Overrides Sub OnPaintSurface(e As SKPaintSurfaceEventArgs)

        If FlowsheetSurface IsNot Nothing Then FlowsheetSurface.UpdateSurface(e.Surface)

    End Sub

    Private Sub FlowsheetSurfaceControl_MouseUp(sender As Object, e As MouseEventArgs) Handles Me.MouseUp
        FlowsheetSurface.InputRelease()
        Invalidate()
    End Sub

    Private Sub FlowsheetSurfaceControl_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        FlowsheetSurface.InputMove(e.X, e.Y)
        Invalidate()
    End Sub

    Private Sub FlowsheetSurfaceControl_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown
        FlowsheetSurface.InputPress(e.X, e.Y)
        Invalidate()
    End Sub

    Private Sub FlowsheetSurfaceControl_DragEnter(sender As Object, e As DragEventArgs) Handles Me.DragEnter
        e.Effect = DragDropEffects.All
    End Sub

    Private Sub FlowsheetSurfaceControl_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles Me.MouseDoubleClick
        Dim obj = FlowsheetSurface.SelectedObject
        If (obj Is Nothing) Then
            FlowsheetSurface.ZoomAll((Width * GlobalSettings.Settings.DpiScale), (Height * GlobalSettings.Settings.DpiScale))
            FlowsheetSurface.ZoomAll((Width * GlobalSettings.Settings.DpiScale), (Height * GlobalSettings.Settings.DpiScale))
            FlowsheetObject.FormSurface.TSTBZoom.Text = FlowsheetSurface.Zoom.ToString("###%")
            Invalidate()
        End If

    End Sub

    Private Sub FlowsheetSurfaceControl_MouseWheel(sender As Object, e As MouseEventArgs) Handles Me.MouseWheel
        FlowsheetSurface.Zoom += e.Delta / 4 / 100.0
        FlowsheetObject.FormSurface.TSTBZoom.Text = FlowsheetSurface.Zoom.ToString("###%")
        Invalidate()
    End Sub

End Class
