Imports System.Timers

Public Class ObjectEditorForm

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Dim lastX, lastY As Integer
    Public WithEvents ToolTipValues As New ToolTip
    Private components As System.ComponentModel.IContainer
    Private _currentToolTipControl As Control = Nothing

    Public InspReportBar As InspectorReportBar

    Private WithEvents DisplayTimer As New Timers.Timer

    Private DisplayTime As DateTime

    Private Sub InitializeComponent()

        DisplayTimer.Interval = 100

        Me.SuspendLayout()
        '
        'ObjectEditorForm
        '
        Me.ClientSize = New System.Drawing.Size(284, 262)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "ObjectEditorForm"
        Me.ResumeLayout(False)

    End Sub

    Protected Sub Editor_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        If ((e.X <> Me.lastX) OrElse (e.Y <> Me.lastY)) Then
            Me.lastX = e.X
            Me.lastY = e.Y
            Dim mouseloc, controlLoc, relativeloc As Drawing.Point
            Dim eventloc As Drawing.Point = e.Location
            If TypeOf sender Is GroupBox Then eventloc = e.Location + DirectCast(sender, GroupBox).Location
            Dim control As Control
            If TypeOf sender Is TabPage Then
                control = sender.GetChildAtPoint(eventloc)
                If control Is Nothing Then control = sender
            Else
                control = GetChildAtPoint(eventloc)
            End If
            If Not control Is Nothing Then
                Dim lastCrp As Control = control
                While Not control Is Nothing
                    lastCrp = control
                    controlLoc = PointToScreen(control.Location)
                    controlLoc = control.Parent?.PointToScreen(control.Location)
                    mouseloc = PointToScreen(eventloc)
                    relativeloc = New Drawing.Point(mouseloc.X - controlLoc.X, mouseloc.Y - controlLoc.Y)
                    control = control.GetChildAtPoint(relativeloc)
                End While
                If (_currentToolTipControl IsNot Nothing AndAlso _currentToolTipControl IsNot lastCrp) Then
                    ToolTipValues.Hide(_currentToolTipControl)
                End If
                If Not lastCrp Is Nothing AndAlso TypeOf lastCrp Is TextBox Then
                    Dim toolTipString As String = ToolTipValues.GetToolTip(lastCrp)
                    If toolTipString <> "" Then
                        ToolTipValues.Hide(lastCrp)
                        ToolTipValues.Show(toolTipString, lastCrp, lastCrp.Width, lastCrp.Height)
                        _currentToolTipControl = lastCrp
                    End If
                Else
                    _currentToolTipControl = Nothing
                End If
            End If
        End If

    End Sub

    Private Sub ToolTipValues_Popup(sender As Object, e As PopupEventArgs) Handles ToolTipValues.Popup
        DisplayTime = Date.Now
        DisplayTimer.Start()
    End Sub

    Private Sub DisplayTimer_Elapsed(sender As Object, e As ElapsedEventArgs) Handles DisplayTimer.Elapsed
        If (e.SignalTime - DisplayTime).TotalSeconds >= 10 Then
            Try
                ToolTipValues.Hide(_currentToolTipControl)
            Catch ex As Exception
            Finally
                DisplayTimer.Stop()
            End Try
        End If
    End Sub

End Class
