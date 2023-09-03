Imports System.IO
Imports System.Text
Imports Microsoft.Scripting.Hosting
Imports System.Drawing.Text
Imports System.Reflection
Imports System.ComponentModel
Public Class ScriptEditorControlMono

    Public form As FormFlowsheet

    Private Sub chkLink_CheckedChanged(sender As Object, e As EventArgs) Handles chkLink.CheckedChanged
        Label1.Enabled = chkLink.Checked
        Label2.Enabled = chkLink.Checked
        cbLinkedEvent.Enabled = chkLink.Checked
        cbLinkedObject.Enabled = chkLink.Checked
    End Sub

    Private Sub ScriptEditorControl_Load(sender As Object, e As EventArgs) Handles Me.Load

        cbLinkedObject.Items.AddRange(New String() {"Simulation", "Solver", "Integrator"})
        cbLinkedEvent.Items.AddRange(New String() {"Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer"})

        For Each obj In form.SimulationObjects.Values
            cbLinkedObject.Items.Add(obj.GraphicObject.Tag)
        Next

    End Sub

    Private Sub cbLinkedObject_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbLinkedObject.SelectedIndexChanged

        ExtensionMethods.ChangeDefaultFont(Me)

        Select Case cbLinkedObject.SelectedIndex
            Case 0
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer"})
            Case 1
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Solver Started", "Solver Finished", "Recycle Loop"})
            Case 2
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Integrator Started", "Integrator Finished", "Integrator Error", "Integrator Step"})
            Case Else
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Object Calculation Started", "Object Calculation Finished", "Object Calculation Error"})
        End Select

        cbLinkedEvent.SelectedIndex = 0

    End Sub

End Class
