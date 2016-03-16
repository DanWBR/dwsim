Imports System.IO
Imports System.Text
Imports Microsoft.Scripting.Hosting
Imports System.Drawing.Text
Imports System.Reflection
Imports System.ComponentModel
Public Class ScriptEditorControl

    Public form As FormFlowsheet
    Public reader As Jolt.XmlDocCommentReader

    Private Sub chkLink_CheckedChanged(sender As Object, e As EventArgs) Handles chkLink.CheckedChanged
        Label1.Enabled = chkLink.Checked
        Label2.Enabled = chkLink.Checked
        cbLinkedEvent.Enabled = chkLink.Checked
        cbLinkedObject.Enabled = chkLink.Checked
    End Sub

    Private Sub ScriptEditorControl_Load(sender As Object, e As EventArgs) Handles Me.Load

        Me.txtScript.Tag = 1

        cbLinkedObject.Items.AddRange(New String() {"Simulation", "Solver"})
        cbLinkedEvent.Items.AddRange(New String() {"Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer"})

        For Each obj As DWSIM.SimulationObjects.UnitOperations.BaseClass In form.Collections.ObjectCollection.Values
            cbLinkedObject.Items.Add(obj.GraphicObject.Tag)
        Next

    End Sub

    Private Sub cbLinkedObject_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbLinkedObject.SelectedIndexChanged

        Select Case cbLinkedObject.SelectedIndex
            Case 0
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer"})
            Case 1
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Solver Started", "Solver Finished", "Recycle Loop"})
            Case Else
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Object Calculation Started", "Object Calculation Finished", "Object Calculation Error"})
        End Select

        cbLinkedEvent.SelectedIndex = 0

    End Sub

    Private Sub txtScript_TextChanged(sender As Object, e As EventArgs) Handles txtScript.TextChanged

        txtScript.SetColumnMargins()

        ShowAutoComplete(txtScript)

        ShowToolTip(txtScript, reader)

    End Sub

End Class
