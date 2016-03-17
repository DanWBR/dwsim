Imports Microsoft.Msdn.Samples.GraphicObjects.ObjectType

Public Class UIRVSelectorForm

    Inherits System.Windows.Forms.Form

    Public formC As FormFlowsheet
    Public selectionDataCV As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ControlledObjectInfo
    Public selectionDataRV As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ReferenceObjectInfo
    Public gi As GridItem

    Private Sub UIRVSelectorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        formC = My.Application.ActiveSimulation

        Label3.Text = Me.selectionDataCV.m_Name & " / " & DWSIM.App.GetPropertyName(Me.selectionDataCV.m_Property)

        Dim obj As Microsoft.Msdn.Samples.GraphicObjects.GraphicObject

        TreeView1.Nodes.Clear()

        With TreeView1.Nodes
            For Each obj In formC.Collections.GraphicObjectCollection.Values
                If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
            Next

        End With

        If formC.Collections.FlowsheetObjectCollection.ContainsKey(Me.selectionDataRV.m_ID) Then
            If Not Me.selectionDataRV.m_ID = "" Then
                If Me.TreeView1.Nodes.ContainsKey(Me.selectionDataRV.m_ID) Then
                    Me.TreeView1.SelectedNode = Me.TreeView1.Nodes.Find(Me.selectionDataRV.m_ID, True)(0)
                    Call Me.TreeView1_AfterSelect(sender, New TreeViewEventArgs(Me.TreeView1.SelectedNode, TreeViewAction.ByKeyboard))
                End If
            End If
        Else
            With Me.selectionDataRV
                .m_ID = ""
                .m_Name = ""
                .m_Property = ""
                .m_Type = ""
            End With
        End If

    End Sub

    Private Sub TreeView1_AfterSelect(ByVal sender As Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles TreeView1.AfterSelect
        Me.selectionDataRV.m_Type = Me.selectionDataCV.m_Type
        Me.selectionDataRV.m_Property = Me.selectionDataCV.m_Property
        Me.selectionDataRV.m_ID = Me.TreeView1.SelectedNode.Tag.ToString
        Me.selectionDataRV.m_Name = Me.TreeView1.SelectedNode.Text
    End Sub

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton2.Click
        Me.Close()
    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click
        Me.Close()
        Call formC.FormProps.PGEx1_PropertyValueChanged(sender, New System.Windows.Forms.PropertyValueChangedEventArgs(gi, Nothing))
    End Sub
End Class