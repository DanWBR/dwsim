Public Class UIRVSelectorForm

    Inherits System.Windows.Forms.Form

    Public formC As FormFlowsheet
    Public selectionDataCV As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ControlledObjectInfo
    Public selectionDataRV As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ReferenceObjectInfo
    Public gi As GridItem

    Private Sub UIRVSelectorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        formC = My.Application.ActiveSimulation

        Label3.Text = Me.selectionDataCV.m_Name & " / " & DWSIM.App.GetPropertyName(Me.selectionDataCV.m_Property)

        Dim obj As Microsoft.MSDN.Samples.GraphicObjects.GraphicObject

        TreeView1.Nodes.Clear()

        With TreeView1.Nodes
            Select Case Me.selectionDataCV.m_Type
                Case DWSIM.App.GetLocalString("CorrentedeMatria")
                    For Each obj In formC.Collections.MaterialStreamCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("CorrentedeEnergyFlow")
                    For Each obj In formC.Collections.EnergyStreamCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Misturador")
                    For Each obj In formC.Collections.MixerCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Divisor")
                    For Each obj In formC.Collections.SplitterCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Tubulao")
                    For Each obj In formC.Collections.PipeCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Vlvulas")
                    For Each obj In formC.Collections.ValveCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Bombas")
                    For Each obj In formC.Collections.PumpCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Tanque")
                    For Each obj In formC.Collections.TankCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("VasoSeparadorGL")
                    For Each obj In formC.Collections.SeparatorCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("CompressorAdiabtico")
                    For Each obj In formC.Collections.CompressorCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("TurbinaAdiabtica")
                    For Each obj In formC.Collections.TurbineCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Aquecedor")
                    For Each obj In formC.Collections.HeaterCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Resfriador")
                    For Each obj In formC.Collections.CoolerCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ReatorConversao")
                    For Each obj In formC.Collections.ReactorConversionCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ReatorEquilibrio")
                    For Each obj In formC.Collections.ReactorEquilibriumCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ReatorGibbs")
                    For Each obj In formC.Collections.ReactorGibbsCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ReatorCSTR")
                    For Each obj In formC.Collections.ReactorCSTRCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ReatorPFR")
                    For Each obj In formC.Collections.ReactorPFRCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("HeatExchanger")
                    For Each obj In formC.Collections.HeatExchangerCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ShortcutColumn")
                    For Each obj In formC.Collections.ShortcutColumnCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("DistillationColumn")
                    For Each obj In formC.Collections.DistillationColumnCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("AbsorptionColumn")
                    For Each obj In formC.Collections.AbsorptionColumnCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ReboiledAbsorber")
                    For Each obj In formC.Collections.ReboiledAbsorberCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("RefluxedAbsorber")
                    For Each obj In formC.Collections.RefluxedAbsorberCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Reciclo")
                    For Each obj In formC.Collections.RecycleCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("EnergyRecycle")
                    For Each obj In formC.Collections.EnergyRecycleCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Especificao")
                    For Each obj In formC.Collections.SpecCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Ajuste")
                    For Each obj In formC.Collections.AdjustCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ComponentSeparator")
                    For Each obj In formC.Collections.ComponentSeparatorCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("OrificePlate")
                    For Each obj In formC.Collections.OrificePlateCollection.Values
                        If obj.Name <> Me.selectionDataCV.m_ID Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("CapeOpenUnitOperation1")
                    For Each obj In formC.Collections.CapeOpenUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("CustomUnitOp")
                    For Each obj In formC.Collections.CustomUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("ExcelUnitOp")
                    For Each obj In formC.Collections.ExcelUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("FlowsheetUnitOp")
                    For Each obj In formC.Collections.FlowsheetUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("SolidsSeparator")
                    For Each obj In formC.Collections.SolidsSeparatorCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case DWSIM.App.GetLocalString("Filter")
                    For Each obj In formC.Collections.FilterCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
            End Select
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