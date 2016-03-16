Imports DWSIM.DWSIM.SimulationObjects
Imports System.Linq

Public Class FormPropSelection

    Inherits System.Windows.Forms.Form

    Public formC As FormFlowsheet
    Public ssheet As SpreadsheetForm

    Public ssmode As Boolean = True

    Public wi As DWSIM.Outros.WatchItem

    Public mode As Integer

    Private Sub UICVSelectorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        formC = My.Application.ActiveSimulation

        With TreeView1.Nodes
            .Clear()
            .Add(DWSIM.App.GetLocalString("CorrentedeMatria"), DWSIM.App.GetLocalString("CorrentedeMatria"))
            .Add(DWSIM.App.GetLocalString("Correntedeenergia"), DWSIM.App.GetLocalString("Correntedeenergia"))
            .Add(DWSIM.App.GetLocalString("Misturador"), DWSIM.App.GetLocalString("Misturador"))
            .Add(DWSIM.App.GetLocalString("Divisor"), DWSIM.App.GetLocalString("Divisor"))
            .Add(DWSIM.App.GetLocalString("Tubulao"), DWSIM.App.GetLocalString("Tubulao"))
            .Add(DWSIM.App.GetLocalString("Vlvula"), DWSIM.App.GetLocalString("Vlvulas"))
            .Add(DWSIM.App.GetLocalString("Bomba"), DWSIM.App.GetLocalString("Bombas"))
            .Add(DWSIM.App.GetLocalString("Tanque"), DWSIM.App.GetLocalString("Tanque"))
            .Add(DWSIM.App.GetLocalString("VasoSeparadorGL"), DWSIM.App.GetLocalString("VasoSeparadorGL"))
            .Add(DWSIM.App.GetLocalString("CompressorAdiabtico"), DWSIM.App.GetLocalString("CompressorAdiabtico"))
            .Add(DWSIM.App.GetLocalString("TurbinaAdiabtica"), DWSIM.App.GetLocalString("TurbinaAdiabtica"))
            .Add(DWSIM.App.GetLocalString("Aquecedor"), DWSIM.App.GetLocalString("Aquecedor"))
            .Add(DWSIM.App.GetLocalString("Resfriador"), DWSIM.App.GetLocalString("Resfriador"))
            .Add(DWSIM.App.GetLocalString("ReatorConversao"), DWSIM.App.GetLocalString("ReatorConversao"))
            .Add(DWSIM.App.GetLocalString("ReatorEquilibrio"), DWSIM.App.GetLocalString("ReatorEquilibrio"))
            .Add(DWSIM.App.GetLocalString("ReatorGibbs"), DWSIM.App.GetLocalString("ReatorGibbs"))
            .Add(DWSIM.App.GetLocalString("ReatorCSTR"), DWSIM.App.GetLocalString("ReatorCSTR"))
            .Add(DWSIM.App.GetLocalString("ReatorPFR"), DWSIM.App.GetLocalString("ReatorPFR"))
            .Add(DWSIM.App.GetLocalString("HeatExchanger"), DWSIM.App.GetLocalString("HeatExchanger"))
            .Add(DWSIM.App.GetLocalString("ShortcutColumn"), DWSIM.App.GetLocalString("ShortcutColumn"))
            .Add(DWSIM.App.GetLocalString("DistillationColumn"), DWSIM.App.GetLocalString("DistillationColumn"))
            .Add(DWSIM.App.GetLocalString("AbsorptionColumn"), DWSIM.App.GetLocalString("AbsorptionColumn"))
            .Add(DWSIM.App.GetLocalString("ReboiledAbsorber"), DWSIM.App.GetLocalString("ReboiledAbsorber"))
            .Add(DWSIM.App.GetLocalString("RefluxedAbsorber"), DWSIM.App.GetLocalString("RefluxedAbsorber"))
            .Add(DWSIM.App.GetLocalString("Reciclo"), DWSIM.App.GetLocalString("Reciclo"))
            .Add(DWSIM.App.GetLocalString("EnergyRecycle"), DWSIM.App.GetLocalString("EnergyRecycle"))
            .Add(DWSIM.App.GetLocalString("Especificao"), DWSIM.App.GetLocalString("Especificao"))
            .Add(DWSIM.App.GetLocalString("Ajuste"), DWSIM.App.GetLocalString("Ajuste"))
            .Add(DWSIM.App.GetLocalString("ComponentSeparator"), DWSIM.App.GetLocalString("ComponentSeparator"))
            .Add(DWSIM.App.GetLocalString("OrificePlate"), DWSIM.App.GetLocalString("OrificePlate"))
            .Add(DWSIM.App.GetLocalString("CapeOpenUnitOperation1"), DWSIM.App.GetLocalString("CapeOpenUnitOperation1"))
            .Add(DWSIM.App.GetLocalString("CustomUnitOp"), DWSIM.App.GetLocalString("CustomUnitOp"))
            .Add(DWSIM.App.GetLocalString("ExcelUnitOp"), DWSIM.App.GetLocalString("ExcelUnitOp"))
            .Add(DWSIM.App.GetLocalString("FlowsheetUnitOp"), DWSIM.App.GetLocalString("FlowsheetUnitOp"))
            .Add(DWSIM.App.GetLocalString("SolidsSeparator"), DWSIM.App.GetLocalString("SolidsSeparator"))
            .Add(DWSIM.App.GetLocalString("Filter"), DWSIM.App.GetLocalString("Filter"))
        End With
   
    End Sub

    Private Sub ListBox1_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles TreeView1.AfterSelect

        Dim obj As Microsoft.MSDN.Samples.GraphicObjects.GraphicObject

        TreeView2.Nodes.Clear()
        TreeView3.Nodes.Clear()

        With TreeView2.Nodes
            Select Case TreeView1.SelectedNode.Index
                Case 0
                    For Each obj In formC.Collections.MaterialStreamCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 1
                    For Each obj In formC.Collections.EnergyStreamCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 2
                    For Each obj In formC.Collections.MixerCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 3
                    For Each obj In formC.Collections.SplitterCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 4
                    For Each obj In formC.Collections.PipeCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 5
                    For Each obj In formC.Collections.ValveCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 6
                    For Each obj In formC.Collections.PumpCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 7
                    For Each obj In formC.Collections.TankCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 8
                    For Each obj In formC.Collections.SeparatorCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 9
                    For Each obj In formC.Collections.CompressorCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 10
                    For Each obj In formC.Collections.TurbineCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 11
                    For Each obj In formC.Collections.HeaterCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 12
                    For Each obj In formC.Collections.CoolerCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 13
                    For Each obj In formC.Collections.ReactorConversionCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 14
                    For Each obj In formC.Collections.ReactorEquilibriumCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 15
                    For Each obj In formC.Collections.ReactorGibbsCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 16
                    For Each obj In formC.Collections.ReactorCSTRCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 17
                    For Each obj In formC.Collections.ReactorPFRCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 18
                    For Each obj In formC.Collections.HeatExchangerCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 19
                    For Each obj In formC.Collections.ShortcutColumnCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 20
                    For Each obj In formC.Collections.DistillationColumnCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 21
                    For Each obj In formC.Collections.AbsorptionColumnCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 22
                    For Each obj In formC.Collections.ReboiledAbsorberCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 23
                    For Each obj In formC.Collections.RefluxedAbsorberCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 24
                    For Each obj In formC.Collections.RecycleCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 25
                    For Each obj In formC.Collections.EnergyRecycleCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 26
                    For Each obj In formC.Collections.SpecCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 27
                    For Each obj In formC.Collections.AdjustCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 28
                    For Each obj In formC.Collections.ComponentSeparatorCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 29
                    For Each obj In formC.Collections.OrificePlateCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 30
                    For Each obj In formC.Collections.CapeOpenUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 31
                    For Each obj In formC.Collections.CustomUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 32
                    For Each obj In formC.Collections.ExcelUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 33
                    For Each obj In formC.Collections.FlowsheetUOCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 34
                    For Each obj In formC.Collections.SolidsSeparatorCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 35
                    For Each obj In formC.Collections.FilterCollection.Values
                        .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
            End Select
        End With

    End Sub

    Private Sub ListBox2_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles TreeView2.AfterSelect

        TreeView3.Nodes.Clear()

        With TreeView3.Nodes
            Dim key As String = e.Node.Tag


            Dim properties As String()
            If mode = 0 Then
                properties = formC.Collections.ObjectCollection(key).GetProperties(SimulationObjects_BaseClass.PropertyType.ALL)
            Else
                properties = formC.Collections.ObjectCollection(key).GetProperties(SimulationObjects_BaseClass.PropertyType.WR)
            End If

            For Each prop As String In properties
                .Add(prop, DWSIM.App.GetPropertyName(prop)).Tag = prop
            Next

        End With

    End Sub

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton2.Click
        Me.Close()
    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click
        If Not Me.TreeView3.SelectedNode Is Nothing Then
            If ssmode Then
                Dim cparams As DWSIM.Utilities.Spreadsheet.SpreadsheetCellParameters
                cparams = ssheet.DataGridView1.SelectedCells(0).Tag
                If mode = 0 Then
                    cparams.Expression = ":" & Me.TreeView2.SelectedNode.Tag & "," & Me.TreeView3.SelectedNode.Tag
                    cparams.CellType = DWSIM.Utilities.Spreadsheet.VarType.Read
                    ssheet.UpdateValue(ssheet.DataGridView1.SelectedCells(0), cparams.Expression)
                Else
                    cparams.ObjectID = Me.TreeView2.SelectedNode.Tag
                    cparams.PropID = Me.TreeView3.SelectedNode.Tag
                    cparams.CellType = DWSIM.Utilities.Spreadsheet.VarType.Write
                    ssheet.UpdateValue(ssheet.DataGridView1.SelectedCells(0), cparams.Expression)
                End If
            Else
                Dim obj As SimulationObjects_BaseClass = formC.GetFlowsheetSimulationObject(Me.TreeView2.SelectedNode.Text)
                If obj.GetProperties(SimulationObjects_BaseClass.PropertyType.RO).Contains(Me.TreeView3.SelectedNode.Tag) Then
                    Me.wi = New DWSIM.Outros.WatchItem(Me.TreeView2.SelectedNode.Tag, Me.TreeView3.SelectedNode.Tag, True)
                Else
                    Me.wi = New DWSIM.Outros.WatchItem(Me.TreeView2.SelectedNode.Tag, Me.TreeView3.SelectedNode.Tag, False)
                End If
            End If
        End If
        Me.Close()
    End Sub

End Class