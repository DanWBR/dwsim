Imports System.Linq
Imports DWSIM.Interfaces.Enums.GraphicObjects.ObjectType

Public Class FormPropSelection

    Inherits System.Windows.Forms.Form

    Public formC As FormFlowsheet
    Public ssheet As unvell.ReoGrid.ReoGridControl

    Public ssmode As Boolean = True

    Public wi As Extras.WatchItem

    Public mode As Integer

    Private Sub UICVSelectorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        formC = My.Application.ActiveSimulation

        With TreeView1.Nodes
            .Clear()
            .Add(DWSIM.App.GetLocalString("CorrentedeMatria"), DWSIM.App.GetLocalString("CorrentedeMatria"))
            .Add(DWSIM.App.GetLocalString("CorrentedeEnergia"), DWSIM.App.GetLocalString("CorrentedeEnergia"))
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

        Dim obj As Drawing.SkiaSharp.GraphicObjects.GraphicObject

        TreeView2.Nodes.Clear()
        TreeView3.Nodes.Clear()

        With TreeView2.Nodes
            Select Case TreeView1.SelectedNode.Index
                Case 0
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = MaterialStream Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 1
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = EnergyStream Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 2
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = NodeIn Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 3
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = NodeOut Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 4
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Pipe Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 5
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Valve Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 6
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Pump Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 7
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Tank Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 8
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Vessel Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 9
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Compressor Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 10
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Expander Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 11
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Heater Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 12
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Cooler Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 13
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = RCT_Conversion Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 14
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = RCT_Equilibrium Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 15
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = RCT_Gibbs Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 16
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = RCT_CSTR Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 17
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = RCT_PFR Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 18
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = HeatExchanger Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 19
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = ShortcutColumn Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 20
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = DistillationColumn Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 21
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = AbsorptionColumn Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 22
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = ReboiledAbsorber Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 23
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = RefluxedAbsorber Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 24
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = OT_Recycle Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 25
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = OT_EnergyRecycle Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 26
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = OT_Spec Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 27
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = OT_Adjust Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 28
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = ComponentSeparator Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 29
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = OrificePlate Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 30
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = CapeOpenUO Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 31
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = CustomUO Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 32
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = ExcelUO Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 33
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = FlowsheetUO Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 34
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = SolidSeparator Then .Add(obj.Name, obj.Tag).Tag = obj.Name
                    Next
                Case 35
                    For Each obj In formC.Collections.GraphicObjectCollection.Values
                        If obj.ObjectType = Filter Then .Add(obj.Name, obj.Tag).Tag = obj.Name
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
                properties = formC.Collections.FlowsheetObjectCollection(key).GetProperties(Interfaces.Enums.PropertyType.ALL)
            Else
                properties = formC.Collections.FlowsheetObjectCollection(key).GetProperties(Interfaces.Enums.PropertyType.WR)
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
            Dim separator = ";"
            If ssmode Then
                Dim scell = ssheet.CurrentWorksheet.GetCell(ssheet.CurrentWorksheet.SelectionRange.StartPos)
                If scell Is Nothing Then scell = ssheet.CurrentWorksheet.CreateAndGetCell(ssheet.CurrentWorksheet.SelectionRange.StartPos)
                If mode = 0 Then
                    scell.Formula = String.Format("GETPROPVAL({3}{1}{3}{0}{3}{2}{3})",
                                                  separator,
                                                  Me.TreeView2.SelectedNode.Tag,
                                                  Me.TreeView3.SelectedNode.Tag,
                                                  Chr(34))
                Else
                    scell.Formula = String.Format("SETPROPVAL({3}{1}{3}{0}{3}{2}{3}{0}{3}{4}{3})",
                                                  separator,
                                                  Me.TreeView2.SelectedNode.Tag,
                                                  Me.TreeView3.SelectedNode.Tag,
                                                  Chr(34),
                                                  If(scell.Formula = "", scell.Data, scell.Formula))
                End If
            Else
                Dim obj As SharedClasses.UnitOperations.BaseClass = formC.GetFlowsheetSimulationObject(Me.TreeView2.SelectedNode.Text)
                If obj.GetProperties(Interfaces.Enums.PropertyType.RO).Contains(Me.TreeView3.SelectedNode.Tag) Then
                    Me.wi = New Extras.WatchItem(Me.TreeView2.SelectedNode.Tag, Me.TreeView3.SelectedNode.Tag, True)
                Else
                    Me.wi = New Extras.WatchItem(Me.TreeView2.SelectedNode.Tag, Me.TreeView3.SelectedNode.Tag, False)
                End If
            End If
        End If
        Me.Close()
    End Sub

End Class