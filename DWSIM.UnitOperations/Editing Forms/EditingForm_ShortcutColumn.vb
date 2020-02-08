Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_ShortcutColumn

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.ShortcutColumn

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        If Host.Items.Where(Function(x) x.Name.Contains(SimObject.GraphicObject.Tag)).Count > 0 Then
            If InspReportBar Is Nothing Then
                InspReportBar = New SharedClasses.InspectorReportBar
                InspReportBar.Dock = DockStyle.Bottom
                AddHandler InspReportBar.Button1.Click, Sub()
                                                            Dim iwindow As New Inspector.Window2
                                                            iwindow.SelectedObject = SimObject
                                                            iwindow.Show(DockPanel)
                                                        End Sub
                Me.Controls.Add(InspReportBar)
                InspReportBar.BringToFront()
            End If
        Else
            If InspReportBar IsNot Nothing Then
                Me.Controls.Remove(InspReportBar)
                InspReportBar = Nothing
            End If
        End If

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            Me.Text = .GraphicObject.Tag & " (" & .GetDisplayName() & ")"

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = System.Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = System.Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
                    lblStatus.ForeColor = System.Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = System.Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            'connections

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray
            Dim eslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.Tag).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            cbOutlet2.Items.Clear()
            cbOutlet2.Items.AddRange(mslist)

            cbEnergy1.Items.Clear()
            cbEnergy1.Items.AddRange(eslist)

            cbEnergy2.Items.Clear()
            cbEnergy2.Items.AddRange(eslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag

            If .GraphicObject.InputConnectors(1).IsAttached Then cbEnergy2.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.EnergyConnector.IsAttached Then cbEnergy1.SelectedItem = .GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.Add("Default")
            cbFlashAlg.Items.AddRange(flashalgos)
            If .PreferredFlashAlgorithmTag <> "" Then cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag Else cbFlashAlg.SelectedIndex = 0

            'parameters

            cbRebPressureUnits.Items.Clear()
            cbRebPressureUnits.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbRebPressureUnits.SelectedItem = units.pressure

            cbCondPressureUnits.Items.Clear()
            cbCondPressureUnits.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbCondPressureUnits.SelectedItem = units.pressure

            cbLKey.Items.Clear()
            cbLKey.Items.AddRange(SimObject.FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToArray)
   
            cbHKey.Items.Clear()
            cbHKey.Items.AddRange(SimObject.FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToArray)
   
            If cbLKey.Items.Contains(.m_lightkey) Then cbLKey.SelectedItem = .m_lightkey
            If cbHKey.Items.Contains(.m_heavykey) Then cbHKey.SelectedItem = .m_heavykey

            tbHKmolfrac.Text = .m_heavykeymolarfrac.ToString(nf)
            tbLKmolfrac.Text = .m_lightkeymolarfrac.ToString(nf)
            tbRefluxRatio.Text = .m_refluxratio.ToString(nf)
            tbCondPressure.Text = su.Converter.ConvertFromSI(units.pressure, .m_condenserpressure).ToString(nf)
            tbRebPressure.Text = su.Converter.ConvertFromSI(units.pressure, .m_boilerpressure).ToString(nf)

            If .condtype = UnitOperations.ShortcutColumn.CondenserType.PartialCond Then
                rbPartialCond.Checked = True
            Else
                rbTotalCond.Checked = True
            End If

            'results

            gridResults.Rows.Clear()

            If .Calculated Then

                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCMinimumRefluxRatio"), .m_Rmin.ToString(nf)})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCNminstages"), .m_Nmin.ToString(nf)})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCNstages"), .m_N.ToString(nf)})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCOptimalFeedStage"), .ofs.ToString(nf)})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCStrippingLiquid"), su.Converter.ConvertFromSI(units.molarflow, .L_).ToString(nf), units.molarflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCRectifyLiquid"), su.Converter.ConvertFromSI(units.molarflow, .L).ToString(nf), units.molarflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCStrippingVapor"), su.Converter.ConvertFromSI(units.molarflow, .V_).ToString(nf), units.molarflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCRectifyVapor"), su.Converter.ConvertFromSI(units.molarflow, .V).ToString(nf), units.molarflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCCondenserDuty"), su.Converter.ConvertFromSI(units.heatflow, .m_Qc).ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("SCReboilerDuty"), su.Converter.ConvertFromSI(units.heatflow, .m_Qb).ToString(nf), units.heatflow})
          
            End If

        End With

        Loaded = True

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Thermodynamics.Calculator.ConfigureFlashInstance(SimObject, cbFlashAlg.SelectedItem.ToString)

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs) Handles btnDisconnectInlet1.Click
        If cbInlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet1.Click
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
        End If
    End Sub

    Sub UpdateProps(sender As Object)

        'Pressão na Saída
        'Variação da Pressão

        Dim uobj = SimObject

        If rbTotalCond.Checked Then
            uobj.condtype = UnitOperations.ShortcutColumn.CondenserType.TotalCond
        Else
            uobj.condtype = UnitOperations.ShortcutColumn.CondenserType.PartialCond
        End If

        If sender Is tbHKmolfrac Then uobj.m_heavykeymolarfrac = tbHKmolfrac.Text.ParseExpressionToDouble
        If sender Is tbLKmolfrac Then uobj.m_lightkeymolarfrac = tbLKmolfrac.Text.ParseExpressionToDouble
        If sender Is tbRefluxRatio Then uobj.m_refluxratio = tbRefluxRatio.Text.ParseExpressionToDouble
        If sender Is tbRebPressure Then uobj.m_boilerpressure = su.Converter.ConvertToSI(cbRebPressureUnits.SelectedItem.ToString, tbRebPressure.Text.ParseExpressionToDouble)
        If sender Is tbCondPressure Then uobj.m_condenserpressure = su.Converter.ConvertToSI(cbCondPressureUnits.SelectedItem.ToString, tbCondPressure.Text.ParseExpressionToDouble)

        RequestCalc()

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbLKmolfrac.TextChanged, tbHKmolfrac.TextChanged,
                                                                        tbRefluxRatio.TextChanged,
                                                                        tbRebPressure.TextChanged, tbCondPressure.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbLKmolfrac.KeyDown, tbHKmolfrac.KeyDown,
                                                                        tbRefluxRatio.KeyDown,
                                                                        tbRebPressure.KeyDown, tbCondPressure.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            SimObject.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedItem.ToString
            RequestCalc()
        End If
    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbInlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

            End If

        End If

    End Sub

    Private Sub cbOutlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet1.Text

            If text <> "" Then

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(0).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

            End If

        End If

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click, btnCreateAndConnectOutlet2.Click, btnCreateAndConnectEnergy1.Click, btnCreateAndConnectEnergy2.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        Dim iidx As Integer = -1
        Dim oidx As Integer = -1

        If sender Is btnCreateAndConnectInlet1 Then

            iidx = 0

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            oidx = 0

        ElseIf sender Is btnCreateAndConnectOutlet2 Then

            oidx = 1

        ElseIf sender Is btnCreateAndConnectEnergy2 Then

            iidx = 1

        End If

        If iidx = 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(iidx).Position.X - 50, sgobj.InputConnectors(iidx).Position.Y, "")

            If sgobj.InputConnectors(iidx).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(iidx).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, iidx)

        End If

        If iidx = 1 Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(iidx).Position.X + 50, sgobj.InputConnectors(iidx).Position.Y, "")

            obj.GraphicObject.FlippedH = True

            If sgobj.InputConnectors(iidx).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(iidx).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, iidx)

        End If

        If oidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(oidx).Position.X + 30, sgobj.OutputConnectors(oidx).Position.Y, "")

            If sgobj.OutputConnectors(oidx).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(oidx).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, oidx, 0)

        End If

        If sender Is btnCreateAndConnectEnergy1 Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.EnergyConnector.Position.X + 30, sgobj.EnergyConnector.Position.Y, "")

            If sgobj.EnergyConnector.IsAttached Then fs.DisconnectObjects(sgobj, sgobj.EnergyConnector.AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub cbInlet2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet2.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet2.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

            End If

        End If

    End Sub

    Private Sub cbOutlet2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy1.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.EnergyConnector.IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.EnergyConnector.AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

            End If

        End If

    End Sub

    Private Sub btnDisconnect2_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet2.Click
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet2_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy1.Click
        If cbEnergy1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo)
            cbEnergy1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub cbEnergy2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy2.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy2.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

            End If

        End If

    End Sub

    Private Sub btnDisconnectEnergy2_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy2.Click
        If cbEnergy2.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbEnergy2.SelectedItem = Nothing
        End If
    End Sub

    Private Sub rbTotalCond_CheckedChanged(sender As Object, e As EventArgs) Handles rbTotalCond.CheckedChanged, rbPartialCond.CheckedChanged

        If Loaded Then
            If rbTotalCond.Checked Then SimObject.condtype = UnitOperations.ShortcutColumn.CondenserType.TotalCond Else SimObject.condtype = UnitOperations.ShortcutColumn.CondenserType.PartialCond
            RequestCalc()
        End If

    End Sub

    Private Sub cbLKey_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbLKey.SelectedIndexChanged
        If Loaded Then SimObject.m_lightkey = cbLKey.SelectedItem.ToString
    End Sub

    Private Sub cbHKey_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbHKey.SelectedIndexChanged
        If Loaded Then SimObject.m_heavykey = cbHKey.SelectedItem.ToString
    End Sub

    Private Sub cbCondPressureUnits_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCondPressureUnits.SelectedIndexChanged, cbRebPressureUnits.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbCondPressureUnits Then
                    tbCondPressure.Text = su.Converter.Convert(cbCondPressureUnits.SelectedItem.ToString, units.temperature, Double.Parse(tbCondPressure.Text)).ToString(nf)
                    cbCondPressureUnits.SelectedItem = units.pressure
                    UpdateProps(tbCondPressure)
                ElseIf sender Is cbRebPressureUnits Then
                    tbRebPressure.Text = su.Converter.Convert(cbRebPressureUnits.SelectedItem.ToString, units.deltaT, Double.Parse(tbRebPressure.Text)).ToString(nf)
                    cbRebPressureUnits.SelectedItem = units.pressure
                    UpdateProps(tbRebPressure)
                End If
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class