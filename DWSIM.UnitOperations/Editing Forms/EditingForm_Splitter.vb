Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_Splitter

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.Splitter

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

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            cbOutlet2.Items.Clear()
            cbOutlet2.Items.AddRange(mslist)

            cbOutlet3.Items.Clear()
            cbOutlet3.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(2).IsAttached Then cbOutlet3.SelectedItem = .GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'parameters

            Select Case .OperationMode
                Case UnitOperations.Splitter.OpMode.SplitRatios
                    cbCalcMode.SelectedIndex = 0
                    cbFlowSpec1.Items.Clear()
                    cbFlowSpec2.Items.Clear()

                    tbFlowSpec1.Text = ""
                    tbFlowSpec2.Text = ""

                    TrackBar1.Enabled = True
                    TrackBar2.Enabled = True

                    TrackBar1.Value = Convert.ToDouble(.Ratios(0)) * 100
                    TrackBar2.Value = Convert.ToDouble(.Ratios(1)) * 100

                    tbRatio1.Text = Convert.ToDouble(.Ratios(0)).ToString("N4")
                    tbRatio2.Text = Convert.ToDouble(.Ratios(1)).ToString("N4")

                Case UnitOperations.Splitter.OpMode.StreamMassFlowSpec
                    cbCalcMode.SelectedIndex = 1
                    cbFlowSpec1.Items.Clear()
                    cbFlowSpec1.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.massflow).ToArray)
                    cbFlowSpec1.SelectedItem = units.massflow

                    cbFlowSpec2.Items.Clear()
                    cbFlowSpec2.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.massflow).ToArray)
                    cbFlowSpec2.SelectedItem = units.massflow

                    tbFlowSpec1.Text = su.Converter.ConvertFromSI(units.massflow, .StreamFlowSpec).ToString(nf)
                    tbFlowSpec2.Text = su.Converter.ConvertFromSI(units.massflow, .Stream2FlowSpec).ToString(nf)

                    TrackBar1.Enabled = False
                    TrackBar2.Enabled = False

                Case UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
                    cbCalcMode.SelectedIndex = 2
                    cbFlowSpec1.Items.Clear()
                    cbFlowSpec1.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molarflow).ToArray)
                    cbFlowSpec1.SelectedItem = units.molarflow

                    cbFlowSpec2.Items.Clear()
                    cbFlowSpec2.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molarflow).ToArray)
                    cbFlowSpec2.SelectedItem = units.molarflow

                    tbFlowSpec1.Text = su.Converter.ConvertFromSI(units.molarflow, .StreamFlowSpec).ToString(nf)
                    tbFlowSpec2.Text = su.Converter.ConvertFromSI(units.molarflow, .Stream2FlowSpec).ToString(nf)

                    TrackBar1.Enabled = False
                    TrackBar2.Enabled = False
            End Select

        End With

        Loaded = True

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click
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

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged

        Select Case cbCalcMode.SelectedIndex
            Case 0
                TrackBar1.Enabled = True
                TrackBar2.Enabled = True
                tbRatio1.Enabled = True
                tbRatio2.Enabled = True
                tbFlowSpec1.Enabled = False
                tbFlowSpec2.Enabled = False
                cbFlowSpec1.Enabled = False
                cbFlowSpec2.Enabled = False
                SimObject.OperationMode = UnitOperations.Splitter.OpMode.SplitRatios
            Case 1
                TrackBar1.Enabled = False
                TrackBar2.Enabled = False
                tbRatio1.Enabled = False
                tbRatio2.Enabled = False
                tbFlowSpec1.Enabled = True
                tbFlowSpec2.Enabled = True
                cbFlowSpec1.Enabled = True
                cbFlowSpec2.Enabled = True
                cbFlowSpec1.Items.Clear()
                cbFlowSpec1.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.massflow).ToArray)
                cbFlowSpec1.SelectedItem = units.massflow
                cbFlowSpec2.Items.Clear()
                cbFlowSpec2.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.massflow).ToArray)
                cbFlowSpec2.SelectedItem = units.massflow
                SimObject.OperationMode = UnitOperations.Splitter.OpMode.StreamMassFlowSpec
            Case 2
                TrackBar1.Enabled = False
                TrackBar2.Enabled = False
                tbRatio1.Enabled = False
                tbRatio2.Enabled = False
                tbFlowSpec1.Enabled = True
                tbFlowSpec2.Enabled = True
                cbFlowSpec1.Enabled = True
                cbFlowSpec2.Enabled = True
                cbFlowSpec1.Items.Clear()
                cbFlowSpec1.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molarflow).ToArray)
                cbFlowSpec1.SelectedItem = units.molarflow
                cbFlowSpec2.Items.Clear()
                cbFlowSpec2.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molarflow).ToArray)
                cbFlowSpec2.SelectedItem = units.molarflow
                SimObject.OperationMode = UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
        End Select

    End Sub

    Private Sub cb_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlowSpec1.SelectedIndexChanged, cbFlowSpec2.SelectedIndexChanged

        If Loaded Then
            Try
                Dim sunits = ""
                If SimObject.OperationMode = UnitOperations.Splitter.OpMode.StreamMassFlowSpec Then
                    sunits = units.massflow
                Else
                    sunits = units.molarflow
                End If
                If sender Is cbFlowSpec1 Then
                    tbFlowSpec1.Text = su.Converter.Convert(cbFlowSpec1.SelectedItem.ToString, sunits, Double.Parse(tbFlowSpec1.Text)).ToString(nf)
                    cbFlowSpec1.SelectedItem = sunits
                    UpdateProps(tbFlowSpec1)
                ElseIf sender Is cbFlowSpec2 Then
                    tbFlowSpec2.Text = su.Converter.Convert(cbFlowSpec2.SelectedItem.ToString, sunits, Double.Parse(tbFlowSpec2.Text)).ToString(nf)
                    cbFlowSpec2.SelectedItem = sunits
                    UpdateProps(tbFlowSpec2)
                End If
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Sub UpdateProps(sender As Object)

        Dim uobj = SimObject

        Select Case cbCalcMode.SelectedIndex
            Case 0
                uobj.OperationMode = UnitOperations.Splitter.OpMode.SplitRatios
            Case 1
                uobj.OperationMode = UnitOperations.Splitter.OpMode.StreamMassFlowSpec
            Case 2
                uobj.OperationMode = UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
        End Select

        If sender Is tbFlowSpec1 Then uobj.StreamFlowSpec = su.Converter.ConvertToSI(cbFlowSpec1.SelectedItem.ToString, tbFlowSpec1.Text.ParseExpressionToDouble)
        If sender Is tbFlowSpec2 Then uobj.Stream2FlowSpec = su.Converter.ConvertToSI(cbFlowSpec2.SelectedItem.ToString, tbFlowSpec2.Text.ParseExpressionToDouble)
        If sender Is tbRatio1 Then
            uobj.Ratios(0) = tbRatio1.Text.ParseExpressionToDouble()
            uobj.Ratios(1) = 1.0 - uobj.Ratios(0)
            tbRatio2.Text = CDbl(uobj.Ratios(1)).ToString("N4")
            TrackBar1.Value = CInt(uobj.Ratios(0) * 100)
            TrackBar2.Value = CInt(uobj.Ratios(1) * 100)
        End If
        If sender Is tbRatio2 Then
            uobj.Ratios(1) = tbRatio2.Text.ParseExpressionToDouble()
            uobj.Ratios(0) = 1.0 - uobj.Ratios(1)
            tbRatio1.Text = CDbl(uobj.Ratios(0)).ToString("N4")
            TrackBar1.Value = CInt(uobj.Ratios(0) * 100)
            TrackBar2.Value = CInt(uobj.Ratios(1) * 100)
        End If

        RequestCalc()

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbFlowSpec1.TextChanged, tbFlowSpec2.TextChanged, tbRatio1.TextChanged, tbRatio2.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbFlowSpec1.KeyDown, tbFlowSpec2.KeyDown, tbRatio2.KeyDown, tbRatio1.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

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

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

            End If

        End If

    End Sub

    Private Sub cbOutlet2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet2.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet2.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

            End If

        End If

    End Sub

    Private Sub cbOutlet3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet3.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet3.Text

            If text <> "" Then

                Dim index As Integer = 2

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

            End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub TrackBar1_Scroll(sender As Object, e As EventArgs) Handles TrackBar1.Scroll
        If Loaded Then
            SimObject.Ratios(0) = TrackBar1.Value / 100.0
            If Not SimObject.GraphicObject.OutputConnectors(2).IsAttached Then
                TrackBar2.Value = 100 - TrackBar1.Value
                SimObject.Ratios(1) = 1.0 - SimObject.Ratios(0)
                tbRatio2.Text = Convert.ToDouble(SimObject.Ratios(1)).ToString("N4")
            End If
        End If
        tbRatio1.Text = Convert.ToDouble(SimObject.Ratios(0)).ToString("N4")
    End Sub

    Private Sub TrackBar2_Scroll(sender As Object, e As EventArgs) Handles TrackBar2.Scroll
        If Loaded Then
            SimObject.Ratios(1) = TrackBar2.Value / 100
            If Not SimObject.GraphicObject.OutputConnectors(2).IsAttached Then
                TrackBar1.Value = 100 - TrackBar2.Value
                SimObject.Ratios(0) = 1.0 - SimObject.Ratios(1)
                tbRatio1.Text = CDbl(SimObject.Ratios(0)).ToString("N4")
            End If
        End If
        tbRatio2.Text = CDbl(SimObject.Ratios(1)).ToString("N4")
    End Sub

    Private Sub btnDisconnectOutlet2_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet2.Click
        If cbOutlet2.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo)
            cbOutlet2.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet3_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet3.Click
        If cbOutlet3.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo)
            cbOutlet3.SelectedItem = Nothing
        End If
    End Sub

    Private Sub TrackBar1_MouseUp(sender As Object, e As MouseEventArgs) Handles TrackBar1.MouseUp, TrackBar2.MouseUp
        RequestCalc()
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click, btnCreateAndConnectOutlet2.Click, btnCreateAndConnectOutlet3.Click

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

        ElseIf sender Is btnCreateAndConnectOutlet3 Then

            oidx = 2

        End If

        If iidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(iidx).Position.X - 50, sgobj.InputConnectors(iidx).Position.Y, "")

            If sgobj.InputConnectors(iidx).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(iidx).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, iidx)

        End If

        If oidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(oidx).Position.X + 30, sgobj.OutputConnectors(oidx).Position.Y, "")

            If sgobj.OutputConnectors(oidx).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(oidx).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, oidx, 0)

        End If

        UpdateInfo()
        RequestCalc()

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