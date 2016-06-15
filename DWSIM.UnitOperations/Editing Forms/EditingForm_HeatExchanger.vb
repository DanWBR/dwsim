Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_HeatExchanger

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As UnitOperations.Valve

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

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            Me.Text = .GetDisplayName() & ": " & .GraphicObject.Tag

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
                    lblStatus.ForeColor = Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = Drawing.Color.Black
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

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.AddRange(flashalgos)
            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag

            'parameters

            cbHotFluidPDrop.Items.Clear()
            cbHotFluidPDrop.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbHotFluidPDrop.SelectedItem = units.pressure

            cbColdFluidPDrop.Items.Clear()
            cbColdFluidPDrop.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbColdFluidPDrop.SelectedItem = units.deltaP

            cbColdFluidOutletT.Items.Clear()
            cbColdFluidOutletT.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbColdFluidOutletT.SelectedItem = units.temperature

            cbHotFluidOutletT.Items.Clear()
            cbHotFluidOutletT.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbHotFluidOutletT.SelectedItem = units.deltaT

            Dim uobj = SimObject

            Select Case uobj.CalcMode
                Case UnitOperations.Valve.CalculationMode.OutletPressure
                    cbCalcMode.SelectedIndex = 0
                Case UnitOperations.Valve.CalculationMode.DeltaP
                    cbCalcMode.SelectedIndex = 1
            End Select

            tbHotFluidPDrop.Text = su.Converter.ConvertFromSI(units.temperature, uobj.OutletPressure.GetValueOrDefault).ToString(nf)
            tbColdFluidPDrop.Text = su.Converter.ConvertFromSI(units.deltaP, uobj.DeltaP.GetValueOrDefault).ToString(nf)
            tbColdFluidOutletT.Text = su.Converter.ConvertFromSI(units.temperature, uobj.OutletTemperature).ToString(nf)
            tbHotFluidOutletT.Text = su.Converter.ConvertFromSI(units.deltaT, uobj.DeltaT.GetValueOrDefault).ToString(nf)

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
        If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
        Me.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
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

        'Pressão na Saída
        'Variação da Pressão
        Select Case cbCalcMode.SelectedIndex
            Case 0
                tbColdFluidPDrop.Enabled = False
                tbHotFluidPDrop.Enabled = True
                SimObject.CalcMode = UnitOperations.Valve.CalculationMode.OutletPressure
            Case 1
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = False
                SimObject.CalcMode = UnitOperations.Valve.CalculationMode.DeltaP
        End Select

    End Sub

    Private Sub cb_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbColdFluidPDrop.SelectedIndexChanged,
                                                                                  cbHotFluidPDrop.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbHotFluidPDrop Then
                    tbHotFluidPDrop.Text = su.Converter.Convert(cbHotFluidPDrop.SelectedItem.ToString, units.pressure, Double.Parse(tbHotFluidPDrop.Text)).ToString(nf)
                    cbHotFluidPDrop.SelectedItem = units.pressure
                    UpdateProps(tbHotFluidPDrop)
                ElseIf sender Is cbColdFluidPDrop Then
                    tbColdFluidPDrop.Text = su.Converter.Convert(cbColdFluidPDrop.SelectedItem.ToString, units.deltaP, Double.Parse(tbColdFluidPDrop.Text)).ToString(nf)
                    cbColdFluidPDrop.SelectedItem = units.deltaP
                    UpdateProps(tbColdFluidPDrop)
                End If
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Sub UpdateProps(sender As Object)

        'Pressão na Saída
        'Variação da Pressão

        Dim uobj = SimObject

        Select Case cbCalcMode.SelectedIndex
            Case 0
                uobj.CalcMode = UnitOperations.Valve.CalculationMode.OutletPressure
            Case 1
                uobj.CalcMode = UnitOperations.Valve.CalculationMode.DeltaP
        End Select

        If sender Is tbHotFluidPDrop Then uobj.OutletPressure = su.Converter.ConvertToSI(cbHotFluidPDrop.SelectedItem.ToString, tbHotFluidPDrop.Text)
        If sender Is tbColdFluidPDrop Then uobj.DeltaP = su.Converter.ConvertToSI(cbColdFluidPDrop.SelectedItem.ToString, tbColdFluidPDrop.Text)

        RequestCalc()

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbColdFluidPDrop.TextChanged, tbHotFluidPDrop.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If Double.TryParse(tbox.Text, New Double()) Then
            tbox.ForeColor = Drawing.Color.Blue
        Else
            tbox.ForeColor = Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbColdFluidPDrop.KeyDown, tbHotFluidPDrop.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = Drawing.Color.Blue Then

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

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

            End If

        End If

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        Dim iidx As Integer = -1
        Dim oidx As Integer = -1

        If sender Is btnCreateAndConnectInlet1 Then

            iidx = 0

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            oidx = 0

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

End Class