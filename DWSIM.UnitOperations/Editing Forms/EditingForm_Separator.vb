Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_Separator

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property VesselObject As UnitOperations.Vessel

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EF_Mixer_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        units = VesselObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = VesselObject.FlowSheet.FlowsheetOptions.NumberFormat

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        Loaded = False

        With VesselObject

            chkActive.Checked = VesselObject.GraphicObject.Active

            Me.Text = .GetDisplayName() & ": " & .GraphicObject.Tag

            lblTag.Text = .GraphicObject.Tag & " (" & .FlowSheet.GetTranslatedString(.ComponentDescription) & ")"
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    lblStatus.ForeColor = Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray

            cbInlet1.Items.Clear()
            cbInlet2.Items.Clear()
            cbInlet3.Items.Clear()
            cbInlet4.Items.Clear()
            cbInlet5.Items.Clear()
            cbInlet6.Items.Clear()

            cbInlet1.Items.AddRange(mslist)
            cbInlet2.Items.AddRange(mslist)
            cbInlet3.Items.AddRange(mslist)
            cbInlet4.Items.AddRange(mslist)
            cbInlet5.Items.AddRange(mslist)
            cbInlet6.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet2.Items.Clear()
            cbOutlet3.Items.Clear()

            cbOutlet1.Items.AddRange(mslist)
            cbOutlet2.Items.AddRange(mslist)
            cbOutlet3.Items.AddRange(mslist)

            If Not .GetInletMaterialStream(0) Is Nothing Then cbInlet1.SelectedItem = .GetInletMaterialStream(0).GraphicObject.Tag
            If Not .GetInletMaterialStream(1) Is Nothing Then cbInlet2.SelectedItem = .GetInletMaterialStream(1).GraphicObject.Tag
            If Not .GetInletMaterialStream(2) Is Nothing Then cbInlet3.SelectedItem = .GetInletMaterialStream(2).GraphicObject.Tag
            If Not .GetInletMaterialStream(3) Is Nothing Then cbInlet4.SelectedItem = .GetInletMaterialStream(3).GraphicObject.Tag
            If Not .GetInletMaterialStream(4) Is Nothing Then cbInlet5.SelectedItem = .GetInletMaterialStream(4).GraphicObject.Tag
            If Not .GetInletMaterialStream(5) Is Nothing Then cbInlet6.SelectedItem = .GetInletMaterialStream(5).GraphicObject.Tag

            If Not .GetOutletMaterialStream(0) Is Nothing Then cbOutlet1.SelectedItem = .GetOutletMaterialStream(0).GraphicObject.Tag
            If Not .GetOutletMaterialStream(1) Is Nothing Then cbOutlet2.SelectedItem = .GetOutletMaterialStream(1).GraphicObject.Tag
            If Not .GetOutletMaterialStream(2) Is Nothing Then cbOutlet3.SelectedItem = .GetOutletMaterialStream(2).GraphicObject.Tag

            'parameters

            Select Case .PressureCalculation
                Case UnitOperations.Vessel.PressureBehavior.Minimum
                    cbPressureCalcMode.SelectedIndex = 0
                Case UnitOperations.Vessel.PressureBehavior.Average
                    cbPressureCalcMode.SelectedIndex = 1
                Case UnitOperations.Vessel.PressureBehavior.Maximum
                    cbPressureCalcMode.SelectedIndex = 2
            End Select

            cbTemp.Items.Clear()
            cbTemp.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbTemp.SelectedItem = units.temperature

            cbPress.Items.Clear()
            cbPress.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbPress.SelectedItem = units.pressure

            chkOverrideP.Checked = .OverrideP
            chkOverrideT.Checked = .OverrideT

            tbTemperature.Text = su.Converter.ConvertFromSI(units.temperature, .FlashTemperature).ToString(nf)
            tbPressure.Text = su.Converter.ConvertFromSI(units.pressure, .FlashPressure).ToString(nf)

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray

            cbPropPack.Items.Clear()

            cbPropPack.Items.AddRange(proppacks)

            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = [Enum].GetNames(.PreferredFlashAlgorithm.GetType)

            cbFlashAlg.Items.Clear()

            cbFlashAlg.Items.AddRange(flashalgos)

            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithm.ToString

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

        End With

        Loaded = True

    End Sub

    Sub HandleInletConnections(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged, cbInlet2.SelectedIndexChanged, cbInlet3.SelectedIndexChanged, cbInlet4.SelectedIndexChanged, cbInlet5.SelectedIndexChanged, cbInlet6.SelectedIndexChanged
        If Loaded Then UpdateInletConnection(sender)
    End Sub

    Sub HandleOutletConnections(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged, cbOutlet2.SelectedIndexChanged, cbOutlet3.SelectedIndexChanged
        If Loaded Then UpdateOutletConnection(sender)
    End Sub

    Sub UpdateInletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(cb.Name.Length - 1)) - 1

            Dim gobj = VesselObject.GraphicObject
            Dim flowsheet = VesselObject.FlowSheet

            If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Exit Sub
            End If
            If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
            flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

        End If

    End Sub

    Sub UpdateOutletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(cb.Name.Length - 1)) - 1

            Dim gobj = VesselObject.GraphicObject
            Dim flowsheet = VesselObject.FlowSheet

            If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Exit Sub
            End If
            If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
            flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

        End If

    End Sub

    Private Sub btnDisconnect_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click, btnDisconnect2.Click, btnDisconnect3.Click,
                                                                            btnDisconnect4.Click, btnDisconnect5.Click, btnDisconnect6.Click,
                                                                            btnDisconnectOutlet1.Click, btnDisconnectOutlet2.Click, btnDisconnectOutlet3.Click

        Dim iindex As Integer = -1
        Dim oindex As Integer = -1

        Select Case DirectCast(sender, Button).Name
            Case "btnDisconnect1"
                If cbInlet1.SelectedItem.ToString <> "" Then
                    iindex = 0
                    cbInlet1.SelectedItem = Nothing
                End If
            Case "btnDisconnect2"
                If cbInlet2.SelectedItem.ToString <> "" Then
                    iindex = 1
                    cbInlet2.SelectedItem = Nothing
                End If
            Case "btnDisconnect3"
                If cbInlet3.SelectedItem.ToString <> "" Then
                    iindex = 2
                    cbInlet3.SelectedItem = Nothing
                End If
            Case "btnDisconnect4"
                If cbInlet4.SelectedItem.ToString <> "" Then
                    iindex = 3
                    cbInlet4.SelectedItem = Nothing
                End If
            Case "btnDisconnect5"
                If cbInlet5.SelectedItem.ToString <> "" Then
                    iindex = 4
                    cbInlet5.SelectedItem = Nothing
                End If
            Case "btnDisconnect6"
                If cbInlet6.SelectedItem.ToString <> "" Then
                    iindex = 5
                    cbInlet6.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet1"
                If cbOutlet1.SelectedItem.ToString <> "" Then
                    oindex = 0
                    cbOutlet1.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet2"
                If cbOutlet1.SelectedItem.ToString <> "" Then
                    oindex = 1
                    cbOutlet2.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet3"
                If cbOutlet1.SelectedItem.ToString <> "" Then
                    oindex = 2
                    cbOutlet3.SelectedItem = Nothing
                End If
        End Select

        If iindex > -1 Then VesselObject.FlowSheet.DisconnectObjects(VesselObject.GraphicObject.InputConnectors(iindex).AttachedConnector.AttachedFrom, VesselObject.GraphicObject)
        If oindex > -1 Then VesselObject.FlowSheet.DisconnectObjects(VesselObject.GraphicObject, VesselObject.GraphicObject.OutputConnectors(oindex).AttachedConnector.AttachedTo)

    End Sub

    Private Sub btnDisconnectEnergy_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy.Click

        If cbEnergy.SelectedItem IsNot Nothing Then
            VesselObject.FlowSheet.DisconnectObjects(VesselObject.GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom, VesselObject.GraphicObject)
            cbEnergy.SelectedItem = Nothing
        End If

    End Sub

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy.Text


            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = VesselObject.GraphicObject
                Dim flowsheet = VesselObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

            End If

        End If

    End Sub


    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            VesselObject.PropertyPackage = VesselObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            VesselObject.PreferredFlashAlgorithm = cbFlashAlg.SelectedIndex
            RequestCalc()
        End If
    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then VesselObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then VesselObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Sub RequestCalc()

        VesselObject.FlowSheet.RequestCalculation(VesselObject)

    End Sub

    Private Sub chkOverrideT_CheckedChanged(sender As Object, e As EventArgs) Handles chkOverrideT.CheckedChanged
        tbTemperature.Enabled = chkOverrideT.Checked
        VesselObject.OverrideT = chkOverrideT.Checked
        If Loaded Then RequestCalc()
    End Sub

    Private Sub chkOverrideP_CheckedChanged(sender As Object, e As EventArgs) Handles chkOverrideP.CheckedChanged
        tbPressure.Enabled = chkOverrideP.Checked
        VesselObject.OverrideP = chkOverrideP.Checked
        If Loaded Then RequestCalc()
    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbPressure.KeyDown, tbTemperature.KeyDown
        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        If sender Is tbTemperature Then VesselObject.FlashTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbTemperature.Text)
        If sender Is tbPressure Then VesselObject.FlashPressure = su.Converter.ConvertToSI(cbPress.SelectedItem.ToString, tbPressure.Text)
      
        RequestCalc()

    End Sub

    Private Sub cb_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTemp.SelectedIndexChanged, cbPress.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbTemp Then
                    tbTemperature.Text = su.Converter.Convert(cbTemp.SelectedItem.ToString, units.temperature, Double.Parse(tbTemperature.Text)).ToString(nf)
                    cbTemp.SelectedItem = units.temperature
                    UpdateProps(tbTemperature)
                ElseIf sender Is cbPress Then
                    tbPressure.Text = su.Converter.Convert(cbPress.SelectedItem.ToString, units.pressure, Double.Parse(tbPressure.Text)).ToString(nf)
                    cbPress.SelectedItem = units.pressure
                    UpdateProps(tbPressure)
                End If

            Catch ex As Exception
                VesselObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        VesselObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Dim fa As Interfaces.Enums.FlashMethod = [Enum].Parse(VesselObject.PreferredFlashAlgorithm.GetType, cbFlashAlg.SelectedItem)

        Dim f As New Thermodynamics.FlashAlgorithmConfig() With {.Settings = VesselObject.FlowSheet.FlowsheetOptions.FlashSettings(fa),
                                                                .AvailableCompounds = VesselObject.FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToList,
                                                                 .FlashAlgo = fa}
        f.ShowDialog(Me)

        VesselObject.FlowSheet.FlowsheetOptions.FlashSettings(fa) = f.Settings

    End Sub

    Private Sub cbPressureCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPressureCalcMode.SelectedIndexChanged

        If Loaded Then

            Select Case cbPressureCalcMode.SelectedIndex
                Case 0
                    VesselObject.PressureCalculation = UnitOperations.Vessel.PressureBehavior.Minimum
                Case 1
                    VesselObject.PressureCalculation = UnitOperations.Vessel.PressureBehavior.Average
                Case 2
                    VesselObject.PressureCalculation = UnitOperations.Vessel.PressureBehavior.Maximum
            End Select

            RequestCalc()

        End If

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then VesselObject.GraphicObject.Tag = lblTag.Text
    End Sub

End Class