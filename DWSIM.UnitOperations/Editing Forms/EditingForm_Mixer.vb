Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects

Public Class EditingForm_Mixer

    Inherits SharedClasses.ObjectEditorForm

    Public Property MixerObject As UnitOperations.Mixer

    Public Loaded As Boolean = False

    Private Sub EF_Mixer_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        Loaded = False

        If Host.Items.Where(Function(x) x.Name.Contains(MixerObject.GraphicObject.Tag)).Count > 0 Then
            If InspReportBar Is Nothing Then
                InspReportBar = New SharedClasses.InspectorReportBar
                InspReportBar.Dock = DockStyle.Bottom
                AddHandler InspReportBar.Button1.Click, Sub()
                                                            Dim iwindow As New Inspector.Window2
                                                            iwindow.SelectedObject = MixerObject
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

        With MixerObject

            chkActive.Checked = MixerObject.GraphicObject.Active

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

            cbOutlet1.Items.AddRange(mslist)

            If Not .GetInletMaterialStream(0) Is Nothing Then cbInlet1.SelectedItem = .GetInletMaterialStream(0).GraphicObject.Tag
            If Not .GetInletMaterialStream(1) Is Nothing Then cbInlet2.SelectedItem = .GetInletMaterialStream(1).GraphicObject.Tag
            If Not .GetInletMaterialStream(2) Is Nothing Then cbInlet3.SelectedItem = .GetInletMaterialStream(2).GraphicObject.Tag
            If Not .GetInletMaterialStream(3) Is Nothing Then cbInlet4.SelectedItem = .GetInletMaterialStream(3).GraphicObject.Tag
            If Not .GetInletMaterialStream(4) Is Nothing Then cbInlet5.SelectedItem = .GetInletMaterialStream(4).GraphicObject.Tag
            If Not .GetInletMaterialStream(5) Is Nothing Then cbInlet6.SelectedItem = .GetInletMaterialStream(5).GraphicObject.Tag

            If Not .GetOutletMaterialStream(0) Is Nothing Then cbOutlet1.SelectedItem = .GetOutletMaterialStream(0).GraphicObject.Tag

            Select Case .PressureCalculation
                Case UnitOperations.Mixer.PressureBehavior.Minimum
                    cbPressureCalcMode.SelectedIndex = 0
                Case UnitOperations.Mixer.PressureBehavior.Average
                    cbPressureCalcMode.SelectedIndex = 1
                Case UnitOperations.Mixer.PressureBehavior.Maximum
                    cbPressureCalcMode.SelectedIndex = 2
            End Select

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.Add("Default")
            cbFlashAlg.Items.AddRange(flashalgos)
            If .PreferredFlashAlgorithmTag <> "" Then cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag Else cbFlashAlg.SelectedIndex = 0

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

    Sub HandleOutletConnections(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged
        If Loaded Then UpdateOutletConnection(sender)
    End Sub

    Sub UpdateInletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(cb.Name.Length - 1)) - 1

            Dim gobj = MixerObject.GraphicObject
            Dim flowsheet = MixerObject.FlowSheet

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

            Dim gobj = MixerObject.GraphicObject
            Dim flowsheet = MixerObject.FlowSheet

            If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Exit Sub
            End If
            If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
            flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

        End If

    End Sub

    Private Sub btnDisconnect_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click, btnDisconnect2.Click, btnDisconnect3.Click, btnDisconnect4.Click, btnDisconnect5.Click, btnDisconnect6.Click, btnDisconnectOutlet1.Click

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
        End Select

        If iindex > -1 Then MixerObject.FlowSheet.DisconnectObjects(MixerObject.GraphicObject.InputConnectors(iindex).AttachedConnector.AttachedFrom, MixerObject.GraphicObject)
        If oindex > -1 Then MixerObject.FlowSheet.DisconnectObjects(MixerObject.GraphicObject, MixerObject.GraphicObject.OutputConnectors(oindex).AttachedConnector.AttachedTo)

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            MixerObject.PropertyPackage = MixerObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            MixerObject.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedItem.ToString
            RequestCalc()
        End If
    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then MixerObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then MixerObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub cbPressureCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPressureCalcMode.SelectedIndexChanged

        If Loaded Then

            Select Case cbPressureCalcMode.SelectedIndex
                Case 0
                    MixerObject.PressureCalculation = UnitOperations.Mixer.PressureBehavior.Minimum
                Case 1
                    MixerObject.PressureCalculation = UnitOperations.Mixer.PressureBehavior.Average
                Case 2
                    MixerObject.PressureCalculation = UnitOperations.Mixer.PressureBehavior.Maximum
            End Select

            RequestCalc()

        End If

    End Sub

    Sub RequestCalc()

        MixerObject.FlowSheet.RequestCalculation(MixerObject)

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        MixerObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Thermodynamics.Calculator.ConfigureFlashInstance(MixerObject, cbFlashAlg.SelectedItem.ToString)

    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectInlet2.Click, btnCreateAndConnectInlet3.Click, btnCreateAndConnectInlet4.Click, btnCreateAndConnectInlet5.Click, btnCreateAndConnectInlet6.Click, btnCreateAndConnectOutlet1.Click

        Dim sgobj = MixerObject.GraphicObject
        Dim fs = MixerObject.FlowSheet

        Dim iidx As Integer = -1

        If sender Is btnCreateAndConnectInlet1 Then

            iidx = 0

        ElseIf sender Is btnCreateAndConnectInlet2 Then

            iidx = 1

        ElseIf sender Is btnCreateAndConnectInlet3 Then

            iidx = 2

        ElseIf sender Is btnCreateAndConnectInlet4 Then

            iidx = 3

        ElseIf sender Is btnCreateAndConnectInlet5 Then

            iidx = 4

        ElseIf sender Is btnCreateAndConnectInlet6 Then

            iidx = 5

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(0).Position.X + 30, sgobj.OutputConnectors(0).Position.Y, "")

            If sgobj.OutputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(0).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

        End If

        If iidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(iidx).Position.X - 50, sgobj.InputConnectors(iidx).Position.Y, "")

            If sgobj.InputConnectors(iidx).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(iidx).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, iidx)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            If Loaded Then MixerObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then MixerObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = MixerObject.GraphicObject.Tag & " (" & MixerObject.GetDisplayName() & ")"
            DirectCast(MixerObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class