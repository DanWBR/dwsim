Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects

Public Class EditingForm_Mixer

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property MixerObject As UnitOperations.Mixer

    Public Loaded As Boolean = False

    Private Sub EF_Mixer_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        Loaded = False

        With MixerObject

            chkActive.Checked = MixerObject.GraphicObject.Active

            Me.Text = .GetDisplayName() & ": " & .GraphicObject.Tag

            lblObject.Text = .GraphicObject.Tag & " (" & .FlowSheet.GetTranslatedString(.ComponentDescription) & ")"
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

            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = [Enum].GetNames(.PreferredFlashAlgorithm.GetType)

            cbFlashAlg.Items.Clear()

            cbFlashAlg.Items.AddRange(flashalgos)

            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithm.ToString

            rtbAnnotations.Rtf = .Annotation

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
            If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.InputConnectors(0).AttachedConnector.AttachedTo)
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
        If Loaded Then MixerObject.PropertyPackage = MixerObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then MixerObject.PreferredFlashAlgorithm = cbFlashAlg.SelectedIndex
    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then MixerObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then MixerObject.GraphicObject.Active = chkActive.Checked
    End Sub
End Class