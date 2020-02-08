Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary

Public Class EditingForm_SpreadsheetUO

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.ExcelUO

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

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
            cbInlet2.Items.Clear()
            cbInlet2.Items.AddRange(mslist)
            cbInlet3.Items.Clear()
            cbInlet3.Items.AddRange(mslist)
            cbInlet4.Items.Clear()
            cbInlet4.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)
            cbOutlet2.Items.Clear()
            cbOutlet2.Items.AddRange(mslist)
            cbOutlet3.Items.Clear()
            cbOutlet3.Items.AddRange(mslist)
            cbOutlet4.Items.Clear()
            cbOutlet4.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(1).IsAttached Then cbInlet2.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(2).IsAttached Then cbInlet3.SelectedItem = .GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(3).IsAttached Then cbInlet4.SelectedItem = .GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(2).IsAttached Then cbOutlet3.SelectedItem = .GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(3).IsAttached Then cbOutlet4.SelectedItem = .GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).ToArray

            cbEnergyE.Items.Clear()
            cbEnergyE.Items.AddRange(eslist)

            If .GraphicObject.InputConnectors(4).IsAttached Then cbEnergyE.SelectedItem = .GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag

            'file

            TbFileName.Text = .Filename

            'parameters
            .ReadExcelParams()

            dgvinputvars.Rows.Clear()
            For Each par In .InputParams
                Dim I As Integer = dgvinputvars.Rows.Add(New Object() {par.Value.Name, par.Value.Value.ToString(nf), par.Value.Unit})
                dgvinputvars.Rows(I).Cells(0).ToolTipText = par.Value.Annotation
            Next

            dgvoutputvars.Rows.Clear()
            For Each par In .OutputParams
                Dim I As Integer = dgvoutputvars.Rows.Add(New Object() {par.Value.Name, par.Value.Value.ToString(nf), par.Value.Unit})
                dgvoutputvars.Rows(I).Cells(0).ToolTipText = par.Value.Annotation
            Next

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

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs)
        If cbInlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs)
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
        End If
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

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

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergyE.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergyE.Text

            If text <> "" Then

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If gobj.InputConnectors(4).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(4).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, 4)

            End If

        End If

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectInlet2.Click,
                                                                                            btnCreateAndConnectInlet3.Click, btnCreateAndConnectInlet4.Click,
                                                                                            btnCreateAndConnectOutlet1.Click, btnCreateAndConnectOutlet2.Click,
                                                                                            btnCreateAndConnectOutlet3.Click, btnCreateAndConnectOutlet4.Click, btnCreateAndConnectEnergy.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        Dim iidx As Integer = -1
        Dim oidx As Integer = -1

        If sender Is btnCreateAndConnectInlet1 Then

            iidx = 0

        ElseIf sender Is btnCreateAndConnectInlet2 Then

            iidx = 1

        ElseIf sender Is btnCreateAndConnectInlet3 Then

            iidx = 2

        ElseIf sender Is btnCreateAndConnectInlet4 Then

            iidx = 3

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            oidx = 0

        ElseIf sender Is btnCreateAndConnectOutlet2 Then

            oidx = 1

        ElseIf sender Is btnCreateAndConnectOutlet3 Then

            oidx = 2

        ElseIf sender Is btnCreateAndConnectOutlet4 Then

            oidx = 3

        End If

        If iidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(iidx).Position.X - 50, sgobj.InputConnectors(iidx).Position.Y, "")

            If sgobj.InputConnectors(iidx).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(iidx).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, iidx)

        End If

        If oidx > 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(oidx).Position.X + 30, sgobj.OutputConnectors(oidx).Position.Y, "")

            If sgobj.OutputConnectors(oidx).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(oidx).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, oidx, 0)

        End If

        If sender Is btnCreateAndConnectEnergy Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(4).Position.X - 50, sgobj.InputConnectors(3).Position.Y, "")

            If sgobj.InputConnectors(4).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(4).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 4)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub btnDisconnect_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click, btnDisconnect2.Click, btnDisconnect3.Click, btnDisconnect4.Click,
                                                                              btnDisconnectOutlet1.Click, btnDisconnectOutlet2.Click, btnDisconnectOutlet3.Click, btnDisconnectOutlet4.Click,
                                                                              btnDisconnectEnergyE.Click

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
            Case "btnDisconnectOutlet1"
                If cbOutlet1.SelectedItem.ToString <> "" Then
                    oindex = 0
                    cbOutlet1.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet2"
                If cbOutlet2.SelectedItem.ToString <> "" Then
                    oindex = 1
                    cbOutlet2.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet3"
                If cbOutlet3.SelectedItem.ToString <> "" Then
                    oindex = 2
                    cbOutlet3.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet4"
                If cbOutlet4.SelectedItem.ToString <> "" Then
                    oindex = 3
                    cbOutlet4.SelectedItem = Nothing
                End If
            Case "btnDisconnectEnergyE"
                If cbEnergyE.SelectedItem.ToString <> "" Then
                    iindex = 4
                    cbEnergyE.SelectedItem = Nothing
                End If
        End Select

        If iindex > -1 Then SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(iindex).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
        If oindex > -1 Then SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(oindex).AttachedConnector.AttachedTo)

    End Sub

    Sub HandleInletConnections(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged, cbInlet2.SelectedIndexChanged, cbInlet3.SelectedIndexChanged, cbInlet4.SelectedIndexChanged
        If Loaded Then UpdateInletConnection(sender)
    End Sub

    Sub HandleOutletConnections(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged, cbOutlet2.SelectedIndexChanged, cbOutlet3.SelectedIndexChanged, cbOutlet4.SelectedIndexChanged
        If Loaded Then UpdateOutletConnection(sender)
    End Sub

    Sub UpdateInletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(cb.Name.Length - 1)) - 1

            If index >= 3 Then index += 1

            Dim gobj = SimObject.GraphicObject
            Dim flowsheet = SimObject.FlowSheet

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

            If index >= 3 Then index += 1

            Dim gobj = SimObject.GraphicObject
            Dim flowsheet = SimObject.FlowSheet

            If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Exit Sub
            End If
            If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
            flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

        End If

    End Sub

    Private Sub BtnSearch_Click(sender As Object, e As EventArgs) Handles BtnSearch.Click
        OpenFileDialog1.FileName = TbFileName.Text
        OpenFileDialog1.Filter = "Spreadsheet files|*.xlsx; *.xls; *.ods"

        OpenFileDialog1.ValidateNames = True
        OpenFileDialog1.CheckFileExists = True
        OpenFileDialog1.CheckPathExists = True

        If OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            TbFileName.Text = OpenFileDialog1.FileName
            SimObject.ParamsLoaded = False

        End If
    End Sub

    Private Sub BtnEdit_Click(sender As Object, e As EventArgs) Handles BtnEdit.Click
        If TbFileName.Text <> "" Then
            If My.Computer.FileSystem.FileExists(TbFileName.Text) Then
                If Not Thermodynamics.Calculator.IsRunningOnMono Then
                    Process.Start(TbFileName.Text)
                Else
                    Process.Start(New ProcessStartInfo("xdg-open", TbFileName.Text) With {.UseShellExecute = False})
                End If
            Else
                MessageBox.Show(SimObject.FlowSheet.GetTranslatedString("Oarquivonoexisteoufo"), SimObject.FlowSheet.GetTranslatedString("Erroaoabrirarquivo"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
            SimObject.ParamsLoaded = False
        End If
    End Sub

    Private Sub BtnNew_Click(sender As Object, e As EventArgs) Handles BtnNew.Click

        Dim FileName As String = SimObject.GraphicObject.Name

        OpenFileDialog1.Title = "New Filename"
        OpenFileDialog1.Filter = "Spreadsheet files|*.xlsx; *.xls; *.ods"
        OpenFileDialog1.ValidateNames = False
        OpenFileDialog1.CheckFileExists = False
        OpenFileDialog1.CheckPathExists = True
        OpenFileDialog1.InitialDirectory = IO.Path.GetDirectoryName(FileName)

        If OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            Dim s As String = OpenFileDialog1.FileName
            If IO.Path.GetExtension(s).ToLower = ".ods" Then
                FileCopy(My.Application.Info.DirectoryPath & IO.Path.DirectorySeparatorChar & "TemplateExcelUO.ods", s)
            ElseIf IO.Path.GetExtension(s).ToLower = ".xls" Then
                FileCopy(My.Application.Info.DirectoryPath & IO.Path.DirectorySeparatorChar & "TemplateExcelUO.xls", s)
            Else
                FileCopy(My.Application.Info.DirectoryPath & IO.Path.DirectorySeparatorChar & "TemplateExcelUO.xlsx", s)
            End If
            TbFileName.Text = s
            SimObject.ParamsLoaded = False

        End If
    End Sub

    Private Sub TbFileName_TextChanged(sender As Object, e As EventArgs) Handles TbFileName.TextChanged
        If Loaded Then SimObject.Filename = TbFileName.Text
    End Sub

    Private Sub dgvinputvars_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvinputvars.CellValueChanged
        If Loaded Then
            Dim row = dgvinputvars.Rows(e.RowIndex)
            SimObject.InputParams(row.Cells(0).Value).Value = row.Cells(1).Value
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