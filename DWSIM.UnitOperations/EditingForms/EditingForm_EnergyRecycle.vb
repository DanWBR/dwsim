Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_EnergyRecycle

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As SpecialOps.EnergyRecycle

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

            ToolTip1.SetToolTip(chkActive, .FlowSheet.GetTranslatedString("AtivoInativo"))

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
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Erro")
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

            Dim eslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.Tag).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(eslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(eslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'parameters

            cbE.Items.Clear()
            cbE.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.heatflow).ToArray)
            cbE.SelectedItem = units.heatflow

            tbET.Text = su.Converter.ConvertFromSI(units.heatflow, .ConvergenceParameters.Energy).ToString(nf)
            tbEE.Text = su.Converter.ConvertFromSI(units.heatflow, .ConvergenceHistory.EnergyE).ToString(nf)

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

    Sub UpdateProps(sender As Object)

        If sender Is tbET Then SimObject.ConvergenceParameters.Energy = su.Converter.ConvertToSI(cbE.SelectedItem.ToString, tbET.Text.ParseExpressionToDouble)

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation2(False)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbET.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbET.KeyDown

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
                Else
                    Try
                        If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                        flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If
                UpdateInfo()
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
                Else
                    Try
                        If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End If
            UpdateInfo()
        End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then
            SimObject.GraphicObject.Active = chkActive.Checked
            SimObject.FlowSheet.UpdateInterface()
            UpdateInfo()
        End If
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

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(iidx).Position.X - 50, sgobj.InputConnectors(iidx).Position.Y, "")

            If sgobj.InputConnectors(iidx).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(iidx).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, iidx)

        End If

        If oidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.OutputConnectors(oidx).Position.X + 30, sgobj.OutputConnectors(oidx).Position.Y, "")

            If sgobj.OutputConnectors(oidx).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(oidx).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, oidx, 0)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout)
            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class