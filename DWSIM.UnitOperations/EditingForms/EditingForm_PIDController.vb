Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_PIDController

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As SpecialOps.PIDController

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

            chkControllerActive.Checked = SimObject.Active

            chkReverse.Checked = SimObject.ReverseActing

            Me.Text = .GraphicObject.Tag & " (" & .GetDisplayName() & ")"

            lblTag.Text = .GraphicObject.Tag

            'connections

            Dim objlist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is ISimulationObject).Select(Function(m) m.GraphicObject.Tag).OrderBy(Function(m) m).ToArray

            cbSourceObj.Items.Clear()
            cbSourceObj.Items.AddRange(objlist)

            cbTargetObj.Items.Clear()
            cbTargetObj.Items.AddRange(objlist)

            Dim unittypes = [Enum].GetNames(.ManipulatedObjectData.UnitsType.GetType)

            cbSourceType.Items.Clear()
            cbSourceType.Items.AddRange(unittypes)

            cbTargetType.Items.Clear()
            cbTargetType.Items.AddRange(unittypes)

            If .ManipulatedObjectData.ID <> "" Then
                Dim obj = .GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = .ManipulatedObjectData.ID).SingleOrDefault
                If Not obj Is Nothing Then
                    .ManipulatedObjectData.Name = obj.GraphicObject.Tag
                    cbSourceObj.SelectedItem = .ManipulatedObjectData.Name
                    cbSourceProp.SelectedItem = .FlowSheet.GetTranslatedString(.ManipulatedObjectData.PropertyName)
                    cbSourceType.SelectedItem = [Enum].GetName(.ManipulatedObjectData.UnitsType.GetType, .ManipulatedObjectData.UnitsType)
                    Try
                        cbSourceUnits.SelectedItem = .ManipulatedObjectData.Units
                    Catch ex As Exception
                    End Try
                End If
            End If
            If .ControlledObjectData.ID <> "" Then
                Dim obj2 = .GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = .ControlledObjectData.ID).SingleOrDefault
                If Not obj2 Is Nothing Then
                    .ControlledObjectData.Name = obj2.GraphicObject.Tag
                    cbTargetObj.SelectedItem = .ControlledObjectData.Name
                    cbTargetProp.SelectedItem = .FlowSheet.GetTranslatedString(.ControlledObjectData.PropertyName)
                    cbTargetType.SelectedItem = [Enum].GetName(.ControlledObjectData.UnitsType.GetType, .ControlledObjectData.UnitsType)
                    Try
                        cbTargetUnits.SelectedItem = .ControlledObjectData.Units
                    Catch ex As Exception
                    End Try
                    Try
                        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault
                        'parameters
                        tbSetPoint.Text = su.Converter.ConvertFromSI(obj.GetPropertyUnit(SimObject.ControlledObjectData.PropertyName, units), Double.Parse(SimObject.AdjustValue)).ToString(nf)
                    Catch ex As Exception
                    End Try
                    lblTargetVal.Text = su.Converter.ConvertFromSI(.ControlledObjectData.Units, Convert.ToDouble(obj2.GetPropertyValue(SimObject.ControlledObjectData.PropertyName))).ToString(nf) &
                        " (" & (su.Converter.ConvertFromSI(.ControlledObjectData.Units, Convert.ToDouble(obj2.GetPropertyValue(SimObject.ControlledObjectData.PropertyName))) -
                        SimObject.AdjustValue).ToString("+0.####;-0.####;0") &
                        ") " & .ControlledObjectData.Units
                End If
            End If

            tbKd.Text = SimObject.Kd

            tbKp.Text = SimObject.Kp

            tbKi.Text = SimObject.Ki

            tbSetPoint.Text = SimObject.AdjustValue

            tbOffset.Text = SimObject.Offset

            tbOutputMin.Text = SimObject.OutputMin

            tbOutputMax.Text = SimObject.OutputMax

            tbWG.Text = SimObject.WindupGuard

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try


        End With

        Loaded = True

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs)
        If cbSourceObj.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbSourceObj.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs)
        If cbSourceProp.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbSourceProp.SelectedItem = Nothing
        End If
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation3(SimObject, False)

    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceObj.SelectedIndexChanged

        If SimObject.FlowSheet.SimulationObjects.ContainsKey(SimObject.ManipulatedObjectData.ID) Then
            With SimObject.FlowSheet.SimulationObjects(SimObject.ManipulatedObjectData.ID)
                .IsAdjustAttached = False
                .AttachedAdjustId = ""
                .AdjustVarType = Enums.AdjustVarType.None
            End With
        End If

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbSourceObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            With Me.SimObject.ManipulatedObjectData
                .ID = obj.Name
                .Name = obj.GraphicObject.Tag
            End With

            With obj
                .IsAdjustAttached = True
                .AttachedAdjustId = SimObject.Name
                .AdjustVarType = Enums.AdjustVarType.Manipulated
            End With

            Dim props = obj.GetProperties(Enums.PropertyType.WR)

            cbSourceProp.Items.Clear()
            For Each p In props
                cbSourceProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
            Next

            SimObject.ManipulatedObject = SimObject.FlowSheet.SimulationObjects(SimObject.ManipulatedObjectData.ID)
            DirectCast(SimObject.GraphicObject, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.PIDControllerGraphic).ConnectedToMv = SimObject.ManipulatedObject.GraphicObject

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub cbTargetObj_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetObj.SelectedIndexChanged

        If SimObject.FlowSheet.SimulationObjects.ContainsKey(SimObject.ControlledObjectData.ID) Then
            With SimObject.FlowSheet.SimulationObjects(SimObject.ControlledObjectData.ID)
                .IsAdjustAttached = False
                .AttachedAdjustId = ""
                .AdjustVarType = Enums.AdjustVarType.None
            End With
        End If

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            With Me.SimObject.ControlledObjectData
                .ID = obj.Name
                .Name = obj.GraphicObject.Tag
            End With

            With obj
                .IsAdjustAttached = True
                .AttachedAdjustId = SimObject.Name
                .AdjustVarType = Enums.AdjustVarType.Controlled
            End With

            Dim props = obj.GetProperties(Enums.PropertyType.ALL)

            cbTargetProp.Items.Clear()
            For Each p In props
                cbTargetProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
            Next

            SimObject.ControlledObject = SimObject.FlowSheet.SimulationObjects(SimObject.ControlledObjectData.ID)
            DirectCast(SimObject.GraphicObject, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.PIDControllerGraphic).ConnectedToCv = SimObject.ControlledObject.GraphicObject

        End If

    End Sub

    Private Sub cbSourceProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceProp.SelectedIndexChanged

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbSourceObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            Dim props = obj.GetProperties(Enums.PropertyType.WR)

            For Each p In props
                If SimObject.FlowSheet.GetTranslatedString(p) = cbSourceProp.SelectedItem.ToString Then
                    SimObject.ManipulatedObjectData.PropertyName = p
                    lblSourceVal.Text = Convert.ToDouble(obj.GetPropertyValue(p, units)).ToString(nf) & " " & obj.GetPropertyUnit(p, units)
                    Exit For
                End If
            Next

        End If

    End Sub

    Private Sub cbTargetProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetProp.SelectedIndexChanged

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            Dim props = obj.GetProperties(Enums.PropertyType.ALL)

            For Each p In props
                If SimObject.FlowSheet.GetTranslatedString(p) = cbTargetProp.SelectedItem.ToString Then
                    SimObject.ControlledObjectData.PropertyName = p
                    lblTargetVal.Text = Convert.ToDouble(obj.GetPropertyValue(p, units)).ToString(nf) & " (" & (Convert.ToDouble(obj.GetPropertyValue(p, units)) - su.Converter.ConvertFromSI(obj.GetPropertyUnit(p, units), SimObject.AdjustValue)).ToString("+0.####;-0.####;0") & ") " & obj.GetPropertyUnit(p, units)
                    Exit For
                End If
            Next

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

    Private Sub cbSourceUnitType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceType.SelectedIndexChanged

        If SimObject.ManipulatedObjectData IsNot Nothing Then

            SimObject.ManipulatedObjectData.UnitsType = cbSourceType.SelectedIndex

            Dim units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem.GetUnitSet(SimObject.ManipulatedObjectData.UnitsType)

            cbSourceUnits.Items.Clear()
            cbSourceUnits.Items.Add("")
            cbSourceUnits.Items.AddRange(units.ToArray)

            Try
                cbSourceUnits.SelectedItem = SimObject.ManipulatedObjectData.Units
            Catch ex As Exception
            End Try

        End If

    End Sub

    Private Sub cbSourceUnits_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceUnits.SelectedIndexChanged

        If SimObject.ManipulatedObjectData IsNot Nothing Then

            SimObject.ManipulatedObjectData.Units = cbSourceUnits.SelectedItem.ToString

            Dim SelectedObject = SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.Name = SimObject.ManipulatedObjectData.ID).FirstOrDefault

            Try
                lblSourceVal.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(SimObject.ManipulatedObjectData.Units, SelectedObject.GetPropertyValue(SimObject.ManipulatedObjectData.PropertyName))
            Catch ex As Exception
                lblSourceVal.Text = "N/A"
            End Try

        End If

    End Sub

    Private Sub cbTargetType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetType.SelectedIndexChanged

        If SimObject.ControlledObjectData IsNot Nothing Then

            SimObject.ControlledObjectData.UnitsType = cbTargetType.SelectedIndex

            Dim units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem.GetUnitSet(SimObject.ControlledObjectData.UnitsType)

            cbTargetUnits.Items.Clear()
            cbTargetUnits.Items.Add("")
            cbTargetUnits.Items.AddRange(units.ToArray)

            Try
                cbTargetUnits.SelectedItem = SimObject.ControlledObjectData.Units
            Catch ex As Exception
            End Try

        End If

    End Sub

    Private Sub cbTargetUnits_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetUnits.SelectedIndexChanged

        If SimObject.ControlledObjectData IsNot Nothing Then

            SimObject.ControlledObjectData.Units = cbTargetUnits.SelectedItem.ToString

            Dim SelectedObject = SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.Name = SimObject.ControlledObjectData.ID).FirstOrDefault

            Try
                lblTargetVal.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(SimObject.ControlledObjectData.Units, SelectedObject.GetPropertyValue(SimObject.ControlledObjectData.PropertyName))
            Catch ex As Exception
                lblTargetVal.Text = "N/A"
            End Try

        End If

    End Sub

    Private Sub tbSetPoint_TextChanged(sender As Object, e As EventArgs) Handles tbSetPoint.TextChanged, tbKd.TextChanged, tbKi.TextChanged, tbKp.TextChanged, tbOffset.TextChanged, tbOutputMin.TextChanged, tbOutputMax.TextChanged, tbWG.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub tbSetPoint_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSetPoint.KeyDown, tbKd.KeyDown, tbKi.KeyDown, tbKp.KeyDown, tbOffset.KeyDown, tbOutputMin.KeyDown, tbOutputMax.KeyDown, tbWG.KeyDown

        If e.KeyCode = Keys.Enter And Loaded Then

            Select Case sender.Name
                Case "tbSetPoint"
                    SimObject.AdjustValue = Double.Parse(tbSetPoint.Text.ParseExpressionToDouble)
                Case "tbOutputMin"
                    SimObject.OutputMin = Double.Parse(tbOutputMin.Text.ParseExpressionToDouble)
                Case "tbOutputMax"
                    SimObject.OutputMax = Double.Parse(tbOutputMax.Text.ParseExpressionToDouble)
                Case "tbOffset"
                    SimObject.Offset = Double.Parse(tbOffset.Text.ParseExpressionToDouble)
                Case "tbKp"
                    SimObject.Kp = Double.Parse(tbKp.Text.ParseExpressionToDouble)
                Case "tbKi"
                    SimObject.Ki = Double.Parse(tbKi.Text.ParseExpressionToDouble)
                Case "tbKd"
                    SimObject.Kd = Double.Parse(tbKd.Text.ParseExpressionToDouble)
                Case "tbWG"
                    SimObject.WindupGuard = Double.Parse(tbWG.Text.ParseExpressionToDouble)
            End Select

            UpdateInfo()

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Private Sub chkControllerActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkControllerActive.CheckedChanged
        SimObject.Active = chkControllerActive.Checked
    End Sub

    Private Sub chkReverse_CheckedChanged(sender As Object, e As EventArgs) Handles chkReverse.CheckedChanged
        SimObject.ReverseActing = chkReverse.Checked
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        SimObject.EstimateParameters()

        tbKd.Text = SimObject.Kd

        tbKp.Text = SimObject.Kp

        tbKi.Text = SimObject.Ki

    End Sub

End Class