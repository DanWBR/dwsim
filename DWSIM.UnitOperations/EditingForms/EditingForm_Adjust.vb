Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_Adjust

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As SpecialOps.Adjust

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Public cp As EditingForm_Adjust_ControlPanel

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        cp = New EditingForm_Adjust_ControlPanel With {.myADJ = SimObject}
        cp.Dock = DockStyle.Fill
        gbControlPanel.Controls.Add(cp)

        UpdateInfo()

        ChangeDefaultFont()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            Me.Text = .GraphicObject.Tag & " (" & .GetDisplayName() & ")"

            lblTag.Text = .GraphicObject.Tag

            'connections

            Dim objlist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is ISimulationObject).Select(Function(m) m.GraphicObject.Tag).OrderBy(Function(m) m).ToArray

            cbSourceObj.Items.Clear()
            cbSourceObj.Items.AddRange(objlist)

            cbTargetObj.Items.Clear()
            cbTargetObj.Items.AddRange(objlist)

            cbRefObj.Items.Clear()
            cbRefObj.Items.AddRange(objlist)

            If .ManipulatedObjectData.ID <> "" Then
                Dim obj = .GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = .ManipulatedObjectData.ID).SingleOrDefault
                If Not obj Is Nothing Then
                    .ManipulatedObjectData.Name = obj.GraphicObject.Tag
                    cbSourceObj.SelectedItem = .ManipulatedObjectData.Name
                    cbSourceProp.SelectedItem = .FlowSheet.GetTranslatedString(.ManipulatedObjectData.PropertyName)
                End If
            End If
            If .ControlledObjectData.ID <> "" Then
                Dim obj2 = .GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = .ControlledObjectData.ID).SingleOrDefault
                If Not obj2 Is Nothing Then
                    .ControlledObjectData.Name = obj2.GraphicObject.Tag
                    cbTargetObj.SelectedItem = .ControlledObjectData.Name
                    cbTargetProp.SelectedItem = .FlowSheet.GetTranslatedString(.ControlledObjectData.PropertyName)
                    Try
                        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault
                        'parameters
                        tbSetPoint.Text = su.Converter.ConvertFromSI(obj.GetPropertyUnit(SimObject.ControlledObjectData.PropertyName, units), Double.Parse(SimObject.AdjustValue)).ToString(nf)
                    Catch ex As Exception
                    End Try
                    lblTargetVal.Text = Convert.ToDouble(obj2.GetPropertyValue(SimObject.ControlledObjectData.PropertyName, units)).ToString(nf) &
                        " (" & (Convert.ToDouble(obj2.GetPropertyValue(SimObject.ControlledObjectData.PropertyName, units)) -
                        su.Converter.ConvertFromSI(obj2.GetPropertyUnit(SimObject.ControlledObjectData.PropertyName, units), SimObject.AdjustValue)).ToString("+0.####;-0.####;0") &
                        ") " & obj2.GetPropertyUnit(SimObject.ControlledObjectData.PropertyName, units)
                End If
            End If
            If .ReferencedObjectData.ID <> "" Then
                Dim obj2 = .GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = .ReferencedObjectData.ID).SingleOrDefault
                If Not obj2 Is Nothing Then
                    .ReferencedObjectData.Name = obj2.GraphicObject.Tag
                    cbRefObj.SelectedItem = .ReferencedObjectData.Name
                    cbRefProp.SelectedItem = .FlowSheet.GetTranslatedString(.ReferencedObjectData.PropertyName)
                    If .Referenced Then
                        Try
                            Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault
                            Dim objr = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbRefObj.SelectedItem.ToString).FirstOrDefault
                            Dim punit = obj.GetPropertyUnit(SimObject.ReferencedObjectData.PropertyName, units)
                            Dim value As Double = 0.0
                            If units.GetUnitType(punit) = Enums.UnitOfMeasure.temperature Then
                                value = su.Converter.ConvertFromSI(punit & ".", Double.Parse(SimObject.AdjustValue))
                            Else
                                value = su.Converter.ConvertFromSI(punit, Double.Parse(SimObject.AdjustValue))
                            End If
                            tbSetPoint.Text = value.ToString(nf)
                            lblTargetVal.Text = Convert.ToDouble(obj.GetPropertyValue(SimObject.ControlledObjectData.PropertyName, units)).ToString(nf) &
                                    " (" & (Convert.ToDouble(obj.GetPropertyValue(SimObject.ControlledObjectData.PropertyName, units)) -
                                    Convert.ToDouble(objr.GetPropertyValue(SimObject.ReferencedObjectData.PropertyName, units)) -
                                    value).ToString("+0.####;-0.####;0") &
                                    ") " & obj.GetPropertyUnit(SimObject.ControlledObjectData.PropertyName, units)
                        Catch ex As Exception
                        End Try
                    End If
                End If
            End If

            chkUseReferenced.Checked = .Referenced

            chkSolveGlobal.Checked = SimObject.SimultaneousAdjust

            tbTolerance.Text = .Tolerance.ToString(nf)

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

        SimObject.FlowSheet.RequestCalculation2(False)

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
                DirectCast(SimObject.GraphicObject, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.AdjustGraphic).ConnectedToMv = SimObject.ManipulatedObject.GraphicObject

            End If

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
                DirectCast(SimObject.GraphicObject, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.AdjustGraphic).ConnectedToCv = SimObject.ControlledObject.GraphicObject

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

            cp.UpdateInfo()

    End Sub

    Private Sub cbTargetProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetProp.SelectedIndexChanged

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault

            If Not obj Is Nothing Then

                Dim props = obj.GetProperties(Enums.PropertyType.ALL)

                For Each p In props
                    If SimObject.FlowSheet.GetTranslatedString(p) = cbTargetProp.SelectedItem.ToString Then
                        SimObject.ControlledObjectData.PropertyName = p
                        lblTargetVal.Text = Convert.ToDouble(obj.GetPropertyValue(p, units)).ToString(nf) & " (" & (Convert.ToDouble(obj.GetPropertyValue(p, units)) - su.Converter.ConvertFromSI(obj.GetPropertyUnit(p, units), SimObject.AdjustValue)).ToString("+0.####;-0.####;0") & ") " & obj.GetPropertyUnit(p, units)
                        lblSPUnits.Text = obj.GetPropertyUnit(p, units)
                        Exit For
                    End If
                Next

            End If

            cp.UpdateInfo()

    End Sub

    Private Sub chkSolveGlobal_CheckedChanged(sender As Object, e As EventArgs) Handles chkSolveGlobal.CheckedChanged
        SimObject.SimultaneousAdjust = chkSolveGlobal.Checked
        gbControlPanel.Enabled = Not chkSolveGlobal.Checked
    End Sub

    Private Sub tbSetPoint_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSetPoint.KeyDown, tbTolerance.KeyDown

        If e.KeyCode = Keys.Enter And Loaded Then

            If SimObject.Referenced Then
                Try
                    Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault
                    Dim punit = obj.GetPropertyUnit(SimObject.ReferencedObjectData.PropertyName, units)
                    If units.GetUnitType(punit) = Enums.UnitOfMeasure.temperature Then
                        SimObject.AdjustValue = su.Converter.ConvertToSI(punit & ".", tbSetPoint.Text.ParseExpressionToDouble)
                    Else
                        SimObject.AdjustValue = su.Converter.ConvertToSI(obj.GetPropertyUnit(SimObject.ReferencedObjectData.PropertyName, units), Double.Parse(tbSetPoint.Text.ParseExpressionToDouble))
                    End If
                Catch ex As Exception
                End Try
            Else
                Try
                    Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault
                    SimObject.AdjustValue = su.Converter.ConvertToSI(obj.GetPropertyUnit(SimObject.ControlledObjectData.PropertyName, units), Double.Parse(tbSetPoint.Text.ParseExpressionToDouble))
                Catch ex As Exception
                End Try
            End If

            Try
                SimObject.Tolerance = Double.Parse(tbTolerance.Text.ParseExpressionToDouble)
            Catch ex As Exception
            End Try

            UpdateInfo()

            DirectCast(sender, TextBox).SelectAll()

        End If

        cp.UpdateInfo()

    End Sub

    Private Sub tbSetPoint_TextChanged(sender As Object, e As EventArgs) Handles tbSetPoint.TextChanged, tbTolerance.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
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

    Private Sub ChkUseReferenced_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseReferenced.CheckedChanged
        SimObject.Referenced = chkUseReferenced.Checked
    End Sub

    Private Sub cbRefObj_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbRefObj.SelectedIndexChanged

        If SimObject.FlowSheet.SimulationObjects.ContainsKey(SimObject.ReferencedObjectData.ID) Then
            With SimObject.FlowSheet.SimulationObjects(SimObject.ReferencedObjectData.ID)
                .IsAdjustAttached = False
                .AttachedAdjustId = ""
                .AdjustVarType = Enums.AdjustVarType.None
            End With
        End If

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbRefObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            With Me.SimObject.ReferencedObjectData
                .ID = obj.Name
                .Name = obj.GraphicObject.Tag
            End With

            With obj
                .IsAdjustAttached = True
                .AttachedAdjustId = SimObject.Name
                .AdjustVarType = Enums.AdjustVarType.Controlled
            End With

            Dim props = obj.GetProperties(Enums.PropertyType.ALL)

            cbRefProp.Items.Clear()
            For Each p In props
                cbRefProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
            Next

            SimObject.ReferenceObject = SimObject.FlowSheet.SimulationObjects(SimObject.ReferencedObjectData.ID)
            DirectCast(SimObject.GraphicObject, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.AdjustGraphic).ConnectedToRv = SimObject.ReferenceObject.GraphicObject

        End If

    End Sub

    Private Sub cbRefProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbRefProp.SelectedIndexChanged

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbRefObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            Dim props = obj.GetProperties(Enums.PropertyType.ALL)

            For Each p In props
                If SimObject.FlowSheet.GetTranslatedString(p) = cbRefProp.SelectedItem.ToString Then
                    SimObject.ReferencedObjectData.PropertyName = p
                    lblRefVal.Text = Convert.ToDouble(obj.GetPropertyValue(p, units)).ToString(nf) & " " & obj.GetPropertyUnit(p, units)
                    Exit For
                End If
            Next

        End If

        cp.UpdateInfo()

    End Sub

End Class