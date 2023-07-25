Public Class EditingForm_Input

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.Input

    Public Loaded As Boolean = False

    Friend tab1, tab2 As Integer

    Private Sub EditingForm_DigitalGauge_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

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

            Dim SelectedObject = SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.Name = SimObject.SelectedObjectID).FirstOrDefault

            If SelectedObject IsNot Nothing Then
                If objlist.Contains(SelectedObject.GraphicObject.Tag) Then
                    cbSourceObj.SelectedItem = SelectedObject.GraphicObject.Tag
                    cbSourceProp.SelectedItem = .FlowSheet.GetTranslatedString(SimObject.SelectedProperty)
                Else
                    SimObject.SelectedObjectID = ""
                End If
            End If

            Dim unittypes = [Enum].GetNames(SimObject.SelectedPropertyType.GetType)

            cbSourceUnitType.Items.Clear()
            cbSourceUnitType.Items.AddRange(unittypes)

            cbSourceUnitType.SelectedItem = [Enum].GetName(SimObject.SelectedPropertyType.GetType, SimObject.SelectedPropertyType)

        End With

        Loaded = True

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceObj.SelectedIndexChanged

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbSourceObj.SelectedItem.ToString).FirstOrDefault

        If obj IsNot Nothing Then

            SimObject.SelectedObjectID = obj.Name

            Dim props = obj.GetProperties(Enums.PropertyType.ALL)

            cbSourceProp.Items.Clear()
            For Each p In props
                cbSourceProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
            Next

        End If

    End Sub

    Private Sub cbSourceProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceProp.SelectedIndexChanged

        Dim SelectedObject = SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.Name = SimObject.SelectedObjectID).FirstOrDefault

        If Not SelectedObject Is Nothing Then

            Dim props = SelectedObject.GetProperties(Enums.PropertyType.WR)

            For Each p In props
                If SimObject.FlowSheet.GetTranslatedString(p) = cbSourceProp.SelectedItem.ToString Then
                    SimObject.SelectedProperty = p
                    lblSourceVal.Text = SelectedObject.GetPropertyValue(p)
                    Exit For
                End If
            Next


        End If

    End Sub

    Private Sub cbSourceUnitType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceUnitType.SelectedIndexChanged

        SimObject.SelectedPropertyType = cbSourceUnitType.SelectedIndex

        Dim units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem.GetUnitSet(SimObject.SelectedPropertyType)

        cbSourceUnits.Items.Clear()
        cbSourceUnits.Items.Add("")
        cbSourceUnits.Items.AddRange(units.ToArray)

        Try
            cbSourceUnits.SelectedItem = SimObject.SelectedPropertyUnits
        Catch ex As Exception
        End Try

    End Sub

    Private Sub cbSourceUnits_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceUnits.SelectedIndexChanged

        SimObject.SelectedPropertyUnits = cbSourceUnits.SelectedItem.ToString

        Dim SelectedObject = SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.Name = SimObject.SelectedObjectID).FirstOrDefault

        Try
            lblSourceVal.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(SimObject.SelectedPropertyUnits, SelectedObject.GetPropertyValue(SimObject.SelectedProperty))
        Catch ex As Exception
            lblSourceVal.Text = "N/A"
        End Try

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