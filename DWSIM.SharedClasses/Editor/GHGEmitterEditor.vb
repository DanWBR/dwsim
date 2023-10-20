Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class GHGEmitterEditor

    Public SimObject As ISimulationObject

    Public Loaded As Boolean = False

    Dim units As SystemsOfUnits.Units
    Dim nf As String

    Private Sub GHGEmitterEditor_Load(sender As Object, e As EventArgs) Handles Me.Load

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Dim fs = SimObject.GetFlowsheet()

        Loaded = False

        cbGasComposition.Items.Clear()
        Dim comps = fs.GHGEmissionCompositions.Values.Select(Function(gc) gc.Name).ToArray()
        cbGasComposition.Items.AddRange(comps)

        If TypeOf SimObject Is IMaterialStream Then PanelUOParameters.Visible = False

        With SimObject.GHGEmissionData

            lblEFUnits.Text = units.emission_factor
            lbECUnits.Text = units.heatflow

            chkEmissionActive.Checked = .Active
            chkCO2eq.Checked = .EmissionFactorIsInCO2eq
            chkOverridePower.Checked = .UsesUserDefinedEnergyConsumption
            tbEmissionFactor.Text = cv.ConvertFromSI(units.emission_factor, .GHGEmissionFactor).ToString(nf)
            tbOverridePower.Text = cv.ConvertFromSI(units.heatflow, .UserDefinedEnergyConsumption).ToString(nf)

            If fs.GHGEmissionCompositions.ContainsKey(.GHGEmissionCompositionID) Then
                cbGasComposition.SelectedItem = fs.GHGEmissionCompositions(.GHGEmissionCompositionID).Name
            End If

        End With

        Loaded = True

    End Sub

    Private Sub chkOverridePower_CheckedChanged(sender As Object, e As EventArgs) Handles chkOverridePower.CheckedChanged
        tbOverridePower.Enabled = chkOverridePower.Checked
        If Loaded Then
            SimObject.GHGEmissionData.UsesUserDefinedEnergyConsumption = chkOverridePower.Checked
        End If
    End Sub

    Private Sub cbGasComposition_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbGasComposition.SelectedIndexChanged
        If Loaded Then
            SimObject.GHGEmissionData.GHGEmissionCompositionID =
                SimObject.GetFlowsheet().GHGEmissionCompositions.Values.Where(Function(gc) gc.Name =
                cbGasComposition.SelectedItem.ToString()).FirstOrDefault().ID
        End If
    End Sub

    Private Sub chkCO2eq_CheckedChanged(sender As Object, e As EventArgs) Handles chkCO2eq.CheckedChanged
        If Loaded Then
            SimObject.GHGEmissionData.EmissionFactorIsInCO2eq = chkCO2eq.Checked
        End If
    End Sub

    Private Sub tbEmissionFactor_TextChanged(sender As Object, e As EventArgs) Handles tbEmissionFactor.TextChanged
        Dim val = tbEmissionFactor.Text
        If Double.TryParse(val, New Double()) Then
            SimObject.GHGEmissionData.GHGEmissionFactor = cv.ConvertToSI(units.emission_factor, Double.Parse(val))
            tbEmissionFactor.ForeColor = Drawing.Color.Blue
        Else
            tbEmissionFactor.ForeColor = Drawing.Color.Red
        End If
    End Sub

    Private Sub tbOverridePower_TextChanged(sender As Object, e As EventArgs) Handles tbOverridePower.TextChanged
        Dim val = tbOverridePower.Text
        If Double.TryParse(val, New Double()) Then
            SimObject.GHGEmissionData.UserDefinedEnergyConsumption = cv.ConvertToSI(units.heatflow, Double.Parse(val))
            tbOverridePower.ForeColor = Drawing.Color.Blue
        Else
            tbOverridePower.ForeColor = Drawing.Color.Red
        End If
    End Sub

    Private Sub chkEmissionActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkEmissionActive.CheckedChanged
        If Loaded Then
            SimObject.GHGEmissionData.Active = chkEmissionActive.Checked
        End If
    End Sub
End Class
