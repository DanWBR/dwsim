Imports DWSIM.ExtensionMethods
Imports System.Linq

Public Class FormGHGEmissionsSummary

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Dim units As SystemsOfUnits.Units
    Dim nf As String

    Private Sub FormGHGEmissionsSummary_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Public Sub UpdateInfo()

        nf = Flowsheet.FlowsheetOptions.NumberFormat
        units = Flowsheet.FlowsheetOptions.SelectedUnitSystem

        lblEmissionUnits.Text = units.massflow
        lblEmissionMolarUnits.Text = units.molarflow

        tbTotalEmission.Text = Flowsheet.Results.GHGEmissionsSummary.TotalGHGMassEmission.ConvertFromSI(units.massflow).ToString(nf)
        tbTotalEmissionMolar.Text = Flowsheet.Results.GHGEmissionsSummary.TotalGHGMolarEmission.ConvertFromSI(units.molarflow).ToString(nf)

        tbTotalEmissionCO2eq.Text = Flowsheet.Results.GHGEmissionsSummary.TotalCO2eqMassEmission.ConvertFromSI(units.massflow).ToString(nf)
        tbTotalEmissionCO2eqMolar.Text = Flowsheet.Results.GHGEmissionsSummary.TotalCO2eqMolarEmission.ConvertFromSI(units.molarflow).ToString(nf)

        gridEmissions.Rows.Clear()
        For Each obj In Flowsheet.SimulationObjects.Values.OrderBy(Function(o) o.GraphicObject.Tag)
            If obj.GHGEmissionData.Active Then
                Dim data = obj.GHGEmissionData
                Dim refp As Double
                If data.UsesUserDefinedEnergyConsumption Then
                    refp = data.UserDefinedEnergyConsumption.ConvertFromSI(units.heatflow)
                Else
                    refp = obj.GetEnergyConsumption().ConvertFromSI(units.heatflow)
                End If
                gridEmissions.Rows.Add(New Object() {obj.GraphicObject.Tag, obj.GetDisplayName(),
                                       refp.ToString(nf), data.GHGEmissionFactor.ConvertFromSI(units.emission_factor).ToString(nf),
                                       data.GHGEmissionMassFlow.ConvertFromSI(units.massflow).ToString(nf),
                                       data.CO2eqEmissionMassFlow.ConvertFromSI(units.massflow).ToString(nf),
                                       data.GHGEmissionMolarFlow.ConvertFromSI(units.molarflow).ToString(nf),
                                       data.CO2eqEmissionMolarFlow.ConvertFromSI(units.molarflow).ToString(nf)})
            End If
        Next

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        UpdateInfo()

    End Sub

End Class