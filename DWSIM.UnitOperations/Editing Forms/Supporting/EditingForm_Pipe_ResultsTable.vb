Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe

Public Class EditingForm_Pipe_ResultsTable

    Inherits UserControl

    Public PipeOp As UnitOperations.Pipe

    Private Sub FormTable_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim su = PipeOp.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        Me.DataGridView1.Rows.Clear()

        With Me.DataGridView1.Columns
            .Item(0).HeaderText = Replace(.Item(0).HeaderText, "(m)", "(" & su.distance & ")")
            .Item(1).HeaderText = Replace(.Item(1).HeaderText, "(kPa)", "(" & su.pressure & ")")
            .Item(2).HeaderText = Replace(.Item(2).HeaderText, "(°C)", "(" & su.temperature & ")")
            .Item(3).HeaderText = Replace(.Item(3).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(4).HeaderText = Replace(.Item(4).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(5).HeaderText = Replace(.Item(5).HeaderText, "(kW)", "(" & su.heatflow & ")")
            .Item(8).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(9).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(10).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(11).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(12).HeaderText += " (" & su.heat_transf_coeff & ")"
        End With

        With PipeOp.Profile

            Dim ps As PipeSection
            Dim res As PipeResults
            Dim comp_ant As Double = 0
            For Each ps In .Sections.Values
                If ps.TipoSegmento = "Tubulaosimples" Then
                    For Each res In ps.Resultados
                        Me.DataGridView1.Rows.Add(New Object() {cv.ConvertFromSI(su.distance, comp_ant), cv.ConvertFromSI(su.pressure, res.PressaoInicial.GetValueOrDefault), cv.ConvertFromSI(su.temperature, res.TemperaturaInicial.GetValueOrDefault), cv.ConvertFromSI(su.velocity, res.LiqVel), cv.ConvertFromSI(su.velocity, res.VapVel), cv.ConvertFromSI(su.heatflow, res.CalorTransferido), res.HoldupDeLiquido, PipeOp.FlowSheet.GetTranslatedString(res.TipoFluxo), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_internal), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_pipewall), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_insulation), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_external)})
                        comp_ant += ps.Comprimento / ps.Incrementos
                    Next
                Else
                    For Each res In ps.Resultados
                        Me.DataGridView1.Rows.Add(New Object() {cv.ConvertFromSI(su.distance, comp_ant), cv.ConvertFromSI(su.pressure, res.PressaoInicial.GetValueOrDefault), cv.ConvertFromSI(su.temperature, res.TemperaturaInicial.GetValueOrDefault), cv.ConvertFromSI(su.velocity, res.LiqVel), cv.ConvertFromSI(su.velocity, res.VapVel), cv.ConvertFromSI(su.heatflow, res.CalorTransferido), res.HoldupDeLiquido, PipeOp.FlowSheet.GetTranslatedString(res.TipoFluxo)})
                    Next
                End If
            Next

        End With

    End Sub

End Class
