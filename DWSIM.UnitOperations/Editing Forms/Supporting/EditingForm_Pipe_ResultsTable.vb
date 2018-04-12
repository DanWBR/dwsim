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
            .Item(2).HeaderText = Replace(.Item(2).HeaderText, "(kPa)", "(" & su.pressure & ")")
            .Item(3).HeaderText = Replace(.Item(3).HeaderText, "(°C)", "(" & su.temperature & ")")
            .Item(4).HeaderText = Replace(.Item(4).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(5).HeaderText = Replace(.Item(5).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(6).HeaderText = Replace(.Item(6).HeaderText, "(kW)", "(" & su.heatflow & ")")
            .Item(9).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(10).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(11).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(12).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(13).HeaderText += " (" & su.heat_transf_coeff & ")"
        End With

        Dim nf As String = PipeOp.FlowSheet.FlowsheetOptions.NumberFormat

        With PipeOp.Profile

            Dim ps As PipeSection
            Dim res As PipeResults
            Dim comp_ant As Double = 0
            Dim qi As Integer = 1
            For Each ps In .Sections.Values
                For qi = 1 To ps.Quantidade
                    If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                        For Each res In ps.Resultados
                            Me.DataGridView1.Rows.Add(New Object() {cv.ConvertFromSI(su.distance, comp_ant).ToString(nf), (Math.Atan(ps.Elevacao / (ps.Comprimento ^ 2 - ps.Elevacao ^ 2) ^ 0.5) * 180 / Math.PI).ToString(nf), cv.ConvertFromSI(su.pressure, res.PressaoInicial.GetValueOrDefault).ToString(nf), cv.ConvertFromSI(su.temperature, res.TemperaturaInicial.GetValueOrDefault).ToString(nf), cv.ConvertFromSI(su.velocity, res.LiqVel).ToString(nf), cv.ConvertFromSI(su.velocity, res.VapVel).ToString(nf), cv.ConvertFromSI(su.heatflow, res.CalorTransferido).ToString(nf), res.HoldupDeLiquido.GetValueOrDefault.ToString(nf), res.TipoFluxo, cv.ConvertFromSI(su.heat_transf_coeff, res.HTC).ToString(nf), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_internal).ToString(nf), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_pipewall).ToString(nf), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_insulation).ToString(nf), cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_external).ToString(nf)})
                        Next
                    Else
                        For Each res In ps.Resultados
                            Me.DataGridView1.Rows.Add(New Object() {cv.ConvertFromSI(su.distance, comp_ant).ToString(nf), (Math.Atan(ps.Elevacao / (ps.Comprimento ^ 2 - ps.Elevacao ^ 2) ^ 0.5) * 180 / Math.PI).ToString(nf), cv.ConvertFromSI(su.pressure, res.PressaoInicial.GetValueOrDefault).ToString(nf), cv.ConvertFromSI(su.temperature, res.TemperaturaInicial.GetValueOrDefault).ToString(nf), cv.ConvertFromSI(su.velocity, res.LiqVel).ToString(nf), cv.ConvertFromSI(su.velocity, res.VapVel).ToString(nf), cv.ConvertFromSI(su.heatflow, res.CalorTransferido).ToString(nf), res.HoldupDeLiquido.GetValueOrDefault.ToString(nf), res.TipoFluxo})
                        Next
                    End If
                    comp_ant += ps.Comprimento / ps.Incrementos
                Next
            Next

        End With

    End Sub

End Class
