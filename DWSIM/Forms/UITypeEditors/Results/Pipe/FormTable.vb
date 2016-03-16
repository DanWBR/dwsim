Public Class FormTable
    Inherits System.Windows.Forms.Form

    Protected m_results As PipeProfile
    Public m_form As FormFlowsheet

    Public Property Profile() As PipeProfile
        Get
            Return m_results
        End Get
        Set(ByVal value As PipeProfile)
            m_results = value
        End Set
    End Property

    Private Sub FormTable_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim cv As New DWSIM.SystemsOfUnits.Converter
        Dim su As DWSIM.SystemsOfUnits.Units = m_form.Options.SelectedUnitSystem

        Me.Text = m_form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & DWSIM.App.GetLocalString("VisualizarResultados3")

        Me.DataGridView1.Rows.Clear()

        With Me.DataGridView1.Columns
            .Item(0).HeaderText = Replace(.Item(0).HeaderText, "(m)", "(" & su.distance & ")")
            .Item(1).HeaderText = Replace(.Item(1).HeaderText, "(kPa)", "(" & su.pressure & ")")
            .Item(2).HeaderText = Replace(.Item(2).HeaderText, "(C)", "(" & su.temperature & ")")
            .Item(3).HeaderText = Replace(.Item(3).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(4).HeaderText = Replace(.Item(4).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(5).HeaderText = Replace(.Item(5).HeaderText, "(kW)", "(" & su.heatflow & ")")
            .Item(8).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(9).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(10).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(11).HeaderText += " (" & su.heat_transf_coeff & ")"
            .Item(12).HeaderText += " (" & su.heat_transf_coeff & ")"
        End With

        With Me.Profile

            Dim ps As PipeSection
            Dim res As PipeResults
            Dim comp_ant As Double = 0
            For Each ps In .Sections.Values
                If ps.ObjectType = "Tubulaosimples" Then
                    For Each res In ps.Resultados
                        Me.DataGridView1.Rows.Add(New Object() {Converter.ConvertFromSI(su.distance, comp_ant), Converter.ConvertFromSI(su.pressure, res.PressaoInicial.GetValueOrDefault), Converter.ConvertFromSI(su.temperature, res.TemperaturaInicial.GetValueOrDefault), Converter.ConvertFromSI(su.velocity, res.LiqVel), Converter.ConvertFromSI(su.velocity, res.VapVel), Converter.ConvertFromSI(su.heatflow, res.CalorTransferido), res.HoldupDeLiquido, DWSIM.App.GetLocalString(res.TipoFluxo), Converter.ConvertFromSI(su.heat_transf_coeff, res.HTC), Converter.ConvertFromSI(su.heat_transf_coeff, res.HTC_internal), Converter.ConvertFromSI(su.heat_transf_coeff, res.HTC_pipewall), Converter.ConvertFromSI(su.heat_transf_coeff, res.HTC_insulation), Converter.ConvertFromSI(su.heat_transf_coeff, res.HTC_external)})
                        comp_ant += ps.Comprimento / ps.Incrementos
                    Next
                Else
                    For Each res In ps.Resultados
                        Me.DataGridView1.Rows.Add(New Object() {Converter.ConvertFromSI(su.distance, comp_ant), Converter.ConvertFromSI(su.pressure, res.PressaoInicial.GetValueOrDefault), Converter.ConvertFromSI(su.temperature, res.TemperaturaInicial.GetValueOrDefault), Converter.ConvertFromSI(su.velocity, res.LiqVel), Converter.ConvertFromSI(su.velocity, res.VapVel), Converter.ConvertFromSI(su.heatflow, res.CalorTransferido), res.HoldupDeLiquido, DWSIM.App.GetLocalString(res.TipoFluxo)})
                    Next
                End If
            Next

        End With

    End Sub

End Class
