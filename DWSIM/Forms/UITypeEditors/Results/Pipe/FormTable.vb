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

        Dim cv As New DWSIM.SistemasDeUnidades.Conversor
        Dim su As DWSIM.SistemasDeUnidades.Unidades = m_form.Options.SelectedUnitSystem

        Me.Text = m_form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & DWSIM.App.GetLocalString("VisualizarResultados3")

        Me.DataGridView1.Rows.Clear()

        With Me.DataGridView1.Columns
            .Item(0).HeaderText = Replace(.Item(0).HeaderText, "(m)", "(" & su.distance & ")")
            .Item(1).HeaderText = Replace(.Item(1).HeaderText, "(kPa)", "(" & su.spmp_pressure & ")")
            .Item(2).HeaderText = Replace(.Item(2).HeaderText, "(C)", "(" & su.spmp_temperature & ")")
            .Item(3).HeaderText = Replace(.Item(3).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(4).HeaderText = Replace(.Item(4).HeaderText, "(m/s)", "(" & su.velocity & ")")
            .Item(5).HeaderText = Replace(.Item(5).HeaderText, "(kW)", "(" & su.spmp_heatflow & ")")
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
                If ps.Tipo = "Tubulaosimples" Then
                    For Each res In ps.Resultados
                        Me.DataGridView1.Rows.Add(New Object() {Conversor.ConverterDoSI(su.distance, comp_ant), Conversor.ConverterDoSI(su.spmp_pressure, res.PressaoInicial.GetValueOrDefault), Conversor.ConverterDoSI(su.spmp_temperature, res.TemperaturaInicial.GetValueOrDefault), Conversor.ConverterDoSI(su.velocity, res.LiqVel), Conversor.ConverterDoSI(su.velocity, res.VapVel), Conversor.ConverterDoSI(su.spmp_heatflow, res.CalorTransferido), res.HoldupDeLiquido, DWSIM.App.GetLocalString(res.TipoFluxo), Conversor.ConverterDoSI(su.heat_transf_coeff, res.HTC), Conversor.ConverterDoSI(su.heat_transf_coeff, res.HTC_internal), Conversor.ConverterDoSI(su.heat_transf_coeff, res.HTC_pipewall), Conversor.ConverterDoSI(su.heat_transf_coeff, res.HTC_insulation), Conversor.ConverterDoSI(su.heat_transf_coeff, res.HTC_external)})
                        comp_ant += ps.Comprimento / ps.Incrementos
                    Next
                Else
                    For Each res In ps.Resultados
                        Me.DataGridView1.Rows.Add(New Object() {Conversor.ConverterDoSI(su.distance, comp_ant), Conversor.ConverterDoSI(su.spmp_pressure, res.PressaoInicial.GetValueOrDefault), Conversor.ConverterDoSI(su.spmp_temperature, res.TemperaturaInicial.GetValueOrDefault), Conversor.ConverterDoSI(su.velocity, res.LiqVel), Conversor.ConverterDoSI(su.velocity, res.VapVel), Conversor.ConverterDoSI(su.spmp_heatflow, res.CalorTransferido), res.HoldupDeLiquido, DWSIM.App.GetLocalString(res.TipoFluxo)})
                    Next
                End If
            Next

        End With

    End Sub

End Class
