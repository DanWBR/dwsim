Public Class FormGraph
    Inherits System.Windows.Forms.Form

    Protected m_results As PipeProfile

    Protected m_vx As Double()
    Protected m_vy As Double()
    Protected m_ytitle As String
    Protected m_xtitle As String
    Public m_form As FormFlowsheet

    Public Property Profile() As PipeProfile
        Get
            Return m_results
        End Get
        Set(ByVal value As PipeProfile)
            m_results = value
        End Set
    End Property

    Private Sub FormGraph_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        Me.ComboBox1.SelectedIndex = 0
        Me.ComboBox2.SelectedIndex = 1

        Me.Text = m_form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & DWSIM.App.GetLocalString("VisualizarResultados")

    End Sub

    Sub GetXY(ByVal EixoX As Integer, ByVal EixoY As Integer)

        Dim cv As New DWSIM.SystemsOfUnits.Converter
        Dim su As DWSIM.SystemsOfUnits.Units = m_form.Options.SelectedUnitSystem

        Dim count As Integer = 0
        Dim ps0 As PipeSection
        For Each ps0 In Me.Profile.Sections.Values
            'If ps0.ObjectType = DWSIM.App.GetLocalString("Tubulaosimples") Then
            '    count += ps0.Resultados.Count - 1
            'Else
            '    count += ps0.Resultados.Count
            'End If
            count += ps0.Resultados.Count
        Next
        count = count - 1

        Dim vx(count), vy(count) As Double

        Dim n As Integer = 0

        Select Case EixoX
            Case 0
                Me.m_xtitle = Replace(DWSIM.App.GetLocalString("Comprimentom"), "(m)", "(" & su.distance & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.distance, comp_ant)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.distance, comp_ant)
                                i += 1
                            Next
                        End If

                    Next
                End With
            Case 1
                Me.m_xtitle = Replace(DWSIM.App.GetLocalString("PressoPa"), "(Pa)", "(" & su.pressure & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.pressure, res.PressaoInicial)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.pressure, res.PressaoInicial)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 2
                Me.m_xtitle = Replace(DWSIM.App.GetLocalString("TemperaturaK"), "(K)", "(" & su.temperature & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.temperature, res.TemperaturaInicial)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.temperature, res.TemperaturaInicial)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 3
                Me.m_xtitle = Replace(DWSIM.App.GetLocalString("VelocidadedoLquidoms"), "(m/s)", "(" & su.velocity & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.velocity, res.LiqVel)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.velocity, res.LiqVel)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 4
                Me.m_xtitle = Replace(DWSIM.App.GetLocalString("VelocidadedoVaporms"), "(m/s)", "(" & su.velocity & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.velocity, res.VapVel)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.velocity, res.VapVel)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 5
                Me.m_xtitle = Replace(DWSIM.App.GetLocalString("FluxodeCalorkW"), "(kW)", "(" & su.heatflow & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.heatflow, res.CalorTransferido)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vx(i) = Converter.ConvertFromSI(su.heatflow, res.CalorTransferido)
                                i += 1
                            Next
                        End If
                    Next
                End With
        End Select

        Select Case EixoY
            'CoefdeTransfdeCalor
            Case 7
                Me.m_ytitle = DWSIM.App.GetLocalString("CoefdeTransfdeCalor") & " (" & su.heat_transf_coeff & ")"
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        For Each res In ps.Resultados
                            vy(i) = Converter.ConvertFromSI(su.heat_transf_coeff, res.HTC)
                            i += 1
                        Next
                    Next
                End With
            Case 6
                Me.m_ytitle = DWSIM.App.GetLocalString("FraodeLquido")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vy(i) = res.HoldupDeLiquido
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vy(i) = res.HoldupDeLiquido
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 0
                Me.m_ytitle = Replace(DWSIM.App.GetLocalString("Comprimentom"), "(m)", "(" & su.distance & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.distance, comp_ant)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.distance, comp_ant)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 1
                Me.m_ytitle = Replace(DWSIM.App.GetLocalString("PressoPa"), "(Pa)", "(" & su.pressure & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.pressure, res.PressaoInicial)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.pressure, res.PressaoInicial)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 2
                Me.m_ytitle = Replace(DWSIM.App.GetLocalString("TemperaturaK"), "(K)", "(" & su.temperature & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.temperature, res.TemperaturaInicial)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.temperature, res.TemperaturaInicial)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 3
                Me.m_ytitle = Replace(DWSIM.App.GetLocalString("VelocidadedoLquidoms"), "(m/s)", "(" & su.velocity & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.velocity, res.LiqVel)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.velocity, res.LiqVel)
                                i += 1
                            Next
                        End If
                    Next
                End With
            Case 4
                Me.m_ytitle = Replace(DWSIM.App.GetLocalString("VelocidadedoVaporms"), "(m/s)", "(" & su.velocity & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.velocity, res.VapVel)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                i += 1
                                vy(i) = Converter.ConvertFromSI(su.velocity, res.VapVel)
                            Next
                        End If
                    Next
                End With
            Case 5
                Me.m_ytitle = Replace(DWSIM.App.GetLocalString("FluxodeCalorkW"), "(kW)", "(" & su.heatflow & ")")
                With Me.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    For Each ps In .Sections.Values
                        If ps.Tipo = "Tubulaosimples" Then
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.heatflow, res.CalorTransferido)
                                comp_ant += ps.Comprimento / ps.Incrementos
                                i += 1
                            Next
                        Else
                            For Each res In ps.Resultados
                                vy(i) = Converter.ConvertFromSI(su.heatflow, res.CalorTransferido)
                                i += 1
                            Next
                        End If
                    Next
                End With
        End Select

        Me.m_vx = vx
        Me.m_vy = vy

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox1.SelectedIndexChanged

        If Not Me.ComboBox2.SelectedItem Is Nothing Then
            Me.GetXY(Me.ComboBox1.SelectedIndex, Me.ComboBox2.SelectedIndex)

            With Me.ZedGraphControl1.GraphPane
                .CurveList.Clear()
                With .AddCurve(Me.ComboBox2.SelectedItem, Me.m_vx, Me.m_vy, Color.SlateBlue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.Text = Me.ComboBox2.SelectedItem
                .XAxis.Title.Text = Me.m_xtitle
                .YAxis.Title.Text = Me.m_ytitle
                .Legend.IsVisible = False
                .AxisChange(Me.CreateGraphics)
            End With
            Me.ZedGraphControl1.Invalidate()
        End If

    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox2.SelectedIndexChanged

        Me.GetXY(Me.ComboBox1.SelectedIndex, Me.ComboBox2.SelectedIndex)

        With Me.ZedGraphControl1.GraphPane
            .CurveList.Clear()
            With .AddCurve(Me.ComboBox2.SelectedItem, Me.m_vx, Me.m_vy, Color.SlateBlue, ZedGraph.SymbolType.Circle)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            .Title.Text = Me.ComboBox2.SelectedItem
            .XAxis.Title.Text = Me.m_xtitle
            .YAxis.Title.Text = Me.m_ytitle
            .Legend.IsVisible = False
            .AxisChange(Me.CreateGraphics)
        End With

        Me.ZedGraphControl1.Invalidate()

    End Sub

    Private Sub ZedGraphControl1_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles ZedGraphControl1.Paint
        'Me.ZedGraphControl1.GraphPane.AxisChange(e.Graphics)
        'Me.ZedGraphControl1.Invalidate()
    End Sub
End Class
