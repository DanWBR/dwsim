Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe
Imports System.Drawing

Public Class EditingForm_Pipe_ResultsChart

    Inherits UserControl

    Protected m_vx As Double()
    Protected m_vy As Double()
    Protected m_ytitle As String
    Protected m_xtitle As String

    Public PipeOp As UnitOperations.Pipe

    Private Sub FormGraph_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        Me.ComboBox1.SelectedIndex = 0
        Me.ComboBox2.SelectedIndex = 1

    End Sub

    Sub GetXY(ByVal EixoX As Integer, ByVal EixoY As Integer)

        Dim su = PipeOp.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        Dim count As Integer = 0
        Dim ps0 As PipeSection
        For Each ps0 In PipeOp.Profile.Sections.Values
            count += ps0.Results.Count
        Next
        count = count - 1

        Dim vx(count), vy(count) As Double

        Dim n As Integer = 0

        Select Case EixoX
            Case 0
                Me.m_xtitle = Replace(PipeOp.FlowSheet.GetTranslatedString("Comprimentom"), "(m)", "(" & su.distance & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.distance, comp_ant)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.distance, comp_ant)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 1
                Me.m_xtitle = Replace(PipeOp.FlowSheet.GetTranslatedString("PressoPa"), "(Pa)", "(" & su.pressure & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.pressure, res.Pressure_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.pressure, res.Pressure_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 2
                Me.m_xtitle = Replace(PipeOp.FlowSheet.GetTranslatedString("TemperaturaK"), "(K)", "(" & su.temperature & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.temperature, res.Temperature_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.temperature, res.Temperature_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 3
                Me.m_xtitle = Replace(PipeOp.FlowSheet.GetTranslatedString("VelocidadedoLquidoms"), "(m/s)", "(" & su.velocity & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.velocity, res.LiqVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.velocity, res.LiqVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 4
                Me.m_xtitle = Replace(PipeOp.FlowSheet.GetTranslatedString("VelocidadedoVaporms"), "(m/s)", "(" & su.velocity & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.velocity, res.VapVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.velocity, res.VapVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 5
                Me.m_xtitle = Replace(PipeOp.FlowSheet.GetTranslatedString("FluxodeCalorkW"), "(kW)", "(" & su.heatflow & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.heatflow, res.HeatTransferred)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vx(i) = cv.ConvertFromSI(su.heatflow, res.HeatTransferred)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 5
                Me.m_xtitle = PipeOp.FlowSheet.GetTranslatedString("Mach Number")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vx(i) = res.MachNumber
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vx(i) = res.MachNumber
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
        End Select

        Select Case EixoY
            'CoefdeTransfdeCalor
            Case 7
                Me.m_ytitle = PipeOp.FlowSheet.GetTranslatedString("CoefdeTransfdeCalor") & " (" & su.heat_transf_coeff & ")"
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            For Each res In ps.Results
                                vy(i) = cv.ConvertFromSI(su.heat_transf_coeff, res.HTC)
                                i += 1
                            Next
                        Next
                    Next
                End With
            Case 6
                Me.m_ytitle = PipeOp.FlowSheet.GetTranslatedString("FraodeLquido")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = res.LiquidHoldup
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = res.LiquidHoldup
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 0
                Me.m_ytitle = Replace(PipeOp.FlowSheet.GetTranslatedString("Comprimentom"), "(m)", "(" & su.distance & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.distance, comp_ant)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.distance, comp_ant)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 1
                Me.m_ytitle = Replace(PipeOp.FlowSheet.GetTranslatedString("PressoPa"), "(Pa)", "(" & su.pressure & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.pressure, res.Pressure_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.pressure, res.Pressure_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 2
                Me.m_ytitle = Replace(PipeOp.FlowSheet.GetTranslatedString("TemperaturaK"), "(K)", "(" & su.temperature & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.temperature, res.Temperature_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.temperature, res.Temperature_Initial)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 3
                Me.m_ytitle = Replace(PipeOp.FlowSheet.GetTranslatedString("VelocidadedoLquidoms"), "(m/s)", "(" & su.velocity & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.velocity, res.LiqVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.velocity, res.LiqVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 4
                Me.m_ytitle = Replace(PipeOp.FlowSheet.GetTranslatedString("VelocidadedoVaporms"), "(m/s)", "(" & su.velocity & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.velocity, res.VapVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.velocity, res.VapVel)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 5
                Me.m_ytitle = Replace(PipeOp.FlowSheet.GetTranslatedString("FluxodeCalorkW"), "(kW)", "(" & su.heatflow & ")")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.heatflow, res.HeatTransferred)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.heatflow, res.HeatTransferred)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 8
                Me.m_ytitle = "Text (" & su.temperature & ")"
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.temperature, res.External_Temperature)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = cv.ConvertFromSI(su.temperature, res.External_Temperature)
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
                    Next
                End With
            Case 9
                Me.m_ytitle = PipeOp.FlowSheet.GetTranslatedString("Mach Number")
                With PipeOp.Profile
                    Dim ps As PipeSection
                    Dim res As PipeResults
                    Dim comp_ant As Double = 0
                    Dim i As Integer = 0
                    Dim qi As Integer = 1
                    For Each ps In .Sections.Values
                        For qi = 1 To 1
                            If ps.TipoSegmento = "Tubulaosimples" Or ps.TipoSegmento = "Straight Tube Section" Then
                                For Each res In ps.Results
                                    vy(i) = res.MachNumber
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            Else
                                For Each res In ps.Results
                                    vy(i) = res.MachNumber
                                    comp_ant += ps.Comprimento / ps.Incrementos
                                    i += 1
                                Next
                            End If
                        Next
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
                With .AddCurve(Me.ComboBox2.SelectedItem, Me.m_vx, Me.m_vy, Color.SlateBlue, ZedGraph.SymbolType.None)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Line.Width = 3
                End With
                .Title.Text = Me.ComboBox2.SelectedItem
                .Title.FontSpec.Size = 24
                .XAxis.Title.Text = Me.m_xtitle
                .XAxis.Scale.FontSpec.Size = 20
                .XAxis.Title.FontSpec.Size = 20
                .YAxis.Title.Text = Me.m_ytitle
                .YAxis.Scale.FontSpec.Size = 20
                .YAxis.Title.FontSpec.Size = 20
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
                .Line.IsVisible = True
                .Symbol.IsVisible = False
            End With
            .Title.Text = Me.ComboBox2.SelectedItem
            .Title.FontSpec.Size = 24
            .XAxis.Title.Text = Me.m_xtitle
            .XAxis.Scale.FontSpec.Size = 20
            .XAxis.Title.FontSpec.Size = 20
            .YAxis.Title.Text = Me.m_ytitle
            .YAxis.Scale.FontSpec.Size = 20
            .YAxis.Title.FontSpec.Size = 20
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
