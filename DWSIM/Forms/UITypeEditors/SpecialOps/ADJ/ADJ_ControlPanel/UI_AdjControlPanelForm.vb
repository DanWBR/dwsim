Imports DWSIM.DWSIM.SimulationObjects
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver

Public Class UI_AdjControlPanelForm

    Inherits System.Windows.Forms.Form

    Public formC As FormFlowsheet
    Public status As String = ""
    Public cancelar As Boolean = False
    Public usemaxmin As Boolean = False

    Public myADJ As SpecialOps.Adjust
    Public py1, py2 As ArrayList

    Public su As DWSIM.SystemsOfUnits.Units
    Public cv As New DWSIM.SystemsOfUnits.Converter
    Public nf As String

    Private Sub UI_AdjControlPanelForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        formC = My.Application.ActiveSimulation

        Me.su = formC.Options.SelectedUnitSystem
        Me.nf = formC.Options.NumberFormat

        myADJ = formC.Collections.CLCS_AdjustCollection(formC.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

        With myADJ
            Me.tbAjuste.Text = Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), .AdjustValue)
            Me.tbMaxIt.Text = .MaximumIterations
            Me.tbStep.Text = .StepSize
            Me.tbTol.Text = .Tolerance
            If .MaxVal.GetValueOrDefault = 0 And .MinVal.GetValueOrDefault = 0 Then
                Me.rbBrent.Enabled = False
                Me.usemaxmin = False
            Else
                Me.rbBrent.Enabled = True
                Me.usemaxmin = True
            End If
        End With

        Me.lblStatus.Text = DWSIM.App.GetLocalString("StatusOcioso")
        Me.lblItXdeY.Text = ""
        Me.tbErro.Text = ""

        With Me.GraphControl.GraphPane
            .Title.IsVisible = False
            .XAxis.Title.Text = DWSIM.App.GetLocalString("Iterao")
            .YAxis.Title.Text = DWSIM.App.GetPropertyName(myADJ.ControlledObjectData.m_Property) & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, Me.su) & ")"
        End With

        py1 = New ArrayList
        py2 = New ArrayList

        Me.Text = myADJ.GraphicObject.Tag & " - " & DWSIM.App.GetLocalString("PaineldeControle")

    End Sub

    Private Sub btnIniciar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnIniciar.Click

        Me.btnIniciar.Enabled = False
        Me.myADJ.GraphicObject.Calculated = False

        py1.Clear()
        py2.Clear()

        With Me.GraphControl.GraphPane
            .CurveList.Clear()
            .AxisChange(Me.CreateGraphics)
            Me.GraphControl.IsAutoScrollRange = True
            Me.GraphControl.Invalidate()
        End With
        With Me.GraphControl.GraphPane.Legend
            .Position = ZedGraph.LegendPos.TopCenter
            .Border.IsVisible = False
            .FontSpec.Size = 10
            .FontSpec.IsDropShadow = False
        End With


        Dim mvVal, cvVal, rfVal, maxval, minval As Double

        If formC.Collections.FlowsheetObjectCollection(myADJ.ControlledObjectData.m_ID).GraphicObject.Calculated Then
            cvVal = Me.GetCtlVarValue()
        End If
        If formC.Collections.FlowsheetObjectCollection(myADJ.ManipulatedObjectData.m_ID).GraphicObject.Calculated Then
            mvVal = Me.GetMnpVarValue()
            maxval = myADJ.MaxVal.GetValueOrDefault
            minval = myADJ.MinVal.GetValueOrDefault
        End If
        If myADJ.Referenced Then
            If formC.Collections.FlowsheetObjectCollection(myADJ.ReferencedObjectData.m_ID).GraphicObject.Calculated Then
                rfVal = Me.GetMnpVarValue()
            End If
        End If
        Dim tol, maxit, adjval, stepsize, max, min As Double
        With myADJ
            If myADJ.Referenced Then
                If Not rfVal = Nothing Then
                    adjval = rfVal + .AdjustValue
                Else
                    Me.btnIniciar.Enabled = True
                    Exit Sub
                End If
            Else
                adjval = .AdjustValue
            End If
            maxit = .MaximumIterations
            stepsize = .StepSize
            tol = .Tolerance
            If Me.usemaxmin Then
                min = .MinVal.GetValueOrDefault
                max = .MaxVal.GetValueOrDefault
            End If
        End With

        With Me.Grid1.Columns
            .Clear()
            .Add("c1", DWSIM.App.GetLocalString("Iter"))
            .Add("c2", "Var.")
            .Add("c3", DWSIM.App.GetLocalString("Erro"))
        End With
        For Each co As DataGridViewColumn In Me.Grid1.Columns
            co.SortMode = DataGridViewColumnSortMode.NotSortable
            co.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            co.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        Next

        If Me.rbSecante.Checked Then

            Dim cnt As Integer = 0
            Dim var, varAnt, varAnt2, fi, fi_ant, fi_ant2 As Double
            var = mvVal
            Do
                fi_ant2 = fi_ant
                fi_ant = fi
                fi = cvVal - adjval

                Me.lblStatus.Text = DWSIM.App.GetLocalString("Ajustando")
                Me.lblItXdeY.Text = DWSIM.App.GetLocalString("Iterao") & " " & (cnt + 1) & " " & DWSIM.App.GetLocalString("de") & " " & maxit
                Me.tbErro.Text = fi

                If cnt <= 2 Then
                    varAnt2 = varAnt
                    varAnt = var
                    If cnt > 1 And fi > fi_ant Then
                        var = var - 2 * stepsize
                    Else
                        var = var + stepsize
                    End If
                Else
                    varAnt2 = varAnt
                    varAnt = var
                    If fi <> fi_ant2 Then
                        var = var - fi * (var - varAnt2) / (fi - fi_ant2)
                    End If
                End If

                If Me.usemaxmin Then
                    If var <= min Or var >= max Then
                        Dim msgres As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Avarivelmanipuladaat") _
                                        & vbCrLf & DWSIM.App.GetLocalString("Desejacontinuaroproc"), _
                                        DWSIM.App.GetLocalString("Limitesdavarivelmani"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                        If msgres = MsgBoxResult.No Then
                            cancelar = True
                            Exit Do
                        End If
                    End If
                End If

                Me.SetMnpVarValue(var)

                DWSIM.Flowsheet.FlowsheetSolver.CalculateObject(My.Application.ActiveSimulation, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                cnt += 1

                py1.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), adjval))
                py2.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), cvVal))

                AtualizaGrafico()

                If fi = fi_ant Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Avarivelmanipuladano") _
                                    & vbCrLf & DWSIM.App.GetLocalString("Desejacontinuaroproc"), _
                                    DWSIM.App.GetLocalString("Problemasnaconvergnc"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        cancelar = True
                        Exit Do
                    End If
                End If

                If cnt >= maxit Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Onmeromximodeiteraes"), _
                                                    DWSIM.App.GetLocalString("Nmeromximodeiteraesa3"), _
                                                    MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        cancelar = False
                        Me.lblStatus.Text = DWSIM.App.GetLocalString("Mximodeiteraesatingi")
                        Me.btnIniciar.Enabled = True
                        Me.myADJ.GraphicObject.Calculated = False
                        Exit Sub
                    Else
                        cnt = 1
                    End If
                End If
                Application.DoEvents()
                If cancelar = True Then Exit Do
            Loop Until Math.Abs(fi) < tol Or Double.IsNaN(var)

            If cancelar = True Then
                Me.lblStatus.Text = DWSIM.App.GetLocalString("Ajustecanceladopelou")
                Me.myADJ.GraphicObject.Calculated = False
            Else
                Me.lblStatus.Text = DWSIM.App.GetLocalString("Valorajustadocomsuce")
                Me.myADJ.GraphicObject.Calculated = True
            End If

        ElseIf Me.rbBrent.Checked Then

            minval = myADJ.MinVal.GetValueOrDefault
            maxval = myADJ.MaxVal.GetValueOrDefault

            Dim l As Integer = 0
            Dim i As Integer = 0

            Dim f, f_inf, nsub, delta As Double

            nsub = 5

            delta = (maxval - minval) / nsub

            Do
                Me.SetMnpVarValue(minval)

                DWSIM.Flowsheet.FlowsheetSolver.CalculateObject(My.Application.ActiveSimulation, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                f = cvVal - adjval
                Me.lblStatus.Text = DWSIM.App.GetLocalString("Ajustando")
                Me.lblItXdeY.Text = DWSIM.App.GetLocalString("Procurandosubinterva")
                Me.tbErro.Text = f
                py1.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), adjval))
                py2.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), cvVal))
                AtualizaGrafico()
                minval = minval + delta
                Me.SetMnpVarValue(minval)

                DWSIM.Flowsheet.FlowsheetSolver.CalculateObject(My.Application.ActiveSimulation, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                f_inf = cvVal - adjval
                Me.lblStatus.Text = DWSIM.App.GetLocalString("Ajustando")
                Me.lblItXdeY.Text = DWSIM.App.GetLocalString("Procurandosubinterva")
                Me.tbErro.Text = f_inf
                py1.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), adjval))
                py2.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), cvVal))
                AtualizaGrafico()
                l += 1
                If l > 5 Then
                    MessageBox.Show(DWSIM.App.GetLocalString("Oajustenoencontrouum"), DWSIM.App.GetLocalString("Semsoluo"), MessageBoxButtons.OK, MessageBoxIcon.Information)
                    Me.lblStatus.Text = DWSIM.App.GetLocalString("Noexistesoluonointer")
                    Me.lblItXdeY.Text = ""
                    cancelar = False
                    Me.btnIniciar.Enabled = True
                    Me.myADJ.GraphicObject.Calculated = False
                    Exit Sub
                End If
                If f = f_inf Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Avarivelmanipuladano") _
                                    & vbCrLf & DWSIM.App.GetLocalString("Desejacontinuaroproc"), _
                                    DWSIM.App.GetLocalString("Problemasnaconvergnc"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        Me.lblStatus.Text = DWSIM.App.GetLocalString("Ajustecanceladopelou")
                        Me.btnIniciar.Enabled = True
                        Me.myADJ.GraphicObject.Calculated = False
                        Exit Sub
                    End If
                End If
            Loop Until f * f_inf < 0
            maxval = minval
            minval = minval - delta

            'método de Brent
            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = maxit
            Dim iter2 As Integer
            aaa = minval
            bbb = maxval
            ccc = maxval
            faa = f
            fbb = f_inf
            fcc = fbb
            iter2 = 0
            Do
                Me.lblStatus.Text = DWSIM.App.GetLocalString("Ajustando")
                Me.lblItXdeY.Text = DWSIM.App.GetLocalString("Iterao") & " " & (iter2 + l + 1) & " " & DWSIM.App.GetLocalString("de") & " " & maxit
                Me.tbErro.Text = fbb
                Application.DoEvents()

                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = tol
                xmm = 0.5 * (ccc - bbb)
                If Math.Abs(fbb) < tol Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                Me.SetMnpVarValue(bbb)

                DWSIM.Flowsheet.FlowsheetSolver.CalculateObject(My.Application.ActiveSimulation, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                fbb = cvVal - adjval
                Me.tbErro.Text = fbb
                iter2 += 1

                py1.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), adjval))
                py2.Add(Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, su), cvVal))

                AtualizaGrafico()

                If iter2 + l - 1 >= maxit Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Onmeromximodeiteraes"), _
                                                    DWSIM.App.GetLocalString("Nmeromximodeiteraesa3"), _
                                                    MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        cancelar = False
                        Me.lblStatus.Text = DWSIM.App.GetLocalString("Mximodeiteraesatingi")
                        Me.btnIniciar.Enabled = True
                        Me.myADJ.GraphicObject.Calculated = False
                        Exit Sub
                    Else
                        iter2 = 1
                    End If
                End If
                If cancelar = True Then Exit Do
            Loop Until iter2 >= ITMAX2

Final3:
            If cancelar = True Then
                Me.lblStatus.Text = DWSIM.App.GetLocalString("Ajustecanceladopelou")
                Me.myADJ.GraphicObject.Calculated = False
            Else
                Me.lblStatus.Text = DWSIM.App.GetLocalString("Valorajustadocomsuce")
                Me.myADJ.GraphicObject.Calculated = True
            End If

        End If

        Me.btnIniciar.Enabled = True

        cancelar = False

        Dim j, k As Integer
        Dim data(2, py1.Count - 1) As String
        j = 0
        For Each d As Double In py1
            data(0, j) = j
            data(1, j) = py2(j)
            data(2, j) = -py1(j) + py2(j)
            j = j + 1
        Next
        With Me.Grid1.Rows
            .Clear()
            If data.Length > 0 Then
                k = 0
                Do
                    .Add()
                    j = 0
                    Do
                        If Double.TryParse(data(j, k), New Double) Then
                            .Item(k).Cells(j).Value = Format(Convert.ToDouble(data(j, k)), nf)
                        Else
                            .Item(k).Cells(j).Value = data(j, k)
                        End If
                        j = j + 1
                    Loop Until j = 3
                    k = k + 1
                Loop Until k = py1.Count
            End If
        End With

    End Sub

    'Private Function SetCtlVarValue(ByVal val As Nullable(Of Double))

    '    With Me.myADJ.ControlledObjectData
    '        Select Case .m_Type
    '            Case DWSIM.App.GetLocalString("CorrentedeMatria")
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("Temperatura")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.m_ID).Phases(0).Properties.temperature = val
    '                    Case DWSIM.App.GetLocalString("Presso")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.m_ID).Phases(0).Properties.pressure = val
    '                    Case DWSIM.App.GetLocalString("Vazomssica")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.m_ID).Phases(0).Properties.massflow = val
    '                    Case DWSIM.App.GetLocalString("Vazovolumtrica")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.m_ID).Phases(0).Properties.volumetric_flow = val
    '                    Case DWSIM.App.GetLocalString("Vazomolar")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.m_ID).Phases(0).Properties.molarflow = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case "Corrente de EnergyFlow"
    '                Return Me.formC.Collections.CLCS_EnergyStreamCollection(.m_ID).EnergyFlow.GetValueOrDefault
    '            Case DWSIM.App.GetLocalString("Misturadores")
    '                Return Nothing
    '            Case DWSIM.App.GetLocalString("MisturadoresMatEn")
    '                Return Nothing
    '            Case DWSIM.App.GetLocalString("Divisores")
    '                Return Nothing
    '            Case DWSIM.App.GetLocalString("Tubulaes")
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("DeltaP")
    '                        Me.formC.Collections.CLCS_PipeCollection(.m_ID).DeltaP = val
    '                    Case DWSIM.App.GetLocalString("DeltaT")
    '                        Me.formC.Collections.CLCS_PipeCollection(.m_ID).DeltaT = val
    '                    Case DWSIM.App.GetLocalString("Calortrocado")
    '                        Me.formC.Collections.CLCS_PipeCollection(.m_ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case DWSIM.App.GetLocalString("Vlvulas")
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("DeltaT")
    '                        Me.formC.Collections.CLCS_ValveCollection(.m_ID).DeltaT = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case DWSIM.App.GetLocalString("Bombas")
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("DeltaT")
    '                        Me.formC.Collections.CLCS_PumpCollection(.m_ID).DeltaT = val
    '                    Case DWSIM.App.GetLocalString("Potnciarequerida")
    '                        Me.formC.Collections.CLCS_PumpCollection(.m_ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case DWSIM.App.GetLocalString("Tanques")
    '                Return Nothing
    '            Case DWSIM.App.GetLocalString("Separadores")
    '                Return Nothing
    '            Case DWSIM.App.GetLocalString("Compressores"))
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("DeltaT")
    '                        Me.formC.Collections.CLCS_CompressorCollection(.m_ID).DeltaT = val
    '                    Case DWSIM.App.GetLocalString("Potnciarequerida")
    '                        Me.formC.Collections.CLCS_CompressorCollection(.m_ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case DWSIM.App.GetLocalString("Turbinas")
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("DeltaT")
    '                        Me.formC.Collections.CLCS_TurbineCollection(.m_ID).DeltaT = val
    '                    Case DWSIM.App.GetLocalString("Potnciagerada")
    '                        Me.formC.Collections.CLCS_TurbineCollection(.m_ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case DWSIM.App.GetLocalString("Aquecedores")
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("DeltaT")
    '                        Me.formC.Collections.CLCS_HeaterCollection(.m_ID).DeltaT = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case DWSIM.App.GetLocalString("Resfriadores")
    '                Select Case .m_Property
    '                    Case DWSIM.App.GetLocalString("DeltaT")
    '                        Me.formC.Collections.CLCS_CoolerCollection(.m_ID).DeltaT = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '        End Select
    '    End With

    '    Return 1

    'End Function

    Private Function GetCtlVarValue()

        With Me.myADJ.ControlledObjectData
            Return formC.Collections.FlowsheetObjectCollection(.m_ID).GetPropertyValue(.m_Property)
        End With

    End Function

    Private Function GetMnpVarValue()

        With Me.myADJ.ManipulatedObjectData
            Return formC.Collections.FlowsheetObjectCollection(.m_ID).GetPropertyValue(.m_Property)
        End With

    End Function

    Private Function SetMnpVarValue(ByVal val As Nullable(Of Double))

        With Me.myADJ.ManipulatedObjectData
            formC.Collections.FlowsheetObjectCollection(.m_ID).SetPropertyValue(.m_Property, val)
        End With

        Return 1

    End Function

    Private Function GetRefVarValue()

        With Me.myADJ.ManipulatedObjectData
            With Me.myADJ.ControlledObjectData
                Return formC.Collections.FlowsheetObjectCollection(.m_ID).GetPropertyValue(.m_Name, su)
            End With
        End With

    End Function

    Sub AtualizaGrafico()
        ' Make sure that the curvelist has at least one curve
        If GraphControl.GraphPane.CurveList.Count > 1 Then
            Dim curve As ZedGraph.LineItem = GraphControl.GraphPane.CurveList(0)
            If curve Is Nothing Then Return
            Dim list As ZedGraph.IPointListEdit = curve.Points
            If list Is Nothing Then Return
            list.Add(py1.Count, py1(py1.Count - 1))

            Dim curve2 As ZedGraph.LineItem = GraphControl.GraphPane.CurveList(1)
            If curve2 Is Nothing Then Return
            Dim list2 As ZedGraph.IPointListEdit = curve2.Points
            If list2 Is Nothing Then Return
            list2.Add(py2.Count, py2(py2.Count - 1))

            GraphControl.AxisChange()
            ' Force a redraw
            GraphControl.Invalidate()
        Else
            With Me.GraphControl.GraphPane
                .XAxis.Scale.MinorStep = 1
                .XAxis.Scale.MajorStep = 1
                .CurveList.Clear()
                With .AddCurve("Set-point" & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, Me.su) & ")", New Double() {1}, New Double() {Convert.ToDouble(py1(0))}, Color.SteelBlue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                With .AddCurve(DWSIM.App.GetLocalString("VarivelControlada") & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.m_Property, Me.su) & ")", New Double() {1}, New Double() {Convert.ToDouble(py2(0))}, Color.SpringGreen, ZedGraph.SymbolType.Circle)
                    .Color = Color.SpringGreen
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .AxisChange(Me.CreateGraphics)
                Me.GraphControl.IsAutoScrollRange = True
                Me.GraphControl.Invalidate()
            End With
        End If
    End Sub

    Private Sub btnParar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnParar.Click
        Me.cancelar = True
    End Sub

End Class