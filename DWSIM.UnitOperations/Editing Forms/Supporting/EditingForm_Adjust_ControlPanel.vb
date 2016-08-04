Imports DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Windows.Forms
Imports System.Drawing

Public Class EditingForm_Adjust_ControlPanel

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private formC As IFlowsheet
    Public status As String = ""
    Public cancelar As Boolean = False
    Public usemaxmin As Boolean = False

    Public myADJ As SpecialOps.Adjust
    Public py1, py2 As ArrayList

    Public su As SharedClasses.SystemsOfUnits.Units
    Public nf As String

    Private loaded As Boolean = False

    Private Sub UI_AdjControlPanelForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        formC = myADJ.FlowSheet

        Me.su = myADJ.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        Me.nf = myADJ.FlowSheet.FlowsheetOptions.NumberFormat

        With myADJ
            Me.tbAjuste.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), .AdjustValue)
            Me.tbMaxIt.Text = .MaximumIterations
            Me.tbStep.Text = .StepSize
            Me.tbTol.Text = .Tolerance
            Me.tbMin.Text = .MinVal.GetValueOrDefault
            Me.tbMax.Text = .MaxVal.GetValueOrDefault
        End With

        Me.lblStatus.Text = formC.GetTranslatedString("StatusOcioso")
        Me.lblItXdeY.Text = ""
        Me.tbErro.Text = ""

        With Me.GraphControl.GraphPane
            .Title.IsVisible = False
            .XAxis.Title.Text = formC.GetTranslatedString("Iterao")
            .YAxis.Title.Text = formC.GetTranslatedString(myADJ.ControlledObjectData.PropertyName) & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, Me.su) & ")"
        End With

        py1 = New ArrayList
        py2 = New ArrayList

        Me.Text = myADJ.GraphicObject.Tag & " - " & formC.GetTranslatedString("PaineldeControle")
        Me.TabText = Me.Text

        loaded = True

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

        If formC.SimulationObjects(myADJ.ControlledObjectData.ID).GraphicObject.Calculated Then
            cvVal = Me.GetCtlVarValue()
        End If
        If formC.SimulationObjects(myADJ.ManipulatedObjectData.ID).GraphicObject.Calculated Then
            mvVal = Me.GetMnpVarValue()
            maxval = myADJ.MaxVal.GetValueOrDefault
            minval = myADJ.MinVal.GetValueOrDefault
        End If
        If myADJ.Referenced Then
            If formC.SimulationObjects(myADJ.ReferencedObjectData.ID).GraphicObject.Calculated Then
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
            .Add("c1", formC.GetTranslatedString("Iter"))
            .Add("c2", "Var.")
            .Add("c3", formC.GetTranslatedString("Erro"))
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

                Me.lblStatus.Text = formC.GetTranslatedString("Ajustando")
                Me.lblItXdeY.Text = formC.GetTranslatedString("Iterao") & " " & (cnt + 1) & " " & formC.GetTranslatedString("de") & " " & maxit
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
                        Dim msgres As MsgBoxResult = MessageBox.Show(formC.GetTranslatedString("Avarivelmanipuladaat") _
                                        & vbCrLf & formC.GetTranslatedString("Desejacontinuaroproc"), _
                                        formC.GetTranslatedString("Limitesdavarivelmani"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                        If msgres = MsgBoxResult.No Then
                            cancelar = True
                            Exit Do
                        End If
                    End If
                End If

                Me.SetMnpVarValue(var)

                DWSIM.FlowsheetSolver.FlowsheetSolver.CalculateObject(formC, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                cnt += 1

                py1.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), adjval))
                py2.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), cvVal))

                AtualizaGrafico()

                If fi = fi_ant Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(formC.GetTranslatedString("Avarivelmanipuladano") _
                                    & vbCrLf & formC.GetTranslatedString("Desejacontinuaroproc"), _
                                    formC.GetTranslatedString("Problemasnaconvergnc"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        cancelar = True
                        Exit Do
                    End If
                End If

                If cnt >= maxit Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(formC.GetTranslatedString("Onmeromximodeiteraes"), _
                                                    formC.GetTranslatedString("Nmeromximodeiteraesa3"), _
                                                    MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        cancelar = False
                        Me.lblStatus.Text = formC.GetTranslatedString("Mximodeiteraesatingi")
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
                Me.lblStatus.Text = formC.GetTranslatedString("Ajustecanceladopelou")
                Me.myADJ.GraphicObject.Calculated = False
            Else
                Me.lblStatus.Text = formC.GetTranslatedString("Valorajustadocomsuce")
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

                DWSIM.FlowsheetSolver.FlowsheetSolver.CalculateObject(formC, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                f = cvVal - adjval
                Me.lblStatus.Text = formC.GetTranslatedString("Ajustando")
                Me.lblItXdeY.Text = formC.GetTranslatedString("Procurandosubinterva")
                Me.tbErro.Text = f
                py1.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), adjval))
                py2.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), cvVal))
                AtualizaGrafico()
                minval = minval + delta
                Me.SetMnpVarValue(minval)

                DWSIM.FlowsheetSolver.FlowsheetSolver.CalculateObject(formC, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                f_inf = cvVal - adjval
                Me.lblStatus.Text = formC.GetTranslatedString("Ajustando")
                Me.lblItXdeY.Text = formC.GetTranslatedString("Procurandosubinterva")
                Me.tbErro.Text = f_inf
                py1.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), adjval))
                py2.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), cvVal))
                AtualizaGrafico()
                l += 1
                If l > 5 Then
                    MessageBox.Show(formC.GetTranslatedString("Oajustenoencontrouum"), formC.GetTranslatedString("Semsoluo"), MessageBoxButtons.OK, MessageBoxIcon.Information)
                    Me.lblStatus.Text = formC.GetTranslatedString("Noexistesoluonointer")
                    Me.lblItXdeY.Text = ""
                    cancelar = False
                    Me.btnIniciar.Enabled = True
                    Me.myADJ.GraphicObject.Calculated = False
                    Exit Sub
                End If
                If f = f_inf Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(formC.GetTranslatedString("Avarivelmanipuladano") _
                                    & vbCrLf & formC.GetTranslatedString("Desejacontinuaroproc"), _
                                    formC.GetTranslatedString("Problemasnaconvergnc"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        Me.lblStatus.Text = formC.GetTranslatedString("Ajustecanceladopelou")
                        Me.btnIniciar.Enabled = True
                        Me.myADJ.GraphicObject.Calculated = False
                        Exit Sub
                    End If
                End If
                If cancelar = True Then Exit Do
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
                If cancelar = True Then Exit Do
                Me.lblStatus.Text = formC.GetTranslatedString("Ajustando")
                Me.lblItXdeY.Text = formC.GetTranslatedString("Iterao") & " " & (iter2 + l + 1) & " " & formC.GetTranslatedString("de") & " " & maxit
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

                DWSIM.FlowsheetSolver.FlowsheetSolver.CalculateObject(formC, myADJ.ManipulatedObject.GraphicObject.Name)

                cvVal = Me.GetCtlVarValue()
                fbb = cvVal - adjval
                Me.tbErro.Text = fbb
                iter2 += 1

                py1.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), adjval))
                py2.Add(SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), cvVal))

                AtualizaGrafico()

                If iter2 + l - 1 >= maxit Then
                    Dim msgres As MsgBoxResult = MessageBox.Show(formC.GetTranslatedString("Onmeromximodeiteraes"), _
                                                    formC.GetTranslatedString("Nmeromximodeiteraesa3"), _
                                                    MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    If msgres = MsgBoxResult.No Then
                        cancelar = False
                        Me.lblStatus.Text = formC.GetTranslatedString("Mximodeiteraesatingi")
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
                Me.lblStatus.Text = formC.GetTranslatedString("Ajustecanceladopelou")
                Me.myADJ.GraphicObject.Calculated = False
            Else
                Me.lblStatus.Text = formC.GetTranslatedString("Valorajustadocomsuce")
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
                            .Item(k).Cells(j).Value = Format(CDbl(data(j, k)), nf)
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
    '            Case formC.GetTranslatedString("CorrentedeMatria")
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("Temperatura")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.ID).Fases(0).SPMProperties.temperature = val
    '                    Case formC.GetTranslatedString("Presso")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.ID).Fases(0).SPMProperties.pressure = val
    '                    Case formC.GetTranslatedString("Vazomssica")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.ID).Fases(0).SPMProperties.massflow = val
    '                    Case formC.GetTranslatedString("Vazovolumtrica")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.ID).Fases(0).SPMProperties.volumetric_flow = val
    '                    Case formC.GetTranslatedString("Vazomolar")
    '                        Me.formC.Collections.CLCS_MaterialStreamCollection(.ID).Fases(0).SPMProperties.molarflow = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case "Corrente de Energia"
    '                Return Me.formC.Collections.CLCS_EnergyStreamCollection(.ID).Energia.GetValueOrDefault
    '            Case formC.GetTranslatedString("Misturadores")
    '                Return Nothing
    '            Case formC.GetTranslatedString("MisturadoresMatEn")
    '                Return Nothing
    '            Case formC.GetTranslatedString("Divisores")
    '                Return Nothing
    '            Case formC.GetTranslatedString("Tubulaes")
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("DeltaP")
    '                        Me.formC.Collections.CLCS_PipeCollection(.ID).DeltaP = val
    '                    Case formC.GetTranslatedString("DeltaT")
    '                        Me.formC.Collections.CLCS_PipeCollection(.ID).DeltaT = val
    '                    Case formC.GetTranslatedString("Calortrocado")
    '                        Me.formC.Collections.CLCS_PipeCollection(.ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case formC.GetTranslatedString("Vlvulas")
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("DeltaT")
    '                        Me.formC.Collections.CLCS_ValveCollection(.ID).DeltaT = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case formC.GetTranslatedString("Bombas")
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("DeltaT")
    '                        Me.formC.Collections.CLCS_PumpCollection(.ID).DeltaT = val
    '                    Case formC.GetTranslatedString("Potnciarequerida")
    '                        Me.formC.Collections.CLCS_PumpCollection(.ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case formC.GetTranslatedString("Tanques")
    '                Return Nothing
    '            Case formC.GetTranslatedString("Separadores")
    '                Return Nothing
    '            Case formC.GetTranslatedString("Compressores"))
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("DeltaT")
    '                        Me.formC.Collections.CLCS_CompressorCollection(.ID).DeltaT = val
    '                    Case formC.GetTranslatedString("Potnciarequerida")
    '                        Me.formC.Collections.CLCS_CompressorCollection(.ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case formC.GetTranslatedString("Turbinas")
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("DeltaT")
    '                        Me.formC.Collections.CLCS_TurbineCollection(.ID).DeltaT = val
    '                    Case formC.GetTranslatedString("Potnciagerada")
    '                        Me.formC.Collections.CLCS_TurbineCollection(.ID).DeltaQ = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case formC.GetTranslatedString("Aquecedores")
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("DeltaT")
    '                        Me.formC.Collections.CLCS_HeaterCollection(.ID).DeltaT = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '            Case formC.GetTranslatedString("Resfriadores")
    '                Select Case .PropertyName
    '                    Case formC.GetTranslatedString("DeltaT")
    '                        Me.formC.Collections.CLCS_CoolerCollection(.ID).DeltaT = val
    '                    Case Else
    '                        Return Nothing
    '                End Select
    '        End Select
    '    End With

    '    Return 1

    'End Function

    Private Function GetCtlVarValue()

        With Me.myADJ.ControlledObjectData
            Return formC.SimulationObjects(.ID).GetPropertyValue(.PropertyName)
        End With

    End Function

    Private Function GetMnpVarValue()

        With Me.myADJ.ManipulatedObjectData
            Return formC.SimulationObjects(.ID).GetPropertyValue(.PropertyName)
        End With

    End Function

    Private Function SetMnpVarValue(ByVal val As Nullable(Of Double))

        With Me.myADJ.ManipulatedObjectData
            formC.SimulationObjects(.ID).SetPropertyValue(.PropertyName, val)
        End With

        Return 1

    End Function

    Private Function GetRefVarValue()

        With Me.myADJ.ManipulatedObjectData
            With Me.myADJ.ControlledObjectData
                Return formC.SimulationObjects(.ID).GetPropertyValue(.Name, su)
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
                With .AddCurve("Set-point" & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, Me.su) & ")", New Double() {1}, New Double() {CDbl(py1(0))}, Color.SteelBlue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                With .AddCurve(formC.GetTranslatedString("VarivelControlada") & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, Me.su) & ")", New Double() {1}, New Double() {CDbl(py2(0))}, Color.SpringGreen, ZedGraph.SymbolType.Circle)
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

    Private Sub tbMaxIt_TextChanged(sender As Object, e As EventArgs) Handles tbMaxIt.TextChanged
        If loaded Then myADJ.MaximumIterations = Integer.Parse(tbMaxIt.Text)
    End Sub

    Private Sub tbStep_TextChanged(sender As Object, e As EventArgs) Handles tbStep.TextChanged
        If loaded Then myADJ.StepSize = Double.Parse(tbStep.Text)
    End Sub

    Private Sub tbTol_TextChanged(sender As Object, e As EventArgs) Handles tbTol.TextChanged
        If loaded Then myADJ.Tolerance = Double.Parse(tbTol.Text)
    End Sub

    Private Sub rbBrent_CheckedChanged(sender As Object, e As EventArgs) Handles rbBrent.CheckedChanged
        tbMin.Enabled = rbBrent.Checked
        tbMax.Enabled = rbBrent.Checked
        tbStep.Enabled = Not rbBrent.Checked
    End Sub

    Private Sub tbMin_TextChanged(sender As Object, e As EventArgs) Handles tbMin.TextChanged
        If loaded Then myADJ.MinVal = Double.Parse(tbMin.Text)
    End Sub

    Private Sub tbMax_TextChanged(sender As Object, e As EventArgs) Handles tbMax.TextChanged
        If loaded Then myADJ.MaxVal = Double.Parse(tbMax.Text)
    End Sub

    Public Sub DockingHandler(sender As Object, e As EventArgs) Handles tsbDockingLeft.Click, tsbDockingBottom.Click, tsbDockingDocument.Click,
                                                                    tsbDockingFloat.Click, tsbDockingLeftAutoHide.Click, tsbDockingRight.Click,
                                                                    tsbDockingRightAutoHide.Click, tsbDockingTop.Click

        If sender Is tsbDockingLeft Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        ElseIf sender Is tsbDockingLeftAutoHide Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
        ElseIf sender Is tsbDockingRight Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
        ElseIf sender Is tsbDockingRightAutoHide Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
        ElseIf sender Is tsbDockingTop Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
        ElseIf sender Is tsbDockingBottom Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
        ElseIf sender Is tsbDockingDocument Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
        ElseIf sender Is tsbDockingFloat Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
        End If

    End Sub

    Private Sub tsbClose_Click(sender As Object, e As EventArgs) Handles tsbClose.Click
        Me.Close()
    End Sub

    Private Sub rbSecante_CheckedChanged(sender As Object, e As EventArgs) Handles rbSecante.CheckedChanged
        If loaded Then
            If myADJ.MaxVal.GetValueOrDefault = 0 And myADJ.MinVal.GetValueOrDefault = 0 Then
                Me.usemaxmin = False
            Else
                Me.usemaxmin = True
            End If
        End If
    End Sub
End Class