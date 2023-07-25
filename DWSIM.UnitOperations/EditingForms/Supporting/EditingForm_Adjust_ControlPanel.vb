Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Windows.Forms
Imports System.Drawing
Imports DWSIM.ExtensionMethods

Public Class EditingForm_Adjust_ControlPanel

    Inherits UserControl

    Private formC As IFlowsheet
    Public status As String = ""
    Public cancelar As Boolean = False
    Public usemaxmin As Boolean = True

    Public myADJ As SpecialOps.Adjust
    Public py1, py2, px As New List(Of Double)

    Public su As SharedClasses.SystemsOfUnits.Units
    Public nf As String

    Private loaded As Boolean = False

    Private Sub UI_AdjControlPanelForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        Init()

    End Sub

    Public Sub Init()

        formC = myADJ.FlowSheet

        Me.su = myADJ.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        Me.nf = myADJ.FlowSheet.FlowsheetOptions.NumberFormat

        Try
            UpdateInfo()
        Catch ex As Exception
        End Try

        py1 = New List(Of Double)
        py2 = New List(Of Double)
        px = New List(Of Double)

        Text = myADJ.GraphicObject.Tag & " - " & formC.GetTranslatedString("PaineldeControle")

        ComboBox1.SelectedIndex = myADJ.SolvingMethodSelf

        loaded = True

        DWSIM.ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Public Sub UpdateInfo()

        With myADJ

            If myADJ.ControlledObject IsNot Nothing Then
                Me.tbAjuste.Text = cv.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su), .AdjustValue)
            End If
            Me.tbMaxIt.Text = .MaximumIterations
            Me.tbStep.Text = .StepSize
            Me.tbTol.Text = .Tolerance

            If myADJ.ManipulatedObject IsNot Nothing Then
                If Not .MinVal.HasValue Then .MinVal = (Convert.ToDouble(GetMnpVarValue()) * 0.2).ConvertFromSI(myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, su))
                If Not .MaxVal.HasValue Then .MaxVal = (Convert.ToDouble(GetMnpVarValue()) * 2.0).ConvertFromSI(myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, su))
            End If

            Me.tbMin.Text = .MinVal.GetValueOrDefault().ToString(formC.FlowsheetOptions.NumberFormat)
            Me.tbMax.Text = .MaxVal.GetValueOrDefault().ToString(formC.FlowsheetOptions.NumberFormat)

        End With

        Me.lblStatus.Text = formC.GetTranslatedString("StatusOcioso")
        Me.lblItXdeY.Text = ""
        Me.tbErro.Text = ""

        If myADJ.ControlledObject IsNot Nothing And myADJ.ManipulatedObject IsNot Nothing Then
            With Me.GraphControl.GraphPane
                .Title.IsVisible = False
                .XAxis.Title.Text = formC.GetTranslatedString("Iterao")
                .YAxis.Title.Text = "SP/CV - " + formC.GetTranslatedString(myADJ.ControlledObjectData.PropertyName) & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, Me.su) & ")"
                .Y2Axis.IsVisible = True
                .Y2Axis.Title.Text = "MV - " + formC.GetTranslatedString(myADJ.ManipulatedObjectData.PropertyName) & " (" & myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, Me.su) & ")"
            End With
        End If

    End Sub

    Private Sub btnIniciar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnIniciar.Click

        Me.btnIniciar.Enabled = False
        Me.myADJ.GraphicObject.Calculated = False

        py1.Clear()
        py2.Clear()
        px.Clear()

        With Me.GraphControl.GraphPane
            .CurveList.Clear()
            .AxisChange(Me.CreateGraphics)
            Me.GraphControl.IsAutoScrollRange = True
            Me.GraphControl.Invalidate()
        End With
        With Me.GraphControl.GraphPane.Legend
            .Position = ZedGraph.LegendPos.TopCenter
            .Border.IsVisible = False
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
                rfVal = Me.GetRefVarValue()
            End If
        End If
        Dim tol, maxit, adjval, stepsize, max, min As Double
        With myADJ
            If myADJ.Referenced Then
                If Not rfVal = Nothing Then
                    Dim punit = formC.SimulationObjects(myADJ.ReferencedObjectData.ID).GetPropertyUnit(.ReferencedObjectData.PropertyName, su)
                    If su.GetUnitType(punit) = Enums.UnitOfMeasure.temperature Then
                        adjval = rfVal + cv.ConvertFromSI(punit & ".", .AdjustValue)
                    Else
                        adjval = rfVal + cv.ConvertFromSI(punit, .AdjustValue)
                    End If
                Else
                    Me.btnIniciar.Enabled = True
                    Exit Sub
                End If
            Else
                adjval = cv.ConvertFromSI(.ControlledObject.GetPropertyUnit(.ControlledObjectData.PropertyName, su), .AdjustValue)
            End If
            maxit = .MaximumIterations
            stepsize = .StepSize
            tol = .Tolerance.ConvertToSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su))
            If Me.usemaxmin Then
                min = .MinVal.GetValueOrDefault.ConvertToSI(myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, su))
                max = .MaxVal.GetValueOrDefault.ConvertToSI(myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, su))
            End If
        End With

        With Me.Grid1.Columns
            .Clear()
            .Add("c1", formC.GetTranslatedString("Iter"))
            .Add("c2", "MV")
            .Add("c3", "CV")
            .Add("c4", "SP")
            .Add("c5", formC.GetTranslatedString("Erro"))
        End With
        For Each co As DataGridViewColumn In Me.Grid1.Columns
            co.SortMode = DataGridViewColumnSortMode.NotSortable
            co.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            co.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        Next

        Dim updateproc = Sub(xval As Double, f As Double, c As Integer)

                             Me.lblStatus.Text = formC.GetTranslatedString("Ajustando")
                             Me.lblItXdeY.Text = formC.GetTranslatedString("Iterao") & " " & (c + 1) & " " & formC.GetTranslatedString("de") & " " & maxit
                             Me.tbErro.Text = f.ConvertFromSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su)).ToString("G")

                             px.Add(xval.ConvertFromSI(myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, su)))
                             py1.Add(adjval)
                             py2.Add(cvVal)

                             AtualizaGrafico()

                         End Sub

        Dim fval As Double

        Dim cnt = 0

        Dim mvVal0 = mvVal

        Dim funcproc As Func(Of Double, Double) =
            Function(xval)

                If cancelar Then
                    Throw New TaskCanceledException(formC.GetTranslatedString("Ajustecanceladopelou"))
                End If

                Me.SetMnpVarValue(xval)

                DWSIM.FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(formC, GlobalSettings.Settings.SolverMode)

                If myADJ.Referenced Then
                    rfVal = Me.GetRefVarValue()
                    Dim punit = formC.SimulationObjects(myADJ.ReferencedObjectData.ID).GetPropertyUnit(myADJ.ReferencedObjectData.PropertyName, su)
                    If su.GetUnitType(punit) = Enums.UnitOfMeasure.temperature Then
                        adjval = rfVal + cv.ConvertFromSI(punit & ".", myADJ.AdjustValue)
                    Else
                        adjval = rfVal + cv.ConvertFromSI(punit, myADJ.AdjustValue)
                    End If
                End If

                cvVal = Me.GetCtlVarValue()

                fval = cvVal.ConvertToSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su)) -
                       adjval.ConvertToSI(myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su))

                If InvokeRequired Then
                    UIThreadInvoke(Sub()
                                       updateproc.Invoke(xval, fval, cnt)
                                   End Sub)
                Else
                    updateproc.Invoke(xval, fval, cnt)
                End If

                cnt += 1

                Return fval

            End Function

        Dim funcrestore = Sub(xvar As Double)
                              Me.SetMnpVarValue(xvar)
                              DWSIM.FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(formC, GlobalSettings.Settings.SolverMode)
                          End Sub

        Dim funcfinish = Sub()

                             If cancelar = True Then
                                 Me.lblStatus.Text = formC.GetTranslatedString("Ajustecanceladopelou")
                                 Me.myADJ.GraphicObject.Calculated = False
                             Else
                                 Me.lblStatus.Text = formC.GetTranslatedString("Valorajustadocomsuce")
                                 Me.myADJ.GraphicObject.Calculated = True
                             End If

                             Me.btnIniciar.Enabled = True

                             cancelar = False

                             Dim j, k As Integer
                             Dim data(4, py1.Count - 1) As String
                             j = 0
                             For Each d As Double In py1
                                 data(0, j) = j
                                 data(1, j) = px(j)
                                 data(2, j) = py2(j)
                                 data(3, j) = py1(j)
                                 data(4, j) = -py1(j) + py2(j)
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
                                         Loop Until j = 5
                                         k = k + 1
                                     Loop Until k = py1.Count
                                 End If
                             End With

                         End Sub

        If ComboBox1.SelectedIndex = 0 Then

            Task.Factory.StartNew(Sub()
                                      mvVal = MathNet.Numerics.RootFinding.Secant.FindRoot(
                                        Function(xval)
                                            If Double.IsNaN(xval) Or Double.IsInfinity(xval) Then
                                                Return 1.0E+20
                                            Else
                                                Return funcproc.Invoke(xval)
                                            End If
                                        End Function, mvVal, mvVal * 1.01, min, max, tol, maxit)
                                  End Sub).ContinueWith(Sub(t)
                                                            UIThread(Sub() formC.UpdateOpenEditForms())
                                                            UIThread(Sub() funcfinish.Invoke())
                                                            If t.Exception IsNot Nothing Then
                                                                funcrestore.Invoke(mvVal0)
                                                                MessageBox.Show(t.Exception.Message, formC.GetTranslatedString("Erro"),
                                                                                MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                            End If
                                                        End Sub)

        ElseIf ComboBox1.SelectedIndex = 1 Then

            minval = myADJ.MinVal.GetValueOrDefault.ConvertToSI(myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, su))
            maxval = myADJ.MaxVal.GetValueOrDefault.ConvertToSI(myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, su))

            Task.Factory.StartNew(Sub()
                                      mvVal = MathNet.Numerics.RootFinding.Brent.FindRoot(
                                        Function(xval)
                                            Return funcproc.Invoke(xval)
                                        End Function, minval, maxval, tol, maxit)
                                  End Sub).ContinueWith(Sub(t)
                                                            UIThread(Sub() formC.UpdateOpenEditForms())
                                                            UIThread(Sub() funcfinish.Invoke())
                                                            If t.Exception IsNot Nothing Then
                                                                funcrestore.Invoke(mvVal0)
                                                                MessageBox.Show(t.Exception.Message, formC.GetTranslatedString("Erro"),
                                                                                MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                            End If
                                                        End Sub)

        ElseIf ComboBox1.SelectedIndex = 2 Then

            Dim nsolv As New DWSIM.MathOps.MathEx.Optimization.NewtonSolver()
            nsolv.EnableDamping = False
            nsolv.MaxIterations = maxit
            nsolv.Tolerance = tol ^ 2

            Task.Factory.StartNew(Sub()
                                      mvVal = nsolv.Solve(Function(xvars)
                                                              Return New Double() {funcproc.Invoke(xvars(0))}
                                                          End Function, New Double() {mvVal})(0)
                                  End Sub).ContinueWith(Sub(t)
                                                            UIThread(Sub() formC.UpdateOpenEditForms())
                                                            UIThread(Sub() funcfinish.Invoke())
                                                            If t.Exception IsNot Nothing Then
                                                                funcrestore.Invoke(mvVal0)
                                                                MessageBox.Show(t.Exception.Message, formC.GetTranslatedString("Erro"),
                                                                                MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                            End If
                                                        End Sub)

        ElseIf ComboBox1.SelectedIndex = 3 Then

            Dim isolv As New DWSIM.MathOps.MathEx.Optimization.IPOPTSolver()
            isolv.MaxIterations = maxit
            isolv.Tolerance = tol

            Task.Factory.StartNew(Sub()
                                      mvVal = isolv.Solve(Function(xvars)
                                                              Return funcproc.Invoke(xvars(0)) ^ 2
                                                          End Function, Nothing, New Double() {mvVal}, New Double() {minval}, New Double() {maxval})(0)
                                  End Sub).ContinueWith(Sub(t)
                                                            UIThread(Sub() formC.UpdateOpenEditForms())
                                                            UIThread(Sub() funcfinish.Invoke())
                                                            If t.Exception IsNot Nothing Then
                                                                funcrestore.Invoke(mvVal0)
                                                                MessageBox.Show(t.Exception.Message, formC.GetTranslatedString("Erro"),
                                                                                MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                            End If
                                                        End Sub)

        End If


    End Sub

    Private Function GetCtlVarValue()

        With Me.myADJ.ControlledObjectData
            Return formC.SimulationObjects(.ID).GetPropertyValue(.PropertyName, su)
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

        With Me.myADJ.ReferencedObjectData
            Return formC.SimulationObjects(.ID).GetPropertyValue(.PropertyName, su)
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

            Dim curve3 As ZedGraph.LineItem = GraphControl.GraphPane.CurveList(2)
            If curve3 Is Nothing Then Return
            Dim list3 As ZedGraph.IPointListEdit = curve3.Points
            If list3 Is Nothing Then Return
            list3.Add(px.Count, px(px.Count - 1))

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
                If myADJ.Referenced Then
                    With .AddCurve("Referenced Variable (" & myADJ.ReferenceObject.GetPropertyUnit(myADJ.ReferencedObjectData.PropertyName, Me.su) & ")", New Double() {1}, New Double() {CDbl(py2(0))}, Color.SpringGreen, ZedGraph.SymbolType.Circle)
                        .Color = Color.SpringGreen
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                Else
                    With .AddCurve(formC.GetTranslatedString("VarivelControlada") & " (" & myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, Me.su) & ")", New Double() {1}, New Double() {CDbl(py2(0))}, Color.SpringGreen, ZedGraph.SymbolType.Circle)
                        .Color = Color.SpringGreen
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                End If
                With .AddCurve(formC.GetTranslatedString("VarivelManipulada") & " (" & myADJ.ManipulatedObject.GetPropertyUnit(myADJ.ManipulatedObjectData.PropertyName, Me.su) & ")", New Double() {1}, New Double() {CDbl(px(0))}, Color.Salmon, ZedGraph.SymbolType.Circle)
                    .Color = Color.Salmon
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .IsY2Axis = True
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
        If Double.TryParse(tbMaxIt.Text, New Double) Then
            If loaded Then myADJ.MaximumIterations = Integer.Parse(tbMaxIt.Text)
        End If
    End Sub

    Private Sub tbStep_TextChanged(sender As Object, e As EventArgs) Handles tbStep.TextChanged
        If Double.TryParse(tbStep.Text, New Double) Then
            If loaded Then myADJ.StepSize = Double.Parse(tbStep.Text)
        End If
    End Sub

    Private Sub tbTol_TextChanged(sender As Object, e As EventArgs) Handles tbTol.TextChanged
        If Double.TryParse(tbTol.Text, New Double) Then
            If loaded Then myADJ.Tolerance = Double.Parse(tbTol.Text)
        End If
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        If ComboBox1.SelectedIndex = 1 Then
            tbStep.Enabled = False
        Else
            tbStep.Enabled = True
        End If
        If loaded Then
            If myADJ.MaxVal.GetValueOrDefault = 0 And myADJ.MinVal.GetValueOrDefault = 0 Then
                Me.usemaxmin = False
            Else
                Me.usemaxmin = True
            End If
            myADJ.SolvingMethodSelf = ComboBox1.SelectedIndex
        End If
    End Sub

    Private Sub tbMin_TextChanged(sender As Object, e As EventArgs) Handles tbMin.TextChanged
        If loaded Then
            Try
                Dim units = myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su)
                If Double.TryParse(tbMin.Text, New Double) Then
                    myADJ.MinVal = Double.Parse(tbMin.Text)
                End If
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub tbMax_TextChanged(sender As Object, e As EventArgs) Handles tbMax.TextChanged
        If loaded Then
            Try
                Dim units = myADJ.ControlledObject.GetPropertyUnit(myADJ.ControlledObjectData.PropertyName, su)
                If Double.TryParse(tbMax.Text, New Double) Then
                    myADJ.MaxVal = Double.Parse(tbMax.Text)
                End If
            Catch ex As Exception
            End Try
        End If
    End Sub

End Class