'    Sensitivity Analysis Classes
'    Copyright 2009 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.Optimization
Imports DWSIM.DrawingTools
Imports DWSIM.MathOps.MathEx
Imports Ciloci.Flee
Imports System.Math
Imports ZedGraph
Imports DotNumerics
Imports DWSIM.FlowsheetSolver
Imports Cureos.Numerics
Imports DWSIM.SharedClasses
Imports DWSIM.SharedClasses.Flowsheet.Optimization
Imports DWSIM.SharedClasses.DWSIM.Flowsheet
Imports System.Linq

Public Class FormOptimization

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public nf As String
    Public su As SystemsOfUnits.Units
    Public cv As SystemsOfUnits.Converter
    Public form As FormFlowsheet

    Public abortCalc As Boolean = False
    Public selectedindex0 As Integer = 0

    Public cbc1, cbc2, cbc3 As DataGridViewComboBoxCell
    Public keysind, keysaux, keyscon As ArrayList
    Public funcval As Double
    Public keydep As String
    Public selectedoptcase As OptimizationCase

    Public fmin As Double
    Public info As Integer

    Private selected As Boolean = False

    Private _penval As Double = 0

    Private Sub FormOptimization_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If Not Me.DockHandler Is Nothing OrElse Not Me.DockHandler.FloatPane Is Nothing Then
            ' set the bounds of this form's FloatWindow to our desired position and size
            If Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float Then
                Dim floatWin = Me.DockHandler.FloatPane.FloatWindow
                If Not floatWin Is Nothing Then
                    floatWin.SetBounds(floatWin.Location.X, floatWin.Location.Y, 771, 616)
                End If
            End If
        End If

        Me.ComboBox1.SelectedIndex = 0

        keysind = New ArrayList
        keysaux = New ArrayList
        keyscon = New ArrayList

        form = My.Application.ActiveSimulation

        cv = New SystemsOfUnits.Converter
        su = form.Options.SelectedUnitSystem
        nf = form.Options.NumberFormat

        Me.lbCases.Items.Clear()

        If form.Collections.OPT_OptimizationCollection Is Nothing Then form.Collections.OPT_OptimizationCollection = New List(Of OptimizationCase)

        For Each optcase As OptimizationCase In form.Collections.OPT_OptimizationCollection
            Me.lbCases.Items.Add(optcase.name)
        Next

        cbc1 = New DataGridViewComboBoxCell
        cbc1.Items.AddRange(New Object() {"IND", "AUX", "DEP", "CON"})

        cbc2 = New DataGridViewComboBoxCell
        cbc2.Sorted = True
        cbc2.Items.Add(DWSIM.App.GetLocalString("SpreadsheetCell"))
        cbc2.Items.Add(DWSIM.App.GetLocalString("ReactionProperty"))
        For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            cbc2.Items.Add(obj.GraphicObject.Tag)
        Next
        cbc3 = New DataGridViewComboBoxCell

        Dim tbc1 As New DataGridViewTextBoxCell()
        Dim tbc2 As New DataGridViewTextBoxCell()
        Dim tbc3 As New DataGridViewTextBoxCell()
        With tbc1
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With
        With tbc2
            .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        End With
        With tbc3
            .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
            .Style.BackColor = Color.FromKnownColor(KnownColor.Control)
        End With

        With Me.dgVariables
            .Columns(0).CellTemplate = tbc1
            .Columns(1).CellTemplate = tbc2
            .Columns(2).CellTemplate = cbc1
            .Columns(3).CellTemplate = cbc2
            .Columns(4).CellTemplate = cbc3
            .Columns(5).CellTemplate = tbc2
            .Columns(6).CellTemplate = tbc2
            .Columns(7).CellTemplate = tbc3
            .Columns(8).CellTemplate = tbc3
            .Columns(9).CellTemplate = tbc3
            .Columns(1).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        If Me.lbCases.Items.Count > 0 Then Me.lbCases.SelectedIndex = 0

        form.WriteToLog(DWSIM.App.GetLocalTipString("FOPT001"), Color.Black, MessageType.Tip)

    End Sub

    Private Sub btnDeleteCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDeleteCase.Click
        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = Windows.Forms.DialogResult.Yes Then
            form.Collections.OPT_OptimizationCollection.RemoveAt(lbCases.SelectedIndex)
            Me.lbCases.Items.Remove(Me.lbCases.SelectedItem)
            If lbCases.Items.Count > 0 Then Me.lbCases.SelectedIndex = 0
        End If
    End Sub

    Private Sub btnCopyCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCopyCase.Click
        Dim optcase2 As New OptimizationCase
        Dim optcase = form.Collections.OPT_OptimizationCollection(Me.lbCases.SelectedIndex)
        optcase2 = optcase.Clone
        optcase2.name = optcase.name & "_1"

        Me.lbCases.Items.Add(optcase2.name)
        Me.lbCases.SelectedItem = optcase2.name
        form.Collections.OPT_OptimizationCollection.Add(optcase2)
    End Sub

    Private Sub btnSaveCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSaveCase.Click

        Dim prevselected = lbCases.SelectedIndex

        For i As Integer = 0 To lbCases.Items.Count - 1
            lbCases.SelectedIndex = i
            Dim optcase = form.Collections.OPT_OptimizationCollection(Me.lbCases.SelectedIndex)
            SaveForm(optcase)
        Next

        lbCases.SelectedIndex = prevselected

    End Sub

    Private Sub btnNewCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnNewCase.Click

        Dim optcase As New OptimizationCase

        optcase.name = "optcase" & form.Collections.OPT_OptimizationCollection.Count

        Me.lbCases.Items.Add(optcase.name)
        Me.lbCases.SelectedItem = optcase.name

        form.Collections.OPT_OptimizationCollection.Add(optcase)

    End Sub

    Private Sub lbCases_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbCases.SelectedIndexChanged

        Me.selectedindex0 = Me.lbCases.SelectedIndex

        If Not Me.lbCases.SelectedItem Is Nothing Then
            For Each optcase As OptimizationCase In form.Collections.OPT_OptimizationCollection
                If optcase.name = Me.lbCases.SelectedItem.ToString Then
                    Me.selectedoptcase = optcase
                    Me.PopulateForm(optcase)
                End If
            Next
        End If

        selected = True

    End Sub

    Private Sub tsbAddVar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbAddVar.Click
        Me.dgVariables.Rows.Add()
        Me.dgVariables.Rows(Me.dgVariables.Rows.Count - 1).HeaderCell.Value = Me.dgVariables.Rows.Count.ToString
        Me.dgVariables.Rows(Me.dgVariables.Rows.Count - 1).Cells(0).Value = Guid.NewGuid.ToString
    End Sub

    Private Sub tsbDelVar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbDelVar.Click
        If Me.dgVariables.SelectedRows.Count > 0 Then
            For i As Integer = 0 To Me.dgVariables.SelectedRows.Count - 1
                Me.dgVariables.Rows.Remove(Me.dgVariables.SelectedRows(0))
            Next
        End If
    End Sub

    Private Sub dgVariables_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgVariables.CellValueChanged
        If e.RowIndex >= 0 Then
            Select Case e.ColumnIndex
                Case 3
                    Dim cbc As DataGridViewComboBoxCell = Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex + 1)
                    cbc.Items.Clear()
                    With cbc.Items
                        Dim objname = Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString
                        If objname <> "" Then
                            Dim props As String()
                            If objname = DWSIM.App.GetLocalString("SpreadsheetCell") Then
                                props = form.FormSpreadsheet.GetCellString()
                            ElseIf objname = DWSIM.App.GetLocalString("ReactionProperty") Then
                                Dim rprops As New List(Of String)
                                For Each rx In form.Reactions.Values
                                    Dim rprops2 As String() = rx.GetPropertyList
                                    For Each p In rprops2
                                        rprops.Add(rx.Name & "|" & p)
                                    Next
                                Next
                                props = rprops.ToArray
                            Else
                                If Me.dgVariables.Rows(e.RowIndex).Cells(2).Value <> "IND" Then
                                    props = Me.ReturnProperties(Me.dgVariables.Rows(e.RowIndex).Cells(3).Value, True)
                                Else
                                    props = Me.ReturnProperties(Me.dgVariables.Rows(e.RowIndex).Cells(3).Value, False)
                                End If
                            End If
                            For Each prop As String In props
                                .Add(form.GetTranslatedString1(prop))
                            Next
                        End If
                    End With
                    cbc.Sorted = True
                Case 4
                    If Not Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value Is Nothing Then
                        Dim objname = Me.dgVariables.Rows(e.RowIndex).Cells(3).Value.ToString
                        If objname = DWSIM.App.GetLocalString("SpreadsheetCell") Then
                            Me.dgVariables.Rows(e.RowIndex).Cells(7).Value = form.FormSpreadsheet.GetCellValue(Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString).Data
                            Me.dgVariables.Rows(e.RowIndex).Cells(8).Value = form.FormSpreadsheet.GetCellValue(Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString).Data
                        ElseIf objname = DWSIM.App.GetLocalString("ReactionProperty") Then
                            Dim rx = form.Reactions.Values.Where(Function(x) x.Name = Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString.Split("|")(0)).FirstOrDefault
                            Dim val = rx.GetPropertyValue(Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString.Split("|")(1))
                            Me.dgVariables.Rows(e.RowIndex).Cells(7).Value = val
                            Me.dgVariables.Rows(e.RowIndex).Cells(8).Value = val
                        Else
                            Dim tbc As DataGridViewTextBoxCell = Me.dgVariables.Rows(e.RowIndex).Cells(7)
                            Dim tbc0 As DataGridViewTextBoxCell = Me.dgVariables.Rows(e.RowIndex).Cells(8)
                            Dim tbc1 As DataGridViewTextBoxCell = Me.dgVariables.Rows(e.RowIndex).Cells(9)
                            Dim props As String() = Me.ReturnProperties(Me.dgVariables.Rows(e.RowIndex).Cells(3).Value, True)
                            For Each prop As String In props
                                If form.GetTranslatedString1(prop) = Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString Then
                                    Dim obj As SharedClasses.UnitOperations.BaseClass = ReturnObject(Me.dgVariables.Rows(e.RowIndex).Cells(3).Value)
                                    tbc.Value = Format(obj.GetPropertyValue(prop, su), nf)
                                    tbc0.Value = Format(obj.GetPropertyValue(prop, su), nf)
                                    tbc1.Value = obj.GetPropertyUnit(prop, su)
                                End If
                            Next
                        End If
                    End If
            End Select
        End If
    End Sub

    Private Function ReturnProperties(ByVal objectTAG As String, ByVal dependent As Boolean) As String()

        For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            If objectTAG = obj.GraphicObject.Tag Then
                If dependent Then
                    Return obj.GetProperties(Interfaces.Enums.PropertyType.ALL)
                Else
                    Return obj.GetProperties(Interfaces.Enums.PropertyType.WR)
                End If
                Exit Function
            End If
        Next

        Return Nothing

    End Function

    Private Function ReturnObject(ByVal objectTAG As String) As SharedClasses.UnitOperations.BaseClass

        For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            If objectTAG = obj.GraphicObject.Tag Then
                Return obj
                Exit Function
            End If
        Next

        Return Nothing

    End Function

    Private Function ReturnPropertyID(ByVal objectID As String, ByVal propTAG As String) As String

        Dim props As String() = form.Collections.FlowsheetObjectCollection(objectID).GetProperties(Interfaces.Enums.PropertyType.ALL)
        For Each prop As String In props
            If form.GetTranslatedString1(prop) = propTAG Then
                Return prop
            End If
        Next

        Return Nothing

    End Function

    Private Sub btnRun_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRun.Click

        Dim idx As Integer = 0
        Dim counter As Integer = 0
        Dim res As New ArrayList

        SaveForm(selectedoptcase)

        Me.keysaux.Clear()
        Me.keysind.Clear()
        Me.keyscon.Clear()

        'variables
        For Each var As OPTVariable In selectedoptcase.variables.Values
            Select Case var.type
                Case OPTVariableType.Independent
                    keysind.Add(var.id)
                    counter += 1
                Case OPTVariableType.Dependent
                    keydep = var.id
                Case OPTVariableType.Auxiliary
                    keysaux.Add(var.id)
                Case OPTVariableType.Constraint
                    keyscon.Add(var.id)
            End Select
        Next

        Dim lconstr(keysind.Count), uconstr(keysind.Count), initval(keysind.Count) As Double
        Dim lconstr2(keysind.Count - 1), uconstr2(keysind.Count - 1), initval2(keysind.Count - 1) As Double
        Dim nbd(keysind.Count) As Integer
        Dim n As Integer = keysind.Count
        Dim m As Integer = 5

        If m > n Then m = n - 1

        Dim i As Integer = 1
        For Each key As String In keysind
            With selectedoptcase.variables(key)
                lconstr(i) = .lowerlimit
                uconstr(i) = .upperlimit
                nbd(i) = .boundtype
                initval(i) = .initialvalue
            End With
            i = i + 1
        Next

        i = 0
        For Each key As String In keysind
            With selectedoptcase.variables(key)
                lconstr2(i) = .lowerlimit
                uconstr2(i) = .upperlimit
                initval2(i) = .initialvalue
            End With
            i = i + 1
        Next

        Dim fval As Double = 0

        Try

            Me.SetupGraph()

            Me.btnRun.Enabled = False
            Me.btnAbort.Enabled = True
            Me.btnRestore.Enabled = False
            Me.abortCalc = False

            Me.selectedoptcase.results.Clear()
            Dim xmin As Double

            With selectedoptcase
                form.WriteToLog("Optimization started with method " & Me.ComboBox1.SelectedItem.ToString, Color.SeaGreen, MessageType.Information)
                Select Case .solvm
                    Case OptimizationCase.SolvingMethod.AL_BRENT
                        Dim brentsolver As New BrentOpt.BrentMinimize
                        brentsolver.DefineFuncDelegate(AddressOf CalcFuncVal_Brent)
                        fmin = brentsolver.brentoptimize(0, initval(0) * 100, .tolerance, xmin)
                    Case OptimizationCase.SolvingMethod.AL_BRENT_B
                        Dim brentsolver As New BrentOpt.BrentMinimize
                        brentsolver.DefineFuncDelegate(AddressOf CalcFuncVal_Brent)
                        fmin = brentsolver.brentoptimize(.variables(keysind(0)).lowerlimit, .variables(keysind(0)).upperlimit, .tolerance, xmin)
                    Case OptimizationCase.SolvingMethod.AL_LBFGS
                        Dim lbfgssolver As New LBFGS.lbfgs
                        lbfgssolver.DefineFuncGradDelegate(AddressOf FuncGrad_LBFGS)
                        lbfgssolver.DefineNewIterDelegate(AddressOf NewIterUpdate)
                        lbfgssolver.lbfgsminimize(n, m, initval, 0.000001, .epsF, 0.000001, .maxits, info)
                    Case OptimizationCase.SolvingMethod.AL_LBFGS_B
                        Dim lbfgsbsolver As New LBFGSB.lbfgsb
                        lbfgsbsolver.DefineFuncGradDelegate(AddressOf FuncGrad_LBFGS)
                        lbfgsbsolver.DefineNewIterDelegate(AddressOf NewIterUpdate)
                        lbfgsbsolver.lbfgsbminimize(n, m, initval, 0.000001, .epsF, 0.000001, .maxits, nbd, lconstr, uconstr, info)
                    Case OptimizationCase.SolvingMethod.DN_LBFGS
                        Dim solver As New Optimization.L_BFGS_B
                        solver.Tolerance = .tolerance
                        solver.MaxFunEvaluations = .maxits
                        solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, initval2)
                    Case OptimizationCase.SolvingMethod.DN_NELDERMEAD_SIMPLEX
                        Dim solver As New Optimization.Simplex
                        solver.Tolerance = .tolerance
                        solver.MaxFunEvaluations = .maxits
                        solver.ComputeMin(AddressOf FunctionValue, initval2)
                    Case OptimizationCase.SolvingMethod.DN_TRUNCATED_NEWTON
                        Dim solver As New Optimization.TruncatedNewton
                        solver.Tolerance = .tolerance
                        solver.MaxFunEvaluations = .maxits
                        solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, initval2)
                    Case OptimizationCase.SolvingMethod.DN_LBFGS_B
                        Dim variables(keysind.Count - 1) As Optimization.OptBoundVariable
                        For i = 0 To keysind.Count - 1
                            variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), lconstr2(i), uconstr2(i))
                        Next
                        Dim solver As New Optimization.L_BFGS_B
                        solver.Tolerance = .tolerance
                        solver.MaxFunEvaluations = .maxits
                        solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    Case OptimizationCase.SolvingMethod.DN_NELDERMEAD_SIMPLEX_B
                        Dim variables(keysind.Count - 1) As Optimization.OptBoundVariable
                        For i = 0 To keysind.Count - 1
                            variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), lconstr2(i), uconstr2(i))
                        Next
                        Dim solver As New Optimization.Simplex
                        solver.Tolerance = .tolerance
                        solver.MaxFunEvaluations = .maxits
                        solver.ComputeMin(AddressOf FunctionValue, variables)
                    Case OptimizationCase.SolvingMethod.DN_TRUNCATED_NEWTON_B
                        Dim variables(keysind.Count - 1) As Optimization.OptBoundVariable
                        For i = 0 To keysind.Count - 1
                            variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), lconstr2(i), uconstr2(i))
                        Next
                        Dim solver As New Optimization.TruncatedNewton
                        solver.Tolerance = .tolerance
                        solver.MaxFunEvaluations = .maxits
                        solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    Case OptimizationCase.SolvingMethod.IPOPT
                        Dim obj As Double
                        Dim status As IpoptReturnCode
                        Using problem As New Ipopt(initval2.Length, lconstr2, uconstr2, 0, Nothing, Nothing,
                         0, 0, AddressOf eval_f, AddressOf eval_g,
                         AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                            problem.AddOption("tol", .tolerance)
                            problem.AddOption("max_iter", .maxits)
                            problem.AddOption("mu_strategy", "adaptive")
                            problem.AddOption("hessian_approximation", "limited-memory")
                            ' solve the problem 
                            status = problem.SolveProblem(initval2, obj, Nothing, Nothing, Nothing, Nothing)
                        End Using
                End Select
            End With
            form.WriteToLog("Optimization finished successfully.", Color.SeaGreen, MessageType.Information)
        Catch ex As Exception
            form.WriteToLog("Optimization error: " & ex.Message, Color.Red, MessageType.GeneralError)
        Finally
            Me.btnRun.Enabled = True
            Me.btnAbort.Enabled = False
            Me.btnRestore.Enabled = True
        End Try

    End Sub

    Public Function CalcFuncVal_Brent(ByVal t As Double) As Double

        If Me.abortCalc Then
            Throw New Exception("Optimization aborted")
        End If

        Dim objID, objProp, objName As String
        Dim AobjID(Me.keysaux.Count), AobjProp(Me.keysaux.Count), AobjName(Me.keysaux.Count) As String
        Dim i As Integer

        objID = Me.selectedoptcase.variables(Me.keysind(0)).objectID
        objProp = Me.selectedoptcase.variables(Me.keysind(0)).propID
        objName = Me.selectedoptcase.variables(Me.keysind(0)).name

        If objID <> "SpreadsheetCell" And objID <> "ReactionProperty" Then
            form.Collections.FlowsheetObjectCollection(objID).SetPropertyValue(objProp, t)
            FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID)
        ElseIf objID = "ReactionProperty" Then
            Dim rx = form.Reactions.Values.Where(Function(x) x.Name = objProp.Split("|")(0)).FirstOrDefault
            rx.SetPropertyValue(objProp.Split("|")(1), t)
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
        Else
            form.FormSpreadsheet.SetCellValue(objProp, t)
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
        End If

        Dim pen_val As Double

        UpdateVariablesValues()
        pen_val = ReturnPenaltyValue()

        Dim value As Double

        If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
            For i = 1 To Me.keysaux.Count
                AobjID(i) = Me.selectedoptcase.variables(Me.keysaux(i - 1)).objectID
                AobjProp(i) = Me.selectedoptcase.variables(Me.keysaux(i - 1)).propID
                AobjName(i) = Me.selectedoptcase.variables(Me.keysaux(i - 1)).name
            Next
            Me.selectedoptcase.econtext = New ExpressionContext
            With Me.selectedoptcase.econtext
                .Imports.AddType(GetType(System.Math))
                .Variables.Add(objName, SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(Me.keysind(0)).unit, t))
                For i = 1 To Me.keysaux.Count
                    For j = 1 To Me.keysaux.Count
                        Dim j2 = j
                        If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                            .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(Me.keysaux(i - 1)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                        ElseIf AobjID(i) = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j2).Split("|")(0)).FirstOrDefault
                            .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                        Else
                            .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                        End If
                    Next
                Next
                Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
            End With
            value = Me.selectedoptcase.exbase.Evaluate
        Else
            objID = Me.selectedoptcase.variables(Me.keydep).objectID
            objProp = Me.selectedoptcase.variables(Me.keydep).propID
            If objID <> "SpreadsheetCell" And objID <> "ReactionProperty" Then
                value = form.Collections.FlowsheetObjectCollection(objID).GetPropertyValue(objProp)
            ElseIf objID = "ReactionProperty" Then
                Dim rx = form.Reactions.Values.Where(Function(x) x.Name = objProp.Split("|")(0)).FirstOrDefault
                value = rx.GetPropertyValue(objProp.Split("|")(1))
            Else
                value = form.FormSpreadsheet.GetCellValue(objProp).Data
            End If
        End If

        Dim var As OPTVariable
        For Each row As DataGridViewRow In Me.dgVariables.Rows
            var = Me.selectedoptcase.variables(row.Cells(0).Value)
            If var.objectID <> "SpreadsheetCell" And var.objectID <> "ReactionProperty" Then
                row.Cells(8).Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(row.Cells(0).Value).unit, form.Collections.FlowsheetObjectCollection(var.objectID).GetPropertyValue(var.propID)), nf)
            ElseIf var.objectID = "ReactionProperty" Then
                Dim rx = form.Reactions.Values.Where(Function(x) x.Name = var.propID.Split("|")(0)).FirstOrDefault
                row.Cells(8).Value = Format(rx.GetPropertyValue(var.propID.Split("|")(1)), nf)
            Else
                row.Cells(8).Value = form.FormSpreadsheet.GetCellValue(var.propID).Data
            End If
        Next

        Me.selectedoptcase.results.Add(value)

        Dim curve As LineItem = Me.grProgress.GraphPane.CurveList(0)
        Dim list As IPointListEdit = curve.Points
        list.Add(Convert.ToInt32(Me.selectedoptcase.results.Count), Me.selectedoptcase.results(Me.selectedoptcase.results.Count - 1))
        Me.grProgress.GraphPane.XAxis.Scale.Min = 0
        Me.grProgress.GraphPane.XAxis.Scale.Max = Me.selectedoptcase.results.Count
        Me.grProgress.AxisChange()
        Me.grProgress.Invalidate()

        Application.DoEvents()

        If Me.selectedoptcase.type = OPTType.Minimization Then Return value + pen_val Else Return -(value + pen_val)

    End Function

    Sub FuncGrad_LBFGS(ByVal x As Double(), ByRef f As Double, ByRef g As Double())

        Dim varID(x.Length), objID(x.Length - 1), objProp(x.Length - 1), objName(x.Length - 1), FobjID, FobjProp As String
        Dim AvarID(x.Length), AobjID(Me.keysaux.Count), AobjProp(Me.keysaux.Count), AobjName(Me.keysaux.Count) As String
        Dim i, j As Integer
        Dim f0, f1, f2, f3, f4, x0, x1, x2, x3, x4 As Double

        For i = 1 To Me.keysind.Count
            objID(i) = Me.selectedoptcase.variables(Me.keysind(i - 1)).objectID
            objProp(i) = Me.selectedoptcase.variables(Me.keysind(i - 1)).propID
            objName(i) = Me.selectedoptcase.variables(Me.keysind(i - 1)).name
            varID(i) = Me.selectedoptcase.variables(Me.keysind(i - 1)).id
        Next

        For i = 1 To Me.keysaux.Count
            AobjID(i) = Me.selectedoptcase.variables(Me.keysaux(i - 1)).objectID
            AobjProp(i) = Me.selectedoptcase.variables(Me.keysaux(i - 1)).propID
            AobjName(i) = Me.selectedoptcase.variables(Me.keysaux(i - 1)).name
            AvarID(i) = Me.selectedoptcase.variables(Me.keysaux(i - 1)).id
        Next

        'penalty function value

        Dim pen_val As Double = 0

        'calculate gradients

        If Not Double.IsNaN(x(1)) Then

            Me.selectedoptcase.econtext = New ExpressionContext
            With Me.selectedoptcase.econtext
                .Imports.AddType(GetType(System.Math))
                For i = 1 To Me.keysind.Count
                    .Variables.Add(objName(i), SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x(i)))
                Next
                For i = 1 To Me.keysaux.Count
                    .Variables.Add(AobjName(i), SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(i)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(i - 1)).objectID).GetPropertyValue(AobjProp(i))))
                Next
            End With

            Dim h As Double = Me.selectedoptcase.epsilon

            For i = 1 To Me.keysind.Count
                x0 = x(i)
                x1 = x(i) * (1 - 2 * h)
                x2 = x(i) * (1 - h)
                x3 = x(i) * (1 + h)
                x4 = x(i) * (1 + 2 * h)
                If Me.selectedoptcase.numdevscheme = 2 Then
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x0)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x0)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x0)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x0)
                            For j = 1 To Me.keysaux.Count
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f0 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f0 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f0 = rx.GetPropertyValue(FobjProp.Split("|")(1))
                        Else
                            f0 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                End If
                If Me.selectedoptcase.numdevscheme = 4 Then
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x1)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x1)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x1)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x1)
                            For j = 1 To Me.keysaux.Count
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f1 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f1 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f1 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                        Else
                            f1 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x2)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x2)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x2)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x2)
                            For j = 1 To Me.keysaux.Count
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f2 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f2 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f2 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                        Else
                            f2 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                End If
                If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                    form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x3)
                    FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                ElseIf objID(i) = "ReactionProperty" Then
                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                    rx.SetPropertyValue(objProp(i).Split("|")(1), x3)
                    FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                Else
                    form.FormSpreadsheet.SetCellValue(objProp(i), x3)
                    FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                End If
                FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                UpdateVariablesValues()
                pen_val = ReturnPenaltyValue()
                If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                    With Me.selectedoptcase.econtext
                        .Imports.AddType(GetType(System.Math))
                        .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x3)
                        For j = 1 To Me.keysaux.Count
                            If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                            ElseIf AobjID(i) = "ReactionProperty" Then
                                Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                            Else
                                .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                            End If
                        Next
                        Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                    End With
                    f3 = Me.selectedoptcase.exbase.Evaluate + pen_val
                Else
                    FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                    FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                    If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                        f3 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                    ElseIf FobjID = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                        f3 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                    Else
                        f3 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                    End If
                End If
                If Me.selectedoptcase.numdevscheme = 4 Then
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x4)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x4)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x4)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x4)
                            For j = 1 To Me.keysaux.Count
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f4 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f4 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f4 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                        Else
                            f4 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                End If
                If Me.selectedoptcase.numdevscheme = 2 Then
                    g(i) = 1 / (h * x(i)) * (f3 - f0)
                Else
                    g(i) = 1 / (12 * h * x(i)) * (f1 - 8 * f2 + 8 * f3 - f4)
                End If
                If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                    form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x0)
                    FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                ElseIf objID(i) = "ReactionProperty" Then
                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                    rx.SetPropertyValue(objProp(i).Split("|")(1), x0)
                Else
                    form.FormSpreadsheet.SetCellValue(objProp(i), x0)
                    FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                End If
                UpdateVariablesValues()
            Next

            pen_val = ReturnPenaltyValue()

            If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                With Me.selectedoptcase.econtext
                    For i = 1 To Me.keysind.Count
                        .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x(i))
                    Next
                    For j = 1 To Me.keysaux.Count
                        If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                            .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                        ElseIf AobjID(i) = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                            .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                        Else
                            .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                        End If
                    Next
                    Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                End With
                Dim value As Double = Me.selectedoptcase.exbase.Evaluate
                If Me.selectedoptcase.type = OPTType.Minimization Then f = value + pen_val Else f = -(value + pen_val)
                'Me.selectedoptcase.results.Add(value)
            Else
                FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                Dim value As Double = 0.0#
                If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                    value = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                ElseIf FobjID = "ReactionProperty" Then
                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                    value = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                Else
                    value = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                End If
                If Me.selectedoptcase.type = OPTType.Minimization Then f = value + pen_val Else f = -(value + pen_val)
            End If

        End If

    End Sub

    Private Function FunctionValue(ByVal x() As Double) As Double

        Dim f As Double

        Application.DoEvents()

        If Me.abortCalc Then
            Throw New Exception("Optimization aborted")
        End If

        Dim varID(x.Length), objID(x.Length), objProp(x.Length), objName(x.Length), FobjID, FobjProp As String
        Dim AvarID(x.Length), AobjID(Me.keysaux.Count - 1), AobjProp(Me.keysaux.Count - 1), AobjName(Me.keysaux.Count - 1) As String
        Dim i As Integer

        For i = 0 To Me.keysind.Count - 1
            objID(i) = Me.selectedoptcase.variables(Me.keysind(i)).objectID
            objProp(i) = Me.selectedoptcase.variables(Me.keysind(i)).propID
            objName(i) = Me.selectedoptcase.variables(Me.keysind(i)).name
            varID(i) = Me.selectedoptcase.variables(Me.keysind(i)).id
        Next

        For i = 0 To Me.keysaux.Count - 1
            AobjID(i) = Me.selectedoptcase.variables(Me.keysaux(i)).objectID
            AobjProp(i) = Me.selectedoptcase.variables(Me.keysaux(i)).propID
            AobjName(i) = Me.selectedoptcase.variables(Me.keysaux(i)).name
            AvarID(i) = Me.selectedoptcase.variables(Me.keysaux(i)).id
        Next

        Me.selectedoptcase.econtext = New ExpressionContext
        With Me.selectedoptcase.econtext
            .Imports.AddType(GetType(System.Math))
            For i = 0 To Me.keysind.Count - 1
                .Variables.DefineVariable(objName(i), GetType(Double))
                .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x(i))
            Next
            For i = 0 To Me.keysaux.Count - 1
                .Variables.DefineVariable(AobjName(i), GetType(Double))
                .Variables(AobjName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(i)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(i)).objectID).GetPropertyValue(AobjProp(i)))
            Next
        End With

        Dim exceptions As New List(Of Exception)

        'penalty function value

        Dim pen_val As Double

        pen_val = ReturnPenaltyValue()
        For i = 0 To Me.keysind.Count - 1
            If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x(i))
                exceptions = FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
            ElseIf objID(i) = "ReactionProperty" Then
                Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                rx.SetPropertyValue(objProp(i).Split("|")(1), x(i))
            Else
                form.FormSpreadsheet.SetCellValue(objProp(i), x(i))
                exceptions = FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
            End If
            If exceptions.Count > 0 Then Throw New AggregateException(exceptions)
            UpdateVariablesValues()
        Next

        Dim value As Double
        If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
            Me.selectedoptcase.exbase = Me.selectedoptcase.econtext.CompileGeneric(Of Double)(Me.selectedoptcase.expression)
            value = Me.selectedoptcase.exbase.Evaluate
        Else
            FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
            FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
            If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                value = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp)
            ElseIf FobjID = "ReactionProperty" Then
                Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                value = rx.GetPropertyValue(FobjProp.Split("|")(1))
            Else
                value = form.FormSpreadsheet.GetCellValue(FobjProp).Data
            End If
        End If

        If Me.selectedoptcase.type = OPTType.Minimization Then
            f = value
            f += pen_val
        Else
            f = value
            f -= pen_val
        End If

        Me.tbCurrentValue.Text = value

        Application.DoEvents()

        Dim newx(x.Length) As Double

        For i = 1 To x.Length
            newx(i) = x(i - 1)
        Next

        NewIterUpdate(newx, f, Nothing, abortCalc)

        Return f

    End Function

    Private Function FunctionGradient(ByVal x() As Double) As Double()

        Dim g(x.Length - 1) As Double

        Application.DoEvents()

        If Me.abortCalc Then
            Throw New Exception("Optimization aborted")
        End If

        Dim varID(x.Length - 1), objID(x.Length - 1), objProp(x.Length - 1), objName(x.Length - 1), FobjID, FobjProp As String
        Dim AvarID(Me.keysaux.Count - 1), AobjID(Me.keysaux.Count - 1), AobjProp(Me.keysaux.Count - 1), AobjName(Me.keysaux.Count - 1) As String
        Dim i, j As Integer
        Dim f0, f1, f2, f3, f4, x0, x1, x2, x3, x4 As Double

        For i = 0 To Me.keysind.Count - 1
            objID(i) = Me.selectedoptcase.variables(Me.keysind(i)).objectID
            objProp(i) = Me.selectedoptcase.variables(Me.keysind(i)).propID
            objName(i) = Me.selectedoptcase.variables(Me.keysind(i)).name
            varID(i) = Me.selectedoptcase.variables(Me.keysind(i)).id
        Next

        For i = 0 To Me.keysaux.Count - 1
            AobjID(i) = Me.selectedoptcase.variables(Me.keysaux(i)).objectID
            AobjProp(i) = Me.selectedoptcase.variables(Me.keysaux(i)).propID
            AobjName(i) = Me.selectedoptcase.variables(Me.keysaux(i)).name
            AvarID(i) = Me.selectedoptcase.variables(Me.keysaux(i)).id
        Next

        'penalty function value

        Dim pen_val As Double = 0

        'calculate gradients

        If Not Double.IsNaN(x(0)) Then

            Me.selectedoptcase.econtext = New ExpressionContext
            With Me.selectedoptcase.econtext
                .Imports.AddType(GetType(System.Math))
                For i = 0 To Me.keysind.Count - 1
                    .Variables.Add(objName(i), SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x(i)))
                Next
                For i = 0 To Me.keysaux.Count - 1
                    .Variables.Add(AobjName(i), SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(i)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(i - 1)).objectID).GetPropertyValue(AobjProp(i))))
                Next
            End With

            Dim h As Double = Me.selectedoptcase.epsilon

            For i = 0 To Me.keysind.Count - 1
                If Me.abortCalc Then
                    Dim g2(x.Length - 1) As Double
                    Return g2
                End If
                x0 = x(i)
                x1 = x(i) * (1 - 2 * h)
                x2 = x(i) * (1 - h)
                x3 = x(i) * (1 + h)
                x4 = x(i) * (1 + 2 * h)
                If Me.selectedoptcase.numdevscheme = 2 Then
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x0)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x0)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x0)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x0)
                            For j = 0 To Me.keysaux.Count - 1
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f0 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f0 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f0 = rx.GetPropertyValue(FobjProp.Split("|")(1))
                        Else
                            f0 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                End If
                If Me.selectedoptcase.numdevscheme = 4 Then
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x1)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x1)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x1)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x1)
                            For j = 0 To Me.keysaux.Count - 1
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f1 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f1 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f1 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                        Else
                            f1 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x2)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x2)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x2)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x2)
                            For j = 0 To Me.keysaux.Count - 1
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f2 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f2 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f2 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                        Else
                            f2 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                End If
                If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                    form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x3)
                    FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                ElseIf objID(i) = "ReactionProperty" Then
                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                    rx.SetPropertyValue(objProp(i).Split("|")(1), x3)
                    FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                Else
                    form.FormSpreadsheet.SetCellValue(objProp(i), x3)
                    FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                End If
                FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                UpdateVariablesValues()
                pen_val = ReturnPenaltyValue()
                If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                    With Me.selectedoptcase.econtext
                        .Imports.AddType(GetType(System.Math))
                        .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x3)
                        For j = 0 To Me.keysaux.Count - 1
                            If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                            ElseIf AobjID(i) = "ReactionProperty" Then
                                Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                            Else
                                .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                            End If
                        Next
                        Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                    End With
                    f3 = Me.selectedoptcase.exbase.Evaluate + pen_val
                Else
                    FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                    FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                    If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                        f3 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                    ElseIf FobjID = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                        f3 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                    Else
                        f3 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                    End If
                End If
                If Me.selectedoptcase.numdevscheme = 4 Then
                    If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x4)
                        FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                    ElseIf objID(i) = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(objProp(i).Split("|")(1), x4)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    Else
                        form.FormSpreadsheet.SetCellValue(objProp(i), x4)
                        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                    End If
                    UpdateVariablesValues()
                    pen_val = ReturnPenaltyValue()
                    If Me.selectedoptcase.objfunctype = OPTObjectiveFunctionType.Expression Then
                        With Me.selectedoptcase.econtext
                            .Variables(objName(i)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(varID(i)).unit, x4)
                            For j = 0 To Me.keysaux.Count - 1
                                If AobjID(i) <> "SpreadsheetCell" And AobjID(i) <> "ReactionProperty" Then
                                    .Variables(AobjName(j)) = SystemsOfUnits.Converter.ConvertFromSI(Me.selectedoptcase.variables(AvarID(j)).unit, form.Collections.FlowsheetObjectCollection(Me.selectedoptcase.variables(Me.keysaux(j - 1)).objectID).GetPropertyValue(AobjProp(j)))
                                ElseIf AobjID(i) = "ReactionProperty" Then
                                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = AobjProp(j).Split("|")(0)).FirstOrDefault
                                    .Variables(AobjName(j)) = rx.GetPropertyValue(AobjProp(j).Split("|")(1))
                                Else
                                    .Variables(AobjName(j)) = form.FormSpreadsheet.GetCellValue(AobjProp(j)).Data
                                End If
                            Next
                            Me.selectedoptcase.exbase = .CompileGeneric(Of Double)(Me.selectedoptcase.expression)
                        End With
                        f4 = Me.selectedoptcase.exbase.Evaluate + pen_val
                    Else
                        FobjID = Me.selectedoptcase.variables(Me.keydep).objectID
                        FobjProp = Me.selectedoptcase.variables(Me.keydep).propID
                        If FobjID <> "SpreadsheetCell" And FobjID <> "ReactionProperty" Then
                            f4 = form.Collections.FlowsheetObjectCollection(FobjID).GetPropertyValue(FobjProp) + pen_val
                        ElseIf FobjID = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = FobjProp.Split("|")(0)).FirstOrDefault
                            f4 = rx.GetPropertyValue(FobjProp.Split("|")(1)) + pen_val
                        Else
                            f4 = form.FormSpreadsheet.GetCellValue(FobjProp).Data + pen_val
                        End If
                    End If
                End If
                If Me.selectedoptcase.numdevscheme = 2 Then
                    g(i) = 1 / (h * x(i)) * (f3 - f0)
                Else
                    g(i) = 1 / (12 * h * x(i)) * (f1 - 8 * f2 + 8 * f3 - f4)
                End If
                If objID(i) <> "SpreadsheetCell" And objID(i) <> "ReactionProperty" Then
                    form.Collections.FlowsheetObjectCollection(objID(i)).SetPropertyValue(objProp(i), x0)
                    FlowsheetSolver.FlowsheetSolver.CalculateObject(form, objID(i))
                ElseIf objID(i) = "ReactionProperty" Then
                    Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = objProp(i).Split("|")(0)).FirstOrDefault
                    rx.SetPropertyValue(objProp(i).Split("|")(1), x0)
                Else
                    form.FormSpreadsheet.SetCellValue(objProp(i), x0)
                    FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, My.Settings.SolverMode)
                End If
                UpdateVariablesValues()
            Next

        End If

        Return g

    End Function

    Private Sub UpdateVariablesValues()

        Dim var As OPTVariable

        For Each var In selectedoptcase.variables.Values
            If var.objectID <> "SpreadsheetCell" And var.objectID <> "ReactionProperty" Then
                var.currentvalue = form.Collections.FlowsheetObjectCollection(var.objectID).GetPropertyValue(var.propID)
            ElseIf var.objectID = "ReactionProperty" Then
                Dim rx = form.Reactions.Values.Where(Function(x_) x_.Name = var.propID.Split("|")(0)).FirstOrDefault
                var.currentvalue = rx.GetPropertyValue(var.propID.Split("|")(1))
            Else
                var.currentvalue = form.FormSpreadsheet.GetCellValue(var.propID).Data
            End If
        Next

        For Each row As DataGridViewRow In Me.dgVariables.Rows
            var = Me.selectedoptcase.variables(row.Cells(0).Value)
            row.Cells(8).Value = Format(SystemsOfUnits.Converter.ConvertFromSI(var.unit, var.currentvalue), nf)
        Next

        Application.DoEvents()

    End Sub

    Private Function ReturnPenaltyValue() As Double

        'calculate penalty functions for constraint variables

        Dim i As Integer

        Dim con_lc(keyscon.Count - 1), con_uc(keyscon.Count - 1), con_val(keyscon.Count - 1) As Double
        Dim con_lc2(keysind.Count - 1), con_uc2(keysind.Count - 1), con_val2(keysind.Count - 1) As Double
        Dim pen_val As Double = 0
        Dim delta1, delta2 As Double
        Dim pen_multpl As Double = Me.selectedoptcase.barriermultiplier

        If pen_multpl = 0 Then pen_multpl = 0.0001

        i = 0
        For Each id As String In Me.keyscon
            With selectedoptcase.variables(id)
                con_lc(i) = .lowerlimit / .upperlimit
                con_uc(i) = .upperlimit / .upperlimit
                con_val(i) = .currentvalue / .upperlimit
            End With
            i += 1
        Next

        i = 0
        pen_val = 0
        For Each id As String In Me.keyscon
            delta1 = con_val(i) - con_lc(i)
            delta2 = con_val(i) - con_uc(i)
            If delta1 < 0 Then
                pen_val += -delta1 * 100000000000
            ElseIf delta2 > 1 Then
                pen_val += -delta2 * 100000000000
            Else
                pen_val += 1 / delta1 - 1 / delta2
            End If
            i += 1
        Next

        'i = 0
        'For Each id As String In Me.keysind
        '    With selectedoptcase.variables(id)
        '        con_lc2(i) = .lowerlimit / .upperlimit
        '        con_uc2(i) = .upperlimit / .upperlimit
        '        con_val2(i) = .currentvalue / .upperlimit
        '    End With
        '    i += 1
        'Next

        'i = 0
        'For Each id As String In Me.keysind
        '    delta1 = con_val2(i) - con_lc2(i)
        '    delta2 = con_val2(i) - con_uc2(i)
        '    If delta1 < 0 Then
        '        pen_val += -delta1
        '    ElseIf delta2 > 1 Then
        '        pen_val += -delta2
        '    Else
        '        pen_val += 1 / delta1 - 1 / delta2
        '    End If
        '    i += 1
        'Next

        pen_val *= pen_multpl

        If Double.IsNaN(pen_val) Then pen_val = 0

        _penval = pen_val

        Return pen_val

    End Function

    Private Sub SaveForm(ByRef optcase As OptimizationCase, Optional ByVal OnlyVars As Boolean = False)

        If Me.selectedoptcase Is Nothing Then Me.selectedoptcase = form.Collections.OPT_OptimizationCollection(0)

        With Me.selectedoptcase

            If OnlyVars = False Then

                .description = Me.tbCaseDesc.Text
                .epsilon = Me.tbH.Text
                .epsF = Me.tbToleranceValue.Text
                .epsG = Me.tbToleranceValue.Text
                .epsX = Me.tbToleranceValue.Text
                .expression = Me.tbExpression.Text
                .maxits = Me.tbMaxIterations.Text
                .name = Me.tbCaseName.Text
                .barriermultiplier = Me.tbBarrierMultiplier.Text
                If Me.rb2PointDeriv.Checked Then
                    .numdevscheme = 2
                Else
                    .numdevscheme = 4
                End If
                If Me.rbExpression.Checked Then
                    .objfunctype = OPTObjectiveFunctionType.Expression
                Else
                    .objfunctype = OPTObjectiveFunctionType.Variable
                End If
                If Me.rbMaximize.Checked Then
                    .type = OPTType.Maximization
                Else
                    .type = OPTType.Minimization
                End If
                .tolerance = Me.tbToleranceValue.Text
                .solvm = Me.ComboBox1.SelectedIndex

            End If

            .variables.Clear()
            For Each dgrow As DataGridViewRow In Me.dgVariables.Rows
                Dim var As New OPTVariable
                With var
                    .id = dgrow.Cells(0).Value
                    .name = dgrow.Cells(1).Value
                    .objectTAG = dgrow.Cells(3).Value
                    If .objectTAG = DWSIM.App.GetLocalString("SpreadsheetCell") Then
                        .objectID = "SpreadsheetCell"
                        .propID = dgrow.Cells(4).Value
                    ElseIf .objectTAG = DWSIM.App.GetLocalString("ReactionProperty") Then
                        .objectID = "ReactionProperty"
                        .propID = dgrow.Cells(4).Value
                    Else
                        .objectID = Me.ReturnObject(dgrow.Cells(3).Value).Name
                        .propID = Me.ReturnPropertyID(.objectID, dgrow.Cells(4).Value)
                    End If
                    .lowerlimit = SystemsOfUnits.Converter.ConvertToSI(dgrow.Cells(9).Value, dgrow.Cells(5).Value)
                    .upperlimit = SystemsOfUnits.Converter.ConvertToSI(dgrow.Cells(9).Value, dgrow.Cells(6).Value)
                    .initialvalue = SystemsOfUnits.Converter.ConvertToSI(dgrow.Cells(9).Value, dgrow.Cells(7).Value)
                    .currentvalue = SystemsOfUnits.Converter.ConvertToSI(dgrow.Cells(9).Value, dgrow.Cells(8).Value)
                    Select Case dgrow.Cells(2).Value.ToString
                        Case "DEP"
                            .type = OPTVariableType.Dependent
                        Case "IND "
                            .type = OPTVariableType.Independent
                        Case "AUX"
                            .type = OPTVariableType.Auxiliary
                        Case "CON"
                            .type = OPTVariableType.Constraint
                    End Select
                    .unit = dgrow.Cells(9).Value
                    If Not dgrow.Cells(5).Value Is Nothing And Not dgrow.Cells(6).Value Is Nothing Then
                        .boundtype = BoundType.LowerAndUpper
                    ElseIf Not dgrow.Cells(5).Value Is Nothing Then
                        .boundtype = BoundType.Lower
                    ElseIf Not dgrow.Cells(6).Value Is Nothing Then
                        .boundtype = BoundType.Upper
                    Else
                        .boundtype = BoundType.None
                    End If
                End With
                .variables.Add(var.id, var)
            Next
        End With

    End Sub

    Private Sub PopulateForm(ByRef optcase As OptimizationCase)

        With optcase
            Me.tbCaseDesc.Text = .description
            Me.tbToleranceValue.Text = .tolerance
            Me.tbExpression.Text = .expression
            Me.tbMaxIterations.Text = .maxits
            Me.tbCaseName.Text = .name
            Me.tbBarrierMultiplier.Text = .barriermultiplier
            Me.tbH.Text = .epsilon
            If .numdevscheme = 2 Then rb2PointDeriv.Checked = True Else rb4PointDeriv.Checked = True
            If .objfunctype = OPTObjectiveFunctionType.Expression Then Me.rbExpression.Checked = True Else Me.rbVariable.Checked = True
            If .type = OPTType.Maximization Then Me.rbMaximize.Checked = True Else Me.rbMinimize.Checked = True
            Me.ComboBox1.SelectedIndex = .solvm

            Me.dgVariables.Rows.Clear()
            UpdateVariablesValues()
            For Each var As OPTVariable In .variables.Values
                With var
                    Me.dgVariables.Rows.Add()
                    Dim dgrow As DataGridViewRow = Me.dgVariables.Rows(Me.dgVariables.Rows.Count - 1)
                    dgrow.Cells(0).Value = .id
                    dgrow.Cells(1).Value = .name
                    If .objectID = "SpreadsheetCell" Then
                        dgrow.Cells(3).Value = DWSIM.App.GetLocalString(.objectID)
                        dgrow.Cells(4).Value = .propID
                    ElseIf .objectID = "ReactionProperty" Then
                        dgrow.Cells(3).Value = DWSIM.App.GetLocalString(.objectID)
                        dgrow.Cells(4).Value = .propID
                    Else
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(.objectID) Then
                            dgrow.Cells(3).Value = form.Collections.FlowsheetObjectCollection(.objectID).GraphicObject.Tag
                            dgrow.Cells(4).Value = form.GetTranslatedString1(.propID)
                        End If
                    End If
                    dgrow.Cells(9).Value = .unit
                    dgrow.Cells(5).Value = SystemsOfUnits.Converter.ConvertFromSI(.unit, .lowerlimit)
                    dgrow.Cells(6).Value = SystemsOfUnits.Converter.ConvertFromSI(.unit, .upperlimit)
                    dgrow.Cells(7).Value = SystemsOfUnits.Converter.ConvertFromSI(.unit, .initialvalue)
                    dgrow.Cells(8).Value = SystemsOfUnits.Converter.ConvertFromSI(.unit, .currentvalue)
                    Select Case .type
                        Case OPTVariableType.Dependent
                            dgrow.Cells(2).Value = "DEP"
                        Case OPTVariableType.Independent
                            dgrow.Cells(2).Value = "IND"
                        Case OPTVariableType.Auxiliary
                            dgrow.Cells(2).Value = "AUX"
                        Case OPTVariableType.Constraint
                            dgrow.Cells(2).Value = "CON"
                    End Select
                End With
            Next

            Me.SetupGraph()

            Dim curve As LineItem = Me.grProgress.GraphPane.CurveList(0)
            Dim list As IPointListEdit = curve.Points
            Dim i As Integer = 1
            For Each d As Double In .results
                list.Add(i, d)
                i += 1
            Next
            Me.grProgress.AxisChange()
            Me.grProgress.Invalidate()

        End With

    End Sub

    Private Sub btnClear_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClear.Click
        Me.tbExpression.Text = ""
    End Sub

    Private Sub btnVerify_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnVerify.Click
        Try
            SaveForm(selectedoptcase, True)
            Dim econtext As New ExpressionContext
            econtext.Imports.AddType(GetType(System.Math))
            For Each row As DataGridViewRow In Me.dgVariables.Rows
                If row.Cells(2).Value <> "DEP" Then
                    With econtext
                        .Variables.Add(row.Cells(1).Value, Convert.ToDouble(row.Cells(8).Value))
                    End With
                End If
            Next
            Dim exbase As IGenericExpression(Of Double) = econtext.CompileGeneric(Of Double)(Me.tbExpression.Text)
            Me.tbCurrentValue.Text = exbase.Evaluate
        Catch ex As Exception
            Me.tbCurrentValue.Text = ex.Message
        End Try

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAbort.Click
        abortCalc = True
    End Sub

    Private Sub NewIterUpdate(ByRef x As Double(), ByVal f As Double, ByRef g As Double(), ByRef abort As Boolean)

        Application.DoEvents()

        If Me.abortCalc Then
            abort = True
            Exit Sub
        End If

        If Me.selectedoptcase.type = OPTType.Minimization Then
            If Me.selectedoptcase.solvm <> OptimizationCase.SolvingMethod.IPOPT Then
                Me.selectedoptcase.results.Add(f - _penval)
            Else
                Me.selectedoptcase.results.Add(f)
            End If
        Else
            If Me.selectedoptcase.solvm <> OptimizationCase.SolvingMethod.IPOPT Then
                Me.selectedoptcase.results.Add(-(f - _penval))
            Else
                Me.selectedoptcase.results.Add(-f)
            End If
        End If

        form.WriteToLog("Optimization iteration #" & Me.selectedoptcase.results.Count & ", objective function value = " & Me.selectedoptcase.results(Me.selectedoptcase.results.Count - 1), Color.SeaGreen, MessageType.Information)

        Dim curve As LineItem = Me.grProgress.GraphPane.CurveList(0)
        Dim list As IPointListEdit = curve.Points
        list.Add(Me.selectedoptcase.results.Count, Me.selectedoptcase.results(Me.selectedoptcase.results.Count - 1))
        Me.grProgress.AxisChange()
        Me.grProgress.AxisChange()
        Me.grProgress.Invalidate()

        Application.DoEvents()

    End Sub

    Private Sub btnRestore_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRestore.Click

        Dim var As OPTVariable
        For Each var In Me.selectedoptcase.variables.Values
            form.Collections.FlowsheetObjectCollection(var.objectID).SetPropertyValue(var.propID, var.initialvalue)
        Next
        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, Settings.SolverMode)

    End Sub

    Sub SetupGraph()

        Dim myPane As GraphPane = Me.grProgress.GraphPane
        myPane.Title.Text = Me.selectedoptcase.name
        myPane.XAxis.Title.Text = DWSIM.App.GetLocalString("OPTIterationNo")
        myPane.YAxis.Title.Text = DWSIM.App.GetLocalString("OPTFunctionValue")
        myPane.CurveList.Clear()
        Dim list As New RollingPointPairList(Me.selectedoptcase.maxits)
        Dim curve As LineItem = myPane.AddCurve("", list, Color.SlateBlue, SymbolType.Diamond)
        With curve
            .Color = Color.SteelBlue
            .Line.IsSmooth = False
            .Symbol.Fill.Type = ZedGraph.FillType.Solid
        End With
        myPane.XAxis.Scale.Format = "###"

        With myPane
            .XAxis.Scale.FontSpec.Size = 16
            .YAxis.Scale.FontSpec.Size = 16
            .Title.FontSpec.Size = 20
            .XAxis.Title.FontSpec.Size = 18
            .YAxis.Title.FontSpec.Size = 18
        End With

        Me.grProgress.IsAutoScrollRange = True

    End Sub

    Private Sub dgVariables_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgVariables.DataError
        'My.Application.ActiveSimulation.WriteToLog(e.Exception.Message.ToString, Color.Red, MessageType.GeneralError)
    End Sub

    'IPOPT

    Public Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean
        Dim fval As Double = FunctionValue(x)
        obj_value = fval
        Return True
    End Function

    Public Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean
        Dim g As Double() = FunctionGradient(x)
        grad_f = g
        Return True
    End Function

    Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
        'g(0) = x(0) * x(1) * x(2) * x(3)
        'g(1) = x(0) * x(0) + x(1) * x(1) + x(2) * x(2) + x(3) * x(3)
        Return True
    End Function

    Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(), _
     ByRef jCol As Integer(), ByRef values As Double()) As Boolean
        If values Is Nothing Then
            ' set the structure of the jacobian 
            ' this particular jacobian is dense 
            'iRow(0) = 0
            'jCol(0) = 0
            'iRow(1) = 0
            'jCol(1) = 1
            'iRow(2) = 0
            'jCol(2) = 2
            'iRow(3) = 0
            'jCol(3) = 3
            'iRow(4) = 1
            'jCol(4) = 0
            'iRow(5) = 1
            'jCol(5) = 1
            'iRow(6) = 1
            'jCol(6) = 2
            'iRow(7) = 1
            'jCol(7) = 3
        Else
            '' return the values of the jacobian of the constraints 
            'values(0) = x(1) * x(2) * x(3)  ' 0,0 
            'values(1) = x(0) * x(2) * x(3)  ' 0,1 
            'values(2) = x(0) * x(1) * x(3)  ' 0,2 
            'values(3) = x(0) * x(1) * x(2)  ' 0,3 

            'values(4) = 2 * x(0)            ' 1,0 
            'values(5) = 2 * x(1)            ' 1,1 
            'values(6) = 2 * x(2)            ' 1,2 
            'values(7) = 2 * x(3)            ' 1,3 
        End If

        Return False
    End Function

    Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(), _
     ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean
        Return False
    End Function

    Private Sub FloatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FloatToolStripMenuItem.Click, DocumentToolStripMenuItem.Click,
                                                                             DockLeftToolStripMenuItem.Click, DockLeftAutoHideToolStripMenuItem.Click,
                                                                             DockRightAutoHideToolStripMenuItem.Click, DockRightToolStripMenuItem.Click,
                                                                             DockTopAutoHideToolStripMenuItem.Click, DockTopToolStripMenuItem.Click,
                                                                             DockBottomAutoHideToolStripMenuItem.Click, DockBottomToolStripMenuItem.Click

        For Each ts As ToolStripMenuItem In dckMenu.Items
            ts.Checked = False
        Next

        sender.Checked = True

        Select Case sender.Name
            Case "FloatToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
            Case "DocumentToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
            Case "DockLeftToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
            Case "DockLeftAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
            Case "DockRightAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
            Case "DockRightToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
            Case "DockBottomAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottomAutoHide
            Case "DockBottomToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
            Case "DockTopAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTopAutoHide
            Case "DockTopToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
            Case "HiddenToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Hidden
        End Select

    End Sub

    Private Sub tbCaseName_TextChanged(sender As Object, e As EventArgs) Handles tbCaseName.TextChanged
        If selected Then
            Me.lbCases.Items(Me.lbCases.SelectedIndex) = Me.tbCaseName.Text
            form.Collections.OPT_OptimizationCollection(Me.lbCases.SelectedIndex).name = Me.tbCaseName.Text
        End If
    End Sub

    Private Sub tbCaseDesc_TextChanged(sender As Object, e As EventArgs) Handles tbCaseDesc.TextChanged
        If selected Then
            form.Collections.OPT_OptimizationCollection(Me.lbCases.SelectedIndex).description = Me.tbCaseDesc.Text
        End If
    End Sub
End Class