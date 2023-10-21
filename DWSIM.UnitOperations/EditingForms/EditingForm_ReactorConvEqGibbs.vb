Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.ExtensionMethods

Public Class EditingForm_ReactorConvEqGibbs

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As Reactors.Reactor

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Dim eeditor As EditingForm_Gibbs_ElementMatrixEditor

    Private Sub EditingForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        ChangeDefaultFont()

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        UpdateGHGEditor(gbGHG, SimObject)

        If Host.Items.Where(Function(x) x.Name.Contains(SimObject.GraphicObject.Tag)).Count > 0 Then
            If InspReportBar Is Nothing Then
                InspReportBar = New SharedClasses.InspectorReportBar
                InspReportBar.Dock = DockStyle.Bottom
                AddHandler InspReportBar.Button1.Click, Sub()
                                                            Dim iwindow As New Inspector.Window2
                                                            iwindow.SelectedObject = SimObject
                                                            iwindow.Show(DockPanel)
                                                        End Sub
                Me.Controls.Add(InspReportBar)
                InspReportBar.BringToFront()
            End If
        Else
            If InspReportBar IsNot Nothing Then
                Me.Controls.Remove(InspReportBar)
                InspReportBar = Nothing
            End If
        End If

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            ToolTip1.SetToolTip(chkActive, .FlowSheet.GetTranslatedString("AtivoInativo"))

            Me.Text = .GraphicObject.Tag & " (" & .GetDisplayName() & ")"

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = System.Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = System.Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Erro")
                    lblStatus.ForeColor = System.Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = System.Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            'connections

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).OrderBy(Function(m) m).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            cbOutlet2.Items.Clear()
            cbOutlet2.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).OrderBy(Function(m) m).ToArray()

            cbEnergy.Items.Clear()
            cbEnergy.Items.AddRange(eslist)

            If TypeOf SimObject Is Reactors.Reactor_Conversion Then
                If .GraphicObject.OutputConnectors(2).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            Else
                If .GraphicObject.InputConnectors(1).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            End If


            'parameters

            cbTemp.Items.Clear()
            cbTemp.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbTemp.SelectedItem = units.temperature

            cbPDrop.Items.Clear()
            cbPDrop.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbPDrop.SelectedItem = units.deltaP

            Select Case .ReactorOperationMode
                Case Reactors.OperationMode.Isothermic
                    cbCalcMode.SelectedIndex = 0
                Case Reactors.OperationMode.Adiabatic
                    cbCalcMode.SelectedIndex = 1
                Case Reactors.OperationMode.OutletTemperature
                    cbCalcMode.SelectedIndex = 2
            End Select

            tbOutletTemperature.Text = su.Converter.ConvertFromSI(units.temperature, .OutletTemperature).ToString(nf)
            tbPDrop.Text = su.Converter.ConvertFromSI(units.deltaP, .DeltaP.GetValueOrDefault).ToString(nf)

            If TypeOf SimObject Is Reactors.Reactor_Gibbs Then

                If tabstrip1.TabPages.Contains(TabPage2) Then
                    tabstrip1.TabPages.Remove(TabPage2)
                End If

                chkGibbsUseAlternSolver.Checked = DirectCast(SimObject, Reactors.Reactor_Gibbs).AlternateSolvingMethod

                cbReacSet.Enabled = False

                'chkEnableDamping.Checked = DirectCast(SimObject, Reactors.Reactor_Gibbs).EnableDamping

                'txtDampingLowerLimit.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).DampingLowerLimit.ToString("G")
                'txtDampingUpperLimit.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).DampingUpperLimit.ToString("G")

                chkGibbsUsePreviousSolution.Checked = DirectCast(SimObject, Reactors.Reactor_Gibbs).InitializeFromPreviousSolution

                tbIntLoopMaxIts.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).MaximumInternalIterations
                tbIntLoopTol.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).InternalTolerance

                'tbNumDeriv.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).DerivativePerturbation

                'key compounds

                ListViewCompounds.Items.Clear()
                For Each comp In SimObject.FlowSheet.SelectedCompounds.Values
                    Dim lvi As New ListViewItem()
                    With lvi
                        .Text = comp.Name
                        .Tag = comp.Name
                        .Name = comp.Name
                    End With
                    ListViewCompounds.Items.Add(lvi)
                Next
                Me.ListViewCompounds.SelectedItems.Clear()

                For Each lvi As ListViewItem In Me.ListViewCompounds.Items
                    If DirectCast(SimObject, Reactors.Reactor_Gibbs).ComponentIDs.Contains(lvi.Tag) Then lvi.Checked = True
                Next

                'other editors

                TabPageElements.Controls.Clear()
                eeditor = New EditingForm_Gibbs_ElementMatrixEditor With {.gr = Me.SimObject}
                eeditor.Dock = DockStyle.Fill
                TabPageElements.Controls.Add(eeditor)

                TabControlParameters.TabPages.Remove(TabPageEqParams)

                LabelExternalSolver.Enabled = True
                cbExternalSolver.Enabled = True

                'external solvers

                cbExternalSolver.Items.Clear()
                cbExternalSolver.Items.Add("")
                For Each solver In .FlowSheet.ExternalSolvers.Values
                    If solver.Category = Enums.ExternalSolverCategory.NonLinearMinimization Then
                        cbExternalSolver.Items.Add(solver.DisplayText)
                    End If
                Next
                Dim selectedsolver = .FlowSheet.ExternalSolvers.Values.Where(Function(s) s.ID = .ExternalSolverID).FirstOrDefault()
                If selectedsolver IsNot Nothing Then
                    cbExternalSolver.SelectedItem = selectedsolver.DisplayText
                Else
                    cbExternalSolver.SelectedIndex = 0
                End If

                chkGibbsUseIPOPT.Checked = DirectCast(SimObject, Reactors.Reactor_Gibbs).UseIPOPTSolver

                cbReactivePhaseBehavior.SelectedIndex = DirectCast(SimObject, Reactors.Reactor_Gibbs).ReactivePhaseBehavior

            ElseIf TypeOf SimObject Is Reactors.Reactor_Equilibrium Then

                TabControlParameters.TabPages.Remove(TabPageCompounds)
                TabControlParameters.TabPages.Remove(TabPageElements)
                TabControlParameters.TabPages.Remove(TabPageGibbsParams)

                chkInitializeExtents.Checked = DirectCast(SimObject, Reactors.Reactor_Equilibrium).UsePreviousSolution

                tbExtLoopMaxItsEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopMaximumIterations
                tbIntLoopMaxItsEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopMaximumIterations
                tbExtLoopTolEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopTolerance
                tbIntLoopTolEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopTolerance

                'external solvers

                cbExternalSolver.Items.Clear()
                cbExternalSolver.Items.Add("")
                For Each solver In .FlowSheet.ExternalSolvers.Values
                    If solver.Category = Enums.ExternalSolverCategory.NonLinearSystem Then
                        cbExternalSolver.Items.Add(solver.DisplayText)
                    End If
                Next
                Dim selectedsolver = .FlowSheet.ExternalSolvers.Values.Where(Function(s) s.ID = .ExternalSolverID).FirstOrDefault()
                If selectedsolver IsNot Nothing Then
                    cbExternalSolver.SelectedItem = selectedsolver.DisplayText
                Else
                    cbExternalSolver.SelectedIndex = 0
                End If

            Else

                TabControlParameters.TabPages.Remove(TabPageCompounds)
                TabControlParameters.TabPages.Remove(TabPageElements)
                TabControlParameters.TabPages.Remove(TabPageGibbsParams)
                TabControlParameters.TabPages.Remove(TabPageEqParams)

                LabelExternalSolver.Enabled = False
                cbExternalSolver.Enabled = False

            End If

            Dim rsets As String() = .FlowSheet.ReactionSets.Values.Select(Function(m) m.Name).ToArray
            cbReacSet.Items.Clear()
            cbReacSet.Items.AddRange(rsets)

            Try
                If Not .FlowSheet.ReactionSets.ContainsKey(.ReactionSetID) Then .ReactionSetID = "DefaultSet"
                cbReacSet.SelectedItem = .FlowSheet.ReactionSets(.ReactionSetID).Name
            Catch ex As Exception
            End Try

            'results

            gridResults.Rows.Clear()
            gridReactions.Rows.Clear()

            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("DeltaT"), su.Converter.ConvertFromSI(units.deltaT, .DeltaT.GetValueOrDefault).ToString(nf), units.deltaT})
            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("RConvPGridItem3"), su.Converter.ConvertFromSI(units.heatflow, .DeltaQ.GetValueOrDefault).ToString(nf), units.heatflow})

            If TypeOf SimObject Is Reactors.Reactor_Gibbs Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Gibbs)

                gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("RGInitialG"), su.Converter.ConvertFromSI(units.heatflow, robj.InitialGibbsEnergy).ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("RGFinalG"), su.Converter.ConvertFromSI(units.heatflow, robj.FinalGibbsEnergy).ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("RGElementBalance"), robj.ElementBalance.ToString("E"), ""})

            ElseIf TypeOf SimObject Is Reactors.Reactor_Equilibrium Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Equilibrium)

                gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("RGInitialG"), su.Converter.ConvertFromSI(units.heatflow, robj.InitialGibbsEnergy).ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("RGFinalG"), su.Converter.ConvertFromSI(units.heatflow, robj.FinalGibbsEnergy).ToString(nf), units.heatflow})


            End If

            'reaction props

            If TypeOf SimObject Is Reactors.Reactor_Conversion Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Conversion)

                If Not robj.Conversions Is Nothing Then

                    For Each dbl As KeyValuePair(Of String, Double) In robj.Conversions
                        gridReactions.Rows.Add(New Object() { .FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionConversion"), (dbl.Value * 100).ToString(nf), "%"})
                    Next

                End If

            ElseIf TypeOf SimObject Is Reactors.Reactor_Equilibrium Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Equilibrium)

                If Not robj.ReactionExtents Is Nothing Then

                    For Each dbl As KeyValuePair(Of String, Double) In robj.ReactionExtents
                        gridReactions.Rows.Add(New Object() { .FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionCoordinate"), su.Converter.ConvertFromSI(units.molarflow, dbl.Value).ToString(nf), units.molarflow})
                    Next

                End If

            End If

            'conversions

            gridConversions.Rows.Clear()
            For Each dbl As KeyValuePair(Of String, Double) In .ComponentConversions
                If dbl.Value > 0.0# And Not Double.IsInfinity(dbl.Value) Then
                    gridConversions.Rows.Add(New Object() {dbl.Key, Format(dbl.Value * 100, nf)})
                End If
            Next

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

        End With

        cbExternalSolver.SetDropDownMaxWidth()

        Dim ssolver = SimObject.FlowSheet.ExternalSolvers.Values.Where(Function(s) s.ID = SimObject.ExternalSolverID).FirstOrDefault()
        If ssolver IsNot Nothing Then
            If TryCast(ssolver, IExternalSolverConfiguration) IsNot Nothing Then
                btnConfigExtSolver.Enabled = True
            Else
                btnConfigExtSolver.Enabled = False
            End If
        End If

        Loaded = True

    End Sub

    Private Sub ListViewCompounds_ItemChecked(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemCheckedEventArgs) Handles ListViewCompounds.ItemChecked
        If Loaded Then
            Dim robj = DirectCast(SimObject, Reactors.Reactor_Gibbs)
            For Each lvi As ListViewItem In Me.ListViewCompounds.Items
                If Not lvi Is Nothing Then
                    If lvi.Checked Then
                        If Not robj.ComponentIDs.Contains(lvi.Tag) Then
                            robj.ComponentIDs.Add(lvi.Tag)
                        End If
                    Else
                        If robj.ComponentIDs.Contains(lvi.Tag) Then
                            robj.ComponentIDs.Remove(lvi.Tag)
                        End If
                    End If
                End If
            Next
            eeditor.CreateMatrix()
            eeditor.SaveMatrix()
        End If
    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag =  cbPropPack.SelectedItem.ToString).FirstOrDefault()?.DisplayGroupedEditingForm()
    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click
        If cbInlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet1.Click
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectEnergy_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy.Click
        If cbEnergy.SelectedItem IsNot Nothing Then
            If TypeOf SimObject Is Reactors.Reactor_Conversion Then
                SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo)
            Else
                SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            End If

            cbEnergy.SelectedItem = Nothing
        End If
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation2(False)

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbInlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Else
                    Try
                        If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                        flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If
                UpdateInfo()
            End If

        End If

    End Sub

    Private Sub cbOutlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Else
                    Try
                        If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If
                UpdateInfo()
            End If

        End If

    End Sub

    Private Sub cbOutlet2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet2.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet2.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Else
                    Try
                        If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                        flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If
                UpdateInfo()
            End If

        End If

    End Sub

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If TypeOf SimObject Is Reactors.Reactor_Conversion Then
                    If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                        MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Else
                        Try
                            If gobj.OutputConnectors(2).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 2, 0)
                        Catch ex As Exception
                            MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        End Try
                    End If
                Else
                    If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                        MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Else
                        Try
                            If gobj.InputConnectors(1).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.InputConnectors(1).AttachedConnector.AttachedTo)
                            flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, 1)
                        Catch ex As Exception
                            MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        End Try
                    End If
                End If
                UpdateInfo()
            End If



            End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then
            SimObject.GraphicObject.Active = chkActive.Checked
            SimObject.FlowSheet.UpdateInterface()
            UpdateInfo()
        End If
    End Sub


    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbOutletTemperature.TextChanged, tbPDrop.TextChanged, tbIntLoopMaxIts.TextChanged, tbIntLoopTol.TextChanged, tbExtLoopMaxItsEq.TextChanged, tbIntLoopMaxItsEq.TextChanged,
        tbExtLoopTolEq.TextChanged, tbIntLoopTolEq.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbOutletTemperature.KeyDown, tbPDrop.KeyDown, tbIntLoopMaxIts.KeyDown, tbIntLoopTol.KeyDown, tbExtLoopMaxItsEq.KeyDown, tbIntLoopMaxItsEq.KeyDown,
        tbExtLoopTolEq.KeyDown, tbIntLoopTolEq.KeyDown


        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        If sender Is tbOutletTemperature Then SimObject.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text.ParseExpressionToDouble)
        If sender Is tbPDrop Then SimObject.DeltaP = su.Converter.ConvertToSI(cbPDrop.SelectedItem.ToString, tbPDrop.Text.ParseExpressionToDouble)
        If sender Is tbIntLoopMaxIts Then DirectCast(SimObject, Reactors.Reactor_Gibbs).MaximumInternalIterations = tbIntLoopMaxIts.Text.ParseExpressionToDouble
        If sender Is tbIntLoopTol Then DirectCast(SimObject, Reactors.Reactor_Gibbs).InternalTolerance = tbIntLoopTol.Text.ParseExpressionToDouble
        If sender Is tbExtLoopMaxItsEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopMaximumIterations = tbExtLoopMaxItsEq.Text.ParseExpressionToDouble
        If sender Is tbIntLoopMaxItsEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopMaximumIterations = tbIntLoopMaxItsEq.Text.ParseExpressionToDouble
        If sender Is tbExtLoopTolEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopTolerance = tbExtLoopTolEq.Text.ParseExpressionToDouble
        If sender Is tbIntLoopTolEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopTolerance = tbIntLoopTolEq.Text.ParseExpressionToDouble

        RequestCalc()

    End Sub

    Private Sub cbReacSet_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbReacSet.SelectedIndexChanged
        If Loaded Then
            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
            SimObject.ReactionSetID = SimObject.FlowSheet.ReactionSets.Values.Where(Function(x) x.Name = cbReacSet.SelectedItem.ToString).FirstOrDefault.ID
            RequestCalc()
        End If
    End Sub

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged

        If Loaded Then SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

        Select Case cbCalcMode.SelectedIndex
            Case 0
                tbOutletTemperature.Enabled = False
                cbTemp.Enabled = False
                SimObject.ReactorOperationMode = Reactors.OperationMode.Isothermic
            Case 1
                tbOutletTemperature.Enabled = False
                cbTemp.Enabled = False
                SimObject.ReactorOperationMode = Reactors.OperationMode.Adiabatic
            Case 2
                tbOutletTemperature.Enabled = True
                cbTemp.Enabled = True
                SimObject.ReactorOperationMode = Reactors.OperationMode.OutletTemperature
        End Select
        If Loaded Then RequestCalc()

    End Sub

    Private Sub btnCreateAndConnect_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click, btnCreateAndConnectOutlet2.Click, btnCreateAndConnectEnergy.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        If sender Is btnCreateAndConnectInlet1 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(0).Position.X - 50, sgobj.InputConnectors(0).Position.Y, "")

            If sgobj.InputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(0).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 0)

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(0).Position.X + 30, sgobj.OutputConnectors(0).Position.Y, "")

            If sgobj.OutputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(0).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

        ElseIf sender Is btnCreateAndConnectOutlet2 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(0).Position.X + 30, sgobj.OutputConnectors(1).Position.Y, "")

            If sgobj.OutputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(1).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 1, 0)

        ElseIf sender Is btnCreateAndConnectEnergy Then
            If TypeOf SimObject Is Reactors.Reactor_Conversion Then
                Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.OutputConnectors(0).Position.X + 30, sgobj.OutputConnectors(2).Position.Y + 40, "")

                If sgobj.OutputConnectors(2).IsAttached Then fs.DisconnectObjects(sgobj.OutputConnectors(2).AttachedConnector.AttachedFrom, sgobj)
                fs.ConnectObjects(sgobj, obj.GraphicObject, 2, 0)
            Else
                Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(0).Position.X - 50, sgobj.InputConnectors(1).Position.Y + 40, "")

                If sgobj.InputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(1).AttachedConnector.AttachedFrom, sgobj)
                fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 1)
            End If


        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub chkInitializeExtents_CheckedChanged(sender As Object, e As EventArgs) Handles chkInitializeExtents.CheckedChanged
        If TypeOf SimObject Is Reactors.Reactor_Equilibrium And Loaded Then
            DirectCast(SimObject, Reactors.Reactor_Equilibrium).UsePreviousSolution = chkInitializeExtents.Checked
        End If
    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout)

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

    Private Sub chkGibbsUsePreviousSolution_CheckedChanged(sender As Object, e As EventArgs) Handles chkGibbsUsePreviousSolution.CheckedChanged

        If Loaded Then SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
        If TypeOf SimObject Is Reactors.Reactor_Gibbs And Loaded Then
            DirectCast(SimObject, Reactors.Reactor_Gibbs).InitializeFromPreviousSolution = chkGibbsUsePreviousSolution.Checked
        End If

    End Sub

    Private Sub btnDisconnectOutlet2_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet2.Click
        If cbOutlet2.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo)
            cbOutlet2.SelectedItem = Nothing
        End If
    End Sub

    Private Sub cbExternalSolver_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbExternalSolver.SelectedIndexChanged
        If Loaded Then
            Dim selectedsolver = SimObject.FlowSheet.ExternalSolvers.Values.Where(
                Function(s) s.DisplayText = cbExternalSolver.SelectedItem.ToString()).FirstOrDefault()
            If selectedsolver IsNot Nothing Then
                SimObject.ExternalSolverID = selectedsolver.ID
                If TryCast(selectedsolver, IExternalSolverConfiguration) IsNot Nothing Then
                    btnConfigExtSolver.Enabled = True
                Else
                    btnConfigExtSolver.Enabled = False
                End If
            Else
                SimObject.ExternalSolverID = ""
                btnConfigExtSolver.Enabled = False
            End If
        End If
    End Sub

    Private Sub btnConfigExtSolver_Click(sender As Object, e As EventArgs) Handles btnConfigExtSolver.Click
        Try
            Dim selectedsolver = SimObject.FlowSheet.ExternalSolvers.Values.Where(
                Function(s) s.DisplayText = cbExternalSolver.SelectedItem.ToString()).FirstOrDefault()
            If TryCast(selectedsolver, IExternalSolverConfiguration) IsNot Nothing Then
                SimObject.ExternalSolverConfigData = DirectCast(selectedsolver, IExternalSolverConfiguration).Edit(SimObject.ExternalSolverConfigData)
            End If
        Catch ex As Exception
        End Try
    End Sub

    Private Sub chkGibbsUseIPOPT_CheckedChanged(sender As Object, e As EventArgs) Handles chkGibbsUseIPOPT.CheckedChanged

        If Loaded Then SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
        If TypeOf SimObject Is Reactors.Reactor_Gibbs And Loaded Then
            DirectCast(SimObject, Reactors.Reactor_Gibbs).UseIPOPTSolver = chkGibbsUseIPOPT.Checked
        End If

    End Sub

    Private Sub chkGibbsUseAlternSolver_CheckedChanged(sender As Object, e As EventArgs) Handles chkGibbsUseAlternSolver.CheckedChanged

        If Loaded Then SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
        If TypeOf SimObject Is Reactors.Reactor_Gibbs And Loaded Then
            DirectCast(SimObject, Reactors.Reactor_Gibbs).AlternateSolvingMethod = chkGibbsUseAlternSolver.Checked
        End If

    End Sub

    Private Sub cbReactivePhaseBehavior_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbReactivePhaseBehavior.SelectedIndexChanged

        If Loaded Then SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
        If TypeOf SimObject Is Reactors.Reactor_Gibbs And Loaded Then
            DirectCast(SimObject, Reactors.Reactor_Gibbs).ReactivePhaseBehavior = cbReactivePhaseBehavior.SelectedIndex
        End If

    End Sub
End Class