Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations

Public Class EditingForm_ReactorConvEqGibbs

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As Reactors.Reactor

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Dim eeditor As EditingForm_Gibbs_ElementMatrixEditor
    Dim ieditor As EditingForm_Gibbs_InitialEstimatesEditor

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

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
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
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

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            cbOutlet2.Items.Clear()
            cbOutlet2.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).ToArray

            cbEnergy.Items.Clear()
            cbEnergy.Items.AddRange(eslist)

            If .GraphicObject.InputConnectors(1).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag

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

                cbGibbsMinMode.Enabled = True
                cbGibbsMinMode.SelectedIndex = DirectCast(SimObject, Reactors.Reactor_Gibbs).SolvMethod

                chkEnableDamping.Checked = DirectCast(SimObject, Reactors.Reactor_Gibbs).EnableDamping

                txtDampingLowerLimit.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).DampingLowerLimit.ToString("G")
                txtDampingUpperLimit.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).DampingUpperLimit.ToString("G")

                tbExtLoopMaxIts.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).MaximumExternalIterations
                tbIntLoopMaxIts.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).MaximumInternalIterations
                tbExtLoopTol.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).ExternalTolerance
                tbIntLoopTol.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).InternalTolerance

                tbNumDeriv.Text = DirectCast(SimObject, Reactors.Reactor_Gibbs).DerivativePerturbation

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

                TabPageInitialEstimates.Controls.Clear()
                ieditor = New EditingForm_Gibbs_InitialEstimatesEditor With {.gr = Me.SimObject}
                ieditor.Dock = DockStyle.Fill
                TabPageInitialEstimates.Controls.Add(ieditor)

                TabControlParameters.TabPages.Remove(TabPageEqParams)

            ElseIf TypeOf SimObject Is Reactors.Reactor_Equilibrium Then

                TabControlParameters.TabPages.Remove(TabPageCompounds)
                TabControlParameters.TabPages.Remove(TabPageElements)
                TabControlParameters.TabPages.Remove(TabPageInitialEstimates)
                TabControlParameters.TabPages.Remove(TabPageGibbsParams)

                chkInitializeExtents.Checked = DirectCast(SimObject, Reactors.Reactor_Equilibrium).UsePreviousReactionExtents

                chkAlternateInit.Checked = DirectCast(SimObject, Reactors.Reactor_Equilibrium).AlternateBoundsInitializer

                tbExtentsInitializer.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).ReactionExtentsInitializer

                tbExtLoopMaxItsEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopMaximumIterations
                tbIntLoopMaxItsEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopMaximumIterations
                tbExtLoopTolEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopTolerance
                tbIntLoopTolEq.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopTolerance

                tbNumDeriv2.Text = DirectCast(SimObject, Reactors.Reactor_Equilibrium).DerivativePerturbation

            Else

                TabControlParameters.TabPages.Remove(TabPageCompounds)
                TabControlParameters.TabPages.Remove(TabPageElements)
                TabControlParameters.TabPages.Remove(TabPageInitialEstimates)
                TabControlParameters.TabPages.Remove(TabPageGibbsParams)
                TabControlParameters.TabPages.Remove(TabPageEqParams)

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

            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DeltaT"), su.Converter.ConvertFromSI(units.deltaT, .DeltaT.GetValueOrDefault).ToString(nf), units.deltaT})
            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RConvPGridItem3"), su.Converter.ConvertFromSI(units.heatflow, .DeltaQ.GetValueOrDefault).ToString(nf), units.heatflow})

            If TypeOf SimObject Is Reactors.Reactor_Gibbs Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Gibbs)

                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RGInitialG"), su.Converter.ConvertFromSI(units.heatflow, robj.InitialGibbsEnergy).ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RGFinalG"), su.Converter.ConvertFromSI(units.heatflow, robj.FinalGibbsEnergy).ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RGElementBalance"), robj.ElementBalance.ToString("E"), ""})

            ElseIf TypeOf SimObject Is Reactors.Reactor_Equilibrium Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Equilibrium)

                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RGInitialG"), su.Converter.ConvertFromSI(units.heatflow, robj.InitialGibbsEnergy).ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RGFinalG"), su.Converter.ConvertFromSI(units.heatflow, robj.FinalGibbsEnergy).ToString(nf), units.heatflow})


            End If

            'reaction props

            If TypeOf SimObject Is Reactors.Reactor_Conversion Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Conversion)

                If Not robj.Conversions Is Nothing Then

                    For Each dbl As KeyValuePair(Of String, Double) In robj.Conversions
                        gridReactions.Rows.Add(New Object() {.FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionConversion"), (dbl.Value * 100).ToString(nf), "%"})
                    Next

                End If

            ElseIf TypeOf SimObject Is Reactors.Reactor_Equilibrium Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Equilibrium)

                If Not robj.ReactionExtents Is Nothing Then

                    For Each dbl As KeyValuePair(Of String, Double) In robj.ReactionExtents
                        gridReactions.Rows.Add(New Object() { .FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionCoordinate"), su.Converter.ConvertFromSI(units.molarflow, dbl.Value).ToString(nf), units.molarflow})
                    Next

                End If

            ElseIf TypeOf SimObject Is Reactors.Reactor_Gibbs Then

                Dim robj = DirectCast(SimObject, Reactors.Reactor_Gibbs)

                If Not robj.ReactionExtents Is Nothing And robj.SolvMethod = Reactors.Reactor_Gibbs.SolvingMethod.ReactionExtents Then

                    For Each dbl As KeyValuePair(Of String, Double) In robj.ReactionExtents
                        gridReactions.Rows.Add(New Object() { .FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionCoordinate"), su.Converter.ConvertFromSI(units.molarflow, dbl.Value).ToString(nf), units.molarflow})
                    Next

                End If

            End If

            'conversions

            gridConversions.Rows.Clear()
            For Each dbl As KeyValuePair(Of String, Double) In .ComponentConversions
                If dbl.Value >= 0.0# Then
                    gridConversions.Rows.Add(New Object() {dbl.Key, Format(dbl.Value * 100, nf)})
                End If
            Next

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.Add("Default")
            cbFlashAlg.Items.AddRange(flashalgos)
            If .PreferredFlashAlgorithmTag <> "" Then cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag Else cbFlashAlg.SelectedIndex = 0

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

        End With

        Loaded = True

    End Sub

    Private Sub ListViewCompounds_ItemChecked(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemCheckedEventArgs) Handles ListViewCompounds.ItemChecked
        If Loaded Then
            Dim robj = DirectCast(SimObject, Reactors.Reactor_Gibbs)
            For Each lvi as ListViewItem In Me.ListViewCompounds.Items
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
            ieditor.GibbsInitialEstimatesEditorForm_Load(sender, e)
        End If
    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Thermodynamics.Calculator.ConfigureFlashInstance(SimObject, cbFlashAlg.SelectedItem.ToString)

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
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbEnergy.SelectedItem = Nothing
        End If
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            SimObject.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedItem.ToString
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
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

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
                    Exit Sub
                End If
                If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

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
                    Exit Sub
                End If
                If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

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

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If gobj.InputConnectors(1).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.InputConnectors(1).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, 1)

            End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub


    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbOutletTemperature.TextChanged, tbPDrop.TextChanged,
        txtDampingLowerLimit.TextChanged, txtDampingUpperLimit.TextChanged, tbExtLoopMaxIts.TextChanged, tbIntLoopMaxIts.TextChanged,
        tbExtLoopTol.TextChanged, tbIntLoopTol.TextChanged, tbExtLoopMaxItsEq.TextChanged, tbIntLoopMaxItsEq.TextChanged,
        tbExtLoopTolEq.TextChanged, tbIntLoopTolEq.TextChanged, tbExtentsInitializer.TextChanged, tbNumDeriv.TextChanged, tbNumDeriv2.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbOutletTemperature.KeyDown, tbPDrop.KeyDown,
        txtDampingLowerLimit.KeyDown, txtDampingUpperLimit.KeyDown, tbExtLoopMaxIts.KeyDown, tbIntLoopMaxIts.KeyDown,
        tbExtLoopTol.KeyDown, tbIntLoopTol.KeyDown, tbExtLoopMaxItsEq.KeyDown, tbIntLoopMaxItsEq.KeyDown,
        tbExtLoopTolEq.KeyDown, tbIntLoopTolEq.KeyDown, tbExtentsInitializer.KeyDown, tbNumDeriv.KeyDown, tbNumDeriv2.KeyDown


        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        If sender Is tbOutletTemperature Then SimObject.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text.ParseExpressionToDouble)
        If sender Is tbPDrop Then SimObject.DeltaP = su.Converter.ConvertToSI(cbPDrop.SelectedItem.ToString, tbPDrop.Text.ParseExpressionToDouble)
        If sender Is txtDampingLowerLimit Then DirectCast(SimObject, Reactors.Reactor_Gibbs).DampingLowerLimit = txtDampingLowerLimit.Text.ParseExpressionToDouble
        If sender Is txtDampingUpperLimit Then DirectCast(SimObject, Reactors.Reactor_Gibbs).DampingUpperLimit = txtDampingUpperLimit.Text.ParseExpressionToDouble
        If sender Is tbExtLoopMaxIts Then DirectCast(SimObject, Reactors.Reactor_Gibbs).MaximumExternalIterations = tbExtLoopMaxIts.Text.ParseExpressionToDouble
        If sender Is tbIntLoopMaxIts Then DirectCast(SimObject, Reactors.Reactor_Gibbs).MaximumInternalIterations = tbIntLoopMaxIts.Text.ParseExpressionToDouble
        If sender Is tbExtLoopTol Then DirectCast(SimObject, Reactors.Reactor_Gibbs).ExternalTolerance = tbExtLoopTol.Text.ParseExpressionToDouble
        If sender Is tbIntLoopTol Then DirectCast(SimObject, Reactors.Reactor_Gibbs).InternalTolerance = tbIntLoopTol.Text.ParseExpressionToDouble
        If sender Is tbExtLoopMaxItsEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopMaximumIterations = tbExtLoopMaxItsEq.Text.ParseExpressionToDouble
        If sender Is tbIntLoopMaxItsEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopMaximumIterations = tbIntLoopMaxItsEq.Text.ParseExpressionToDouble
        If sender Is tbExtLoopTolEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).ExternalLoopTolerance = tbExtLoopTolEq.Text.ParseExpressionToDouble
        If sender Is tbIntLoopTolEq Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).InternalLoopTolerance = tbIntLoopTolEq.Text.ParseExpressionToDouble
        If sender Is tbExtentsInitializer Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).ReactionExtentsInitializer = tbExtentsInitializer.Text.ParseExpressionToDouble
        If sender Is tbNumDeriv Then DirectCast(SimObject, Reactors.Reactor_Gibbs).DerivativePerturbation = tbNumDeriv.Text.ParseExpressionToDouble
        If sender Is tbNumDeriv2 Then DirectCast(SimObject, Reactors.Reactor_Equilibrium).DerivativePerturbation = tbNumDeriv2.Text.ParseExpressionToDouble

        RequestCalc()

    End Sub

    Private Sub cbReacSet_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbReacSet.SelectedIndexChanged
        If Loaded Then
            SimObject.ReactionSetID = SimObject.FlowSheet.ReactionSets.Values.Where(Function(x) x.Name = cbReacSet.SelectedItem.ToString).FirstOrDefault.ID
            RequestCalc()
        End If
    End Sub

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged

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

    Private Sub cbGibbsMinMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbGibbsMinMode.SelectedIndexChanged
        If Loaded Then
            Select Case cbGibbsMinMode.SelectedIndex
                Case 1
                    DirectCast(SimObject, Reactors.Reactor_Gibbs).SolvMethod = Reactors.Reactor_Gibbs.SolvingMethod.DirectMinimization
                Case 0
                    DirectCast(SimObject, Reactors.Reactor_Gibbs).SolvMethod = Reactors.Reactor_Gibbs.SolvingMethod.ReactionExtents
            End Select
            RequestCalc()
        End If
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click, btnCreateAndConnectOutlet2.Click, btnCreateAndConnectEnergy.Click

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

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(1).Position.X + 30, sgobj.OutputConnectors(1).Position.Y, "")

            If sgobj.OutputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(1).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 1, 0)

        ElseIf sender Is btnCreateAndConnectEnergy Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(1).Position.X - 30, sgobj.InputConnectors(1).Position.Y + 30, "")

            If sgobj.InputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(1).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 1)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub chkEnableDamping_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableDamping.CheckedChanged
        If TypeOf SimObject Is Reactors.Reactor_Gibbs Then
            DirectCast(SimObject, Reactors.Reactor_Gibbs).EnableDamping = chkEnableDamping.Checked
        End If
    End Sub

    Private Sub chkInitializeExtents_CheckedChanged(sender As Object, e As EventArgs) Handles chkInitializeExtents.CheckedChanged
        If TypeOf SimObject Is Reactors.Reactor_Equilibrium Then
            DirectCast(SimObject, Reactors.Reactor_Equilibrium).UsePreviousReactionExtents = chkInitializeExtents.Checked
            tbExtentsInitializer.Enabled = Not chkInitializeExtents.Checked
            RequestCalc()
        End If
    End Sub

    Private Sub chkAlternateInit_CheckedChanged(sender As Object, e As EventArgs) Handles chkAlternateInit.CheckedChanged
        If TypeOf SimObject Is Reactors.Reactor_Equilibrium Then
            DirectCast(SimObject, Reactors.Reactor_Equilibrium).AlternateBoundsInitializer = chkAlternateInit.Checked
            RequestCalc()
        End If
    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class