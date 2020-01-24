Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports System.Windows.Forms
Imports Converter = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports WeifenLuo.WinFormsUI.Docking
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class MaterialStreamEditor

    Inherits SharedClasses.ObjectEditorForm

    Public Property MatStream As Streams.MaterialStream

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf, nff As String

    Private committing As Boolean = False

    Private Sub MaterialStreamEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        UpdateInfo()

        'restore view state

        Dim vs As New Streams.Editors.MaterialStreamEditorState

        vs = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Streams.Editors.MaterialStreamEditorState)(MatStream.EditorState)

        With vs
            TabControlMain.SelectedIndex = .MainSelectedTab
            cbCompBasis.SelectedIndex = .InputCompositionBasis
            TabControlCompound.SelectedIndex = .CompoundsSelectedTab
            cbCalculatedAmountsBasis.SelectedIndex = .CompoundsAmountBasis
            cbCompoundPhaseProperties.SelectedIndex = .CompoundsProperty
            TabPhaseComps.SelectedIndex = .CompoundsAmountSelectedTab
            TabCompoundPhaseProps.SelectedIndex = .CompoundsPropertySelectedTab
            TabPhaseProps.SelectedIndex = .PhasePropsSelectedTab
            TabControlMain0.SelectedIndex = .MainSelectedTab0
        End With

        rtbAnnotations.ToolbarVisible = True

    End Sub

    Private Sub MaterialStreamEditor_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        SaveViewState()

    End Sub

    Sub UpdateInfo()

        units = MatStream.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = MatStream.FlowSheet.FlowsheetOptions.NumberFormat
        nff = MatStream.FlowSheet.FlowsheetOptions.FractionNumberFormat

        Loaded = False

        If Host.Items.Where(Function(x) x.Name.Contains(MatStream.GraphicObject.Tag)).Count > 0 Then
            If InspReportBar Is Nothing Then
                InspReportBar = New SharedClasses.InspectorReportBar
                InspReportBar.Dock = DockStyle.Bottom
                AddHandler InspReportBar.Button1.Click, Sub()
                                                            Dim iwindow As New Inspector.Window2
                                                            iwindow.SelectedObject = MatStream
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

        With MatStream

            'first block

            chkActive.Checked = MatStream.GraphicObject.Active

            Me.Text = .GraphicObject.Tag & " (" & .GetDisplayName() & ")"

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
                    lblStatus.ForeColor = Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            'connections

            Dim objlist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is SharedClasses.UnitOperations.BaseClass Or x.GraphicObject.ObjectType = ObjectType.OT_Recycle).Select(Function(m) m.GraphicObject.Tag).ToArray

            cbInlet.Items.Clear()
            cbInlet.Items.AddRange(objlist)

            cbOutlet.Items.Clear()
            cbOutlet.Items.AddRange(objlist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            'conditions

            cbSpec.SelectedIndex = .SpecType

            cbUnitsT.Items.Clear()
            cbUnitsT.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbUnitsT.SelectedItem = units.temperature

            cbUnitsP.Items.Clear()
            cbUnitsP.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbUnitsP.SelectedItem = units.pressure

            cbUnitsQ.Items.Clear()
            cbUnitsQ.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.volumetricFlow).ToArray)
            cbUnitsQ.SelectedItem = units.volumetricFlow

            cbUnitsW.Items.Clear()
            cbUnitsW.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.massflow).ToArray)
            cbUnitsW.SelectedItem = units.massflow

            cbUnitsM.Items.Clear()
            cbUnitsM.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molarflow).ToArray)
            cbUnitsM.SelectedItem = units.molarflow

            cbUnitsH.Items.Clear()
            cbUnitsH.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.enthalpy).ToArray)
            cbUnitsH.SelectedItem = units.enthalpy

            cbUnitsS.Items.Clear()
            cbUnitsS.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.entropy).ToArray)
            cbUnitsS.SelectedItem = units.entropy

            tbTemp.Text = su.Converter.ConvertFromSI(units.temperature, .Phases(0).Properties.temperature.GetValueOrDefault).ToString(nf)
            tbPressure.Text = su.Converter.ConvertFromSI(units.pressure, .Phases(0).Properties.pressure.GetValueOrDefault).ToString(nf)
            tbMassFlow.Text = su.Converter.ConvertFromSI(units.massflow, .Phases(0).Properties.massflow.GetValueOrDefault).ToString(nf)
            tbMoleFlow.Text = su.Converter.ConvertFromSI(units.molarflow, .Phases(0).Properties.molarflow.GetValueOrDefault).ToString(nf)
            tbVolFlow.Text = su.Converter.ConvertFromSI(units.volumetricFlow, .Phases(0).Properties.volumetric_flow.GetValueOrDefault).ToString(nf)
            tbEnth.Text = su.Converter.ConvertFromSI(units.enthalpy, .Phases(0).Properties.enthalpy.GetValueOrDefault).ToString(nf)
            tbEntr.Text = su.Converter.ConvertFromSI(units.entropy, .Phases(0).Properties.entropy.GetValueOrDefault).ToString(nf)

            tbFracSpec.Text = .Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(nf)

            'reference solvent

            Dim complist As List(Of String) = .FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToList

            complist.Insert(0, "")

            cbSolvent.Items.Clear()
            cbSolvent.Items.AddRange(complist.ToArray)

            cbSolvent.SelectedItem = MatStream.ReferenceSolvent

            'composition

            cbCompBasis.SelectedIndex = 0

            gridInputComposition.Rows.Clear()
            gridInputComposition.Columns(1).CellTemplate.Style.Format = nff

            For Each cp In .FlowSheet.SelectedCompounds.Values
                Dim comp = .Phases(0).Compounds(cp.Name)
                gridInputComposition.Rows(gridInputComposition.Rows.Add(New Object() {comp.Name, comp.MoleFraction.GetValueOrDefault})).Cells(0).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
            Next

            Dim sum As Double = 0.0#
            For Each row As DataGridViewRow In gridInputComposition.Rows
                sum += Double.Parse(row.Cells(1).Value)
            Next
            lblInputAmount.Text = "Total: " & sum.ToString(nf)
            Me.lblInputAmount.ForeColor = Drawing.Color.Blue

            'property package

            Try
                Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
                cbPropPack.Items.Clear()
                cbPropPack.Items.AddRange(proppacks)
                cbPropPack.SelectedItem = .PropertyPackage.Tag
            Catch ex As Exception
            End Try

            Try
                Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
                cbFlashAlg.Items.Clear()
                cbFlashAlg.Items.Add("Default")
                cbFlashAlg.Items.AddRange(flashalgos)
                If .PreferredFlashAlgorithmTag <> "" Then cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag Else cbFlashAlg.SelectedIndex = 0
            Catch ex As Exception
            End Try

            'compound amounts floating table

            cbFloatingTableCompoundAmountBasis.SelectedIndex = MatStream.FloatingTableAmountBasis

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            cbCalculatedAmountsBasis.SelectedIndex = 0

            cbCompoundPhaseProperties.SelectedIndex = 0

            If .Calculated Then

                If Not TabControlMain.TabPages.Contains(TabPageResultsComp) Then TabControlMain.TabPages.Add(TabPageResultsComp)
                If Not TabControlMain.TabPages.Contains(TabPageResultsProps) Then TabControlMain.TabPages.Add(TabPageResultsProps)

                'result compositions

                TabPhaseComps.TabPages.Clear()
                TabPhaseComps.TabPages.Add(tabCompMix)

                PopulateCompGrid(gridCompMixture, .Phases(0).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                If .Phases(2).Properties.molarfraction.HasValue Then
                    PopulateCompGrid(gridCompVapor, .Phases(2).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    TabPhaseComps.TabPages.Add(tabCompVapor)
                Else
                    TabPhaseComps.TabPages.Remove(tabCompVapor)
                End If
                If .Phases(3).Properties.molarfraction.GetValueOrDefault > 0.0# AndAlso .Phases(4).Properties.molarfraction.GetValueOrDefault > 0.0# Then
                    PopulateCompGrid(gridCompLiqMix, .Phases(1).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    TabPhaseComps.TabPages.Add(tabCompLiqMix)
                Else
                    TabPhaseComps.TabPages.Remove(tabCompLiqMix)
                End If
                If .Phases(3).Properties.molarfraction.HasValue Then
                    PopulateCompGrid(gridCompLiq1, .Phases(3).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    TabPhaseComps.TabPages.Add(tabCompLiq1)
                Else
                    TabPhaseComps.TabPages.Remove(tabCompLiq1)
                End If
                If .Phases(4).Properties.molarfraction.HasValue Then
                    PopulateCompGrid(gridCompLiq2, .Phases(4).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    TabPhaseComps.TabPages.Add(tabCompLiq2)
                Else
                    TabPhaseComps.TabPages.Remove(tabCompLiq2)
                End If
                If .Phases(7).Properties.molarfraction.HasValue Then
                    PopulateCompGrid(gridCompSolid, .Phases(7).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    TabPhaseComps.TabPages.Add(tabCompSolid)
                Else
                    TabPhaseComps.TabPages.Remove(tabCompSolid)
                End If

                'result compound properties

                TabCompoundPhaseProps.TabPages.Clear()

                If .Phases(2).Properties.molarfraction.HasValue Then
                    PopulateCompPropGrid(gridCompPropVapor, .Phases(2).Compounds.Values.ToList)
                    TabCompoundPhaseProps.TabPages.Add(TabCompPropVapor)
                Else
                    TabCompoundPhaseProps.TabPages.Remove(TabCompPropVapor)
                End If
                If .Phases(3).Properties.molarfraction.HasValue Then
                    PopulateCompPropGrid(gridCompPropLiq1, .Phases(3).Compounds.Values.ToList)
                    TabCompoundPhaseProps.TabPages.Add(TabCompPropLiq1)
                Else
                    TabCompoundPhaseProps.TabPages.Remove(TabCompPropLiq1)
                End If
                If .Phases(4).Properties.molarfraction.HasValue Then
                    PopulateCompPropGrid(gridCompPropLiq2, .Phases(4).Compounds.Values.ToList)
                    TabCompoundPhaseProps.TabPages.Add(TabCompPropLiq2)
                Else
                    TabCompoundPhaseProps.TabPages.Remove(TabCompPropLiq2)
                End If
                If .Phases(7).Properties.molarfraction.HasValue Then
                    PopulateCompPropGrid(gridCompPropSolid, .Phases(7).Compounds.Values.ToList)
                    TabCompoundPhaseProps.TabPages.Add(TabCompPropSolid)
                Else
                    TabCompoundPhaseProps.TabPages.Remove(TabCompPropSolid)
                End If

                'result properties

                TabPhaseProps.TabPages.Clear()
                TabPhaseProps.TabPages.Add(tabPropsMix)

                'MatStream.PropertyPackage.CurrentMaterialStream = MatStream

                PopulatePropGrid(gridPropertiesMixture, .Phases(0))
                If .Phases(2).Properties.molarfraction.HasValue Then
                    PopulatePropGrid(gridPropertiesVapor, .Phases(2))
                    TabPhaseProps.TabPages.Add(tabPropsVapor)
                Else
                    TabPhaseProps.TabPages.Remove(tabPropsVapor)
                End If
                If .Phases(1).Properties.molarfraction.GetValueOrDefault > 0.0# Then
                    PopulatePropGrid(gridPropertiesLiqMix, .Phases(1))
                    TabPhaseProps.TabPages.Add(tabPropsLiqMix)
                Else
                    TabPhaseProps.TabPages.Remove(tabPropsLiqMix)
                End If
                If .Phases(3).Properties.molarfraction.HasValue Then
                    PopulatePropGrid(gridPropertiesLiq1, .Phases(3))
                    TabPhaseProps.TabPages.Add(tabPropsLiq1)
                Else
                    TabPhaseProps.TabPages.Remove(tabPropsLiq1)
                End If
                If .Phases(4).Properties.molarfraction.HasValue Then
                    PopulatePropGrid(gridPropertiesLiq2, .Phases(4))
                    TabPhaseProps.TabPages.Add(tabPropsLiq2)
                Else
                    TabPhaseProps.TabPages.Remove(tabPropsLiq2)
                End If
                If .Phases(7).Properties.molarfraction.HasValue Then
                    PopulatePropGrid(gridPropertiesSolid, .Phases(7))
                    TabPhaseProps.TabPages.Add(tabPropsSolid)
                Else
                    TabPhaseProps.TabPages.Remove(tabPropsSolid)
                End If

            Else

                If TabControlMain.TabPages.Contains(TabPageResultsComp) Then TabControlMain.TabPages.Remove(TabPageResultsComp)
                If TabControlMain.TabPages.Contains(TabPageResultsProps) Then TabControlMain.TabPages.Remove(TabPageResultsProps)

            End If

            If .GraphicObject.InputConnectors(0).IsAttached Then
                If .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.OT_Recycle Then
                    UpdateEditableStatus()
                    tbMassFlow.Enabled = True
                    tbMoleFlow.Enabled = True
                    tbVolFlow.Enabled = True
                    TabPageInputComposition.Enabled = True
                Else
                    DisableEditableStatus()
                    tbMassFlow.Enabled = False
                    tbMoleFlow.Enabled = False
                    tbVolFlow.Enabled = False
                    TabPageInputComposition.Enabled = False
                End If
            Else
                UpdateEditableStatus()
                tbMassFlow.Enabled = True
                tbMoleFlow.Enabled = True
                tbVolFlow.Enabled = True
                TabPageInputComposition.Enabled = True
            End If

        End With

        Loaded = True

    End Sub

    Sub PopulateCompGrid(grid As DataGridView, complist As List(Of Interfaces.ICompound), amounttype As String)

        grid.ReadOnly = True
        grid.Rows.Clear()
        grid.Columns(1).CellTemplate.Style.Format = nff
        Select Case MatStream.FlowSheet.Options.CompoundOrderingMode
            Case Interfaces.Enums.CompoundOrdering.CAS_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.CAS_Number).ToList
            Case Interfaces.Enums.CompoundOrdering.CAS_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.CAS_Number).ToList
            Case Interfaces.Enums.CompoundOrdering.MW_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.Molar_Weight).ToList
            Case Interfaces.Enums.CompoundOrdering.MW_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.Molar_Weight).ToList
            Case Interfaces.Enums.CompoundOrdering.Name_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.Name).ToList
            Case Interfaces.Enums.CompoundOrdering.Name_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.Name).ToList
            Case Interfaces.Enums.CompoundOrdering.NBP_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.NBP.GetValueOrDefault).ToList
            Case Interfaces.Enums.CompoundOrdering.NBP_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.NBP.GetValueOrDefault).ToList
        End Select
        For Each comp In complist
            grid.Rows(grid.Rows.Add(New Object() {comp.Name, comp.MoleFraction.GetValueOrDefault})).Cells(0).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
        Next

    End Sub

    Sub PopulateCompPropGrid(grid As DataGridView, complist As List(Of Interfaces.ICompound))

        grid.ReadOnly = True
        grid.Rows.Clear()
        grid.Columns(1).CellTemplate.Style.Format = nf
        Select Case MatStream.FlowSheet.Options.CompoundOrderingMode
            Case Interfaces.Enums.CompoundOrdering.CAS_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.CAS_Number).ToList
            Case Interfaces.Enums.CompoundOrdering.CAS_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.CAS_Number).ToList
            Case Interfaces.Enums.CompoundOrdering.MW_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.Molar_Weight).ToList
            Case Interfaces.Enums.CompoundOrdering.MW_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.Molar_Weight).ToList
            Case Interfaces.Enums.CompoundOrdering.Name_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.Name).ToList
            Case Interfaces.Enums.CompoundOrdering.Name_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.Name).ToList
            Case Interfaces.Enums.CompoundOrdering.NBP_ASC
                complist = complist.OrderBy(Function(c) c.ConstantProperties.NBP.GetValueOrDefault).ToList
            Case Interfaces.Enums.CompoundOrdering.NBP_DESC
                complist = complist.OrderByDescending(Function(c) c.ConstantProperties.NBP.GetValueOrDefault).ToList
        End Select
        For Each comp In complist
            grid.Rows(grid.Rows.Add(New Object() {comp.Name, 0.0#})).Cells(0).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
        Next

    End Sub


    Sub PopulatePropGrid(grid As DataGridView, p As Interfaces.IPhase)

        grid.ReadOnly = True
        grid.Rows.Clear()
        grid.Columns(1).CellTemplate.Style.Format = nf

        Dim refval As Nullable(Of Double), val As Double

        With grid.Rows

            refval = p.Properties.massflow.GetValueOrDefault / Convert.ToDouble(p.Properties.density.GetValueOrDefault)
            If refval.HasValue Then val = Converter.ConvertFromSI(units.volumetricFlow, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("VazoTP"), val, units.volumetricFlow})
            refval = p.Properties.massflow.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.massflow, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Vazomssica"), val, units.massflow})
            refval = p.Properties.molarflow.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molarflow, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Vazomolar"), val, units.molarflow})

            If p.Name <> "Mixture" Then
                refval = p.Properties.molarfraction.GetValueOrDefault
                If refval.HasValue Then val = Format(refval, nf)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("FraomolardaPhase"), val})
                refval = p.Properties.massfraction.GetValueOrDefault
                If refval.HasValue Then val = refval
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("FraomssicadaPhase"), val})
                refval = p.Properties.compressibilityFactor.GetValueOrDefault
                If refval.HasValue Then val = refval
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("CompressibilityFactor"), val})
            End If

            refval = p.Properties.enthalpy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("EntalpiaEspecfica"), val, units.enthalpy})
            refval = p.Properties.molar_enthalpy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molar_enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("MolarEnthalpy"), val, units.molar_enthalpy})

            refval = p.Properties.entropy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.entropy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("EntropiaEspecfica"), val, units.entropy})
            refval = p.Properties.molar_entropy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molar_entropy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("MolarEntropy"), val, units.molar_entropy})

            refval = p.Properties.internal_energy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("InternalEnergy"), val, units.enthalpy})
            refval = p.Properties.molar_internal_energy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molar_enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("MolarInternalEnergy"), val, units.molar_enthalpy})

            refval = p.Properties.gibbs_free_energy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("GibbsEnergy"), val, units.enthalpy})
            refval = p.Properties.molar_gibbs_free_energy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molar_enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("MolarGibbsEnergy"), val, units.molar_enthalpy})

            refval = p.Properties.helmholtz_energy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("HelmholtzEnergy"), val, units.enthalpy})
            refval = p.Properties.molar_helmholtz_energy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molar_enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("MolarHelmholtzEnergy"), val, units.molar_enthalpy})

            refval = p.Properties.molecularWeight.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molecularWeight, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Massamolar"), val, units.molecularWeight})
            refval = p.Properties.density.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.density, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Massaespecfica"), val, units.density})

            refval = p.Properties.heatCapacityCp.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.heatCapacityCp, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("HeatCapacityCp"), val, units.heatCapacityCp})
            refval = p.Properties.heatCapacityCp.GetValueOrDefault / p.Properties.heatCapacityCv.GetValueOrDefault
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("HeatCapacityRatio"), refval.GetValueOrDefault})
            refval = p.Properties.thermalConductivity.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.thermalConductivity, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Condutividadetrmica"), val, units.thermalConductivity})

            If p.Name <> "Mixture" Then

                If Not p.Name.Contains("Overall") Then

                    refval = p.Properties.isothermal_compressibility.GetValueOrDefault
                    If refval.HasValue Then val = Converter.ConvertFromSI(units.compressibility, refval)
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("IsothermalCompressibility"), val, units.compressibility})

                    refval = p.Properties.bulk_modulus.GetValueOrDefault
                    If refval.HasValue Then val = Converter.ConvertFromSI(units.pressure, refval)
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("BulkModulus"), val, units.pressure})

                    refval = p.Properties.speedOfSound.GetValueOrDefault
                    If refval.HasValue Then val = Converter.ConvertFromSI(units.speedOfSound, refval)
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("SpeedOfSound"), val, units.speedOfSound})

                    refval = p.Properties.jouleThomsonCoefficient.GetValueOrDefault
                    If refval.HasValue Then val = Converter.ConvertFromSI(units.jouleThomsonCoefficient, refval)
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("JouleThomsonCoefficient"), val, units.jouleThomsonCoefficient})

                End If

                refval = p.Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue Then val = Converter.ConvertFromSI(units.cinematic_viscosity, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Viscosidadecinemtica"), val, units.cinematic_viscosity})
                refval = p.Properties.viscosity.GetValueOrDefault
                If refval.HasValue Then val = Converter.ConvertFromSI(units.viscosity, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Viscosidadedinmica"), val, units.viscosity})

            End If

            refval = p.Properties.bubblePressure
            If refval.HasValue Then
                val = Converter.ConvertFromSI(units.pressure, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("BubblePress"), val, units.pressure})
            End If
            refval = p.Properties.dewPressure
            If refval.HasValue Then
                val = Converter.ConvertFromSI(units.pressure, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("DewPress"), val, units.pressure})
            End If
            refval = p.Properties.bubbleTemperature
            If refval.HasValue Then
                val = Converter.ConvertFromSI(units.temperature, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("BubbleTemp"), val, units.pressure})
            End If
            refval = p.Properties.dewTemperature
            If refval.HasValue Then
                val = Converter.ConvertFromSI(units.temperature, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("DewTemp"), val, units.pressure})
            End If

            If p.Name.Contains("Overall") Then
                refval = MatStream.Phases(0).Properties.surfaceTension.GetValueOrDefault
                If refval.HasValue Then val = Converter.ConvertFromSI(units.surfaceTension, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Tensosuperficial"), val, units.surfaceTension})
            End If

            If p.Name = "Liquid1" Then

                If TypeOf MatStream.PropertyPackage Is PropertyPackages.SeawaterPropertyPackage Then

                    Dim water As BaseClasses.Compound = (From subst As BaseClasses.Compound In MatStream.Phases(3).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
                    Dim salt As BaseClasses.Compound = (From subst As BaseClasses.Compound In MatStream.Phases(3).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

                    Dim salinity As Double = salt.MassFraction.GetValueOrDefault / water.MassFraction.GetValueOrDefault
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Salinity"), salinity, ""})

                End If

                If TypeOf MatStream.PropertyPackage Is PropertyPackages.SourWaterPropertyPackage Then

                    refval = MatStream.Phases(3).Properties.pH.GetValueOrDefault
                    .Add(New Object() {"pH", refval, ""})

                End If

                If MatStream.PropertyPackage.IsElectrolytePP Then

                    refval = MatStream.Phases(3).Properties.pH.GetValueOrDefault
                    .Add(New Object() {"pH", refval, ""})

                    refval = MatStream.Phases(3).Properties.osmoticCoefficient.GetValueOrDefault
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("OsmoticCoefficient"), refval, ""})

                    refval = MatStream.Phases(3).Properties.freezingPoint.GetValueOrDefault
                    val = Converter.ConvertFromSI(units.temperature, refval)
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("FreezingPoint"), val, units.temperature})

                    refval = MatStream.Phases(3).Properties.freezingPointDepression.GetValueOrDefault
                    val = Converter.ConvertFromSI(units.deltaT, refval)
                    .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("FreezingPointDepression"), val, units.deltaT})

                End If

            End If

        End With

        For Each row As DataGridViewRow In grid.Rows
            row.Cells(0).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
            row.Cells(2).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
        Next

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Public Sub DisableEditableStatus()

        tbTemp.Enabled = False
        tbPressure.Enabled = False
        tbEnth.Enabled = False
        tbEntr.Enabled = False
        tbFracSpec.Enabled = False

    End Sub

    Public Sub UpdateEditableStatus()

        DisableEditableStatus()

        MatStream.SpecType = cbSpec.SelectedIndex

        Select Case cbSpec.SelectedIndex
            Case 0
                tbTemp.Enabled = True
                tbPressure.Enabled = True
            Case 1
                tbPressure.Enabled = True
                tbEnth.Enabled = True
            Case 2
                tbPressure.Enabled = True
                tbEntr.Enabled = True
            Case 3
                tbPressure.Enabled = True
                tbFracSpec.Enabled = True
            Case 4
                tbTemp.Enabled = True
                tbFracSpec.Enabled = True
            Case 5
                tbPressure.Enabled = True
                tbFracSpec.Enabled = True
        End Select

    End Sub


    Private Sub cbSpec_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSpec.SelectedIndexChanged

        UpdateEditableStatus()

        If Loaded Then RequestCalc()

    End Sub

    Private Sub btnNormalizeInput_Click(sender As Object, e As EventArgs) Handles btnNormalizeInput.Click
        Dim total As Double = 0.0#
        For Each row As DataGridViewRow In gridInputComposition.Rows
            total += row.Cells(1).Value
        Next
        For Each row As DataGridViewRow In gridInputComposition.Rows
            row.Cells(1).Value = row.Cells(1).Value / total
        Next
    End Sub

    Private Sub btnEqualizeInput_Click(sender As Object, e As EventArgs) Handles btnEqualizeInput.Click
        Dim total As Double = 0.0#
        For Each row As DataGridViewRow In gridInputComposition.Rows
            row.Cells(1).Value = 1.0# / gridInputComposition.Rows.Count
        Next
    End Sub

    Private Sub btnEraseInput_Click(sender As Object, e As EventArgs) Handles btnEraseInput.Click
        For Each row As DataGridViewRow In gridInputComposition.Rows
            row.Cells(1).Value = 0.0#
        Next
    End Sub

    Private Sub btnCompAcceptChanges_Click(sender As Object, e As EventArgs) Handles btnCompAcceptChanges.Click


        Dim W, Q As Double

        MatStream.PropertyPackage.CurrentMaterialStream = MatStream

        If Me.ValidateData() Then

            Dim mmtotal As Double = 0
            Dim mtotal As Double = 0

            Select Case cbCompBasis.SelectedIndex

                Case 0

                    committing = True
                    btnNormalizeInput_Click(sender, e)
                    committing = False
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                    Next

                Case 1

                    committing = True
                    btnNormalizeInput_Click(sender, e)
                    committing = False
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MassFraction = row.Cells(1).Value
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mmtotal += comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MoleFraction = comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / mmtotal
                    Next

                Case 2

                    Dim total As Double = 0
                    For Each row As DataGridViewRow In gridInputComposition.Rows
                        total += row.Cells(1).Value
                    Next
                    Q = Converter.ConvertToSI(units.molarflow, total)
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value / total
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next
                    W = 0
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                        W += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / 1000 * Q
                    Next
                    MatStream.Phases(0).Properties.molarflow = Q
                    MatStream.Phases(0).Properties.massflow = W

                Case 3

                    Dim total As Double = 0
                    For Each row As DataGridViewRow In gridInputComposition.Rows
                        total += row.Cells(1).Value
                    Next
                    W = Converter.ConvertToSI(units.massflow, total)
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MassFraction = row.Cells(1).Value / total
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mmtotal += comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight
                    Next
                    Q = 0
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MoleFraction = comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / mmtotal
                        Q += comp.MassFraction.GetValueOrDefault * W / comp.ConstantProperties.Molar_Weight * 1000
                    Next
                    MatStream.Phases(0).Properties.molarflow = Q
                    MatStream.Phases(0).Properties.massflow = W

                Case 5

                    Dim refsolv = cbSolvent.SelectedItem.ToString

                    MatStream.ReferenceSolvent = refsolv

                    Dim T As Double = MatStream.Phases(0).Properties.temperature.GetValueOrDefault

                    'molarity = mol solute per liter solution

                    Dim V = MatStream.Phases(0).Properties.volumetric_flow.GetValueOrDefault * 1000 'L

                    Dim comp As Interfaces.ICompound

                    Dim total As Double = 0
                    Dim vs As Double = 0.0#
                    Dim liqdens = 0.0#
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        If Not row.Cells(0).Value.ToString.Contains(refsolv) Then
                            comp = MatStream.Phases(0).Compounds(row.Cells(0).Value.ToString)
                            total += row.Cells(1).Value * V 'mol
                            liqdens = MatStream.PropertyPackage.AUX_LIQDENSi(comp, T)
                            vs += row.Cells(1).Value * V * comp.ConstantProperties.Molar_Weight / 1000 / liqdens * 1000
                        End If
                    Next
                    comp = MatStream.Phases(0).Compounds(refsolv)
                    Dim solvent_amount As Double = (V - vs) / 1000 * MatStream.PropertyPackage.AUX_LIQDENSi(comp, T) / comp.ConstantProperties.Molar_Weight * 1000 / V
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        If row.Cells(0).Value.ToString.Contains(refsolv) Then
                            row.Cells(1).Value = solvent_amount.ToString(nff)
                        End If
                    Next

                    total += solvent_amount * V
                    Q = total

                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value * V / total
                    Next

                    mtotal = 0.0#
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next

                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                    Next

                    MatStream.Phases(0).Properties.molarflow = Q
                    MatStream.Phases(0).Properties.massflow = Q / 1000 * MatStream.PropertyPackage.AUX_MMM(PropertyPackages.Phase.Mixture)

                Case 6

                    'molality = mol solute per kg solvent

                    W = MatStream.Phases(0).Properties.massflow.GetValueOrDefault

                    Dim refsolv = cbSolvent.SelectedItem.ToString

                    MatStream.ReferenceSolvent = refsolv

                    Dim comp As Interfaces.ICompound

                    Dim Ws As Double = 0
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        If Not row.Cells(0).Value.ToString.Contains(refsolv) Then
                            comp = MatStream.Phases(0).Compounds(row.Cells(0).Value.ToString)
                            Ws += row.Cells(1).Value * comp.ConstantProperties.Molar_Weight / 1000 'total kg solute / kg solvent
                        End If
                    Next
                    Dim solvent_amount As Double = W / (Ws + 1)
                    comp = MatStream.Phases(0).Compounds(refsolv)
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        If row.Cells(0).Value.ToString.Contains(refsolv) Then
                            row.Cells(1).Value = (1000 / comp.ConstantProperties.Molar_Weight).ToString(nff)
                        End If
                    Next

                    Q = 0.0#
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        Q += row.Cells(1).Value * solvent_amount
                    Next

                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value * solvent_amount / Q
                    Next

                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next

                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                    Next

                    MatStream.Phases(0).Properties.molarflow = Q

                Case 4

                    'liquid vol. frac
                    Dim n As Integer = MatStream.Phases(0).Compounds.Count
                    Dim liqdens(n - 1), nbp(n - 1), volfrac(n - 1), totalvol As Double
                    Dim ipp As New Thermodynamics.PropertyPackages.RaoultPropertyPackage()
                    ipp.CurrentMaterialStream = MatStream
                    Dim T As Double = 273.15 + 15.56 'standard temperature
                    Dim i As Integer = 0
                    totalvol = 0.0#
                    For Each s In MatStream.Phases(0).Compounds.Values
                        nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                        If T > nbp(i) Then
                            liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                        Else
                            liqdens(i) = ipp.AUX_LIQDENSi(s, T)
                        End If
                        i += 1
                    Next
                    mtotal = 0.0#
                    i = 0
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        mtotal += row.Cells(1).Value * liqdens(i)
                        i += 1
                    Next
                    i = 0
                    For Each row As DataGridViewRow In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MassFraction = row.Cells(1).Value * liqdens(i) / mtotal
                        i += 1
                    Next
                    mmtotal = 0.0#
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mmtotal += comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MoleFraction = comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / mmtotal
                    Next
                    ipp = Nothing

            End Select

            RequestCalc()

        End If

    End Sub

    Function ValidateData() As Boolean

        For Each row As DataGridViewRow In Me.gridInputComposition.Rows
            If Not Double.TryParse(row.Cells(1).Value, New Double) Then
                Return False
            End If
        Next
        Return True

    End Function

    Private Sub cbCompBasis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCompBasis.SelectedIndexChanged

        UpdateCompBasis(cbCompBasis, gridInputComposition, MatStream.Phases(0))

        If cbCompBasis.SelectedIndex = 5 Or cbCompBasis.SelectedIndex = 6 Then
            cbSolvent.Enabled = True
            For i = 0 To gridInputComposition.RowCount - 1
                If gridInputComposition.Rows(i).Cells(0).Value = MatStream.ReferenceSolvent Then
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = True
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
                ElseIf MatStream.ReferenceSolvent = "" Then
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = True
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
                Else
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Nothing
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = False
                End If

            Next
        Else
            cbSolvent.Enabled = False
            gridInputComposition.Columns(1).ReadOnly = False
            For i = 0 To gridInputComposition.RowCount - 1
                gridInputComposition.Rows(i).Cells(1).Style.BackColor = Nothing
            Next
        End If

    End Sub

    Sub UpdateCompBasis(cb As ComboBox, grid As DataGridView, phase As Interfaces.IPhase)

        Dim W, Q As Double, suffix As String = ""
        W = phase.Properties.massflow.GetValueOrDefault
        Q = phase.Properties.molarflow.GetValueOrDefault
        Select Case cb.SelectedIndex
            Case 0
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).MoleFraction.GetValueOrDefault
                Next
            Case 1
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).MassFraction.GetValueOrDefault
                Next
            Case 2
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = Converter.ConvertFromSI(units.molarflow, phase.Compounds(row.Cells(0).Value).MoleFraction.GetValueOrDefault * Q)
                Next
                suffix = units.molarflow
            Case 3
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = Converter.ConvertFromSI(units.massflow, phase.Compounds(row.Cells(0).Value).MassFraction.GetValueOrDefault * W)
                Next
                suffix = units.massflow
            Case 5
                'molarity = mol solute per liter solution
                Dim i As Integer = 0
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).Molarity.GetValueOrDefault / 1000
                    i += 1
                Next
                suffix = "mol/L"
            Case 6
                'molality = mol solute per kg solvent
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).Molality.GetValueOrDefault
                Next
                suffix = "mol/kg solv."
            Case 4
                'liquid vol. frac
                Dim n As Integer = phase.Compounds.Count
                Dim liqdens(n - 1), nbp(n - 1), volfrac(n - 1), totalvol As Double
                Dim ipp As New Thermodynamics.PropertyPackages.RaoultPropertyPackage()
                ipp.CurrentMaterialStream = MatStream
                Dim i As Integer = 0
                totalvol = 0.0#
                For Each s In phase.Compounds.Values
                    nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                    If 298.15 > nbp(i) Then
                        liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                    Else
                        liqdens(i) = ipp.AUX_LIQDENSi(s, 298.15)
                    End If
                    totalvol += s.MoleFraction.GetValueOrDefault * s.ConstantProperties.Molar_Weight / liqdens(i)
                    i += 1
                Next
                i = 0
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).MoleFraction.GetValueOrDefault * phase.Compounds(row.Cells(0).Value).ConstantProperties.Molar_Weight / liqdens(i) / totalvol
                    i += 1
                Next
                ipp = Nothing
        End Select

        Dim sum As Double = 0.0#
        For Each row As DataGridViewRow In grid.Rows
            sum += Double.Parse(row.Cells(1).Value)
        Next

        If cb Is cbCompBasis Then lblInputAmount.Text = "Total: " & sum.ToString(nf) & " " & suffix
        If cb Is cbCalculatedAmountsBasis Then lblAmountTotal.Text = suffix

    End Sub


    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbTemp.KeyDown, tbPressure.KeyDown, tbEnth.KeyDown, tbEntr.KeyDown,
                                                                            tbFracSpec.KeyDown, tbMassFlow.KeyDown, tbMoleFlow.KeyDown, tbVolFlow.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        Dim oldvalue, newvalue As Double, propname As String = ""

        If sender Is tbMassFlow Then
            MatStream.Phases(0).Properties.molarflow = Nothing
            MatStream.Phases(0).Properties.volumetric_flow = Nothing
        ElseIf sender Is tbMoleFlow Then
            MatStream.Phases(0).Properties.massflow = Nothing
            MatStream.Phases(0).Properties.volumetric_flow = Nothing
        ElseIf sender Is tbVolFlow Then
            MatStream.Phases(0).Properties.massflow = Nothing
            MatStream.Phases(0).Properties.molarflow = Nothing
        End If

        With MatStream.Phases(0).Properties

            If sender Is tbTemp Then
                oldvalue = .temperature.GetValueOrDefault
                .temperature = Converter.ConvertToSI(cbUnitsT.SelectedItem.ToString, tbTemp.Text.ParseExpressionToDouble)
                newvalue = .temperature.GetValueOrDefault
                propname = "PROP_MS_0"
            End If
            If sender Is tbPressure Then
                oldvalue = .pressure.GetValueOrDefault
                .pressure = Converter.ConvertToSI(cbUnitsP.SelectedItem.ToString, tbPressure.Text.ParseExpressionToDouble)
                newvalue = .pressure.GetValueOrDefault
                propname = "PROP_MS_1"
            End If
            If sender Is tbMassFlow Then
                oldvalue = .massflow.GetValueOrDefault
                .massflow = Converter.ConvertToSI(cbUnitsW.SelectedItem.ToString, tbMassFlow.Text.ParseExpressionToDouble)
                newvalue = .massflow.GetValueOrDefault
                propname = "PROP_MS_2"
            End If
            If sender Is tbMoleFlow Then
                oldvalue = .molarflow.GetValueOrDefault
                .molarflow = Converter.ConvertToSI(cbUnitsM.SelectedItem.ToString, tbMoleFlow.Text.ParseExpressionToDouble)
                newvalue = .molarflow.GetValueOrDefault
                propname = "PROP_MS_3"
            End If
            If sender Is tbVolFlow Then
                oldvalue = .volumetric_flow.GetValueOrDefault
                .volumetric_flow = Converter.ConvertToSI(cbUnitsQ.SelectedItem.ToString, tbVolFlow.Text.ParseExpressionToDouble)
                newvalue = .volumetric_flow.GetValueOrDefault
                propname = "PROP_MS_4"
            End If
            If sender Is tbEnth Then
                oldvalue = .enthalpy.GetValueOrDefault
                .enthalpy = Converter.ConvertToSI(cbUnitsH.SelectedItem.ToString, tbEnth.Text.ParseExpressionToDouble)
                newvalue = .enthalpy.GetValueOrDefault
                propname = "PROP_MS_7"
            End If
            If sender Is tbEntr Then
                oldvalue = .entropy.GetValueOrDefault
                .entropy = Converter.ConvertToSI(cbUnitsS.SelectedItem.ToString, tbEntr.Text.ParseExpressionToDouble)
                newvalue = .entropy.GetValueOrDefault
                propname = "PROP_MS_8"
            End If

        End With

        If sender Is tbFracSpec Then
            oldvalue = MatStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            MatStream.Phases(2).Properties.molarfraction = tbFracSpec.Text.ParseExpressionToDouble
            newvalue = MatStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            propname = "PROP_MS_27"
        End If

        MatStream.FlowSheet.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.SimulationObjectPropertyChanged,
                                                                .ObjID = MatStream.Name,
                                                                .OldValue = oldvalue,
                                                                .NewValue = newvalue,
                                                                .PropertyName = propname,
                                                                .Tag = MatStream.FlowSheet.FlowsheetOptions.SelectedUnitSystem,
                                                                .Name = String.Format(MatStream.FlowSheet.GetTranslatedString("UndoRedo_FlowsheetObjectPropertyChanged"), MatStream.GraphicObject.Tag, MatStream.FlowSheet.GetTranslatedString(.PropertyName), .OldValue, .NewValue)})

        RequestCalc()

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            MatStream.PropertyPackage = MatStream.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            MatStream.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedItem
            RequestCalc()
        End If
    End Sub

    Sub RequestCalc()

        SaveViewState()
        MatStream.FlowSheet.RequestCalculation(MatStream)

    End Sub

    Private Sub tbTemp_TextChanged(sender As Object, e As EventArgs) Handles tbTemp.TextChanged, tbPressure.TextChanged, tbEnth.TextChanged, tbEntr.TextChanged,
                                                                            tbFracSpec.TextChanged, tbMassFlow.TextChanged, tbMoleFlow.TextChanged, tbVolFlow.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = Drawing.Color.Blue
        Else
            tbox.ForeColor = Drawing.Color.Red
        End If

    End Sub

    Private Sub cbCalculatedAmountsBasis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalculatedAmountsBasis.SelectedIndexChanged

        UpdateCompBasis(cbCalculatedAmountsBasis, gridCompMixture, MatStream.Phases(0))
        UpdateCompBasis(cbCalculatedAmountsBasis, gridCompVapor, MatStream.Phases(2))
        UpdateCompBasis(cbCalculatedAmountsBasis, gridCompLiqMix, MatStream.Phases(1))
        UpdateCompBasis(cbCalculatedAmountsBasis, gridCompLiq1, MatStream.Phases(3))
        UpdateCompBasis(cbCalculatedAmountsBasis, gridCompLiq2, MatStream.Phases(4))
        UpdateCompBasis(cbCalculatedAmountsBasis, gridCompSolid, MatStream.Phases(7))

    End Sub

    Private Sub gridInputComposition_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridInputComposition.CellValueChanged

        If Loaded Then
            If e.ColumnIndex = 1 Then
                Try
                    Dim sum As Double = 0.0#
                    For Each row As DataGridViewRow In gridInputComposition.Rows
                        sum += Double.Parse(row.Cells(1).Value)
                    Next
                    lblInputAmount.Text = "Total: " & sum.ToString(nf)
                    Me.lblInputAmount.ForeColor = Drawing.Color.Blue
                Catch ex As Exception
                    Me.lblInputAmount.ForeColor = Drawing.Color.Red
                End Try

            End If
        End If

    End Sub
    Private Sub gridInputComposition_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles gridInputComposition.CellContentClick

        If e.ColumnIndex = 2 Then
            For i = 0 To gridInputComposition.RowCount - 1
                If i <> e.RowIndex Then
                    gridInputComposition.Rows(i).Cells(2).Value = False
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = False
                    gridInputComposition.Rows(i).Cells(2).ReadOnly = False
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Nothing
                Else
                    'gridInputComposition.Rows(i).Cells(2).Value = True
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = True
                    gridInputComposition.Rows(i).Cells(2).ReadOnly = True
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
                    MatStream.ReferenceSolvent = gridInputComposition.Rows(i).Cells(0).Value
                End If
            Next
        End If
    End Sub

    Private Sub cbInlet_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInlet.SelectedIndexChanged
        If Loaded Then UpdateInletConnection(sender)
    End Sub

    Private Sub cbOutlet_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet.SelectedIndexChanged
        If Loaded Then UpdateOutletConnection(sender)
    End Sub

    Private Sub btnDisconnectI_Click(sender As Object, e As EventArgs) Handles btnDisconnectI.Click

        If cbInlet.SelectedItem IsNot Nothing Then
            MatStream.FlowSheet.DisconnectObjects(MatStream.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, MatStream.GraphicObject)
            cbInlet.SelectedItem = Nothing
        End If

    End Sub

    Private Sub btnDisconnectO_Click(sender As Object, e As EventArgs) Handles btnDisconnectO.Click

        If cbOutlet.SelectedItem IsNot Nothing Then
            MatStream.FlowSheet.DisconnectObjects(MatStream.GraphicObject, MatStream.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet.SelectedItem = Nothing
        End If

    End Sub

    Sub UpdateInletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim gobj = MatStream.GraphicObject
            Dim flowsheet = MatStream.FlowSheet

            Dim i As Integer = 0
            For Each oc As Interfaces.IConnectionPoint In flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors
                If Not oc.IsAttached Then
                    If gobj.InputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(0).AttachedConnector.AttachedFrom, gobj)
                    flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, i, 0)
                    Exit Sub
                End If
                i += 1
            Next

            MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End If

    End Sub

    Sub UpdateOutletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim gobj = MatStream.GraphicObject
            Dim flowsheet = MatStream.FlowSheet

            Dim i As Integer = 0
            For Each ic As Interfaces.IConnectionPoint In flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors
                If Not ic.IsAttached Then
                    If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                    flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, i)
                    Exit Sub
                End If
                i += 1
            Next

            MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End If

    End Sub

    Private Sub cbUnitsT_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbUnitsT.SelectedIndexChanged, cbUnitsP.SelectedIndexChanged,
                                                                                        cbUnitsW.SelectedIndexChanged, cbUnitsM.SelectedIndexChanged,
                                                                                        cbUnitsQ.SelectedIndexChanged, cbUnitsH.SelectedIndexChanged,
                                                                                        cbUnitsS.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbUnitsT Then
                    tbTemp.Text = Converter.Convert(cbUnitsT.SelectedItem.ToString, units.temperature, Double.Parse(tbTemp.Text)).ToString(nf)
                    cbUnitsT.SelectedItem = units.temperature
                    UpdateProps(tbTemp)
                ElseIf sender Is cbUnitsP Then
                    tbPressure.Text = Converter.Convert(cbUnitsP.SelectedItem.ToString, units.pressure, Double.Parse(tbPressure.Text)).ToString(nf)
                    cbUnitsP.SelectedItem = units.pressure
                    UpdateProps(tbPressure)
                ElseIf sender Is cbUnitsW Then
                    tbMassFlow.Text = Converter.Convert(cbUnitsW.SelectedItem.ToString, units.massflow, Double.Parse(tbMassFlow.Text)).ToString(nf)
                    cbUnitsW.SelectedItem = units.massflow
                    UpdateProps(tbMassFlow)
                ElseIf sender Is cbUnitsM Then
                    tbMoleFlow.Text = Converter.Convert(cbUnitsM.SelectedItem.ToString, units.molarflow, Double.Parse(tbMoleFlow.Text)).ToString(nf)
                    cbUnitsM.SelectedItem = units.molarflow
                    UpdateProps(tbMoleFlow)
                ElseIf sender Is cbUnitsQ Then
                    tbVolFlow.Text = Converter.Convert(cbUnitsQ.SelectedItem.ToString, units.volumetricFlow, Double.Parse(tbVolFlow.Text)).ToString(nf)
                    cbUnitsQ.SelectedItem = units.volumetricFlow
                    UpdateProps(tbVolFlow)
                ElseIf sender Is cbUnitsH Then
                    tbEnth.Text = Converter.Convert(cbUnitsH.SelectedItem.ToString, units.enthalpy, Double.Parse(tbEnth.Text)).ToString(nf)
                    cbUnitsH.SelectedItem = units.enthalpy
                    UpdateProps(tbEnth)
                ElseIf sender Is cbUnitsS Then
                    tbEntr.Text = Converter.Convert(cbUnitsS.SelectedItem.ToString, units.entropy, Double.Parse(tbEntr.Text)).ToString(nf)
                    cbUnitsS.SelectedItem = units.entropy
                    UpdateProps(tbEntr)
                End If

            Catch ex As Exception
                MatStream.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles btnUtils.Click
        UtilitiesCtxMenu.Show(btnUtils, New Drawing.Point(20, 0))
    End Sub

    Private Sub DiagramaDeFasesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DiagramaDeFasesToolStripMenuItem.Click, BinaryTSMI.Click, TernaryTSMI.Click,
                                                                                                PetroleumPropsTSMI.Click, HydratesTSMI.Click, TCPTSMI.Click

        Dim utility As Interfaces.IAttachedUtility = Nothing

        If sender Is DiagramaDeFasesToolStripMenuItem Then
            utility = MatStream.FlowSheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PhaseEnvelope)
            utility.Name = "PhaseEnvelope" & (MatStream.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PhaseEnvelope).Count + 1).ToString
        ElseIf sender Is BinaryTSMI Then
            utility = MatStream.FlowSheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeBinary)
            utility.Name = "BinaryEnvelope" & (MatStream.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeBinary).Count + 1).ToString
        ElseIf sender Is TernaryTSMI Then
            utility = MatStream.FlowSheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeTernary)
            utility.Name = "TernaryEnvelope" & (MatStream.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeTernary).Count + 1).ToString
        ElseIf sender Is PetroleumPropsTSMI Then
            utility = MatStream.FlowSheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PetroleumProperties)
            utility.Name = "PetroleumProperties" & (MatStream.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PetroleumProperties).Count + 1).ToString
        ElseIf sender Is HydratesTSMI Then
            utility = MatStream.FlowSheet.GetUtility(Interfaces.Enums.FlowsheetUtility.NaturalGasHydrates)
            utility.Name = "NaturalGasHydrates" & (MatStream.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.NaturalGasHydrates).Count + 1).ToString
        ElseIf sender Is TCPTSMI Then
            utility = MatStream.FlowSheet.GetUtility(Interfaces.Enums.FlowsheetUtility.TrueCriticalPoint)
            utility.Name = "TrueCriticalPoint" & (MatStream.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.TrueCriticalPoint).Count + 1).ToString
        End If

        utility.AttachedTo = MatStream

        With DirectCast(utility, DockContent)
            .ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Float
        End With

        MatStream.AttachedUtilities.Add(utility)
        MatStream.FlowSheet.DisplayForm(utility)

        AddHandler DirectCast(utility, DockContent).FormClosed, Sub()
                                                                    MatStream.AttachedUtilities.Remove(utility)
                                                                    utility.AttachedTo = Nothing
                                                                End Sub

    End Sub

    Private Sub UtilitiesCtxMenu_Opening(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles UtilitiesCtxMenu.Opening

        For Each item In MatStream.AttachedUtilities
            Dim ts As New ToolStripMenuItem(item.Name)
            AddHandler ts.Click, Sub()
                                     Dim f = DirectCast(item, DockContent)
                                     If f.Visible Then
                                         f.Select()
                                     Else
                                         MatStream.FlowSheet.DisplayForm(f)
                                     End If
                                 End Sub
            UtilitiesCtxMenu.Items.Add(ts)
            AddHandler UtilitiesCtxMenu.Closed, Sub() If UtilitiesCtxMenu.Items.Contains(ts) Then UtilitiesCtxMenu.Items.Remove(ts)
            AddHandler DirectCast(item, DockContent).FormClosed, Sub()
                                                                     MatStream.AttachedUtilities.Remove(item)
                                                                     item.AttachedTo = Nothing
                                                                 End Sub
        Next

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        MatStream.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).FirstOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Thermodynamics.Calculator.ConfigureFlashInstance(MatStream, cbFlashAlg.SelectedItem.ToString)

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then MatStream.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then MatStream.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub gridInputComposition_KeyDown(sender As Object, e As KeyEventArgs) Handles gridInputComposition.KeyDown
        If e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then PasteData(gridInputComposition)
    End Sub

    Private Sub rbSpecVapor_CheckedChanged(sender As Object, e As EventArgs)

        If Loaded Then
            tbFracSpec.Text = MatStream.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(nf)
        End If

    End Sub

    Private Sub rtbAnnotations_Load(sender As Object, e As EventArgs) Handles rtbAnnotations.Load

    End Sub

    Private Sub cbCompoundPhaseProperties_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCompoundPhaseProperties.SelectedIndexChanged

        UpdateCompPropBasis(cbCompoundPhaseProperties, gridCompPropVapor, MatStream.Phases(2))
        UpdateCompPropBasis(cbCompoundPhaseProperties, gridCompPropLiq1, MatStream.Phases(3))
        UpdateCompPropBasis(cbCompoundPhaseProperties, gridCompPropLiq2, MatStream.Phases(4))
        UpdateCompPropBasis(cbCompoundPhaseProperties, gridCompPropSolid, MatStream.Phases(7))

    End Sub

    Sub UpdateCompPropBasis(cb As ComboBox, grid As DataGridView, phase As Interfaces.IPhase)


        Dim W, Q As Double, suffix As String = ""
        W = phase.Properties.massflow.GetValueOrDefault
        Q = phase.Properties.molarflow.GetValueOrDefault
        Select Case cb.SelectedIndex
            Case 0
                'Coeficientes de Fugacidade
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).FugacityCoeff.GetValueOrDefault
                Next
            Case 1
                'Log dos Coeficientes de Fugacidade
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = Math.Log(phase.Compounds(row.Cells(0).Value).FugacityCoeff.GetValueOrDefault)
                Next
            Case 2
                'Coeficientes de Atividade
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).ActivityCoeff.GetValueOrDefault
                Next
            Case 3
                'Pressões Parciais
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).PartialPressure.GetValueOrDefault
                Next
                suffix = units.pressure
            Case 4
                'Volumes Parciais
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).PartialVolume.GetValueOrDefault
                Next
                suffix = units.molar_volume
            Case 5
                'Coeficientes de Difusão
                For Each row As DataGridViewRow In grid.Rows
                    row.Cells(1).Value = phase.Compounds(row.Cells(0).Value).DiffusionCoefficient.GetValueOrDefault
                Next
                suffix = units.diffusivity
        End Select

        If cb Is cbCompoundPhaseProperties Then lblCompPropUnits.Text = suffix

    End Sub

    Private Sub cbSolvent_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSolvent.SelectedIndexChanged

        If Loaded Then

            MatStream.ReferenceSolvent = cbSolvent.SelectedItem.ToString

            UpdateCompBasis(cbCompBasis, gridInputComposition, MatStream.Phases(0))

            cbSolvent.Enabled = True
            For i = 0 To gridInputComposition.RowCount - 1
                If gridInputComposition.Rows(i).Cells(0).Value = MatStream.ReferenceSolvent Then
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = True
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
                ElseIf MatStream.ReferenceSolvent = "" Then
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = True
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
                Else
                    gridInputComposition.Rows(i).Cells(1).Style.BackColor = Nothing
                    gridInputComposition.Rows(i).Cells(1).ReadOnly = False
                End If

            Next

        End If

    End Sub

    Private Sub cbFloatingTableCompoundAmountBasis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFloatingTableCompoundAmountBasis.SelectedIndexChanged
        MatStream.FloatingTableAmountBasis = cbFloatingTableCompoundAmountBasis.SelectedIndex
    End Sub

    Private Sub TabPageInputConditions_MouseMove(sender As Object, e As MouseEventArgs) Handles TabPageInputConditions.MouseMove
        Me.Editor_MouseMove(sender, e)
    End Sub

    Private Sub SaveViewState()

        'save view state

        Dim vs As New Streams.Editors.MaterialStreamEditorState

        With vs
            .MainSelectedTab = TabControlMain.SelectedIndex
            .InputCompositionBasis = cbCompBasis.SelectedIndex
            .CompoundsSelectedTab = TabControlCompound.SelectedIndex
            .CompoundsAmountBasis = cbCalculatedAmountsBasis.SelectedIndex
            .CompoundsProperty = cbCompoundPhaseProperties.SelectedIndex
            .CompoundsAmountSelectedTab = TabPhaseComps.SelectedIndex
            .CompoundsPropertySelectedTab = TabCompoundPhaseProps.SelectedIndex
            .PhasePropsSelectedTab = TabPhaseProps.SelectedIndex
        End With

        MatStream.EditorState = Newtonsoft.Json.JsonConvert.SerializeObject(vs)

    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            If Loaded Then MatStream.GraphicObject.Tag = lblTag.Text
            If Loaded Then MatStream.FlowSheet.UpdateOpenEditForms()
            Me.Text = MatStream.GraphicObject.Tag & " (" & MatStream.GetDisplayName() & ")"
            DirectCast(MatStream.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class