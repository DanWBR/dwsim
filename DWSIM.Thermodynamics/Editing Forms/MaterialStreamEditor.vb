Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports System.Windows.Forms
Imports Converter = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class MaterialStreamEditor

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property MatStream As Streams.MaterialStream

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf, nff As String

    Private Sub MaterialStreamEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.Width = 427
        If Me.DockPanel IsNot Nothing Then Me.DockPanel.DockLeftPortion = 420

        units = MatStream.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = MatStream.FlowSheet.FlowsheetOptions.NumberFormat
        nff = MatStream.FlowSheet.FlowsheetOptions.FractionNumberFormat

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        Loaded = False

        With MatStream

            'first block

            chkActive.Checked = MatStream.GraphicObject.Active

            Me.Text = .GraphicObject.Tag

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
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

            Dim objlist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is DWSIM.SharedClasses.UnitOperations.UnitOpBaseClass).Select(Function(m) m.GraphicObject.Tag).ToArray

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
            cbUnitsT.SelectedItem = .FlowSheet.FlowsheetOptions.SelectedUnitSystem.temperature

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

            tbTemp.Text = .Phases(0).Properties.temperature.GetValueOrDefault.ToString(nf)
            tbPressure.Text = .Phases(0).Properties.pressure.GetValueOrDefault.ToString(nf)
            tbMassFlow.Text = .Phases(0).Properties.massflow.GetValueOrDefault.ToString(nf)
            tbMoleFlow.Text = .Phases(0).Properties.molarflow.GetValueOrDefault.ToString(nf)
            tbVolFlow.Text = .Phases(0).Properties.volumetric_flow.GetValueOrDefault.ToString(nf)
            tbEnth.Text = .Phases(0).Properties.enthalpy.GetValueOrDefault.ToString(nf)
            tbEntr.Text = .Phases(0).Properties.entropy.GetValueOrDefault.ToString(nf)

            If rbSpecVapor.Checked Then
                tbFracSpec.Text = .Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(nf)
            ElseIf rbSpecLiquid.Checked Then
                tbFracSpec.Text = .Phases(1).Properties.molarfraction.GetValueOrDefault.ToString(nf)
            Else
                tbFracSpec.Text = .Phases(7).Properties.molarfraction.GetValueOrDefault.ToString(nf)
            End If

            'composition

            cbCompBasis.SelectedIndex = 0

            gridInputComposition.Rows.Clear()
            gridInputComposition.Columns(1).CellTemplate.Style.Format = nff
            For Each comp In .Phases(0).Compounds.Values
                gridInputComposition.Rows(gridInputComposition.Rows.Add(New Object() {comp.Name, comp.MoleFraction})).Cells(0).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
            Next

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = [Enum].GetNames(.PreferredFlashAlgorithm.GetType)
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.AddRange(flashalgos)
            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithm.ToString

            'annotation

            rtbAnnotations.Rtf = .Annotation

            btnExpand.Enabled = .Calculated

            cbCalculatedAmountsBasis.SelectedIndex = 0

            If .Calculated Then

                'result compositions

                PopulateCompGrid(gridCompMixture, .Phases(0).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                If .Phases(1).Properties.molarfraction > 0.0# Then
                    PopulateCompGrid(gridCompLiqMix, .Phases(1).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    tabCompLiqMix.Hide()
                End If
                If .Phases(2).Properties.molarfraction > 0.0# Then
                    PopulateCompGrid(gridCompVapor, .Phases(2).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    tabCompVapor.Hide()
                End If
                If .Phases(3).Properties.molarfraction > 0.0# Then
                    PopulateCompGrid(gridCompLiq1, .Phases(3).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    tabCompLiq1.Hide()
                End If
                If .Phases(4).Properties.molarfraction > 0.0# Then
                    PopulateCompGrid(gridCompLiq2, .Phases(4).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    tabCompLiq2.Hide()
                End If
                If .Phases(7).Properties.molarfraction > 0.0# Then
                    PopulateCompGrid(gridCompSolid, .Phases(7).Compounds.Values.ToList, cbCalculatedAmountsBasis.SelectedItem.ToString)
                    tabCompSolid.Hide()
                End If

                'result properties

                PopulatePropGrid(gridPropertiesMixture, .Phases(0))
                If .Phases(1).Properties.molarfraction > 0.0# Then
                    PopulatePropGrid(gridPropertiesLiqMix, .Phases(1))
                    tabPropsLiqMix.Hide()
                End If
                If .Phases(2).Properties.molarfraction > 0.0# Then
                    PopulatePropGrid(gridPropertiesVapor, .Phases(2))
                    tabPropsVapor.Hide()
                End If
                If .Phases(3).Properties.molarfraction > 0.0# Then
                    PopulatePropGrid(gridPropertiesLiq1, .Phases(3))
                    tabPropsLiq1.Hide()
                End If
                If .Phases(4).Properties.molarfraction > 0.0# Then
                    PopulatePropGrid(gridPropertiesLiq2, .Phases(4))
                    tabPropsLiq2.Hide()
                End If
                If .Phases(7).Properties.molarfraction > 0.0# Then
                    PopulatePropGrid(gridPropertiesSolid, .Phases(7))
                    tabPropsSolid.Hide()
                End If

            End If

        End With

        Loaded = True

    End Sub

    Sub PopulateCompGrid(grid As DataGridView, complist As List(Of Interfaces.ICompound), amounttype As String)

        grid.ReadOnly = True
        grid.Rows.Clear()
        grid.Columns(1).CellTemplate.Style.Format = nff
        For Each comp In complist
            grid.Rows(grid.Rows.Add(New Object() {comp.Name, comp.MoleFraction})).Cells(0).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
        Next

    End Sub

    Sub PopulatePropGrid(grid As DataGridView, p As Interfaces.IPhase)

        grid.ReadOnly = True
        grid.Rows.Clear()
        grid.Columns(1).CellTemplate.Style.Format = nff

        Dim refval As Nullable(Of Double), val As Double

        With grid.Rows

            refval = p.Properties.enthalpy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("EntalpiaEspecfica"), val, units.enthalpy})
            refval = p.Properties.entropy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.entropy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("EntropiaEspecfica"), val, units.entropy})
            refval = p.Properties.molar_enthalpy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molar_enthalpy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("MolarEnthalpy"), val, units.molar_enthalpy})
            refval = p.Properties.molar_entropy.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molar_entropy, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("MolarEntropy"), val, units.molar_entropy})
            refval = p.Properties.molecularWeight.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molecularWeight, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Massamolar"), val, units.molecularWeight})
            refval = p.Properties.density.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.density, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Massaespecfica"), val, units.density})
            refval = p.Properties.massflow.GetValueOrDefault / Convert.ToDouble(p.Properties.density.GetValueOrDefault)
            If refval.HasValue Then val = Converter.ConvertFromSI(units.volumetricFlow, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("VazoTP"), val, units.volumetricFlow})
            refval = p.Properties.massflow.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.massflow, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Vazomssica"), val, units.massflow})
            refval = p.Properties.molarflow.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.molarflow, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Vazomolar"), val, units.molarflow})
            refval = p.Properties.molarfraction.GetValueOrDefault
            If refval.HasValue Then val = Format(refval, nf)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("FraomolardaPhase"), val})
            refval = p.Properties.massfraction.GetValueOrDefault
            If refval.HasValue Then val = refval
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("FraomssicadaPhase"), val})
            refval = p.Properties.compressibilityFactor.GetValueOrDefault
            If refval.HasValue Then val = refval
            .Add(New Object() {"Z", val})
            refval = p.Properties.heatCapacityCp.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.heatCapacityCp, refval)
            .Add(New Object() {"Cp", val, units.heatCapacityCp})
            refval = p.Properties.heatCapacityCp.GetValueOrDefault / p.Properties.heatCapacityCv.GetValueOrDefault
            .Add(New Object() {"Cp/Cv", refval.GetValueOrDefault})
            refval = p.Properties.thermalConductivity.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.thermalConductivity, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Condutividadetrmica"), val, units.thermalConductivity})
            refval = p.Properties.kinematic_viscosity.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.cinematic_viscosity, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Viscosidadecinemtica"), val, units.cinematic_viscosity})
            refval = p.Properties.viscosity.GetValueOrDefault
            If refval.HasValue Then val = Converter.ConvertFromSI(units.viscosity, refval)
            .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("Viscosidadedinmica"), val, units.viscosity})

            If MatStream.FlowSheet.FlowsheetOptions.CalculateBubbleAndDewPoints And p.Name = "Mixture" Then
                refval = p.Properties.bubblePressure.GetValueOrDefault
                If refval.HasValue Then val = Converter.ConvertFromSI(units.pressure, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("BubblePress"), val, units.pressure})
                refval = p.Properties.dewPressure.GetValueOrDefault
                If refval.HasValue Then val = Converter.ConvertFromSI(units.pressure, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("DewPress"), val, units.pressure})
                refval = p.Properties.bubbleTemperature.GetValueOrDefault
                If refval.HasValue Then val = Converter.ConvertFromSI(units.temperature, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("BubbleTemp"), val, units.pressure})
                refval = p.Properties.dewTemperature.GetValueOrDefault
                If refval.HasValue Then val = Converter.ConvertFromSI(units.temperature, refval)
                .Add(New Object() {MatStream.FlowSheet.GetTranslatedString("DewTemp"), val, units.pressure})
            End If

        End With

        For Each row In grid.Rows
            row.Cells(0).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
            row.Cells(2).Style.BackColor = Drawing.Color.FromKnownColor(Drawing.KnownColor.Control)
        Next

    End Sub

    Private Sub btnExpand_CheckedChanged(sender As Object, e As EventArgs) Handles btnExpand.CheckedChanged
        If btnExpand.Checked Then Me.Width = 859 Else Me.Width = 427
        If Me.DockPanel IsNot Nothing Then
            If btnExpand.Checked Then Me.DockPanel.DockLeftPortion = 850 Else Me.DockPanel.DockLeftPortion = 414
        End If
    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then MatStream.GraphicObject.Tag = lblTag.Text
    End Sub

    Private Sub cbSpec_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSpec.SelectedIndexChanged

        tbTemp.Enabled = False
        tbPressure.Enabled = False
        tbMassFlow.Enabled = False
        tbMoleFlow.Enabled = False
        tbVolFlow.Enabled = False
        tbEnth.Enabled = False
        tbEntr.Enabled = False
        tbFracSpec.Enabled = False
        rbSpecVapor.Enabled = False
        rbSpecLiquid.Enabled = False
        rbSpecSolid.Enabled = False

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
                rbSpecVapor.Enabled = True
            Case 4
                tbTemp.Enabled = True
                tbFracSpec.Enabled = True
            Case 5
                tbPressure.Enabled = True
                tbFracSpec.Enabled = True
                rbSpecSolid.Enabled = True
        End Select

        If Loaded Then RequestCalc()

    End Sub

    Private Sub btnNormalizeInput_Click(sender As Object, e As EventArgs) Handles btnNormalizeInput.Click
        Dim total As Double = 0
        For Each row In gridInputComposition.Rows
            total += row.Cells(1).Value
        Next
        For Each row In gridInputComposition.Rows
            row.Cells(1).Value = row.Cells(1).Value / total
        Next
    End Sub

    Private Sub btnEqualizeInput_Click(sender As Object, e As EventArgs) Handles btnEqualizeInput.Click
        Dim total As Double = 0
        For Each row In gridInputComposition.Rows
            row.Cells(1).Value = 1 / gridInputComposition.Rows.Count
        Next
    End Sub

    Private Sub btnEraseInput_Click(sender As Object, e As EventArgs) Handles btnEraseInput.Click
        For Each row In gridInputComposition.Rows
            row.Cells(1).Value = 0
        Next
    End Sub

    Private Sub btnCompAcceptChanges_Click(sender As Object, e As EventArgs) Handles btnCompAcceptChanges.Click

        Dim W, Q As Double

        If Me.ValidateData() Then

            Dim mmtotal As Double = 0
            Dim mtotal As Double = 0

            Select Case cbCompBasis.SelectedIndex

                Case 0

                    btnNormalizeInput_Click(sender, e)
                    For Each row In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                    Next

                Case 1

                    btnNormalizeInput_Click(sender, e)
                    For Each row In Me.gridInputComposition.Rows
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
                    For Each row In gridInputComposition.Rows
                        total += row.Cells(1).Value
                    Next
                    Q = Converter.ConvertToSI(units.molarflow, total)
                    For Each row In Me.gridInputComposition.Rows
                        MatStream.Phases(0).Compounds(row.cells(0).value).MoleFraction = row.Cells(1).Value / total
                    Next
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next
                    W = 0
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                        W += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / 1000 * Q
                    Next

                Case 3

                    Dim total As Double = 0
                    For Each row In gridInputComposition.Rows
                        total += row.Cells(0).Value
                    Next
                    W = Converter.ConvertToSI(units.massflow, total)
                    For Each row In Me.gridInputComposition.Rows
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

                Case 5

                    'molarity = mol solute per liter solution
                    Dim n As Integer = MatStream.Phases(0).Compounds.Count
                    Dim liqdens(n - 1), nbp(n - 1) As Double
                    Dim ipp As New Thermodynamics.PropertyPackages.RaoultPropertyPackage()
                    ipp.CurrentMaterialStream = MatStream
                    Dim i As Integer = 0
                    For Each s In MatStream.Phases(0).Compounds.Values
                        nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                        If 298.15 > nbp(i) Then
                            liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                        Else
                            liqdens(i) = ipp.AUX_LIQDENSi(s, 298.15)
                        End If
                        i += 1
                    Next

                    Dim total As Double = 0
                    Dim val As Double = 0
                    i = 0
                    For Each row In Me.gridInputComposition.Rows
                        If row.Cells(0).Value.ToString.Contains("Water") Then
                            total += row.Cells(1).Value / 1000 * liqdens(i) / MatStream.Phases(0).Compounds(row.Cells(0).Value).ConstantProperties.Molar_Weight * 1000
                        Else
                            total += row.Cells(1).Value
                        End If
                        i += 1
                    Next

                    Q = total

                    i = 0
                    For Each row In Me.gridInputComposition.Rows
                        If row.Cells(0).Value.ToString.Contains("Water") Then
                            MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value / 1000 * liqdens(i) / MatStream.Phases(0).Compounds(row.Cells(0).Value).ConstantProperties.Molar_Weight * 1000 / total
                        Else
                            MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value / total
                        End If
                        i += 1
                    Next

                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next

                    W = 0
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                        W += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / 1000 * Q
                    Next

                    ipp = Nothing

                Case 6

                    'molarity = mol solute per kg solvent

                    Dim total As Double = 0
                    Dim val As Double = 0
                    For Each row In Me.gridInputComposition.Rows
                        If row.Cells(0).Value.ToString.Contains("Water") Then
                            total += row.Cells(1).Value / MatStream.Phases(0).Compounds(row.Cells(0).Value).ConstantProperties.Molar_Weight * 1000
                        Else
                            total += row.Cells(1).Value
                        End If
                    Next

                    Q = total

                    For Each row In Me.gridInputComposition.Rows
                        If row.Cells(0).Value.ToString.Contains("Water") Then
                            MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value / MatStream.Phases(0).Compounds(row.Cells(0).Value).ConstantProperties.Molar_Weight * 1000 / total
                        Else
                            MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction = row.Cells(1).Value / total
                        End If
                    Next

                    For Each comp In MatStream.Phases(0).Compounds.Values
                        mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                    Next

                    W = 0
                    For Each comp In MatStream.Phases(0).Compounds.Values
                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                        W += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / 1000 * Q
                    Next

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
                    For Each row In Me.gridInputComposition.Rows
                        mtotal += row.Cells(1).Value * liqdens(i)
                        i += 1
                    Next
                    i = 0
                    For Each row In Me.gridInputComposition.Rows
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

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

    End Sub

    Function ValidateData() As Boolean

        Dim row As DataGridViewRow
        For Each row In Me.gridInputComposition.Rows
            If Not Double.TryParse(row.Cells(1).Value, New Double) Then
                Return False
            End If
        Next
        Return True

    End Function

    Private Sub cbCompBasis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCompBasis.SelectedIndexChanged
        Dim W, Q As Double
        W = MatStream.Phases(0).Properties.massflow.GetValueOrDefault
        Q = MatStream.Phases(0).Properties.molarflow.GetValueOrDefault
        Select Case cbCompBasis.SelectedIndex
            Case 0
                For Each row In Me.gridInputComposition.Rows
                    row.Cells(1).Value = MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction
                Next
            Case 1
                For Each row In Me.gridInputComposition.Rows
                    row.Cells(1).Value = MatStream.Phases(0).Compounds(row.Cells(0).Value).MassFraction
                Next
            Case 2
                For Each row In Me.gridInputComposition.Rows
                    row.Cells(1).Value = Converter.ConvertFromSI(units.molarflow, MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction.GetValueOrDefault * Q)
                Next
            Case 3
                For Each row In Me.gridInputComposition.Rows
                    row.Cells(1).Value = Converter.ConvertFromSI(units.massflow, MatStream.Phases(0).Compounds(row.Cells(0).Value).MassFraction.GetValueOrDefault * W)
                Next
            Case 5
                'molarity = mol solute per liter solution
                Dim n As Integer = MatStream.Phases(0).Compounds.Count
                Dim liqdens(n - 1), nbp(n - 1) As Double
                Dim ipp As New Thermodynamics.PropertyPackages.RaoultPropertyPackage()
                ipp.CurrentMaterialStream = MatStream
                Dim i As Integer = 0
                For Each s In MatStream.Phases(0).Compounds.Values
                    nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                    If 298.15 > nbp(i) Then
                        liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                    Else
                        liqdens(i) = ipp.AUX_LIQDENSi(s, 298.15)
                    End If
                    i += 1
                Next
                i = 0
                For Each row In Me.gridInputComposition.Rows
                    If row.Cells(0).Value.ToString.Contains("Water") Then
                        row.Cells(1).Value = MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction.GetValueOrDefault * Q * MatStream.Phases(0).Compounds(row.Cells(0).Value).ConstantProperties.Molar_Weight / 1000 / liqdens(i) * 1000
                    Else
                        row.Cells(1).Value = MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction.GetValueOrDefault * Q
                    End If
                    i += 1
                Next
            Case 6
                'molarity = mol solute per kg solvent
                For Each row In Me.gridInputComposition.Rows
                    If row.Cells(0).Value.ToString.Contains("Water") Then
                        row.Cells(1).Value = MatStream.Phases(0).Compounds(row.Cells(0).Value).MassFraction.GetValueOrDefault * W
                    Else
                        row.Cells(1).Value = MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction.GetValueOrDefault * Q
                    End If
                Next
            Case 4
                'liquid vol. frac
                Dim n As Integer = MatStream.Phases(0).Compounds.Count
                Dim liqdens(n - 1), nbp(n - 1), volfrac(n - 1), totalvol As Double
                Dim ipp As New Thermodynamics.PropertyPackages.RaoultPropertyPackage()
                ipp.CurrentMaterialStream = MatStream
                Dim i As Integer = 0
                totalvol = 0.0#
                For Each s In MatStream.Phases(0).Compounds.Values
                    nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                    If 298.15 > nbp(i) Then
                        liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                    Else
                        liqdens(i) = ipp.AUX_LIQDENSi(s, 298.15)
                    End If
                    totalvol += s.MoleFraction * s.ConstantProperties.Molar_Weight / liqdens(i)
                    i += 1
                Next
                i = 0
                For Each row In Me.gridInputComposition.Rows
                    row.Cells(1).Value = MatStream.Phases(0).Compounds(row.Cells(0).Value).MoleFraction * MatStream.Phases(0).Compounds(row.Cells(0).Value).ConstantProperties.Molar_Weight / liqdens(i) / totalvol
                    i += 1
                Next
                ipp = Nothing
        End Select
    End Sub


    Private Sub tbTemp_KeyDown(sender As Object, e As KeyEventArgs) Handles tbTemp.KeyDown, tbPressure.KeyDown, tbEnth.KeyDown, tbEntr.KeyDown,
                                                                            tbFracSpec.KeyDown, tbMassFlow.KeyDown, tbMoleFlow.KeyDown, tbVolFlow.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = Drawing.Color.Blue Then

            With MatStream.Phases(0).Properties

                If sender Is tbTemp Then .temperature = Converter.ConvertToSI(cbUnitsT.SelectedItem.ToString, Double.Parse(tbTemp.Text))
                If sender Is tbPressure Then .pressure = Converter.ConvertToSI(cbUnitsP.SelectedItem.ToString, Double.Parse(tbPressure.Text))
                If sender Is tbMassFlow Then .massflow = Converter.ConvertToSI(cbUnitsW.SelectedItem.ToString, Double.Parse(tbMassFlow.Text))
                If sender Is tbMoleFlow Then .molarflow = Converter.ConvertToSI(cbUnitsM.SelectedItem.ToString, Double.Parse(tbMoleFlow.Text))
                If sender Is tbVolFlow Then .volumetric_flow = Converter.ConvertToSI(cbUnitsQ.SelectedItem.ToString, Double.Parse(tbVolFlow.Text))
                If sender Is tbEnth Then .enthalpy = Converter.ConvertToSI(cbUnitsH.SelectedItem.ToString, Double.Parse(tbEnth.Text))
                If sender Is tbEntr Then .entropy = Converter.ConvertToSI(cbUnitsS.SelectedItem.ToString, Double.Parse(tbEntr.Text))

            End With

            If sender Is tbFracSpec And rbSpecVapor.Checked Then
                MatStream.Phases(2).Properties.molarfraction = Double.Parse(tbFracSpec.Text)
            ElseIf sender Is tbFracSpec And rbSpecLiquid.Checked Then
                MatStream.Phases(2).Properties.molarfraction = 1.0# - Double.Parse(tbFracSpec.Text)
            ElseIf sender Is tbFracSpec And rbSpecSolid.Checked Then
                MatStream.Phases(7).Properties.molarfraction = Double.Parse(tbFracSpec.Text)
            End If

            RequestCalc()

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            MatStream.PropertyPackage = MatStream.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            MatStream.PreferredFlashAlgorithm = cbFlashAlg.SelectedIndex
            RequestCalc()
        End If
    End Sub

    Sub RequestCalc()

        MatStream.FlowSheet.RequestCalculation(MatStream)

    End Sub

    Private Sub tbTemp_TextChanged(sender As Object, e As EventArgs) Handles tbTemp.TextChanged, tbPressure.TextChanged, tbEnth.TextChanged, tbEntr.TextChanged,
                                                                            tbFracSpec.TextChanged, tbMassFlow.TextChanged, tbMoleFlow.TextChanged, tbVolFlow.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If Double.TryParse(tbox.Text, New Double()) Then
            tbox.ForeColor = Drawing.Color.Blue
        Else
            tbox.ForeColor = Drawing.Color.Red
        End If

    End Sub

End Class