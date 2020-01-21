Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Interfaces
Imports System.Linq
Imports CapeOpen
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.CapeOpen
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps
Imports DWSIM.UnitOperations.UnitOperations.Column

Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports Controls

Public Class PropertyGridEditors

    Public Shared Function FT(ByRef prop As String, ByVal unit As String)
        Return prop & " (" & unit & ")"
    End Function

    Public Shared Sub BasePopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, uo As ISimulationObject)

        With pgrid

            If TypeOf uo Is MaterialStream Then
                .Item.Add(DWSIM.App.GetLocalString("UOPropertyPackage"), DirectCast(uo, MaterialStream).PropertyPackage.Tag, False, "Property Package", "", True)
            Else
                .Item.Add(DWSIM.App.GetLocalString("UOPropertyPackage"), uo.PropertyPackage.Tag, False, "Property Package", "", True)
            End If
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.PropertyPackages.UIPPSelector
            End With
            If uo.PreferredFlashAlgorithmTag = "" Then uo.PreferredFlashAlgorithmTag = "Default"
            .Item.Add("Flash Algorithm", uo, "PreferredFlashAlgorithmTag", False, "Property Package", "", True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.PropertyPackages.UIFASelector
            End With

            If Not uo.GraphicObject Is Nothing Then
                .Item.Add(DWSIM.App.GetLocalString("Ativo"), uo.GraphicObject, "Active", False, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
            End If
            If uo.IsSpecAttached = True Then
                .Item.Add(DWSIM.App.GetLocalString("ObjetoUtilizadopor"), uo.FlowSheet.Collections.ObjectCollection(uo.AttachedSpecId).GraphicObject.Tag, True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                Select Case uo.SpecVarType
                    Case SpecVarType.Target
                        .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), DWSIM.App.GetLocalString(uo.SpecVarType.ToString), True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                    Case SpecVarType.Source
                        .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), DWSIM.App.GetLocalString("SpecSource"), True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                End Select
            End If
            If Not uo.Annotation Is Nothing Then
                .Item.Add(DWSIM.App.GetLocalString("Anotaes"), uo, "Annotation", False, DWSIM.App.GetLocalString("Outros"), DWSIM.App.GetLocalString("Cliquenobotocomretic"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.Annotation.UIAnnotationEditor
                End With
            End If
            .Item.Add("ID", uo.Name, True, DWSIM.App.GetLocalString("Outros"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("LastUpdatedOn"), uo.LastUpdated.ToString("O"), True, DWSIM.App.GetLocalString("Outros"), "", True)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal ms As MaterialStream)

        Dim su = ms.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .Item.Clear()

            Dim valor As Double

            Dim roprops As Boolean = False

            If ms.GraphicObject.InputConnectors(0).IsAttached AndAlso ms.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType <> ObjectType.OT_Recycle Then
                roprops = True
            End If

            .Item.Add("[1] State Variables", ms, "SpecType", False, "1A. State Specification", "")
            .Item(.Item.Count - 1).Tag2 = "SpecType"
            .Item(.Item.Count - 1).IsReadOnly = roprops

            valor = Format(Converter.ConvertFromSI(su.temperature, ms.Phases(0).Properties.temperature.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
            .Item.Add("[2] " & FT(DWSIM.App.GetLocalString("Temperatura"), su.temperature), valor, True, "1A. State Specification", DWSIM.App.GetLocalString("Temperaturadacorrent"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_MS_0"
                Select Case ms.SpecType
                    Case StreamSpec.Pressure_and_Enthalpy, StreamSpec.Pressure_and_Entropy, StreamSpec.Pressure_and_VaporFraction
                        .IsReadOnly = True
                    Case Else
                        .IsReadOnly = False
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        .Tag = New Object() {ms.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                End Select
                If Not .IsReadOnly Then .IsReadOnly = roprops
            End With
            valor = Format(Converter.ConvertFromSI(su.pressure, ms.Phases(0).Properties.pressure.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
            .Item.Add(FT("[3] " & DWSIM.App.GetLocalString("Presso"), su.pressure), valor, True, "1A. State Specification", DWSIM.App.GetLocalString("Pressodacorrente"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_MS_1"
                Select Case ms.SpecType
                    Case StreamSpec.Temperature_and_VaporFraction
                        .IsReadOnly = True
                    Case Else
                        .IsReadOnly = False
                        .Tag = New Object() {ms.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End Select
                If Not .IsReadOnly Then .IsReadOnly = roprops
            End With
            valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(0).Properties.massflow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
            .Item.Add("[1] " & FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), valor, False, "1B. Flow Specification", DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_MS_2"
                .Tag = New Object() {ms.FlowSheet.Options.NumberFormat, su.massflow, "W"}
                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                .IsReadOnly = roprops
            End With
            valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(0).Properties.molarflow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
            .Item.Add("[2] " & FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), valor, False, "1B. Flow Specification", DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_MS_3"
                .Tag = New Object() {ms.FlowSheet.Options.NumberFormat, su.molarflow, "M"}
                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                .IsReadOnly = roprops
            End With
            valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(0).Properties.volumetric_flow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
            .Item.Add("[3] " & FT(DWSIM.App.GetLocalString("Vazovolumtrica"), su.volumetricFlow), valor, False, "1B. Flow Specification", DWSIM.App.GetLocalString("Vazovolumtricadacorr"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_MS_4"
                .Tag = New Object() {ms.FlowSheet.Options.NumberFormat, su.volumetricFlow, "Q"}
                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                .IsReadOnly = roprops
            End With

            Dim f As New PropertyGridEx.CustomPropertyCollection()
            valor = Format(ms.Phases(7).Properties.molarfraction.GetValueOrDefault, ms.FlowSheet.Options.NumberFormat)
            f.Add(DWSIM.App.GetLocalString("Solid"), valor, True, "1A. State Specification", DWSIM.App.GetLocalString("Fraomolardafasenamis"), True) 'solid
            f.Item(0).IsReadOnly = True
            f.Item(0).Tag2 = "PROP_MS_146"
            valor = Format(ms.Phases(1).Properties.molarfraction.GetValueOrDefault, ms.FlowSheet.Options.NumberFormat)
            f.Add(DWSIM.App.GetLocalString("OverallLiquid"), valor, True, "1A. State Specification", DWSIM.App.GetLocalString("Fraomolardafasenamis"), True) ' liquid
            f.Item(1).IsReadOnly = True
            valor = Format(ms.Phases(2).Properties.molarfraction.GetValueOrDefault, ms.FlowSheet.Options.NumberFormat)
            f.Add(DWSIM.App.GetLocalString("Vapor"), valor, True, "1A. State Specification", DWSIM.App.GetLocalString("Fraomolardafasenamis"), True) 'vapour
            f.Item(2).Tag2 = "PROP_MS_27"

            If Not ms.GraphicObject.InputConnectors(0).IsAttached And
                    (ms.SpecType = StreamSpec.Pressure_and_VaporFraction Or ms.SpecType = StreamSpec.Temperature_and_VaporFraction) _
                    Then f.Item(f.Count - 1).IsReadOnly = False

            .Item.Add("[4] Phase Molar Fraction", f, True, "1A. State Specification", DWSIM.App.GetLocalString("Fraomolardafase"), True)
            With .Item(.Item.Count - 1)
                .IsReadOnly = False
                .IsBrowsable = True
                .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                .CustomEditor = New System.Drawing.Design.UITypeEditor
            End With

            valor = Format(Converter.ConvertFromSI(su.enthalpy, ms.Phases(0).Properties.enthalpy.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
            .Item.Add("[8] " & FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), valor, True, "1A. State Specification", DWSIM.App.GetLocalString("EntalpiaEspecficadam"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_MS_7"
                Select Case ms.SpecType
                    Case StreamSpec.Pressure_and_Enthalpy
                        .IsReadOnly = False
                    Case Else
                        .IsReadOnly = True
                End Select
                If Not .IsReadOnly Then .IsReadOnly = roprops
            End With
            valor = Format(Converter.ConvertFromSI(su.entropy, ms.Phases(0).Properties.entropy.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
            .Item.Add("[9] " & FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), valor, True, "1A. State Specification", DWSIM.App.GetLocalString("EntropiaEspecficadam"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_MS_8"
                Select Case ms.SpecType
                    Case StreamSpec.Pressure_and_Entropy
                        .IsReadOnly = False
                    Case Else
                        .IsReadOnly = True
                End Select
                If Not .IsReadOnly Then .IsReadOnly = roprops
            End With
            .Item.Add("[1] " & DWSIM.App.GetLocalString("EditordeComposies"), ms.Phases(0), "Compounds", False, "1C. Compound Amounts", DWSIM.App.GetLocalString("UtilizeoEditordeComp"), True)
            If ms.GraphicObject.InputConnectors(0).IsAttached Then
                If ms.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType <> ObjectType.OT_Recycle Then
                    .Item(.Item.Count - 1).IsReadOnly = True
                    .Item(.Item.Count - 1).DefaultType = GetType(Dictionary(Of String, Compound))
                    .Item(.Item.Count - 1).Visible = False
                Else
                    .Item(.Item.Count - 1).DefaultValue = Nothing
                    .Item(.Item.Count - 1).DefaultType = GetType(Dictionary(Of String, Compound))
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.Composition.UICompositionEditor
                End If
            Else
                .Item(.Item.Count - 1).DefaultValue = Nothing
                .Item(.Item.Count - 1).DefaultType = GetType(Dictionary(Of String, Compound))
                .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.Composition.UICompositionEditor
            End If
            .Item.Add("[2] " & DWSIM.App.GetLocalString("Basedacomposio"), ms, "CompositionBasis", False, "1C. Compound Amounts", DWSIM.App.GetLocalString("Selecioneabaseparaav"), True)

            If ms.GraphicObject.InputConnectors(0).IsAttached Then
                If ms.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType <> ObjectType.OT_Recycle Then
                    .Item(1).IsReadOnly = True
                    .Item(2).IsReadOnly = True
                    .Item(3).IsReadOnly = True
                    .Item(4).IsReadOnly = True
                    .Item(5).IsReadOnly = True
                    .Item(6).IsReadOnly = True
                    .Item(7).IsReadOnly = True
                    .Item(8).IsReadOnly = True
                End If
            End If

            ms.PropertyPackage.CurrentMaterialStream = ms
            PopulatePropertyGrid_MaterialStream2(pgrid, ms)

            BasePopulatePropertyGrid(pgrid, ms)

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True

            Dim item = .SelectedGridItem

            pgrid.ExpandGroup("[4] Phase Molar Fraction")

            pgrid.SelectedGridItem = item

        End With

    End Sub

    Private Shared Sub PopulatePropertyGrid_MaterialStream2(ByVal pg As PropertyGridEx.PropertyGridEx, ms As MaterialStream)

        Dim su = ms.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        Dim valor As Double = 0.0#

        With pg

            Select Case ms.CompositionBasis
                Case CompositionBasis.Molar_Fractions
                    'PropertyGridEx.CustomPropertyCollection - Mistura
                    Dim m As New PropertyGridEx.CustomPropertyCollection()
                    Dim comp As ConstantProperties
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(0).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        m.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaMistura"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Vapor
                    Dim v As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(2).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        v.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseVapor"), True)
                        v.Item(v.Count - 1).IsReadOnly = True
                        v.Item(v.Count - 1).DefaultValue = Nothing
                        v.Item(v.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(1).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l.Item(l.Count - 1).IsReadOnly = True
                        l.Item(l.Count - 1).DefaultValue = Nothing
                        l.Item(l.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l1 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(3).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l1.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l1.Item(l1.Count - 1).IsReadOnly = True
                        l1.Item(l1.Count - 1).DefaultValue = Nothing
                        l1.Item(l1.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l2 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(4).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l2.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l2.Item(l2.Count - 1).IsReadOnly = True
                        l2.Item(l2.Count - 1).DefaultValue = Nothing
                        l2.Item(l2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l3 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(5).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l3.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l3.Item(l3.Count - 1).IsReadOnly = True
                        l3.Item(l3.Count - 1).DefaultValue = Nothing
                        l3.Item(l3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l4 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(6).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l4.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l4.Item(l4.Count - 1).IsReadOnly = True
                        l4.Item(l4.Count - 1).DefaultValue = Nothing
                        l4.Item(l4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim s As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(7).Compounds(comp.Name).MoleFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        s.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaPhasesolida"), True)
                        s.Item(s.Count - 1).IsReadOnly = True
                        s.Item(s.Count - 1).DefaultValue = Nothing
                        s.Item(s.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    .Item.Add("[1] " & DWSIM.App.GetLocalString("Mistura"), m, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Composiomolardamistu"), True)
                    With .Item(.Item.Count - 1)
                        If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                    If ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[2] " & DWSIM.App.GetLocalString("Vapor"), v, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[3] " & DWSIM.App.GetLocalString("OverallLiquid"), l, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas2"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[4] " & DWSIM.App.GetLocalString("Liquid1"), l1, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas2"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[5] " & DWSIM.App.GetLocalString("Liquid2"), l2, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas2"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[6] " & DWSIM.App.GetLocalString("Liquid3"), l3, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas2"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[7] " & DWSIM.App.GetLocalString("Aqueous"), l4, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas2"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[8] " & DWSIM.App.GetLocalString("Solid"), s, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas2"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                Case CompositionBasis.Mass_Fractions
                    'PropertyGridEx.CustomPropertyCollection - Mistura
                    Dim m As New PropertyGridEx.CustomPropertyCollection()
                    Dim comp As ConstantProperties
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(0).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        m.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaMistura"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Vapor
                    Dim v As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(2).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        v.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaFaseVapo"), True)
                        v.Item(v.Count - 1).IsReadOnly = True
                        v.Item(v.Count - 1).DefaultValue = Nothing
                        v.Item(v.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(1).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaFaseLqui"), True)
                        l.Item(l.Count - 1).IsReadOnly = True
                        l.Item(l.Count - 1).DefaultValue = Nothing
                        l.Item(l.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l1 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(3).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l1.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaFaseLqui"), True)
                        l1.Item(l1.Count - 1).IsReadOnly = True
                        l1.Item(l1.Count - 1).DefaultValue = Nothing
                        l1.Item(l1.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l2 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(4).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l2.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaFaseLqui"), True)
                        l2.Item(l2.Count - 1).IsReadOnly = True
                        l2.Item(l2.Count - 1).DefaultValue = Nothing
                        l2.Item(l2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l3 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(5).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l3.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaFaseLqui"), True)
                        l3.Item(l3.Count - 1).IsReadOnly = True
                        l3.Item(l3.Count - 1).DefaultValue = Nothing
                        l3.Item(l3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l4 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(6).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l4.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaFaseLqui"), True)
                        l4.Item(l4.Count - 1).IsReadOnly = True
                        l4.Item(l4.Count - 1).DefaultValue = Nothing
                        l4.Item(l4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Solido
                    Dim s As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(7).Compounds(comp.Name).MassFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        s.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomssicanaFaseLqui"), True)
                        s.Item(s.Count - 1).IsReadOnly = True
                        s.Item(s.Count - 1).DefaultValue = Nothing
                        s.Item(s.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    .Item.Add("[1] " & DWSIM.App.GetLocalString("Mistura"), m, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Composiomssicadamist"), True)
                    With .Item(.Item.Count - 1)
                        If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                    If ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[2] " & DWSIM.App.GetLocalString("Vapor"), v, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas3"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[3] " & DWSIM.App.GetLocalString("OverallLiquid"), l, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[4] " & DWSIM.App.GetLocalString("Liquid1"), l1, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[5] " & DWSIM.App.GetLocalString("Liquid2"), l2, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[6] " & DWSIM.App.GetLocalString("Liquid3"), l3, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[7] " & DWSIM.App.GetLocalString("Aqueous"), l4, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[8] " & DWSIM.App.GetLocalString("Solid"), s, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If

                Case CompositionBasis.Volumetric_Fractions
                    'PropertyGridEx.CustomPropertyCollection - Mistura
                    Dim m As New PropertyGridEx.CustomPropertyCollection()
                    Dim comp As ConstantProperties
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(0).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        m.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionMixture"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Vapor
                    Dim v As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(2).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        v.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionVaporPhase"), True)
                        v.Item(v.Count - 1).IsReadOnly = True
                        v.Item(v.Count - 1).DefaultValue = Nothing
                        v.Item(v.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(1).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionLiqPhase"), True)
                        l.Item(l.Count - 1).IsReadOnly = True
                        l.Item(l.Count - 1).DefaultValue = Nothing
                        l.Item(l.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l1 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(3).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l1.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionLiqPhase"), True)
                        l1.Item(l1.Count - 1).IsReadOnly = True
                        l1.Item(l1.Count - 1).DefaultValue = Nothing
                        l1.Item(l1.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l2 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(4).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l2.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionLiqPhase"), True)
                        l2.Item(l2.Count - 1).IsReadOnly = True
                        l2.Item(l2.Count - 1).DefaultValue = Nothing
                        l2.Item(l2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l3 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(5).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l3.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionLiqPhase"), True)
                        l3.Item(l3.Count - 1).IsReadOnly = True
                        l3.Item(l3.Count - 1).DefaultValue = Nothing
                        l3.Item(l3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l4 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(6).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        l4.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionLiqPhase"), True)
                        l4.Item(l4.Count - 1).IsReadOnly = True
                        l4.Item(l4.Count - 1).DefaultValue = Nothing
                        l4.Item(l4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Solid
                    Dim s As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(7).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault, ms.FlowSheet.Options.FractionNumberFormat)
                        s.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VolFractionSolPhase"), True)
                        s.Item(s.Count - 1).IsReadOnly = True
                        s.Item(s.Count - 1).DefaultValue = Nothing
                        s.Item(s.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    .Item.Add("[1] " & DWSIM.App.GetLocalString("Mistura"), m, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Composiomssicadamist"), True)
                    With .Item(.Item.Count - 1)
                        If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                    If ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[2] " & DWSIM.App.GetLocalString("Vapor"), v, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas3"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[3] " & DWSIM.App.GetLocalString("OverallLiquid"), l, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[4] " & DWSIM.App.GetLocalString("Liquid1"), l1, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[5] " & DWSIM.App.GetLocalString("Liquid2"), l2, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[6] " & DWSIM.App.GetLocalString("Liquid3"), l3, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[7] " & DWSIM.App.GetLocalString("Aqueous"), l4, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[8] " & DWSIM.App.GetLocalString("Solid"), s, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas4"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                Case CompositionBasis.Mass_Flows
                    'PropertyGridEx.CustomPropertyCollection - Mistura
                    Dim m As New PropertyGridEx.CustomPropertyCollection()
                    Dim comp As ConstantProperties
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(0).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        m.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaMistura"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Vapor
                    Dim v As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(2).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        v.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaFaseVapo"), True)
                        v.Item(v.Count - 1).IsReadOnly = True
                        v.Item(v.Count - 1).DefaultValue = Nothing
                        v.Item(v.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(1).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaFaseLqui"), True)
                        l.Item(l.Count - 1).IsReadOnly = True
                        l.Item(l.Count - 1).DefaultValue = Nothing
                        l.Item(l.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l1 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(3).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l1.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaFaseLqui"), True)
                        l1.Item(l1.Count - 1).IsReadOnly = True
                        l1.Item(l1.Count - 1).DefaultValue = Nothing
                        l1.Item(l1.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l2 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(4).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l2.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaFaseLqui"), True)
                        l2.Item(l2.Count - 1).IsReadOnly = True
                        l2.Item(l2.Count - 1).DefaultValue = Nothing
                        l2.Item(l2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l3 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(5).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l3.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaFaseLqui"), True)
                        l3.Item(l3.Count - 1).IsReadOnly = True
                        l3.Item(l3.Count - 1).DefaultValue = Nothing
                        l3.Item(l3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l4 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(6).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l4.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaFaseLqui"), True)
                        l4.Item(l4.Count - 1).IsReadOnly = True
                        l4.Item(l4.Count - 1).DefaultValue = Nothing
                        l4.Item(l4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Solid
                    Dim s As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.massflow, ms.Phases(7).Compounds(comp.Name).MassFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        s.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.massflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazomssicanaPhasesolida"), True)
                        s.Item(s.Count - 1).IsReadOnly = True
                        s.Item(s.Count - 1).DefaultValue = Nothing
                        s.Item(s.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    .Item.Add("[1] " & DWSIM.App.GetLocalString("Mistura"), m, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Vazomssicadamistura"), True)
                    With .Item(.Item.Count - 1)
                        If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                    If ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[2] " & DWSIM.App.GetLocalString("Vapor"), v, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas5"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[3] " & DWSIM.App.GetLocalString("OverallLiquid"), l, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas6"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[4] " & DWSIM.App.GetLocalString("Liquid1"), l1, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas6"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[5] " & DWSIM.App.GetLocalString("Liquid2"), l2, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas6"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[6] " & DWSIM.App.GetLocalString("Liquid3"), l3, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas6"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[7] " & DWSIM.App.GetLocalString("Aqueous"), l4, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas6"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[8] " & DWSIM.App.GetLocalString("Solid"), s, True, DWSIM.App.GetLocalString("Composiesmssicas2"), DWSIM.App.GetLocalString("Mostraacomposiodafas7"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                Case CompositionBasis.Molar_Flows
                    'PropertyGridEx.CustomPropertyCollection - Mistura
                    Dim m As New PropertyGridEx.CustomPropertyCollection()
                    Dim comp As ConstantProperties
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(0).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        m.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaMistura"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Vapor
                    Dim v As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(2).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        v.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseVapor"), True)
                        v.Item(v.Count - 1).IsReadOnly = True
                        v.Item(v.Count - 1).DefaultValue = Nothing
                        v.Item(v.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(1).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l.Item(l.Count - 1).IsReadOnly = True
                        l.Item(l.Count - 1).DefaultValue = Nothing
                        l.Item(l.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l1 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(3).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l1.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l1.Item(l1.Count - 1).IsReadOnly = True
                        l1.Item(l1.Count - 1).DefaultValue = Nothing
                        l1.Item(l1.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l2 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(4).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l2.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l2.Item(l2.Count - 1).IsReadOnly = True
                        l2.Item(l2.Count - 1).DefaultValue = Nothing
                        l2.Item(l2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l3 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(5).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l3.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l3.Item(l3.Count - 1).IsReadOnly = True
                        l3.Item(l3.Count - 1).DefaultValue = Nothing
                        l3.Item(l3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l4 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(6).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l4.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaFaseLquid"), True)
                        l4.Item(l4.Count - 1).IsReadOnly = True
                        l4.Item(l4.Count - 1).DefaultValue = Nothing
                        l4.Item(l4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Solid
                    Dim s As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.molarflow, ms.Phases(7).Compounds(comp.Name).MolarFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        s.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.molarflow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("FraomolarnaPhasesolida"), True)
                        s.Item(s.Count - 1).IsReadOnly = True
                        s.Item(s.Count - 1).DefaultValue = Nothing
                        s.Item(s.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    .Item.Add("[1] " & DWSIM.App.GetLocalString("Mistura"), m, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Composiomolardamistu"), True)
                    With .Item(.Item.Count - 1)
                        If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                    If ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[2] " & DWSIM.App.GetLocalString("Vapor"), v, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas7"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[3] " & DWSIM.App.GetLocalString("OverallLiquid"), l, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas8"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[4] " & DWSIM.App.GetLocalString("Liquid1"), l1, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas8"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[5] " & DWSIM.App.GetLocalString("Liquid2"), l2, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas8"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[6] " & DWSIM.App.GetLocalString("Liquid3"), l3, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas8"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[7] " & DWSIM.App.GetLocalString("Aqueous"), l4, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas8"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[8] " & DWSIM.App.GetLocalString("Solid"), s, True, DWSIM.App.GetLocalString("Composiesmolares2"), DWSIM.App.GetLocalString("Mostraacomposiodafas8"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                Case CompositionBasis.Volumetric_Flows
                    'PropertyGridEx.CustomPropertyCollection - Mistura
                    Dim m As New PropertyGridEx.CustomPropertyCollection()
                    Dim comp As ConstantProperties
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(0).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        m.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaMist"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Vapor
                    Dim v As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(2).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        v.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaFase"), True)
                        v.Item(v.Count - 1).IsReadOnly = True
                        v.Item(v.Count - 1).DefaultValue = Nothing
                        v.Item(v.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(1).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaFase"), True)
                        l.Item(l.Count - 1).IsReadOnly = True
                        l.Item(l.Count - 1).DefaultValue = Nothing
                        l.Item(l.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l1 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(3).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l1.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaFase"), True)
                        l1.Item(l1.Count - 1).IsReadOnly = True
                        l1.Item(l1.Count - 1).DefaultValue = Nothing
                        l1.Item(l1.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l2 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(4).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l2.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaFase"), True)
                        l2.Item(l2.Count - 1).IsReadOnly = True
                        l2.Item(l2.Count - 1).DefaultValue = Nothing
                        l2.Item(l2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l3 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(5).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l3.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaFase"), True)
                        l3.Item(l3.Count - 1).IsReadOnly = True
                        l3.Item(l3.Count - 1).DefaultValue = Nothing
                        l3.Item(l3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Liquido
                    Dim l4 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(6).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        l4.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaFase"), True)
                        l4.Item(l4.Count - 1).IsReadOnly = True
                        l4.Item(l4.Count - 1).DefaultValue = Nothing
                        l4.Item(l4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    'PropertyGridEx.CustomPropertyCollection - Solid
                    Dim s As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(Converter.ConvertFromSI(su.volumetricFlow, ms.Phases(7).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault), ms.FlowSheet.Options.NumberFormat)
                        s.Add(FT(DWSIM.App.GetLocalString(comp.Name), su.volumetricFlow), Format(valor, ms.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("VazovolumtricanaPhasesolida"), True)
                        s.Item(s.Count - 1).IsReadOnly = True
                        s.Item(s.Count - 1).DefaultValue = Nothing
                        s.Item(s.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    .Item.Add("[1] " & DWSIM.App.GetLocalString("Mistura"), m, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Vazovolumtricadamist"), True)
                    With .Item(.Item.Count - 1)
                        If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                    If ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[2] " & DWSIM.App.GetLocalString("Vapor"), v, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas9"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[3] " & DWSIM.App.GetLocalString("OverallLiquid"), l, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas10"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[4] " & DWSIM.App.GetLocalString("Liquid1"), l1, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas10"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[5] " & DWSIM.App.GetLocalString("Liquid2"), l2, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas10"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[6] " & DWSIM.App.GetLocalString("Liquid3"), l3, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas10"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[7] " & DWSIM.App.GetLocalString("Aqueous"), l4, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas10"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                    If ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                        .Item.Add("[8] " & DWSIM.App.GetLocalString("Solid"), s, True, DWSIM.App.GetLocalString("Composiesvolumtrica2"), DWSIM.App.GetLocalString("Mostraacomposiodafas10"), True)
                        With .Item(.Item.Count - 1)
                            If ms.GraphicObject.InputConnectors(0).IsAttached Then .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
                Case Else

            End Select

            If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 And
                    ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                'Kvalues
                Dim comp As ConstantProperties
                Dim k0 As New PropertyGridEx.CustomPropertyCollection()
                For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                    valor = Format(ms.Phases(0).Compounds(comp.Name).Kvalue, ms.FlowSheet.Options.NumberFormat)
                    k0.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("Kvalues"), DWSIM.App.GetLocalString("Kvalues"), True)
                    k0.Item(k0.Count - 1).IsReadOnly = True
                    k0.Item(k0.Count - 1).DefaultValue = Nothing
                    k0.Item(k0.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next
                .Item.Add(DWSIM.App.GetLocalString("Kvalues"), k0, True, DWSIM.App.GetLocalString("ComponentDistribution"), DWSIM.App.GetLocalString("ComponentDistribution"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                End With
                Dim k1 As New PropertyGridEx.CustomPropertyCollection()
                For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                    valor = Format(ms.Phases(0).Compounds(comp.Name).lnKvalue, ms.FlowSheet.Options.NumberFormat)
                    k1.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("LnKvalues"), DWSIM.App.GetLocalString("LnKvalues"), True)
                    k1.Item(k1.Count - 1).IsReadOnly = True
                    k1.Item(k1.Count - 1).DefaultValue = Nothing
                    k1.Item(k1.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next
                .Item.Add(DWSIM.App.GetLocalString("LnKvalues"), k1, True, DWSIM.App.GetLocalString("ComponentDistribution"), DWSIM.App.GetLocalString("ComponentDistribution"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                End With
            End If

            Dim val, refval As Nullable(Of Double)

            Dim tmp As Nullable(Of Double)
            Dim it As PropertyGridEx.CustomProperty = Nothing

            If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 And
                    (ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Or
                     ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0) Then

                Dim pm As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Mistura
                refval = ms.Phases(0).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("EntalpiaEspecficadam"), True)
                refval = ms.Phases(0).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("EntropiaEspecficadam"), True)
                refval = ms.Phases(0).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(0).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(0).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("Massamolardamistura"), True)
                refval = ms.Phases(0).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("Massaespecficadamist"), True)
                refval = ms.Phases(0).Properties.massflow.GetValueOrDefault / CDbl(ms.Phases(0).Properties.density.GetValueOrDefault)
                'If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.Flowsheet.Options.NumberFormat)
                'pm.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                'refval = ms.Phases(0).Properties.massflow.GetValueOrDefault
                'If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.Flowsheet.Options.NumberFormat)
                'pm.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(0).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("Condutividadetrmicad"), True)

                If ms.PropertyPackage.FlashBase.FlashSettings(FlashSetting.CalculateBubbleAndDewPoints) = True Then
                    refval = ms.Phases(0).Properties.bubblePressure.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.pressure, refval), ms.FlowSheet.Options.NumberFormat)
                    pm.Add(FT(DWSIM.App.GetLocalString("BubblePress"), su.pressure), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("BubblePress"), True)
                    refval = ms.Phases(0).Properties.dewPressure.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.pressure, refval), ms.FlowSheet.Options.NumberFormat)
                    pm.Add(FT(DWSIM.App.GetLocalString("DewPress"), su.pressure), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("DewPress"), True)
                    refval = ms.Phases(0).Properties.bubbleTemperature.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.temperature, refval), ms.FlowSheet.Options.NumberFormat)
                    pm.Add(FT(DWSIM.App.GetLocalString("BubbleTemp"), su.temperature), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("BubbleTemp"), True)
                    refval = ms.Phases(0).Properties.dewTemperature.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.temperature, refval), ms.FlowSheet.Options.NumberFormat)
                    pm.Add(FT(DWSIM.App.GetLocalString("DewTemp"), su.temperature), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("DewTemp"), True)
                End If

                For Each it In pm
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P1] " & DWSIM.App.GetLocalString("Mistura"), pm, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("Propriedadesdamistur"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            ElseIf ms.PropertyPackage.FlashBase.FlashSettings(FlashSetting.CalculateBubbleAndDewPoints) = True Then

                Dim pm As New PropertyGridEx.CustomPropertyCollection()

                refval = ms.Phases(0).Properties.bubblePressure.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.pressure, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("BubblePress"), su.pressure), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("BubblePress"), True)
                refval = ms.Phases(0).Properties.dewPressure.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.pressure, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("DewPress"), su.pressure), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("DewPress"), True)
                refval = ms.Phases(0).Properties.bubbleTemperature.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.temperature, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("BubbleTemp"), su.temperature), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("BubbleTemp"), True)
                refval = ms.Phases(0).Properties.dewTemperature.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.temperature, refval), ms.FlowSheet.Options.NumberFormat)
                pm.Add(FT(DWSIM.App.GetLocalString("DewTemp"), su.temperature), val, True, DWSIM.App.GetLocalString("Mistura"), DWSIM.App.GetLocalString("DewTemp"), True)

                For Each it In pm
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P1] " & DWSIM.App.GetLocalString("Mistura"), pm, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("Propriedadesdamistur"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            val = Nothing

            If ms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then

                Dim pv As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Vapor
                refval = ms.Phases(2).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("EntalpiaEspecficadaf"), True)
                refval = ms.Phases(2).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("EntropiaEspecficadaf"), True)
                refval = ms.Phases(2).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(2).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(2).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Massamolardafasevapo"), True)
                refval = ms.Phases(2).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Massaespecficadafase"), True)
                refval = ms.Phases(2).Properties.massflow.GetValueOrDefault / CDbl(ms.Phases(2).Properties.density.GetValueOrDefault)
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                refval = ms.Phases(2).Properties.massflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(2).Properties.molarflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molarflow, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
                refval = ms.Phases(2).Properties.molarfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pv.Add(DWSIM.App.GetLocalString("Fraomolardafase"), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Fraomolardafasenamis"), True)
                refval = ms.Phases(2).Properties.massfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pv.Add(DWSIM.App.GetLocalString("Fraomssicadafase"), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Fraomssicadafasenami"), True)
                refval = ms.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pv.Add("Compressibility Factor (Z)", val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Fatordecompressibili"), True)
                refval = ms.Phases(2).Properties.heatCapacityCp.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.heatCapacityCp, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT("Heat Capacity at Constant Pressure (Cp)", su.heatCapacityCp), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Capacidadecalorficad"), True)
                refval = ms.Phases(2).Properties.heatCapacityCp.GetValueOrDefault / CDbl(ms.Phases(2).Properties.heatCapacityCv.GetValueOrDefault)
                If refval.HasValue = True And Double.IsNaN(refval) = False Then tmp = Format(refval, ms.FlowSheet.Options.NumberFormat) Else tmp = 0.0#
                pv.Add("Heat Capacity Ratio (Cp/Cv)", tmp, True, DWSIM.App.GetLocalString("lquida"), DWSIM.App.GetLocalString("Razoentreascapacidad"), True)
                refval = ms.Phases(2).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pv.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Condutividadetrmicad1"), True)
                refval = ms.Phases(2).Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then val = Format(Converter.ConvertFromSI(su.cinematic_viscosity, refval), "E")
                pv.Add(FT(DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Viscosidadecinemtica2"), True)
                refval = ms.Phases(2).Properties.viscosity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.viscosity, refval), "E")
                pv.Add(FT(DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity), val, True, DWSIM.App.GetLocalString("Vapor"), DWSIM.App.GetLocalString("Viscosidadedinmicada"), True)

                pv.Add(FT("Bulk Modulus", su.pressure),
                       ms.Phases(2).Properties.bulk_modulus.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Vapor"))
                pv.Add(FT("Isothermal Compressibility", su.compressibility),
                       ms.Phases(2).Properties.compressibility.GetValueOrDefault.ConvertFromSI(su.compressibility).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Vapor"))
                pv.Add(FT("Speed of Sound", su.speedOfSound),
                       ms.Phases(2).Properties.speedOfSound.GetValueOrDefault.ConvertFromSI(su.velocity).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Vapor"))
                pv.Add(FT("Joule-Thomson Coefficient", su.jouleThomsonCoefficient),
                       ms.Phases(2).Properties.jouleThomsonCoefficient.GetValueOrDefault.ConvertFromSI(su.jouleThomsonCoefficient).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Vapor"))


                For Each it In pv
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P2] " & DWSIM.App.GetLocalString("Vapor"), pv, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("Propriedadesdafaseva"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            val = Nothing

            If ms.Phases(1).Properties.molarfraction.GetValueOrDefault > 0 Then

                Dim pl As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Liquido
                refval = ms.Phases(1).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntalpiaEspecficadaf2"), True)
                refval = ms.Phases(1).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntropiaEspecficadaf2"), True)
                refval = ms.Phases(1).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(1).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(1).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massamolardafaselqui"), True)
                refval = ms.Phases(1).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massaespecficadafase2"), True)
                refval = ms.Phases(1).Properties.massflow.GetValueOrDefault / ms.Phases(1).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                refval = ms.Phases(1).Properties.massflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(1).Properties.molarflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molarflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
                refval = ms.Phases(1).Properties.molarfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomolardafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomolardafasenamis"), True)
                refval = ms.Phases(1).Properties.massfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomssicadafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomssicadafasenami"), True)
                'refval = ms.Phases(1).Properties.compressibilityFactor.GetValueOrDefault
                'If refval.HasValue = True Then val = Format(refval, ms.Flowsheet.Options.NumberFormat)
                'pl.Add("Compressibility Factor (Z)", val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fatordecompressibili"), True)
                refval = ms.Phases(1).Properties.heatCapacityCp.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.heatCapacityCp, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT("Heat Capacity at Constant Pressure (Cp)", su.heatCapacityCp), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Capacidadecalorficad"), True)
                refval = ms.Phases(1).Properties.heatCapacityCp.GetValueOrDefault / ms.Phases(1).Properties.heatCapacityCv.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then tmp = Format(refval, ms.FlowSheet.Options.NumberFormat) Else tmp = 0.0#
                pl.Add("Heat Capacity Ratio (Cp/Cv)", tmp, True, DWSIM.App.GetLocalString("lquida"), DWSIM.App.GetLocalString("Razoentreascapacidad"), True)
                refval = ms.Phases(0).Properties.surfaceTension.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.surfaceTension, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Tensosuperficial"), su.surfaceTension), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Tensosuperficialentr"), True)
                refval = ms.Phases(1).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Condutividadetrmicad2"), True)
                refval = ms.Phases(1).Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then val = Format(Converter.ConvertFromSI(su.cinematic_viscosity, refval), "E")
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadecinemtica2"), True)
                refval = ms.Phases(1).Properties.viscosity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.viscosity, refval), "E")
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadedinmicada"), True)

                pl.Add(FT("Bulk Modulus", su.pressure),
                       ms.Phases(1).Properties.bulk_modulus.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Isothermal Compressibility", su.compressibility),
                       ms.Phases(1).Properties.compressibility.GetValueOrDefault.ConvertFromSI(su.compressibility).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Speed of Sound", su.speedOfSound),
                       ms.Phases(1).Properties.speedOfSound.GetValueOrDefault.ConvertFromSI(su.velocity).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Joule-Thomson Coefficient", su.jouleThomsonCoefficient),
                       ms.Phases(1).Properties.jouleThomsonCoefficient.GetValueOrDefault.ConvertFromSI(su.jouleThomsonCoefficient).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))

                For Each it In pl
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P3] " & DWSIM.App.GetLocalString("OverallLiquid"), pl, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("PropriedadesdaFaseLq"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            If ms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then

                Dim pl As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Liquido

                If TypeOf ms.PropertyPackage Is SeawaterPropertyPackage Then

                    Dim water As Compound = (From subst As ICompound In ms.Phases(3).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
                    Dim salt As Compound = (From subst As ICompound In ms.Phases(3).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

                    Dim salinity As Double = salt.MassFraction.GetValueOrDefault / water.MassFraction.GetValueOrDefault

                    val = Format(salinity, ms.FlowSheet.Options.NumberFormat)
                    pl.Add(DWSIM.App.GetLocalString("Salinity"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Salinity"), True)

                End If

                If TypeOf ms.PropertyPackage Is SourWaterPropertyPackage Then

                    refval = ms.Phases(3).Properties.pH.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                    pl.Add(DWSIM.App.GetLocalString("pH"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("pH"), True)

                End If

                If ms.PropertyPackage.IsElectrolytePP Then

                    'Liquid Phase Activity Coefficients
                    Dim comp As ConstantProperties
                    Dim k0 As New PropertyGridEx.CustomPropertyCollection()
                    For Each comp In ms.FlowSheet.Options.SelectedComponents.Values
                        valor = Format(ms.Phases(3).Compounds(comp.Name).ActivityCoeff, ms.FlowSheet.Options.NumberFormat)
                        k0.Add(DWSIM.App.GetLocalString(comp.Name), valor, False, DWSIM.App.GetLocalString("ActivityCoefficients"), DWSIM.App.GetLocalString("ActivityCoefficients"), True)
                        k0.Item(k0.Count - 1).IsReadOnly = True
                        k0.Item(k0.Count - 1).DefaultValue = Nothing
                        k0.Item(k0.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next
                    pl.Add(DWSIM.App.GetLocalString("ActivityCoefficients"), k0, True, DWSIM.App.GetLocalString("ActivityCoefficients"), DWSIM.App.GetLocalString("LiquidPhaseActivityCoefficients"), True)
                    With pl.Item(pl.Count - 1)
                        .IsReadOnly = True
                        .IsBrowsable = True
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    End With

                    refval = ms.Phases(3).Properties.pH.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                    pl.Add(DWSIM.App.GetLocalString("pH"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("pH"), True)

                    refval = ms.Phases(3).Properties.osmoticCoefficient.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                    pl.Add(DWSIM.App.GetLocalString("OsmoticCoefficient"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("OsmoticCoefficient"), True)

                    refval = ms.Phases(3).Properties.freezingPoint.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.temperature, refval), ms.FlowSheet.Options.NumberFormat)
                    pl.Add(FT(DWSIM.App.GetLocalString("FreezingPoint"), su.temperature), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("FreezingPoint"), True)

                    refval = ms.Phases(3).Properties.freezingPointDepression.GetValueOrDefault
                    If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.deltaT, refval), ms.FlowSheet.Options.NumberFormat)
                    pl.Add(FT(DWSIM.App.GetLocalString("FreezingPointDepression"), su.deltaT), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("FreezingPointDepression"), True)

                End If

                refval = ms.Phases(3).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntalpiaEspecficadaf2"), True)
                refval = ms.Phases(3).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntropiaEspecficadaf2"), True)
                refval = ms.Phases(3).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(3).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(3).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massamolardafaselqui"), True)
                refval = ms.Phases(3).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massaespecficadafase2"), True)
                refval = ms.Phases(3).Properties.massflow.GetValueOrDefault / ms.Phases(3).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                refval = ms.Phases(3).Properties.massflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(3).Properties.molarflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molarflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
                refval = ms.Phases(3).Properties.molarfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomolardafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomolardafasenamis"), True)
                refval = ms.Phases(3).Properties.massfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomssicadafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomssicadafasenami"), True)
                refval = ms.Phases(3).Properties.compressibilityFactor.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add("Compressibility Factor (Z)", val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fatordecompressibili"), True)
                refval = ms.Phases(3).Properties.heatCapacityCp.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.heatCapacityCp, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT("Heat Capacity at Constant Pressure (Cp)", su.heatCapacityCp), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Capacidadecalorficad"), True)
                refval = ms.Phases(3).Properties.heatCapacityCp.GetValueOrDefault / ms.Phases(3).Properties.heatCapacityCv.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then tmp = Format(refval, ms.FlowSheet.Options.NumberFormat) Else tmp = 0.0#
                pl.Add("Heat Capacity Ratio (Cp/Cv)", tmp, True, DWSIM.App.GetLocalString("lquida"), DWSIM.App.GetLocalString("Razoentreascapacidad"), True)
                refval = ms.Phases(3).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Condutividadetrmicad2"), True)
                refval = ms.Phases(3).Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then val = Format(Converter.ConvertFromSI(su.cinematic_viscosity, refval), "E")
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadecinemtica2"), True)
                refval = ms.Phases(3).Properties.viscosity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.viscosity, refval), "E")
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadedinmicada"), True)

                pl.Add(FT("Bulk Modulus", su.pressure),
                       ms.Phases(3).Properties.bulk_modulus.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Isothermal Compressibility", su.compressibility),
                       ms.Phases(3).Properties.compressibility.GetValueOrDefault.ConvertFromSI(su.compressibility).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Speed of Sound", su.speedOfSound),
                       ms.Phases(3).Properties.speedOfSound.GetValueOrDefault.ConvertFromSI(su.velocity).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Joule-Thomson Coefficient", su.jouleThomsonCoefficient),
                       ms.Phases(3).Properties.jouleThomsonCoefficient.GetValueOrDefault.ConvertFromSI(su.jouleThomsonCoefficient).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))

                For Each it In pl
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P4] " & DWSIM.App.GetLocalString("Liquid1"), pl, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("PropriedadesdaFaseLq"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            If ms.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then

                Dim pl As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Liquido
                refval = ms.Phases(4).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntalpiaEspecficadaf2"), True)
                refval = ms.Phases(4).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntropiaEspecficadaf2"), True)
                refval = ms.Phases(4).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(4).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(4).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massamolardafaselqui"), True)
                refval = ms.Phases(4).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massaespecficadafase2"), True)
                refval = ms.Phases(4).Properties.massflow.GetValueOrDefault / ms.Phases(4).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                refval = ms.Phases(4).Properties.massflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(4).Properties.molarflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molarflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
                refval = ms.Phases(4).Properties.molarfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomolardafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomolardafasenamis"), True)
                refval = ms.Phases(4).Properties.massfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomssicadafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomssicadafasenami"), True)
                refval = ms.Phases(4).Properties.compressibilityFactor.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add("Compressibility Factor (Z)", val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fatordecompressibili"), True)
                refval = ms.Phases(4).Properties.heatCapacityCp.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.heatCapacityCp, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT("Heat Capacity at Constant Pressure (Cp)", su.heatCapacityCp), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Capacidadecalorficad"), True)
                refval = ms.Phases(4).Properties.heatCapacityCp.GetValueOrDefault / ms.Phases(4).Properties.heatCapacityCv.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then tmp = Format(refval, ms.FlowSheet.Options.NumberFormat) Else tmp = 0.0#
                pl.Add("Heat Capacity Ratio (Cp/Cv)", tmp, True, DWSIM.App.GetLocalString("lquida"), DWSIM.App.GetLocalString("Razoentreascapacidad"), True)
                refval = ms.Phases(4).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Condutividadetrmicad2"), True)
                refval = ms.Phases(4).Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then val = Format(Converter.ConvertFromSI(su.cinematic_viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadecinemtica2"), True)
                refval = ms.Phases(4).Properties.viscosity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadedinmicada"), True)

                pl.Add(FT("Bulk Modulus", su.pressure),
                       ms.Phases(4).Properties.bulk_modulus.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Isothermal Compressibility", su.compressibility),
                       ms.Phases(4).Properties.compressibility.GetValueOrDefault.ConvertFromSI(su.compressibility).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Speed of Sound", su.speedOfSound),
                       ms.Phases(4).Properties.speedOfSound.GetValueOrDefault.ConvertFromSI(su.velocity).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))
                pl.Add(FT("Joule-Thomson Coefficient", su.jouleThomsonCoefficient),
                       ms.Phases(4).Properties.jouleThomsonCoefficient.GetValueOrDefault.ConvertFromSI(su.jouleThomsonCoefficient).ToString(ms.FlowSheet.FlowsheetOptions.NumberFormat),
                       True, DWSIM.App.GetLocalString("Lquido"))

                For Each it In pl
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P5] " & DWSIM.App.GetLocalString("Liquid2"), pl, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("PropriedadesdaFaseLq"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            If ms.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then

                Dim pl As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Liquido
                refval = ms.Phases(5).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntalpiaEspecficadaf2"), True)
                refval = ms.Phases(5).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntropiaEspecficadaf2"), True)
                refval = ms.Phases(5).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(5).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(5).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massamolardafaselqui"), True)
                refval = ms.Phases(5).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massaespecficadafase2"), True)
                refval = ms.Phases(5).Properties.massflow.GetValueOrDefault / ms.Phases(5).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                refval = ms.Phases(5).Properties.massflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(5).Properties.molarflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molarflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
                refval = ms.Phases(5).Properties.molarfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomolardafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomolardafasenamis"), True)
                refval = ms.Phases(5).Properties.massfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomssicadafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomssicadafasenami"), True)
                refval = ms.Phases(5).Properties.compressibilityFactor.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add("Compressibility Factor (Z)", val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fatordecompressibili"), True)
                refval = ms.Phases(5).Properties.heatCapacityCp.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.heatCapacityCp, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT("Heat Capacity at Constant Pressure (Cp)", su.heatCapacityCp), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Capacidadecalorficad"), True)
                refval = ms.Phases(5).Properties.heatCapacityCp.GetValueOrDefault / ms.Phases(5).Properties.heatCapacityCv.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then tmp = Format(refval, ms.FlowSheet.Options.NumberFormat) Else tmp = 0.0#
                pl.Add("Heat Capacity Ratio (Cp/Cv)", tmp, True, DWSIM.App.GetLocalString("lquida"), DWSIM.App.GetLocalString("Razoentreascapacidad"), True)
                refval = ms.Phases(5).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Condutividadetrmicad2"), True)
                refval = ms.Phases(5).Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then val = Format(Converter.ConvertFromSI(su.cinematic_viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadecinemtica2"), True)
                refval = ms.Phases(5).Properties.viscosity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadedinmicada"), True)

                For Each it In pl
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P6] " & DWSIM.App.GetLocalString("Liquid3"), pl, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("PropriedadesdaFaseLq"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            If ms.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then

                Dim pl As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Liquido
                refval = ms.Phases(6).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntalpiaEspecficadaf2"), True)
                refval = ms.Phases(6).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("EntropiaEspecficadaf2"), True)
                refval = ms.Phases(6).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(6).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(6).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massamolardafaselqui"), True)
                refval = ms.Phases(6).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Massaespecficadafase2"), True)
                refval = ms.Phases(6).Properties.massflow.GetValueOrDefault / ms.Phases(6).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                refval = ms.Phases(6).Properties.massflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(6).Properties.molarflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molarflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
                refval = ms.Phases(6).Properties.molarfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomolardafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomolardafasenamis"), True)
                refval = ms.Phases(6).Properties.massfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomssicadafase"), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fraomssicadafasenami"), True)
                refval = ms.Phases(6).Properties.compressibilityFactor.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add("Compressibility Factor (Z)", val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Fatordecompressibili"), True)
                refval = ms.Phases(6).Properties.heatCapacityCp.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.heatCapacityCp, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT("Heat Capacity at Constant Pressure (Cp)", su.heatCapacityCp), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Capacidadecalorficad"), True)
                refval = ms.Phases(6).Properties.heatCapacityCp.GetValueOrDefault / ms.Phases(6).Properties.heatCapacityCv.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then tmp = Format(refval, ms.FlowSheet.Options.NumberFormat) Else tmp = 0.0#
                pl.Add("Heat Capacity Ratio (Cp/Cv)", tmp, True, DWSIM.App.GetLocalString("lquida"), DWSIM.App.GetLocalString("Razoentreascapacidad"), True)
                refval = ms.Phases(6).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Condutividadetrmicad2"), True)
                refval = ms.Phases(6).Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then val = Format(Converter.ConvertFromSI(su.cinematic_viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadecinemtica2"), True)
                refval = ms.Phases(6).Properties.viscosity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity), val, True, DWSIM.App.GetLocalString("Lquido"), DWSIM.App.GetLocalString("Viscosidadedinmicada"), True)

                For Each it In pl
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P7] " & DWSIM.App.GetLocalString("Aqueous"), pl, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("PropriedadesdaFaseLq"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            If ms.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then

                Dim pl As New PropertyGridEx.CustomPropertyCollection()
                'PropertyGridEx.CustomPropertyCollection - Solid
                refval = ms.Phases(7).Properties.enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("EntalpiaEspecficadaf2"), True)
                refval = ms.Phases(7).Properties.entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("EntropiaEspecficadaf2"), True)
                refval = ms.Phases(7).Properties.molar_enthalpy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_enthalpy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("MolarEnthalpy"), True)
                refval = ms.Phases(7).Properties.molar_entropy.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molar_entropy, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("MolarEntropy"), True)
                refval = ms.Phases(7).Properties.molecularWeight.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molecularWeight, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Massamolardafaselqui"), True)
                refval = ms.Phases(7).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.density, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Massaespecfica"), su.density), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Massaespecficadafase2"), True)
                refval = ms.Phases(7).Properties.massflow.GetValueOrDefault / ms.Phases(7).Properties.density.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.volumetricFlow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("VazoTP"), su.volumetricFlow), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Vazovolumtricanascon"), True)
                refval = ms.Phases(7).Properties.massflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.massflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomssica"), su.massflow), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Vazomssicadacorrente"), True)
                refval = ms.Phases(7).Properties.molarflow.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.molarflow, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Vazomolar"), su.molarflow), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Vazomolardacorrente"), True)
                refval = ms.Phases(7).Properties.molarfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomolardafase"), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Fraomolardafasenamis"), True)
                refval = ms.Phases(7).Properties.massfraction.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add(DWSIM.App.GetLocalString("Fraomssicadafase"), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Fraomssicadafasenami"), True)
                refval = ms.Phases(7).Properties.compressibilityFactor.GetValueOrDefault
                If refval.HasValue = True Then val = Format(refval, ms.FlowSheet.Options.NumberFormat)
                pl.Add("Compressibility Factor (Z)", val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Fatordecompressibili"), True)
                refval = ms.Phases(7).Properties.heatCapacityCp.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.heatCapacityCp, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT("Heat Capacity at Constant Pressure (Cp)", su.heatCapacityCp), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Capacidadecalorficad"), True)
                refval = ms.Phases(7).Properties.heatCapacityCp.GetValueOrDefault / ms.Phases(7).Properties.heatCapacityCv.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then tmp = Format(refval, ms.FlowSheet.Options.NumberFormat) Else tmp = 0.0#
                pl.Add("Heat Capacity Ratio (Cp/Cv)", tmp, True, DWSIM.App.GetLocalString("lquida"), DWSIM.App.GetLocalString("Razoentreascapacidad"), True)
                refval = ms.Phases(7).Properties.thermalConductivity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.thermalConductivity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Condutividadetrmicad2"), True)
                refval = ms.Phases(7).Properties.kinematic_viscosity.GetValueOrDefault
                If refval.HasValue = True And Double.IsNaN(refval) = False Then val = Format(Converter.ConvertFromSI(su.cinematic_viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Viscosidadecinemtica2"), True)
                refval = ms.Phases(7).Properties.viscosity.GetValueOrDefault
                If refval.HasValue = True Then val = Format(Converter.ConvertFromSI(su.viscosity, refval), ms.FlowSheet.Options.NumberFormat)
                pl.Add(FT(DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity), val, True, DWSIM.App.GetLocalString("Solid"), DWSIM.App.GetLocalString("Viscosidadedinmicada"), True)

                For Each it In pl
                    it.DefaultValue = Nothing
                    it.DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add("[P8] " & DWSIM.App.GetLocalString("Solid"), pl, True, DWSIM.App.GetLocalString("Propriedades3"), DWSIM.App.GetLocalString("PropriedadesdaPhasesolida"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, uo As EnergyStream)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Conectadoaentrada"), ent, True, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
            End With

            .Item.Add(DWSIM.App.GetLocalString("Conectadoasada"), saida, True, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
            End With

            Dim valor = Format(Converter.ConvertFromSI(su.heatflow, uo.EnergyFlow.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetPropertyName("PROP_ES_0"), su.heatflow), valor, False, DWSIM.App.GetLocalString("Propriedades2"), DWSIM.App.GetLocalString("Quantidadedeenergiap"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_ES_0"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.InputConnectors(0).IsAttached Then
                If Not uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.AbsorptionColumn And
                        Not uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.DistillationColumn And
                        Not uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.ReboiledAbsorber And
                        Not uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.RefluxedAbsorber Then
                    .Item(2).IsReadOnly = True
                End If
            Else
                .Item(2).IsReadOnly = False
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As CapeOpenUO)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        uo.UpdatePortsFromConnectors()

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            'identify
            .Item.Add("Name", uo._seluo.Name, True, "1. CAPE-OPEN Object Info", "", True)
            .Item.Add("Description", uo._seluo.Description, True, "1. CAPE-OPEN Object Info", "", True)
            .Item.Add("ProgID", uo._seluo.TypeName, True, "1. CAPE-OPEN Object Info", "", True)
            .Item.Add("Version", uo._seluo.Version, True, "1. CAPE-OPEN Object Info", "", True)
            .Item.Add("CAPE-OPEN Version", uo._seluo.CapeVersion, True, "1. CAPE-OPEN Object Info", "", True)
            .Item.Add("File Location", uo._seluo.Location, True, "1. CAPE-OPEN Object Info", "", True)
            .Item.Add("Vendor URL", uo._seluo.VendorURL, True, "1. CAPE-OPEN Object Info", "", True)
            '.Item.Add("Help URL", _seluo.HelpURL, True, "1. CAPE-OPEN Object Info", "", True)

            'show edit form if available
            .Item.Add("Editing Form", "click to show ->", False, "2. Editing Form", "", True)
            .Item(.Item.Count - 1).OnClick = Function(sender As Object, e As EventArgs)
                                                 uo.Edit()
                                                 Return Nothing
                                             End Function

            Dim cnobj As Object = Nothing

            'populate ports
            For Each p As UnitPort In uo._ports
                If p.portType = CapePortType.CAPE_MATERIAL Then
                    Dim tag As String = ""
                    Try
                        cnobj = p.connectedObject
                    Catch ex As Exception
                        cnobj = Nothing
                    End Try
                    Dim conobj As MaterialStream = cnobj
                    If Not conobj Is Nothing Then tag = conobj.GraphicObject.Tag
                    .Item.Add(p.ComponentName + " [" + p.direction.ToString + ", " + p.portType.ToString() + "]", tag, False, "3. Ports", p.ComponentDescription, True)
                    With .Item(.Item.Count - 1)
                        .Tag = uo._ports.IndexOf(p)
                        If p.direction = CapePortDirection.CAPE_INLET Then
                            .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                        Else
                            .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                        End If
                    End With
                ElseIf p.portType = CapePortType.CAPE_ENERGY Then
                    Dim tag As String = ""
                    Try
                        cnobj = p.connectedObject
                    Catch ex As Exception
                        cnobj = Nothing
                    End Try
                    Dim conobj As EnergyStream = cnobj
                    If Not conobj Is Nothing Then tag = conobj.GraphicObject.Tag
                    .Item.Add(p.ComponentName + " [" + p.direction.ToString + ", " + p.portType.ToString() + "]", tag, False, "3. Ports", p.ComponentDescription, True)
                    With .Item(.Item.Count - 1)
                        .Tag = uo._ports.IndexOf(p)
                        If p.direction = CapePortDirection.CAPE_INLET Then
                            .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
                        Else
                            .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
                        End If
                    End With
                End If
            Next

            .Item.Add("Shape Override", uo.GraphicObject, "ShapeOverride", False, "4. Parameters", "Overrides the graphical representation of the object in the Flowsheet.", True)
            .Item.Add("Recalculate Output Streams", uo, "RecalcOutputStreams", False, "4. Parameters", "Recalculate output streams using the selected property package.", True)

            'If Not TryCast(uo._couo, ICapeKineticReactionContext) Is Nothing Then
            '    .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), uo.FlowSheet.Options.ReactionSets(uo.ReactionSetID).Name, False, "4. Parameters", DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
            '    With .Item(.Item.Count - 1)
            '        .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
            '        .IsDropdownResizable = True
            '    End With
            'End If

            'populate parameters
            For Each p As Object In uo._params
                Dim id As String = ""
                Dim desc As String = ""
                id = CType(p, ICapeIdentification).ComponentName
                desc = CType(p, ICapeIdentification).ComponentDescription
                'find parameter type
                Dim myp As ICapeParameterSpec = TryCast(p, ICapeParameterSpec)
                Select Case myp.Type
                    Case CapeParamType.CAPE_ARRAY
                        Dim par As CapeArrayParameter = p
                        Dim m2 As New PropertyGridEx.CustomPropertyCollection()
                        Dim i As Integer = 0
                        For Each o In DirectCast(par.value, System.Array)
                            m2.Add(i.ToString, o, True, "", "", True)
                            m2.Item(m2.Count - 1).IsReadOnly = True
                            i += 1
                        Next
                        .Item.Add(id, m2, If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                        With .Item(.Item.Count - 1)
                            .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    Case CapeParamType.CAPE_BOOLEAN
                        Dim par As BooleanParameter = TryCast(p, BooleanParameter)
                        .Item.Add(id, par, "Value", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                        .Item(.Item.Count - 1).Tag2 = id
                    Case CapeParamType.CAPE_INT
                        Dim par As IntegerParameter = TryCast(p, IntegerParameter)
                        .Item.Add(id, par, "Value", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                        .Item(.Item.Count - 1).Tag2 = id
                    Case CapeParamType.CAPE_OPTION
                        Dim par As OptionParameter = TryCast(p, OptionParameter)
                        .Item.Add(id, par, "Value", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                        .Item(.Item.Count - 1).Tag2 = id
                        With .Item(.Item.Count - 1)
                            If Not par.OptionList Is Nothing Then .Choices = New PropertyGridEx.CustomChoices(par.OptionList, False)
                        End With
                    Case CapeParamType.CAPE_REAL
                        Dim par As RealParameter = TryCast(p, RealParameter)
                        .Item.Add(id, par, "SIValue", If(par.Mode = CapeParamMode.CAPE_OUTPUT, True, False), "4. Parameters", desc, True)
                        .Item(.Item.Count - 1).Tag2 = id
                End Select
            Next

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea5"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As ComponentSeparator)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida1, saida2, en As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                en = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                en = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("OutletStream1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("OutletStream2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), en, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("CSepSpecStream"), uo, "SpecifiedStreamIndex", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = 0
            End With

            .Item.Add(DWSIM.App.GetLocalString("CSepSeparationSpecs"), uo, "ComponentSepSpecs", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
                .CustomEditor = New DWSIM.Editors.ComponentSeparator.UICSepSpecEditor
            End With

            .Item.Add(FT(DWSIM.App.GetLocalString("CSepEnergyImbalance"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.EnergyImb), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Compressor)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("HeaterCoolerCalcMode"), uo, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "CalcMode"

            Select Case uo.CalcMode
                Case Compressor.CalculationMode.Delta_P
                    Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("Delta P", su.deltaP), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_CO_0"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case Compressor.CalculationMode.OutletPressure
                    Dim valor = Format(Converter.ConvertFromSI(su.pressure, uo.POut), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Presso"), su.pressure), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Pressoajusante"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_CO_4"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
            End Select

            .Item.Add(DWSIM.App.GetLocalString("EficinciaAdiabtica01"), uo, "EficienciaAdiabatica", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciadocompresso"), True)
            With .Item(.Item.Count - 1)
                .Tag2 = "PROP_CO_1"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            .Item.Add(DWSIM.App.GetLocalString("IgnorarLquidonaEntra"), uo, "IgnorePhase", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("SelecioLiquidrueparaign2"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Boolean)
            End With

            .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            .Item.Add(FT(DWSIM.App.GetLocalString("Energianecessria"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Potnciarequeridapelo"), True)
            With .Item(.Item.Count - 1)
                .Tag2 = "PROP_CO_3"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Cooler)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                energ = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            End With

            Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad4"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_CL_0"
                .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
            End With

            .Item.Add(DWSIM.App.GetLocalString("HeaterCoolerCalcMode"), uo, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "CalcMode"

            Select Case uo.CalcMode

                Case Cooler.CalculationMode.HeatRemoved

                    valor = Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("CalorRemovido"), su.heatflow), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quantidadedecalorced"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_CL_3"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.heatflow, "E"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With

                Case Cooler.CalculationMode.OutletTemperature

                    valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_CL_2"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With

                Case Cooler.CalculationMode.OutletVaporFraction

                    valor = Format(uo.OutletVaporFraction.GetValueOrDefault, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(DWSIM.App.GetLocalString("FraomolardafaseFaseV"), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    .Item(.Item.Count - 1).Tag2 = "PROP_CL_4"

            End Select

            .Item.Add(DWSIM.App.GetLocalString("Eficincia0100"), uo, "Eficiencia", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciadoaquecedor2"), True)
            With .Item(.Item.Count - 1)
                .Tag2 = "PROP_CL_1"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated And Not uo.CalcMode = Cooler.CalculationMode.HeatRemoved Then
                valor = Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("CalorRemovido"), su.heatflow), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Quantidadedecalorced"), True)
            End If

            .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As CustomUO)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent1, ent2, ent3, ent4, ent5, ent6, ent7 As String

            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent1 = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent1 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                ent2 = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                ent2 = ""
            End If
            If uo.GraphicObject.InputConnectors(2).IsAttached = True Then
                ent3 = uo.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
            Else
                ent3 = ""
            End If
            If uo.GraphicObject.InputConnectors(3).IsAttached = True Then
                ent4 = uo.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag
            Else
                ent4 = ""
            End If
            If uo.GraphicObject.InputConnectors(4).IsAttached = True Then
                ent5 = uo.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag
            Else
                ent5 = ""
            End If
            If uo.GraphicObject.InputConnectors(5).IsAttached = True Then
                ent6 = uo.GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Tag
            Else
                ent6 = ""
            End If
            If uo.GraphicObject.InputConnectors(6).IsAttached = True Then
                ent7 = uo.GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom.Tag
            Else
                ent7 = ""
            End If

            Dim saida1, saida2, saida3, saida4, saida5, saida6, saida7 As String

            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.OutputConnectors(2).IsAttached = True Then
                saida3 = uo.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            Else
                saida3 = ""
            End If
            If uo.GraphicObject.OutputConnectors(3).IsAttached = True Then
                saida4 = uo.GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag
            Else
                saida4 = ""
            End If
            If uo.GraphicObject.OutputConnectors(4).IsAttached = True Then
                saida5 = uo.GraphicObject.OutputConnectors(4).AttachedConnector.AttachedTo.Tag
            Else
                saida5 = ""
            End If
            If uo.GraphicObject.OutputConnectors(5).IsAttached = True Then
                saida6 = uo.GraphicObject.OutputConnectors(5).AttachedConnector.AttachedTo.Tag
            Else
                saida6 = ""
            End If
            If uo.GraphicObject.OutputConnectors(6).IsAttached = True Then
                saida7 = uo.GraphicObject.OutputConnectors(6).AttachedConnector.AttachedTo.Tag
            Else
                saida7 = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), ent1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), ent2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada3"), ent3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada4"), ent5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada5"), ent6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada6"), ent7, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("CorrentedeEnergyFlowE"), ent4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida3"), saida3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida4"), saida5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida5"), saida6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida6"), saida7, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("CorrentedeEnergyFlowS"), saida4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("CUO_ScriptLanguage"), uo, "ExecutionEngine", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)

            .Item.Add(DWSIM.App.GetLocalString("InputVariables"), uo, "InputVariables", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            With .Item(.Item.Count - 1)
                If Not DWSIM.App.IsRunningOnMono Then
                    .CustomEditor = New Wexman.Design.GenericDictionaryEditor(Of String, Double)(Type.GetType("System.Collections.Generic.Dictionary(Of String, Double)")) With {.Title = DWSIM.App.GetLocalString("InputVariables")}
                End If
            End With

            .Item.Add(DWSIM.App.GetLocalString("CUO_ScriptText"), uo, "ScriptText", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Cliquenobotocomretic"), True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.CustomUO.UIScriptEditor
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea5"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            Else
                For Each p In uo.OutputVariables
                    .Item.Add(p.Key, p.Value, True, DWSIM.App.GetLocalString("OutputVariables"), DWSIM.App.GetLocalString(""), True)
                Next
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As ExcelUO)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent1, ent2, ent3, ent4, saida1, saida2, saida3, saida4, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent1 = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent1 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                ent2 = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                ent2 = ""
            End If
            If uo.GraphicObject.InputConnectors(2).IsAttached = True Then
                ent3 = uo.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
            Else
                ent3 = ""
            End If
            If uo.GraphicObject.InputConnectors(3).IsAttached = True Then
                ent4 = uo.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag
            Else
                ent4 = ""
            End If


            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.OutputConnectors(2).IsAttached = True Then
                saida3 = uo.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            Else
                saida3 = ""
            End If
            If uo.GraphicObject.OutputConnectors(3).IsAttached = True Then
                saida4 = uo.GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag
            Else
                saida4 = ""
            End If

            If uo.GraphicObject.InputConnectors(4).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            '==== Streams (1) =======================
            '==== Input streams ===
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), ent1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), ent2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada3"), ent3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada4"), ent4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            '==== Output streams ===
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida3"), saida3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida4"), saida4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            '==== Energy stream ===
            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With


            '==== Input Parameters (2) =======================
            '======== Input parameters from Excel ============
            For Each Prop As ExcelParameter In uo.InputParams.Values
                .Item.Add(FT(Prop.Name, Prop.Unit), Prop.Value, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), Prop.Annotation, True)
            Next

            .Item.Add(DWSIM.App.GetLocalString("ExcelUOEditor"), uo, "Filename", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("ExcelFile"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.ExcelUO.UIExcelUOEditor
            End With

            '==== Results (3) =================================
            '======== Output parameters from Excel ============
            For Each Prop As ExcelParameter In uo.OutputParams.Values
                .Item.Add(FT(Prop.Name, Prop.Unit), Prop.Value, True, DWSIM.App.GetLocalString("Resultados3"), Prop.Annotation, True)
            Next

            '======== heat due to enthalpy balance ============
            Dim valor = Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("CalorFornecido"), su.heatflow), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Quantidadedecalortro"), True)
            .Item(.Item.Count - 1).Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.heatflow, "E"}

            '========== Error message =========================
            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Filter)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida1, saida2, en As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                en = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                en = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("OutletStream1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("OutletStream2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), en, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            End With

            Dim value As Double

            value = Converter.ConvertFromSI(su.mediumresistance, uo.FilterMediumResistance)
            .Item.Add(FT(DWSIM.App.GetLocalString("FilterMediumResistance"), su.mediumresistance), Format(value, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterMediumResistanceDesc"), True)
            .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
            .Item(.Item.Count - 1).Tag2 = "PROP_FT_4"
            value = Converter.ConvertFromSI(su.cakeresistance, uo.SpecificCakeResistance)
            .Item.Add(FT(DWSIM.App.GetLocalString("FilterSpecificCakeResistance"), su.cakeresistance), Format(value, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterSpecificCakeResistanceDesc"), True)
            .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
            .Item(.Item.Count - 1).Tag2 = "PROP_FT_5"
            value = Converter.ConvertFromSI(su.tiuo, uo.FilterCycleTime)
            .Item.Add(FT(DWSIM.App.GetLocalString("FilterCycleTime"), su.time), Format(value, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterCycleTimeDesc"), True)
            .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
            .Item(.Item.Count - 1).Tag2 = "PROP_FT_3"

            .Item.Add(DWSIM.App.GetLocalString("FilterSubmergedAreaFraction"), uo, "SubmergedAreaFraction", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterSubmergedAreaFractionDesc"), True)
            .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
            .Item(.Item.Count - 1).Tag2 = "PROP_FT_6"
            .Item.Add(DWSIM.App.GetLocalString("FilterCakeRelativeHumidity"), uo, "CakeRelativeHumidity", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterCakeRelativeHumidityDesc"), True)
            .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
            .Item(.Item.Count - 1).Tag2 = "PROP_FT_2"

            .Item.Add(DWSIM.App.GetLocalString("FilterCalculationMode"), uo, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterCalculationModeDesc"), True)
            .Item(.Item.Count - 1).Tag2 = "CalcMode"

            Select Case uo.CalcMode
                Case Filter.CalculationMode.Design
                    value = Converter.ConvertFromSI(su.deltaP, uo.PressureDrop)
                    .Item.Add(FT(DWSIM.App.GetLocalString("FilterPressureDrop"), su.deltaP), Format(value, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterPressureDropDesc"), True)
                    .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
                    .Item(.Item.Count - 1).Tag2 = "PROP_FT_7"
                Case Filter.CalculationMode.Simulation
                    value = Converter.ConvertFromSI(su.area, uo.TotalFilterArea)
                    .Item.Add(FT(DWSIM.App.GetLocalString("FilterArea"), su.area), Format(value, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterAreaDesc"), True)
                    .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
                    .Item(.Item.Count - 1).Tag2 = "PROP_FT_1"
            End Select

            If uo.GraphicObject.Calculated Then
                Select Case uo.CalcMode
                    Case Filter.CalculationMode.Design
                        .Item.Add(FT(DWSIM.App.GetLocalString("FilterArea"), su.area), Format(Converter.ConvertFromSI(su.area, uo.TotalFilterArea), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterAreaDesc"), True)
                    Case Filter.CalculationMode.Simulation
                        .Item.Add(FT(DWSIM.App.GetLocalString("FilterPressureDrop"), su.deltaP), Format(Converter.ConvertFromSI(su.deltaP, uo.PressureDrop), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("FilterPressureDropDesc"), True)
                End Select
                .Item.Add(FT(DWSIM.App.GetLocalString("CSepEnergyImbalance"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.EnergyImb), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
            End If

            If uo.IsSpecAttached = True Then
                .Item.Add(DWSIM.App.GetLocalString("ObjetoUtilizadopor"), uo.FlowSheet.Collections.ObjectCollection(uo.AttachedSpecId).GraphicObject.Tag, True, DWSIM.App.GetLocalString("Miscelnea2"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), uo.SpecVarType, True, DWSIM.App.GetLocalString("Miscelnea3"), "", True)
            End If

            If Not uo.Annotation Is Nothing Then
                .Item.Add(DWSIM.App.GetLocalString("Anotaes"), uo, "Annotation", False, DWSIM.App.GetLocalString("Outros"), DWSIM.App.GetLocalString("Cliquenobotocomretic"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.Annotation.UIAnnotationEditor
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Flowsheet)


        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent1, ent2, ent3, ent4, ent5, ent6, ent7, ent8, ent9, ent10, saida1, saida2, saida3, saida4, saida5, saida6, saida7, saida8, saida9, saida10 As String

            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then ent1 = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag Else ent1 = ""
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then ent2 = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag Else ent2 = ""
            If uo.GraphicObject.InputConnectors(2).IsAttached = True Then ent3 = uo.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag Else ent3 = ""
            If uo.GraphicObject.InputConnectors(3).IsAttached = True Then ent4 = uo.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag Else ent4 = ""
            If uo.GraphicObject.InputConnectors(4).IsAttached = True Then ent5 = uo.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag Else ent5 = ""
            If uo.GraphicObject.InputConnectors(5).IsAttached = True Then ent6 = uo.GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Tag Else ent6 = ""
            If uo.GraphicObject.InputConnectors(6).IsAttached = True Then ent7 = uo.GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom.Tag Else ent7 = ""
            If uo.GraphicObject.InputConnectors(7).IsAttached = True Then ent8 = uo.GraphicObject.InputConnectors(7).AttachedConnector.AttachedFrom.Tag Else ent8 = ""
            If uo.GraphicObject.InputConnectors(8).IsAttached = True Then ent9 = uo.GraphicObject.InputConnectors(8).AttachedConnector.AttachedFrom.Tag Else ent9 = ""
            If uo.GraphicObject.InputConnectors(9).IsAttached = True Then ent10 = uo.GraphicObject.InputConnectors(9).AttachedConnector.AttachedFrom.Tag Else ent10 = ""

            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag Else saida1 = ""
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag Else saida2 = ""
            If uo.GraphicObject.OutputConnectors(2).IsAttached = True Then saida3 = uo.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag Else saida3 = ""
            If uo.GraphicObject.OutputConnectors(3).IsAttached = True Then saida4 = uo.GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag Else saida4 = ""
            If uo.GraphicObject.OutputConnectors(4).IsAttached = True Then saida5 = uo.GraphicObject.OutputConnectors(4).AttachedConnector.AttachedTo.Tag Else saida5 = ""
            If uo.GraphicObject.OutputConnectors(5).IsAttached = True Then saida6 = uo.GraphicObject.OutputConnectors(5).AttachedConnector.AttachedTo.Tag Else saida6 = ""
            If uo.GraphicObject.OutputConnectors(6).IsAttached = True Then saida7 = uo.GraphicObject.OutputConnectors(6).AttachedConnector.AttachedTo.Tag Else saida7 = ""
            If uo.GraphicObject.OutputConnectors(7).IsAttached = True Then saida8 = uo.GraphicObject.OutputConnectors(7).AttachedConnector.AttachedTo.Tag Else saida8 = ""
            If uo.GraphicObject.OutputConnectors(8).IsAttached = True Then saida9 = uo.GraphicObject.OutputConnectors(8).AttachedConnector.AttachedTo.Tag Else saida9 = ""
            If uo.GraphicObject.OutputConnectors(9).IsAttached = True Then saida10 = uo.GraphicObject.OutputConnectors(9).AttachedConnector.AttachedTo.Tag Else saida10 = ""

            '==== Streams (1) =======================
            '==== Input streams ===
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), ent1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), ent2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada3"), ent3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada4"), ent4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada5"), ent5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada6"), ent6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada7"), ent7, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada8"), ent8, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada9"), ent9, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada10"), ent10, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            '==== Output streams ===
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida3"), saida3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida4"), saida4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida5"), saida5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida6"), saida6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida7"), saida7, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida8"), saida8, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida9"), saida9, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida10"), saida10, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("SimulationFile"), uo, "SimulationFile", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("SimulationFileDesc"), True)
            .Item(.Item.Count - 1).CustomEditor = New PropertyGridEx.UIFilenameEditor

            If IO.File.Exists(uo.SimulationFile) Then
                .Item.Add(DWSIM.App.GetLocalString("FlowsheetUOEditor"), New DummyClass, False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("FlowsheetUOEditorDesc"), True)
                .Item(.Item.Count - 1).DefaultValue = Nothing
                .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.FlowsheetUO.UIFlowsheetUOEditor
            End If

            .Item.Add(DWSIM.App.GetLocalString("InitializeOnLoad"), uo, "InitializeOnLoad", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("InitializeOnLoadDesc"), True)
            .Item(.Item.Count - 1).Tag2 = "InitializeOnLoad"
            .Item.Add(DWSIM.App.GetLocalString("UpdateOnSave"), uo, "UpdateOnSave", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("UpdateOnSaveDesc"), True)
            .Item(.Item.Count - 1).Tag2 = "UpdateOnSave"
            .Item.Add(DWSIM.App.GetLocalString("RedirectOutput"), uo, "RedirectOutput", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("RedirectOutputDesc"), True)
            .Item(.Item.Count - 1).Tag2 = "RedirectOutput"

            If uo.Initialized Then

                .Item.Add(DWSIM.App.GetLocalString("FlowsheetUOViewer"), New DummyClass, False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("FlowsheetUOViewerDesc"), True)
                .Item(.Item.Count - 1).DefaultValue = Nothing
                .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.FlowsheetUO.UIFlowsheetUOViewer

                For Each p In uo.InputParams.Values
                    If uo.Fsheet.SimulationObjects.ContainsKey(p.ObjectID) Then
                        .Item.Add(uo.Fsheet.SimulationObjects(p.ObjectID).GraphicObject.Tag & ", " &
                                      DWSIM.App.GetPropertyName(p.ObjectProperty) &
                                      " (" & uo.Fsheet.SimulationObjects(p.ObjectID).GetPropertyUnit(p.ObjectProperty, uo.FlowSheet.Options.SelectedUnitSystem) & ")",
                                      uo.Fsheet.SimulationObjects(p.ObjectID).GetPropertyValue(p.ObjectProperty, uo.FlowSheet.Options.SelectedUnitSystem), False,
                                      DWSIM.App.GetLocalString("LinkedInputParms"), DWSIM.App.GetLocalString(""), True)
                        .Item(.Item.Count - 1).Tag = p.ID
                        .Item(.Item.Count - 1).Tag2 = "[I][" & p.ID & "]"
                    End If
                Next

                For Each p In uo.OutputParams.Values
                    If uo.Fsheet.SimulationObjects.ContainsKey(p.ObjectID) Then
                        .Item.Add(uo.Fsheet.SimulationObjects(p.ObjectID).GraphicObject.Tag & ", " &
                                      DWSIM.App.GetPropertyName(p.ObjectProperty) &
                                      " (" & uo.Fsheet.SimulationObjects(p.ObjectID).GetPropertyUnit(p.ObjectProperty, uo.FlowSheet.Options.SelectedUnitSystem) & ")",
                                      uo.Fsheet.SimulationObjects(p.ObjectID).GetPropertyValue(p.ObjectProperty, uo.FlowSheet.Options.SelectedUnitSystem), True,
                                      DWSIM.App.GetLocalString("LinkedOutputParms"), DWSIM.App.GetLocalString(""), True)
                        .Item(.Item.Count - 1).Tag = p.ID
                    End If
                Next

            End If

            If uo.Calculated Then

                .Item.Add(DWSIM.App.GetLocalString("MassBalanceError"), Format(uo.MassBalanceError, uo.FlowSheet.Options.NumberFormat), True, "5. " & DWSIM.App.GetLocalString("Resultados"), DWSIM.App.GetLocalString(""), True)

            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Heater)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad3"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_HT_0"
                .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
            End With


            .Item.Add(DWSIM.App.GetLocalString("HeaterCoolerCalcMode"), uo, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "CalcMode"

            Select Case uo.CalcMode

                Case Heater.CalculationMode.EnergyStream

                Case Heater.CalculationMode.HeatAdded

                    valor = Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("CalorFornecido"), su.heatflow), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quantidadedecalorced"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HT_3"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.heatflow, "E"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With

                Case Heater.CalculationMode.OutletTemperature

                    valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HT_2"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With

                Case Heater.CalculationMode.OutletVaporFraction

                    valor = Format(uo.OutletVaporFraction.GetValueOrDefault, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(DWSIM.App.GetLocalString("FraomolardafaseFaseV"), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    With .Item(.Item.Count - 1)
                        .Tag2 = "PROP_HT_4"
                    End With

            End Select

            .Item.Add(DWSIM.App.GetLocalString("Eficincia0100"), uo, "Eficiencia", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciadoaquecedor"), True)
            With .Item(.Item.Count - 1)
                .Tag2 = "PROP_HT_1"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated And Not uo.CalcMode = Heater.CalculationMode.HeatAdded Then
                valor = Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("CalorFornecido"), su.heatflow), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Quantidadedecalorced"), True)
            End If

            .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If


            BasePopulatePropertyGrid(pgrid, uo)


        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As UnitOperations.UnitOperations.HeatExchanger)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        'To be implemented according to the heat exchanger connectors.

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim In1, In2, Out1, Out2, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                In1 = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                In1 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                In2 = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                In2 = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                Out1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                Out1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                Out2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                Out2 = ""
            End If

            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                energ = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), In1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), In2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida1"), Out1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida2"), Out2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("HXCalculationMode"), uo, "CalculationMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXCalculationModeDesc"), True)
            .Item.Add(DWSIM.App.GetLocalString("HXIgnoreLMTDError"), uo, "IgnoreLMTDError", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXIgnoreLMTDErrorDesc"), True)

            Dim AValue As Double

            Select Case uo.CalculationMode
                Case HeatExchangerCalcMode.CalcArea, HeatExchangerCalcMode.CalcBothTemp, HeatExchangerCalcMode.CalcBothTemp_UA, HeatExchangerCalcMode.CalcTempColdOut, HeatExchangerCalcMode.CalcTempHotOut
                    .Item.Add(DWSIM.App.GetLocalString("HXFlowDirection"), uo, "FlowDir", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXFlowDirectionDesc"), True)
                    AValue = Format(Converter.ConvertFromSI(su.deltaP, uo.HotSidePressureDrop), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSidePressureDrop"), su.deltaP), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXHotSidePressureDrop"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    AValue = Format(Converter.ConvertFromSI(su.deltaP, uo.ColdSidePressureDrop), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSidePressureDrop"), su.deltaP), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXColdSidePressureDrop"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
            End Select

            Select Case uo.CalculationMode
                Case HeatExchangerCalcMode.CalcBothTemp_UA
                    AValue = Format(Converter.ConvertFromSI(su.area, uo.Area), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Area"), su.area), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HeatTransferArea"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_1"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.area, "A"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_0"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.heat_transf_coeff, "U"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case HeatExchangerCalcMode.CalcBothTemp
                    AValue = Format(Converter.ConvertFromSI(su.area, uo.Area), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Area"), su.area), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HeatTransferArea"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_1"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.area, "A"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    AValue = Format(Converter.ConvertFromSI(su.heatflow, uo.Q), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HeatLoad"), su.heatflow), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HeatLoadOfEquipment"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_2"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.heatflow, "E"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case HeatExchangerCalcMode.CalcTempColdOut
                    AValue = Format(Converter.ConvertFromSI(su.area, uo.Area), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Area"), su.area), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HeatTransferArea"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_1"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.area, "A"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    AValue = Format(Converter.ConvertFromSI(su.temperature, uo.HotSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSideOutletTemperature"), su.temperature), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXHotSideOutletTemperatureDesc"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_4"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case HeatExchangerCalcMode.CalcTempHotOut
                    AValue = Format(Converter.ConvertFromSI(su.area, uo.Area), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Area"), su.area), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HeatTransferArea"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_1"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.area, "A"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    AValue = Format(Converter.ConvertFromSI(su.temperature, uo.ColdSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSideOutletTemperature"), su.temperature), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXColdSideOutletTemperatureDesc"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_3"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case HeatExchangerCalcMode.CalcArea
                    .Item.Add(DWSIM.App.GetLocalString("HXDefinedTemperature"), uo, "DefinedTemperature", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXDefinedTemperatureDesc"), True)
                    Select Case uo.DefinedTemperature
                        Case SpecifiedTemperature.Cold_Fluid
                            AValue = Format(Converter.ConvertFromSI(su.temperature, uo.ColdSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                            .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSideOutletTemperature"), su.temperature), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXColdSideOutletTemperatureDesc"), True)
                            With .Item(.Item.Count - 1)
                                .CustomTypeConverter = New System.ComponentModel.StringConverter
                                .Tag2 = "PROP_HX_3"
                                .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                            End With
                        Case SpecifiedTemperature.Hot_Fluid
                            AValue = Format(Converter.ConvertFromSI(su.temperature, uo.HotSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                            .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSideOutletTemperature"), su.temperature), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXHotSideOutletTemperatureDesc"), True)
                            With .Item(.Item.Count - 1)
                                .CustomTypeConverter = New System.ComponentModel.StringConverter
                                .Tag2 = "PROP_HX_4"
                                .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                            End With
                    End Select
                    AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                Case HeatExchangerCalcMode.ShellandTube_Rating
                    .Item.Add(DWSIM.App.GetLocalString("STHXProperties"), uo, "STProperties", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("STHXPropertiesDesc"), True)
                    With .Item(.Item.Count - 1)
                        .CustomEditor = New DWSIM.Editors.HeatExchanger.UISTHXEditor
                    End With
                Case HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor
                    AValue = Format(Converter.ConvertFromSI(su.temperature, uo.ColdSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSideOutletTemperature"), su.temperature), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXColdSideOutletTemperatureDesc"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_3"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    AValue = Format(Converter.ConvertFromSI(su.temperature, uo.HotSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSideOutletTemperature"), su.temperature), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("HXHotSideOutletTemperatureDesc"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_HX_4"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    .Item.Add(DWSIM.App.GetLocalString("STHXProperties"), uo, "STProperties", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("STHXPropertiesDesc"), True)
                    With .Item(.Item.Count - 1)
                        .CustomEditor = New DWSIM.Editors.HeatExchanger.UISTHXEditor
                    End With
                Case HeatExchangerCalcMode.PinchPoint
                    AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                    AValue = Format(Converter.ConvertFromSI(su.deltaT, uo.MITA), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("Minimum Internal Temperature Approach (MITA)", su.deltaT), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                Case HeatExchangerCalcMode.ThermalEfficiency
                    AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                    .Item.Add("Thermal Efficiency (%)", uo, "ThermalEfficiency", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            End Select

            If uo.GraphicObject.Calculated = True Then

                'Shows some calculated properties, such as the heat load.

                Select Case uo.CalculationMode
                    Case HeatExchangerCalcMode.CalcBothTemp_UA
                        AValue = Format(Converter.ConvertFromSI(su.heatflow, uo.Q), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HeatLoad"), su.heatflow), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatLoadOfEquipment"), True)
                    Case HeatExchangerCalcMode.CalcBothTemp
                        AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.temperature, uo.HotSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXHotSideOutletTemperatureDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.temperature, uo.ColdSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXColdSideOutletTemperatureDesc"), True)
                    Case HeatExchangerCalcMode.CalcTempColdOut
                        AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.heatflow, uo.Q), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HeatLoad"), su.heatflow), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatLoadOfEquipment"), True)
                        AValue = Format(Converter.ConvertFromSI(su.temperature, uo.ColdSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXColdSideOutletTemperatureDesc"), True)
                    Case HeatExchangerCalcMode.CalcTempHotOut
                        AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.heatflow, uo.Q), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HeatLoad"), su.heatflow), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatLoadOfEquipment"), True)
                        AValue = Format(Converter.ConvertFromSI(su.temperature, uo.HotSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXHotSideOutletTemperatureDesc"), True)
                    Case HeatExchangerCalcMode.CalcArea
                        Select Case uo.DefinedTemperature
                            Case SpecifiedTemperature.Cold_Fluid
                                AValue = Format(Converter.ConvertFromSI(su.temperature, uo.HotSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                                .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXHotSideOutletTemperatureDesc"), True)
                            Case SpecifiedTemperature.Hot_Fluid
                                AValue = Format(Converter.ConvertFromSI(su.temperature, uo.ColdSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                                .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXColdSideOutletTemperatureDesc"), True)
                        End Select
                        AValue = Format(Converter.ConvertFromSI(su.area, uo.Area), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("Area"), su.area), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatTransferArea"), True)
                        AValue = Format(Converter.ConvertFromSI(su.heatflow, uo.Q), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HeatLoad"), su.heatflow), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatLoadOfEquipment"), True)
                    Case HeatExchangerCalcMode.ShellandTube_Rating
                        AValue = Format(Converter.ConvertFromSI(su.temperature, uo.HotSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXHotSideOutletTemperatureDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.temperature, uo.ColdSideOutletTemperature), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSideOutletTemperature"), su.temperature), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXColdSideOutletTemperatureDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.heatflow, uo.Q), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HeatLoad"), su.heatflow), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatLoadOfEquipment"), True)
                        AValue = Format(Converter.ConvertFromSI(su.area, uo.Area), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("Area"), su.area), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatTransferArea"), True)
                        AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.deltaP, uo.HotSidePressureDrop), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSidePressureDrop"), su.deltaP), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXHotSidePressureDrop"), True)
                        AValue = Format(Converter.ConvertFromSI(su.deltaP, uo.ColdSidePressureDrop), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSidePressureDrop"), su.deltaP), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXColdSidePressureDrop"), True)
                        AValue = Format(uo.LMTD_F, uo.FlowSheet.Options.NumberFormat)
                        .Item.Add("F", AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXColdSidePressureDrop"), True)
                    Case HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor
                        AValue = Converter.ConvertFromSI(su.foulingfactor, uo.STProperties.OverallFoulingFactor)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXFoulingFactor"), su.foulingfactor), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXFoulingFactorDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.heatflow, uo.Q), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HeatLoad"), su.heatflow), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatLoadOfEquipment"), True)
                        AValue = Format(Converter.ConvertFromSI(su.area, uo.Area), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("Area"), su.area), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HeatTransferArea"), True)
                        AValue = Format(Converter.ConvertFromSI(su.heat_transf_coeff, uo.OverallCoefficient), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient"), su.heat_transf_coeff), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("OverallHeatTranferCoefficientDesc"), True)
                        AValue = Format(Converter.ConvertFromSI(su.deltaP, uo.HotSidePressureDrop), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXHotSidePressureDrop"), su.deltaP), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXHotSidePressureDrop"), True)
                        AValue = Format(Converter.ConvertFromSI(su.deltaP, uo.ColdSidePressureDrop), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HXColdSidePressureDrop"), su.deltaP), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXColdSidePressureDrop"), True)
                        AValue = Format(uo.LMTD_F, uo.FlowSheet.Options.NumberFormat)
                        .Item.Add("F", AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXLMTDCorr"), True)
                End Select

                .Item.Add(FT(DWSIM.App.GetLocalString("MaximumHeatExchange"), su.heatflow), Format(uo.MaxHeatExchange.ConvertFromSI(su.heatflow), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("MaximumHeatExchangeDesc"), True)
                .Item.Add(FT(DWSIM.App.GetLocalString("ThermalEfficiency"), "%"), Format(uo.ThermalEfficiency, uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("ThermalEfficiencyDesc"), True)

                AValue = Format(Converter.ConvertFromSI(su.deltaT, uo.LMTD), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("HXLMTD"), su.deltaT), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXLMTDDesc"), True)

                If uo.CalculationMode = HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor Or uo.CalculationMode = HeatExchangerCalcMode.ShellandTube_Rating Then
                    AValue = Format(uo.STProperties.ReS, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add("Re Shell", AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXReShell"), True)
                    AValue = Format(uo.STProperties.ReT, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add("Re Tube", AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXReTube"), True)
                    AValue = Format(uo.STProperties.Fs, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("F Shell", su.foulingfactor), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXFShell"), True)
                    AValue = Format(uo.STProperties.Ft, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("F Tube", su.foulingfactor), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXFTube"), True)
                    AValue = Format(uo.STProperties.Fc, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("F Pipe", su.foulingfactor), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXFPipe"), True)
                    AValue = Format(uo.STProperties.Ff, uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("F Fouling", su.foulingfactor), AValue, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("HXFFouling"), True)
                End If

            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Mixer)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent1, ent2, ent3, ent4, ent5, ent6, saida As String

            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent1 = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent1 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                ent2 = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                ent2 = ""
            End If
            If uo.GraphicObject.InputConnectors(2).IsAttached = True Then
                ent3 = uo.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
            Else
                ent3 = ""
            End If
            If uo.GraphicObject.InputConnectors(3).IsAttached = True Then
                ent4 = uo.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag
            Else
                ent4 = ""
            End If
            If uo.GraphicObject.InputConnectors(4).IsAttached = True Then
                ent5 = uo.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag
            Else
                ent5 = ""
            End If
            If uo.GraphicObject.InputConnectors(5).IsAttached = True Then
                ent6 = uo.GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Tag
            Else
                ent6 = ""
            End If

            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), ent1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), ent2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada3"), ent3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada4"), ent4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada5"), ent5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada6"), ent6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Conectadoasada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Pressoajusante"), uo, "PressureCalculation", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Selecioneumaopoquein"), True)
            .Item(.Item.Count - 1).Tag2 = "PressureCalculation"
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
            End With

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As OrificePlate)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            '.Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            'With .Item(.Item.Count - 1)
            '    .DefaultValue = Nothing
            '    .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            'End With

            '.Item.Add(DWSIM.App.GetLocalString("OPCalculationMethod"), uo, "CalculationMethod", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("OPOrificeType"), uo, "OrifType", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "OrifType"

            Dim valor As Double = 0

            valor = Converter.ConvertFromSI(su.diameter, uo.OrificeDiameter)
            .Item.Add(FT(DWSIM.App.GetLocalString("OPOrificeDiameter"), su.diameter), Format(valor, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
            .Item(.Item.Count - 1).Tag2 = "PROP_OP_1"
            .Item.Add(DWSIM.App.GetLocalString("OPBeta"), Format(uo.Beta, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "PROP_OP_2"
            .Item.Add(DWSIM.App.GetLocalString("OPCorrectionFactor"), Format(uo.CorrectionFactor, uo.FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "PROP_OP_3"

            .Item.Add(FT(DWSIM.App.GetLocalString("OPOrificePressureDrop"), su.deltaP), Format(Converter.ConvertFromSI(su.deltaP, uo.OrificePressureDrop), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            .Item(.Item.Count - 1).Tag2 = "PROP_OP_4"
            .Item.Add(FT(DWSIM.App.GetLocalString("OPOverallPressureDrop"), su.deltaP), Format(Converter.ConvertFromSI(su.deltaP, uo.OverallPressureDrop), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            .Item(.Item.Count - 1).Tag2 = "PROP_OP_5"
            .Item.Add(FT(DWSIM.App.GetLocalString("OPDeltaT"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            .Item(.Item.Count - 1).Tag2 = "PROP_OP_6"


            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As UnitOperations.UnitOperations.Pipe)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                energ = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("PerfilHidrulico"), uo, False, DWSIM.App.GetLocalString("Perfis2"), DWSIM.App.GetLocalString("Cliquenobotocomretic2"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Pipe.PipeProfile)
                .CustomEditor = New DWSIM.Editors.PipeEditor.UIPipeEditor
                .CustomTypeConverter = New DWSIM.Editors.PipeEditor.PipeEditorConverter
            End With
            .Item.Add(DWSIM.App.GetLocalString("Equaopfluxo"), uo, "SelectedFlowPackage", False, DWSIM.App.GetLocalString("Perfis2"), DWSIM.App.GetLocalString("Selecioneaequaoparac"), True)
            .Item(.Item.Count - 1).Tag2 = "SelectedFlowPackage"
            .Item.Add("Status", uo.Profile, "Status", True, DWSIM.App.GetLocalString("Perfis2"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("PerfilTrmico"), uo, False, DWSIM.App.GetLocalString("Perfis2"), DWSIM.App.GetLocalString("Cliquenobotocomretic3"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Global.DWSIM.DWSIM.Editors.PipeEditor.ThermalEditorDefinitions)
                .CustomEditor = New DWSIM.Editors.PipeEditor.UIThermalProfileEditor
                .CustomTypeConverter = New DWSIM.Editors.PipeEditor.ThermalProfileEditorConverter
            End With

            .Item.Add(DWSIM.App.GetLocalString("PipeSpecMode"), uo, "Specification", False, DWSIM.App.GetLocalString("Parmetros3"), DWSIM.App.GetLocalString("PipeSpecModeDesc"), True)
            .Item(.Item.Count - 1).Tag2 = "Specification"

            Dim valor As Double

            Select Case uo.Specification
                Case UnitOperations.UnitOperations.Pipe.Specmode.Length
                Case UnitOperations.UnitOperations.Pipe.Specmode.OutletPressure
                    valor = Format(Converter.ConvertFromSI(su.pressure, uo.OutletPressure), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("ValveOutletPressure"), su.pressure), valor, False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
                    With .Item(.Item.Count - 1)
                        .Tag2 = "PROP_PI_3"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case UnitOperations.UnitOperations.Pipe.Specmode.OutletTemperature
                    valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), valor, False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
                    With .Item(.Item.Count - 1)
                        .Tag2 = "PROP_PI_4"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
            End Select

            .Item.Add(DWSIM.App.GetLocalString("MximodeIteraesemP"), uo, "MaxPressureIterations", False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("MximodeIteraesemT"), uo, "MaxTemperatureIterations", False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("IncludeJTEffect"), uo, "IncludeJTEffect", False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
            '.Item.Add(DWSIM.App.GetLocalString("Tolernciapreclculode") & " (%)", uo, "TriggerFlashP", False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
            '.Item.Add(DWSIM.App.GetLocalString("Tolernciapreclculode2") & " (%)", uo, "TriggerFlashT", False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("Erromximodapresso") & " (Pa)", uo, "TolP", False, DWSIM.App.GetLocalString("Parmetros3"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("Erromximodatemperatu") & " (K)", uo, "TolT", False, DWSIM.App.GetLocalString("Parmetros3"), "", True)

            .Item.Add(FT("Delta P", su.deltaP), Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados4"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados4"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            .Item.Add(FT(DWSIM.App.GetLocalString("Calortrocado"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados4"), DWSIM.App.GetLocalString("Quantidadedecalortro"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            .Item.Add(DWSIM.App.GetLocalString("Tabela"), uo, False, DWSIM.App.GetLocalString("Resultados4"), DWSIM.App.GetLocalString("Cliquenobotocomretic4"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Global.DWSIM.DWSIM.Editors.PipeEditor.ThermalEditorDefinitions)
                .CustomEditor = New DWSIM.Editors.Results.UIFormTable
            End With

            .Item.Add(DWSIM.App.GetLocalString("Grfico"), uo, False, DWSIM.App.GetLocalString("Resultados4"), DWSIM.App.GetLocalString("Cliquenobotocomretic5"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Global.DWSIM.DWSIM.Editors.PipeEditor.ThermalEditorDefinitions)
                .CustomEditor = New DWSIM.Editors.Results.UIFormGraph
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea5"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Pump)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("PumpCalcMode"), uo, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "CalcMode"

            Select Case uo.CalcMode
                Case Pump.CalculationMode.Curves
                    .Item.Add(DWSIM.App.GetLocalString("PumpSetupCurves"), uo, "Curves", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                    With .Item(.Item.Count - 1)
                        .DefaultValue = Nothing
                        .CustomEditor = New DWSIM.Editors.Pump.UIPumpCurvesEditor
                    End With
                    If uo.Curves.ContainsKey("EFF") Then
                        If Not uo.Curves("EFF").Enabled Then
                            .Item.Add(DWSIM.App.GetLocalString("Eficincia0100"), uo, "Eficiencia", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciaglobaldabom"), True)
                            With .Item(.Item.Count - 1)
                                .Tag2 = "PROP_PI_1"
                            End With
                        End If
                    End If
                Case Pump.CalculationMode.Delta_P
                    Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("Delta P", su.deltaP), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_PI_0"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    .Item.Add(DWSIM.App.GetLocalString("Eficincia0100"), uo, "Eficiencia", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciaglobaldabom"), True)
                    With .Item(.Item.Count - 1)
                        .Tag2 = "PROP_PI_1"
                    End With
                Case Pump.CalculationMode.EnergyStream
                    .Item.Add(DWSIM.App.GetLocalString("Eficincia0100"), uo, "Eficiencia", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciaglobaldabom"), True)
                    With .Item(.Item.Count - 1)
                        .Tag2 = "PROP_PI_1"
                    End With
                Case Pump.CalculationMode.Power
                    Dim valor = Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Energianecessria"), su.heatflow), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Potnciarequeridapela"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_PI_3"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "E"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case Pump.CalculationMode.OutletPressure
                    Dim valor = Format(Converter.ConvertFromSI(su.pressure, uo.Pout), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Pressoajusante"), su.pressure), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    .Item.Add(DWSIM.App.GetLocalString("Eficincia0100"), uo, "Eficiencia", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciaglobaldabom"), True)
                    With .Item(.Item.Count - 1)
                        .Tag2 = "PROP_PI_1"
                    End With
            End Select

            .Item.Add(DWSIM.App.GetLocalString("IgnorarVapornaEntrad"), uo, "IgnorePhase", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("SelecioLiquidrueparaign"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Boolean)
            End With

            If uo.GraphicObject.Calculated Then
                Select Case uo.CalcMode
                    Case Pump.CalculationMode.Curves
                        .Item.Add(DWSIM.App.GetLocalString("PumpViewCurves"), uo, "Curves", False, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .CustomEditor = New DWSIM.Editors.Pump.UIPumpCurvesEditor
                        End With
                        .Item.Add(FT(DWSIM.App.GetLocalString("PumpCurveHead"), su.head), Format(Converter.ConvertFromSI(su.head, uo.CurveHead), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                        .Item.Add(DWSIM.App.GetLocalString("PumpCurveEfficiency"), Format(uo.CurveEff, uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                        .Item.Add(FT(DWSIM.App.GetLocalString("PumpCurvePower"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.CurvePower), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                        Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT("Delta P", su.deltaP), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                        .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                        .Item.Add(FT(DWSIM.App.GetLocalString("PumpCurveNPSHr"), su.head), Format(Converter.ConvertFromSI(su.head, uo.CurveNPSHr), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                    Case Pump.CalculationMode.Delta_P
                        .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                        .Item.Add(FT(DWSIM.App.GetLocalString("Energianecessria"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Potnciarequeridapela"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                    Case Pump.CalculationMode.EnergyStream, Pump.CalculationMode.Power
                        Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT("Delta P", su.deltaP), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                        .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                    Case Pump.CalculationMode.OutletPressure
                        Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT("Delta P", su.deltaP), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                        .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                        .Item.Add(FT(DWSIM.App.GetLocalString("Energianecessria"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Potnciarequeridapela"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                End Select
            End If
            .Item.Add(FT(DWSIM.App.GetLocalString("PumpNPSHd"), su.head), Format(Converter.ConvertFromSI(su.head, uo.NPSH.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Potnciarequeridapela"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Column)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        Dim nf As String = uo.FlowSheet.Options.NumberFormat

        Dim obj = uo.Specs

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            .Item.Add(DWSIM.App.GetLocalString("DCNumbStages"), uo, "NumberOfStages", False, DWSIM.App.GetLocalString("DCStages"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
            End With

            .Item.Add(DWSIM.App.GetLocalString("DCStagesE"), uo, False, DWSIM.App.GetLocalString("DCStages"), "", True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.Distillation.UIStagesEditor
            End With

            Dim val As Double

            Select Case uo.ColumnType
                Case ColType.DistillationColumn
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.CondenserPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    .Item(.Item.Count - 1).Tag2 = "PROP_DC_0"
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.ReboilerPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    .Item(.Item.Count - 1).Tag2 = "PROP_DC_1"
                    .Item.Add(DWSIM.App.GetLocalString("DCCondenserType"), uo, "CondenserType", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    val = Format(Converter.ConvertFromSI(su.deltaP, uo.CondenserDeltaP), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDeltaP"), su.deltaP), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    '.Item.Add(DWSIM.App.GetLocalString("DCRefluxRatio"), uo, "RefluxRatio", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    Dim units As String() = New String() {}

                    Dim cspec As New PropertyGridEx.CustomPropertyCollection()
                    With cspec
                        'condenser spec
                        If uo.CondenserType = condtype.Partial_Condenser Then
                            Dim cspecv As New PropertyGridEx.CustomPropertyCollection()
                            With cspecv
                                .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), uo, "VaporFlowRate", False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                                .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), uo, "VaporFlowRateUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                With .Item(.Count - 1)
                                    .Choices = New PropertyGridEx.CustomChoices(units, False)
                                End With
                            End With
                            .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), cspecv, False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                            With .Item(.Count - 1)
                                .IsBrowsable = True
                                .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                                .CustomEditor = New System.Drawing.Design.UITypeEditor
                            End With
                        End If
                        .Add(DWSIM.App.GetLocalString("DCCondenserSpecType"), uo.Specs("C"), "SType", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                        If uo.Specs("C").SType = ColumnSpec.SpecType.Component_Fraction Or
                                uo.Specs("C").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or
                                uo.Specs("C").SType = ColumnSpec.SpecType.Component_Recovery Or
                                uo.Specs("C").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                            .Add(DWSIM.App.GetLocalString("DCCondenserSpecComp"), DWSIM.App.GetLocalString(uo.Specs("C").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            With .Item(.Count - 1)
                                .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                            End With
                        End If
                        .Add(DWSIM.App.GetLocalString("DCCondenserSpecValue"), uo.Specs("C"), "SpecValue", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                        .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), uo.Specs("C"), "SpecUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                        Select Case uo.Specs("C").SType
                            Case ColumnSpec.SpecType.Component_Fraction
                                units = New String() {"M", "We"}
                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                            Case ColumnSpec.SpecType.Component_Recovery
                                units = New String() {"% M/M", "% W/W"}
                            Case ColumnSpec.SpecType.Heat_Duty
                                units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                            Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                            Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                            Case ColumnSpec.SpecType.Stream_Ratio
                                units = New String() {""}
                            Case ColumnSpec.SpecType.Temperature
                                units = New String() {"K", "R", "C", "F"}
                        End Select
                        With .Item(.Count - 1)
                            .Choices = New PropertyGridEx.CustomChoices(units, False)
                        End With
                    End With
                    .Item.Add(DWSIM.App.GetLocalString("DCCondenserSpecs"), cspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                    Dim rspec As New PropertyGridEx.CustomPropertyCollection()
                    With rspec
                        'reboiler spec
                        .Add(DWSIM.App.GetLocalString("DCReboilerSpecType"), uo.Specs("R"), "SType", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                        If uo.Specs("R").SType = ColumnSpec.SpecType.Component_Fraction Or
                               uo.Specs("R").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or
                               uo.Specs("R").SType = ColumnSpec.SpecType.Component_Recovery Or
                               uo.Specs("R").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecComp"), DWSIM.App.GetLocalString(uo.Specs("R").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            With .Item(.Count - 1)
                                .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                            End With
                        End If
                        .Add(DWSIM.App.GetLocalString("DCReboilerSpecValue"), uo.Specs("R"), "SpecValue", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                        .Add(DWSIM.App.GetLocalString("DCReboilerSpecUnit"), uo.Specs("R"), "SpecUnit", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                        Select Case uo.Specs("R").SType
                            Case ColumnSpec.SpecType.Component_Fraction
                                units = New String() {"M", "We"}
                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                            Case ColumnSpec.SpecType.Component_Recovery
                                units = New String() {"% M/M", "% W/W"}
                            Case ColumnSpec.SpecType.Heat_Duty
                                units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                            Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                            Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                            Case ColumnSpec.SpecType.Stream_Ratio
                                units = New String() {""}
                            Case ColumnSpec.SpecType.Temperature
                                units = New String() {"K", "R", "C", "F"}
                        End Select
                        With .Item(.Count - 1)
                            .Choices = New PropertyGridEx.CustomChoices(units, False)
                        End With
                    End With
                    .Item.Add(DWSIM.App.GetLocalString("DCReboilerSpecs"), rspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                Case ColType.AbsorptionColumn
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.CondenserPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCFirstStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.ReboilerPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCLastStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCOperationMode"), uo, "OperationMode", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                Case ColType.ReboiledAbsorber
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.CondenserPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCFirstStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.ReboilerPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    Dim units As String() = New String() {}
                    Dim rspec As New PropertyGridEx.CustomPropertyCollection()
                    With rspec
                        'reboiler spec
                        .Add(DWSIM.App.GetLocalString("DCReboilerSpecType"), uo.Specs("R"), "SType", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                        If uo.Specs("R").SType = ColumnSpec.SpecType.Component_Fraction Or
                               uo.Specs("R").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or
                               uo.Specs("R").SType = ColumnSpec.SpecType.Component_Recovery Or
                               uo.Specs("R").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecComp"), DWSIM.App.GetLocalString(uo.Specs("R").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            With .Item(.Count - 1)
                                .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                            End With
                        End If
                        .Add(DWSIM.App.GetLocalString("DCReboilerSpecValue"), uo.Specs("R"), "SpecValue", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                        .Add(DWSIM.App.GetLocalString("DCReboilerSpecUnit"), uo.Specs("R"), "SpecUnit", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                        Select Case uo.Specs("R").SType
                            Case ColumnSpec.SpecType.Component_Fraction
                                units = New String() {"M", "We"}
                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                            Case ColumnSpec.SpecType.Component_Recovery
                                units = New String() {"% M/M", "% W/W"}
                            Case ColumnSpec.SpecType.Heat_Duty
                                units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                            Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                            Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                            Case ColumnSpec.SpecType.Stream_Ratio
                                units = New String() {""}
                            Case ColumnSpec.SpecType.Temperature
                                units = New String() {"K", "R", "C", "F"}
                        End Select
                        With .Item(.Count - 1)
                            .Choices = New PropertyGridEx.CustomChoices(units, False)
                        End With
                    End With
                    .Item.Add(DWSIM.App.GetLocalString("DCReboilerSpecs"), rspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                Case ColType.RefluxedAbsorber
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.CondenserPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.ReboilerPressure), nf)
                    val = Format(Converter.ConvertFromSI(su.pressure, uo.ReboilerPressure), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCLastStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCCondenserType"), uo, "CondenserType", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    val = Format(Converter.ConvertFromSI(su.deltaP, uo.CondenserDeltaP), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDeltaP"), su.deltaP), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    Dim units As String() = New String() {}
                    If Not uo.CondenserType = condtype.Full_Reflux Then
                        Dim cspec As New PropertyGridEx.CustomPropertyCollection()
                        With cspec
                            'condenser spec
                            If uo.CondenserType = condtype.Full_Reflux Or uo.CondenserType = condtype.Partial_Condenser Then
                                Dim cspecv As New PropertyGridEx.CustomPropertyCollection()
                                With cspecv
                                    .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), uo, "VaporFlowRate", False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                                    .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), uo, "VaporFlowRateUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                    With .Item(.Count - 1)
                                        .Choices = New PropertyGridEx.CustomChoices(units, False)
                                    End With
                                End With
                                .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), cspecv, False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                                With .Item(.Count - 1)
                                    .IsBrowsable = True
                                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                                End With
                            End If

                            .Add(DWSIM.App.GetLocalString("DCCondenserSpecType"), uo.Specs("C"), "SType", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            If uo.Specs("C").SType = ColumnSpec.SpecType.Component_Fraction Or
                                    uo.Specs("C").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or
                                    uo.Specs("C").SType = ColumnSpec.SpecType.Component_Recovery Or
                                    uo.Specs("C").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                                .Add(DWSIM.App.GetLocalString("DCCondenserSpecComp"), DWSIM.App.GetLocalString(uo.Specs("C").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                With .Item(.Count - 1)
                                    .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                                End With
                            End If
                            .Add(DWSIM.App.GetLocalString("DCCondenserSpecValue"), uo.Specs("C"), "SpecValue", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), uo.Specs("C"), "SpecUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            Select Case uo.Specs("C").SType
                                Case ColumnSpec.SpecType.Component_Fraction
                                    units = New String() {"M", "We"}
                                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Component_Recovery
                                    units = New String() {"% M/M", "% W/W"}
                                Case ColumnSpec.SpecType.Heat_Duty
                                    units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Stream_Ratio
                                    units = New String() {""}
                                Case ColumnSpec.SpecType.Temperature
                                    units = New String() {"K", "R", "C", "F"}
                            End Select
                            With .Item(.Count - 1)
                                .Choices = New PropertyGridEx.CustomChoices(units, False)
                            End With
                        End With
                        .Item.Add(DWSIM.App.GetLocalString("DCCondenserSpecs"), cspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        With .Item(.Item.Count - 1)
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    End If
            End Select

            .Item.Add(DWSIM.App.GetLocalString("DCEditConnections"), uo, False, DWSIM.App.GetLocalString("DCConnections"), "", True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.Distillation.UIConnectionsEditor
            End With

            .Item.Add(DWSIM.App.GetLocalString("DCUseIE"), uo, "UseTemperatureEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("DCUseIE1"), uo, "UseVaporFlowEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("DCUseIE3"), uo, "UseLiquidFlowEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("DCUseIE2"), uo, "UseCompositionEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("DCInitialEstimates2"), uo, False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.Distillation.UIInitialEstimates
            End With
            .Item.Add(DWSIM.App.GetLocalString("DCAutoUpdInitEst"), uo, "AutoUpdateInitialEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)

            If uo.SolvingMethod = 2 Then
                .Item.Add(DWSIM.App.GetLocalString("DCILTol"), uo, "InternalLoopTolerance", False, DWSIM.App.GetLocalString("DCTolerances"), "", True)
            End If
            .Item.Add(DWSIM.App.GetLocalString("DCELTol"), uo, "ExternalLoopTolerance", False, DWSIM.App.GetLocalString("DCTolerances"), "", True)
            .Item.Add(DWSIM.App.GetLocalString("DCMaxIt"), uo, "MaxIterations", False, DWSIM.App.GetLocalString("DCTolerances"), "", True)

            Dim methods = New String() {"Wang-Henke (Bubble Point)", "Naphtali-Sandholm (Newton)", "Russell (Inside-Out)", "Burningham-Otto (Sum Rates) (Absorber Only)"}
            Dim strategies = New String() {"Ideal K first, then Rigorous", "Ideal H first, then Rigorous", "Ideal K+H first, then Rigorous", "Direct Rigorous"}

            .Item.Add(DWSIM.App.GetLocalString("DCSolvingMethod"), methods(uo.SolvingMethod), False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
            With .Item(.Item.Count - 1)
                .Choices = New PropertyGridEx.CustomChoices(methods)
            End With

            .Item.Add("Solving Strategy", strategies(uo.SolverScheme), False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
            With .Item(.Item.Count - 1)
                .Choices = New PropertyGridEx.CustomChoices(strategies)
            End With

            .Item.Add(FT("Maximum Temperature Change", su.deltaT), uo.MaximumTemperatureStep.ConvertFromSI(su.deltaT).ToString(nf), False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)

            .Item.Add("Convergence Tolerance (External Loop)", uo, "ExternalLoopTolerance", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
            .Item.Add("Convergence Tolerance (Internal Loop)", uo, "InternalLoopTolerance", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)

            If uo.SolvingMethod = 0 Then
                .Item.Add("Stop at iteration number", uo, "StopAtIterationNumber", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
            ElseIf uo.SolvingMethod = 1 Then
                .Item.Add("Non-Linear Solver", uo.NS_Solver, False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Iteration Variables: Lower Bound", uo, "NS_LowerBound", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Iteration Variables: Upper Bound", uo, "NS_UpperBound", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Iteration Variables: Derivative Perturbation", uo, "SC_NumericalDerivativeStep", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Iteration Variables: Simplex Preconditioning", uo, "NS_SimplexPreconditioning", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
            ElseIf uo.SolvingMethod = 2 Then
                .Item.Add("Non-Linear Solver", uo.IO_Solver, False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Iteration Variables: Lower Bound", uo, "IO_LowerBound", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Iteration Variables: Upper Bound", uo, "IO_UpperBound", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Iteration Variables: Derivative Perturbation", uo, "IO_NumericalDerivativeStep", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Adjust Sb Scaling Factor", uo, "AdjustSb", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                .Item.Add("Calculate Kb by Weighted Average", uo, "KbjWeightedAverage", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
            ElseIf uo.SolvingMethod = 3 Then

            End If

            If uo.GraphicObject.Calculated Then
                If uo.SolvingMethod = 2 Then
                    .Item.Add(DWSIM.App.GetLocalString("DCILIts"), uo, "ic", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCELIts"), uo, "ec", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                ElseIf uo.SolvingMethod = 1 Then
                    .Item.Add(DWSIM.App.GetLocalString("DCELIts"), uo, "ec", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                Else
                    .Item.Add(DWSIM.App.GetLocalString("DCELIts"), uo, "ic", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                End If
                Select Case uo.ColumnType
                    Case ColType.DistillationColumn
                        val = Format(Converter.ConvertFromSI(su.heatflow, uo.CondenserDuty), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                        val = Format(Converter.ConvertFromSI(su.heatflow, uo.ReboilerDuty), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                    Case ColType.AbsorptionColumn
                    Case ColType.ReboiledAbsorber
                        val = Format(Converter.ConvertFromSI(su.heatflow, uo.ReboilerDuty), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                    Case ColType.RefluxedAbsorber
                        val = Format(Converter.ConvertFromSI(su.heatflow, uo.CondenserDuty), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                End Select
                .Item.Add(DWSIM.App.GetLocalString("DCResults1"), uo, False, DWSIM.App.GetLocalString("DCResults"), "", True)
                With .Item(.Item.Count - 1)
                    .CustomEditor = New DWSIM.Editors.Distillation.UIResults
                End With

            End If


            BasePopulatePropertyGrid(pgrid, uo)


            .ExpandAllGridItems()

        End With
    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As ShortcutColumn)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida1, saida2, ec, er As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                er = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                er = ""
            End If
            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                ec = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                ec = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("SCFeed"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("SCDistillate"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("SCBottoms"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("SCCondenserDuty"), ec, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("SCReboilerDuty"), er, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("SCCondenserType"), uo, "condtype", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCCondenserType"), True)
            .Item(.Item.Count - 1).Tag2 = "condtype"
            .Item.Add(DWSIM.App.GetLocalString("SCRefluxRatio"), uo, "m_refluxratio", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCRefluxRatio"), True)
            .Item(.Item.Count - 1).Tag2 = "PROP_SC_0"
            .Item.Add(DWSIM.App.GetLocalString("SCLightKey"), DWSIM.App.GetLocalString(uo.m_lightkey), False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCLightKeyMF"), True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
                .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                .DefaultValue = ""
                .DefaultType = GetType(String)
            End With
            .Item.Add(DWSIM.App.GetLocalString("SCLightKeyMF"), uo, "m_lightkeymolarfrac", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCLightKeyMF"), True)
            .Item(.Item.Count - 1).Tag2 = "PROP_SC_2"
            .Item.Add(DWSIM.App.GetLocalString("SCHeavyKey"), DWSIM.App.GetLocalString(uo.m_heavykey), False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCHeavyKey"), True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
                .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                .DefaultValue = ""
                .DefaultType = GetType(String)
            End With
            .Item.Add(DWSIM.App.GetLocalString("SCHeavyKeyMF"), uo, "m_heavykeymolarfrac", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCHeavyKeyMF"), True)
            .Item(.Item.Count - 1).Tag2 = "PROP_SC_1"
            Dim valor = Format(Converter.ConvertFromSI(su.pressure, uo.m_condenserpressure), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("SCCondenserPressure"), su.pressure), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCCondenserPressure"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_SC_3"
                .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
            End With
            valor = Format(Converter.ConvertFromSI(su.pressure, uo.m_boilerpressure), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("SCReboilerPressure"), su.pressure), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SCReboilerPressure"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_SC_4"
                .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
            End With

            If uo.GraphicObject.Calculated Then

                .Item.Add(DWSIM.App.GetLocalString("SCMinimumRefluxRatio"), Format(uo.m_Rmin, uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCMinimumRefluxRatio"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(DWSIM.App.GetLocalString("SCNminstages"), Format(uo.m_Nmin, uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCNminstages"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(DWSIM.App.GetLocalString("SCNstages"), Format(uo.m_N, uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCNstages"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(DWSIM.App.GetLocalString("SCOptimalFeedStage"), Format(uo.ofs, uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCOptimalFeedStage"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(FT(DWSIM.App.GetLocalString("SCStrippingLiquid"), su.molarflow), Format(Converter.ConvertFromSI(su.molarflow, uo.L_), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCStrippingLiquid"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(FT(DWSIM.App.GetLocalString("SCRectifyLiquid"), su.molarflow), Format(Converter.ConvertFromSI(su.molarflow, uo.L), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCRectifyLiquid"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(FT(DWSIM.App.GetLocalString("SCStrippingVapor"), su.molarflow), Format(Converter.ConvertFromSI(su.molarflow, uo.V_), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCStrippingVapor"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(FT(DWSIM.App.GetLocalString("SCRectifyVapor"), su.molarflow), Format(Converter.ConvertFromSI(su.molarflow, uo.V), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCRectifyVapor"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(FT(DWSIM.App.GetLocalString("SCCondenserDuty"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.m_Qc), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCCondenserDuty"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(FT(DWSIM.App.GetLocalString("SCReboilerDuty"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.m_Qb), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("SCReboilerDuty"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As SolidsSeparator)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem


        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida1, saida2 As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("OutletStream1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("OutletStream2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("SolidSepEfficiency"), uo, "SeparationEfficiency", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("SolidSepEfficiencyDesc"), True)
            .Item.Add(DWSIM.App.GetLocalString("LiquidSepEfficiency"), uo, "LiquidSeparationEfficiency", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("LiquidSepEfficiencyDesc"), True)

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As UnitOperations.UnitOperations.Splitter)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim saida1, saida2, saida3, ent As String

            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.OutputConnectors(2).IsAttached = True Then
                saida3 = uo.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            Else
                saida3 = ""
            End If

            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = 1
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesaida2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = 0
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            If saida1 <> "" And saida2 <> "" Then
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida3"), saida3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = 0
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
            End If

            .Item.Add(DWSIM.App.GetLocalString("SplitterOperationMode"), uo, "OperationMode", False, DWSIM.App.GetLocalString("Parmetros2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "OperationMode"

            Dim n As Integer = 0
            For Each cp In uo.GraphicObject.OutputConnectors
                If cp.IsAttached Then
                    n += 1
                End If
            Next

            Select Case uo.OperationMode
                Case UnitOperations.UnitOperations.Splitter.OpMode.SplitRatios
                    Dim i As Integer = 0
                    Dim RO As Boolean
                    Dim cg2 As ConnectionPoint
                    For Each cg2 In uo.GraphicObject.OutputConnectors
                        If cg2.IsAttached = True Then
                            RO = False
                            If i = 1 And saida3 = "" Then RO = True
                            If i = 2 Then RO = True
                            .Item.Add("[Split Ratio] " & cg2.AttachedConnector.AttachedTo.Tag, uo.Ratios.Item(i), RO, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Digiteumvalorentre0e"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = GetType(System.Double)
                            End With
                        End If
                        i += 1
                    Next
                Case UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec
                    Dim valor = Format(Converter.ConvertFromSI(su.massflow, uo.StreamFlowSpec), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetPropertyName("PROP_SP_1"), su.massflow), valor, False, DWSIM.App.GetLocalString("Parmetros2"), "", True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_SP_1"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.massflow, "W"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    If n = 3 Then
                        valor = Format(Converter.ConvertFromSI(su.massflow, uo.Stream2FlowSpec), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetPropertyName("PROP_SP_2"), su.massflow), valor, False, DWSIM.App.GetLocalString("Parmetros2"), "", True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_SP_2"
                            .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.massflow, "W"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With
                    End If
                Case UnitOperations.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
                    Dim valor = Format(Converter.ConvertFromSI(su.molarflow, uo.StreamFlowSpec), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetPropertyName("PROP_SP_1"), su.molarflow), valor, False, DWSIM.App.GetLocalString("Parmetros2"), "", True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_SP_1"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.molarflow, "M"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                    If n = 3 Then
                        valor = Format(Converter.ConvertFromSI(su.molarflow, uo.Stream2FlowSpec), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetPropertyName("PROP_SP_2"), su.molarflow), valor, False, DWSIM.App.GetLocalString("Parmetros2"), "", True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_SP_2"
                            .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.molarflow, "M"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With
                    End If
            End Select

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Tank)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                energ = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad5"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_TK_0"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            valor = Format(Converter.ConvertFromSI(su.voluuo, uo.Volume), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("TKVol"), su.volume), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("TKVol"), True)
            With .Item(.Item.Count - 1)
                .CustomTypeConverter = New System.ComponentModel.StringConverter
                .Tag2 = "PROP_TK_1"
                .DefaultValue = Nothing
                .DefaultType = GetType(Double)
            End With

            .Item.Add(DWSIM.App.GetLocalString("IgnorarVapornaEntrad"), uo, "IgnorePhase", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("IgnorarVapornaEntrad"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Boolean)
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea3"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            Else
                .Item.Add(FT(DWSIM.App.GetLocalString("TKResTime"), su.time), Format(Converter.ConvertFromSI(su.tiuo, uo.ResidenceTime), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("TKResTime"), True)
                With .Item(.Item.Count - 1)
                    .CustomTypeConverter = New System.ComponentModel.StringConverter
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Double)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Expander)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()



            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.EnergyConnector.IsAttached = True Then
                energ = uo.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("HeaterCoolerCalcMode"), uo, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "CalcMode"

            Select Case uo.CalcMode
                Case Expander.CalculationMode.Delta_P
                    Dim valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT("Delta P", su.deltaP), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_EX_0"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                Case Expander.CalculationMode.OutletPressure
                    Dim valor = Format(Converter.ConvertFromSI(su.pressure, uo.POut), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Presso"), su.pressure), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Pressoajusante"), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_EX_4"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
            End Select

            .Item.Add(DWSIM.App.GetLocalString("EficinciaAdiabtica01"), uo, "EficienciaAdiabatica", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "Eficiência da turbina em relação ao processo ideal isentrópico/adiabático", True)
            With .Item(.Item.Count - 1)
                .Tag2 = "PROP_EX_1"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            .Item.Add(DWSIM.App.GetLocalString("IgnorarLquidonaEntra"), uo, "IgnorePhase", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("SelecioLiquidrueparaign3"), True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(Boolean)
            End With

            .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
            With .Item(.Item.Count - 1)
                .Tag2 = "PROP_EX_2"
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            .Item.Add(FT(DWSIM.App.GetLocalString("Energiadisponvel"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Potnciageradapelatur"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Valve)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            '.Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            'With .Item(.Item.Count - 1)
            '    .DefaultValue = Nothing
            '    .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
            'End With

            .Item.Add(DWSIM.App.GetLocalString("ValveCalcMode"), uo, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
            .Item(.Item.Count - 1).Tag2 = "CalcMode"

            Dim valor As Double

            Select Case uo.CalcMode

                Case Valve.CalculationMode.DeltaP

                    valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString(""), True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_VA_1"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With

                Case Valve.CalculationMode.OutletPressure

                    valor = Format(Converter.ConvertFromSI(su.pressure, uo.OutletPressure.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("ValveOutletPressure"), su.pressure), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    With .Item(.Item.Count - 1)
                        .CustomTypeConverter = New System.ComponentModel.StringConverter
                        .Tag2 = "PROP_VA_2"
                        .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With

                Case Valve.CalculationMode.Kv_Gas, Valve.CalculationMode.Kv_Liquid, Valve.CalculationMode.Kv_Steam

                    .Item.Add("Kv (max)", uo, "Kv", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    .Item.Add("Use Kv versus Opening relatioship", uo, "EnableOpeningKvRelationship", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    .Item.Add("Kv/Kvmax (%) = f(OP(%)) expression", uo, "PercentOpeningVersusPercentKvExpression", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    .Item.Add("Valve Opening (OP) (%)", uo, "OpeningPct", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)

            End Select


            If uo.GraphicObject.Calculated = False Then
                .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), uo, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(System.String)
                End With
            Else

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                Select Case uo.CalcMode

                    Case Valve.CalculationMode.DeltaP

                        valor = Format(Converter.ConvertFromSI(su.pressure, uo.OutletPressure.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("ValveOutletPressure"), su.pressure), valor, False, DWSIM.App.GetLocalString("Resultados3"), "", True)
                        With .Item(.Item.Count - 1)
                            .IsReadOnly = True
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_VA_2"
                            .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                    Case Valve.CalculationMode.OutletPressure

                        valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                        With .Item(.Item.Count - 1)
                            .IsReadOnly = True
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_VA_1"
                            .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                    Case Valve.CalculationMode.Kv_Gas, Valve.CalculationMode.Kv_Liquid, Valve.CalculationMode.Kv_Steam

                        valor = Format(Converter.ConvertFromSI(su.pressure, uo.OutletPressure.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("ValveOutletPressure"), su.pressure), valor, False, DWSIM.App.GetLocalString("Resultados3"), "", True)
                        With .Item(.Item.Count - 1)
                            .IsReadOnly = True
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_VA_2"
                            .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                        valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                        With .Item(.Item.Count - 1)
                            .IsReadOnly = True
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_VA_1"
                            .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                End Select

            End If

            BasePopulatePropertyGrid(pgrid, uo)

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Vessel)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent1, ent2, ent3, ent4, ent5, ent6, saida1, saida2, saida3, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent1 = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent1 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                ent2 = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                ent2 = ""
            End If
            If uo.GraphicObject.InputConnectors(2).IsAttached = True Then
                ent3 = uo.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
            Else
                ent3 = ""
            End If
            If uo.GraphicObject.InputConnectors(3).IsAttached = True Then
                ent4 = uo.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag
            Else
                ent4 = ""
            End If
            If uo.GraphicObject.InputConnectors(4).IsAttached = True Then
                ent5 = uo.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag
            Else
                ent5 = ""
            End If
            If uo.GraphicObject.InputConnectors(5).IsAttached = True Then
                ent6 = uo.GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Tag
            Else
                ent6 = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.OutputConnectors(2).IsAttached = True Then
                saida3 = uo.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            Else
                saida3 = ""
            End If
            If uo.GraphicObject.InputConnectors(6).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), ent1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), ent2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada3"), ent3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada4"), ent4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada5"), ent5, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With
            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada6"), ent6, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadevapor"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadelquido"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadelquido") & " (2)", saida3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Pressoajusante"), uo, "PressureCalculation", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Selecioneumaopoquein"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
            End With

            .Item.Add(DWSIM.App.GetLocalString("FlashSpecification"), uo, "FlashSpecification", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("FlashSpecificationDesc"), True)

            .Item.Add(DWSIM.App.GetLocalString("SobreporTemperaturad"), uo, "OverrideT", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SelecioLiquidrueparaign4"), True)
            If uo.OverrideT Then
                Dim valor = Format(Converter.ConvertFromSI(su.temperature, uo.FlashTemperature), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Temperatura"), su.temperature), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Temperaturadeseparao"), True)
                With .Item(.Item.Count - 1)
                    .CustomTypeConverter = New System.ComponentModel.StringConverter
                    .Tag2 = "PROP_SV_0"
                    .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With
            End If
            .Item.Add(DWSIM.App.GetLocalString("SobreporPressodesepa"), uo, "OverrideP", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SelecioLiquidrueparaign5"), True)
            If uo.OverrideP Then
                Dim valor = Format(Converter.ConvertFromSI(su.pressure, uo.FlashPressure), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Presso"), su.pressure), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Pressodeseparao"), True)
                With .Item(.Item.Count - 1)
                    .CustomTypeConverter = New System.ComponentModel.StringConverter
                    .Tag2 = "PROP_SV_1"
                    .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.pressure, "P"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With
            End If

            If uo.GraphicObject.Calculated Then
                .Item.Add(FT(DWSIM.App.GetLocalString("RConvPGridItem3"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Reactor_Conversion)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida1, saida2, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadevapor"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadelquido"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            If Not uo.FlowSheet.Options.ReactionSets.ContainsKey(uo.ReactionSetID) Then uo.ReactionSetID = "DefaultSet"
            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), uo.FlowSheet.Options.ReactionSets(uo.ReactionSetID).Nauo, False, DWSIM.App.GetLocalString("Paruotrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
                .IsDropdownResizable = True
            End With

            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem2"), uo, "ReactorOperationMode", False, DWSIM.App.GetLocalString("Paruotrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem2Help"), True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
            End With

            Dim valor As Double

            If uo.ReactorOperationMode = OperationMode.OutletTemperature Then
                valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), valor, False, DWSIM.App.GetLocalString("Paruotrosdeclculo2"), "", True)
                With .Item(.Item.Count - 1)
                    .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With
            End If

            valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Paruotrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad6"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated Then

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("RConvPGridItem3"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                'CustomPropertyCollection
                Dim m As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.Conversions
                    valor = Format(dbl.Value * 100, uo.FlowSheet.Options.NumberFormat)
                    m.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Nauo, valor, False, DWSIM.App.GetLocalString("ReacoesConversoes"), DWSIM.App.GetLocalString("RConvPGridItem4Help"), True)
                    m.Item(m.Count - 1).IsReadOnly = True
                    m.Item(m.Count - 1).DefaultValue = Nothing
                    m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(DWSIM.App.GetLocalString("ReacoesConversoes"), m, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RConvPGridItem3Help"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Reactor_CSTR)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            If Not uo.FlowSheet.Options.ReactionSets.ContainsKey(uo.ReactionSetID) Then uo.ReactionSetID = "DefaultSet"
            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), uo.FlowSheet.Options.ReactionSets(uo.ReactionSetID).Name, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
                .IsDropdownResizable = True
            End With

            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem2"), uo, "ReactorOperationMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem2Help"), True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
            End With

            Dim valor As Double

            If uo.ReactorOperationMode = OperationMode.OutletTemperature Then
                valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                With .Item(.Item.Count - 1)
                    .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With
            End If

            If uo.ReactorOperationMode = OperationMode.Isothermic Then
                valor = Format(Converter.ConvertFromSI(su.temperature, uo.IsothermalTemperature), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("RSCTRIsothermalTemperature"), su.temperature), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RCSTRPGridItem1Help"), True)
            End If

            valor = Format(Converter.ConvertFromSI(su.mass, uo.CatalystAmount), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("CSTRCatalystAmount"), su.mass), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("CSTRCatalystAmountDesc"), True)

            valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad6"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            valor = Format(Converter.ConvertFromSI(su.volume, uo.Volume), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("RCSTRPGridItem1"), su.volume), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RCSTRPGridItem1Help"), True)

            valor = Format(Converter.ConvertFromSI(su.volume, uo.Headspace), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Headspace"), su.volume), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)

            If uo.GraphicObject.Calculated Then

                .Item.Add(FT(DWSIM.App.GetLocalString("TKResTime"), su.time), Format(Converter.ConvertFromSI(su.time, uo.ResidenceTimeL), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("TKResTime"), True)

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("RConvPGridItem3"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                'CustomPropertyCollection
                Dim m As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.ComponentConversions
                    valor = Format(dbl.Value * 100, uo.FlowSheet.Options.NumberFormat)
                    If dbl.Value >= 0 Then
                        m.Add(dbl.Key, valor, False, DWSIM.App.GetLocalString("ComponentesConversoes"), DWSIM.App.GetLocalString("RCSTRPGridItem3Help"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    End If
                Next

                .Item.Add(DWSIM.App.GetLocalString("ComponentesConversoes") & " (%)", m, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RCSTRPGridItem2Help"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                'CustomPropertyCollection
                Dim m2 As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.RxiT
                    m2.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, dbl.Value, False, DWSIM.App.GetLocalString("ReactionExtents"), DWSIM.App.GetLocalString(""), True)
                    m2.Item(m2.Count - 1).IsReadOnly = True
                    m2.Item(m2.Count - 1).DefaultValue = Nothing
                    m2.Item(m2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(FT(DWSIM.App.GetLocalString("ReactionExtents"), su.molarflow), m2, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                'CustomPropertyCollection
                Dim m3 As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.RxiT
                    m3.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, dbl.Value / uo.Volume, False, DWSIM.App.GetLocalString("ReactionExtents"), DWSIM.App.GetLocalString(""), True)
                    m3.Item(m3.Count - 1).IsReadOnly = True
                    m3.Item(m3.Count - 1).DefaultValue = Nothing
                    m3.Item(m3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(FT(DWSIM.App.GetLocalString("ReactionRates"), su.reac_rate), m3, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                'CustomPropertyCollection
                Dim m4 As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.DHRi
                    m4.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, Converter.ConvertFromSI(su.heatflow, dbl.Value), False, DWSIM.App.GetLocalString("ReactionHeats"), DWSIM.App.GetLocalString(""), True)
                    m4.Item(m4.Count - 1).IsReadOnly = True
                    m4.Item(m4.Count - 1).DefaultValue = Nothing
                    m4.Item(m4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(FT(DWSIM.App.GetLocalString("ReactionHeats"), su.heatflow), m4, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Reactor_Equilibrium)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida1, saida2, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadevapor"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadelquido"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            If Not uo.FlowSheet.Options.ReactionSets.ContainsKey(uo.ReactionSetID) Then uo.ReactionSetID = "DefaultSet"
            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), uo.FlowSheet.Options.ReactionSets(uo.ReactionSetID).Name, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
                .IsDropdownResizable = True
            End With

            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem2"), uo, "ReactorOperationMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem2Help"), True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
            End With

            Dim valor As Double

            If uo.ReactorOperationMode = OperationMode.OutletTemperature Then
                valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                With .Item(.Item.Count - 1)
                    .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With
            End If

            valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad6"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated Then

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("RConvPGridItem3"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("RGInitialG"), su.heatflow), Format(Converter.ConvertFromSI(su.molar_enthalpy, uo.InitialGibbsEnergy), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RGInitialG_description"), True)
                .Item.Add(FT(DWSIM.App.GetLocalString("RGFinalG"), su.heatflow), Format(Converter.ConvertFromSI(su.molar_enthalpy, uo.FinalGibbsEnergy), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RGFinalG_description"), True)

                'CustomPropertyCollection
                Dim m As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.ComponentConversions
                    valor = Format(dbl.Value * 100, uo.FlowSheet.Options.NumberFormat)
                    m.Add((dbl.Key), valor, False, DWSIM.App.GetLocalString("ComponentesConversoes"), DWSIM.App.GetLocalString("RCSTRPGridItem3Help"), True)
                    m.Item(m.Count - 1).IsReadOnly = True
                    m.Item(m.Count - 1).DefaultValue = Nothing
                    m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(DWSIM.App.GetLocalString("ComponentesConversoes"), m, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RCSTRPGridItem2Help"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                'CustomPropertyCollection
                Dim m2 As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.ReactionExtents
                    valor = Format(dbl.Value, uo.FlowSheet.Options.NumberFormat)
                    m2.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, valor, False, DWSIM.App.GetLocalString("CoordenadasReacoes"), DWSIM.App.GetLocalString("REqPGridItem1Help"), True)
                    m2.Item(m2.Count - 1).IsReadOnly = True
                    m2.Item(m2.Count - 1).DefaultValue = Nothing
                    m2.Item(m2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(DWSIM.App.GetLocalString("CoordenadasReacoes"), m2, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("REqPGridItem2Help"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Reactor_PFR)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida = ""
            End If

            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            If Not uo.FlowSheet.Options.ReactionSets.ContainsKey(uo.ReactionSetID) Then uo.ReactionSetID = "DefaultSet"
            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), uo.FlowSheet.Options.ReactionSets(uo.ReactionSetID).Name, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
            With .Item(.Item.Count - 1)
                .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
                .IsDropdownResizable = True
            End With

            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem2"), uo, "ReactorOperationMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem2Help"), True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
            End With

            Dim valor As Double

            If uo.ReactorOperationMode = OperationMode.OutletTemperature Then
                valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                With .Item(.Item.Count - 1)
                    .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With
            End If

            valor = Format(Converter.ConvertFromSI(su.volume, uo.Volume), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("RCSTRPGridItem1"), su.volume), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RCSTRPGridItem1Help"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            valor = Format(Converter.ConvertFromSI(su.distance, uo.Length), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("PFRLength"), su.distance), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("PFRLengthDesc"), True)

            valor = Format(Converter.ConvertFromSI(su.density, uo.CatalystLoading), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("PFRCatalystLoading"), su.density), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("PFRCatalystLoadingDesc"), True)

            valor = Format(Converter.ConvertFromSI(su.diameter, uo.CatalystParticleDiameter), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("PFRCatalystParticleDiameter"), su.diameter), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("PFRCatalystParticleDiameterDesc"), True)

            .Item.Add(DWSIM.App.GetLocalString("PFRCatalystVoidFraction"), uo, "CatalystVoidFraction", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("PFRCatalystVoidFractionDesc"), True)

            If uo.GraphicObject.Calculated Then

                .Item.Add(FT(DWSIM.App.GetLocalString("TKResTime"), su.time), Format(Converter.ConvertFromSI(su.time, uo.ResidenceTime), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("TKResTime"), True)

                valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Quedadepressoaplicad6"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("RConvPGridItem3"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                'CustomPropertyCollection
                Dim m As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.ComponentConversions
                    valor = Format(dbl.Value * 100, uo.FlowSheet.Options.NumberFormat)
                    If dbl.Value >= 0 Then
                        m.Add((dbl.Key), valor, False, DWSIM.App.GetLocalString("ComponentesConversoes"), DWSIM.App.GetLocalString("RCSTRPGridItem3Help"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    End If
                Next

                .Item.Add(DWSIM.App.GetLocalString("ComponentesConversoes") & " (%)", m, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RCSTRPGridItem2Help"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                'CustomPropertyCollection
                Dim m2 As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.RxiT
                    Dim value = dbl.Value
                    m2.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, value, False, DWSIM.App.GetLocalString("ReactionExtents"), DWSIM.App.GetLocalString(""), True)
                    m2.Item(m2.Count - 1).IsReadOnly = True
                    m2.Item(m2.Count - 1).DefaultValue = Nothing
                    m2.Item(m2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(FT(DWSIM.App.GetLocalString("ReactionExtents"), su.molarflow), m2, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                'CustomPropertyCollection
                Dim m3 As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.RxiT
                    m3.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, dbl.Value / uo.Volume, False, DWSIM.App.GetLocalString("ReactionExtents"), DWSIM.App.GetLocalString(""), True)
                    m3.Item(m3.Count - 1).IsReadOnly = True
                    m3.Item(m3.Count - 1).DefaultValue = Nothing
                    m3.Item(m3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(FT(DWSIM.App.GetLocalString("ReactionRates"), su.reac_rate), m3, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                'CustomPropertyCollection
                Dim m4 As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.DHRi
                    m4.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, Converter.ConvertFromSI(su.heatflow, dbl.Value), False, DWSIM.App.GetLocalString("ReactionHeats"), DWSIM.App.GetLocalString(""), True)
                    m4.Item(m4.Count - 1).IsReadOnly = True
                    m4.Item(m4.Count - 1).DefaultValue = Nothing
                    m4.Item(m4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(FT(DWSIM.App.GetLocalString("ReactionHeats"), su.heatflow), m4, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                .Item.Add(DWSIM.App.GetLocalString("RPFRPGridItem2"), uo, "points", False, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RPFRPGridItem2Help"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(Global.DWSIM.DWSIM.Editors.PipeEditor.ThermalEditorDefinitions)
                    .CustomEditor = New DWSIM.Editors.Results.UIFormGraphPFR
                End With

            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

    Public Shared Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal uo As Reactor_Gibbs)

        Dim su = uo.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With pgrid

            .PropertySort = PropertySort.Categorized
            .ShowCustomProperties = True
            .Item.Clear()

            Dim ent, saida1, saida2, energ As String
            If uo.GraphicObject.InputConnectors(0).IsAttached = True Then
                ent = uo.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            Else
                ent = ""
            End If
            If uo.GraphicObject.OutputConnectors(0).IsAttached = True Then
                saida1 = uo.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            Else
                saida1 = ""
            End If
            If uo.GraphicObject.OutputConnectors(1).IsAttached = True Then
                saida2 = uo.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            Else
                saida2 = ""
            End If
            If uo.GraphicObject.InputConnectors(1).IsAttached = True Then
                energ = uo.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                energ = ""
            End If

            .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadevapor"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Saidadelquido"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
            End With

            .Item.Add(DWSIM.App.GetLocalString("RGCalcMode"), uo, "SolvMethod", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RGCalcMode_description"), True)

            If uo.SolvMethod = Reactor_Gibbs.SolvingMethod.DirectMinimization Then

                .Item.Add(DWSIM.App.GetLocalString("RGComponents"), uo, "ComponentIDs", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RGComponents_description"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.Reactors.UIGibbsComponentSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("RGInitialEstimates"), uo, "InitialEstimates", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RGInitialEstimates_description"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.Reactors.UIGibbsInitialEstimatesEditor
                End With

                .Item.Add(DWSIM.App.GetLocalString("RGElementMatrix"), uo, "ElementMatrix", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RGElementMatrix_description"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.Reactors.UIElementMatrixEditor
                End With

            Else

                If Not uo.FlowSheet.Options.ReactionSets.ContainsKey(uo.ReactionSetID) Then uo.ReactionSetID = "DefaultSet"
                .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), uo.FlowSheet.Options.ReactionSets(uo.ReactionSetID).Name, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
                With .Item(.Item.Count - 1)
                    .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
                    .IsDropdownResizable = True
                End With

            End If

            .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem2"), uo, "ReactorOperationMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem2Help"), True)
            With .Item(.Item.Count - 1)
                .IsBrowsable = False
            End With

            Dim valor As Double

            If uo.ReactorOperationMode = OperationMode.OutletTemperature Then
                valor = Format(Converter.ConvertFromSI(su.temperature, uo.OutletTemperature), uo.FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.temperature), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                With .Item(.Item.Count - 1)
                    .Tag = New Object() {uo.FlowSheet.Options.NumberFormat, su.temperature, "T"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With
            End If

            valor = Format(Converter.ConvertFromSI(su.deltaP, uo.DeltaP.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat)
            .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.deltaP), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad6"), True)
            With .Item(.Item.Count - 1)
                .DefaultValue = Nothing
                .DefaultType = GetType(Nullable(Of Double))
            End With

            If uo.GraphicObject.Calculated Then

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.deltaT), Format(Converter.ConvertFromSI(su.deltaT, uo.DeltaT.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("RConvPGridItem3"), su.heatflow), Format(Converter.ConvertFromSI(su.heatflow, uo.DeltaQ.GetValueOrDefault), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("RGInitialG"), su.heatflow), Format(Converter.ConvertFromSI(su.molar_enthalpy, uo.InitialGibbsEnergy), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RGInitialG_description"), True)
                .Item.Add(FT(DWSIM.App.GetLocalString("RGFinalG"), su.heatflow), Format(Converter.ConvertFromSI(su.molar_enthalpy, uo.FinalGibbsEnergy), uo.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RGFinalG_description"), True)

                'CustomPropertyCollection
                Dim m As New PropertyGridEx.CustomPropertyCollection()
                For Each dbl As KeyValuePair(Of String, Double) In uo.ComponentConversions
                    valor = Format(dbl.Value * 100, uo.FlowSheet.Options.NumberFormat)
                    m.Add((dbl.Key), valor, False, DWSIM.App.GetLocalString("ComponentesConversoes"), DWSIM.App.GetLocalString("RCSTRPGridItem3Help"), True)
                    m.Item(m.Count - 1).IsReadOnly = True
                    m.Item(m.Count - 1).DefaultValue = Nothing
                    m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                Next

                .Item.Add(DWSIM.App.GetLocalString("ComponentesConversoes"), m, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RCSTRPGridItem2Help"), True)
                With .Item(.Item.Count - 1)
                    .IsReadOnly = True
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                If uo.SolvMethod = Reactor_Gibbs.SolvingMethod.DirectMinimization Then

                    .Item.Add(DWSIM.App.GetLocalString("RGElementBalance"), uo.ElementBalance, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RGElementBalance_description"), True)

                Else

                    If Not uo.ReactionExtents Is Nothing Then

                        'CustomPropertyCollection
                        Dim m2 As New PropertyGridEx.CustomPropertyCollection()

                        For Each dbl As KeyValuePair(Of String, Double) In uo.ReactionExtents
                            valor = Format(dbl.Value, uo.FlowSheet.Options.NumberFormat)
                            m2.Add(uo.FlowSheet.Options.Reactions(dbl.Key).Name, valor, False, DWSIM.App.GetLocalString("CoordenadasReacoes"), DWSIM.App.GetLocalString("REqPGridItem1Help"), True)
                            m2.Item(m2.Count - 1).IsReadOnly = True
                            m2.Item(m2.Count - 1).DefaultValue = Nothing
                            m2.Item(m2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                        Next

                        .Item.Add(DWSIM.App.GetLocalString("CoordenadasReacoes"), m2, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("REqPGridItem2Help"), True)
                        With .Item(.Item.Count - 1)
                            .IsReadOnly = True
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With

                    End If

                End If

            End If

            BasePopulatePropertyGrid(pgrid, uo)

            .ExpandAllGridItems()

        End With

    End Sub

End Class
