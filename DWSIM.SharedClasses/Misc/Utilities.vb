Public Class Utility

    Shared Sub UpdateElement(xel As XElement)

        If xel.Name = "TipoObjeto" Then xel.Name = "ObjectType"
        If xel.Name = "Nome" Then xel.Name = "Name"
        If xel.Name = "Descricao" Then xel.Name = "Description"
        If xel.Name = "Tipo" Then xel.Name = "Type"
        If xel.Name = "FracaoMolar" Then xel.Name = "MoleFraction"
        If xel.Name = "FracaoMassica" Then xel.Name = "MassFraction"
        If xel.Name = "FracaoDePetroleo" Then xel.Name = "PetroleumFraction"
        If xel.Value = "Nenhum" Then xel.Value = "None"
        If xel.Value = "Destino" Then xel.Value = "Target"
        If xel.Value = "Fonte" Then xel.Value = "Source"
        If xel.Value = "Manipulada" Then xel.Value = "Manipulated"
        If xel.Value = "Referencia" Then xel.Value = "Referenced"
        If xel.Value = "Controlada" Then xel.Value = "Controlled"

        If xel.Value.EndsWith("Streams.MaterialStream") Then xel.Value = "DWSIM.Thermodynamics.Streams.MaterialStream"

        If xel.Value.EndsWith("Streams.EnergyStream") Then xel.Value = "DWSIM.UnitOperations.Streams.EnergyStream"

        If xel.Value.EndsWith("UnitOps.Vessel") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Vessel"
        If xel.Value.EndsWith("UnitOps.Compressor") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Compressor"
        If xel.Value.EndsWith("UnitOps.Expander") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Expander"
        If xel.Value.EndsWith("UnitOps.Heater") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Heater"
        If xel.Value.EndsWith("UnitOps.Cooler") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Cooler"
        If xel.Value.EndsWith("UnitOps.HeatExchanger") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.HeatExchanger"
        If xel.Value.EndsWith("UnitOps.Filter") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Filter"
        If xel.Value.EndsWith("UnitOps.Mixer") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Mixer"
        If xel.Value.EndsWith("UnitOps.Splitter") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Splitter"
        If xel.Value.EndsWith("UnitOps.ComponentSeparator") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ComponentSeparator"
        If xel.Value.EndsWith("UnitOps.ShortcutColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ShortcutColumn"
        If xel.Value.EndsWith("UnitOps.CustomUO") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.CustomUO"
        If xel.Value.EndsWith("UnitOps.CapeOpenUO") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.CapeOpenUO"
        If xel.Value.EndsWith("UnitOps.Flowsheet") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Flowsheet"
        If xel.Value.EndsWith("UnitOps.OrificePlate") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.OrificePlate"
        If xel.Value.EndsWith("UnitOps.SolidsSeparator") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.SolidsSeparator"
        If xel.Value.EndsWith("UnitOps.ExcelUO") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ExcelUO"
        If xel.Value.EndsWith("UnitOps.Tank") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Tank"
        If xel.Value.EndsWith("UnitOps.Valve") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Valve"
        If xel.Value.EndsWith("UnitOps.Pump") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Pump"
        If xel.Value.EndsWith("UnitOps.Pipe") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Pipe"
        If xel.Value.EndsWith("UnitOps.DistillationColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.DistillationColumn"
        If xel.Value.EndsWith("UnitOps.AbsorptionColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.AbsorptionColumn"
        If xel.Value.EndsWith("UnitOps.ReboiledAbsorber") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ReboiledAbsorber"
        If xel.Value.EndsWith("UnitOps.RefluxedAbsorber") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.RefluxedAbsorber"

        If xel.Value.EndsWith("Reactors.Reactor_Conversion") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Conversion"
        If xel.Value.EndsWith("Reactors.Reactor_Equilibrium") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Equilibrium"
        If xel.Value.EndsWith("Reactors.Reactor_Gibbs") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Gibbs"
        If xel.Value.EndsWith("Reactors.Reactor_PFR") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_PFR"
        If xel.Value.EndsWith("Reactors.Reactor_CSTR") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_CSTR"

        If xel.Value.EndsWith("SpecialOps.Adjust") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Adjust"
        If xel.Value.EndsWith("SpecialOps.Spec") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Spec"
        If xel.Value.EndsWith("SpecialOps.Recycle") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Recycle"
        If xel.Value.EndsWith("SpecialOps.EnergyRecycle") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.EnergyRecycle"

        If xel.Value.EndsWith("DWSIM.MaterialStream") Then xel.Value = "DWSIM.Thermodynamics.Streams.MaterialStream"

        If xel.Value.EndsWith("DWSIM.EnergyStream") Then xel.Value = "DWSIM.UnitOperations.Streams.EnergyStream"

        If xel.Value.EndsWith("DWSIM.Vessel") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Vessel"
        If xel.Value.EndsWith("DWSIM.Compressor") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Compressor"
        If xel.Value.EndsWith("DWSIM.Expander") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Expander"
        If xel.Value.EndsWith("DWSIM.Heater") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Heater"
        If xel.Value.EndsWith("DWSIM.Cooler") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Cooler"
        If xel.Value.EndsWith("DWSIM.HeatExchanger") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.HeatExchanger"
        If xel.Value.EndsWith("DWSIM.Filter") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Filter"
        If xel.Value.EndsWith("DWSIM.Mixer") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Mixer"
        If xel.Value.EndsWith("DWSIM.Splitter") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Splitter"
        If xel.Value.EndsWith("DWSIM.ComponentSeparator") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ComponentSeparator"
        If xel.Value.EndsWith("DWSIM.ShortcutColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ShortcutColumn"
        If xel.Value.EndsWith("DWSIM.CustomUO") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.CustomUO"
        If xel.Value.EndsWith("DWSIM.CapeOpenUO") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.CapeOpenUO"
        If xel.Value.EndsWith("DWSIM.Flowsheet") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Flowsheet"
        If xel.Value.EndsWith("DWSIM.OrificePlate") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.OrificePlate"
        If xel.Value.EndsWith("DWSIM.SolidsSeparator") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.SolidsSeparator"
        If xel.Value.EndsWith("DWSIM.ExcelUO") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ExcelUO"
        If xel.Value.EndsWith("DWSIM.Tank") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Tank"
        If xel.Value.EndsWith("DWSIM.Valve") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Valve"
        If xel.Value.EndsWith("DWSIM.Pump") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Pump"
        If xel.Value.EndsWith("DWSIM.Pipe") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Pipe"
        If xel.Value.EndsWith("DWSIM.DistillationColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.DistillationColumn"
        If xel.Value.EndsWith("DWSIM.AbsorptionColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.AbsorptionColumn"
        If xel.Value.EndsWith("DWSIM.ReboiledAbsorber") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ReboiledAbsorber"
        If xel.Value.EndsWith("DWSIM.RefluxedAbsorber") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.RefluxedAbsorber"

        If xel.Value.EndsWith("DWSIM.Reactor_Conversion") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Conversion"
        If xel.Value.EndsWith("DWSIM.Reactor_Equilibrium") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Equilibrium"
        If xel.Value.EndsWith("DWSIM.Reactor_Gibbs") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Gibbs"
        If xel.Value.EndsWith("DWSIM.Reactor_PFR") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_PFR"
        If xel.Value.EndsWith("DWSIM.Reactor_CSTR") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_CSTR"

        If xel.Value.EndsWith("DWSIM.Adjust") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Adjust"
        If xel.Value.EndsWith("DWSIM.Spec") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Spec"
        If xel.Value.EndsWith("DWSIM.Recycle") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Recycle"
        If xel.Value.EndsWith("DWSIM.EnergyRecycle") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.EnergyRecycle"

    End Sub

    Shared Sub UpdateElementForMobileXML(xel As XElement)

        If xel.Name = "TipoObjeto" Then xel.Name = "ObjectType"
        If xel.Name = "Nome" Then xel.Name = "Name"
        If xel.Name = "Descricao" Then xel.Name = "Description"
        If xel.Name = "Tipo" Then xel.Name = "Type"
        If xel.Name = "FracaoMolar" Then xel.Name = "MoleFraction"
        If xel.Name = "FracaoMassica" Then xel.Name = "MassFraction"
        If xel.Name = "FracaoDePetroleo" Then xel.Name = "PetroleumFraction"
        If xel.Value = "Nenhum" Then xel.Value = "None"
        If xel.Value = "Destino" Then xel.Value = "Target"
        If xel.Value = "Fonte" Then xel.Value = "Source"
        If xel.Value = "Manipulada" Then xel.Value = "Manipulated"
        If xel.Value = "Referencia" Then xel.Value = "Referenced"
        If xel.Value = "Controlada" Then xel.Value = "Controlled"

        If xel.Name = "Fill" Then xel.Value = "True"

        If xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.EquilibriumReactorGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.ReactorEquilibriumGraphic"
        ElseIf xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.ConversionReactorGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.ReactorConversionGraphic"
        ElseIf xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.CSTRGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.ReactorCSTRGraphic"
        ElseIf xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.PFRGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.ReactorPFRGraphic"
        ElseIf xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.MixerGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.NodeInGraphic"
        ElseIf xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.SplitterGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.NodeOutGraphic"
        ElseIf xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.HeaterCoolerGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.HeaterGraphic"
        ElseIf xel.Value.Equals("PortableFlowsheetDrawing.GraphicObjects.Shapes.CompressorExpanderGraphic") Then
            xel.Value = "DWSIM.DrawingTools.GraphicObjects.CompressorGraphic"
        End If

        If xel.Value.StartsWith("PortableFlowsheetDrawing.GraphicObjects.Shapes") And xel.Name = "Type" Then
            xel.Value = xel.Value.Replace("PortableFlowsheetDrawing.GraphicObjects.Shapes", "DWSIM.DrawingTools.GraphicObjects")
        End If

        If xel.Value.EndsWith("Streams.MaterialStream") Then xel.Value = "DWSIM.Thermodynamics.Streams.MaterialStream"
        If xel.Value.EndsWith("Streams.EnergyStream") Then xel.Value = "DWSIM.UnitOperations.Streams.EnergyStream"

        If xel.Value.EndsWith("UnitOperations.Separator") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Vessel"
        If xel.Value.EndsWith("UnitOperations.AdiabaticExpanderCompressor") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Compressor"
        If xel.Value.EndsWith("UnitOperations.HeaterCooler") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Heater"
        If xel.Value.EndsWith("UnitOperations.HeatExchanger") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.HeatExchanger"
        If xel.Value.EndsWith("UnitOperations.Mixer") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Mixer"
        If xel.Value.EndsWith("UnitOperations.Splitter") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Splitter"
        If xel.Value.EndsWith("UnitOperations.ComponentSeparator") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ComponentSeparator"
        If xel.Value.EndsWith("UnitOperations.ShortcutColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.ShortcutColumn"
        If xel.Value.EndsWith("UnitOperations.Valve") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Valve"
        If xel.Value.EndsWith("UnitOperations.Pump") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Pump"
        If xel.Value.EndsWith("UnitOperations.Pipe") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.Pipe"
        If xel.Value.EndsWith("UnitOperations.DistillationColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.DistillationColumn"
        If xel.Value.EndsWith("UnitOperations.AbsorptionColumn") Then xel.Value = "DWSIM.UnitOperations.UnitOperations.AbsorptionColumn"

        If xel.Value.EndsWith("Reactors.Reactor_Conversion") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Conversion"
        If xel.Value.EndsWith("Reactors.Reactor_Equilibrium") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_Equilibrium"
        If xel.Value.EndsWith("Reactors.Reactor_PFR") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_PFR"
        If xel.Value.EndsWith("Reactors.Reactor_CSTR") Then xel.Value = "DWSIM.UnitOperations.Reactors.Reactor_CSTR"

        If xel.Value.EndsWith("UnitOperations.Adjust") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Adjust"
        If xel.Value.EndsWith("UnitOperations.Recycle") Then xel.Value = "DWSIM.UnitOperations.SpecialOps.Recycle"

    End Sub


End Class
