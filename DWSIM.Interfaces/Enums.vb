Namespace Enums

    Public Enum StreamSpec
        Temperature_and_Pressure = 0
        Pressure_and_Enthalpy = 1
        Pressure_and_Entropy = 2
        Pressure_and_VaporFraction = 3
        Temperature_and_VaporFraction = 4
    End Enum

    Public Enum CompositionBasis
        Molar_Fractions
        Mass_Fractions
        Volumetric_Fractions
        Molar_Flows
        Mass_Flows
        Volumetric_Flows
    End Enum

    Public Enum PhaseName
        Liquid
        Vapor
        Mixture
        Solid
    End Enum

    Public Enum ReactionType
        Equilibrium
        Kinetic
        Heterogeneous_Catalytic
        Conversion
    End Enum

    Public Enum ReactionBasis
        Activity
        Fugacity
        MolarConc
        MassConc
        MolarFrac
        MassFrac
        PartialPress
    End Enum

    Public Enum KOpt
        Gibbs
        Expression
        Constant
    End Enum

End Namespace

Namespace Enums.GraphicObjects

    Public Enum ConType

        ConIn = -1
        ConOut = 1
        ConEn = 0
        ConSp = 2

    End Enum
    Public Enum ConDir
        Up
        Down
        Right
        Left
    End Enum

    Public Enum ObjectType

        NodeIn
        NodeOut
        NodeEn
        Pump
        Tank
        Vessel
        MaterialStream
        EnergyStream
        Compressor
        Expander
        TPVessel
        Cooler
        Heater
        Pipe
        Valve
        Nenhum
        GO_Table
        GO_Text
        GO_Image
        GO_FloatingTable
        OT_Adjust
        OT_Spec
        OT_Recycle
        RCT_Conversion
        RCT_Equilibrium
        RCT_Gibbs
        RCT_CSTR
        RCT_PFR
        HeatExchanger
        ShortcutColumn
        DistillationColumn
        AbsorptionColumn
        RefluxedAbsorber
        ReboiledAbsorber
        OT_EnergyRecycle
        GO_Animation
        ComponentSeparator
        OrificePlate
        CustomUO
        ExcelUO
        CapeOpenUO
        FlowsheetUO
        GO_MasterTable
        SolidSeparator
        Filter
        GO_SpreadsheetTable

    End Enum

    Public Enum ShapeIcon

        DefaultShape
        NodeIn
        NodeOut
        Pump
        Tank
        Vessel
        Compressor
        Expander
        Cooler
        Heater
        Pipe
        Valve
        RCT_Conversion
        RCT_Equilibrium
        RCT_Gibbs
        RCT_CSTR
        RCT_PFR
        HeatExchanger
        DistillationColumn
        AbsorptionColumn
        ComponentSeparator
        OrificePlate

    End Enum

    Public Enum Status
        Calculated
        Calculating
        ErrorCalculating
        Inactive
        Idle
        NotCalculated
    End Enum

End Namespace



