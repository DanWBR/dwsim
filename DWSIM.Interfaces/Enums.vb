Namespace Enums

    Public Enum FlowsheetUtility
        PhaseEnvelope = 0
        PhaseEnvelopeBinary = 1
        PhaseEnvelopeTernary = 2
        NaturalGasHydrates = 3
        TrueCriticalPoint = 4
        PSVSizing = 5
        SeparatorSizing = 6
        PetroleumProperties = 7
        PureCompoundProperties = 8
    End Enum

    Public Enum UnitOfMeasure

        accel
        activity
        activityCoefficient
        area
        boilingPointTemperature
        cakeresistance
        cinematic_viscosity
        compressibility
        compressibilityFactor
        deltaP
        deltaT
        density
        diameter
        distance
        enthalpy
        entropy
        excessEnthalpy
        excessEntropy
        force
        foulingfactor
        fugacity
        fugacityCoefficient
        gor
        head
        heat_transf_coeff
        heatCapacityCp
        heatCapacityCv
        heatflow
        idealGasHeatCapacity
        jouleThomsonCoefficient
        kvalue
        logFugacityCoefficient
        logKvalue
        mass
        mass_conc
        massflow
        massfraction
        mediumresistance
        meltingTemperature
        molar_conc
        molar_enthalpy
        molar_entropy
        molar_volume
        molarflow
        molarfraction
        molecularWeight
        pressure
        reac_rate
        reac_rate_heterog
        spec_vol
        speedOfSound
        surfaceTension
        temperature
        thermalConductivity
        thermalConductivityOfLiquid
        thermalConductivityOfVapor
        thickness
        time
        vaporPressure
        velocity
        viscosity
        viscosityOfLiquid
        viscosityOfVapor
        volume
        volumetricFlow

    End Enum

    Public Enum OptimizationMethod
        Limited_Memory_BGFS = 0
        Truncated_Newton = 1
        Simplex = 2
        IPOPT = 3
        ParticleSwarm = 4
        LocalUnimodalSampling = 5
        GradientDescent = 6
        DifferentialEvolution = 7
        ParticleSwarmOptimization = 8
        ManyOptimizingLiaisons = 9
        Mesh = 10
    End Enum

    Public Enum AccelMethod
        None
        Wegstein
        Dominant_Eigenvalue
        GlobalBroyden
    End Enum

    Public Enum FlashMethod
        Default_Algorithm = 0
        Nested_Loops_VLE = 1
        Nested_Loops_VLLE = 2
        Nested_Loops_Immiscible_VLLE = 3
        Inside_Out_VLE = 4
        Inside_Out_VLLE = 5
        Gibbs_Minimization_VLE = 6
        Gibbs_Minimization_VLLE = 7
        Simple_LLE = 8
        Nested_Loops_SLE_Eutectic = 9
        Nested_Loops_SLE_SolidSolution = 10
    End Enum

    Public Enum FlashSetting

        PTFlash_External_Loop_Tolerance = 0
        PTFlash_Internal_Loop_Tolerance = 1
        PTFlash_Maximum_Number_Of_External_Iterations = 2
        PTFlash_Maximum_Number_Of_Internal_Iterations = 3

        PHFlash_Internal_Loop_Tolerance = 4
        PHFlash_External_Loop_Tolerance = 5
        PHFlash_Maximum_Number_Of_Internal_Iterations = 6
        PHFlash_Maximum_Number_Of_External_Iterations = 7
    
        PSFlash_Internal_Loop_Tolerance = 8
        PSFlash_External_Loop_Tolerance = 9
        PSFlash_Maximum_Number_Of_Internal_Iterations = 10
        PSFlash_Maximum_Number_Of_External_Iterations = 11

        ThreePhaseFlashStabTestSeverity = 12
        ThreePhaseFlashStabTestCompIds = 13

        CalculateBubbleAndDewPoints = 14

        ValidateEquilibriumCalc = 15
        ValidationGibbsTolerance = 16

        UsePhaseIdentificationAlgorithm = 17

        Replace_PTFlash = 18

        GM_OptimizationMethod = 19
        IO_FastMode = 20
        NL_FastMode = 21

    End Enum

    Public Enum FlashCalculationType
        PressureTemperature = 0
        PressureEnthalpy = 1
        PressureEntropy = 2
        TemperatureEnthalpy = 3
        TemperatureEntropy = 4
        PressureVaporFraction = 5
        TemperatureVaporFraction = 6
        PressureSolidFraction = 7
        TemperatureSolidFraction = 8
    End Enum

    Public Enum AdjustVarType
        Manipulated = 0
        Controlled = 1
        Reference = 2
        None = 3
    End Enum

    Public Enum SpecVarType
        Source = 0
        Target = 1
        None = 2
    End Enum

    Public Enum PropertyType
        RO = 0
        RW = 1
        WR = 2
        ALL = 3
    End Enum

    Public Enum StreamSpec
        Temperature_and_Pressure = 0
        Pressure_and_Enthalpy = 1
        Pressure_and_Entropy = 2
        Pressure_and_VaporFraction = 3
        Temperature_and_VaporFraction = 4
        Pressure_and_SolidFraction = 5
    End Enum

    Public Enum CompositionBasis
        Molar_Fractions
        Mass_Fractions
        Volumetric_Fractions
        Molar_Flows
        Mass_Flows
        Volumetric_Flows
    End Enum

    Public Enum PhaseLabel
        Mixture
        Vapor
        LiquidMixture
        Liquid1
        Liquid2
        Liquid3
        Aqueous
        Solid
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



