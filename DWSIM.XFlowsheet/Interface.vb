Namespace Interfaces

    Public Interface IConnectionPort

        Property ID As String

        Property Name As String

        Property Description As String

        Property Index As Integer

        Property IsConnected As Boolean

        Property IsInput As Boolean

        Property IsOutput As Boolean

        Property IsEnergyPort As Boolean

        Property ConnectedToObjectID As String

        Property ConnectedToObjectPortIndex As Integer

        Property RelativeX As Double

        Property RelativeY As Double

    End Interface

    Public Interface IPFDObject

        Property ID As String

        Property Name As String

        Property Description As String

        Property X As Double

        Property Y As Double

        Property Width As Double

        Property Height As Double

        Property ObjectType As ObjType

        Property Ports As List(Of IConnectionPort)

        Property HasAssociatedSimulationObject As Boolean

        Property AssociatedSimulationObjectID As String

    End Interface

    Public Interface ISimulationObject

        Property ID As String

        Property Name As String

        Property Description As String

        Property PropertyPackageID As String

        Property PFDObjectID As String

        Property ObjectType As ObjType

        Property Parameters As List(Of IParameter)

        Property ExtendedParameters As List(Of IParameter)

    End Interface

    Public Interface IPropertyPackage

        Property ID As String

        Property Name As String

        Property Description As String

        Property Model As PropPackageModel

        Property Parameters As List(Of IParameter)

    End Interface

    Public Interface IFlowsheet

        Property ID As String

        Property Name As String

        Property Description As String

        Property Compounds As List(Of String)

        Property PFDObjects As List(Of IPFDObject)

        Property SimulationObjects As List(Of ISimulationObject)

        Property PropertyPackages As List(Of IPropertyPackage)

        Property Parameters As List(Of IParameter)

        Property DisplayedUnitsOfMeasure As UnitOfMeasureSet

    End Interface

    Public Interface IParameter

        Property ID As String

        Property Name As String

        Property Description As String

        Property Value As Object

        Property ValueType As ParamValueType

        Property Units As UnitOfMeasure

        Property DefaultUnits As String

    End Interface

    Public Interface IUnitsOfMeasure

        Function GetUnitSet(measureID As UnitOfMeasure) As List(Of String)

        Function GetCurrentUnits(measureID As UnitOfMeasure) As String

        Function GetUnitType(unit As String) As UnitOfMeasure

        Property ID As String

        Property Name As String

        Property Description As String

        Property Mass As String

        Property Reacrateheterog As String

        Property Area As String

        Property Conductance As String

        Property Distance As String

        Property Time As String

        Property Volume As String

        Property Molarvolume As String

        Property Diameter As String

        Property Thickness As String

        Property Molarconc As String

        Property Massconc As String

        Property Heattransfcoeff As String

        Property Force As String

        Property Accel As String

        Property Specvol As String

        Property Reacrate As String

        Property Velocity As String

        Property Foulingfactor As String

        Property Cakeresistance As String

        Property Mediumresistance As String

        Property Molarenthalpy As String

        Property Molarentropy As String

        Property Compressibility As String

        Property Density As String

        Property MassEnthalpy As String

        Property MassEntropy As String

        Property Molarflow As String

        Property MassFlow As String

        Property HeatCapacity As String

        Property JouleThomsonCoefficient As String

        Property MolecularWeight As String

        Property Pressure As String

        Property Temperature As String

        Property SpeedOfSound As String

        Property ThermalConductivity As String

        Property DynamicViscosity As String

        Property Kinematicviscosity As String

        Property VolumetricFlow As String

        Property Heatflow As String

        Property Head As String

        Property DeltaT As String

        Property DeltaP As String

        Property SurfaceTension As String

        Property Diffusivity As String

    End Interface

    Public Enum UnitOfMeasureSet

        SI = 0
        SI_Engineering = 1
        CGS = 2
        Imperial = 3

    End Enum

    Public Enum UnitOfMeasure

        Accel = 0

        Activity = 1

        ActivityCoefficient = 2

        Area = 3

        CakeResistance = 4

        Conductance = 5

        Compressibility = 6

        DeltaP = 7

        DeltaT = 8

        Density = 9

        Diameter = 10

        Diffusivity = 11

        Distance = 12

        DynamicViscosity = 13

        MassEnthalpy = 14

        MassEntropy = 15

        Force = 16

        Foulingfactor = 17

        Head = 18

        HeatTransfCoeff = 19

        HeatCapacity = 20

        HeatFlow = 21

        JouleThomsonCoefficient = 22

        KinematicViscosity = 23

        Mass = 24

        MassConc = 25

        MassFlow = 26

        Mediumresistance = 27

        MolarConc = 28

        MolarEnthalpy = 29

        MolarEntropy = 30

        MolarVolume = 31

        MolarFlow = 32

        MolecularWeight = 33

        Pressure = 34

        ReacRate = 35

        ReacRateHeterog = 36

        SpecificVol = 37

        SpeedOfSound = 38

        SurfaceTension = 39

        Temperature = 40

        ThermalConductivity = 41

        Thickness = 42

        Time = 43

        Velocity = 44

        Volume = 45

        VolumetricFlow = 46

        None = 47

    End Enum

    Public Enum PropPackageModel

        Ideal = 0

        PR_EOS = 1

        PR78_EOS = 2

        SRK_EOS = 3

        NRTL = 4

        UNIQUAC = 5

        UNIFAC = 6

        Mod_UNIFAC_Dortmund = 7

        Mod_UNIFAC_NIST = 8

        Lee_Kesler_Plocker = 9

        Chao_Seader = 10

        Grayson_Streed = 11

    End Enum

    Public Enum ParamValueType

        Type_Byte = 0

        Type_Int32 = 1

        Type_Int64 = 2

        Type_String = 3

        Type_Single = 4

        Type_Double = 5

        Type_Boolean = 6

        Type_ListString = 7

        Type_ListInt32 = 8

        Type_ListInt64 = 9

        Type_ListSingle = 10

        Type_ListDouble = 11

        Type_DictStringString = 12

        Type_DictStringInt32 = 13

        Type_DictStringInt64 = 14

        Type_DictStringSingle = 15

        Type_DictStringDouble = 16

        Type_DictInt32String = 17

        Type_DictInt32Int32 = 18

        Type_DictInt32Int64 = 19

        Type_DictInt32Single = 20

        Type_DictInt32Double = 21

    End Enum

    Public Enum ObjType

        MaterialStream = 0

        EnergyStream = 1

        Mixer = 2

        Splitter = 3

        Heater = 4

        Cooler = 5

        Compressor = 6

        Expander = 7

        Pump = 8

        Valve = 9

        SeparatorVessel = 10

        HeatExchanger = 11

    End Enum

End Namespace
