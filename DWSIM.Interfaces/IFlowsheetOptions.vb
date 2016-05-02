Public Interface IFlowsheetOptions

    Property NumberFormat As String
    Property FractionNumberFormat As String

    Property SempreCalcularFlashPH As Boolean

    Property CalculateBubbleAndDewPoints As Boolean

    Property ValidateEquilibriumCalc As Boolean
    Property UsePhaseIdentificationAlgorithm As Boolean

    Property SimulationName As String
    Property SimulationAuthor As String
    Property SimulationComments As String

    Property FilePath As String

    Property BackupFileName As String

    Property ThreePhaseFlashStabTestSeverity As Integer
    Property ThreePhaseFlashStabTestCompIds As String()

    Property FlashValidationDGETolerancePct As Double

    <Xml.Serialization.XmlIgnore> Property Password As String
    <Xml.Serialization.XmlIgnore> Property UsePassword As Boolean

    Property FlowsheetSnapToGrid As Boolean
    Property FlowsheetQuickConnect As Boolean
    Property FlowsheetShowConsoleWindow As Boolean
    Property FlowsheetShowCOReportsWindow As Boolean
    Property FlowsheetShowCalculationQueue As Boolean
    Property FlowsheetShowWatchWindow As Boolean

    Property BinaryEnvelopeExpData As String

    Property Key As String

    Property PropertyPackageFlashAlgorithm As Integer

    Property SelectedUnitSystem As IUnitsOfMeasure
    Property PreferredFlashAlgorithmOptimizer As Enums.OptimizationMethod

End Interface
