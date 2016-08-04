Public Interface IFlowsheetOptions

    Property NumberFormat As String
    Property FractionNumberFormat As String

    Property SimulationName As String
    Property SimulationAuthor As String
    Property SimulationComments As String

    Property FilePath As String

    Property BackupFileName As String

    <Xml.Serialization.XmlIgnore> Property Password As String
    <Xml.Serialization.XmlIgnore> Property UsePassword As Boolean

    Property FlowsheetSnapToGrid As Boolean
    Property FlowsheetQuickConnect As Boolean
    Property FlowsheetShowConsoleWindow As Boolean
    Property FlowsheetShowCOReportsWindow As Boolean
    Property FlowsheetShowCalculationQueue As Boolean
    Property FlowsheetShowWatchWindow As Boolean

    Property Key As String

    Property SelectedUnitSystem As IUnitsOfMeasure

    Property VisibleProperties As Dictionary(Of String, List(Of String))

    Property FlashAlgorithms As List(Of IFlashAlgorithm)

    Property SimultaneousAdjustSolverEnabled As Boolean

End Interface
