Public Interface IIndicator

    Property MinimumValue As Double

    Property MaximumValue As Double

    Property SelectedObjectID As String

    Property SelectedProperty As String

    Property SelectedPropertyType As Enums.UnitOfMeasure

    Property SelectedPropertyUnits As String

    Property DecimalDigits As Integer

    Property IntegralDigits As Integer

    Property VeryLowAlarmEnabled As Boolean

    Property LowAlarmEnabled As Boolean

    Property HighAlarmEnabled As Boolean

    Property VeryHighAlarmEnabled As Boolean

    Property VeryLowAlarmValue As Double

    Property LowAlarmValue As Double

    Property HighAlarmValue As Double

    Property VeryHighAlarmValue As Double

    Property VeryLowAlarmActive As Boolean

    Property LowAlarmActive As Boolean

    Property HighAlarmActive As Boolean

    Property VeryHighAlarmActive As Boolean

    Property ShowAlarms As Boolean

End Interface
