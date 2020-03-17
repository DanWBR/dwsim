Public Interface IDynamicsCauseAndEffectItem

    Property ID As String

    Property Description As String

    Property Enabled As Boolean

    Property AssociatedIndicator As String

    Property AssociatedIndicatorAlarm As Enums.Dynamics.DynamicsAlarmType

    Property SimulationObjectID As String

    Property SimulationObjectProperty As String

    Property SimulationObjectPropertyValue As String

    Property SimulationObjectPropertyUnits As String

    Property ScriptID As String

End Interface
