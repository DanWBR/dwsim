<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IRecycle

    ReadOnly Property Errors As Dictionary(Of String, Double)
    ReadOnly Property Values As Dictionary(Of String, Double)
    Property AccelerationMethod() As Enums.AccelMethod
    Sub SetOutletStreamProperties()
    Property Converged As Boolean
    Property ConvergenceHistory As IRecycleConvergenceHistory

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IRecycleConvergenceHistory

    Property Temperatura As Double
    Property Pressao As Double
    Property VazaoMassica As Double
    Property Entalpia As Double
    Property Entropia As Double
    Property Temperatura0 As Double
    Property Pressao0 As Double
    Property VazaoMassica0 As Double
    Property Entalpia0 As Double
    Property Entropia0 As Double

    Property TemperaturaE As Double
    Property PressaoE As Double
    Property VazaoMassicaE As Double
    Property EntalpiaE As Double
    Property EntropiaE As Double
    Property TemperaturaE0 As Double
    Property PressaoE0 As Double
    Property VazaoMassicaE0 As Double
    Property EntalpiaE0 As Double
    Property EntropiaE0 As Double

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ISpec

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IAdjust

    Property SimultaneousAdjust As Boolean

    Property Referenced As Boolean

    Property AdjustValue As Double

    Property ControlledObjectData As ISpecialOpObjectInfo

    Property ManipulatedObjectData As ISpecialOpObjectInfo

    Property ReferencedObjectData As ISpecialOpObjectInfo

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ISpecialOpObjectInfo

    Property Type As String
    Property Name As String
    Property ID As String
    Property PropertyName As String

End Interface
