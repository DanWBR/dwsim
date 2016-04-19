<System.Serializable()> Public Class CalculationArgs

    Implements Interfaces.ICalculationArgs

    Public Property Sender As String = "" Implements Interfaces.ICalculationArgs.Sender
    Public Property Calculated As Boolean = False Implements Interfaces.ICalculationArgs.Calculated
    Public Property Tag As String = "" Implements Interfaces.ICalculationArgs.Tag
    Public Property Name As String = "" Implements Interfaces.ICalculationArgs.Name
    Public Property ObjectType As Interfaces.Enums.GraphicObjects.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.Nenhum Implements Interfaces.ICalculationArgs.ObjectType

End Class

