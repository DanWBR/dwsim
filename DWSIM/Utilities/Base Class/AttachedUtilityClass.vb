Public Class AttachedUtilityClass

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property Name1 As String Implements Interfaces.IAttachedUtility.Name

End Class
