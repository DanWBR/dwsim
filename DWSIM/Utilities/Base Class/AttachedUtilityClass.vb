Public Class AttachedUtilityClass

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property UtilityName As String Implements Interfaces.IAttachedUtility.Name

    Public Overridable Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Throw New NotImplementedException
    End Function

    Public Overridable Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Throw New NotImplementedException
    End Function

    Public Overridable Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue
        Throw New NotImplementedException
    End Sub

    Public Overridable Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Throw New NotImplementedException
    End Function

    Public Overridable Sub UpdateResults() Implements Interfaces.IAttachedUtility.Update
        Throw New NotImplementedException
    End Sub

End Class
