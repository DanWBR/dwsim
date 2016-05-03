Public Interface IAttachedUtility

    Property ID As Integer

    Property Name As String

    Property AttachedTo As ISimulationObject

    Property GetPropertyList As List(Of String)

    Function GetPropertyValue(pname As String) As Object

    Function GetPropertyUnits(pname As String) As String

    Sub SetPropertyValue(pname As String, pvalue As Object)

End Interface
