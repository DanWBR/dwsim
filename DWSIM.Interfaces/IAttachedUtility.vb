Public Interface IAttachedUtility

    Property ID As Integer

    Property Name As String

    Property AttachedTo As ISimulationObject

    Function GetPropertyList() As List(Of String)

    Function GetPropertyValue(pname As String) As Object

    Function GetPropertyUnits(pname As String) As String

    Sub SetPropertyValue(pname As String, pvalue As Object)

    Sub Update()

End Interface
