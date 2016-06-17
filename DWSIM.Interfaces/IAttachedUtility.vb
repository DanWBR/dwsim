Public Interface IAttachedUtility

    Property ID As Integer

    Property Name As String

    Property AttachedTo As ISimulationObject

    Function GetPropertyList() As List(Of String)

    Function GetPropertyValue(pname As String) As Object

    Function GetPropertyUnits(pname As String) As String

    Sub SetPropertyValue(pname As String, pvalue As Object)

    Sub Update()

    Function GetUtilityType() As Enums.FlowsheetUtility

    Property AutoUpdate As Boolean

    Function SaveData() As Dictionary(Of String, Object)

    Sub LoadData(data As Dictionary(Of String, Object))

    Sub Initialize()

    Sub Populate()

End Interface
