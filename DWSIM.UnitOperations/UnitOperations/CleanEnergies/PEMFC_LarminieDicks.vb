Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations

Public Class PEMFC_LarminieDicks

    Inherits PEMFuelCellUnitOpBase

    Public Overrides Property Prefix As String = "FCA-"

    Public Overrides Function GetDisplayName() As String
        Return "PEM Fuel Cell (Larminie-Dicks)"
    End Function

    Public Overrides Function GetDisplayDescription() As String
        Return "PEM Fuel Cell (OPEM Larminie-Dicks Static Model)"
    End Function

    Public Sub New()

        MyBase.New()

    End Sub

    Public Overrides Sub PopulateEditorPanel(ctner As Object)

    End Sub

    Public Overrides Sub DisplayEditForm()

    End Sub

    Public Overrides Sub UpdateEditForm()

    End Sub

    Public Overrides Sub CloseEditForm()

    End Sub

    Public Overrides Function ReturnInstance(typename As String) As Object

        Return New PEMFC_LarminieDicks

    End Function

    Public Overrides Function GetIconBitmap() As Object

        Return My.Resources.fuel_cell

    End Function

    Public Overrides Function CloneXML() As Object

        Dim obj As ICustomXMLSerialization = New PEMFC_LarminieDicks()
        obj.LoadData(Me.SaveData)
        Return obj

    End Function

    Public Overrides Function CloneJSON() As Object

        Throw New NotImplementedException()

    End Function

End Class
