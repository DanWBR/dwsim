Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations

Public Class PEMFC_ChamberLineKim

    Inherits PEMFuelCellUnitOpBase

    Public Overrides Property Prefix As String = "FCA-"

    Public Sub New()

        MyBase.New()

        _name = "PEM Fuel Cell (Chamberline-Kim)"
        _desc = "PEM Fuel Cell (OPEM Chamberline-Kim Static Model)"

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

        Return New PEMFC_ChamberLineKim

    End Function

    Public Overrides Function GetIconBitmap() As Object

        Return My.Resources.fuel_cell

    End Function

    Public Overrides Function CloneXML() As Object

        Dim obj As ICustomXMLSerialization = New PEMFC_ChamberLineKim()
        obj.LoadData(Me.SaveData)
        Return obj

    End Function

    Public Overrides Function CloneJSON() As Object

        Throw New NotImplementedException()

    End Function

End Class
