Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations

Namespace UnitOperations

    Public Class PEMFC_ChamberLineKim

        Inherits PEMFuelCellUnitOpBase

        Public Overrides Property Prefix As String = "FCA-"

        Public Overrides Function GetDisplayName() As String
            Return "PEM Fuel Cell (Chamberline-Kim)"
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return "PEM Fuel Cell (OPEM Chamberline-Kim Static Model)"
        End Function

        Public Sub New()

            MyBase.New()

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

End Namespace
