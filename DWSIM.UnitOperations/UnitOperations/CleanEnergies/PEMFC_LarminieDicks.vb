Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations
Imports Python.Runtime

Namespace UnitOperations

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

            InputParameters.Clear()
            InputParameters.Add("i-start", New Auxiliary.PEMFuelCellModelParameter("i-start", "Cell Operating Current Starting Point", 0, "A"))
            InputParameters.Add("i-stop", New Auxiliary.PEMFuelCellModelParameter("i-stop", "Cell Operating Current Ending Point", 75, "A"))
            InputParameters.Add("i-step", New Auxiliary.PEMFuelCellModelParameter("i-step", "Cell Operating Current Step", 0.1, "A"))
            InputParameters.Add("i_n", New Auxiliary.PEMFuelCellModelParameter("i_n", "Internal Current", 1, "A"))
            InputParameters.Add("i_0", New Auxiliary.PEMFuelCellModelParameter("i_0", "Exchange Current", 1, "A"))
            InputParameters.Add("i_L", New Auxiliary.PEMFuelCellModelParameter("i_L", "Limiting Current", 1, "A"))
            InputParameters.Add("E0", New Auxiliary.PEMFuelCellModelParameter("E0", "Fuel cell reversible no loss voltage", 0, "V"))
            InputParameters.Add("A", New Auxiliary.PEMFuelCellModelParameter("A", "Slope of Tafel Line", 0, "V"))
            InputParameters.Add("RM", New Auxiliary.PEMFuelCellModelParameter("RM", "Membrane and Contact Resistances", 1, "ohm"))
            InputParameters.Add("N", New Auxiliary.PEMFuelCellModelParameter("N", "Number of Single Cells", 1, ""))

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

End Namespace
