Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations
Imports Python.Runtime

Public Class PEMFC_Amphlett

    Inherits PEMFuelCellUnitOpBase

    Public Overrides Property Prefix As String = "FCA-"

    Public Overrides Function GetDisplayName() As String
        Return "PEM Fuel Cell (Amphlett)"
    End Function

    Public Overrides Function GetDisplayDescription() As String
        Return "PEM Fuel Cell (OPEM Amphlett Static Model)"
    End Function

    Public Sub New()

        MyBase.New()

        InputParameters.Clear()
        InputParameters.Add("i-start", New Auxiliary.PEMFuelCellModelParameter("i-start", "Cell operating current start point", 0, "A"))
        InputParameters.Add("i-stop", New Auxiliary.PEMFuelCellModelParameter("i-stop", "Cell operating current end point", 75, "A"))
        InputParameters.Add("i-step", New Auxiliary.PEMFuelCellModelParameter("i-step", "Cell operating current", 0.1, "A"))
        InputParameters.Add("A", New Auxiliary.PEMFuelCellModelParameter("A", "Active area", 50.6, "cm2"))
        InputParameters.Add("l", New Auxiliary.PEMFuelCellModelParameter("l", "Membrane thickness", 0.0178, "cm"))
        InputParameters.Add("lambda", New Auxiliary.PEMFuelCellModelParameter("lambda", "An adjustable parameter with a min value of 14 And max value of 23", 23, ""))
        InputParameters.Add("R", New Auxiliary.PEMFuelCellModelParameter("R", "R-Electronic", 0, "ohm"))
        InputParameters.Add("JMax", New Auxiliary.PEMFuelCellModelParameter("JMax", "Maximum current density", 1.5, "A/cm2"))
        InputParameters.Add("N", New Auxiliary.PEMFuelCellModelParameter("N", "Number of single cells", 1, ""))

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

        Return New PEMFC_Amphlett

    End Function

    Public Overrides Function GetIconBitmap() As Object

        Return My.Resources.fuel_cell

    End Function

    Public Overrides Function CloneXML() As Object

        Dim obj As ICustomXMLSerialization = New PEMFC_Amphlett()
        obj.LoadData(Me.SaveData)
        Return obj

    End Function

    Public Overrides Function CloneJSON() As Object

        Throw New NotImplementedException()

    End Function

    Public Overrides Sub Calculate(Optional args As Object = Nothing)

        DWSIM.GlobalSettings.Settings.ShutdownPythonEnvironment()

        DWSIM.GlobalSettings.Settings.InitializePythonEnvironment(OPEMPath)

        Using Py.GIL

            Dim opem As Object = Py.Import("opem.Static.Amphlett")

            Dim parameters As New PyDict()
            parameters("T") = 343.15.ToPython()
            parameters("PH2") = 1.0.ToPython()
            parameters("PO2") = 1.0.ToPython()
            parameters("i-start") = InputParameters("i-start").Value.ToPython()
            parameters("i-step") = InputParameters("i-step").Value.ToPython()
            parameters("i-stop") = InputParameters("i-stop").Value.ToPython()
            parameters("A") = InputParameters("A").Value.ToPython()
            parameters("l") = InputParameters("l").Value.ToPython()
            parameters("lambda") = InputParameters("lambda").Value.ToPython()
            parameters("N") = InputParameters("N").Value.ToPython()
            parameters("R") = InputParameters("R").Value.ToPython()
            parameters("JMax") = InputParameters("JMax").Value.ToPython()
            parameters("Name") = "Amphlett_Test".ToPython()

            'Test_Vector = {"T":  343.15,"PH2": 1,"PO2": 1,"i-start": 0,
            '"i-stop": 75,"i-step": 0.1,"A": 50.6,"l": 0.0178,
            '"lambda": 23,"N": 1,"R": 0,"JMax": 1.5,"Name": "Amphlett_Test"}

            Dim results = opem.Static_Analysis(InputMethod:=parameters.ToPython(), TestMode:=True.ToPython(), PrintMode:=False.ToPython(), ReportMode:=False.ToPython())

            Dim P = ToList(results("P"))
            Dim I = ToList(results("I"))
            Dim V = ToList(results("V"))
            Dim EFF = ToList(results("EFF"))
            Dim Ph = ToList(results("Ph"))
            Dim V0 = ToList(results("V0"))
            Dim K = ToList(results("K"))
            Dim VE = ToList(results("VE"))
            Dim Eta_Active = ToList(results("Eta_Active"))
            Dim Eta_Ohmic = ToList(results("Eta_Ohmic"))
            Dim Eta_Conc = ToList(results("Eta_Conc"))

            Console.WriteLine(results)

        End Using

    End Sub

End Class
