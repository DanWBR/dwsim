Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations
Imports Python.Runtime
Imports Eto.Forms
Imports DWSIM.UI.Shared.Common
Imports System.Globalization
Imports DWSIM.Thermodynamics.Streams

Namespace UnitOperations

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

            AddDefaultInputParameters()

        End Sub

        Public Overrides Sub AddDefaultInputParameters()

            InputParameters = New Dictionary(Of String, Auxiliary.PEMFuelCellModelParameter)()
            InputParameters.Add("i-start", New Auxiliary.PEMFuelCellModelParameter("i-start", "Cell Operating Current Starting Point", 0, "A"))
            InputParameters.Add("i-stop", New Auxiliary.PEMFuelCellModelParameter("i-stop", "Cell Operating Current Ending Point", 75, "A"))
            InputParameters.Add("i-step", New Auxiliary.PEMFuelCellModelParameter("i-step", "Cell Operating Current Step", 0.1, "A"))
            InputParameters.Add("A", New Auxiliary.PEMFuelCellModelParameter("A", "Active Area", 50.6, "cm2"))
            InputParameters.Add("l", New Auxiliary.PEMFuelCellModelParameter("l", "Membrane Thickness", 0.0178, "cm"))
            InputParameters.Add("lambda", New Auxiliary.PEMFuelCellModelParameter("lambda", "Adjustable Parameter (14-23)", 23, ""))
            InputParameters.Add("R", New Auxiliary.PEMFuelCellModelParameter("R", "R-Electronic", 0, "ohm"))
            InputParameters.Add("JMax", New Auxiliary.PEMFuelCellModelParameter("JMax", "Maximum Current Density", 1.5, "A/cm2"))
            InputParameters.Add("N", New Auxiliary.PEMFuelCellModelParameter("N", "Number of Single Cells", 1, ""))

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

            If Settings.RunningPlatform() = Settings.Platform.Windows Then

                DWSIM.GlobalSettings.Settings.InitializePythonEnvironment()

            End If

            Dim msin1, msin2, msout As MaterialStream

            msin1 = GetInletMaterialStream(0)
            msin2 = GetInletMaterialStream(1)
            msout = GetOutletMaterialStream(0)

            If msin2 Is Nothing Then
                Throw New Exception("Please update your model and connect a second inlet stream to this Fuel Cell.")
            End If

            Dim esout = GetOutletEnergyStream(1)

            Dim names = msin1.Phases(0).Compounds.Keys.ToList()

            If Not names.Contains("Water") Then Throw New Exception("Needs Water compound")
            If Not names.Contains("Hydrogen") Then Throw New Exception("Needs Hydrogen compound")
            If Not names.Contains("Oxygen") Then Throw New Exception("Needs Oxygen compound")

            Dim Pin1, Pin2 As Double

            Pin1 = msin1.GetPressure()
            Pin2 = msin1.GetPressure()

            Dim T = (msin1.GetTemperature() + msin2.GetTemperature()) / 2

            Dim m1, m2, w1, w2, xH2, xO2 As Double

            m1 = msin1.GetMolarFlow()
            m2 = msin2.GetMolarFlow()

            w1 = msin1.GetMassFlow()
            w2 = msin2.GetMassFlow()

            xH2 = msin1.Phases(2).Compounds("Hydrogen").MoleFraction.GetValueOrDefault()
            xO2 = msin2.Phases(2).Compounds("Oxygen").MoleFraction.GetValueOrDefault()

            Dim PH2 = m1 / (m1 + m2) * xH2 * Pin1 / 101325.0
            Dim PO2 = m2 / (m1 + m2) * xO2 * Pin2 / 101325.0

            Dim results As Object = Nothing

            Using Py.GIL

                Dim sys As Object = Py.Import("sys")
                Dim libpath = Path.Combine(Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location), "python_packages")
                sys.path.append(libpath)

                Dim opem As Object = Py.Import("opem.Static.Amphlett")

                Dim parameters As New PyDict()
                parameters("T") = T.ToPython()
                parameters("PH2") = PH2.ToPython()
                parameters("PO2") = PO2.ToPython()
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

                Dim htmlpath = SharedClasses.Utility.GetTempFileName()
                Dim csvpath = SharedClasses.Utility.GetTempFileName()
                Dim opempath = SharedClasses.Utility.GetTempFileName()

                File.WriteAllText(htmlpath, "")
                File.WriteAllText(csvpath, "")
                File.WriteAllText(opempath, "")

                results = opem.Static_Analysis(InputMethod:=parameters.ToPython(), TestMode:=True.ToPython(),
                                               PrintMode:=False.ToPython(), ReportMode:=True.ToPython(),
                                               HTMLfilepath:=htmlpath, CSVfilepath:=csvpath, OPEMfilepath:=opempath)

                If results Is Nothing OrElse results("Status").ToString() = "False" Then
                    Throw New Exception("Calculation error")
                End If

                Try
                    File.Delete(htmlpath)
                    File.Delete(csvpath)
                    File.Delete(opempath)
                Catch ex As Exception
                End Try

                Dim P = ToList(results("P"))
                Dim I = ToList(results("I"))
                Dim V = ToList(results("V"))
                Dim EFF = ToList(results("EFF"))
                Dim Ph = ToList(results("Ph"))
                Dim V0 = results("V0").ToString().ToDoubleFromInvariant()
                Dim K = results("K").ToString().ToDoubleFromInvariant()
                Dim VE = ToList(results("VE"))
                Dim Eta_Active = ToList(results("Eta_Active"))
                Dim Eta_Ohmic = ToList(results("Eta_Ohmic"))
                Dim Eta_Conc = ToList(results("Eta_Conc"))

                HTMLreport = results("HTML").ToString()
                CSVreport = results("CSV").ToString()
                OPEMreport = results("OPEM").ToString()

                OutputParameters.Clear()
                OutputParameters.Add("I", New Auxiliary.PEMFuelCellModelParameter("I", "Cell Operating Current", I.Last(), "A"))
                OutputParameters.Add("P", New Auxiliary.PEMFuelCellModelParameter("P", "Power", P.Last(), "W"))
                OutputParameters.Last.Value.ValuesX = I
                OutputParameters.Last.Value.ValuesY = P
                OutputParameters.Last.Value.TitleX = "Current"
                OutputParameters.Last.Value.TitleY = "Power"
                OutputParameters.Last.Value.UnitsX = "A"
                OutputParameters.Last.Value.UnitsY = "W"
                OutputParameters.Add("Ph", New Auxiliary.PEMFuelCellModelParameter("Ph", "Thermal Power", Ph.Last(), "W"))
                OutputParameters.Last.Value.ValuesX = I
                OutputParameters.Last.Value.ValuesY = Ph
                OutputParameters.Last.Value.TitleX = "Current"
                OutputParameters.Last.Value.TitleY = "Thermal Power"
                OutputParameters.Last.Value.UnitsX = "A"
                OutputParameters.Last.Value.UnitsY = "W"
                OutputParameters.Add("EFF", New Auxiliary.PEMFuelCellModelParameter("EFF", "Efficiency", EFF.Last(), ""))
                OutputParameters.Last.Value.ValuesX = I
                OutputParameters.Last.Value.ValuesY = EFF
                OutputParameters.Last.Value.TitleX = "Current"
                OutputParameters.Last.Value.TitleY = "Efficiency"
                OutputParameters.Last.Value.UnitsX = "A"
                OutputParameters.Last.Value.UnitsY = ""
                OutputParameters.Add("V", New Auxiliary.PEMFuelCellModelParameter("V", "FC Voltage", V.Last(), "V"))
                OutputParameters.Last.Value.ValuesX = I
                OutputParameters.Last.Value.ValuesY = V
                OutputParameters.Last.Value.TitleX = "Current"
                OutputParameters.Last.Value.TitleY = "FC Voltage"
                OutputParameters.Last.Value.UnitsX = "A"
                OutputParameters.Last.Value.UnitsY = "V"
                OutputParameters.Add("VE", New Auxiliary.PEMFuelCellModelParameter("VE", "Estimated FC Voltage", VE.Last(), "V"))
                OutputParameters.Last.Value.ValuesX = I
                OutputParameters.Last.Value.ValuesY = VE
                OutputParameters.Last.Value.TitleX = "Current"
                OutputParameters.Last.Value.TitleY = "Estimated FC Voltage"
                OutputParameters.Last.Value.UnitsX = "A"
                OutputParameters.Last.Value.UnitsY = "V"

                Dim Current = I.Last()

                Dim WasteHeat = Ph.Last() / 1000.0 'kW

                Dim ElectronTransfer = Current / 96485.3365 * InputParameters("N").Value 'mol/s

                Dim waterr = ElectronTransfer / 4 * 2 'mol/s
                Dim h2r = ElectronTransfer / 4 * 2 'mol/s
                Dim o2r = ElectronTransfer / 4 'mol/s

                Dim N01 = msin1.Phases(0).Compounds.Values.Select(Function(c) c.MolarFlow.GetValueOrDefault()).ToList()
                Dim N02 = msin2.Phases(0).Compounds.Values.Select(Function(c) c.MolarFlow.GetValueOrDefault()).ToList()

                Dim Nf = New List(Of Double)(N01)

                For j As Integer = 0 To N01.Count - 1
                    If names(j) = "Water" Then
                        Nf(j) = N01(j) + N02(j) + waterr
                    ElseIf names(j) = "Hydrogen" Then
                        Nf(j) = N01(j) + N02(j) - h2r
                        If (Nf(j) < 0.0) Then Throw New Exception("Negative Hydrogen molar flow calculated. Please check inputs.")
                    ElseIf names(j) = "Oxygen" Then
                        Nf(j) = N01(j) + N02(j) - o2r
                        If (Nf(j) < 0.0) Then Throw New Exception("Negative Oxygen molar flow calculated. Please check inputs.")
                    End If
                Next

                msout.Clear()
                msout.ClearAllProps()

                msout.SetOverallComposition(Nf.ToArray().MultiplyConstY(1.0 / Nf.Sum))
                msout.SetMolarFlow(Nf.Sum)
                msout.SetPressure(Math.Min(Pin1, Pin2) / 2)
                msout.SetMassEnthalpy(w1 / (w1 + w2) * msin1.GetMassEnthalpy() + w2 / (w1 + w2) * msin2.GetMassEnthalpy() + WasteHeat / (w1 + w2))
                msout.SetFlashSpec("PH")

                msout.AtEquilibrium = False

                esout.EnergyFlow = P.Last() / 1000.0

            End Using

        End Sub


        Public Overrides Sub PopulateEditorPanel(ctner As Object)


            Dim container As DynamicLayout = ctner

            Dim su = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            For Each param In InputParameters.Values
                container.CreateAndAddTextBoxRow(nf, param.Name + " (" + param.Units + ")", param.Value,
                                                Sub(tb, e)
                                                    If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                        param.Value = tb.Text.ToDoubleFromInvariant()
                                                    End If
                                                End Sub)
                container.CreateAndAddDescriptionRow(param.Description)
            Next

        End Sub

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As CultureInfo, nf As String) As String

            Dim sb As New Text.StringBuilder()

            For Each param In OutputParameters.Values
                sb.AppendLine(String.Format("{0}: {1} {2}", param.Name, param.Value.ToString(nf), param.Units))
            Next

            Return sb.ToString()

        End Function

    End Class

End Namespace