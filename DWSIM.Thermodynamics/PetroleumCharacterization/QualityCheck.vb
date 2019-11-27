'    Petroleum Assay Characterization Quality Check
'    Copyright 2018 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.Text
Imports DWSIM.Interfaces
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses.Utilities.PetroleumCharacterization.Assay
Imports Eto.Forms
Imports DWSIM.UI.Shared.Common
Imports DWSIM.Thermodynamics.BaseClasses
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class QualityCheck

    Private _ms As MaterialStream
    Private _assay As Assay

    Private _report As New StringBuilder
    Private _compounds As New List(Of ICompoundConstantProperties)
    Private _dlgresult As DialogResult

    Sub New(assay As Assay, ms As MaterialStream)
        _assay = assay
        _ms = ms
    End Sub

    Sub DoQualityCheck()

        GetQualityCheckReport()

    End Sub

    Function GetQualityCheckReport() As String

        _compounds = _ms.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties).ToList

        _report.Clear()
        _report.AppendLine("Petroleum Assay Characterization Quality Check")
        _report.AppendLine()

        Dim su As Interfaces.IUnitsOfMeasure

        If Not _ms.FlowSheet Is Nothing Then
            su = _ms.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        Else
            su = New SharedClasses.SystemsOfUnits.SI
        End If

        Dim pp = _ms.PropertyPackage

        If _assay.IsBulk Then

            'bulk characterization

            'check mw
            If _assay.MW > 0 Then
                _ms.PropertyPackage = pp
                _ms.ClearCalculatedProps()
                pp.CurrentMaterialStream = _ms
                Dim mwcalc = _ms.PropertyPackage.AUX_MMM(PropertyPackages.Phase.Mixture)
                Dim mwerr = (_assay.MW - mwcalc) / _assay.MW
                _report.AppendLine(String.Format("Molecular Weight (Specified): {0:N2}", _assay.MW))
                _report.AppendLine(String.Format("Molecular Weight (Calculated): {0:N2}", mwcalc))
                _report.AppendLine(String.Format("Molecular Weight Error: {0:P}", mwerr))
                _report.AppendLine()
            End If

            If _assay.SG60 > 0 Then
                _ms.PropertyPackage = pp
                _ms.ClearCalculatedProps()
                pp.CurrentMaterialStream = _ms
                _ms.Phases(0).Properties.temperature = 15.56 + 273.15
                _ms.Phases(0).Properties.pressure = 101325
                _ms.SpecType = Enums.StreamSpec.Temperature_and_Pressure
                Try
                    _ms.Calculate()
                    Dim sgcalc = _ms.Phases(3).Properties.density.GetValueOrDefault / 1000
                    Dim sgerr = (_assay.SG60 - sgcalc) / _assay.SG60
                    _report.AppendLine(String.Format("Specific Gravity (Specified): {0:N4}", _assay.SG60))
                    _report.AppendLine(String.Format("Specific Gravity (Calculated): {0:N4}", sgcalc))
                    _report.AppendLine(String.Format("Specific Gravity Error: {0:P}", sgerr))
                Catch ex As Exception
                    _ms.Calculate()
                    _report.AppendLine(String.Format("Specific Gravity (Specified): {0:N4}", _assay.SG60))
                    _report.AppendLine(String.Format("Specific Gravity (Calculated): ERROR"))
                End Try
                _report.AppendLine()
            End If

            If _assay.NBPAVG > 0 Then
                Dim nbpcalc = _ms.Phases(0).Compounds.Values.Select(Function(x) x.MoleFraction.GetValueOrDefault * x.ConstantProperties.NBP).Sum.GetValueOrDefault
                Dim nbperr = (_assay.NBPAVG - nbpcalc) / _assay.NBPAVG
                _report.AppendLine(String.Format("Normal Boiling Point (Specified): {0:N2} {1}", cv.ConvertFromSI(su.temperature, _assay.NBPAVG), su.temperature))
                _report.AppendLine(String.Format("Normal Boiling Point (Calculated): {0:N2} {1}", cv.ConvertFromSI(su.temperature, nbpcalc), su.temperature))
                _report.AppendLine(String.Format("Normal Boiling Point Error: {0:P}", nbperr))
                _report.AppendLine()
            End If

            If _assay.V1 > 0 Then
                _ms.PropertyPackage = pp
                _ms.ClearCalculatedProps()
                pp.CurrentMaterialStream = _ms
                _ms.Phases(0).Properties.temperature = _assay.T1
                _ms.Phases(0).Properties.pressure = 101325
                _ms.SpecType = Enums.StreamSpec.Temperature_and_Pressure
                Try
                    _ms.Calculate()
                    Dim v1calc = _ms.Phases(3).Properties.kinematic_viscosity.GetValueOrDefault
                    Dim v1err = (_assay.V1 - v1calc) / _assay.V1
                    _report.AppendLine(String.Format("Kinematic Viscosity (1) (Specified): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, _assay.V1), su.cinematic_viscosity))
                    _report.AppendLine(String.Format("Kinematic Viscosity (1) (Calculated): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v1calc), su.cinematic_viscosity))
                    _report.AppendLine(String.Format("Kinematic Viscosity (1) Error: {0:P}", v1err))
                Catch ex As Exception
                    _report.AppendLine(String.Format("Kinematic Viscosity (1) (Specified): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, _assay.V1), su.cinematic_viscosity))
                    _report.AppendLine(String.Format("Kinematic Viscosity (1) (Calculated): ERROR"))
                End Try
                _report.AppendLine()
            End If

            If _assay.V2 > 0 Then
                _ms.PropertyPackage = pp
                pp.CurrentMaterialStream = _ms
                _ms.Phases(0).Properties.temperature = _assay.T2
                _ms.Phases(0).Properties.pressure = 101325
                _ms.SpecType = Enums.StreamSpec.Temperature_and_Pressure
                Try
                    _ms.Calculate()
                    Dim v2calc = _ms.Phases(3).Properties.kinematic_viscosity.GetValueOrDefault
                    Dim v2err = (_assay.V2 - v2calc) / _assay.V2
                    _report.AppendLine(String.Format("Kinematic Viscosity (2) (Specified): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, _assay.V2), su.cinematic_viscosity))
                    _report.AppendLine(String.Format("Kinematic Viscosity (2) (Calculated): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v2calc), su.cinematic_viscosity))
                    _report.AppendLine(String.Format("Kinematic Viscosity (2) Error: {0:P}", v2err))
                Catch ex As Exception
                    _report.AppendLine(String.Format("Kinematic Viscosity (2) (Specified): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, _assay.V2), su.cinematic_viscosity))
                    _report.AppendLine(String.Format("Kinematic Viscosity (2) (Calculated): ERROR"))
                End Try
                _report.AppendLine()
            End If

        Else

            'distillation curves characterization

            'check mw
            If _assay.MW > 0 Then
                _ms.PropertyPackage = pp
                _ms.ClearCalculatedProps()
                pp.CurrentMaterialStream = _ms
                Dim mwcalc = _ms.PropertyPackage.AUX_MMM(PropertyPackages.Phase.Mixture)
                Dim mwerr = (_assay.MW - mwcalc) / _assay.MW
                _report.AppendLine(String.Format("Molecular Weight (Specified): {0:N2}", _assay.MW))
                _report.AppendLine(String.Format("Molecular Weight (Calculated): {0:N2}", mwcalc))
                _report.AppendLine(String.Format("Molecular Weight Error: {0:P}", mwerr))
                _report.AppendLine()
            End If

            If _assay.API > 0 Then
                _ms.PropertyPackage = pp
                _ms.ClearCalculatedProps()
                pp.CurrentMaterialStream = _ms
                _ms.Phases(0).Properties.temperature = 15.56 + 273.15
                _ms.Phases(0).Properties.pressure = 101325
                _ms.SpecType = Enums.StreamSpec.Temperature_and_Pressure
                Try
                    _ms.Calculate()
                    Dim apicalc = _ms.Phases(3).Properties.density.GetValueOrDefault / 1000
                    apicalc = 141.5 / apicalc - 131.5
                    Dim apierr = (_assay.API - apicalc) / _assay.API
                    _report.AppendLine(String.Format("API (Specified): {0:N4}", _assay.API))
                    _report.AppendLine(String.Format("API (Calculated): {0:N4}", apicalc))
                    _report.AppendLine(String.Format("API Error: {0:P}", apierr))
                Catch ex As Exception
                    _report.AppendLine(String.Format("API (Specified): {0:N4}", _assay.API))
                    _report.AppendLine(String.Format("API (Calculated): ERROR"))
                End Try
                _report.AppendLine()
            End If

            Dim nbpcalc = _ms.Phases(0).Compounds.Values.Select(Function(x) x.MoleFraction.GetValueOrDefault * x.ConstantProperties.NBP).Sum.GetValueOrDefault
            _report.AppendLine(String.Format("Normal Boiling Point (Calculated Average): {0:N2} {1}", nbpcalc, su.temperature))
            _report.AppendLine()

            If _assay.HasViscCurves Then
                _ms.PropertyPackage = pp
                _ms.ClearCalculatedProps()
                pp.CurrentMaterialStream = _ms
                _ms.Phases(0).Properties.temperature = _assay.T1
                _ms.Phases(0).Properties.pressure = 101325
                _ms.SpecType = Enums.StreamSpec.Temperature_and_Pressure
                Try
                    _ms.Calculate()
                    Dim v1calc = _ms.Phases(3).Properties.kinematic_viscosity.GetValueOrDefault
                    _report.AppendLine(String.Format("Kinematic Viscosity (1) (Calculated): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v1calc), su.cinematic_viscosity))
                Catch ex As Exception
                    _report.AppendLine(String.Format("Kinematic Viscosity (1) (Calculated): ERROR"))
                End Try
                Dim v1min = _assay.PY_V1.ToDoubleList().Min
                Dim v1max = _assay.PY_V1.ToDoubleList().Max
                _report.AppendLine(String.Format("Kinematic Viscosity (1) (Minimum): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v1min), su.cinematic_viscosity))
                _report.AppendLine(String.Format("Kinematic Viscosity (1) (Maximum): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v1max), su.cinematic_viscosity))
                _report.AppendLine()

                _ms.PropertyPackage = pp
                _ms.ClearCalculatedProps()

                pp.CurrentMaterialStream = _ms

                _ms.Phases(0).Properties.temperature = _assay.T2
                _ms.Phases(0).Properties.pressure = 101325
                _ms.SpecType = Enums.StreamSpec.Temperature_and_Pressure
                Try
                    _ms.Calculate()
                    Dim v2calc = _ms.Phases(3).Properties.kinematic_viscosity.GetValueOrDefault
                    _report.AppendLine(String.Format("Kinematic Viscosity (2) (Calculated): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v2calc), su.cinematic_viscosity))
                Catch ex As Exception
                    _report.AppendLine(String.Format("Kinematic Viscosity (2) (Calculated): ERROR"))
                End Try
                Dim v2min = _assay.PY_V2.ToDoubleList().Min
                Dim v2max = _assay.PY_V2.ToDoubleList().Max
                _report.AppendLine(String.Format("Kinematic Viscosity (2) (Minimum): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v2min), su.cinematic_viscosity))
                _report.AppendLine(String.Format("Kinematic Viscosity (2) (Maximum): {0:G4} {1}", cv.ConvertFromSI(su.cinematic_viscosity, v2max), su.cinematic_viscosity))
                _report.AppendLine()
            End If

        End If

        Return _report.ToString

    End Function

    Function DisplayForm(displaycompoundviewer As Action(Of ConstantProperties), positiveanswer As Action) As DialogResult

        Dim co1 = UI.Shared.Common.GetDefaultContainer()
        co1.Tag = "Quality Check Report"
        Dim co2 = UI.Shared.Common.GetDefaultContainer()
        co2.Tag = "Pseudocompound Properties"

        Dim myform = UI.Shared.Common.GetDefaultTabbedForm("Petroleum Characterization Quality Check", 750, 575, {co1, co2})

        co1.CreateAndAddLabelRow("Quality Check Report")
        co1.CreateAndAddMultilineMonoSpaceTextBoxRow(_report.ToString, 400, True, Nothing)
        co1.CreateAndAddLabelRow2("Analyze the report and the properties of the generated pseudocompounds. Click 'Yes' if you want to proceed adding the compounds to the simulation. If you're not satisfied with the generated properties, click 'No', select a different set of property methods and parameters and try again.")
        co1.CreateAndAddLabelAndTwoButtonsRow("", "No", Nothing, "Yes", Nothing,
                                              Sub()
                                                  _dlgresult = DialogResult.No
                                                  myform.Close()
                                              End Sub,
                                              Sub()
                                                  _dlgresult = DialogResult.Yes
                                                  myform.Close()
                                              End Sub)


        co2.CreateAndAddLabelRow("Pseudocompounds")

        For Each c In _compounds
            co2.CreateAndAddLabelAndButtonRow(c.Name, "View Properties",
                                              Nothing,
                                              Sub()
                                                  displaycompoundviewer(c)
                                              End Sub)
        Next

        myform.Topmost = True
        myform.Show()

        AddHandler myform.Closed, Sub() If _dlgresult = DialogResult.Yes Then positiveanswer.Invoke

    End Function

End Class
