'    Solids Separator Calculation Routines 
'    Copyright 2013 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.SimulationObjects.Streams
Imports DWSIM.DWSIM.SimulationObjects.UnitOperations.Auxiliary
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver

Namespace UnitOperations

    <System.Serializable()> Public Class SolidsSeparator

        Inherits SharedClasses.UnitOperations.BaseClass

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean
            Return MyBase.LoadData(data)
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        'Public Property EnergyImb() As Double

        Public Property SeparationEfficiency() As Double = 100.0#
        Public Property LiquidSeparationEfficiency() As Double = 100.0#

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As IFLowsheet = Me.FlowSheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.SolidSeparator
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.SolidSeparator
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.SolidSeparator
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim instr, outstr1, outstr2 As Streams.MaterialStream
            instr = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            outstr1 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
            outstr2 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)

            Dim W As Double = instr.Phases(0).Properties.massflow.GetValueOrDefault
            Dim Wsin As Double = instr.Phases(7).Properties.massflow.GetValueOrDefault
            Dim Wlin As Double = instr.Phases(1).Properties.massflow.GetValueOrDefault
            Dim Wvin As Double = instr.Phases(2).Properties.massflow.GetValueOrDefault
            Dim sse, lse As Double
            sse = Me.SeparationEfficiency / 100
            lse = Me.LiquidSeparationEfficiency / 100
            Dim Wsout As Double = sse * Wsin + (1 - lse) * Wlin
            Dim Wlvout As Double = (1 - sse) * Wsin + lse * Wlin + Wvin

            Dim mw As Double

            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                outstr1 = Me.FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With outstr1
                    .ClearAllProps()
                    .Phases(0).Properties.massflow = Wlvout
                    Dim comp As Interfaces.ICompound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MassFlow = (1 - sse) * instr.Phases(7).Compounds(comp.Name).MassFlow + instr.Phases(2).Compounds(comp.Name).MassFlow
                        comp.MassFlow += lse * (instr.Phases(3).Compounds(comp.Name).MassFlow + instr.Phases(4).Compounds(comp.Name).MassFlow)
                        comp.MassFraction = comp.MassFlow / Wlvout
                    Next
                    mw = 0.0#
                    For Each comp In .Phases(0).Compounds.Values
                        mw += comp.MassFraction / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = comp.MassFraction / comp.ConstantProperties.Molar_Weight / mw
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight / 1000
                    Next
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                outstr2 = Me.FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With outstr2
                    .ClearAllProps()
                    .Phases(0).Properties.massflow = Wsout
                    Dim comp As Interfaces.ICompound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MassFlow = sse * instr.Phases(7).Compounds(comp.Name).MassFlow.GetValueOrDefault + (1 - lse) * (instr.Phases(3).Compounds(comp.Name).MassFlow + instr.Phases(4).Compounds(comp.Name).MassFlow)
                        comp.MassFraction = If(Wsout > 0.0#, comp.MassFlow / Wsout, 0.0#)
                    Next
                    mw = 0.0#
                    For Each comp In .Phases(0).Compounds.Values
                        mw += comp.MassFraction / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = If(mw > 0.0#, comp.MassFraction / comp.ConstantProperties.Molar_Weight / mw, 0.0#)
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight / 1000
                    Next
                End With
            End If

            'pass conditions

            outstr1.Phases(0).Properties.temperature = InStr.Phases(0).Properties.temperature.GetValueOrDefault
            outstr1.Phases(0).Properties.pressure = InStr.Phases(0).Properties.pressure.GetValueOrDefault
            outstr2.Phases(0).Properties.temperature = InStr.Phases(0).Properties.temperature.GetValueOrDefault
            outstr2.Phases(0).Properties.pressure = InStr.Phases(0).Properties.pressure.GetValueOrDefault

            'call the flowsheet calculator

            With objargs
                .Calculated = True
                .Name = Me.Name
                .Tag = Me.GraphicObject.Tag
                .ObjectType = Me.GraphicObject.ObjectType
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.IFLowsheet = Me.FlowSheet

            Dim j As Integer = 0

            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = Me.FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As Interfaces.ICompound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = Me.FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As Interfaces.ICompound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            With objargs
                .Calculated = False
                .Name = Me.Name
                .ObjectType = ObjectType.SolidSeparator
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 1
                    value = Me.SeparationEfficiency
                Case 2
                    value = Me.LiquidSeparationEfficiency
            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RW
                    'For i = 0 To 0
                    '    proplist.Add("PROP_SS_" + CStr(i))
                    'Next
                Case PropertyType.WR
                    For i = 1 To 2
                        proplist.Add("PROP_SS_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 1 To 2
                        proplist.Add("PROP_SS_" + CStr(i))
                    Next
                Case PropertyType.RO
                    'For i = 0 To 0
                    '    proplist.Add("PROP_SS_" + CStr(i))
                    'Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 1
                    'PROP_SS_1	Solid Separation Efficiency
                    Me.SeparationEfficiency = propval
                Case 2
                    'PROP_SS_2	Liquid Separation Efficiency
                    Me.LiquidSeparationEfficiency = propval
            End Select

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            'If su Is Nothing Then su = New SystemsOfUnits.SI
            'Dim cv As New SystemsOfUnits.Converter
            Dim value As String = "%"
            'Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Return value
        End Function
    End Class

End Namespace
