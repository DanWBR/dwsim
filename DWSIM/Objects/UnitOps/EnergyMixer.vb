'    Energy Mixer Calculation Routines 
'    Copyright 2008 Daniel Wagner O. de Medeiros
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
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.SimulationObjects.UnitOperations

    <System.Serializable()> Public Class EnergyMixer

        Inherits DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description




        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.NodeEn
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            End If

            Me.PropertyPackage.CurrentMaterialStream = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)

            Dim H, Hs, T, W, P, DQ As Double
            H = 0
            Hs = 0
            T = 0
            W = 0
            P = 0
            DQ = 0
            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint
            For Each cp In Me.GraphicObject.InputConnectors
                If cp.IsAttached Then
                    If cp.AttachedConnector.AttachedFrom.Calculated = False Then Throw New Exception(DWSIM.App.GetLocalString("Umaoumaiscorrentesna"))
                    If cp.AttachedConnector.AttachedFrom.ObjectType = ObjectType.EnergyStream Then

                        DQ = form.Collections.FlowsheetObjectCollection(cp.AttachedConnector.AttachedFrom.Name).EnergyFlow.GetValueOrDefault

                    Else

                        ms = form.Collections.FlowsheetObjectCollection(cp.AttachedConnector.AttachedFrom.Name)
                        If ms.Phases(0).Properties.pressure.GetValueOrDefault < P Then
                            P = ms.Phases(0).Properties.pressure
                        ElseIf P = 0 Then
                            P = ms.Phases(0).Properties.pressure
                        End If
                        T = ms.Phases(0).Properties.temperature
                        W += ms.Phases(0).Properties.massflow.GetValueOrDefault
                        H = W * ms.Phases(0).Properties.enthalpy.GetValueOrDefault

                    End If

                End If
            Next
            Hs = (H + DQ) / W

            Dim n As Integer = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name).Phases(0).Compounds.Count
            Dim Vw(n - 1) As Double
            For Each cp In Me.GraphicObject.InputConnectors
                If cp.IsAttached And Not cp.AttachedConnector.AttachedFrom.ObjectType = ObjectType.EnergyStream Then
                    ms = form.Collections.FlowsheetObjectCollection(cp.AttachedConnector.AttachedFrom.Name)
                    Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In ms.Phases(0).Compounds.Values
                        Vw(i) += comp.MassFraction.GetValueOrDefault * ms.Phases(0).Properties.massflow.GetValueOrDefault
                        i += 1
                    Next
                End If
            Next

            With form.Collections.FlowsheetObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                Dim i As Integer = 0
                Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
                For Each comp In .Phases(0).Compounds.Values
                    comp.MassFraction = Vw(i) / W
                    i += 1
                Next
                Dim mass_div_mm As Double = 0
                Dim sub1 As DWSIM.Thermodynamics.BaseClasses.Compound
                For Each sub1 In .Phases(0).Compounds.Values
                    mass_div_mm += sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight
                    i += 1
                Next
                i = 0
                For Each sub1 In .Phases(0).Compounds.Values
                    sub1.MoleFraction = sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight / mass_div_mm
                    i += 1
                Next
                .Phases(0).Properties.temperature = T
                .Phases(0).Properties.enthalpy = Hs
                .Phases(0).Properties.pressure = P
                .Phases(0).Properties.massflow = W
                .Phases(0).Properties.molarfraction = 1
                .Phases(0).Properties.massfraction = 1
                Me.PropertyPackage.CurrentMaterialStream = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                Dim tmp As Object = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P, Hs, T)
                T = tmp(2)
                .Phases(0).Properties.temperature = T
            End With

            'Call function to calculate flowsheet
            With objargs
                .Calculated = True
                .Name = Me.Name
                .Tag = Me.GraphicObject.Tag
                .ObjectType = ObjectType.NodeEn
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then


                'Zerar valores da corrente de matéria conectada a jusante
                With form.Collections.FlowsheetObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.molarflow = Nothing
                    .GraphicObject.Calculated = False
                End With

            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            With objargs
                .Calculated = False
                .Name = Me.Name
                .ObjectType = ObjectType.NodeEn
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            Return 0

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            Return 0

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            Return 0

        End Function
    End Class

End Namespace
