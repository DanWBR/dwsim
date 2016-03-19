'    Flowsheet Object Base Classes 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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

Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Linq
Imports DWSIM.Interfaces2
Imports DWSIM.DWSIM
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports Microsoft.Scripting.Hosting
Imports CapeOpen
Imports System.Runtime.Serialization.Formatters
Imports System.Runtime.InteropServices.Marshal
Imports System.Runtime.InteropServices
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Text
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports PropertyGridEx
Imports DWSIM.DWSIM.DrawingTools
Imports DWSIM.DWSIM.DrawingTools.GraphicObjects2

Namespace DWSIM.SimulationObjects.UnitOperations

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class BaseClass

        Implements ICloneable, IDisposable, XMLSerializer.Interfaces.ICustomXMLSerialization

        Implements ICapeIdentification

        Implements Interfaces.ISimulationObject

        Public Const ClassId As String = ""

        Protected m_IsAdjustAttached As Boolean = False
        Protected m_AdjustId As String = ""
        Protected m_AdjustVarType As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.TipoVar = DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.TipoVar.Nenhum

        Protected m_IsSpecAttached As Boolean = False
        Protected m_SpecId As String = ""
        Protected m_SpecVarType As DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Nenhum

        <System.NonSerialized()> Protected Friend m_flowsheet As FormFlowsheet

        Public Property Calculated As Boolean = False

        Public Property DebugMode As Boolean = False
        Public Property DebugText As String = ""

        <Xml.Serialization.XmlIgnore> Public Property CreatedWithThreadID As Integer = 0
        <Xml.Serialization.XmlIgnore> Public Property LastUpdated As New Date

        Public MustOverride Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As DWSIM.SystemsOfUnits.Units)

        Public Sub New()

        End Sub

        Public Property ComponentDescription() As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        ''' <summary>
        ''' Gets the name of the component.
        ''' </summary>
        ''' <value></value>
        ''' <returns>CapeString</returns>
        ''' <remarks>Implements CapeOpen.ICapeIdentification.ComponentDescription</remarks>
        Public Property ComponentName() As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

        ''' <summary>
        ''' Calculates the object.
        ''' </summary>
        ''' <param name="args"></param>
        ''' <returns></returns>
        ''' <remarks>Use 'Solve()' to calculate the object instead.</remarks>
        Public Overridable Function Calculate(Optional ByVal args As Object = Nothing) As Integer
            Return Nothing
        End Function

        ''' <summary>
        ''' Energy Flow property. Only implemented for Energy Streams.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Overridable Property EnergyFlow() As Nullable(Of Double)
            Get
                Throw New NotImplementedException()
            End Get
            Set(ByVal value As Nullable(Of Double))
                Throw New NotImplementedException()
            End Set
        End Property

        ''' <summary>
        ''' Phase collection, only implemented for Material Streams.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public Overridable ReadOnly Property Phases() As Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.Phase)
            Get
                Throw New NotImplementedException
            End Get
        End Property

        ''' <summary>
        ''' Validates the object, checking its connections and other parameters.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Validate()

            Dim vForm As Global.DWSIM.FormFlowsheet = FlowSheet
            Dim vEventArgs As New DWSIM.Extras.StatusChangeEventArgs
            Dim vCon As ConnectionPoint

            With vEventArgs
                .Calculated = False
                .Name = Me.Name
                .ObjectType = Me.GraphicObject.ObjectType
            End With

            'Validate input connections.
            For Each vCon In Me.GraphicObject.InputConnectors
                If Not vCon.IsAttached Then
                    CalculateFlowsheet(FlowSheet, vEventArgs, Nothing)
                    Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
                End If
            Next

            'Validate output connections.
            For Each vCon In Me.GraphicObject.OutputConnectors
                If Not vCon.IsAttached Then
                    CalculateFlowsheet(vForm, vEventArgs, Nothing)
                    Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
                End If
            Next

        End Sub

        Public Overridable Function GetDebugReport() As String
            Return "Error - function not implemented"
        End Function

        Public Sub AppendDebugLine(text As String)
            DebugText += text & vbCrLf & vbCrLf
        End Sub

        ''' <summary>
        ''' Gets or sets the error message regarding the last calculation attempt.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property ErrorMessage() As String

        ''' <summary>
        ''' Checks if a value is valid.
        ''' </summary>
        ''' <param name="val">Value to be checked.</param>
        ''' <param name="onlypositive">Value should be a positive double or not.</param>
        ''' <param name="paramname">Name of the parameter (ex. P, T, W, H etc.)</param>
        ''' <remarks></remarks>
        Public Sub CheckSpec(val As Double, onlypositive As Boolean, paramname As String)

            If Not val.IsValid Then Throw New ArgumentException(DWSIM.App.GetLocalString("ErrorInvalidUOSpecValue") & " (name: " & paramname & ", value: " & val & ")")
            If onlypositive Then If val.IsNegative Then Throw New ArgumentException(DWSIM.App.GetLocalString("ErrorInvalidUOSpecValue") & " (name: " & paramname & ", value: " & val & ")")

        End Sub

        Public Enum PropertyType
            RO = 0
            RW = 1
            WR = 2
            ALL = 3
        End Enum

        ''' <summary>
        ''' Get a list of all properties of the object.
        ''' </summary>
        ''' <param name="proptype">Type of the property.</param>
        ''' <returns>A list of property identifiers.</returns>
        ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
        Public MustOverride Function GetProperties(ByVal proptype As PropertyType) As String()

        ''' <summary>
        ''' Gets the value of a property.
        ''' </summary>
        ''' <param name="prop">Property identifier.</param>
        ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
        ''' <returns>Property value.</returns>
        ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
        Public MustOverride Function GetPropertyValue(ByVal prop As String, Optional ByVal su As DWSIM.SystemsOfUnits.Units = Nothing)

        ''' <summary>
        ''' Gets the units of a property.
        ''' </summary>
        ''' <param name="prop">Property identifier.</param>
        ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
        ''' <returns>Property units.</returns>
        ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
        Public MustOverride Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As DWSIM.SystemsOfUnits.Units = Nothing)

        ''' <summary>
        ''' Sets the value of a property.
        ''' </summary>
        ''' <param name="prop">Property identifier.</param>
        ''' <param name="propval">Property value to set at the specified units.</param>
        ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
        ''' <returns></returns>
        ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
        Public MustOverride Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SystemsOfUnits.Units = Nothing)

        Public Sub HandlePropertyChange(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

            'handle connection updates

            PropertyValueChanged(s, e)

            'handle other property changes

            Dim sobj As GraphicObject = Me.GraphicObject

            FlowSheet.FormSurface.FlowsheetDesignSurface.SelectedObject = sobj

            If Not sobj Is Nothing Then

                Dim value As Double, units As String
                value = e.ChangedItem.GetValue
                units = e.ChangedItem.GetUnits

                If sobj.ObjectType = ObjectType.FlowsheetUO Then

                    Dim fs As DWSIM.SimulationObjects.UnitOperations.Flowsheet = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.PropertyDescriptor.Category.Equals(DWSIM.App.GetLocalString("LinkedInputParms")) Then

                        Dim pkey As String = CType(e.ChangedItem.PropertyDescriptor, CustomProperty.CustomPropertyDescriptor).CustomProperty.Tag

                        fs.Fsheet.Collections.FlowsheetObjectCollection(fs.InputParams(pkey).ObjectID).SetPropertyValue(fs.InputParams(pkey).ObjectProperty, e.ChangedItem.Value, FlowSheet.Options.SelectedUnitSystem)

                        If FlowSheet.Options.CalculatorActivated Then

                            sobj.Calculated = True
                            FlowSheet.FormProps.HandleObjectStatusChanged(sobj)

                            'Call function to calculate flowsheet
                            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                            With objargs
                                .Calculated = True
                                .Name = sobj.Name
                                .Tag = sobj.Tag
                                .ObjectType = ObjectType.FlowsheetUO
                                .Sender = "PropertyGrid"
                            End With

                            If fs.IsSpecAttached = True And fs.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(fs.AttachedSpecId).Calculate()
                            FlowSheet.CalculationQueue.Enqueue(objargs)

                        End If

                    End If

                ElseIf sobj.ObjectType = ObjectType.MaterialStream Then

                    Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If Not e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Base")) Then

                        Dim T, P, W, Q, QV, HM, SM, VF As Double

                        If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Temperatura")) Then
                            If units <> "" Then
                                T = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                            Else
                                T = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                            End If
                            ms.Phases(0).Properties.temperature = T
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                            If units <> "" Then
                                P = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                            Else
                                P = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                            End If
                            ms.Phases(0).Properties.pressure = P
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vazomssica")) Then
                            If units <> "" Then
                                W = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                            Else
                                W = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.massflow, e.ChangedItem.Value)
                            End If
                            ms.Phases(0).Properties.massflow = W
                            ms.Phases(0).Properties.molarflow = Nothing
                            ms.Phases(0).Properties.volumetric_flow = Nothing
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vazomolar")) Then
                            If units <> "" Then
                                Q = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                            Else
                                Q = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                            End If
                            ms.Phases(0).Properties.molarflow = Q
                            ms.Phases(0).Properties.massflow = Nothing
                            ms.Phases(0).Properties.volumetric_flow = Nothing
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vazovolumtrica")) Then
                            If units <> "" Then
                                QV = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                            Else
                                QV = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.volumetricFlow, e.ChangedItem.Value)
                            End If
                            ms.Phases(0).Properties.volumetric_flow = QV
                            ms.Phases(0).Properties.massflow = Nothing
                            ms.Phases(0).Properties.molarflow = Nothing
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("EntalpiaEspecfica")) Then
                            If units <> "" Then
                                HM = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                            Else
                                HM = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.enthalpy, e.ChangedItem.Value)
                            End If
                            ms.Phases(0).Properties.enthalpy = HM
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("EntropiaEspecfica")) Then
                            If units <> "" Then
                                SM = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                            Else
                                SM = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.entropy, e.ChangedItem.Value)
                            End If
                            ms.Phases(0).Properties.entropy = SM
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vapor")) Then
                            VF = e.ChangedItem.Value
                            ms.Phases(2).Properties.molarfraction = VF
                        End If

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Name = sobj.Name
                            .Tag = sobj.Tag
                            .ObjectType = ObjectType.MaterialStream
                            .Sender = "PropertyGrid"
                        End With

                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.EnergyStream Then

                    Dim es As DWSIM.SimulationObjects.Streams.EnergyStream = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("EnergyFlow")) Then

                        If units <> "" Then
                            es.EnergyFlow = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            es.EnergyFlow = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        sobj.Calculated = True
                        FlowSheet.FormProps.HandleObjectStatusChanged(sobj)

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = True
                            .Name = sobj.Name
                            .Tag = sobj.Tag
                            .ObjectType = ObjectType.EnergyStream
                            .Sender = "PropertyGrid"
                        End With

                        If es.IsSpecAttached = True And es.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(es.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If


                ElseIf sobj.ObjectType = ObjectType.NodeOut Then

                    Dim sp As DWSIM.SimulationObjects.UnitOperations.Splitter = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)
                    sp.OutCount = 0
                    For Each cp In sp.GraphicObject.OutputConnectors
                        If cp.IsAttached Then sp.OutCount += 1
                    Next
                    If e.ChangedItem.Label.Contains("[Split Ratio] ") Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Or Convert.ToDouble(e.ChangedItem.Value) > 1.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                        Dim i, j As Integer
                        Dim total As Double

                        Dim cp As ConnectionPoint
                        For Each cp In sp.GraphicObject.OutputConnectors
                            If cp.IsAttached Then
                                If e.ChangedItem.Label.Contains(cp.AttachedConnector.AttachedTo.Tag) Then j = i
                            End If
                            i += 1
                        Next
                        For i = 0 To sp.OutCount - 2
                            If i <> j Then
                                total += sp.Ratios(i)
                            Else
                                total += e.ChangedItem.Value
                            End If
                        Next
                        If total <= 1 Then
                            sp.Ratios(j) = e.ChangedItem.Value
                            sp.Ratios(sp.OutCount - 1) = 1 - total
                        Else
                            Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetPropertyName("PROP_SP_1")) Then

                        Select Case sp.OperationMode
                            Case SimulationObjects.UnitOperations.Splitter.OpMode.StreamMassFlowSpec
                                If units <> "" Then
                                    sp.StreamFlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                                Else
                                    sp.StreamFlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.massflow, e.ChangedItem.Value)
                                End If
                            Case SimulationObjects.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
                                If units <> "" Then
                                    sp.StreamFlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                                Else
                                    sp.StreamFlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                                End If
                        End Select

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetPropertyName("PROP_SP_2")) Then

                        Select Case sp.OperationMode
                            Case SimulationObjects.UnitOperations.Splitter.OpMode.StreamMassFlowSpec
                                If units <> "" Then
                                    sp.Stream2FlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                                Else
                                    sp.Stream2FlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                                End If
                            Case SimulationObjects.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
                                If units <> "" Then
                                    sp.Stream2FlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                                Else
                                    sp.Stream2FlowSpec = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                                End If
                        End Select

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        sobj.Calculated = True

                        FlowSheet.FormProps.HandleObjectStatusChanged(sobj)

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Name = sobj.Name
                            .Tag = sobj.Tag
                            .ObjectType = ObjectType.NodeOut
                            .Sender = "PropertyGrid"
                        End With

                        If sp.IsSpecAttached = True And sp.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(sp.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Pump Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Pump = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains("Delta P") Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Pressoajusante")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.Pout = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.Pout = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Pump
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Valve Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Valve = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                        'If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ValveOutletPressure")) Then

                        If units <> "" Then
                            bb.OutletPressure = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletPressure = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Valve
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Filter Then

                    Dim ft As DWSIM.SimulationObjects.UnitOperations.Filter = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterMediumResistance")) Then
                        If units <> "" Then
                            ft.FilterMediumResistance = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            ft.FilterMediumResistance = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.mediumresistance, e.ChangedItem.Value)
                        End If
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterSpecificCakeResistance")) Then
                        If units <> "" Then
                            ft.SpecificCakeResistance = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            ft.SpecificCakeResistance = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.cakeresistance, e.ChangedItem.Value)
                        End If
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterCycleTime")) Then
                        If units <> "" Then
                            ft.FilterCycleTime = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            ft.FilterCycleTime = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.time, e.ChangedItem.Value)
                        End If
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterPressureDrop")) Then
                        If units <> "" Then
                            ft.PressureDrop = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            ft.PressureDrop = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterArea")) Then
                        If units <> "" Then
                            ft.TotalFilterArea = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            ft.TotalFilterArea = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.area, e.ChangedItem.Value)
                        End If
                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = sobj.ObjectType
                            .Sender = "PropertyGrid"
                        End With

                        If ft.IsSpecAttached = True And ft.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(ft.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Compressor Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Compressor = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains("Delta P") Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.POut = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.POut = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then
                        If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Compressor
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Expander Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Expander = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains("Delta P") Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.POut = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.POut = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Expander
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Pipe Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Pipe = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ValveOutletPressure")) Then

                        If units <> "" Then
                            bb.OutletPressure = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletPressure = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Pipe
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Heater Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Heater = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Calor")) Then

                        If units <> "" Then
                            bb.DeltaQ = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaQ = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FraomolardaPhasePhaseV")) Then

                        bb.OutletVaporFraction = Double.Parse(e.ChangedItem.Value)

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Heater
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Cooler Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Cooler = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Calor")) Then

                        If units <> "" Then
                            bb.DeltaQ = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaQ = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FraomolardaPhasePhaseV")) Then

                        bb.OutletVaporFraction = Double.Parse(e.ChangedItem.Value)

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Cooler
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.Tank Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.Tank = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("AquecimentoResfriame")) Then

                        If units <> "" Then
                            bb.DeltaQ = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaQ = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("TKVol")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.Volume = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.Volume = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.volume, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Tag = sobj.Tag
                            .Calculated = False
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Tank
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.OT_Adjust Then

                    Dim adj As DWSIM.SimulationObjects.SpecialOps.Adjust = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    With adj
                        If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelControlada")) Then
                            .ControlledObject = FlowSheet.Collections.FlowsheetObjectCollection(.ControlledObjectData.m_ID)
                            .ControlledVariable = .ControlledObjectData.m_Property
                            CType(FlowSheet.Collections.GraphicObjectCollection(adj.Name), AdjustGraphic).ConnectedToCv = .ControlledObject.GraphicObject
                            .ReferenceObject = Nothing
                            .ReferenceVariable = Nothing
                            With .ReferencedObjectData
                                .m_ID = ""
                                .m_Name = ""
                                .m_Property = ""
                                .m_Type = ""
                            End With
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelManipulada")) Then
                            .ManipulatedObject = FlowSheet.Collections.FlowsheetObjectCollection(.ManipulatedObjectData.m_ID)
                            Dim gr As AdjustGraphic = FlowSheet.Collections.GraphicObjectCollection(adj.Name)
                            gr.ConnectedToMv = .ManipulatedObject.GraphicObject
                            .ManipulatedVariable = .ManipulatedObjectData.m_Property
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ObjetoVariveldeRefer")) Then
                            .ReferenceObject = FlowSheet.Collections.FlowsheetObjectCollection(.ReferencedObjectData.m_ID)
                            .ReferenceVariable = .ReferencedObjectData.m_Property
                            Dim gr As AdjustGraphic = FlowSheet.Collections.GraphicObjectCollection(adj.Name)
                            gr.ConnectedToRv = .ReferenceObject.GraphicObject
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Valormnimoopcional")) Then
                            adj.MinVal = DWSIM.SystemsOfUnits.Converter.ConvertToSI(adj.ManipulatedObject.GetPropertyUnit(adj.ManipulatedObjectData.m_Property, FlowSheet.Options.SelectedUnitSystem), e.ChangedItem.Value)
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Valormximoopcional")) Then
                            adj.MaxVal = DWSIM.SystemsOfUnits.Converter.ConvertToSI(adj.ManipulatedObject.GetPropertyUnit(adj.ManipulatedObjectData.m_Property, FlowSheet.Options.SelectedUnitSystem), e.ChangedItem.Value)
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ValordeAjusteouOffse")) Then
                            adj.AdjustValue = DWSIM.SystemsOfUnits.Converter.ConvertToSI(adj.ControlledObject.GetPropertyUnit(adj.ControlledObjectData.m_Property, FlowSheet.Options.SelectedUnitSystem), e.ChangedItem.Value)
                        End If
                    End With

                ElseIf sobj.ObjectType = ObjectType.OT_Spec Then

                    Dim spec As DWSIM.SimulationObjects.SpecialOps.Spec = FlowSheet.Collections.FlowsheetObjectCollection(sobj.Name)

                    With spec
                        If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelDestino")) Then
                            .TargetObject = FlowSheet.Collections.FlowsheetObjectCollection(.TargetObjectData.m_ID)
                            .TargetVariable = .TargetObjectData.m_Property
                            CType(FlowSheet.Collections.GraphicObjectCollection(spec.Name), SpecGraphic).ConnectedToTv = .TargetObject.GraphicObject
                        ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelFonte")) Then
                            .SourceObject = FlowSheet.Collections.FlowsheetObjectCollection(.SourceObjectData.m_ID)
                            Dim gr As SpecGraphic = FlowSheet.Collections.GraphicObjectCollection(spec.Name)
                            gr.ConnectedToSv = .SourceObject.GraphicObject
                            .SourceVariable = .SourceObjectData.m_Property
                        End If
                    End With

                ElseIf sobj.ObjectType = ObjectType.Vessel Then

                    Dim vessel As DWSIM.SimulationObjects.UnitOperations.Vessel = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    Dim T, P As Double
                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Temperatura")) Then
                        If units <> "" Then
                            T = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            T = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If
                        vessel.FlashTemperature = T
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                        If units <> "" Then
                            P = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            P = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If
                        vessel.FlashPressure = P
                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Tag = sobj.Tag
                            .Calculated = False
                            .Name = sobj.Name
                            .ObjectType = ObjectType.Vessel
                            .Sender = "PropertyGrid"
                        End With

                        If vessel.IsSpecAttached = True And vessel.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(vessel.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.OT_Recycle Then

                    Dim rec As DWSIM.SimulationObjects.SpecialOps.Recycle = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    Dim T, P, W As Double
                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Temperatura")) Then
                        T = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaT, e.ChangedItem.Value)
                        rec.ConvergenceParameters.Temperatura = T
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                        P = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        rec.ConvergenceParameters.Pressao = P
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("mssica")) Then
                        W = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.massflow, e.ChangedItem.Value)
                        rec.ConvergenceParameters.VazaoMassica = W
                    End If

                ElseIf sobj.ObjectType = ObjectType.RCT_Conversion Then

                    Dim bb As DWSIM.SimulationObjects.Reactors.Reactor_Conversion = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.RCT_Conversion
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.RCT_Equilibrium Then

                    Dim bb As DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.RCT_Equilibrium
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.RCT_Gibbs Then

                    Dim bb As DWSIM.SimulationObjects.Reactors.Reactor_Gibbs = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.RCT_Gibbs
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.RCT_CSTR Then

                    Dim bb As DWSIM.SimulationObjects.Reactors.Reactor_CSTR = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("RSCTRIsothermalTemperature")) Then

                        If units <> "" Then
                            bb.IsothermalTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.IsothermalTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("CSTRCatalystAmount")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.CatalystAmount = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.CatalystAmount = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.mass, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("RCSTRPGridItem1")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.Volume = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.Volume = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.volume, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.RCT_CSTR
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.RCT_PFR Then

                    Dim bb As DWSIM.SimulationObjects.Reactors.Reactor_PFR = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.DeltaP = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                        If units <> "" Then
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("RCSTRPGridItem1")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.Volume = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.Volume = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.volume, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("PFRLength")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.Length = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.Length = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.distance, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("PFRCatalystLoading")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.CatalystLoading = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.CatalystLoading = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.density, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("PFRCatalystParticleDiameter")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.CatalystParticleDiameter = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.CatalystParticleDiameter = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.diameter, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.RCT_PFR
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.HeatExchanger Then

                    Dim bb As DWSIM.SimulationObjects.UnitOperations.HeatExchanger = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient")) Then

                        If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                        If units <> "" Then
                            bb.OverallCoefficient = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.OverallCoefficient = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.heat_transf_coeff, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Area")) Then

                        If units <> "" Then
                            bb.Area = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.Area = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.area, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeatLoad")) Then

                        If units <> "" Then
                            bb.Q = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.Q = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXHotSidePressureDrop")) Then

                        If units <> "" Then
                            bb.HotSidePressureDrop = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.HotSidePressureDrop = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXColdSidePressureDrop")) Then

                        If units <> "" Then
                            bb.ColdSidePressureDrop = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.ColdSidePressureDrop = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXTempHotOut")) Then

                        If units <> "" Then
                            bb.HotSideOutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.HotSideOutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXTempColdOut")) Then

                        If units <> "" Then
                            bb.ColdSideOutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            bb.ColdSideOutletTemperature = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If

                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Tag = sobj.Tag
                            .Calculated = False
                            .Name = sobj.Name
                            .ObjectType = ObjectType.HeatExchanger
                            .Sender = "PropertyGrid"
                        End With

                        If bb.IsSpecAttached = True And bb.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(bb.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.ShortcutColumn Then

                    Dim sc As DWSIM.SimulationObjects.UnitOperations.ShortcutColumn = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)
                    Dim Pr, Pc As Double

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("SCCondenserType")) Then
                        sc.GraphicObject.Shape = sc.condtype
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("SCCondenserPressure")) Then
                        If units <> "" Then
                            Pc = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            Pc = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If
                        sc.m_condenserpressure = Pc
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("SCReboilerPressure")) Then
                        If units <> "" Then
                            Pr = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            Pr = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If
                        sc.m_boilerpressure = Pr
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCLightKey")) Then
                        sc.m_lightkey = e.ChangedItem.Value
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCHeavyKey")) Then
                        sc.m_heavykey = e.ChangedItem.Value
                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Tag = sobj.Tag
                            .Calculated = False
                            .Name = sobj.Name
                            .ObjectType = ObjectType.ShortcutColumn
                            .Sender = "PropertyGrid"
                        End With

                        If sc.IsSpecAttached = True And sc.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(sc.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.OrificePlate Then

                    Dim op As DWSIM.SimulationObjects.UnitOperations.OrificePlate = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OPOrificeDiameter")) Then
                        If units <> "" Then
                            op.OrificeDiameter = DWSIM.SystemsOfUnits.Converter.ConvertToSI(units, value)
                        Else
                            op.OrificeDiameter = DWSIM.SystemsOfUnits.Converter.ConvertToSI(FlowSheet.Options.SelectedUnitSystem.diameter, e.ChangedItem.Value)
                        End If
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OPBeta")) Then
                        op.Beta = e.ChangedItem.Value
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OPCorrectionFactor")) Then
                        op.CorrectionFactor = e.ChangedItem.Value
                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.OrificePlate
                            .Sender = "PropertyGrid"
                        End With

                        If op.IsSpecAttached = True And op.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(op.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If


                ElseIf sobj.ObjectType = ObjectType.ExcelUO Then

                    Dim eo As DWSIM.SimulationObjects.UnitOperations.ExcelUO = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)
                    Dim P1 As Integer
                    Dim L As String
                    P1 = InStr(1, e.ChangedItem.Label, "(") - 2
                    If P1 > 0 Then
                        L = Strings.Left(e.ChangedItem.Label, P1)
                        If eo.InputParams.ContainsKey(L) Then
                            eo.InputParams(L).Value = e.ChangedItem.Value
                        End If
                    End If

                    If FlowSheet.Options.CalculatorActivated Then

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = False
                            .Tag = sobj.Tag
                            .Name = sobj.Name
                            .ObjectType = ObjectType.ExcelUO
                            .Sender = "PropertyGrid"
                        End With

                        If eo.IsSpecAttached = True And eo.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(eo.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                ElseIf sobj.ObjectType = ObjectType.DistillationColumn Or sobj.ObjectType = ObjectType.AbsorptionColumn Or sobj.ObjectType = ObjectType.ReboiledAbsorber Or
                    sobj.ObjectType = ObjectType.RefluxedAbsorber Or sobj.ObjectType = ObjectType.CapeOpenUO Then


                    If FlowSheet.Options.CalculatorActivated Then

                        sobj.Calculated = True
                        FlowSheet.FormProps.HandleObjectStatusChanged(sobj)

                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                        With objargs
                            .Calculated = True
                            .Name = sobj.Name
                            .Tag = sobj.Tag
                            .ObjectType = sobj.ObjectType
                            .Sender = "PropertyGrid"
                        End With

                        Dim obj = FlowSheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                        If obj.IsSpecAttached = True And obj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.FlowsheetObjectCollection(obj.AttachedSpecId).Calculate()
                        FlowSheet.CalculationQueue.Enqueue(objargs)

                    End If

                End If

            End If

            Call FlowSheet.FormSurface.UpdateSelectedObject()
            Call FlowSheet.FormSurface.FlowsheetDesignSurface.Invalidate()

            CalculateAll2(FlowSheet, My.Settings.SolverMode, , True)

        End Sub

        Public Overridable Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

            Dim sobj As GraphicObject = FlowSheet.FormSurface.FlowsheetDesignSurface.SelectedObject

            If Not sobj Is Nothing Then

                'connections
                If sobj.ObjectType = ObjectType.Cooler Or sobj.ObjectType = ObjectType.Pipe Or sobj.ObjectType = ObjectType.Expander Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergia")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height + 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.EnergyConnector.IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.ExcelUO Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 45, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 15, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 15, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 45, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 45, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 15, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 15, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 45, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergia")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + 75, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(4).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.Compressor Or sobj.ObjectType = ObjectType.Heater Or sobj.ObjectType = ObjectType.Pump Or
                             sobj.ObjectType = ObjectType.RCT_PFR Or sobj.ObjectType = ObjectType.RCT_CSTR Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergia")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.Valve Or sobj.ObjectType = ObjectType.OrificePlate Or sobj.ObjectType = ObjectType.OT_Recycle Or sobj.ObjectType = ObjectType.Tank Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.Vessel Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 50, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 20, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 40, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 3)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 3)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 70, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(4).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 100, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(5).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadevapor")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 20, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadelquido")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 50, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadelquido") & " (2)") Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 100, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 2, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 2, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergia")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + 130, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(6).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.FlowsheetUO Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 60, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 0, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 3)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 3)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 60, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(4).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 90, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(5).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada7")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 120, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(6).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 6)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 6)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada8")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 150, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(7).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 7)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(7).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 7)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(7).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada9")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 180, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(8).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 8)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(8).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 8)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(8).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada10")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 210, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(9).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 9)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(9).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 9)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(9).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 60, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida5")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 60, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(4).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida6")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 90, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(5).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida7")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 120, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(6).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida8")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 150, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(7).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(7).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(7).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida9")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 180, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(8).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(8).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(8).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida10")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 210, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(9).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(9).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(9).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.RCT_Conversion Or sobj.ObjectType = ObjectType.RCT_Equilibrium Or sobj.ObjectType = ObjectType.RCT_Gibbs Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadevapor")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.17 * sobj.Height - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadelquido")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height * 0.843 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergia")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.NodeIn Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 75, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 45, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 15, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 15, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 3)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 3)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 45, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(4).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 75, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(5).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Conectadoasada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.NodeOut Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 2, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 2, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.ShortcutColumn Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCFeed")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 0.5 * sobj.Height - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCReboilerDuty")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCDistillate")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.3 * sobj.Height - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCBottoms")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.98 * sobj.Height - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCCondenserDuty")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + 0.175 * sobj.Height - 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.EnergyConnector.IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.HeatExchanger Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 50, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 + 30, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.OT_EnergyRecycle Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.ComponentSeparator Or sobj.ObjectType = ObjectType.SolidSeparator Or sobj.ObjectType = ObjectType.Filter Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height * 0.5 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("OutletStream1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.17 * sobj.Height - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("OutletStream2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height * 0.83 - 10, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergia")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.EnergyConnector.IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                ElseIf sobj.ObjectType = ObjectType.CustomUO Then
                    If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 65, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 35, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 1)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 5, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 2)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 25, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(4).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 4)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 55, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(5).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 5)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 85, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(6).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 6)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj, 0, 6)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergyFlowE")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + 115, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.InputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            Else
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                                FlowSheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), sobj)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 65, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(0).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 0, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 35, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(1).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 1, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 5, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(2).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 2, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 2, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida4")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 25, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(4).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 4, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 4, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida5")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 55, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(5).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 5, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 5, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida6")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 85, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(6).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 6, 0)
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), 6, 0)
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                            End If
                        End If
                    ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeEnergyFlowS")) Then
                        If e.ChangedItem.Value <> "" Then
                            If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface) Is Nothing Then
                                Dim oguid As String = FlowSheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + 115, e.ChangedItem.Value)
                            ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface), GraphicObject).InputConnectors(0).IsAttached Then
                                MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Exit Sub
                            End If
                            If Not sobj.OutputConnectors(3).IsAttached Then
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            Else
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                                FlowSheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, FlowSheet.FormSurface.FlowsheetDesignSurface))
                            End If
                        Else
                            If e.OldValue.ToString <> "" Then
                                FlowSheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                            End If
                        End If
                    End If
                End If
            End If

        End Sub

        ''' <summary>
        ''' Formats a property string, adding its units in parenthesis.
        ''' </summary>
        ''' <param name="prop">Property string</param>
        ''' <param name="unit">Property units</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function FT(ByRef prop As String, ByVal unit As String)
            Return prop & " (" & unit & ")"
        End Function

        ''' <summary>
        ''' Sets the Flowsheet to which this object belongs to.
        ''' </summary>
        ''' <param name="flowsheet">Flowsheet instance.</param>
        ''' <remarks></remarks>
        Public Sub SetFlowsheet(ByVal flowsheet As FormFlowsheet)
            m_flowsheet = flowsheet
        End Sub

        ''' <summary>
        ''' Gets the current flowsheet where this object is.
        ''' </summary>
        ''' <value></value>
        ''' <returns>Flowsheet instance.</returns>
        ''' <remarks></remarks>
        Public Overridable ReadOnly Property FlowSheet() As Global.DWSIM.FormFlowsheet
            Get
                If Not m_flowsheet Is Nothing Then
                    Return m_flowsheet
                Else
                    Dim frm As FormFlowsheet = My.Application.ActiveSimulation
                    If Not frm Is Nothing Then Return frm Else Return Nothing
                    If Not My.Application.CAPEOPENMode Then
                        If Not FormMain.ActiveMdiChild Is Nothing Then
                            If TypeOf FormMain.ActiveMdiChild Is FormFlowsheet Then
                                If frm Is Nothing Then frm = FormMain.ActiveMdiChild Else m_flowsheet = frm
                                If frm Is Nothing Then frm = m_flowsheet
                                If Not frm Is Nothing Then Return frm Else Return Nothing
                            Else
                                If FormMain.ActiveMdiChild IsNot Nothing Then
                                    If TypeOf FormMain.ActiveMdiChild Is FormFlowsheet Then Return FormMain.ActiveMdiChild Else Return Nothing
                                Else
                                    Return Nothing
                                End If
                            End If
                        Else
                            If frm Is Nothing Then frm = m_flowsheet
                            If Not frm Is Nothing Then Return frm Else Return Nothing
                        End If
                    Else
                        Return Nothing
                    End If

                End If

            End Get
        End Property

        Public Property Annotation() As DWSIM.Extras.Annotation

        ''' <summary>
        ''' Checks if an Adjust operation is attached to this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property IsAdjustAttached() As Boolean = False

        ''' <summary>
        ''' If an Adjust object is attached to this object, returns its ID.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AttachedAdjustId() As String = ""

        ''' <summary>
        ''' If an Adjust object is attached to this object, returns a variable describing how this object is used by it (manipulated, controlled or reference).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AdjustVarType() As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.TipoVar

        ''' <summary>
        ''' Checks if an Specification operation is attached to this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property IsSpecAttached() As Boolean = False

        ''' <summary>
        ''' If an Specification object is attached to this object, returns its ID.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AttachedSpecId() As String = ""

        ''' <summary>
        ''' If an Specification object is attached to this object, returns a variable describing how this object is used by it (target or source).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property SpecVarType() As DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar
     
        ''' <summary>
        ''' Gets or sets the graphic object representation of this object in the flowsheet.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore> Public Property GraphicObject() As GraphicObject

        ''' <summary>
        ''' Object's Unique ID (Name)
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks>This property is the same as the graphic object 'Name' property.</remarks>
        Public Property Name() As String = ""

        ''' <summary>
        ''' Gets or sets the flowsheet table object associated with this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property PropertyTable() As TableGraphic

        Sub CreateNew()
            CreatedWithThreadID = Threading.Thread.CurrentThread.ManagedThreadId
        End Sub

        ''' <summary>
        ''' Clones the current object, returning a new one with identical properties.
        ''' </summary>
        ''' <returns>An object of the same type with the same properties.</returns>
        ''' <remarks>Properties and fields marked with the 'NonSerializable' attribute aren't cloned.</remarks>
        Public Function Clone() As Object Implements System.ICloneable.Clone

            Return ObjectCopy(Me)

        End Function

        Function ObjectCopy(ByVal obj As DWSIM.SimulationObjects.UnitOperations.BaseClass) As DWSIM.SimulationObjects.UnitOperations.BaseClass

            Dim objMemStream As New MemoryStream(250000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()

        End Function

#Region "   IDisposable Support "

        Public disposedValue As Boolean = False        ' To detect redundant calls

        Protected Overridable Sub Dispose(ByVal disposing As Boolean)
            Me.disposedValue = True
        End Sub

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose

            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)

        End Sub
#End Region

        ''' <summary>
        ''' Loads object data stored in a collection of XML elements.
        ''' </summary>
        ''' <param name="data"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        ''' <summary>
        ''' Saves object data in a collection of XML elements.
        ''' </summary>
        ''' <returns>A List of XML elements containing object data.</returns>
        ''' <remarks></remarks>
        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            
            Return elements

        End Function

        ''' <summary>
        ''' Copies the object properties to the Clipboard.
        ''' </summary>
        ''' <param name="su">Units system to use.</param>
        ''' <param name="nf">Number format to use.</param>
        ''' <remarks></remarks>
        Public Sub CopyDataToClipboard(su As DWSIM.SystemsOfUnits.Units, nf As String)

            Dim DT As New DataTable
            DT.Columns.Clear()
            DT.Columns.Add(("Propriedade"), GetType(System.String))
            DT.Columns.Add(("Valor"), GetType(System.String))
            DT.Columns.Add(("Unidade"), GetType(System.String))
            DT.Rows.Clear()

            Dim baseobj As DWSIM.SimulationObjects.UnitOperations.BaseClass
            Dim properties() As String
            Dim description As String
            Dim objtype As ObjectType
            Dim propidx, r1, r2, r3, r4, r5, r6 As Integer
            r1 = 5
            r2 = 12
            r3 = 30
            r4 = 48
            r5 = 66
            r6 = 84

            baseobj = Me
            properties = baseobj.GetProperties(DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType.ALL)
            objtype = baseobj.GraphicObject.ObjectType
            description = DWSIM.App.GetLocalString(baseobj.GraphicObject.Description)
            If objtype = ObjectType.MaterialStream Then
                Dim value As String
                For propidx = 0 To r1 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                For propidx = r1 To r2 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {DWSIM.App.GetLocalString("FraomolarnaMistura"), "", ""})
                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In CType(Me, Streams.MaterialStream).Phases(0).Compounds.Values
                    DT.Rows.Add(New String() {DWSIM.App.GetComponentName(subst.Name), Format(subst.FracaoMolar.GetValueOrDefault, nf), ""})
                Next
                For propidx = r2 To r3 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {DWSIM.App.GetLocalString("FraomolarnaPhaseVapor"), "", ""})
                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In CType(Me, Streams.MaterialStream).Phases(2).Compounds.Values
                    DT.Rows.Add(New String() {DWSIM.App.GetComponentName(subst.Name), Format(subst.FracaoMolar.GetValueOrDefault, nf), ""})
                Next
                For propidx = r3 To r4 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In CType(Me, Streams.MaterialStream).Phases(1).Compounds.Values
                    DT.Rows.Add(New String() {DWSIM.App.GetComponentName(subst.Name), Format(subst.FracaoMolar.GetValueOrDefault, nf), ""})
                Next
                For propidx = r4 To r5 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In CType(Me, Streams.MaterialStream).Phases(3).Compounds.Values
                    DT.Rows.Add(New String() {DWSIM.App.GetComponentName(subst.Name), Format(subst.FracaoMolar.GetValueOrDefault, nf), ""})
                Next
                For propidx = r5 To r6 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In CType(Me, Streams.MaterialStream).Phases(4).Compounds.Values
                    DT.Rows.Add(New String() {DWSIM.App.GetComponentName(subst.Name), Format(subst.FracaoMolar.GetValueOrDefault, nf), ""})
                Next
                For propidx = r6 To 101
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {DWSIM.App.GetLocalString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In CType(Me, Streams.MaterialStream).Phases(6).Compounds.Values
                    DT.Rows.Add(New String() {DWSIM.App.GetComponentName(subst.Name), Format(subst.FracaoMolar.GetValueOrDefault, nf), ""})
                Next
            Else
                For Each prop As String In properties
                    DT.Rows.Add(New String() {DWSIM.App.GetPropertyName(prop), Format(baseobj.GetPropertyValue(prop, su), nf), baseobj.GetPropertyUnit(prop, su)})
                Next
            End If

            Dim st As New StringBuilder(DWSIM.App.GetLocalString(Me.ComponentDescription) & ": " & Me.GraphicObject.Tag & vbCrLf)
            For Each r As DataRow In DT.Rows
                Dim l As String = ""
                For Each o As Object In r.ItemArray
                    l += o.ToString() & vbTab
                Next
                st.AppendLine(l)
            Next

            Clipboard.SetText(st.ToString())

            DT.Clear()
            DT.Dispose()
            DT = Nothing

        End Sub

    End Class

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class UnitOpBaseClass

        Inherits DWSIM.SimulationObjects.UnitOperations.BaseClass

        'CAPE-OPEN Unit Operation Support
        Implements ICapeIdentification, ICapeUnit, ICapeUtilities, ICapeUnitReport

        'CAPE-OPEN Persistence Interface
        Implements IPersistStreamInit

        'CAPE-OPEN Error Interfaces
        Implements ECapeUser, ECapeUnknown, ECapeRoot

        Friend _pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage
        Friend _ppid As String = ""

        Friend _capeopenmode As Boolean = False

        Public Sub New()
            MyBase.CreateNew()
        End Sub

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Try
                Me._ppid = (From xel As XElement In data Select xel Where xel.Name = "PropertyPackage").SingleOrDefault.Value
            Catch

            End Try

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()

            Dim ppid As String = ""
            If _ppid <> "" Then
                ppid = _ppid
            ElseIf Not _pp Is Nothing Then
                ppid = _pp.Name
            Else
                ppid = ""
            End If
            elements.Add(New XElement("PropertyPackage", ppid))

            Return elements

        End Function

#Region "   DWSIM Specific"

        Public Overrides Function GetDebugReport() As String

            Me.DebugMode = True
            Me.DebugText = ""

            Try

                Calculate(Nothing)

                Me.DebugText += vbCrLf & vbCrLf & "Calculated OK."

            Catch ex As Exception

                Dim st As New StackTrace(ex, True)
                Dim frame As StackFrame = st.GetFrame(0)
                Dim fileName As String = Path.GetFileName(frame.GetFileName)
                Dim methodName As String = frame.GetMethod().Name
                Dim line As Integer = frame.GetFileLineNumber()

                AppendDebugLine(String.Format("ERROR: exception raised on file {0}, method {1}, line {2}:", fileName, methodName, line))
                AppendDebugLine(ex.Message.ToString)

            Finally

                Me.DebugMode = False

            End Try

            Return DebugText

        End Function

        ''' <summary>
        ''' Gets or sets the property package associated with this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Property PropertyPackage() As PropertyPackage
            Get
                If Not _pp Is Nothing Then Return _pp
                If _ppid Is Nothing Then _ppid = ""
                If FlowSheet.Options.PropertyPackages.ContainsKey(_ppid) Then
                    Return FlowSheet.Options.PropertyPackages(_ppid)
                Else
                    For Each pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage In Me.FlowSheet.Options.PropertyPackages.Values
                        _ppid = pp.UniqueID
                        Return pp
                        Exit For
                    Next
                End If
                Return Nothing
            End Get
            Set(ByVal value As PropertyPackage)
                If value IsNot Nothing Then
                    _ppid = value.UniqueID
                    _pp = value
                Else
                    _pp = Nothing
                End If
            End Set
        End Property

        ''' <summary>
        ''' Calculates the object.
        ''' </summary>
        ''' <param name="args"></param>
        ''' <remarks></remarks>
        Public Sub Solve(Optional ByVal args As Object = Nothing)

            Calculated = False

            Calculate(args)

            Calculated = True
            LastUpdated = Date.Now

        End Sub

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable Function DeCalculate() As Integer
            Return Nothing
        End Function

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub Unsolve()

            DeCalculate()

            Calculated = False

        End Sub

        ''' <summary>
        ''' Populates the Property Grid with properties from this object.
        ''' </summary>
        ''' <param name="pgrid"></param>
        ''' <param name="su"></param>
        ''' <remarks></remarks>
        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As DWSIM.SystemsOfUnits.Units)

            With pgrid

                .Item.Add(DWSIM.App.GetLocalString("UOPropertyPackage"), Me.PropertyPackage.Tag, False, DWSIM.App.GetLocalString("Outros"), "", True)
                With .Item(.Item.Count - 1)
                    .CustomEditor = New DWSIM.Editors.PropertyPackages.UIPPSelector
                End With
                If Not Me.GraphicObject Is Nothing Then
                    .Item.Add(DWSIM.App.GetLocalString("Ativo"), Me.GraphicObject, "Active", False, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                End If
                If Me.IsSpecAttached = True Then
                    .Item.Add(DWSIM.App.GetLocalString("ObjetoUtilizadopor"), FlowSheet.Collections.FlowsheetObjectCollection(Me.AttachedSpecId).GraphicObject.Tag, True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                    Select Case Me.SpecVarType
                        Case SpecialOps.Helpers.Spec.TipoVar.Destino
                            .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), DWSIM.App.GetLocalString(Me.SpecVarType.ToString), True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                        Case SpecialOps.Helpers.Spec.TipoVar.Fonte
                            .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), DWSIM.App.GetLocalString("SpecSource"), True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                    End Select
                End If
                If Not Me.Annotation Is Nothing Then
                    .Item.Add(DWSIM.App.GetLocalString("Anotaes"), Me, "Annotation", False, DWSIM.App.GetLocalString("Outros"), DWSIM.App.GetLocalString("Cliquenobotocomretic"), True)
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = False
                        .CustomEditor = New DWSIM.Editors.Annotation.UIAnnotationEditor
                    End With
                End If
                .Item.Add("ID", Me.Name, True, DWSIM.App.GetLocalString("Outros"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("LastUpdatedOn"), Me.LastUpdated.ToString("O"), True, DWSIM.App.GetLocalString("Outros"), "", True)

            End With

        End Sub

        Public Overrides Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

            MyBase.PropertyValueChanged(s, e)
            If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("UOPropertyPackage")) Then
                If e.ChangedItem.Value <> "" Then
                    If FlowSheet.Options.PropertyPackages.ContainsKey(e.ChangedItem.Value) Then
                        Me.PropertyPackage = FlowSheet.Options.PropertyPackages(e.ChangedItem.Value)
                    End If
                End If
            End If

        End Sub

        Sub CreateCOPorts()
            _ports = New CapeOpen.PortCollection()
            For Each c As ConnectionPoint In Me.GraphicObject.InputConnectors
                Select Case c.Type
                    Case ConType.ConIn
                        _ports.Add(New UnitPort("Inlet" + Me.GraphicObject.InputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedFrom.Tag))
                            End If
                        End With
                    Case ConType.ConEn
                        _ports.Add(New UnitPort("Inlet" + Me.GraphicObject.InputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedFrom.Tag))
                            End If
                        End With
                End Select
            Next
            For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                Select Case c.Type
                    Case ConType.ConOut
                        _ports.Add(New UnitPort("Outlet" + Me.GraphicObject.OutputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedTo.Tag))
                            End If
                        End With
                    Case ConType.ConEn
                        _ports.Add(New UnitPort("Outlet" + Me.GraphicObject.OutputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedTo.Tag))
                            End If
                        End With
                End Select
            Next
        End Sub

        Sub CreateCOParameters()
            _parameters = New CapeOpen.ParameterCollection
            Dim props() = Me.GetProperties(PropertyType.ALL)
            For Each s As String In props
                _parameters.Add(New CapeOpen.RealParameter(s, Me.GetPropertyValue(s), 0.0#, Me.GetPropertyUnit(s)))
                With _parameters.Item(_parameters.Count - 1)
                    .Mode = CapeParamMode.CAPE_OUTPUT
                End With
            Next
        End Sub

#End Region

#Region "   CAPE-OPEN Unit Operation internal support"

        Friend _ports As CapeOpen.PortCollection
        Friend _parameters As CapeOpen.ParameterCollection
        Friend _simcontext As Object = Nothing
        
        ''' <summary>
        ''' Calculates the Unit Operation.
        ''' </summary>
        ''' <remarks>The Flowsheet Unit performs its calculation, that is, computes the variables that are missing at
        ''' this stage in the complete description of the input and output streams and computes any public
        ''' parameter value that needs to be displayed. Calculate will be able to do progress monitoring
        ''' and checks for interrupts as required using the simulation context. At present, there are no
        ''' standards agreed for this.
        ''' It is recommended that Flowsheet Units perform a suitable flash calculation on all output
        ''' streams. In some cases a Simulation Executive will be able to perform a flash calculation but
        ''' the writer of a Flowsheet Unit is in the best position to decide the correct flash to use.
        ''' Before performing the calculation, this method should perform any final validation tests that
        ''' are required. For example, at this point the validity of Material Objects connected to ports can
        ''' be checked.
        ''' There are no input or output arguments for this method.</remarks>
        Public Overridable Sub Calculate1() Implements CapeOpen.ICapeUnit.Calculate
            'do CAPE calculation here
        End Sub

        ''' <summary>
        ''' Return an interface to a collection containing the list of unit ports (e.g. ICapeUnitCollection).
        ''' </summary>
        ''' <value></value>
        ''' <returns>A reference to the interface on the collection containing the specified ports</returns>
        ''' <remarks>Return the collection of unit ports (i.e. ICapeUnitCollection). These are delivered as a
        ''' collection of elements exposing the interfaces ICapeUnitPort</remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property ports() As Object Implements CapeOpen.ICapeUnit.ports
            Get
                If Not Me._capeopenmode Then
                    If Not Me.GraphicObject.ObjectType = ObjectType.CapeOpenUO Then
                        _ports = New CapeOpen.PortCollection
                        For Each c As ConnectionPoint In Me.GraphicObject.InputConnectors
                            If c.Type = ConType.ConIn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                            ElseIf c.Type = ConType.ConEn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                            End If
                            With _ports(_ports.Count - 1)
                                If c.IsAttached And Not c.AttachedConnector Is Nothing Then .Connect(Me.FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedFrom.Name))
                            End With
                        Next
                        For Each c As ConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConOut Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                            ElseIf c.Type = ConType.ConEn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
                            End If
                            With _ports(_ports.Count - 1)
                                If c.IsAttached And Not c.AttachedConnector Is Nothing Then .Connect(Me.FlowSheet.Collections.FlowsheetObjectCollection(c.AttachedConnector.AttachedTo.Name))
                            End With
                        Next
                    End If
                End If
                Return _ports
            End Get
        End Property

        ''' <summary>Validates the Unit Operation.</summary>
        ''' <param name="message">An optional message describing the cause of the validation failure.</param>
        ''' <returns>TRUE if the Unit is valid</returns>
        ''' <remarks>Sets the flag that indicates whether the Flowsheet Unit is valid by validating the ports and
        ''' parameters of the Flowsheet Unit. For example, this method could check that all mandatory
        ''' ports have connections and that the values of all parameters are within bounds.
        ''' Note that the Simulation Executive can call the Validate routine at any time, in particular it
        ''' may be called before the executive is ready to call the Calculate method. This means that
        ''' Material Objects connected to unit ports may not be correctly configured when Validate is
        ''' called. The recommended approach is for this method to validate parameters and ports but not
        ''' Material Object configuration. A second level of validation to check Material Objects can be
        ''' implemented as part of Calculate, when it is reasonable to expect that the Material Objects
        ''' connected to ports will be correctly configured.</remarks>
        Public Overridable Function Validate1(ByRef message As String) As Boolean Implements CapeOpen.ICapeUnit.Validate
            'do CAPE validation here
            message = "Validation OK"
            Return True
        End Function

        ''' <summary>
        ''' Get the flag that indicates whether the Flowsheet Unit is valid (e.g. some parameter values
        ''' have changed but they have not been validated by using Validate). It has three possible
        ''' values:
        ''' ·  notValidated(CAPE_NOT_VALIDATED): the unit’s validate() method has not been
        ''' called since the last operation that could have changed the validation status of the unit, for
        ''' example an update to a parameter value of a connection to a port.
        ''' ·  invalid(CAPE_INVALID): the last time the unit’s validate() method was called it returned
        ''' false.
        ''' ·  valid(CAPE_VALID): the last time the unit’s validate() method was called it returned true.
        ''' </summary>
        ''' <value>CAPE_VALID meaning the Validate method returned success; CAPE_INVALID meaing the Validate 
        ''' method returned failure; CAPE_NOT_VALIDATED meaning that the Validate method needs to be called 
        ''' to determine whether the unit is valid or not.</value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable ReadOnly Property ValStatus() As CapeOpen.CapeValidationStatus Implements CapeOpen.ICapeUnit.ValStatus
            Get
                Return CapeValidationStatus.CAPE_VALID
            End Get
        End Property

        ''' <summary>
        ''' The PMC displays its user interface and allows the Flowsheet User to interact with it. If no user interface is
        ''' available it returns an error.</summary>
        ''' <remarks></remarks>
        Public Overridable Sub Edit1() Implements CapeOpen.ICapeUtilities.Edit
            Throw New CapeNoImplException("ICapeUtilities.Edit() Method not implemented.")
        End Sub

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Initialize is now
        ''' available for any kind of PMC, ICapeUnit. Initialize is deprecated.
        ''' The PME will order the PMC to get initialized through this method. Any initialisation that could fail must be
        ''' placed here. Initialize is guaranteed to be the first method called by the client (except low level methods such
        ''' as class constructors or initialization persistence methods). Initialize has to be called once when the PMC is
        ''' instantiated in a particular flowsheet.
        ''' When the initialization fails, before signalling an error, the PMC must free all the resources that were
        ''' allocated before the failure occurred. When the PME receives this error, it may not use the PMC anymore.
        ''' The method terminate of the current interface must not either be called. Hence, the PME may only release
        ''' the PMC through the middleware native mechanisms.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Initialize() Implements CapeOpen.ICapeUtilities.Initialize
            'do CAPE Initialization here
            CreateCOPorts()
            CreateCOParameters()
        End Sub

        ''' <summary>
        ''' Returns an ICapeCollection interface.
        ''' </summary>
        ''' <value></value>
        ''' <returns>CapeInterface (ICapeCollection)</returns>
        ''' <remarks>This interface will contain a collection of ICapeParameter interfaces.
        ''' This method allows any client to access all the CO Parameters exposed by a PMC. Initially, this method was
        ''' only present in the ICapeUnit interface. Since ICapeUtilities.GetParameters is now available for any kind of
        ''' PMC, ICapeUnit.GetParameters is deprecated. Consult the “Open Interface Specification: Parameter
        ''' Common Interface” document for more information about parameter. Consult the “Open Interface
        ''' Specification: Collection Common Interface” document for more information about collection.
        ''' If the PMC does not support exposing its parameters, it should raise the ECapeNoImpl error, instead of
        ''' returning a NULL reference or an empty Collection. But if the PMC supports parameters but has for this call
        ''' no parameters, it should return a valid ICapeCollection reference exposing zero parameters.</remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property parameters() As Object Implements CapeOpen.ICapeUtilities.parameters
            Get
                Return _parameters
            End Get
        End Property

        ''' <summary>
        ''' Allows the PME to convey the PMC a reference to the former’s simulation context. 
        ''' </summary>
        ''' <value>The reference to the PME’s simulation context class. For the PMC to
        ''' use this class, this reference will have to be converted to each of the
        ''' defined CO Simulation Context interfaces.</value>
        ''' <remarks>The simulation context
        ''' will be PME objects which will expose a given set of CO interfaces. Each of these interfaces will allow the
        ''' PMC to call back the PME in order to benefit from its exposed services (such as creation of material
        ''' templates, diagnostics or measurement unit conversion). If the PMC does not support accessing the
        ''' simulation context, it is recommended to raise the ECapeNoImpl error.
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.SetSimulationContext
        ''' is now available for any kind of PMC, ICapeUnit. SetSimulationContext is deprecated.</remarks>
        <Xml.Serialization.XmlIgnore()> Public WriteOnly Property simulationContext() As Object Implements CapeOpen.ICapeUtilities.simulationContext
            Set(ByVal value As Object)
                _simcontext = value
            End Set
        End Property

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Terminate is now
        ''' available for any kind of PMC, ICapeUnit.Terminate is deprecated.
        ''' The PME will order the PMC to get destroyed through this method. Any uninitialization that could fail must
        ''' be placed here. ‘Terminate’ is guaranteed to be the last method called by the client (except low level methods
        ''' such as class destructors). ‘Terminate’ may be called at any time, but may be only called once.
        ''' When this method returns an error, the PME should report the user. However, after that the PME is not
        ''' allowed to use the PMC anymore.
        ''' The Unit specification stated that “Terminate may check if the data has been saved and return an error if
        ''' not.” It is suggested not to follow this recommendation, since it’s the PME responsibility to save the state of
        ''' the PMC before terminating it. In the case that a user wants to close a simulation case without saving it, it’s
        ''' better to leave the PME to handle the situation instead of each PMC providing a different implementation.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Terminate1() Implements CapeOpen.ICapeUtilities.Terminate
            If Not _simcontext Is Nothing Then
                If IsComObject(_simcontext) Then
                    ReleaseComObject(_simcontext)
                Else
                    _simcontext = Nothing
                End If
            End If
            Me.Dispose()
        End Sub

#End Region

#Region "   CAPE-OPEN Persistence Implementation"

        Friend m_dirty As Boolean = True

        Public Sub GetClassID(ByRef pClassID As System.Guid) Implements IPersistStreamInit.GetClassID
            pClassID = New Guid(DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass.ClassId)
        End Sub

        Public Sub GetSizeMax(ByRef pcbSize As Long) Implements IPersistStreamInit.GetSizeMax
            pcbSize = 1024 * 1024
        End Sub

        Public Sub InitNew() Implements IPersistStreamInit.InitNew
            'do nothing
        End Sub

        Public Function IsDirty() As Integer Implements IPersistStreamInit.IsDirty
            Return m_dirty
        End Function

        Public Overridable Sub Load(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream) Implements IPersistStreamInit.Load

            ' Read the length of the string  
            Dim arrLen As Byte() = New [Byte](3) {}
            pStm.Read(arrLen, arrLen.Length, IntPtr.Zero)

            ' Calculate the length  
            Dim cb As Integer = BitConverter.ToInt32(arrLen, 0)

            ' Read the stream to get the string    
            Dim bytes As Byte() = New Byte(cb - 1) {}
            Dim pcb As New IntPtr()
            pStm.Read(bytes, bytes.Length, pcb)
            If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            ' Deserialize byte array    

            Dim memoryStream As New System.IO.MemoryStream(bytes)

            Try

                Dim domain As AppDomain = AppDomain.CurrentDomain
                AddHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

                Dim myarr As ArrayList

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                myarr = mySerializer.Deserialize(memoryStream)

                Me._parameters = myarr(0)
                Me._ports = myarr(1)

                myarr = Nothing
                mySerializer = Nothing

                RemoveHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            memoryStream.Close()

        End Sub

        Public Overridable Sub Save(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream, ByVal fClearDirty As Boolean) Implements IPersistStreamInit.Save

            Dim props As New ArrayList

            With props

                .Add(_parameters)
                .Add(_ports)

            End With

            Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
            Dim mstr As New MemoryStream
            mySerializer.Serialize(mstr, props)
            Dim bytes As Byte() = mstr.ToArray()
            mstr.Close()

            ' construct length (separate into two separate bytes)    

            Dim arrLen As Byte() = BitConverter.GetBytes(bytes.Length)
            Try

                ' Save the array in the stream    
                pStm.Write(arrLen, arrLen.Length, IntPtr.Zero)
                pStm.Write(bytes, bytes.Length, IntPtr.Zero)
                If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            If fClearDirty Then
                m_dirty = False
            End If

        End Sub

        Friend Function MyResolveEventHandler(ByVal sender As Object, ByVal args As ResolveEventArgs) As System.Reflection.Assembly
            Return Me.[GetType]().Assembly
        End Function

#End Region

#Region "   CAPE-OPEN Error Interfaces"

        Sub ThrowCAPEException(ByRef ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

            _code = code
            _description = description
            _interfacename = interf
            _moreinfo = moreinfo
            _operation = operation
            _scope = scope

            Throw ex

        End Sub

        Private _name, _description, _interfacename, _moreinfo, _operation, _scope As String, _code As Integer

        Public ReadOnly Property Name2() As String Implements CapeOpen.ECapeRoot.Name
            Get
                Return Me.Name
            End Get
        End Property

        Public ReadOnly Property code() As Integer Implements CapeOpen.ECapeUser.code
            Get
                Return _code
            End Get
        End Property

        Public ReadOnly Property description() As String Implements CapeOpen.ECapeUser.description
            Get
                Return _description
            End Get
        End Property

        Public ReadOnly Property interfaceName() As String Implements CapeOpen.ECapeUser.interfaceName
            Get
                Return _interfacename
            End Get
        End Property

        Public ReadOnly Property moreInfo() As String Implements CapeOpen.ECapeUser.moreInfo
            Get
                Return _moreinfo
            End Get
        End Property

        Public ReadOnly Property operation() As String Implements CapeOpen.ECapeUser.operation
            Get
                Return _operation
            End Get
        End Property

        Public ReadOnly Property scope1() As String Implements CapeOpen.ECapeUser.scope
            Get
                Return _scope
            End Get
        End Property

#End Region

#Region "   CAPE-OPEN Reports"

        Friend _reports As String() = New String() {"log", "last run", "validation results"}
        Friend _selreport As String = ""
        Friend _calclog As String = ""
        Friend _lastrun As String = ""
        Friend _valres As String = ""

        Public Sub ProduceReport(ByRef message As String) Implements CapeOpen.ICapeUnitReport.ProduceReport
            Select Case _selreport
                Case "log"
                    message = _calclog
                Case "last run"
                    message = _lastrun
                Case "validation results"
                    message = _valres
            End Select
        End Sub

        Public ReadOnly Property reports() As Object Implements CapeOpen.ICapeUnitReport.reports
            Get
                Return _reports
            End Get
        End Property

        Public Property selectedReport() As String Implements CapeOpen.ICapeUnitReport.selectedReport
            Get
                Return _selreport
            End Get
            Set(ByVal value As String)
                _selreport = value
            End Set
        End Property

#End Region

    End Class

    <System.Serializable()> Public MustInherit Class SpecialOpBaseClass

        Inherits DWSIM.SimulationObjects.UnitOperations.BaseClass

        Public Sub New()
            MyBase.CreateNew()
        End Sub

    End Class

End Namespace
