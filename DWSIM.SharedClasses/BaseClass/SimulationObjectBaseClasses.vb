'    Flowsheet Object Base Classes 
'    Copyright 2008-2020 Daniel Wagner O. de Medeiros
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

Imports System.Runtime.InteropServices
Imports System.Text
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Interfaces.Enums
Imports System.Dynamic
Imports System.Reflection
Imports DWSIM.ExtensionMethods

Namespace UnitOperations

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class BaseClass

        Implements ICloneable, IDisposable, Interfaces.ICustomXMLSerialization

        Implements Interfaces.ISimulationObject, Interfaces.IProductInformation

        Public Const ClassId As String = ""

        <System.NonSerialized()> Protected Friend m_flowsheet As Interfaces.IFlowsheet

        <Newtonsoft.Json.JsonIgnore> <Xml.Serialization.XmlIgnore> Public Property LastSolutionInputSnapshot As String = ""

        Protected Friend _IsDirty As Boolean = True
        ReadOnly Property IsDirty As Boolean Implements ISimulationObject.IsDirty
            Get
                Return _IsDirty
            End Get
        End Property

        Protected Friend _CanUsePreviousResults As Boolean = False
        ReadOnly Property CanUsePreviousResults As Boolean Implements ISimulationObject.CanUsePreviousResults
            Get
                Return _CanUsePreviousResults
            End Get
        End Property

        Public Property DynamicsSpec As Enums.Dynamics.DynamicsSpecType = Dynamics.DynamicsSpecType.Pressure Implements ISimulationObject.DynamicsSpec

        Public Property DynamicsOnly As Boolean = False Implements ISimulationObject.DynamicsOnly

        Public Property ExtraProperties As New ExpandoObject Implements ISimulationObject.ExtraProperties

        Public Property ExtraPropertiesUnitTypes As New ExpandoObject Implements ISimulationObject.ExtraPropertiesUnitTypes

        Public Property ExtraPropertiesDescriptions As New ExpandoObject Implements ISimulationObject.ExtraPropertiesDescriptions

        Public Property ExtraPropertiesTypes As New ExpandoObject Implements ISimulationObject.ExtraPropertiesTypes

        Public Overridable Property Visible As Boolean = True

        <NonSerialized()> <Xml.Serialization.XmlIgnore> Public LaunchExternalPropertyEditor() As Action(Of ISimulationObject)

        <NonSerialized()> <Xml.Serialization.XmlIgnore> Public ExtraPropertiesEditor As Form

        Public Property OverrideCalculationRoutine As Boolean = False

        <System.NonSerialized()> <Xml.Serialization.XmlIgnore> Public CalculationRoutineOverride As Action

        Public Property StoreDetailedDebugReport As Boolean = False

        Public Property DetailedDebugReport As String

        Public ReadOnly Property IsFunctional As Boolean = True Implements ISimulationObject.IsFunctional

        Public Overridable Property ComponentDescription() As String = ""

        Public Overridable Property ComponentName() As String = ""

        Public Overrides Function ToString() As String

            If GraphicObject IsNot Nothing Then
                Return GraphicObject.Tag
            Else
                Return MyBase.ToString()
            End If

        End Function

        ''' <summary>
        ''' Gets the current flowsheet where this object is located.
        ''' </summary>
        ''' <value></value>
        ''' <returns>Flowsheet instance.</returns>
        ''' <remarks></remarks>
        Public Overridable ReadOnly Property FlowSheet() As Interfaces.IFlowsheet
            Get
                If Not m_flowsheet Is Nothing Then
                    Return m_flowsheet
                Else
                    Return Nothing
                End If
            End Get
        End Property

        Public Overridable Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Other Implements ISimulationObject.ObjectClass

        Public Overridable ReadOnly Property SupportsDynamicMode As Boolean = False Implements ISimulationObject.SupportsDynamicMode

        Public Overridable ReadOnly Property HasPropertiesForDynamicMode As Boolean = False Implements ISimulationObject.HasPropertiesForDynamicMode

#Region "    Constructors"

        Public Sub New()

        End Sub

        Sub CreateNew()

            If ExtraProperties.Count = 0 Then CreateDynamicProperties()

        End Sub

#End Region

#Region "    ISimulationObject"

        Public Sub AddExtraProperty(pname As String, pvalue As Object) Implements ISimulationObject.AddExtraProperty

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            If Not col1.ContainsKey(pname) Then
                col1.Add(pname, pvalue)
            Else
                Throw New Exception("Property already exists.")
            End If

        End Sub

        Public Sub RemoveExtraProperty(pname As String) Implements ISimulationObject.RemoveExtraProperty

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            If col1.ContainsKey(pname) Then
                col1.Remove(pname)
            Else
                Throw New Exception("Property doesn't exist.")
            End If

        End Sub

        Public Sub SetExtraPropertyValue(pname As String, pvalue As Object) Implements ISimulationObject.SetExtraPropertyValue

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            If col1.ContainsKey(pname) Then
                col1(pname) = pvalue
            Else
                Throw New Exception("Property doesn't exist.")
            End If

        End Sub

        Public Function GetExtraPropertyValue(pname As String) As Object Implements ISimulationObject.GetExtraPropertyValue

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            If col1.ContainsKey(pname) Then
                Return col1(pname)
            Else
                Throw New Exception("Property doesn't exist.")
            End If

        End Function

        Public Sub ClearExtraProperties() Implements ISimulationObject.ClearExtraProperties

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))
            Dim col2 = DirectCast(ExtraPropertiesDescriptions, IDictionary(Of String, Object))
            Dim col3 = DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))
            Dim col4 = DirectCast(ExtraPropertiesTypes, IDictionary(Of String, Object))

            Dim toremove As New List(Of String)
            For Each p In col1
                If Not col2.ContainsKey(p.Key) And Not col3.ContainsKey(p.Key) Then
                    toremove.Add(p.Key)
                End If
            Next

            For Each item In toremove
                col1.Remove(item)
            Next

        End Sub

        Public Sub AddDynamicProperty(pname As String, pdesc As String, pvalue As Double,
                               punittype As Enums.UnitOfMeasure, ptype As System.Type) Implements ISimulationObject.AddDynamicProperty

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))
            Dim col2 = DirectCast(ExtraPropertiesDescriptions, IDictionary(Of String, Object))
            Dim col3 = DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))
            Dim col4 = DirectCast(ExtraPropertiesTypes, IDictionary(Of String, Object))

            If Not col1.ContainsKey(pname) Then col1.Add(pname, pvalue)
            If Not col2.ContainsKey(pname) Then col2.Add(pname, pdesc)
            If Not col3.ContainsKey(pname) Then col3.Add(pname, punittype)
            If Not col4.ContainsKey(pname) Then col4.Add(pname, ptype)

        End Sub

        Public Function SetDynamicProperty(id As String, value As Object) As Boolean Implements ISimulationObject.AddDynamicProperty

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            col1(id) = value

            Return True

        End Function

        Public Function GetDynamicProperty(id As String) As Object Implements ISimulationObject.GetDynamicProperty

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            If col1.ContainsKey(id) Then Return col1(id) Else Return Nothing

        End Function

        Public Function GetDynamicPropertyUnitType(id As String) As Enums.UnitOfMeasure Implements ISimulationObject.GetDynamicPropertyUnitType

            Dim col1 = DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))

            If col1.ContainsKey(id) Then Return col1(id) Else Return Nothing

        End Function

        Public Sub RemoveDynamicProperty(pname As String) Implements ISimulationObject.RemoveDynamicProperty

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))
            Dim col2 = DirectCast(ExtraPropertiesDescriptions, IDictionary(Of String, Object))
            Dim col3 = DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))
            Dim col4 = DirectCast(ExtraPropertiesTypes, IDictionary(Of String, Object))

            If col1.ContainsKey(pname) Then col1.Remove(pname)
            If col2.ContainsKey(pname) Then col2.Remove(pname)
            If col1.ContainsKey(pname) Then col3.Remove(pname)
            If col4.ContainsKey(pname) Then col4.Remove(pname)

        End Sub

        Public Function IsDynamicProperty(pname As String) As Boolean Implements ISimulationObject.IsDynamicProperty

            Dim col1 = DirectCast(ExtraPropertiesDescriptions, IDictionary(Of String, Object))

            Return col1.ContainsKey(pname)

        End Function

        Public Overridable Function GetChartModel(name As String) As Object Implements ISimulationObject.GetChartModel
            If CreateChartAction IsNot Nothing Then
                Return CreateChartAction.Invoke(name)
            Else
                Return Nothing
            End If
        End Function

        Public Overridable Function GetPropertyDescription(prop As String) As String Implements ISimulationObject.GetPropertyDescription
            Return "No description is available for this property."
        End Function

        Public Overridable Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String Implements ISimulationObject.GetReport
            Return "No report is available for this object."
        End Function

        Public Overridable Function GetVersion() As Version Implements ISimulationObject.GetVersion
            Return Me.GetType.Assembly.GetName.Version
        End Function

        Public MustOverride Function GetDisplayName() As String Implements ISimulationObject.GetDisplayName

        Public MustOverride Function GetDisplayDescription() As String Implements ISimulationObject.GetDisplayDescription

        Public MustOverride Function GetIconBitmap() As Object Implements ISimulationObject.GetIconBitmap

        <NonSerialized> Private _AttachedUtilities As New List(Of IAttachedUtility)

        Public Property AttachedUtilities As List(Of IAttachedUtility) Implements ISimulationObject.AttachedUtilities
            Get
                If _AttachedUtilities Is Nothing Then _AttachedUtilities = New List(Of IAttachedUtility)
                Return _AttachedUtilities
            End Get
            Set(value As List(Of IAttachedUtility))
                _AttachedUtilities = value
            End Set
        End Property

        Public Property PreferredFlashAlgorithmTag As String = "" Implements ISimulationObject.PreferredFlashAlgorithmTag

        Public Property Calculated As Boolean = False Implements Interfaces.ISimulationObject.Calculated

        Public Property DebugMode As Boolean = False Implements Interfaces.ISimulationObject.DebugMode

        <Xml.Serialization.XmlIgnore> Public Property DebugText As String = "" Implements Interfaces.ISimulationObject.DebugText

        Public Property LastUpdated As New Date Implements Interfaces.ISimulationObject.LastUpdated

        ''' <summary>
        ''' Calculates the object.
        ''' </summary>
        ''' <param name="args"></param>
        ''' <remarks>Use 'Solve()' to calculate the object instead.</remarks>
        Public Overridable Sub Calculate(Optional ByVal args As Object = Nothing) Implements ISimulationObject.Calculate
            Throw New NotImplementedException
        End Sub

        Public Sub DeCalculate(Optional args As Object = Nothing) Implements ISimulationObject.DeCalculate
            'Throw New NotImplementedException
        End Sub

        Public Overridable Sub RunDynamicModel() Implements ISimulationObject.RunDynamicModel

            Throw New Exception("This Unit Operation is not yet supported in Dynamic Mode.")

        End Sub

        Public Overridable Sub PerformPostCalcValidation() Implements ISimulationObject.PerformPostCalcValidation

            If GraphicObject.ObjectType <> ObjectType.MaterialStream And GraphicObject.ObjectType <> ObjectType.EnergyStream And
                GraphicObject.ObjectType <> ObjectType.OT_Adjust And GraphicObject.ObjectType <> ObjectType.OT_Spec And
                GraphicObject.ObjectType <> ObjectType.OT_Recycle And GraphicObject.ObjectType <> ObjectType.OT_EnergyRecycle Then

                'calculate mass balance

                Dim mb As Double = 0.0#
                Dim eb As Double = 0.0#
                Dim mbe As Double = 0.0#
                Dim ebe As Double = 0.0#
                Dim mbt As Double = 0.0#
                Dim ebt As Double = 0.0#
                Dim mi, hi, hf As Double

                Dim imsc As List(Of ISimulationObject) = GraphicObject.InputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedFrom.Name)).ToList
                Dim omsc As List(Of ISimulationObject) = GraphicObject.OutputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedTo.Name)).ToList

                For Each ims In imsc
                    If ims.GraphicObject.Active Then
                        If DirectCast(ims, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                            mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(ims.GetPropertyValue("PROP_MS_6"))
                        Else
                            mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_2"))
                        End If
                        mb += mi
                        mbt += mi
                        hi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_7"))
                        eb -= mi * hi 'kg/s * kJ/kg = kJ/s = kW
                        ebt += Math.Abs(mi * hi)
                        'heats of formation
                        hf = DirectCast(ims, IMaterialStream).GetOverallHeatOfFormation()
                        eb -= hf
                        ebt += Math.Abs(hf)
                    End If
                Next

                For Each oms In omsc
                    If oms.GraphicObject.Active Then
                        If DirectCast(oms, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                            mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(oms.GetPropertyValue("PROP_MS_6"))
                        Else
                            mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_2"))
                        End If
                        mb -= mi
                        mbt += mi
                        hi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_7"))
                        eb += mi * hi 'kg/s * kJ/kg = kJ/s = kW
                        ebt += Math.Abs(mi * hi)
                        'heats of formation
                        hf = DirectCast(oms, IMaterialStream).GetOverallHeatOfFormation()
                        eb += hf
                        ebt += Math.Abs(hf)
                    End If
                Next

                mbe = mb / mbt

                Dim iesc As List(Of ISimulationObject) = GraphicObject.InputConnectors.Where(Function(x) x.IsAttached And (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedFrom.Name)).ToList
                Dim oesc As List(Of ISimulationObject) = GraphicObject.OutputConnectors.Where(Function(x) x.IsAttached And (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedTo.Name)).ToList

                For Each ies In iesc
                    If ies.GraphicObject.Active Then
                        eb -= Convert.ToDouble(ies.GetPropertyValue("PROP_ES_0"))
                        ebt += Math.Abs(Convert.ToDouble(ies.GetPropertyValue("PROP_ES_0")))
                    End If
                Next

                For Each oes In oesc
                    If oes.GraphicObject.Active Then
                        eb += Convert.ToDouble(oes.GetPropertyValue("PROP_ES_0"))
                        ebt += Math.Abs(Convert.ToDouble(oes.GetPropertyValue("PROP_ES_0")))
                    End If
                Next

                If GraphicObject.EnergyConnector.IsAttached Then
                    Dim inobj = FlowSheet.SimulationObjects(GraphicObject.EnergyConnector.AttachedConnector.AttachedFrom.Name)
                    If inobj.GraphicObject.IsEnergyStream And inobj.GraphicObject.Active Then
                        eb -= Convert.ToDouble(inobj.GetPropertyValue("PROP_ES_0"))
                        ebt += Math.Abs(Convert.ToDouble(inobj.GetPropertyValue("PROP_ES_0")))
                    End If
                    Dim outobj = FlowSheet.SimulationObjects(GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                    If outobj.GraphicObject.IsEnergyStream And outobj.GraphicObject.Active Then
                        eb += Convert.ToDouble(outobj.GetPropertyValue("PROP_ES_0"))
                        ebt += Math.Abs(Convert.ToDouble(outobj.GetPropertyValue("PROP_ES_0")))
                    End If
                End If

                If iesc.Count + oesc.Count = 0 And Not GraphicObject.EnergyConnector.IsAttached Then
                    eb = 0
                End If

                mbe = Math.Abs(mb / mbt)

                ebe = Math.Abs(eb / ebt)

                If FlowSheet.FlowsheetOptions.MassBalanceRelativeTolerance = 0.0 Then
                    FlowSheet.FlowsheetOptions.MassBalanceRelativeTolerance = 0.01
                End If

                If FlowSheet.FlowsheetOptions.EnergyBalanceRelativeTolerance = 0.0 Then
                    FlowSheet.FlowsheetOptions.EnergyBalanceRelativeTolerance = 0.01
                End If

                If mbe > FlowSheet.FlowsheetOptions.MassBalanceRelativeTolerance Then
                    If FlowSheet.FlowsheetOptions.MassBalanceCheck = WarningType.RaiseError Then
                        Throw New Exception(GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("MassBalanceMessage") + " (" + mbe.ToString() + " > " + FlowSheet.FlowsheetOptions.MassBalanceRelativeTolerance.ToString + ")")
                    ElseIf FlowSheet.FlowsheetOptions.MassBalanceCheck = WarningType.ShowWarning Then
                        FlowSheet.ShowMessage(GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("MassBalanceMessage") + " (" + mbe.ToString() + " > " + FlowSheet.FlowsheetOptions.MassBalanceRelativeTolerance.ToString + ")", IFlowsheet.MessageType.Warning)
                    End If
                End If

                If ebe > FlowSheet.FlowsheetOptions.EnergyBalanceRelativeTolerance Then
                    If FlowSheet.FlowsheetOptions.EnergyBalanceCheck = WarningType.RaiseError Then
                        Throw New Exception(GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("EnergyBalanceMessage") + " (" + ebe.ToString() + " > " + FlowSheet.FlowsheetOptions.EnergyBalanceRelativeTolerance.ToString + ")")
                    ElseIf FlowSheet.FlowsheetOptions.EnergyBalanceCheck = WarningType.ShowWarning Then
                        FlowSheet.ShowMessage(GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("EnergyBalanceMessage") + " (" + ebe.ToString() + " > " + FlowSheet.FlowsheetOptions.EnergyBalanceRelativeTolerance.ToString + ")", IFlowsheet.MessageType.Warning)
                    End If
                End If

            End If

        End Sub

        Public Sub Solve() Implements ISimulationObject.Solve

            If FlowSheet.FlowsheetOptions.ForceObjectSolving Then
                Calculated = False
                If OverrideCalculationRoutine Then
                    CalculationRoutineOverride.Invoke()
                Else
                    Calculate()
                End If
                Calculated = True
            Else
                CheckDirtyStatus()
                Calculated = False
                If Not CanUsePreviousResults Then Calculate()
                Calculated = True
                PerformPostCalcValidation()
                If GraphicObject IsNot Nothing Then
                    If GraphicObject.ObjectType <> ObjectType.EnergyStream And GraphicObject.ObjectType <> ObjectType.MaterialStream Then
                        Dim xdoc = New XDocument()
                        xdoc.Add(New XElement("Data"))
                        xdoc.Element("Data").Add(SaveData())
                        xdoc.Element("Data").Element("Calculated").Remove()
                        xdoc.Element("Data").Element("LastUpdated").Remove()
                        LastSolutionInputSnapshot = xdoc.ToString()
                        xdoc = Nothing
                    End If
                End If
            End If

        End Sub

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public fd As DynamicsPropertyEditor

        Public Overridable Sub DisplayDynamicsEditForm() Implements ISimulationObject.DisplayDynamicsEditForm

            If fd Is Nothing Then
                fd = New DynamicsPropertyEditor With {.SimObject = Me}
                fd.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
                fd.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(fd)
            Else
                If fd.IsDisposed Then
                    fd = New DynamicsPropertyEditor With {.SimObject = Me}
                    fd.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
                    fd.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(fd)
                Else
                    fd.Activate()
                End If
            End If

        End Sub

        Public Sub UpdateDynamicsEditForm() Implements ISimulationObject.UpdateDynamicsEditForm

            If fd IsNot Nothing Then
                If Not fd.IsDisposed Then
                    fd.UIThread(Sub() fd.UpdateInfo())
                End If
            End If

        End Sub

        Public Sub CloseDynamicsEditForm() Implements ISimulationObject.CloseDynamicsEditForm

            If fd IsNot Nothing Then
                If Not fd.IsDisposed Then
                    fd.Close()
                    fd = Nothing
                End If
            End If

        End Sub

        Public MustOverride Sub DisplayEditForm() Implements ISimulationObject.DisplayEditForm

        Public MustOverride Sub UpdateEditForm() Implements ISimulationObject.UpdateEditForm

        Public Overridable Function GetEditingForm() As Form Implements ISimulationObject.GetEditingForm

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
                Return Nothing
            End Get
            Set(ByVal value As Nullable(Of Double))

            End Set
        End Property

        ''' <summary>
        ''' Phase collection, only implemented for Material Streams.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public Overridable ReadOnly Property Phases() As Dictionary(Of Integer, Interfaces.IPhase)
            Get
                Throw New NotImplementedException
            End Get
        End Property

        ''' <summary>
        ''' Validates the object, checking its connections and other parameters.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Validate() Implements Interfaces.ISimulationObject.Validate

            Dim vForm As Interfaces.IFlowsheet = FlowSheet
            Dim vCon As Interfaces.IConnectionPoint

            'Validate input connections.
            For Each vCon In Me.GraphicObject.InputConnectors
                If Not vCon.IsAttached Then
                    Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If
            Next

            'Validate output connections.
            For Each vCon In Me.GraphicObject.OutputConnectors
                If Not vCon.IsAttached Then
                    Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If
            Next

        End Sub

        Public Overridable Function GetDebugReport() As String Implements Interfaces.ISimulationObject.GetDebugReport
            Return "Error - function not implemented"
        End Function

        Public Sub AppendDebugLine(text As String) Implements Interfaces.ISimulationObject.AppendDebugLine
            DebugText += text & vbCrLf & vbCrLf
        End Sub

        ''' <summary>
        ''' Gets or sets the error message regarding the last calculation attempt.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property ErrorMessage() As String Implements Interfaces.ISimulationObject.ErrorMessage

        ''' <summary>
        ''' Checks if a value is valid.
        ''' </summary>
        ''' <param name="val">Value to be checked.</param>
        ''' <param name="onlypositive">Value should be a positive double or not.</param>
        ''' <param name="paramname">Name of the parameter (ex. P, T, W, H etc.)</param>
        ''' <remarks></remarks>
        Public Sub CheckSpec(val As Double, onlypositive As Boolean, paramname As String) Implements Interfaces.ISimulationObject.CheckSpec

            If Not val.IsValid Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidUOSpecValue") & " (name: " & paramname & ", value: " & val & ")")
            If onlypositive Then If val.IsNegative Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidUOSpecValue") & " (name: " & paramname & ", value: " & val & ")")

        End Sub

        Public Property Annotation() As String = "" Implements Interfaces.ISimulationObject.Annotation

        ''' <summary>
        ''' Checks if an Adjust operation is attached to this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property IsAdjustAttached() As Boolean = False Implements Interfaces.ISimulationObject.IsAdjustAttached

        ''' <summary>
        ''' If an Adjust object is attached to this object, returns its ID.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AttachedAdjustId() As String = "" Implements Interfaces.ISimulationObject.AttachedAdjustId

        ''' <summary>
        ''' If an Adjust object is attached to this object, returns a variable describing how this object is used by it (manipulated, controlled or reference).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AdjustVarType() As Interfaces.Enums.AdjustVarType Implements Interfaces.ISimulationObject.AdjustVarType

        ''' <summary>
        ''' Checks if an Specification operation is attached to this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property IsSpecAttached() As Boolean = False Implements Interfaces.ISimulationObject.IsSpecAttached

        ''' <summary>
        ''' If an Specification object is attached to this object, returns its ID.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AttachedSpecId() As String = "" Implements Interfaces.ISimulationObject.AttachedSpecId

        ''' <summary>
        ''' If an Specification object is attached to this object, returns a variable describing how this object is used by it (target or source).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property SpecVarType() As Interfaces.Enums.SpecVarType Implements Interfaces.ISimulationObject.SpecVarType

        ''' <summary>
        ''' Gets or sets the graphic object representation of this object in the flowsheet.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore> Public Property GraphicObject() As Interfaces.IGraphicObject Implements Interfaces.ISimulationObject.GraphicObject

        ''' <summary>
        ''' Object's Unique ID (Name)
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks>This property is the same as the graphic object 'Name' property.</remarks>
        Public Property Name() As String Implements Interfaces.ISimulationObject.Name
            Get
                Return ComponentName
            End Get
            Set(value As String)
                ComponentName = value
            End Set
        End Property

        Public Overridable Function GetProperties(proptype As PropertyType) As String() Implements Interfaces.ISimulationObject.GetProperties

            Dim proplist As New List(Of String)

            Dim epcol = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            For Each item In epcol
                proplist.Add(item.Key)
            Next

            For Each item In AttachedUtilities
                proplist.AddRange(item.GetPropertyList().ConvertAll(New Converter(Of String, String)(Function(s As String)
                                                                                                         Return item.Name & ": " & s
                                                                                                     End Function)))
            Next

            Return proplist.ToArray

        End Function

        Public Overridable Function GetPropertyUnit(prop As String, Optional su As Interfaces.IUnitsOfMeasure = Nothing) As String Implements Interfaces.ISimulationObject.GetPropertyUnit

            If su Is Nothing Then
                su = FlowSheet.FlowsheetOptions.SelectedUnitSystem
            End If

            Dim epcol = DirectCast(ExtraProperties, IDictionary(Of String, Object))
            Dim epucol = DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))

            If epcol.ContainsKey(prop) Then
                If epucol.ContainsKey(prop) Then
                    Dim utype = epucol(prop)
                    Return su.GetCurrentUnits(utype)
                Else
                    Return ""
                End If
            End If

            For Each item In AttachedUtilities
                If prop.StartsWith(item.Name) Then
                    For Each prop1 In item.GetPropertyList()
                        If prop.Contains(prop1) Then Return item.GetPropertyUnits(prop.Split(": ")(1).Trim)
                    Next
                End If
            Next

            Return "NF"

        End Function

        Public Overridable Function GetPropertyValue(prop As String, Optional su As Interfaces.IUnitsOfMeasure = Nothing) As Object Implements Interfaces.ISimulationObject.GetPropertyValue

            For Each item In AttachedUtilities
                If prop.StartsWith(item.Name) Then
                    For Each prop1 In item.GetPropertyList()
                        If prop.Contains(prop1) Then Return item.GetPropertyValue(prop.Split(": ")(1).Trim)
                    Next
                End If

            Next

            Return Nothing

        End Function

        Public Overridable Function SetPropertyValue(prop As String, propval As Object, Optional su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean Implements Interfaces.ISimulationObject.SetPropertyValue

            Dim epcol = DirectCast(ExtraProperties, IDictionary(Of String, Object))

            If epcol.ContainsKey(prop) Then
                epcol(prop) = propval
                Return True
            End If

            For Each item In AttachedUtilities
                If prop.StartsWith(item.Name) Then
                    For Each prop1 In item.GetPropertyList()
                        If prop.Contains(prop1) Then
                            item.SetPropertyValue(prop.Split(": ")(1).Trim, propval)
                            Return True
                        End If
                    Next
                End If
            Next

            Return False

        End Function

        Public Overridable Function GetDefaultProperties() As String() Implements ISimulationObject.GetDefaultProperties
            Return GetProperties(PropertyType.ALL)
        End Function

        Public Overridable Property PropertyPackage As IPropertyPackage Implements ISimulationObject.PropertyPackage

        Public Function GetFlowsheet() As IFlowsheet Implements ISimulationObject.GetFlowsheet
            Return FlowSheet
        End Function

        Public MustOverride Sub CloseEditForm() Implements ISimulationObject.CloseEditForm

        Public MustOverride Function CloneXML() As Object Implements ISimulationObject.CloneXML

        Public MustOverride Function CloneJSON() As Object Implements ISimulationObject.CloneJSON

        Public MustOverride ReadOnly Property MobileCompatible As Boolean Implements ISimulationObject.MobileCompatible

        Public Property UserDefinedChartNames As New List(Of String)

        Public Property CreateChartAction As Func(Of String, Object)

        Public Overridable Function GetChartModelNames() As List(Of String) Implements ISimulationObject.GetChartModelNames
            If UserDefinedChartNames.Count > 0 Then
                Return UserDefinedChartNames
            Else
                Return New List(Of String)
            End If
        End Function

#End Region

#Region "    ICloneable"

        ''' <summary>
        ''' Clones the current object, returning a new one with identical properties.
        ''' </summary>
        ''' <returns>An object of the same type with the same properties.</returns>
        ''' <remarks>Properties and fields marked with the 'NonSerializable' attribute aren't cloned.</remarks>
        Public Overridable Function Clone() As Object Implements System.ICloneable.Clone

            Return ObjectCopy(Me)

        End Function

        Function ObjectCopy(ByVal obj As UnitOperations.BaseClass) As Object

            Return Me.CloneXML

        End Function

#End Region

#Region "    IDisposable Support "

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

#Region "    IXMLSerialization"

        ''' <summary>
        ''' Loads object data stored in a collection of XML elements.
        ''' </summary>
        ''' <param name="data"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            ExtraProperties = New ExpandoObject

            Dim xel_d = (From xel2 As XElement In data Select xel2 Where xel2.Name = "DynamicProperties")

            If Not xel_d Is Nothing Then
                Dim dataDyn As List(Of XElement) = xel_d.Elements.ToList
                For Each xel As XElement In dataDyn
                    Try
                        Dim propname = xel.Element("Name").Value
                        Dim proptype = xel.Element("PropertyType").Value
                        Dim ptype As Type = Type.GetType(proptype)
                        Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                        DirectCast(ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                    Catch ex As Exception
                    End Try
                Next
            End If

            ExtraPropertiesDescriptions = New ExpandoObject

            Dim xel_dd = (From xel2 As XElement In data Select xel2 Where xel2.Name = "DynamicPropertiesDescriptions")

            If Not xel_dd Is Nothing Then
                Dim dataDyn As List(Of XElement) = xel_dd.Elements.ToList
                For Each xel As XElement In dataDyn
                    Try
                        Dim propname = xel.Element("Name").Value
                        Dim proptype = xel.Element("PropertyType").Value
                        Dim ptype As Type = Type.GetType(proptype)
                        Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                        DirectCast(ExtraPropertiesDescriptions, IDictionary(Of String, Object))(propname) = propval
                    Catch ex As Exception
                    End Try
                Next
            End If

            ExtraPropertiesUnitTypes = New ExpandoObject

            Dim xel_ddt = (From xel2 As XElement In data Select xel2 Where xel2.Name = "DynamicPropertiesUnitTypes")

            If Not xel_ddt Is Nothing Then
                Dim dataDyn As List(Of XElement) = xel_ddt.Elements.ToList
                For Each xel As XElement In dataDyn
                    Try
                        Dim propname = xel.Element("Name").Value
                        Dim proptype = xel.Element("PropertyType").Value
                        Dim ptype As Type = Type.GetType(proptype)
                        Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                        DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))(propname) = propval
                    Catch ex As Exception
                    End Try
                Next
            End If

            'If ExtraProperties.Count = 0 Then
            CreateDynamicProperties()
            'End If

            Dim xel_u = (From xel2 As XElement In data Select xel2 Where xel2.Name = "AttachedUtilities")

            If Not xel_u Is Nothing Then
                Dim dataUtilities As List(Of XElement) = xel_u.Elements.ToList
                For Each xel As XElement In dataUtilities
                    Try
                        Dim u = FlowSheet.GetUtility(xel.Element("UtilityType").Value)
                        u.ID = xel.Element("ID").Value
                        u.Name = xel.Element("Name").Value
                        u.AttachedTo = Me
                        u.Initialize()
                        u.LoadData(Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, Object))(xel.Element("Data").Value))
                        Me.AttachedUtilities.Add(u)
                    Catch ex As Exception
                        FlowSheet.ShowMessage("Error restoring attached utility to " & Me.Name & ": " & ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                    End Try
                Next
            End If

            If Me.Annotation = "DWSIM.DWSIM.Outros.Annotation" Then Me.Annotation = ""

            Return True

        End Function

        ''' <summary>
        ''' Saves object data in a collection of XML elements.
        ''' </summary>
        ''' <returns>A List of XML elements containing object data.</returns>
        ''' <remarks></remarks>
        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)

            With elements
                .Add(New XElement("DynamicProperties"))
                Dim extraprops = DirectCast(ExtraProperties, IDictionary(Of String, Object))
                For Each item In extraprops
                    Try
                        .Item(.Count - 1).Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                               New XElement("PropertyType", item.Value.GetType.ToString),
                                                                               New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
                    Catch ex As Exception
                    End Try
                Next

                .Add(New XElement("DynamicPropertiesDescriptions"))
                Dim extrapropsdesc = DirectCast(ExtraPropertiesDescriptions, IDictionary(Of String, Object))
                For Each item In extrapropsdesc
                    Try
                        .Item(.Count - 1).Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                               New XElement("PropertyType", item.Value.GetType.ToString),
                                                                               New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
                    Catch ex As Exception
                    End Try
                Next

                .Add(New XElement("DynamicPropertiesUnitTypes"))
                Dim extrapropsunits = DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))
                For Each item In extrapropsunits
                    Try
                        .Item(.Count - 1).Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                               New XElement("PropertyType", item.Value.GetType.ToString),
                                                                               New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
                    Catch ex As Exception
                    End Try
                Next

                .Add(New XElement("AttachedUtilities"))
                For Each util In AttachedUtilities
                    Try
                        .Item(.Count - 1).Add(New XElement("AttachedUtility", {New XElement("ID", util.ID),
                                                                               New XElement("Name", util.Name),
                                                                               New XElement("UtilityType", Convert.ToInt32(util.GetUtilityType)),
                                                                               New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(util.SaveData))}))
                    Catch ex As Exception
                    End Try
                Next
            End With

            Return elements

        End Function

#End Region

#Region "    Extras"

        ''' <summary>
        ''' Copies the object properties to the Clipboard.
        ''' </summary>
        ''' <param name="su">Units system to use.</param>
        ''' <param name="nf">Number format to use.</param>
        ''' <remarks></remarks>
        Public Sub CopyDataToClipboard(su As SystemsOfUnits.Units, nf As String)

            Dim DT As New DataTable
            DT.Columns.Clear()
            DT.Columns.Add(("Propriedade"), GetType(System.String))
            DT.Columns.Add(("Valor"), GetType(System.String))
            DT.Columns.Add(("Unidade"), GetType(System.String))
            DT.Rows.Clear()

            Dim baseobj As BaseClass
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
            properties = baseobj.GetProperties(Interfaces.Enums.PropertyType.ALL)
            objtype = baseobj.GraphicObject.ObjectType
            description = Me.FlowSheet.GetTranslatedString(baseobj.GraphicObject.Description)
            If objtype = ObjectType.MaterialStream Then
                Dim value As String
                For propidx = 0 To r1 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                For propidx = r1 To r2 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaMistura"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(0).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r2 To r3 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseVapor"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(2).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r3 To r4 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(1).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r4 To r5 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(3).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r5 To r6 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(4).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r6 To 101
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(6).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
            Else
                For Each prop As String In properties
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(prop), Format(baseobj.GetPropertyValue(prop, su), nf), baseobj.GetPropertyUnit(prop, su)})
                Next
            End If

            Dim st As New StringBuilder(Me.FlowSheet.GetTranslatedString(Me.ComponentDescription) & ": " & Me.GraphicObject.Tag & vbCrLf)
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
        Public Sub SetFlowsheet(ByVal flowsheet As Object) Implements Interfaces.ISimulationObject.SetFlowsheet
            m_flowsheet = flowsheet
        End Sub

        Public Overridable Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String())) Implements ISimulationObject.GetStructuredReport
            Return New List(Of Tuple(Of ReportItemType, String()))
        End Function

        Public Overridable Sub CreateDynamicProperties() Implements ISimulationObject.CreateDynamicProperties

        End Sub

        Public Sub DisplayExtraPropertiesEditForm() Implements ISimulationObject.DisplayExtraPropertiesEditForm

            Dim col1 = DirectCast(ExtraProperties, IDictionary(Of String, Object))
            Dim col2 = DirectCast(ExtraPropertiesDescriptions, IDictionary(Of String, Object))
            Dim count As Integer = 0
            For Each prop In col1
                If Not col2.ContainsKey(prop.Key) Then
                    count += 1
                End If
            Next

            If count > 0 Then
                If ExtraPropertiesEditor Is Nothing Then
                    ExtraPropertiesEditor = New FormExtraProperties With {.SimObject = Me}
                    Me.FlowSheet.DisplayForm(ExtraPropertiesEditor)
                Else
                    If ExtraPropertiesEditor.IsDisposed Then
                        ExtraPropertiesEditor = New FormExtraProperties With {.SimObject = Me}
                        ExtraPropertiesEditor.Tag = "ObjectEditor"
                        Me.FlowSheet.DisplayForm(ExtraPropertiesEditor)
                    Else
                        DirectCast(ExtraPropertiesEditor, FormExtraProperties).Activate()
                    End If
                End If
            End If

        End Sub

        Public Sub UpdateExtraPropertiesEditForm() Implements ISimulationObject.UpdateExtraPropertiesEditForm

            If ExtraPropertiesEditor IsNot Nothing Then
                If Not ExtraPropertiesEditor.IsDisposed Then
                    ExtraPropertiesEditor.UIThread(Sub() DirectCast(ExtraPropertiesEditor, FormExtraProperties).UpdateValues())
                End If
            End If

        End Sub

        Public Function GetDebugWriter() As StringBuilder

            If StoreDetailedDebugReport Then
                Dim drw As New Text.StringBuilder()
                If GraphicObject IsNot Nothing Then
                    drw?.AppendLine("Debug Report: " + GraphicObject.Tag)
                Else
                    drw?.AppendLine("Debug Report: " + ComponentName)
                End If
                drw?.AppendLine("Date: " + Date.Now.ToString())
                drw?.AppendLine()
                drw?.AppendLine("Application & System Info: ")
                drw?.AppendLine()

                Dim version = Assembly.GetExecutingAssembly().GetName().Version.ToString() & " (" &
                    IO.File.GetLastWriteTimeUtc(Assembly.GetExecutingAssembly().Location).ToString() + ")"

                drw?.AppendLine(String.Format("Unit Operations Library Version: {0}", version))
                drw?.AppendLine(String.Format("OS Version: {0}", My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"))
                drw?.AppendLine(String.Format("Runtime Version: {0}", SharedClasses.Utility.GetRuntimeVersion()))

                Try
                    Dim scrh As New System.Management.ManagementObjectSearcher("select * from Win32_Processor")
                    Dim cpu As String = System.Environment.GetEnvironmentVariable("PROCESSOR_IDENTIFIER")
                    For Each qinfo In scrh.Get()
                        cpu += " / " & qinfo.Properties("Name").Value.ToString
                    Next
                    drw?.AppendLine(String.Format("CPU Info: {0}", cpu))
                Catch ex As Exception
                End Try

                drw?.AppendLine()
                drw?.AppendLine("Solver Settings: ")
                drw?.AppendLine()
                drw?.AppendLine(String.Format("Solver Option: {0}", GlobalSettings.Settings.SolverMode))
                drw?.AppendLine(String.Format("Use Parallel CPU Acceleration: {0}", GlobalSettings.Settings.EnableParallelProcessing))
                drw?.AppendLine(String.Format("Use Parallel GPU Acceleration: {0}", GlobalSettings.Settings.EnableGPUProcessing))
                drw?.AppendLine(String.Format("Use CPU SIMD Extensions: {0}", GlobalSettings.Settings.UseSIMDExtensions))
                drw?.AppendLine()
                drw?.AppendLine("Calculation Report: ")
                drw?.AppendLine()
                Return drw
            Else
                Return Nothing
            End If

        End Function

        Public Sub StoreDebugReport(DebugReportWriter As StringBuilder)

            If DebugReportWriter IsNot Nothing Then
                DetailedDebugReport = DebugReportWriter.ToString()
            End If

        End Sub

        Public Function GetEnergyBalanceResidual() As Double Implements ISimulationObject.GetEnergyBalanceResidual

            If GraphicObject.ObjectType <> ObjectType.MaterialStream And GraphicObject.ObjectType <> ObjectType.EnergyStream And
                GraphicObject.ObjectType <> ObjectType.OT_Adjust And GraphicObject.ObjectType <> ObjectType.OT_Spec And
                GraphicObject.ObjectType <> ObjectType.OT_Recycle And GraphicObject.ObjectType <> ObjectType.OT_EnergyRecycle Then

                Dim eb As Double = 0.0#
                Dim ebe As Double = 0.0#
                Dim mi, hi, hf As Double

                Dim imsc As List(Of ISimulationObject) = GraphicObject.InputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedFrom.Name)).ToList
                Dim omsc As List(Of ISimulationObject) = GraphicObject.OutputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedTo.Name)).ToList

                For Each ims In imsc
                    If ims.GraphicObject.Active Then
                        If DirectCast(ims, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                            mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(ims.GetPropertyValue("PROP_MS_6"))
                        Else
                            mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_2"))
                        End If
                        hi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_7"))
                        eb -= mi * hi 'kg/s * kJ/kg = kJ/s = kW
                        'heats of formation
                        hf = DirectCast(ims, IMaterialStream).GetOverallHeatOfFormation()
                        eb -= hf
                    End If
                Next

                For Each oms In omsc
                    If oms.GraphicObject.Active Then
                        If DirectCast(oms, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                            mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(oms.GetPropertyValue("PROP_MS_6"))
                        Else
                            mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_2"))
                        End If
                        hi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_7"))
                        eb += mi * hi 'kg/s * kJ/kg = kJ/s = kW
                        'heats of formation
                        hf = DirectCast(oms, IMaterialStream).GetOverallHeatOfFormation()
                        eb += hf
                    End If
                Next

                Dim iesc As List(Of ISimulationObject) = GraphicObject.InputConnectors.Where(Function(x) x.IsAttached And (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedFrom.Name)).ToList
                Dim oesc As List(Of ISimulationObject) = GraphicObject.OutputConnectors.Where(Function(x) x.IsAttached And (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedTo.Name)).ToList

                For Each ies In iesc
                    If ies.GraphicObject.Active Then
                        eb -= Convert.ToDouble(ies.GetPropertyValue("PROP_ES_0"))
                    End If
                Next

                For Each oes In oesc
                    If oes.GraphicObject.Active Then
                        eb += Convert.ToDouble(oes.GetPropertyValue("PROP_ES_0"))
                    End If
                Next

                If GraphicObject.EnergyConnector.IsAttached Then
                    Dim inobj = FlowSheet.SimulationObjects(GraphicObject.EnergyConnector.AttachedConnector.AttachedFrom.Name)
                    If inobj.GraphicObject.IsEnergyStream And inobj.GraphicObject.Active Then
                        eb -= Convert.ToDouble(inobj.GetPropertyValue("PROP_ES_0"))
                    End If
                    Dim outobj = FlowSheet.SimulationObjects(GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                    If outobj.GraphicObject.IsEnergyStream And outobj.GraphicObject.Active Then
                        eb += Convert.ToDouble(outobj.GetPropertyValue("PROP_ES_0"))
                    End If
                End If

                'If iesc.Count + oesc.Count = 0 And Not GraphicObject.EnergyConnector.IsAttached Then
                '    eb = 0
                'End If

                Return eb

            End If

            Return 0

        End Function

        Public Function GetMassBalanceResidual() As Double Implements ISimulationObject.GetMassBalanceResidual

            If GraphicObject.ObjectType <> ObjectType.MaterialStream And GraphicObject.ObjectType <> ObjectType.EnergyStream And
                GraphicObject.ObjectType <> ObjectType.OT_Adjust And GraphicObject.ObjectType <> ObjectType.OT_Spec And
                GraphicObject.ObjectType <> ObjectType.OT_Recycle And GraphicObject.ObjectType <> ObjectType.OT_EnergyRecycle Then

                'calculate mass balance

                Dim mb As Double = 0.0#
                Dim mi As Double

                Dim imsc As List(Of ISimulationObject) = GraphicObject.InputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedFrom.Name)).ToList
                Dim omsc As List(Of ISimulationObject) = GraphicObject.OutputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedTo.Name)).ToList

                For Each ims In imsc
                    If ims.GraphicObject.Active Then
                        If DirectCast(ims, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                            mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(ims.GetPropertyValue("PROP_MS_6"))
                        Else
                            mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_2"))
                        End If
                        mb += mi
                    End If
                Next

                For Each oms In omsc
                    If oms.GraphicObject.Active Then
                        If DirectCast(oms, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                            mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(oms.GetPropertyValue("PROP_MS_6"))
                        Else
                            mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_2"))
                        End If
                        mb -= mi
                    End If
                Next

                Return mb

            End If

            Return 0

        End Function

        Public Function GetPowerGeneratedOrConsumed() As Double Implements ISimulationObject.GetPowerGeneratedOrConsumed

            If GraphicObject.ObjectType <> ObjectType.MaterialStream And GraphicObject.ObjectType <> ObjectType.EnergyStream And
              GraphicObject.ObjectType <> ObjectType.OT_Adjust And GraphicObject.ObjectType <> ObjectType.OT_Spec And
              GraphicObject.ObjectType <> ObjectType.OT_Recycle And GraphicObject.ObjectType <> ObjectType.OT_EnergyRecycle Then

                Dim eb As Double = 0.0#
                Dim ebe As Double = 0.0#
                Dim mi, hi, hf As Double

                Dim iesc As List(Of ISimulationObject) = GraphicObject.InputConnectors.Where(Function(x) x.IsAttached And (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedFrom.Name)).ToList
                Dim oesc As List(Of ISimulationObject) = GraphicObject.OutputConnectors.Where(Function(x) x.IsAttached And (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedTo.Name)).ToList

                If iesc.Count + oesc.Count = 0 Then


                    Dim imsc As List(Of ISimulationObject) = GraphicObject.InputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedFrom.Name)).ToList
                    Dim omsc As List(Of ISimulationObject) = GraphicObject.OutputConnectors.Where(Function(x) x.IsAttached And Not (x.IsEnergyConnector Or x.Type = ConType.ConEn)).Select(Function(x) FlowSheet.SimulationObjects(x.AttachedConnector.AttachedTo.Name)).ToList

                    For Each ims In imsc
                        If ims.GraphicObject.Active Then
                            If DirectCast(ims, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                                mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(ims.GetPropertyValue("PROP_MS_6"))
                            Else
                                mi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_2"))
                            End If
                            hi = Convert.ToDouble(ims.GetPropertyValue("PROP_MS_7"))
                            eb -= mi * hi 'kg/s * kJ/kg = kJ/s = kW
                            'heats of formation
                            hf = DirectCast(ims, IMaterialStream).GetOverallHeatOfFormation()
                            eb -= hf
                        End If
                    Next

                    For Each oms In omsc
                        If oms.GraphicObject.Active Then
                            If DirectCast(oms, IMaterialStream).DefinedFlow = FlowSpec.Mole Then
                                mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_3")) / 1000.0 * Convert.ToDouble(oms.GetPropertyValue("PROP_MS_6"))
                            Else
                                mi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_2"))
                            End If
                            hi = Convert.ToDouble(oms.GetPropertyValue("PROP_MS_7"))
                            eb += mi * hi 'kg/s * kJ/kg = kJ/s = kW
                            'heats of formation
                            hf = DirectCast(oms, IMaterialStream).GetOverallHeatOfFormation()
                            eb += hf
                        End If
                    Next

                    Return -eb

                Else

                    For Each ies In iesc
                        If ies.GraphicObject.Active Then
                            eb -= Convert.ToDouble(ies.GetPropertyValue("PROP_ES_0"))
                        End If
                    Next

                    For Each oes In oesc
                        If oes.GraphicObject.Active Then
                            eb += Convert.ToDouble(oes.GetPropertyValue("PROP_ES_0"))
                        End If
                    Next

                    If GraphicObject.EnergyConnector.IsAttached Then
                        Dim inobj = FlowSheet.SimulationObjects(GraphicObject.EnergyConnector.AttachedConnector.AttachedFrom.Name)
                        If inobj.GraphicObject.IsEnergyStream And inobj.GraphicObject.Active Then
                            eb -= Convert.ToDouble(inobj.GetPropertyValue("PROP_ES_0"))
                        End If
                        Dim outobj = FlowSheet.SimulationObjects(GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                        If outobj.GraphicObject.IsEnergyStream And outobj.GraphicObject.Active Then
                            eb += Convert.ToDouble(outobj.GetPropertyValue("PROP_ES_0"))
                        End If
                    End If

                    Return eb

                End If

            End If

            Return 0

        End Function

        Public Overridable Function GetDynamicResidenceTime() As Double Implements ISimulationObject.GetDynamicResidenceTime
            Return Double.NaN
        End Function

        Public Overridable Function GetDynamicVolume() As Double Implements ISimulationObject.GetDynamicVolume
            Return Double.NaN
        End Function

        Public Overridable Function GetDynamicContents() As Double Implements ISimulationObject.GetDynamicContents
            Return Double.NaN
        End Function

        Public Function GetAsObject() As Object Implements ISimulationObject.GetAsObject
            Return Me
        End Function

#End Region

#Region "    IProductInformation"

        Public Overridable ReadOnly Property ProductName As String Implements IProductInformation.ProductName
            Get
                Return GetDisplayName()
            End Get
        End Property

        Public Overridable ReadOnly Property ProductDescription As String Implements IProductInformation.ProductDescription
            Get
                Return GetDisplayDescription()
            End Get
        End Property

        Public Overridable ReadOnly Property ProductAuthor As String Implements IProductInformation.ProductAuthor
            Get
                Return "Daniel Medeiros"
            End Get
        End Property

        Public Overridable ReadOnly Property ProductContactInfo As String Implements IProductInformation.ProductContactInfo
            Get
                Return "https://dwsim.inforside.com.br"
            End Get
        End Property

        Public Overridable ReadOnly Property ProductPage As String Implements IProductInformation.ProductPage
            Get
                Return "https://dwsim.inforside.com.br"
            End Get
        End Property

        Public Overridable ReadOnly Property ProductVersion As String Implements IProductInformation.ProductVersion
            Get
                Return Assembly.GetExecutingAssembly().GetName().Version.ToString()
            End Get
        End Property

        Public Overridable ReadOnly Property ProductAssembly As String Implements IProductInformation.ProductAssembly
            Get
                Return Assembly.GetExecutingAssembly().GetName().Name
            End Get
        End Property

        Public Overridable ReadOnly Property IsSource As Boolean = False Implements ISimulationObject.IsSource

        Public Overridable ReadOnly Property IsSink As Boolean = False Implements ISimulationObject.IsSink

#End Region


        Public Sub ConnectFeedMaterialStream(stream As ISimulationObject, portnumber As Integer) Implements ISimulationObject.ConnectFeedMaterialStream

            FlowSheet.ConnectObjects(stream.GraphicObject, GraphicObject, 0, portnumber)

        End Sub

        Public Sub ConnectProductMaterialStream(stream As ISimulationObject, portnumber As Integer) Implements ISimulationObject.ConnectProductMaterialStream

            FlowSheet.ConnectObjects(GraphicObject, stream.GraphicObject, portnumber, 0)

        End Sub

        Public Sub ConnectFeedEnergyStream(stream As ISimulationObject, portnumber As Integer) Implements ISimulationObject.ConnectFeedEnergyStream

            FlowSheet.ConnectObjects(stream.GraphicObject, GraphicObject, 0, portnumber)

        End Sub

        Public Sub ConnectProductEnergyStream(stream As ISimulationObject, portnumber As Integer) Implements ISimulationObject.ConnectProductEnergyStream

            FlowSheet.ConnectObjects(GraphicObject, stream.GraphicObject, 0, portnumber)

        End Sub

        Public Sub ConnectEnergyStream(stream As ISimulationObject) Implements ISimulationObject.ConnectEnergyStream

            If GraphicObject.EnergyConnector.Active Then
                FlowSheet.ConnectObjects(GraphicObject, stream.GraphicObject, 0, 0)
            Else
                Dim i As Integer = 0
                For Each con In GraphicObject.InputConnectors
                    If con.IsEnergyConnector Or con.Type = GraphicObjects.ConType.ConEn Then
                        FlowSheet.ConnectObjects(stream.GraphicObject, GraphicObject, 0, i)
                    End If
                    i += 1
                Next
                i = 0
                For Each con In GraphicObject.InputConnectors
                    If con.IsEnergyConnector Or con.Type = GraphicObjects.ConType.ConEn Then
                        FlowSheet.ConnectObjects(GraphicObject, stream.GraphicObject, i, 0)
                    End If
                    i += 1
                Next

            End If

        End Sub

        Public Function GetConnectionPortsList() As List(Of String) Implements ISimulationObject.GetConnectionPortsList

            Dim l As New List(Of String)

            Dim i As Integer = 0
            For Each con In GraphicObject.InputConnectors
                l.Add(String.Format("Inlet Port #{0}, Name: {1}, Type: {2}, Connected: {3}", i, con.ConnectorName, con.Type.ToString(), con.IsAttached))
                i += 1
            Next
            i = 0
            For Each con In GraphicObject.OutputConnectors
                l.Add(String.Format("Outlet Port #{0}, Name: {1}, Type: {2}, Connected: {3}", i, con.ConnectorName, con.Type.ToString(), con.IsAttached))
                i += 1
            Next

            If GraphicObject.EnergyConnector.Active Then
                Dim con = GraphicObject.EnergyConnector
                l.Add(String.Format("Energy Stream Port, Name: {1}, Type: {2}, Connected: {3}", i, con.ConnectorName, con.Type.ToString(), con.IsAttached))
            End If

            Return l

        End Function

        Public Function GetConnectionPortsInfo() As List(Of IConnectionPortInfo) Implements ISimulationObject.GetConnectionPortsInfo

            Dim list As New List(Of IConnectionPortInfo)

            Dim i As Integer

            If GraphicObject IsNot Nothing Then

                i = 0
                For Each con In GraphicObject.InputConnectors
                    Dim ci As New ConnectionPortInfo()
                    ci.Name = con.ConnectorName
                    ci.Index = i
                    ci.IsInput = True
                    ci.IsConnected = con.IsAttached
                    If con.IsAttached Then
                        ci.ConnectedObject = con.AttachedConnector.AttachedFrom.Owner
                    End If
                    list.Add(ci)
                    i += 1
                Next

                i = 0
                For Each con In GraphicObject.OutputConnectors
                    Dim ci As New ConnectionPortInfo()
                    ci.Name = con.ConnectorName
                    ci.Index = i
                    ci.IsOutput = True
                    ci.IsConnected = con.IsAttached
                    If con.IsAttached Then
                        ci.ConnectedObject = con.AttachedConnector.AttachedTo.Owner
                    End If
                    list.Add(ci)
                    i += 1
                Next

                If GraphicObject.EnergyConnector.Active Then
                    Dim econ = GraphicObject.EnergyConnector
                    Dim ci As New ConnectionPortInfo()
                    ci.Name = econ.ConnectorName
                    ci.Index = 0
                    ci.IsEnergyPort = True
                    ci.IsConnected = econ.IsAttached
                    If econ.IsAttached Then
                        ci.ConnectedObject = econ.AttachedConnector.AttachedTo.Owner
                    End If
                    list.Add(ci)
                End If

            End If

            Return list

        End Function

        Public Sub SetDirtyStatus(value As Boolean) Implements ISimulationObject.SetDirtyStatus
            _IsDirty = value
            SetCanUsePreviousResults(Not _IsDirty)
        End Sub

        Public Sub SetCanUsePreviousResults(value As Boolean) Implements ISimulationObject.SetCanUsePreviousResults
            _CanUsePreviousResults = value
        End Sub

        Public Overridable Sub CheckDirtyStatus() Implements ISimulationObject.CheckDirtyStatus

            SetDirtyStatus(True)
            SetCanUsePreviousResults(False)

        End Sub

        Public Overridable Sub SetPropertyPackageInstance(PP As IPropertyPackage) Implements ISimulationObject.SetPropertyPackageInstance

        End Sub

        Public Overridable Function ClearPropertyPackageInstance() As Boolean Implements ISimulationObject.ClearPropertyPackageInstance

            Return False

        End Function

    End Class

End Namespace
