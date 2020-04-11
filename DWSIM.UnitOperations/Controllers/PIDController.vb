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

Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.UnitOperations.SpecialOps.Helpers

Namespace SpecialOps

    <System.Serializable()> Public Class PIDController

        Inherits UnitOperations.SpecialOpBaseClass

        Implements Interfaces.IAdjust

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Controllers

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_PIDController

        Protected m_ManipulatedObject As SharedClasses.UnitOperations.BaseClass
        Protected m_ControlledObject As SharedClasses.UnitOperations.BaseClass
        Protected m_ReferenceObject As SharedClasses.UnitOperations.BaseClass

        Protected m_ManipulatedVariable As String = ""
        Protected m_ControlledVariable As String = ""
        Protected m_ReferenceVariable As String = ""

        Protected m_Status As String = ""

        Protected m_AdjustValue As Double = 1.0#

        Protected m_IsReferenced As Boolean = False
        Protected m_IsSimultAdjustEnabled As Boolean = False

        Protected m_StepSize As Double = 0.1
        Protected m_Tolerance As Double = 0.0001
        Protected m_MaxIterations As Integer = 10

        Protected m_ManipulatedObjectData As New SpecialOps.Helpers.SpecialOpObjectInfo
        Protected m_ControlledObjectData As New SpecialOps.Helpers.SpecialOpObjectInfo
        Protected m_ReferencedObjectData As New SpecialOps.Helpers.SpecialOpObjectInfo

        Protected m_CV_OK As Boolean = False
        Protected m_MV_OK As Boolean = False
        Protected m_RV_OK As Boolean = False

        Protected m_minVal As Nullable(Of Double) = Nothing
        Protected m_maxVal As Nullable(Of Double) = Nothing
        Protected m_initialEstimate As Nullable(Of Double) = Nothing

        Public Property Offset As Double = 0.0
        Public Property Kp As Double = 1.0
        Public Property Kd As Double = 0.0
        Public Property Ki As Double = 0.0

        Public Property CurrentError As Double = 0.0

        Public Property LastError As Double = 0.0

        Public Property PTerm As Double = 0.0

        Public Property ITerm As Double = 0.0

        Public Property DTerm As Double = 0.0

        Public Property Output As Double = 0.0

        Public Property OutputAbs As Double = 0.0

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New PIDController()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of PIDController)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Property SimultaneousAdjust() As Boolean Implements Interfaces.IAdjust.SimultaneousAdjust
            Get
                Return m_IsSimultAdjustEnabled
            End Get
            Set(ByVal value As Boolean)
                m_IsSimultAdjustEnabled = value
            End Set
        End Property

        Public Property InitialEstimate() As Nullable(Of Double)
            Get
                Return m_initialEstimate
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_initialEstimate = value
            End Set
        End Property

        Public Property MaxVal() As Nullable(Of Double)
            Get
                Return m_maxVal
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_maxVal = value
            End Set
        End Property

        Public Property MinVal() As Nullable(Of Double)
            Get
                Return m_minVal
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_minVal = value
            End Set
        End Property

        Public Property RvOk() As Boolean
            Get
                Return m_RV_OK
            End Get
            Set(ByVal value As Boolean)
                m_RV_OK = value
            End Set
        End Property

        Public Property MvOk() As Boolean
            Get
                Return m_MV_OK
            End Get
            Set(ByVal value As Boolean)
                m_MV_OK = value
            End Set
        End Property

        Public Property CvOk() As Boolean
            Get
                Return m_CV_OK
            End Get
            Set(ByVal value As Boolean)
                m_CV_OK = value
            End Set
        End Property

        Public Property ManipulatedObjectData() As Interfaces.ISpecialOpObjectInfo Implements Interfaces.IAdjust.ManipulatedObjectData
            Get
                Return Me.m_ManipulatedObjectData
            End Get
            Set(ByVal value As Interfaces.ISpecialOpObjectInfo)
                Me.m_ManipulatedObjectData = value
            End Set
        End Property

        Public Property ControlledObjectData() As Interfaces.ISpecialOpObjectInfo Implements Interfaces.IAdjust.ControlledObjectData
            Get
                Return Me.m_ControlledObjectData
            End Get
            Set(ByVal value As Interfaces.ISpecialOpObjectInfo)
                Me.m_ControlledObjectData = value
            End Set
        End Property

        Public Property ReferencedObjectData() As Interfaces.ISpecialOpObjectInfo Implements Interfaces.IAdjust.ReferencedObjectData
            Get
                Return Me.m_ReferencedObjectData
            End Get
            Set(ByVal value As Interfaces.ISpecialOpObjectInfo)
                Me.m_ReferencedObjectData = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ManipulatedObject() As SharedClasses.UnitOperations.BaseClass
            Get
                Return Me.m_ManipulatedObject
            End Get
            Set(ByVal value As SharedClasses.UnitOperations.BaseClass)
                Me.m_ManipulatedObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ControlledObject() As SharedClasses.UnitOperations.BaseClass
            Get
                Return Me.m_ControlledObject
            End Get
            Set(ByVal value As SharedClasses.UnitOperations.BaseClass)
                Me.m_ControlledObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ReferenceObject() As SharedClasses.UnitOperations.BaseClass
            Get
                Return Me.m_ReferenceObject
            End Get
            Set(ByVal value As SharedClasses.UnitOperations.BaseClass)
                Me.m_ReferenceObject = value
            End Set
        End Property

        Public Property ManipulatedVariable() As String
            Get
                Return Me.m_ManipulatedVariable
            End Get
            Set(ByVal value As String)
                Me.m_ManipulatedVariable = value
            End Set
        End Property

        Public Property ControlledVariable() As String
            Get
                Return Me.m_ControlledVariable
            End Get
            Set(ByVal value As String)
                Me.m_ControlledVariable = value
            End Set
        End Property

        Public Property ReferenceVariable() As String
            Get
                Return Me.m_ReferenceVariable
            End Get
            Set(ByVal value As String)
                Me.m_ReferenceVariable = value
            End Set
        End Property

        Public Property Status() As String
            Get
                Return Me.m_Status
            End Get
            Set(ByVal value As String)
                Me.m_Status = value
            End Set
        End Property

        Public Property AdjustValue() As Double Implements Interfaces.IAdjust.AdjustValue
            Get
                Return Me.m_AdjustValue
            End Get
            Set(ByVal value As Double)
                Me.m_AdjustValue = value
            End Set
        End Property

        Public Property Referenced() As Boolean Implements Interfaces.IAdjust.Referenced
            Get
                Return Me.m_IsReferenced
            End Get
            Set(ByVal value As Boolean)
                Me.m_IsReferenced = value
            End Set
        End Property

        Public Property StepSize() As Double
            Get
                Return Me.m_StepSize
            End Get
            Set(ByVal value As Double)
                Me.m_StepSize = value
            End Set
        End Property

        Public Property Tolerance() As Double Implements IAdjust.Tolerance
            Get
                Return Me.m_Tolerance
            End Get
            Set(ByVal value As Double)
                Me.m_Tolerance = value
            End Set
        End Property

        Public Property MaximumIterations() As Integer
            Get
                Return Me.m_MaxIterations
            End Get
            Set(ByVal value As Integer)
                Me.m_MaxIterations = value
            End Set
        End Property

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            MyBase.LoadData(data)

            Dim xel As XElement

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ManipulatedObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ManipulatedObjectData
                    .ID = xel.@ID
                    .Name = xel.@Name
                    .PropertyName = xel.@Property
                    .ObjectType = xel.@ObjectType
                    .Units = xel.@PropertyUnits
                    .UnitsType = [Enum].Parse(.UnitsType.GetType, xel.@PropertyUnitsType)
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ControlledObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ControlledObjectData
                    .ID = xel.@ID
                    .Name = xel.@Name
                    .PropertyName = xel.@Property
                    .ObjectType = xel.@ObjectType
                    .Units = xel.@PropertyUnits
                    .UnitsType = [Enum].Parse(.UnitsType.GetType, xel.@PropertyUnitsType)
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ReferencedObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ReferencedObjectData
                    .ID = xel.@ID
                    .Name = xel.@Name
                    .PropertyName = xel.@Property
                    .ObjectType = xel.@ObjectType
                    .Units = xel.@PropertyUnits
                    .UnitsType = [Enum].Parse(.UnitsType.GetType, xel.@PropertyUnitsType)
                End With

            End If
            Return True
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            If m_ManipulatedObjectData Is Nothing Then m_ManipulatedObjectData = New SpecialOpObjectInfo()
            If m_ControlledObjectData Is Nothing Then m_ControlledObjectData = New SpecialOpObjectInfo()
            If m_ReferencedObjectData Is Nothing Then m_ReferencedObjectData = New SpecialOpObjectInfo()

            If m_ManipulatedObjectData.ObjectType = Nothing Then m_ManipulatedObjectData.ObjectType = ""
            If m_ControlledObjectData.ObjectType = Nothing Then m_ControlledObjectData.ObjectType = ""
            If m_ReferencedObjectData.ObjectType = Nothing Then m_ReferencedObjectData.ObjectType = ""

            With elements
                .Add(New XElement("ManipulatedObjectData", New XAttribute("ID", m_ManipulatedObjectData.ID),
                                  New XAttribute("Name", m_ManipulatedObjectData.Name),
                                  New XAttribute("Property", m_ManipulatedObjectData.PropertyName),
                                  New XAttribute("ObjectType", m_ManipulatedObjectData.ObjectType),
                                  New XAttribute("PropertyUnitsType", m_ManipulatedObjectData.UnitsType),
                                  New XAttribute("PropertyUnits", m_ManipulatedObjectData.Units)))
                .Add(New XElement("ControlledObjectData", New XAttribute("ID", m_ControlledObjectData.ID),
                                  New XAttribute("Name", m_ControlledObjectData.Name),
                                  New XAttribute("Property", m_ControlledObjectData.PropertyName),
                                  New XAttribute("ObjectType", m_ControlledObjectData.ObjectType),
                                  New XAttribute("PropertyUnitsType", m_ControlledObjectData.UnitsType),
                                  New XAttribute("PropertyUnits", m_ControlledObjectData.Units)))
                .Add(New XElement("ReferencedObjectData", New XAttribute("ID", m_ReferencedObjectData.ID),
                                  New XAttribute("Name", m_ReferencedObjectData.Name),
                                  New XAttribute("Property", m_ReferencedObjectData.PropertyName),
                                  New XAttribute("ObjectType", m_ReferencedObjectData.ObjectType),
                                  New XAttribute("PropertyUnitsType", m_ReferencedObjectData.UnitsType),
                                  New XAttribute("PropertyUnits", m_ReferencedObjectData.Units)))
            End With

            Return elements

        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            m_ManipulatedObjectData = New SpecialOps.Helpers.SpecialOpObjectInfo
            m_ControlledObjectData = New SpecialOps.Helpers.SpecialOpObjectInfo
            m_ReferencedObjectData = New SpecialOps.Helpers.SpecialOpObjectInfo
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                Select Case prop
                    Case "MinVal"
                        Return MinVal.GetValueOrDefault
                    Case "MaxVal"
                        Return MaxVal.GetValueOrDefault
                    Case "AdjustValue"
                        Return AdjustValue
                    Case "Tolerance"
                        Return Tolerance
                    Case "StepSize"
                        Return StepSize
                    Case "MaximumIterations"
                        Return MaximumIterations
                    Case Else
                        Return Nothing
                End Select
            End If
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            proplist.Add("MinVal")
            proplist.Add("MaxVal")
            proplist.Add("AdjustValue")
            proplist.Add("Tolerance")
            proplist.Add("StepSize")
            proplist.Add("MaximumIterations")
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            Select Case prop
                Case "MinVal"
                    MinVal = propval
                Case "MaxVal"
                    MaxVal = propval
                Case "AdjustValue"
                    AdjustValue = propval
                Case "Tolerance"
                    Tolerance = propval
                Case "StepSize"
                    StepSize = propval
                Case "MaximumIterations"
                    MaximumIterations = propval
            End Select
            Return True
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then
                Return u0
            Else
                Return ""
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_PIDController With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_PIDController With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.control_panel1
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("PID_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("PID_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Sub Reset()

            PTerm = 0.0
            ITerm = 0.0
            DTerm = 0.0

            LastError = 0.0

            Output = 0.0

        End Sub

        Public Overrides Sub Calculate(Optional args As Object = Nothing)

            ' Calculates PID value for given reference feedback
            ' u(t) = K_p e(t) + K_i \int_{0}^{t} e(t)dt + K_d {de}/{dt}

            Dim integratorID = FlowSheet.DynamicsManager.ScheduleList(FlowSheet.DynamicsManager.CurrentSchedule).CurrentIntegrator
            Dim integrator = FlowSheet.DynamicsManager.IntegratorList(integratorID)

            Dim timestep = integrator.IntegrationStep.TotalSeconds

            Dim ControlledObject = GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = ControlledObjectData.ID).SingleOrDefault

            Dim ManipulatedObject = GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = ManipulatedObjectData.ID).SingleOrDefault

            Dim CurrentValue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(ControlledObjectData.Units, ControlledObject.GetPropertyValue(ControlledObjectData.PropertyName))

            Dim CurrentManipulatedValue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(ManipulatedObjectData.Units, ManipulatedObject.GetPropertyValue(ManipulatedObjectData.PropertyName))

            LastError = CurrentError

            CurrentError = (AdjustValue - CurrentValue) / Math.Abs(AdjustValue)

            Dim delta_error = CurrentError - LastError

            PTerm = Kp * CurrentError

            ITerm += CurrentError * timestep

            DTerm = 0.0

            If LastError > 0 Then DTerm = delta_error / timestep

            Output = PTerm + Ki * ITerm + Kd * DTerm

            OutputAbs = (1.0 - Output) * Math.Abs(AdjustValue)

            Dim OutputValue = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(ManipulatedObjectData.Units, OutputAbs)

            ManipulatedObject.SetPropertyValue(ManipulatedObjectData.PropertyName, OutputValue)

        End Sub

    End Class

End Namespace




