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
Imports OxyPlot
Imports OxyPlot.Axes
Imports OxyPlot.Series

Namespace SpecialOps

    <System.Serializable()> Public Class PIDController

        Inherits UnitOperations.SpecialOpBaseClass

        Implements Interfaces.IAdjust, IControllableObject

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Controllers

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_PIDController

        <Xml.Serialization.XmlIgnore> Public Property ControlPanel As Object Implements IControllableObject.ControlPanel

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

        Public Property Kp As Double = 10.0

        Public Property Kd As Double = 2.0

        Public Property Ki As Double = 2.0

        Public Property WindupGuard As Double = 20.0

        Public Property CurrentError As Double = 0.0

        Public Property LastError As Double = 0.0

        Public Property CumulativeError As Double = 0.0

        Public Property PTerm As Double = 0.0

        Public Property ITerm As Double = 0.0

        Public Property DTerm As Double = 0.0

        Public Property Output As Double = 0.0

        Public Property OutputAbs As Double = 0.0

        Public Property PVHistory As New List(Of Double)

        Public Property MVHistory As New List(Of Double)

        Public Property SPHistory As New List(Of Double)

        Public BaseSP As Nullable(Of Double)

        Public Property Active As Boolean = True

        Public Property ManualOverride As Boolean = False

        Public Property ReverseActing As Boolean = False

        Public Property OutputMin As Double = -1000.0

        Public Property OutputMax As Double = 1000.0

        Public Property PVValue As Double = 0.0

        Public Property SPValue As Double = 0.0

        Public Property MVValue As Double = 0.0

        Public Property SetPoint As Double
            Get
                Return AdjustValue
            End Get
            Set(value As Double)
                AdjustValue = value
            End Set
        End Property

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

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
                    Case "ManualOverride"
                        Return ManualOverride
                    Case "Active"
                        Return Active
                    Case "LastError"
                        Return LastError
                    Case "CurrentError"
                        Return CurrentError
                    Case "CumulativeError"
                        Return CumulativeError
                    Case "SetPointAbs"
                        Return AdjustValue
                    Case "Kp"
                        Return Kp
                    Case "Ki"
                        Return Ki
                    Case "Kd"
                        Return Kd
                    Case "Output"
                        Return Output
                    Case "OutputMin"
                        Return OutputMin
                    Case "OutputMax"
                        Return OutputMax
                    Case "OutputAbs"
                        Return OutputAbs
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
            proplist.Add("Active")
            proplist.Add("ManualOverride")
            proplist.Add("LastError")
            proplist.Add("CurrentError")
            proplist.Add("CumulativeError")
            proplist.Add("SetPointAbs")
            proplist.Add("Kp")
            proplist.Add("Ki")
            proplist.Add("Kd")
            proplist.Add("Output")
            proplist.Add("OutputMin")
            proplist.Add("OutputMax")
            proplist.Add("OutputAbs")
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            Select Case prop
                Case "ManualOverride"
                    ManualOverride = propval
                Case "Active"
                    Active = propval
                Case "SetPointAbs"
                    AdjustValue = propval
                Case "OutputMin"
                    OutputMin = propval
                Case "OutputMax"
                    OutputMax = propval
                Case "Kp"
                    Kp = propval
                Case "Ki"
                    Ki = propval
                Case "Kd"
                    Kd = propval
                Case Else
                    Return False
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

        Public Overrides Function GetEditingForm() As Form
            If f Is Nothing Then
                f = New EditingForm_PIDController With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Return f
            Else
                If f.IsDisposed Then
                    f = New EditingForm_PIDController With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Return f
                Else
                    Return Nothing
                End If
            End If
        End Function

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

            CumulativeError = 0.0

            PVHistory.Clear()
            MVHistory.Clear()
            SPHistory.Clear()

            BaseSP = Nothing

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

            SPValue = AdjustValue

            PVValue = CurrentValue

            If BaseSP Is Nothing Then BaseSP = Math.Abs(AdjustValue)

            SPHistory.Add(AdjustValue / BaseSP)

            PVHistory.Add(CurrentValue / BaseSP)

            LastError = CurrentError

            CurrentError = (CurrentValue - AdjustValue) / BaseSP

            CumulativeError += Math.Abs(CurrentError)

            Dim delta_error = CurrentError - LastError

            PTerm = Kp * CurrentError

            ITerm += CurrentError * timestep

            If ITerm < -WindupGuard Then
                ITerm = -WindupGuard
            ElseIf ITerm > WindupGuard Then
                ITerm = WindupGuard
            End If

            DTerm = 0.0

            If Math.Abs(LastError) > 0.0 Then DTerm = delta_error / timestep

            If Not ManualOverride Then

                Output = PTerm + Ki * ITerm + Kd * DTerm + Offset / BaseSP

                If Not ReverseActing Then
                    OutputAbs = (1.0 - Output) * BaseSP
                Else
                    OutputAbs = (1.0 + Output) * BaseSP
                End If

                If OutputAbs > OutputMax Then OutputAbs = OutputMax

                If OutputAbs < OutputMin Then OutputAbs = OutputMin

                MVValue = SystemsOfUnits.Converter.ConvertToSI(ManipulatedObjectData.Units, OutputAbs)

            Else

                OutputAbs = SystemsOfUnits.Converter.ConvertFromSI(ManipulatedObjectData.Units, MVValue)

                If Not ReverseActing Then
                    Output = 1.0 - OutputAbs / BaseSP
                Else
                    Output = OutputAbs / BaseSP - 1.0
                End If

            End If

            MVHistory.Add(Output)

            ManipulatedObject.SetPropertyValue(ManipulatedObjectData.PropertyName, MVValue)

        End Sub

        Public Overrides Function GetChartModel(name As String) As Object

            Dim model = New PlotModel() With {.Subtitle = name, .Title = GraphicObject.Tag}

            Dim xavals = New List(Of Double)
            For i = 0 To PVHistory.Count - 1
                xavals.Add(i)
            Next

            model.TitleFontSize = 12
            model.SubtitleFontSize = 10

            model.Axes.Add(New LinearAxis() With {
                .MajorGridlineStyle = LineStyle.Dash,
                .MinorGridlineStyle = LineStyle.Dot,
                .Position = AxisPosition.Bottom,
                .FontSize = 10,
                .Title = "Step"
            })

            model.Axes.Add(New LinearAxis() With {
                .MajorGridlineStyle = LineStyle.Dash,
                .MinorGridlineStyle = LineStyle.Dot,
                .Position = AxisPosition.Left,
                .FontSize = 10,
                .Title = "SP/PV",
                .Key = "0"
            })

            model.Axes.Add(New LinearAxis() With {
                .MajorGridlineStyle = LineStyle.Dash,
                .MinorGridlineStyle = LineStyle.Dot,
                .Position = AxisPosition.Right,
                .FontSize = 10,
                .Title = "MV",
                .Key = "1"
            })

            model.LegendFontSize = 10
            model.LegendPlacement = LegendPlacement.Outside
            model.LegendOrientation = LegendOrientation.Horizontal
            model.LegendPosition = LegendPosition.BottomCenter
            model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView

            model.AddLineSeries(xavals, PVHistory, "PV")
            model.AddLineSeries(xavals, SPHistory, "SP")
            model.AddLineSeries(xavals, MVHistory, "MV")

            DirectCast(model.Series.Item(2), LineSeries).YAxisKey = "1"

            Return model

        End Function

        Public Overrides Function GetChartModelNames() As List(Of String)
            Return New List(Of String)({"History"})
        End Function

    End Class

End Namespace




