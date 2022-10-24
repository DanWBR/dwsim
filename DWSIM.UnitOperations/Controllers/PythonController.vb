'    Copyright 2022 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Drawing.SkiaSharp
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics
Imports DWSIM.UnitOperations.SpecialOps.Helpers
Imports Microsoft.Scripting.Hosting
Imports OxyPlot
Imports OxyPlot.Axes
Imports OxyPlot.Series

Namespace SpecialOps

    <System.Serializable()> Public Class PythonController

        Inherits UnitOperations.SpecialOpBaseClass

        Implements Interfaces.IAdjust

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Controllers

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_PythonController

        <NonSerialized> <Xml.Serialization.XmlIgnore> Private engine As ScriptEngine

        <NonSerialized> <Xml.Serialization.XmlIgnore> Private scope As ScriptScope

        Public Property ResetRequested As Boolean = False

        Public Property PythonScript As String = ""

        Public Property Output As Double = 0.0

        Public Property OutputAbs As Double = 0.0

        Public Property PVHistory As New List(Of Double)

        Public Property MVHistory As New List(Of Double)

        Public Property SPHistory As New List(Of Double)

        Public BaseSP As Nullable(Of Double)

        Public Property Active As Boolean = True

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
            Dim obj As ICustomXMLSerialization = New PythonController()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of PythonController)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Property SimultaneousAdjust As Boolean Implements Interfaces.IAdjust.SimultaneousAdjust

        Public Property InitialEstimate As Nullable(Of Double)

        Public Property MaxVal As Nullable(Of Double)

        Public Property MinVal As Nullable(Of Double)

        Public Property RvOk As Boolean

        Public Property MvOk As Boolean

        Public Property CvOk As Boolean

        Public Property ManipulatedObjectData As Interfaces.ISpecialOpObjectInfo Implements Interfaces.IAdjust.ManipulatedObjectData

        Public Property ControlledObjectData As Interfaces.ISpecialOpObjectInfo Implements Interfaces.IAdjust.ControlledObjectData

        Public Property ReferencedObjectData As Interfaces.ISpecialOpObjectInfo Implements Interfaces.IAdjust.ReferencedObjectData

        <Xml.Serialization.XmlIgnore()> Public Property ManipulatedObject As SharedClasses.UnitOperations.BaseClass

        <Xml.Serialization.XmlIgnore()> Public Property ControlledObject As SharedClasses.UnitOperations.BaseClass

        <Xml.Serialization.XmlIgnore()> Public Property ReferenceObject As SharedClasses.UnitOperations.BaseClass

        Public Property ManipulatedVariable As String

        Public Property ControlledVariable As String

        Public Property ReferenceVariable As String

        Public Property Status As String

        Public Property AdjustValue As Double Implements Interfaces.IAdjust.AdjustValue

        Public Property Referenced As Boolean Implements Interfaces.IAdjust.Referenced

        Public Property StepSize As Double

        Public Property Tolerance As Double Implements IAdjust.Tolerance

        Public Property MaximumIterations As Integer

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            MyBase.LoadData(data)

            Dim xel As XElement

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ManipulatedObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With ManipulatedObjectData
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

                With ControlledObjectData
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

                With ReferencedObjectData
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

            If ManipulatedObjectData Is Nothing Then ManipulatedObjectData = New SpecialOpObjectInfo()
            If ControlledObjectData Is Nothing Then ControlledObjectData = New SpecialOpObjectInfo()
            If ReferencedObjectData Is Nothing Then ReferencedObjectData = New SpecialOpObjectInfo()

            If ManipulatedObjectData.ObjectType = Nothing Then ManipulatedObjectData.ObjectType = ""
            If ControlledObjectData.ObjectType = Nothing Then ControlledObjectData.ObjectType = ""
            If ReferencedObjectData.ObjectType = Nothing Then ReferencedObjectData.ObjectType = ""

            With elements
                .Add(New XElement("ManipulatedObjectData", New XAttribute("ID", ManipulatedObjectData.ID),
                                  New XAttribute("Name", ManipulatedObjectData.Name),
                                  New XAttribute("Property", ManipulatedObjectData.PropertyName),
                                  New XAttribute("ObjectType", ManipulatedObjectData.ObjectType),
                                  New XAttribute("PropertyUnitsType", ManipulatedObjectData.UnitsType),
                                  New XAttribute("PropertyUnits", ManipulatedObjectData.Units)))
                .Add(New XElement("ControlledObjectData", New XAttribute("ID", ControlledObjectData.ID),
                                  New XAttribute("Name", ControlledObjectData.Name),
                                  New XAttribute("Property", ControlledObjectData.PropertyName),
                                  New XAttribute("ObjectType", ControlledObjectData.ObjectType),
                                  New XAttribute("PropertyUnitsType", ControlledObjectData.UnitsType),
                                  New XAttribute("PropertyUnits", ControlledObjectData.Units)))
                .Add(New XElement("ReferencedObjectData", New XAttribute("ID", ReferencedObjectData.ID),
                                  New XAttribute("Name", ReferencedObjectData.Name),
                                  New XAttribute("Property", ReferencedObjectData.PropertyName),
                                  New XAttribute("ObjectType", ReferencedObjectData.ObjectType),
                                  New XAttribute("PropertyUnitsType", ReferencedObjectData.UnitsType),
                                  New XAttribute("PropertyUnits", ReferencedObjectData.Units)))
            End With

            Return elements

        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            ManipulatedObjectData = New SpecialOps.Helpers.SpecialOpObjectInfo
            ControlledObjectData = New SpecialOps.Helpers.SpecialOpObjectInfo
            ReferencedObjectData = New SpecialOps.Helpers.SpecialOpObjectInfo
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                Select Case prop
                    Case "Active"
                        Return Active
                    Case "SetPointAbs"
                        Return AdjustValue
                    Case "Output"
                        Return Output
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
            proplist.Add("SetPointAbs")
            proplist.Add("Output")
            proplist.Add("OutputAbs")
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            Select Case prop
                Case "Active"
                    Active = propval
                Case "SetPointAbs"
                    AdjustValue = propval
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
                f = New EditingForm_PythonController With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_PythonController With {.SimObject = Me}
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
            Return My.Resources.typewriter
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return "IronPython Script Custom Controller"
        End Function

        Public Overrides Function GetDisplayName() As String
            Return "Python Controller"
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

        Public Overrides Sub Calculate(Optional args As Object = Nothing)

            Dim integratorID = FlowSheet.DynamicsManager.ScheduleList(FlowSheet.DynamicsManager.CurrentSchedule).CurrentIntegrator
            Dim integrator = FlowSheet.DynamicsManager.IntegratorList(integratorID)

            Dim timestep = integrator.IntegrationStep.TotalSeconds

            Dim ControlledObject = GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = ControlledObjectData.ID).SingleOrDefault

            Dim ManipulatedObject = GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = ManipulatedObjectData.ID).SingleOrDefault

            Dim CurrentValue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(ControlledObjectData.Units, ControlledObject.GetPropertyValue(ControlledObjectData.PropertyName))

            Dim CurrentManipulatedValue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(ManipulatedObjectData.Units, ManipulatedObject.GetPropertyValue(ManipulatedObjectData.PropertyName))

            engine = IronPython.Hosting.Python.CreateEngine()
            engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
            scope = engine.CreateScope()
            scope.SetVariable("Flowsheet", FlowSheet)
            scope.SetVariable("Me", Me)
            scope.SetVariable("This", Me)
            scope.SetVariable("PV", CurrentValue)

            Dim source As Microsoft.Scripting.Hosting.ScriptSource = Me.engine.CreateScriptSourceFromString(PythonScript, Microsoft.Scripting.SourceCodeKind.Statements)
            Try
                Me.ErrorMessage = ""
                source.Execute(Me.scope)
                Output = scope.GetVariable("MV")
                SetPoint = scope.GetVariable("SP")
            Catch ex As Exception
                Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
                Me.ErrorMessage = ops.FormatException(ex).ToString
                Me.DeCalculate()
                engine = Nothing
                scope = Nothing
                source = Nothing
                Throw New Exception(Me.ErrorMessage, ex)
            Finally
                engine = Nothing
                scope = Nothing
                source = Nothing
            End Try

            SPValue = AdjustValue

            PVValue = CurrentValue

            If BaseSP Is Nothing Then BaseSP = Math.Abs(SetPoint)

            SPHistory.Add(AdjustValue / BaseSP)

            PVHistory.Add(CurrentValue / BaseSP)

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




