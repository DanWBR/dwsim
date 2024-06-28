Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations
Imports Python.Runtime
Imports SkiaSharp

Namespace UnitOperations.Auxiliary

    Public Class PEMFuelCellModelParameter

        Implements ICustomXMLSerialization

        Public Property Name As String = ""

        Public Property Description As String = ""

        Public Property Value As Double

        Public Property Units As String = ""

        Public Property TitleX As String = ""

        Public Property TitleY As String = ""

        Public Property ValuesX As List(Of Double)

        Public Property ValuesY As List(Of Double)

        Public Property UnitsX As String = ""

        Public Property UnitsY As String = ""

        Public Sub New()

        End Sub

        Public Sub New(_name As String, _description As String, _value As Double, _units As String)

            Name = _name
            Value = _value
            Units = _units
            Description = _description

        End Sub

        Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me)
        End Function

        Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
        End Function

    End Class

End Namespace

Namespace UnitOperations

    Public MustInherit Class PEMFuelCellUnitOpBase

        Inherits DWSIM.UnitOperations.UnitOperations.UnitOpBaseClass

        Implements DWSIM.Interfaces.IExternalUnitOperation

        Private ImagePath As String = ""

        Private Image As SKImage

        <Xml.Serialization.XmlIgnore> Public f As EditingForm_OPEMFC

        Public Property OPEMPath As String = "main\python-3.9.4.amd64"

        Public Property HTMLreport As String = ""
        Public Property CSVreport As String = ""
        Public Property OPEMreport As String = ""


        Public Property InputParameters As Dictionary(Of String, Auxiliary.PEMFuelCellModelParameter) = New Dictionary(Of String, Auxiliary.PEMFuelCellModelParameter)()

        Public Property OutputParameters As Dictionary(Of String, Auxiliary.PEMFuelCellModelParameter) = New Dictionary(Of String, Auxiliary.PEMFuelCellModelParameter)()

        Public Overrides ReadOnly Property IsSource As Boolean
            Get
                Return True
            End Get
        End Property


        Public Overrides Property ComponentName As String = GetDisplayName()

        Public Overrides Property ComponentDescription As String = GetDisplayDescription()

        Private ReadOnly Property IExternalUnitOperation_Name As String = GetDisplayName() Implements IExternalUnitOperation.Name

        Public MustOverride Property Prefix As String Implements IExternalUnitOperation.Prefix

        Public ReadOnly Property Description As String = GetDisplayDescription() Implements IExternalUnitOperation.Description

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.CleanPowerSources

        Public Overrides ReadOnly Property MobileCompatible As Boolean = False

        Public MustOverride Function ReturnInstance(typename As String) As Object Implements IExternalUnitOperation.ReturnInstance

        Public MustOverride Sub AddDefaultInputParameters()

        Public Sub Draw(g As Object) Implements IExternalUnitOperation.Draw

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            If Image Is Nothing Then

                ImagePath = SharedClasses.Utility.GetTempFileName()
                DirectCast(GetIconBitmap(), System.Drawing.Bitmap).Save(ImagePath)

                Using streamBG = New FileStream(ImagePath, FileMode.Open)
                    Using bitmap = SKBitmap.Decode(streamBG)
                        Image = SKImage.FromBitmap(bitmap)
                    End Using
                End Using

                Try
                    File.Delete(ImagePath)
                Catch ex As Exception
                End Try

            End If

            Using p As New SKPaint With {.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                canvas.DrawImage(Image, New SKRect(GraphicObject.X, GraphicObject.Y, GraphicObject.X + GraphicObject.Width, GraphicObject.Y + GraphicObject.Height), p)
            End Using

        End Sub

        Public Sub CreateConnectors() Implements IExternalUnitOperation.CreateConnectors

            Dim w, h, x, y As Double
            w = GraphicObject.Width
            h = GraphicObject.Height
            x = GraphicObject.X
            y = GraphicObject.Y

            Dim myIC1 As New ConnectionPoint

            myIC1.Position = New Point(x, y + h / 3)
            myIC1.Type = ConType.ConIn
            myIC1.Direction = ConDir.Right

            Dim myIC2 As New ConnectionPoint

            myIC2.Position = New Point(x, y + 2 * h / 3)
            myIC2.Type = ConType.ConIn
            myIC2.Direction = ConDir.Right

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(x + w, y + h / 2)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(x + w / 2, y + h)
            myOC2.Type = ConType.ConOut
            myOC2.Direction = ConDir.Down
            myOC2.Type = ConType.ConEn

            With GraphicObject.InputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x, y + h / 2)
                    .Add(myIC2)
                ElseIf .Count = 2 Then
                    .Item(0).Position = New Point(x, y + h / 3)
                    .Item(1).Position = New Point(x, y + 2 * h / 3)
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                End If
                .Item(0).ConnectorName = "Hydrogen-Rich Inlet"
                .Item(1).ConnectorName = "Oxygen-Rich Inlet"
            End With

            With GraphicObject.OutputConnectors
                If .Count = 2 Then
                    .Item(0).Position = New Point(x + w, y + h / 2)
                    .Item(1).Position = New Point(x + w / 2, y + h)
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                End If
                .Item(0).ConnectorName = "Inerts Outlet"
                .Item(1).ConnectorName = "Power Outlet"
            End With

            Me.GraphicObject.EnergyConnector.Active = False

        End Sub

        Public Sub New(ByVal Name As String, ByVal Description As String)

            MyBase.CreateNew()
            Me.ComponentName = Name
            Me.ComponentDescription = Description

        End Sub

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub PerformPostCalcValidation()

        End Sub

        Private Sub CallSolverIfNeeded()
            If GlobalSettings.Settings.CallSolverOnEditorPropertyChanged Then
                FlowSheet.RequestCalculation()
            End If
        End Sub

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Try

                InputParameters = New Dictionary(Of String, Auxiliary.PEMFuelCellModelParameter)()

                For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InputParameters").SingleOrDefault.Elements.ToList
                    Dim par As New Auxiliary.PEMFuelCellModelParameter()
                    par.LoadData(xel.Elements.ToList())
                    InputParameters.Add(par.Name, par)
                Next

                OutputParameters = New Dictionary(Of String, Auxiliary.PEMFuelCellModelParameter)()

                For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "OutputParameters").SingleOrDefault.Elements.ToList
                    Dim par As New Auxiliary.PEMFuelCellModelParameter()
                    par.LoadData(xel.Elements.ToList())
                    OutputParameters.Add(par.Name, par)
                Next

            Catch ex As Exception

                AddDefaultInputParameters()

            End Try

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("InputParameters"))
                For Each kvp In InputParameters
                    .Item(.Count - 1).Add(New XElement("InputParameter", kvp.Value.SaveData()))
                Next
            End With

            With elements
                .Add(New XElement("OutputParameters"))
                For Each kvp In OutputParameters
                    .Item(.Count - 1).Add(New XElement("OutputParameter", kvp.Value.SaveData()))
                Next
            End With

            Return elements

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_OPEMFC With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_OPEMFC With {.SimObject = Me}
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
                    If f.InvokeRequired Then f.BeginInvoke(Sub() f.UpdateInfo()) Else f.UpdateInfo()
                End If
            End If

        End Sub

        Public Overrides Sub CloseEditForm()

            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If

        End Sub

        Public Function ToList(pythonlist As Object) As List(Of Double)

            Using Py.GIL

                Dim list As New List(Of Double)

                For i As Integer = 0 To pythonlist.Length - 1
                    list.Add(pythonlist(i).ToString().ToDoubleFromInvariant())
                Next

                Return list

            End Using

        End Function

        Public MustOverride Sub PopulateEditorPanel(container As Object) Implements IExternalUnitOperation.PopulateEditorPanel

        Public Overrides Function GetProperties(proptype As PropertyType) As String()

            Select Case proptype
                Case PropertyType.ALL, PropertyType.RW, PropertyType.RO
                    Dim arr = InputParameters.Select(Function(p) p.Value.Name).ToList()
                    arr.AddRange(OutputParameters.Select(Function(p) p.Value.Name))
                    Return arr.ToArray()
                Case Else
                    Return InputParameters.Select(Function(p) p.Value.Name).ToArray()
            End Select

        End Function

        Public Overrides Function GetPropertyValue(prop As String, Optional su As IUnitsOfMeasure = Nothing) As Object

            If InputParameters.ContainsKey(prop) Then
                Return InputParameters(prop).Value.ConvertToSI(InputParameters(prop).Units)
            ElseIf OutputParameters.ContainsKey(prop) Then
                Return OutputParameters(prop).Value.ConvertToSI(OutputParameters(prop).Units)
            Else
                Return 0.0
            End If

        End Function

        Public Overrides Function GetPropertyUnit(prop As String, Optional su As IUnitsOfMeasure = Nothing) As String

            If InputParameters.ContainsKey(prop) Then
                Return InputParameters(prop).Units
            ElseIf OutputParameters.ContainsKey(prop) Then
                Return OutputParameters(prop).Units
            Else
                Return ""
            End If

        End Function

        Public Overrides Function SetPropertyValue(prop As String, propval As Object, Optional su As IUnitsOfMeasure = Nothing) As Boolean

            If InputParameters.ContainsKey(prop) Then
                InputParameters(prop).Value = propval
                Return True
            Else
                Return False
            End If

        End Function

    End Class

End Namespace

