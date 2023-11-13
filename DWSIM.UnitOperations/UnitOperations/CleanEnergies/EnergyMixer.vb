
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.Streams
Imports SkiaSharp

Namespace UnitOperations

    <System.Serializable()> Public Class EnergyMixer

        Inherits DWSIM.UnitOperations.UnitOperations.UnitOpBaseClass

        Implements DWSIM.Interfaces.IExternalUnitOperation

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_Emixer

        Private _name = "Energy Mixer"
        Private _desc = "Energy Stream Mixer"

        Public Enum OpMode
            SplitRatios = 0
            StreamEnergyFlowSpec = 1
        End Enum

        Public Overrides Function GetDisplayName() As String
            Return _name
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return _desc
        End Function

        Public Overrides Function GetPreferredGraphicObjectHeight() As Double
            Return 20.0
        End Function

        Public Overrides Function GetPreferredGraphicObjectWidth() As Double
            Return 20.0
        End Function

        Public Overrides Property ComponentName As String = _name

        Public Overrides Property ComponentDescription As String = _desc

        Private ReadOnly Property IExternalUnitOperation_Name As String = _name Implements IExternalUnitOperation.Name

        Public ReadOnly Property Prefix As String = "EMIX-" Implements IExternalUnitOperation.Prefix

        Public ReadOnly Property Description As String = _desc Implements IExternalUnitOperation.Description

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.MixersSplitters

        Public Overrides ReadOnly Property MobileCompatible As Boolean = False

        Public Function ReturnInstance(typename As String) As Object Implements IExternalUnitOperation.ReturnInstance

            Return New EnergyMixer()

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_Emixer With {.MixerObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Emixer With {.MixerObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Function GetEditingForm() As Form
            If f Is Nothing Then
                f = New EditingForm_Emixer With {.MixerObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Return f
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Emixer With {.MixerObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Return f
                Else
                    Return Nothing
                End If
            End If
        End Function

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    If f.InvokeRequired Then f.BeginInvoke(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object

            Return My.Resources.uo_enmixer_32

        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Sub Draw(g As Object) Implements IExternalUnitOperation.Draw

            Dim X = GraphicObject.X
            Dim Y = GraphicObject.Y
            Dim Height = GraphicObject.Height
            Dim Width = GraphicObject.Width

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim rect As New SKRect(X, Y, X + Width, X + Height)

            Dim gp As New SKPath()

            gp.MoveTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + 0.5 * Height))
            gp.LineTo(Convert.ToInt32(X + 0.5 * Width), Convert.ToInt32(Y))
            gp.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y))
            gp.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + Height))
            gp.LineTo(Convert.ToInt32(X + 0.5 * Width), Convert.ToInt32(Y + Height))
            gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + 0.5 * Height))

            gp.Close()


            Select Case GraphicObject.DrawMode

                Case 0, 2

                    'default

                    Dim gradPen As New SKPaint()
                    With gradPen
                        .Color = GraphicObject.LineColor.WithAlpha(50)
                        .StrokeWidth = GraphicObject.LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, gradPen)

                    Dim myPen As New SKPaint()
                    With myPen
                        .Color = GraphicObject.LineColor
                        .StrokeWidth = GraphicObject.LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, myPen)

                Case 1

                    'b/w

                    Dim myPen As New SKPaint()

                    With myPen
                        .Color = SKColors.Black
                        .StrokeWidth = GraphicObject.LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, myPen)

            End Select

        End Sub

        Public Sub CreateConnectors() Implements IExternalUnitOperation.CreateConnectors

            Me.GraphicObject.EnergyConnector.Active = False

            Dim myIC1 As New ConnectionPoint
            myIC1.Position = New Point(GraphicObject.X, GraphicObject.Y + 0.0 * GraphicObject.Height)
            myIC1.Type = ConType.ConEn

            Dim myIC2 As New ConnectionPoint
            myIC2.Position = New Point(GraphicObject.X, GraphicObject.Y + 0.2 * GraphicObject.Height)
            myIC2.Type = ConType.ConEn

            Dim myIC3 As New ConnectionPoint
            myIC3.Position = New Point(GraphicObject.X, GraphicObject.Y + 0.4 * GraphicObject.Height)
            myIC3.Type = ConType.ConEn

            Dim myIC4 As New ConnectionPoint
            myIC4.Position = New Point(GraphicObject.X, GraphicObject.Y + 0.6 * GraphicObject.Height)
            myIC4.Type = ConType.ConEn

            Dim myIC5 As New ConnectionPoint
            myIC5.Position = New Point(GraphicObject.X, GraphicObject.Y + 0.8 * GraphicObject.Height)
            myIC5.Type = ConType.ConEn

            Dim myIC6 As New ConnectionPoint
            myIC6.Position = New Point(GraphicObject.X, GraphicObject.Y + 1.0 * GraphicObject.Height)
            myIC6.Type = ConType.ConEn

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(GraphicObject.X + GraphicObject.Width, GraphicObject.Y + 0.5 * GraphicObject.Height)
            myOC1.Type = ConType.ConEn

            With GraphicObject.InputConnectors

                If .Count = 6 Then
                    .Item(0).Position = New Point(GraphicObject.X, GraphicObject.Y)
                    .Item(1).Position = New Point(GraphicObject.X, GraphicObject.Y + 0.2 * GraphicObject.Height)
                    .Item(2).Position = New Point(GraphicObject.X, GraphicObject.Y + 0.4 * GraphicObject.Height)
                    .Item(3).Position = New Point(GraphicObject.X, GraphicObject.Y + 0.6 * GraphicObject.Height)
                    .Item(4).Position = New Point(GraphicObject.X, GraphicObject.Y + 0.8 * GraphicObject.Height)
                    .Item(5).Position = New Point(GraphicObject.X, GraphicObject.Y + 1.0 * GraphicObject.Height)
                Else
                    .Clear()
                    .Add(myIC1)
                    .Add(myIC2)
                    .Add(myIC3)
                    .Add(myIC4)
                    .Add(myIC5)
                    .Add(myIC6)
                End If

                .Item(0).ConnectorName = "Inlet Stream 1"
                .Item(1).ConnectorName = "Inlet Stream 2"
                .Item(2).ConnectorName = "Inlet Stream 3"
                .Item(3).ConnectorName = "Inlet Stream 4"
                .Item(4).ConnectorName = "Inlet Stream 5"
                .Item(5).ConnectorName = "Inlet Stream 6"

            End With

            With GraphicObject.OutputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(GraphicObject.X + GraphicObject.Width, GraphicObject.Y + 0.5 * GraphicObject.Height)
                Else
                    .Add(myOC1)
                End If

                .Item(0).ConnectorName = "Mixed Stream"

            End With

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New EnergyMixer()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of EnergyMixer)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)

            Return elements

        End Function

        Public Sub New(ByVal Name As String, ByVal Description As String)

            MyBase.CreateNew()
            Me.ComponentName = Name
            Me.ComponentDescription = Description

        End Sub

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub Calculate(Optional args As Object = Nothing)

            Dim TotalEnergy As Double = 0

            Dim es As EnergyStream
            For Each cp In GraphicObject.InputConnectors
                If cp.IsAttached Then
                    es = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedFrom.Name)
                    TotalEnergy += es.EnergyFlow.GetValueOrDefault
                End If
            Next

            GetOutletEnergyStream(0).EnergyFlow = TotalEnergy

        End Sub

        Public Overrides Sub DeCalculate()

            GetOutletEnergyStream(0).EnergyFlow = Nothing

        End Sub

        Public Sub PopulateEditorPanel(ctner As Object) Implements IExternalUnitOperation.PopulateEditorPanel

        End Sub

        Private Sub CallSolverIfNeeded()
            If GlobalSettings.Settings.CallSolverOnEditorPropertyChanged Then
                FlowSheet.RequestCalculation()
            End If
        End Sub

    End Class

End Namespace