Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces

Namespace GraphicObjects.Shapes

    Public Class ExternalUnitOperationGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()

            Me.ObjectType = ObjectType.External

            'Me.Description = TryCast(Owner, IExternalUnitOperation)?.Description

        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New SKPoint(posX, posY))
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal graphicSize As SKSize)
            Me.New(graphicPosition)
            Me.SetSize(graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As SKSize)
            Me.New(New SKPoint(posX, posY), graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer, ByVal height As Integer)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height))
        End Sub

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean
            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
        End Function

        Public Overrides Function SaveData() As List(Of XElement)
            Dim data = XMLSerializer.XMLSerializer.Serialize(Me)
            data.AddRange(MyBase.SaveData)
            Return data
        End Function

#End Region

        Public Overrides Sub CreateConnectors(InCount As Integer, OutCount As Integer)

            TryCast(Owner, IExternalUnitOperation)?.CreateConnectors()

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Me.Description = TryCast(Owner, IExternalUnitOperation)?.Description

            Me.AdditionalInfo = TryCast(Owner, IExternalUnitOperation)?.Name

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

            TryCast(Owner, IExternalUnitOperation)?.Draw(g)

        End Sub

        Public Overrides Sub PositionConnectors()

            CreateConnectors(0, 0)

        End Sub

    End Class

End Namespace
