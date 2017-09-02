Imports System.Collections.Generic
Imports System.Linq
Imports System.Xml.Linq
Imports DWSIM.Interfaces
Imports Interfaces = DWSIM.Interfaces
Imports System.Drawing

Namespace GraphicObjects

    Public MustInherit Class GraphicObjectExtension

        Implements IGraphicObjectExtension, ICustomXMLSerialization

        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Return True

        End Function

        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements ICustomXMLSerialization.SaveData

            Dim elements As List(Of XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        Public Function GetBoundsRectangle() As Rectangle
            Return New Rectangle(Owner.X + RelativePosition.X, Owner.Y + RelativePosition.Y, Width, Height)
        End Function

        Public Shared Function ReturnInstance(typename As String) As IGraphicObjectExtension
            Dim t As Type = Type.GetType(typename, False)
            If Not t Is Nothing Then Return Activator.CreateInstance(t) Else Return Nothing
        End Function

#Region "Constructors"

        Public Sub New()

        End Sub

        Public Sub New(ByVal position As Point, owner As IGraphicObject)

            Me.New()
            _relposition = position
            Me.Owner = owner

        End Sub

#End Region


        Public Property Active As Boolean Implements IGraphicObjectExtension.Active

        Public Function Clone() As IGraphicObjectExtension Implements IGraphicObjectExtension.Clone
            Return Nothing
        End Function

        Public Property Description As String Implements IGraphicObjectExtension.Description

        Public MustOverride Sub Draw(surface As Object) Implements IGraphicObjectExtension.Draw

        Public Property Height As Integer Implements IGraphicObjectExtension.Height

        Public Function HitTest(zoomedSelection As Object) As Boolean Implements IGraphicObjectExtension.HitTest

            Dim rect As Rectangle = DirectCast(zoomedSelection, Rectangle)

            Return rect.Left <= Owner.X + RelativePosition.X And rect.Top <= Owner.Y + RelativePosition.Y And rect.Right >= Owner.X + RelativePosition.X + Width And rect.Bottom >= Owner.Y + RelativePosition.Y + Height

        End Function

        Public Property Name As String Implements IGraphicObjectExtension.Name

        Public Property Owner As IGraphicObject Implements IGraphicObjectExtension.Owner

        Private _relposition As New Point

        Public Property RelativePosition As IPoint Implements IGraphicObjectExtension.RelativePosition
            Get
                Return _relposition
            End Get
            Set(value As IPoint)
                _relposition = value
            End Set
        End Property

        Public Property Selected As Boolean Implements IGraphicObjectExtension.Selected

        Public Property Tag As String Implements IGraphicObjectExtension.Tag

        Public Property Width As Integer Implements IGraphicObjectExtension.Width

    End Class

End Namespace

