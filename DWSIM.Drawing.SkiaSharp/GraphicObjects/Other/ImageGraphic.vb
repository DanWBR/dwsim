Imports System.IO
Imports SkiaSharp

Namespace GraphicObjects.Shapes

    Public MustInherit Class ImageGraphic

        Inherits GraphicObject

        Protected Sub New()
            MyBase.New()
            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_Image
        End Sub

        Public MustOverride Function GetImage() As SKImage

    End Class

    Public Class EmbeddedImageGraphic

        Inherits ImageGraphic

#Region "Constructors"

        Public Sub New()
            MyBase.New()
            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_Image
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal startingImage As SKImage)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.Image = startingImage
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal startingImage As SKImage)
            Me.New(New SKPoint(posX, posY), startingImage)
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal graphicSize As SKSize, ByVal startingImage As SKImage)
            Me.New(graphicPosition, startingImage)
            Me.SetSize(graphicSize)
            Me.AutoSize = False
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As SKSize, ByVal startingImage As SKImage)
            Me.New(New SKPoint(posX, posY), graphicSize, startingImage)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer, ByVal height As Integer, ByVal startingImage As SKImage)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height), startingImage)
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal Rotation As Single, ByVal startingImage As SKImage)
            Me.New(graphicPosition, startingImage)
            Me.Rotation = Rotation
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal Rotation As Single, ByVal startingImage As SKImage)
            Me.New(New SKPoint(posX, posY), Rotation, startingImage)
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal graphicSize As SKSize, ByVal Rotation As Single, ByVal startingImage As SKImage)
            Me.New(graphicPosition, Rotation, startingImage)
            Me.SetSize(graphicSize)
            Me.AutoSize = False
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As SKSize, ByVal Rotation As Single, ByVal startingImage As SKImage)
            Me.New(New SKPoint(posX, posY), graphicSize, Rotation, startingImage)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer,
                               ByVal height As Integer, ByVal Rotation As Single, ByVal startingImage As SKImage)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height), Rotation, startingImage)
        End Sub

#End Region

        Public Property Image() As SKImage

        Public Overrides Function GetImage() As SKImage
            Return Me.Image
        End Function

        Public Overrides Sub Draw(ByVal g As Object)

            Dim p As New SKPaint
            With p
                p.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                p.FilterQuality = SKFilterQuality.High
            End With

            DirectCast(g, SKCanvas).DrawImage(Image, New SKRect(X, Y, X + Width, Y + Height), p)

        End Sub

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Image = Base64ToImage((From xel2 As XElement In data Select xel2 Where xel2.Name = "ImageData").Single)

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()

            elements.Add(New XElement("ImageData", ImageToBase64(Image, SKEncodedImageFormat.Png)))

            Return elements

        End Function

        Public Shared Function ImageToBase64(image As SKImage, format As SKEncodedImageFormat) As String
            Using ms As New MemoryStream()
                ' Convert Image to byte[]
                Dim data = image.Encode(format, 92)
                data.SaveTo(ms)
                Dim imageBytes As Byte() = ms.ToArray()
                ' Convert byte[] to Base64 String
                Dim base64String As String = Convert.ToBase64String(imageBytes)
                Return base64String
            End Using
        End Function

        Public Shared Function Base64ToImage(base64String As String) As SKImage
            ' Convert Base64 String to byte[]
            Dim imageBytes As Byte() = Convert.FromBase64String(base64String)
            Using ms As New MemoryStream(imageBytes, 0, imageBytes.Length)
                ms.Write(imageBytes, 0, imageBytes.Length)
                ms.Position = 0
                Dim data = SKData.Create(ms)
                Dim img As SKImage = SKImage.FromEncodedData(data)
                Return img
            End Using
        End Function

    End Class

End Namespace
