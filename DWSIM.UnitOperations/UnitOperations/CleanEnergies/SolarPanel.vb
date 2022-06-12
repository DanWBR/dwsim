Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations
Imports SkiaSharp

Public Class SolarPanel

    Inherits CleanEnergyUnitOpBase

    Private ImagePath As String = ""

    Private Image As SKImage

    Public Overrides Property Prefix As String = "SP-"

    Public Sub New()

        MyBase.New()

        _name = "Solar Panel"
        _desc = "Solar Panel"

    End Sub

    Public Overrides Sub Draw(g As Object)

        Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

        If Image Is Nothing Then

            ImagePath = SharedClasses.Utility.GetTempFileName()
            My.Resources.icons8_solar_panel.Save(ImagePath)

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

    Public Overrides Sub CreateConnectors()

        Dim w, h, x, y As Double
        w = GraphicObject.Width
        h = GraphicObject.Height
        x = GraphicObject.X
        y = GraphicObject.Y

        Dim myOC1 As New ConnectionPoint
        myOC1.Position = New Point(x + w, y / 2.0)
        myOC1.Type = ConType.ConOut
        myOC1.Direction = ConDir.Right
        myOC1.Type = ConType.ConEn

        With GraphicObject.OutputConnectors
            If .Count = 1 Then
                .Item(0).Position = New Point(x + w, y / 2.0)
            Else
                .Add(myOC1)
            End If
            .Item(0).ConnectorName = "Power Outlet"
        End With

        Me.GraphicObject.EnergyConnector.Active = False

    End Sub

    Public Overrides Sub PopulateEditorPanel(ctner As Object)

    End Sub

    Public Overrides Sub DisplayEditForm()

    End Sub

    Public Overrides Sub UpdateEditForm()

    End Sub

    Public Overrides Sub CloseEditForm()

    End Sub

    Public Overrides Function ReturnInstance(typename As String) As Object

        Return New SolarPanel

    End Function

    Public Overrides Function GetIconBitmap() As Object

        Return My.Resources.icons8_solar_panel

    End Function

    Public Overrides Function CloneXML() As Object

        Dim obj As ICustomXMLSerialization = New SolarPanel()
        obj.LoadData(Me.SaveData)
        Return obj

    End Function

    Public Overrides Function CloneJSON() As Object

        Throw New NotImplementedException()

    End Function

    Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

        Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

        XMLSerializer.XMLSerializer.Deserialize(Me, data)

        Return True

    End Function

    Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

        Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
        Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

        Return elements

    End Function

End Class
