Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations
Imports SkiaSharp

Public Class WaterElectrolyzer

    Inherits CleanEnergyUnitOpBase

    Private ImagePath As String = ""

    Private Image As SKImage

    Public Overrides Property Prefix As String = "WE-"

    Public Sub New()

        MyBase.New()

        _name = "Water Electrolyzer"
        _desc = "Water Electrolyzer"

    End Sub

    Public Overrides Sub Draw(g As Object)

        Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

        If Image Is Nothing Then

            ImagePath = SharedClasses.Utility.GetTempFileName()
            My.Resources.electrolysis.Save(ImagePath)

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

        Dim myIC1 As New ConnectionPoint

        myIC1.Position = New Point(x, y / 2)
        myIC1.Type = ConType.ConIn
        myIC1.Direction = ConDir.Right

        Dim myIC2 As New ConnectionPoint

        myIC2.Position = New Point(x + 0.5 * w, y + h)
        myIC2.Type = ConType.ConEn
        myIC2.Direction = ConDir.Up
        myIC2.Type = ConType.ConEn

        Dim myOC1 As New ConnectionPoint
        myOC1.Position = New Point(x + w, y / 2)
        myOC1.Type = ConType.ConOut
        myOC1.Direction = ConDir.Left

        With GraphicObject.InputConnectors
            If .Count = 2 Then
                .Item(0).Position = New Point(x, y / 2)
                .Item(1).Position = New Point(x + 0.5 * w, y + h)
            Else
                .Add(myIC1)
                .Add(myIC2)
            End If
            .Item(0).ConnectorName = "Fluid Inlet"
            .Item(1).ConnectorName = "Power Inlet"
        End With

        With GraphicObject.OutputConnectors
            If .Count = 2 Then
                .Item(0).Position = New Point(x + w, y / 2)
            Else
                .Add(myOC1)
            End If
            .Item(0).ConnectorName = "Products Outlet"
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

        Return New WaterElectrolyzer

    End Function

    Public Overrides Function GetIconBitmap() As Object

        Return My.Resources.electrolysis

    End Function

    Public Overrides Function CloneXML() As Object

        Dim obj As ICustomXMLSerialization = New WaterElectrolyzer()
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
