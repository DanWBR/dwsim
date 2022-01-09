Imports DWSIM.Drawing.SkiaSharp
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects

Public Class FormEditGraphicObject

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public gobj As GraphicObject
    Public fs As GraphicsSurface
    Public flowsheet As Interfaces.IFlowsheet

    Private _origtext As String = ""

    Private Sub FormEditGraphicObject_DockStateChanged(sender As Object, e As EventArgs) Handles Me.DockStateChanged

        If Not Me.FloatPane Is Nothing Then
            Me.Width = 400
            Me.Height = 400
            Me.FloatPane.FloatWindow.Width = 400
            Me.FloatPane.FloatWindow.Height = 400
        End If

    End Sub

    Private Sub FormEditGraphicObject_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        _origtext = Me.Text

        If Not gobj Is Nothing Then Me.Text = gobj.Tag + " - " + _origtext

        Me.TabText = Me.Text

        Me.SuspendLayout()

        Me.Width = 400
        Me.Height = 400

        Me.ResumeLayout()

        PopulatePGEx2()

    End Sub

    Private Sub FormEditGraphicObject_Shown(sender As Object, e As EventArgs) Handles Me.Shown

    End Sub

    Private Sub tsbClose_Click(sender As Object, e As EventArgs) Handles tsbClose.Click
        Me.Close()
    End Sub

    Public Sub DockingHandler(sender As Object, e As EventArgs) Handles tsbDockingLeft.Click, tsbDockingBottom.Click, tsbDockingDocument.Click,
                                                                        tsbDockingFloat.Click, tsbDockingLeftAutoHide.Click, tsbDockingRight.Click,
                                                                        tsbDockingRightAutoHide.Click, tsbDockingTop.Click

        If sender Is tsbDockingLeft Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        ElseIf sender Is tsbDockingLeftAutoHide Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
        ElseIf sender Is tsbDockingRight Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
        ElseIf sender Is tsbDockingRightAutoHide Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
        ElseIf sender Is tsbDockingTop Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
        ElseIf sender Is tsbDockingBottom Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
        ElseIf sender Is tsbDockingDocument Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
        ElseIf sender Is tsbDockingFloat Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
        End If

    End Sub

    Public Sub PopulatePGEx2()

        If gobj IsNot Nothing Then

            If gobj.ObjectType = ObjectType.GO_Table Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add("Font Style", gobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item.Add(DWSIM.App.GetLocalString("Size"), gobj, "FontSize", False, "", DWSIM.App.GetLocalString("Size"), True)
                    .Item(.Item.Count - 1).Tag2 = "FontSize"
                    .Item.Add("Border Color", gobj, "BorderColor", False, "", "", True)
                    .Item(.Item.Count - 1).Tag2 = "BorderColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add("Text Color", gobj, "TextColor", False, "", "", True)
                    .Item(.Item.Count - 1).Tag2 = "TextColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Espaamento"), gobj, "Padding", False, "", DWSIM.App.GetLocalString("Espaamentoentreotext"), True)
                    .Item(.Item.Count - 1).Tag2 = "Padding"
                    .Item.Add(DWSIM.App.GetLocalString("EspessuradaBorda"), gobj, "LineWidth", False, "", DWSIM.App.GetLocalString("Espessuradabordadoob"), True)
                    .Item(.Item.Count - 1).Tag2 = "LineWidth"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            ElseIf gobj.ObjectType = ObjectType.GO_MasterTable Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add("Font Style", gobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item.Add(DWSIM.App.GetLocalString("Size"), gobj, "FontSize", False, "", DWSIM.App.GetLocalString("Size"), True)
                    .Item(.Item.Count - 1).Tag2 = "FontSize"
                    .Item.Add("Border Color", gobj, "BorderColor", False, "", "", True)
                    .Item(.Item.Count - 1).Tag2 = "BorderColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add("Text Color", gobj, "TextColor", False, "", "", True)
                    .Item(.Item.Count - 1).Tag2 = "TextColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Espaamento"), gobj, "Padding", False, "", DWSIM.App.GetLocalString("Espaamentoentreotext"), True)
                    .Item(.Item.Count - 1).Tag2 = "Padding"
                    .Item.Add(DWSIM.App.GetLocalString("EspessuradaBorda"), gobj, "LineWidth", False, "", DWSIM.App.GetLocalString("Espessuradabordadoob"), True)
                    .Item(.Item.Count - 1).Tag2 = "LineWidth"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            ElseIf gobj.ObjectType = ObjectType.GO_SpreadsheetTable Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add("Font Style", gobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item.Add(DWSIM.App.GetLocalString("Size"), gobj, "FontSize", False, "", DWSIM.App.GetLocalString("Size"), True)
                    .Item(.Item.Count - 1).Tag2 = "FontSize"
                    .Item.Add("Border Color", gobj, "BorderColor", False, "", "", True)
                    .Item(.Item.Count - 1).Tag2 = "BorderColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add("Text Color", gobj, "TextColor", False, "", "", True)
                    .Item(.Item.Count - 1).Tag2 = "TextColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Espaamento"), gobj, "Padding", False, "", DWSIM.App.GetLocalString("Espaamentoentreotext"), True)
                    .Item(.Item.Count - 1).Tag2 = "Padding"
                    .Item.Add(DWSIM.App.GetLocalString("EspessuradaBorda"), gobj, "LineWidth", False, "", DWSIM.App.GetLocalString("Espessuradabordadoob"), True)
                    .Item(.Item.Count - 1).Tag2 = "LineWidth"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With


            ElseIf gobj.ObjectType = ObjectType.GO_Text Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add("Font Style", gobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item.Add(DWSIM.App.GetLocalString("Nome"), gobj.Tag, False, "", DWSIM.App.GetLocalString("Nomedoobjeto"), True)
                    .Item(.Item.Count - 1).Tag2 = "Tag"
                    .Item.Add(DWSIM.App.GetLocalString("Texto"), gobj, "Text", False, "", DWSIM.App.GetLocalString("Textoaserexibidonaca"), True)
                    .Item(.Item.Count - 1).Tag2 = "Text"
                    .Item(.Item.Count - 1).CustomEditor = New System.ComponentModel.Design.MultilineStringEditor
                    .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj, "Color", False, "", DWSIM.App.GetLocalString("Cordotexto"), True)
                    .Item(.Item.Count - 1).Tag2 = "Color"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Size"), gobj, "Size", False, "", DWSIM.App.GetLocalString("Size"), True)
                    .Item(.Item.Count - 1).Tag2 = "Size"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            ElseIf gobj.ObjectType = ObjectType.GO_HTMLText Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add(DWSIM.App.GetLocalString("Width"), gobj, "Width", False, "", DWSIM.App.GetLocalString("Width"), True)
                    .Item(.Item.Count - 1).Tag2 = "Width"
                    .Item.Add(DWSIM.App.GetLocalString("Height"), gobj, "Height", False, "", DWSIM.App.GetLocalString("Height"), True)
                    .Item(.Item.Count - 1).Tag2 = "Height"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            ElseIf gobj.ObjectType = ObjectType.GO_Image Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add("Font Style", gobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item.Add(DWSIM.App.GetLocalString("Autodimensionar"), gobj, "AutoSize", False, "", DWSIM.App.GetLocalString("SelecioLiquidrueparaque"), True)
                    .Item(.Item.Count - 1).Tag2 = "AutoSize"
                    .Item.Add(DWSIM.App.GetLocalString("Altura"), gobj, "Height", False, "", DWSIM.App.GetLocalString("Alturadafiguraempixe"), True)
                    .Item(.Item.Count - 1).Tag2 = "Height"
                    .Item.Add(DWSIM.App.GetLocalString("Largura"), gobj, "Width", False, "", DWSIM.App.GetLocalString("Larguradafiguraempix"), True)
                    .Item(.Item.Count - 1).Tag2 = "Width"
                    .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj, "Rotation", False, "", DWSIM.App.GetLocalString("Rotaodafigurade0a360"), True)
                    .Item(.Item.Count - 1).Tag2 = "Rotation"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            ElseIf gobj.ObjectType = ObjectType.GO_Rectangle Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add("Font Style", gobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item.Add(DWSIM.App.GetLocalString("Texto"), gobj, "Text", False, "", DWSIM.App.GetLocalString("Textoaserexibidonaca"), True)
                    .Item(.Item.Count - 1).Tag2 = "Text"
                    .Item(.Item.Count - 1).CustomEditor = New System.ComponentModel.Design.MultilineStringEditor
                    .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj, "FontColor", False, "", DWSIM.App.GetLocalString("Cordotexto"), True)
                    .Item(.Item.Count - 1).Tag2 = "Color"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Altura"), gobj, "Height", False, "", DWSIM.App.GetLocalString("Alturadafiguraempixe"), True)
                    .Item(.Item.Count - 1).Tag2 = "Height"
                    .Item.Add(DWSIM.App.GetLocalString("Largura"), gobj, "Width", False, "", DWSIM.App.GetLocalString("Larguradafiguraempix"), True)
                    .Item(.Item.Count - 1).Tag2 = "Width"
                    .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj, "Rotation", False, "", DWSIM.App.GetLocalString("Rotaodafigurade0a360"), True)
                    .Item(.Item.Count - 1).Tag2 = "Rotation"
                    .Item.Add(DWSIM.App.GetLocalString("Gradiente2"), gobj, "GradientMode", False, "", DWSIM.App.GetLocalString("SelecioLiquidrueparaapl"), True)
                    .Item(.Item.Count - 1).Tag2 = "GradientMode"
                    .Item.Add(DWSIM.App.GetLocalString("Gradiente_Cor1"), gobj, "GradientColor1", False, "", DWSIM.App.GetLocalString("Cor1dogradienteseapl"), True)
                    .Item(.Item.Count - 1).Tag2 = "GradientColor1"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Gradiente_Cor2"), gobj, "GradientColor2", False, "", DWSIM.App.GetLocalString("Cor2dogradienteseapl"), True)
                    .Item(.Item.Count - 1).Tag2 = "GradientColor2"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj, "FillColor", False, "", "", True)
                    .Item(.Item.Count - 1).Tag2 = "FillColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("EspessuradaBorda"), gobj, "LineWidth", False, "", DWSIM.App.GetLocalString("Espessuradabordadoob"), True)
                    .Item(.Item.Count - 1).Tag2 = "LineWidth"
                    .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj, "LineColor", False, "", DWSIM.App.GetLocalString("Cordotextodatabela"), True)
                    .Item(.Item.Count - 1).Tag2 = "LineColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("Opacidade0255"), gobj, "Opacity", False, "", DWSIM.App.GetLocalString("Nveldetransparnciada"), True)
                    .Item(.Item.Count - 1).Tag2 = "Opacity"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            ElseIf gobj.ObjectType = ObjectType.GO_Animation Then

                With Me.PGEx2

                    .Item.Clear()

                    .Item.Add("Font Style", gobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item.Add(DWSIM.App.GetLocalString("Autodimensionar"), gobj, "AutoSize", False, "", DWSIM.App.GetLocalString("SelecioLiquidrueparaque"), True)
                    .Item(.Item.Count - 1).Tag2 = "AutoSize"
                    .Item.Add(DWSIM.App.GetLocalString("Altura"), gobj, "Height", False, "", DWSIM.App.GetLocalString("Alturadafiguraempixe"), True)
                    .Item(.Item.Count - 1).Tag2 = "Height"
                    .Item.Add(DWSIM.App.GetLocalString("Largura"), gobj, "Width", False, "", DWSIM.App.GetLocalString("Larguradafiguraempix"), True)
                    .Item(.Item.Count - 1).Tag2 = "Width"
                    .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj, "Rotation", False, "", DWSIM.App.GetLocalString("Rotaodafigurade0a360"), True)
                    .Item(.Item.Count - 1).Tag2 = "Rotation"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            Else

                With Me.PGEx2

                    .Item.Clear()

                    Dim sobj As ShapeGraphic = gobj

                    .Item.Add("Object Name", sobj, "Tag", False, "", DWSIM.App.GetLocalString("Nomedoobjeto"), True)
                    .Item.Add("Font Style", sobj, "FontStyle", False, "", "Style of the Text (Regular, Bold, Italic or Bold/Italic", True)
                    .Item(.Item.Count - 1).Tag2 = "Tag"
                    .Item.Add(DWSIM.App.GetLocalString("Cor"), sobj, "LineColor", False, "", "", True)
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item(.Item.Count - 1).Tag2 = "LineColor"
                    .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                    .Item.Add(DWSIM.App.GetLocalString("EspessuradaBorda"), sobj, "LineWidth", False, "", DWSIM.App.GetLocalString("Espessuradabordadoob"), True)
                    .Item(.Item.Count - 1).Tag2 = "LineWidth"
                    .Item.Add(DWSIM.App.GetLocalString("Comprimento"), sobj, "Width", False, "", DWSIM.App.GetLocalString("Comprimentodoobjetoe"), True)
                    .Item(.Item.Count - 1).Tag2 = "Width"
                    .Item.Add(DWSIM.App.GetLocalString("Altura"), sobj, "Height", False, "", DWSIM.App.GetLocalString("Alturadoobjetoempixe"), True)
                    .Item(.Item.Count - 1).Tag2 = "Height"
                    .Item.Add(DWSIM.App.GetLocalString("Rotao"), sobj, "Rotation", False, "", DWSIM.App.GetLocalString("Rotaodoobjetode0a360"), True)
                    .Item(.Item.Count - 1).Tag2 = "Rotation"
                    .Item.Add("X", gobj, "X", False, "", DWSIM.App.GetLocalString("Coordenadahorizontal"), True)
                    .Item(.Item.Count - 1).Tag2 = "X"
                    .Item.Add("Y", gobj, "Y", False, "", DWSIM.App.GetLocalString("Coordenadaverticaldo"), True)
                    .Item(.Item.Count - 1).Tag2 = "Y"

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            End If

        Else

            With PGEx2

                .PropertySort = PropertySort.NoSort
                .ShowCustomProperties = True
                Try
                    .Item.Clear()
                Catch ex As Exception
                Finally
                    .Item.Clear()
                End Try
                .Item.Add(DWSIM.App.GetLocalString("Cordofundo"), fs, "BackColor", False, "", "", True)
                .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                .Item.Add(DWSIM.App.GetLocalString("Cordagrade"), fs, "GridColor", False, "", "", True)
                .Item(.Item.Count - 1).CustomEditor = New DWSIM.Editors.SKColorEditor.UISKColorEditor
                .Item.Add(DWSIM.App.GetLocalString("Espessuradagrade"), fs, "GridLineWidth", False, "", "", True)
                .Item.Add(DWSIM.App.GetLocalString("SnapToGrid"), fs, "SnapToGrid", False, "", "", True)
                .Item.Add(DWSIM.App.GetLocalString("GridSize"), fs, "GridSize", False, "", "", True)
                .Item.Add(DWSIM.App.GetLocalString("Largura"), fs.SurfaceBounds, "Width", False, "", "", True)
                .Item.Add(DWSIM.App.GetLocalString("Altura"), fs.SurfaceBounds, "Height", False, "", "", True)
                .Item.Add(DWSIM.App.GetLocalString("Larguradeimpresso"), fs.SurfaceMargins, "Width", False, "", "", True)
                .Item.Add(DWSIM.App.GetLocalString("Alturadeimpresso"), fs.SurfaceMargins, "Height", False, "", "", True)

                .Refresh()
            End With

        End If

    End Sub

    Private Sub PGEx2_PropertyValueChanged(s As Object, e As PropertyValueChangedEventArgs) Handles PGEx2.PropertyValueChanged
        If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Nome")) Then
            If Not gobj Is Nothing Then
                Me.Text = gobj.Tag + " - " + _origtext
                Me.TabText = Me.Text
            End If
            flowsheet.UpdateOpenEditForms()
            Me.PGEx2.Focus()
        End If
        flowsheet.UpdateInterface()
    End Sub

End Class