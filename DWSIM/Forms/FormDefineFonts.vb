Imports DWSIM.Drawing.SkiaSharp
Imports DWSIM.Interfaces

Public Class FormDefineFonts

    Public FlowsheetSurface As GraphicsSurface
    Public Flowsheet As IFlowsheet
    Private Loaded As Boolean = False

    Private Sub FormDefineFonts_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        ComboBox1.Items.AddRange(GraphicsSurface.RegularFonts.ToArray())
        ComboBox2.Items.AddRange(GraphicsSurface.BoldFonts.ToArray())
        ComboBox4.Items.AddRange(GraphicsSurface.BoldItalicFonts.ToArray())
        ComboBox3.Items.AddRange(GraphicsSurface.ItalicFonts.ToArray())

        FlowsheetSurface.SetRegularFont(Flowsheet.FlowsheetOptions.RegularFontName)
        FlowsheetSurface.SetBoldFont(Flowsheet.FlowsheetOptions.BoldFontName)
        FlowsheetSurface.SetItalicFont(Flowsheet.FlowsheetOptions.ItalicFontName)
        FlowsheetSurface.SetBoldItalicFont(Flowsheet.FlowsheetOptions.BoldItalicFontName)

        ComboBox1.SelectedItem = Flowsheet.FlowsheetOptions.RegularFontName
        ComboBox2.SelectedItem = Flowsheet.FlowsheetOptions.BoldFontName
        ComboBox4.SelectedItem = Flowsheet.FlowsheetOptions.BoldItalicFontName
        ComboBox3.SelectedItem = Flowsheet.FlowsheetOptions.ItalicFontName

        Loaded = True

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged, ComboBox2.SelectedIndexChanged, ComboBox3.SelectedIndexChanged, ComboBox4.SelectedIndexChanged

        If Loaded Then

            Flowsheet.FlowsheetOptions.RegularFontName = ComboBox1.SelectedItem
            Flowsheet.FlowsheetOptions.BoldFontName = ComboBox2.SelectedItem
            Flowsheet.FlowsheetOptions.BoldItalicFontName = ComboBox4.SelectedItem
            Flowsheet.FlowsheetOptions.ItalicFontName = ComboBox3.SelectedItem

            FlowsheetSurface.SetRegularFont(Flowsheet.FlowsheetOptions.RegularFontName)
            FlowsheetSurface.SetBoldFont(Flowsheet.FlowsheetOptions.BoldFontName)
            FlowsheetSurface.SetItalicFont(Flowsheet.FlowsheetOptions.ItalicFontName)
            FlowsheetSurface.SetBoldItalicFont(Flowsheet.FlowsheetOptions.BoldItalicFontName)

            Flowsheet.UpdateInterface()

        End If

    End Sub

End Class