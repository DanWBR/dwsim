Imports DWSIM.Interfaces
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports AODL.Document

Public Class ReportCreator

    Dim DT As New DataTable
    Protected frm As IFlowsheet
    Dim su As SystemsOfUnits.Units
    Dim nf As String

    Public Sub New(flowsheet As IFlowsheet)
        frm = flowsheet
    End Sub

    Private Sub FillDataTable()

        su = frm.Options.SelectedUnitSystem
        nf = frm.Options.NumberFormat

        If Not DT.Columns.Contains(("Nome")) Then DT.Columns.Add(("Nome"), GetType(System.String))
        If Not DT.Columns.Contains(("Tipo")) Then DT.Columns.Add(("Tipo"), GetType(System.String))
        If Not DT.Columns.Contains(("Propriedade")) Then DT.Columns.Add(("Propriedade"), GetType(System.String))
        If Not DT.Columns.Contains(("Valor")) Then DT.Columns.Add(("Valor"), GetType(System.String))
        If Not DT.Columns.Contains(("Unidade")) Then DT.Columns.Add(("Unidade"), GetType(System.String))
        DT.Rows.Clear()

        Dim properties() As String
        Dim description As String
        Dim objtype As ObjectType
        Dim propidx, r1, r2, r3, r4, r5, r6 As Integer
        r1 = 5
        r2 = 12
        r3 = 30
        r4 = 48
        r5 = 66
        r6 = 84

        For Each baseobj As ISimulationObject In frm.SimulationObjects.Values
            properties = baseobj.GetProperties(Interfaces.Enums.PropertyType.ALL)
            objtype = baseobj.GraphicObject.ObjectType
            description = frm.GetTranslatedString(baseobj.GraphicObject.Description)
            If objtype = ObjectType.MaterialStream Then
                Dim value As String
                For propidx = 0 To r1 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                For propidx = r1 To r2 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString("FraomolarnaMistura"), "", ""})
                For Each subst As ICompound In baseobj.Phases(0).Compounds.Values
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                If DirectCast(baseobj, IMaterialStream).Phases(2).Properties.massflow.HasValue Then
                    For propidx = r2 To r3 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString("FraomolarnaPhaseVapor"), "", ""})
                    For Each subst As ICompound In baseobj.Phases(2).Compounds.Values
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If DirectCast(baseobj, IMaterialStream).Phases(1).Properties.massflow.HasValue Then
                    For propidx = r3 To r4 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                    For Each subst As ICompound In baseobj.Phases(1).Compounds.Values
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If DirectCast(baseobj, IMaterialStream).Phases(3).Properties.massflow.HasValue Then
                    For propidx = r4 To r5 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                    For Each subst As ICompound In baseobj.Phases(3).Compounds.Values
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If DirectCast(baseobj, IMaterialStream).Phases(4).Properties.massflow.HasValue Then
                    For propidx = r5 To r6 - 1
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                    For Each subst As ICompound In baseobj.Phases(4).Compounds.Values
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If DirectCast(baseobj, IMaterialStream).Phases(6).Properties.massflow.HasValue Then
                    For propidx = r6 To 101
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                    For Each subst As ICompound In baseobj.Phases(6).Compounds.Values
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
                If DirectCast(baseobj, IMaterialStream).Phases(7).Properties.massflow.HasValue Then
                    For propidx = 131 To 145
                        value = baseobj.GetPropertyValue(properties(propidx), su)
                        If Double.TryParse(value, New Double) Then
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                        Else
                            DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                        End If
                    Next
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString("FraomolardaPhase"), "", ""})
                    For Each subst As ICompound In baseobj.Phases(7).Compounds.Values
                        DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, subst.Name, Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                    Next
                End If
            Else
                For Each prop As String In properties
                    DT.Rows.Add(New String() {baseobj.GraphicObject.Tag, description, frm.GetTranslatedString(prop), Format(baseobj.GetPropertyValue(prop, su), nf), baseobj.GetPropertyUnit(prop, su)})
                Next
            End If
        Next

    End Sub

    Public Sub CreateAndSaveODTFile(filename As String)

        FillDataTable()

        Dim fname = AODL.Document.Styles.FontFamilies.Arial
        Dim fsize = "10pt"

        'Create a new text document
        Dim document As New TextDocuments.TextDocument()
        document.[New]()

        'Create a table for a text document using the TableBuilder
        Dim table As Content.Tables.Table = Content.Tables.TableBuilder.CreateTextDocumentTable(document, "table1", "table1", 3, 1, 16.99, False, False)

        Dim paragraph As Content.Text.Paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p0")
        paragraph.ParagraphStyle.TextProperties.Bold = "bold"
        paragraph.ParagraphStyle.TextProperties.FontName = fname
        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
        paragraph.TextContent.Add(New Content.Text.SimpleText(document, "DWSIM Simulation Results Report"))
        table.Rows(0).Cells(0).Content.Add(paragraph)

        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p1")
        paragraph.ParagraphStyle.TextProperties.FontName = fname
        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
        paragraph.TextContent.Add(New Content.Text.SimpleText(document, "Simulation File: " & frm.FilePath))
        table.Rows(1).Cells(0).Content.Add(paragraph)

        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p2")
        paragraph.ParagraphStyle.TextProperties.FontName = fname
        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
        paragraph.TextContent.Add(New Content.Text.SimpleText(document, "Date created: " & Date.Now.ToString))
        table.Rows(2).Cells(0).Content.Add(paragraph)

        'Add table to the document
        document.Content.Add(table)

        Try

            With document

                Dim i As Integer = 0
                Dim j As Integer = 1

                Dim prevmat, actualmat As String

                table = Content.Tables.TableBuilder.CreateTextDocumentTable(document, "table2", "table2", DT.Rows.Count + frm.SimulationObjects.Count * 3, 3, 16.99, False, False)

                Do
                    actualmat = Me.DT.Rows(i).Item(0).ToString
                    prevmat = Me.DT.Rows(i).Item(0).ToString

                    'Create a standard paragraph
                    paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p" + i.ToString() + j.ToString())
                    paragraph.ParagraphStyle.TextProperties.FontName = fname
                    paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                    paragraph.ParagraphStyle.TextProperties.Bold = "bold"

                    'Add some simple text
                    paragraph.TextContent.Add(New Content.Text.SimpleText(document, frm.GetTranslatedString("Objeto") & ": " & prevmat & " (" & Me.DT.Rows(i).Item(1).ToString & ")"))

                    'Insert paragraph into the first cell
                    table.Rows(j).Cells(0).Content.Add(paragraph)

                    j = j + 1
                    Do

                        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p1" + i.ToString() + j.ToString())
                        paragraph.ParagraphStyle.TextProperties.FontName = fname
                        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                        paragraph.TextContent.Add(New Content.Text.SimpleText(document, Me.DT.Rows(i).Item(2)))
                        table.Rows(j + 1).Cells(0).Content.Add(paragraph)

                        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p2" + i.ToString() + j.ToString())
                        paragraph.ParagraphStyle.ParagraphProperties.Alignment = "right"
                        paragraph.ParagraphStyle.TextProperties.FontName = fname
                        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                        paragraph.TextContent.Add(New Content.Text.SimpleText(document, Me.DT.Rows(i).Item(3)))
                        table.Rows(j + 1).Cells(1).Content.Add(paragraph)

                        paragraph = Content.Text.ParagraphBuilder.CreateParagraphWithCustomStyle(document, "p3" + i.ToString() + j.ToString())
                        paragraph.ParagraphStyle.ParagraphProperties.MarginLeft = "0.2cm"
                        paragraph.ParagraphStyle.TextProperties.FontName = fname
                        paragraph.ParagraphStyle.TextProperties.FontSize = fsize
                        paragraph.TextContent.Add(New Content.Text.SimpleText(document, Me.DT.Rows(i).Item(4)))
                        table.Rows(j + 1).Cells(2).Content.Add(paragraph)

                        i = i + 1
                        j = j + 1

                        If i < DT.Rows.Count Then actualmat = Me.DT.Rows(i).Item(0).ToString

                    Loop Until actualmat <> prevmat Or i >= DT.Rows.Count

                    j = j + 2

                Loop Until i >= DT.Rows.Count

            End With

            'Add table to the document
            document.Content.Add(table)

            'Save the document
            Using writer = New AODL.IO.OnDiskPackageWriter()
                document.Save(filename, New Export.OpenDocument.OpenDocumentTextExporter(writer))
            End Using

            frm.ShowMessage("Report saved sucessfully.", IFlowsheet.MessageType.Information)

        Catch ex As Exception

            frm.ShowMessage("Error saving report file: " + ex.ToString, IFlowsheet.MessageType.GeneralError)

        Finally

        End Try

    End Sub

    Public Sub CreateAndSaveODSFile(filename As String)

        FillDataTable()

        Dim sheetdoc As New SpreadsheetDocuments.SpreadsheetDocument()
        sheetdoc.[New]()
        Dim mysheet As New Content.Tables.Table(sheetdoc, "DWSIM_Report", "report")
        mysheet.Rows.Add(New Content.Tables.Row(mysheet))

        Try

            With mysheet

                Dim i As Integer = 0
                Dim j As Integer = 1

                Dim prevmat, actualmat As String

                Do

                    actualmat = Me.DT.Rows(i).Item(0).ToString
                    prevmat = Me.DT.Rows(i).Item(0).ToString
                    Dim cell = mysheet.CreateCell()
                    cell.OfficeValueType = "string"
                    Dim paragraph = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                    paragraph.TextContent.Add(New Content.Text.SimpleText(sheetdoc, frm.GetTranslatedString("Objeto") & ": " & prevmat & " (" & Me.DT.Rows(i).Item(1).ToString & ")"))
                    cell.Content.Add(paragraph)
                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                    mysheet.InsertCellAt(j, 0, cell)

                    j = j + 1

                    Do
                        mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                        Dim cell0 = mysheet.CreateCell()
                        cell0.OfficeValueType = "string"
                        Dim paragraph0 = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                        paragraph0.TextContent.Add(New Content.Text.SimpleText(sheetdoc, Me.DT.Rows(i).Item(2)))
                        cell0.Content.Add(paragraph0)
                        mysheet.InsertCellAt(j + 1, 0, cell0)
                        Dim cell1 = mysheet.CreateCell()
                        cell1.OfficeValueType = "string"
                        Dim paragraph1 = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                        paragraph1.TextContent.Add(New Content.Text.SimpleText(sheetdoc, Me.DT.Rows(i).Item(3)))
                        cell1.Content.Add(paragraph1)
                        mysheet.InsertCellAt(j + 1, 1, cell1)
                        Dim cell2 = mysheet.CreateCell()
                        cell2.OfficeValueType = "string"
                        Dim paragraph2 = Content.Text.ParagraphBuilder.CreateSpreadsheetParagraph(sheetdoc)
                        paragraph2.TextContent.Add(New Content.Text.SimpleText(sheetdoc, Me.DT.Rows(i).Item(4)))
                        cell2.Content.Add(paragraph2)
                        mysheet.InsertCellAt(j + 1, 2, cell2)
                        i = i + 1
                        j = j + 1
                        If i < DT.Rows.Count Then actualmat = Me.DT.Rows(i).Item(0).ToString
                    Loop Until actualmat <> prevmat Or i >= DT.Rows.Count

                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))
                    mysheet.Rows.Add(New Content.Tables.Row(mysheet))

                    j = j + 2

                Loop Until i >= DT.Rows.Count

            End With

            sheetdoc.TableCollection.Add(mysheet)

            Using writer As New AODL.IO.OnDiskPackageWriter
                sheetdoc.Save(filename, New Export.OpenDocument.OpenDocumentTextExporter(writer))
            End Using

            frm.ShowMessage("Report saved sucessfully.", IFlowsheet.MessageType.Information)

        Catch ex As Exception

            frm.ShowMessage("Error saving report file: " + ex.ToString, IFlowsheet.MessageType.GeneralError)

        Finally


        End Try

    End Sub

End Class
