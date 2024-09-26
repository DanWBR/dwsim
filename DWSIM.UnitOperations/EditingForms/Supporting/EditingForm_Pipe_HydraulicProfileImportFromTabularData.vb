Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe
Imports unvell.ReoGrid
Imports DWSIM.ExtensionMethods

Public Class EditingForm_Pipe_HydraulicProfileImportFromTabularData

    Public PipeObject As UnitOperations.Pipe

    Private Fittings As New List(Of String)
    Private Materials As New List(Of String)({"Raw Steel", "Carbon Steel", "Cast Iron", "Stainless Steel", "PVC", "PVC+PRFV", "Commercial Copper", "User-defined"})

    Private Sub EditingForm_Pipe_HydraulicProfileImportFromTabularData_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        SetupGrid()

        Fittings.Add("Straight Tube")
        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(
            Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.UnitOperations.fittings.dat"), System.Text.Encoding.Default, True)
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(";")
            Dim linha_atual As String()
            While Not MyReader.EndOfData
                linha_atual = MyReader.ReadFields()
                Fittings.Add(linha_atual(0))
            End While
        End Using

    End Sub

    Private Sub SetupGrid()

        With grid1.Worksheets(0)
            .SetScale(Settings.DpiScale)
            .SetRows(100)
            .SetCols(10)
            .SetColumnsWidth(0, 10, 100)
            .SetRangeStyles(0, 0, 100, 10, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.HorizontalAlign,
                .HAlign = ReoGridHorAlign.Center
            })
            .SetRangeStyles(0, 0, 100, 10, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.VerticalAlign,
                .VAlign = ReoGridVerAlign.Middle
            })
            .SetRangeStyles(0, 0, 100, 10, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.FontAll,
                .FontName = System.Drawing.SystemFonts.MessageBoxFont.Name,
                .FontSize = System.Drawing.SystemFonts.MessageBoxFont.SizeInPoints
            })
            .ColumnHeaders(0).Text = "Type"
            .ColumnHeaders(1).Text = "Quantity"
            .ColumnHeaders(2).Text = "Increments"
            .ColumnHeaders(3).Text = "Material"
            .ColumnHeaders(4).Text = "Rugosity"
            .ColumnHeaders(5).Text = "Thermal Cond."
            .ColumnHeaders(6).Text = "Length"
            .ColumnHeaders(7).Text = "Elevation"
            .ColumnHeaders(8).Text = "Ext. Diam."
            .ColumnHeaders(9).Text = "Int. Diam."
        End With

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim mres = MessageBox.Show("This will replace the current hydraulic profile with data from the table. Proceed?", "Warning", MessageBoxButtons.YesNo, MessageBoxIcon.Question)

        If mres = DialogResult.Yes Then

            ParseRows()

        End If

    End Sub

    Private Sub ParseRows()

        Dim units = PipeObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        Dim slist As New List(Of PipeSection)

        For i = 0 To 99

            Try

                Dim psec As New PipeSection

                psec.Indice = i + 1

                Dim v0 = grid1.Worksheets(0).GetCellData(i, 0)
                Dim v1 = grid1.Worksheets(0).GetCellData(i, 1)
                Dim v2 = grid1.Worksheets(0).GetCellData(i, 2)
                Dim v3 = grid1.Worksheets(0).GetCellData(i, 3)
                Dim v4 = grid1.Worksheets(0).GetCellData(i, 4)
                Dim v5 = grid1.Worksheets(0).GetCellData(i, 5)
                Dim v6 = grid1.Worksheets(0).GetCellData(i, 6)
                Dim v7 = grid1.Worksheets(0).GetCellData(i, 7)
                Dim v8 = grid1.Worksheets(0).GetCellData(i, 8)
                Dim v9 = grid1.Worksheets(0).GetCellData(i, 9)

                If v0 IsNot Nothing Then
                    Try
                        psec.TipoSegmento = Fittings(Convert.ToInt32(v0))
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Type Index at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                Else
                    Exit For
                End If
                If v1 IsNot Nothing Then
                    Try
                        psec.Quantidade = Convert.ToInt32(v1)
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Amount at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                Else
                    Throw New Exception(String.Format("Invalid Quantity at Row {0}", i + 1))
                End If
                If v2 IsNot Nothing Then
                    Try
                        psec.Incrementos = Convert.ToInt32(v2)
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Number of Sections at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                Else
                    Throw New Exception(String.Format("Invalid Number of Sections at Row {0}", i + 1))
                End If
                If v3 IsNot Nothing Then
                    Try
                        psec.Material = Materials(Convert.ToInt32(v3))
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Material Index at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                Else
                    Throw New Exception(String.Format("Invalid Material Index at Row {0}", i + 1))
                End If
                If v4 IsNot Nothing And psec.Material = "User-defined" Then
                    Try
                        psec.PipeWallRugosity = v4.ToString().ToDoubleFromCurrent().ConvertToSI(units.distance)
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Rugosity at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                End If
                If v5 IsNot Nothing And psec.Material = "User-defined" Then
                    Try
                        psec.PipeWallThermalConductivityExpression = v5.ToString().ToDoubleFromCurrent().ConvertToSI(units.thermalConductivity)
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Thermal Conductivity at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                End If
                If v6 IsNot Nothing And Convert.ToInt32(v0) = 0 Then
                    Try
                        psec.Comprimento = v6.ToString().ToDoubleFromCurrent().ConvertToSI(units.distance)
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Length at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                End If
                If v7 IsNot Nothing And Convert.ToInt32(v0) = 0 Then
                    Try
                        psec.Elevacao = v7.ToString().ToDoubleFromCurrent().ConvertToSI(units.distance)
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Elevation at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                End If
                If v8 IsNot Nothing And Convert.ToInt32(v0) = 0 Then
                    Try
                        psec.DE = v8.ToString().ToDoubleFromCurrent().ConvertUnits(units.diameter, "in.")
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for External Diameter at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                End If
                If v9 IsNot Nothing Then
                    Try
                        psec.DI = v9.ToString().ToDoubleFromCurrent().ConvertUnits(units.diameter, "in.")
                    Catch ex As Exception
                        Throw New Exception(String.Format("Invalid Value for Internal Diameter at Row {0}: {1}", i + 1, ex.Message))
                    End Try
                End If

                slist.Add(psec)

            Catch ex As Exception

                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try

        Next

        If slist.Count > 0 Then

            PipeObject.FlowSheet.RegisterSnapshot(Enums.SnapshotType.ObjectData, PipeObject)

            PipeObject.Profile.Sections.Clear()
            For Each s In slist
                PipeObject.Profile.Sections.Add(s.Indice, s)
            Next
            PipeObject.Profile.Status = PipeEditorStatus.OK
            PipeObject.UpdateEditForm()

            MessageBox.Show(String.Format("{0} sections parsed. Hydraulic profile updated successfully.", slist.Count), "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)

        Else

            MessageBox.Show("No sections parsed. Current profile remains unchanged.", "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)

        End If


    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Close()
    End Sub
End Class