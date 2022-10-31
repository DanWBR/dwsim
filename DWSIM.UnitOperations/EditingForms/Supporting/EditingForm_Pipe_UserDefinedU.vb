Imports System.Drawing
Imports unvell.ReoGrid
Imports unvell.ReoGrid.DataFormat

Public Class EditingForm_Pipe_UserDefinedU

    Private Loaded As Boolean = False

    Public Profile As UnitOperations.Auxiliary.Pipe.ThermalEditorDefinitions

    Public Flowsheet As IFlowsheet

    Private Sub EditingForm_Pipe_UserDefinedU_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ChangeDefaultFont()

        Loaded = False

        SetupGrid()

        Loaded = True

    End Sub

    Private Sub SetupGrid()

        Dim units = Flowsheet.FlowsheetOptions.SelectedUnitSystem
        Dim nf = Flowsheet.FlowsheetOptions.NumberFormat

        With grid1.Worksheets(0)
            .SelectionForwardDirection = SelectionForwardDirection.Down
            .SetScale(Settings.DpiScale)
            .SetRows(100)
            .SetCols(3)
            .SetColumnsWidth(0, 3, 120)
            .ColumnHeaders(0).Text = String.Format("Length/Depth ({0})", units.distance)
            .ColumnHeaders(1).Text = String.Format("Ambient Temp. ({0})", units.temperature)
            .ColumnHeaders(2).Text = String.Format("Overall HTC ({0})", units.heat_transf_coeff)
            .SetRangeDataFormat(0, 0, 100, 3, CellDataFormatFlag.Number,
            New NumberDataFormatter.NumberFormatArgs With {
                .DecimalPlaces = 4,
                .UseSeparator = True,
                .NegativeStyle = NumberDataFormatter.NumberNegativeStyle.Minus
            })
            .SetRangeStyles(0, 0, 100, 3, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.HorizontalAlign,
                .HAlign = ReoGridHorAlign.Right
            })
            For i = 0 To profile.UserDefinedU_Length.Count - 1
                .SetCellData(i, 0, profile.UserDefinedU_Length(i).ConvertFromSI(units.distance))
                .SetCellData(i, 1, profile.UserDefinedU_Temp(i).ConvertFromSI(units.temperature))
                .SetCellData(i, 2, profile.UserDefinedU_U(i).ConvertFromSI(units.heat_transf_coeff))
            Next
            AddHandler .CellDataChanged,
                Sub(sender, e)
                    If Loaded Then
                        profile.UserDefinedU_Length.Clear()
                        profile.UserDefinedU_Temp.Clear()
                        profile.UserDefinedU_U.Clear()
                        For i = 0 To 99
                            Dim data1 = .GetCellData(i, 0)
                            Dim data2 = .GetCellData(i, 1)
                            Dim data3 = .GetCellData(i, 2)
                            If data1 IsNot Nothing And data2 IsNot Nothing And data3 IsNot Nothing Then
                                Try
                                    profile.UserDefinedU_Length.Add(data1.ToString().ToDoubleFromCurrent().ConvertToSI(units.distance))
                                    Profile.UserDefinedU_Temp.Add(data2.ToString().ToDoubleFromCurrent().ConvertToSI(units.temperature))
                                    Profile.UserDefinedU_U.Add(data3.ToString().ToDoubleFromCurrent().ConvertToSI(units.heat_transf_coeff))
                                Catch ex As Exception
                                    MessageBox.Show(String.Format("Error on data table: {0}", ex.Message), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                End Try
                            End If
                        Next
                    End If
                End Sub
        End With

    End Sub

End Class