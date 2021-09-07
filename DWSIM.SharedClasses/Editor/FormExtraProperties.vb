Imports System.ComponentModel.Design
Imports System.Text
Imports DWSIM.Interfaces

Public Class FormExtraProperties

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public SimObject As ISimulationObject

    Public Loaded As Boolean = False

    Private Sub FormExtraProperties_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.AutoScaleMode = AutoScaleMode.Font

        For Each control As Control In Me.Controls
            control.Font = Drawing.SystemFonts.MessageBoxFont
            If GlobalSettings.Settings.DpiScale > 1.0 Then
                If TypeOf control Is DataGridView Then
                    Dim dgv = DirectCast(control, DataGridView)
                    dgv.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None
                    dgv.AllowUserToResizeRows = False
                    dgv.RowTemplate.Height = 23 * GlobalSettings.Settings.DpiScale
                    dgv.ColumnHeadersHeight *= GlobalSettings.Settings.DpiScale
                End If
            End If
        Next

        UpdateValues()

    End Sub

    Public Sub UpdateValues()

        Loaded = False

        Text = SimObject.GraphicObject.Tag + ": " + SimObject.GetFlowsheet().GetTranslatedString("AdditionalProperties")
        TabText = Text

        grid1.Rows.Clear()

        Dim col1 = DirectCast(SimObject.ExtraProperties, IDictionary(Of String, Object))
        Dim col2 = DirectCast(SimObject.ExtraPropertiesDescriptions, IDictionary(Of String, Object))

        For Each prop In col1
            If Not col2.ContainsKey(prop.Key) Then
                grid1.Rows.Add(New Object() {prop.Key, prop.Value})
            End If
        Next

        Loaded = True

    End Sub

    Private Sub grid1_KeyDown(sender As Object, e As KeyEventArgs) Handles grid1.KeyDown
        If e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then PasteData(grid1)
    End Sub

    Private Sub grid1_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles grid1.DataError

    End Sub

    Private Sub grid1_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles grid1.CellValueChanged

        If Loaded Then

            If e.ColumnIndex = 1 Then

                Dim col1 = DirectCast(SimObject.ExtraProperties, IDictionary(Of String, Object))

                Dim id = grid1.Rows(e.RowIndex).Cells(0).Value
                Dim value = grid1.Rows(e.RowIndex).Cells(1).Value

                Try

                    If TypeOf col1(id) Is Double Then
                        col1(id) = Double.Parse(value.ToString())
                    Else
                        col1(id) = value
                    End If

                    SimObject.GetFlowsheet().RequestCalculation(SimObject)

                Catch ex As Exception

                    MessageBox.Show(ex.Message, SimObject.GetFlowsheet().GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

                End Try

            End If

        End If

    End Sub

End Class