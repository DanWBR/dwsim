Imports System.Drawing
Imports System.Windows.Forms

Public Module FormExtensions

    <System.Runtime.CompilerServices.Extension()>
    Public Function GetAllChildren(control As Control) As IEnumerable(Of Control)
        Dim controls = control.Controls.Cast(Of Control)
        Return controls.SelectMany(Function(ctrl) GetAllChildren(ctrl)).Concat(controls)
    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Sub ChangeDefaultFont(f As Form)

        f.AutoScaleMode = AutoScaleMode.Dpi

        Dim controls = f.GetAllChildren()

        For Each control As Control In controls
            control.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, control.Font.Size, control.Font.Style)
            If GlobalSettings.Settings.DpiScale > 1.0 Then
                If TypeOf control Is DataGridView Then
                    Dim dgv = DirectCast(control, DataGridView)
                    dgv.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None
                    dgv.AllowUserToResizeRows = False
                    dgv.RowTemplate.Height = 23 * GlobalSettings.Settings.DpiScale
                    dgv.ColumnHeadersHeight = 23 * GlobalSettings.Settings.DpiScale
                ElseIf TypeOf control Is Button Then
                    Dim button = DirectCast(control, Button)
                    If button.Image IsNot Nothing Then
                        button.Image = New Bitmap(button.Image,
                                                  New Size(button.Image.Width * GlobalSettings.Settings.DpiScale,
                                                           button.Image.Height * GlobalSettings.Settings.DpiScale))
                    End If

                End If
            End If
        Next

    End Sub

    Public Sub ChangeDefaultFont(c As UserControl)

        c.AutoScaleMode = AutoScaleMode.Dpi

        Dim controls = c.GetAllChildren()

        For Each control As Control In controls
            control.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, control.Font.Size, control.Font.Style)
            If GlobalSettings.Settings.DpiScale > 1.0 Then
                If TypeOf control Is DataGridView Then
                    Dim dgv = DirectCast(control, DataGridView)
                    dgv.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None
                    dgv.AllowUserToResizeRows = False
                    dgv.RowTemplate.Height = 23 * GlobalSettings.Settings.DpiScale
                    dgv.ColumnHeadersHeight *= GlobalSettings.Settings.DpiScale
                ElseIf TypeOf control Is Button Then
                    Dim button = DirectCast(control, Button)
                    If button.Image IsNot Nothing Then
                        button.Image = New Bitmap(button.Image,
                                                  New Size(button.Image.Width * GlobalSettings.Settings.DpiScale,
                                                           button.Image.Height * GlobalSettings.Settings.DpiScale))
                    End If

                End If
            End If
        Next

    End Sub

End Module
