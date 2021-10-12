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
            If control.Font.Style = FontStyle.Regular Then
                control.Font = SystemFonts.MessageBoxFont
            End If
            If control.Font.SizeInPoints < 12 And control.Font.Style = FontStyle.Bold Then
                control.Font = New Font(SystemFonts.MessageBoxFont, FontStyle.Bold)
            End If
            If GlobalSettings.Settings.DpiScale > 1.0 Then
                If TypeOf control Is DataGridView Then
                    Dim dgv = DirectCast(control, DataGridView)
                    dgv.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None
                    dgv.AllowUserToResizeRows = False
                    dgv.RowTemplate.Height = 23 * GlobalSettings.Settings.DpiScale
                    dgv.ColumnHeadersHeight = 23 * GlobalSettings.Settings.DpiScale
                End If
            End If
        Next

    End Sub

    Public Sub ChangeDefaultFont(c As UserControl)

        c.AutoScaleMode = AutoScaleMode.Dpi

        Dim controls = c.GetAllChildren()

        For Each control As Control In controls
            If control.Font.Style = FontStyle.Regular Then
                control.Font = SystemFonts.MessageBoxFont
            End If
            If control.Font.SizeInPoints < 12 And control.Font.Style = FontStyle.Bold Then
                control.Font = New Font(SystemFonts.MessageBoxFont, FontStyle.Bold)
            End If
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

    End Sub

End Module
