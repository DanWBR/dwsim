Imports System.Windows.Forms

Module DataGridViewExtensions

    <System.Runtime.CompilerServices.Extension()>
    Public Sub ChangeEditModeToOnPropertyChanged(gv As DataGridView)
        AddHandler gv.CurrentCellDirtyStateChanged, Function(sender, args)
                                                        gv.CommitEdit(DataGridViewDataErrorContexts.Commit)
                                                        If gv.CurrentCell Is Nothing Then
                                                            Return Nothing
                                                        End If
                                                        If gv.CurrentCell.EditType <> GetType(DataGridViewTextBoxEditingControl) Then
                                                            Return Nothing
                                                        End If
                                                        gv.BeginEdit(False)
                                                        Dim textBox = DirectCast(gv.EditingControl, TextBox)
                                                        textBox.SelectionStart = textBox.Text.Length
                                                        Return Nothing
                                                    End Function
    End Sub

End Module
