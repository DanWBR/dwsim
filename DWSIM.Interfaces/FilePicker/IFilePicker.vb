Public Interface IFilePicker
    Function ShowOpenDialog(ByVal allowedTypes As IEnumerable(Of IFilePickerAllowedType)) As IFile
    Sub ShowSaveDialog()
End Interface