Public Interface IFilePicker
    Function ShowOpenDialog(ByVal allowedTypes As IEnumerable(Of IFilePickerAllowedType)) As IFile

    Function ShowSaveDialog(ByVal allowedTypes As IEnumerable(Of IFilePickerAllowedType)) As IFile
End Interface