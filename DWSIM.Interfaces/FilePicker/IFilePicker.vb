Public Interface IFilePicker
    Function ShowOpenDialog(ByVal allowedTypes As IEnumerable(Of IFilePickerAllowedType)) As IVirtualFile

    Function ShowSaveDialog(ByVal allowedTypes As IEnumerable(Of IFilePickerAllowedType)) As IVirtualFile
End Interface