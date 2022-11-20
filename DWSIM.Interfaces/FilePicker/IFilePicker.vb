Public Interface IFilePicker
    Function ShowOpenDialog(ByVal allowedTypes As IEnumerable(Of IFilePickerAllowedType)) As IVirtualFile
    Property SuggestedDirectory As String
    Property SuggestedFilename As String
    Function ShowSaveDialog(ByVal allowedTypes As IEnumerable(Of IFilePickerAllowedType)) As IVirtualFile
End Interface