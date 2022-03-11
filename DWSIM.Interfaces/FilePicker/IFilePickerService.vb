Public Interface IFilePickerService
    Sub SetFilePickerFactory(ByVal filePickerFactory As Func(Of IFilePicker))
    Function GetFilePicker() As IFilePicker
End Interface
