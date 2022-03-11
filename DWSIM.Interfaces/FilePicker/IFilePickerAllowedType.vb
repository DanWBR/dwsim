Public Interface IFilePickerAllowedType
    ReadOnly Property Name As String
    ReadOnly Property AllowedExtensions As IEnumerable(Of String)
End Interface
