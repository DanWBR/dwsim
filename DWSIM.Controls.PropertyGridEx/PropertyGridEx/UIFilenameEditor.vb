
Imports System.ComponentModel
Imports System.Drawing.Design
Imports System.Windows.Forms

Namespace PropertyGridEx
    Public Class UIFilenameEditor
        Inherits System.Drawing.Design.UITypeEditor

        Public Overloads Overrides Function GetEditStyle(ByVal context As _
                        ITypeDescriptorContext) As UITypeEditorEditStyle
            If Not context Is Nothing AndAlso Not context.Instance Is Nothing Then
                If Not context.PropertyDescriptor.IsReadOnly Then
                    Return UITypeEditorEditStyle.Modal
                End If
            End If
            Return UITypeEditorEditStyle.None
        End Function

        <RefreshProperties(RefreshProperties.All)> _
        Public Overloads Overrides Function EditValue( _
                    ByVal context As ITypeDescriptorContext, _
                    ByVal provider As System.IServiceProvider, _
                    ByVal value As [Object]) As [Object]
            If context Is Nothing OrElse provider Is Nothing _
                    OrElse context.Instance Is Nothing Then
                Return MyBase.EditValue(provider, value)
            End If

            Dim fileDlg As FileDialog
            If context.PropertyDescriptor.Attributes(GetType(SaveFileAttribute)) Is Nothing Then
                fileDlg = New OpenFileDialog
            Else
                fileDlg = New SaveFileDialog
            End If
            fileDlg.Title = "Select " & context.PropertyDescriptor.DisplayName
            fileDlg.FileName = value

            Dim filterAtt As FileDialogFilterAttribute = _
                context.PropertyDescriptor.Attributes(GetType(FileDialogFilterAttribute))
            If Not filterAtt Is Nothing Then fileDlg.Filter = filterAtt.Filter
            If fileDlg.ShowDialog() = DialogResult.OK Then
                value = fileDlg.FileName
            End If
            fileDlg.Dispose()
            Return value
        End Function

        <AttributeUsage(AttributeTargets.Property)> _
    Public Class FileDialogFilterAttribute
            Inherits Attribute
            Private _filter As String

            Public ReadOnly Property Filter() As String
                Get
                    Return Me._filter
                End Get
            End Property

            Public Sub New(ByVal filter As String)
                MyBase.New()
                Me._filter = filter
            End Sub
        End Class

        <AttributeUsage(AttributeTargets.Property)> _
        Public Class SaveFileAttribute
            Inherits Attribute
        End Class

        Public Enum FileDialogType
            LoadFileDialog
            SaveFileDialog
        End Enum
    End Class
End Namespace
