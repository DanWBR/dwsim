Imports System.ComponentModel
Imports System.Drawing.Design
Imports System.Windows.Forms

Namespace PropertyGridEx
    Public Class UICustomEventEditor
        Inherits System.Drawing.Design.UITypeEditor

        Public Delegate Function OnClick(ByVal sender As Object, ByVal e As EventArgs) As Object
        Protected m_MethodDelegate As UICustomEventEditor.OnClick
        Protected m_sender As CustomProperty.CustomPropertyDescriptor

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
            If m_MethodDelegate Is Nothing Then
                Dim attr As DelegateAttribute = context.PropertyDescriptor.Attributes(GetType(DelegateAttribute))
                m_MethodDelegate = attr.GetMethod
            End If
            If m_sender Is Nothing Then
                m_sender = TryCast(context.PropertyDescriptor, CustomProperty.CustomPropertyDescriptor)
            End If
            Return m_MethodDelegate.Invoke(m_sender, Nothing)
        End Function

        <AttributeUsage(AttributeTargets.Property)> _
    Public Class DelegateAttribute
            Inherits Attribute
            Protected m_MethodDelegate As UICustomEventEditor.OnClick

            Public ReadOnly Property GetMethod() As UICustomEventEditor.OnClick
                Get
                    Return Me.m_MethodDelegate
                End Get
            End Property

            Public Sub New(ByVal MethodDelegate As UICustomEventEditor.OnClick)
                MyBase.New()
                Me.m_MethodDelegate = MethodDelegate
            End Sub
        End Class

    End Class
End Namespace
