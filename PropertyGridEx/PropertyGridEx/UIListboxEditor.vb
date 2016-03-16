Imports System.Windows.Forms
Imports System.Windows.Forms.Design
Imports System.ComponentModel
Imports System.Drawing
Imports System.Drawing.Design
Imports System.Globalization
Imports System.Reflection

Namespace PropertyGridEx

    Public Class UIListboxEditor
        Inherits UITypeEditor

        Private bIsDropDownResizable As Boolean = False
        Private WithEvents oList As New ListBox
        Private oSelectedValue As Object = Nothing
        Private oEditorService As IWindowsFormsEditorService

        Public Overloads Overrides Function GetEditStyle(ByVal context As _
        ITypeDescriptorContext) As UITypeEditorEditStyle
            If Not context Is Nothing AndAlso Not context.Instance Is Nothing Then
                Dim attribute As UIListboxIsDropDownResizable = context.PropertyDescriptor.Attributes(GetType(UIListboxIsDropDownResizable))
                If attribute IsNot Nothing Then
                    bIsDropDownResizable = True
                End If
                Return UITypeEditorEditStyle.DropDown
            End If
            Return UITypeEditorEditStyle.None
        End Function

        Public Overrides ReadOnly Property IsDropDownResizable() As Boolean
            Get
                Return bIsDropDownResizable
            End Get
        End Property

        <RefreshProperties(RefreshProperties.All)> _
        Public Overrides Function EditValue(ByVal context As System.ComponentModel.ITypeDescriptorContext, ByVal provider As System.IServiceProvider, ByVal value As Object) As Object
            If context Is Nothing OrElse provider Is Nothing _
            OrElse context.Instance Is Nothing Then
                Return MyBase.EditValue(provider, value)
            End If

            oEditorService = provider.GetService(GetType(IWindowsFormsEditorService))
            If oEditorService IsNot Nothing Then

                ' Get the Back reference to the Custom Property
                Dim oDescriptor As CustomProperty.CustomPropertyDescriptor = context.PropertyDescriptor
                Dim cp As CustomProperty = oDescriptor.CustomProperty

                ' Declare attributes
                Dim datasource As UIListboxDatasource
                Dim valuemember As UIListboxValueMember
                Dim displaymember As UIListboxDisplayMember

                ' Get attributes
                With context.PropertyDescriptor
                    datasource = .Attributes(GetType(UIListboxDatasource))
                    valuemember = .Attributes(GetType(UIListboxValueMember))
                    displaymember = .Attributes(GetType(UIListboxDisplayMember))
                End With

                With oList
                    .BorderStyle = BorderStyle.None
                    .IntegralHeight = True

                    If datasource IsNot Nothing Then
                        .DataSource = datasource.Value
                    End If

                    If displaymember IsNot Nothing Then
                        .DisplayMember = displaymember.Value
                    End If

                    If valuemember IsNot Nothing Then
                        .ValueMember = valuemember.Value
                    End If

                    If value IsNot Nothing Then
                        If value.GetType.Name = "String" Then
                            oList.Text = value
                        Else
                            oList.SelectedItem = value
                        End If
                    End If

                End With

                AddHandler oList.SelectedIndexChanged, AddressOf Me.SelectedItem

                oEditorService.DropDownControl(oList)
                If oList.SelectedIndices.Count = 1 Then
                    cp.SelectedItem = oList.SelectedItem
                    cp.SelectedValue = oSelectedValue
                    value = oList.Text
                End If
                oEditorService.CloseDropDown()
            Else
                Return MyBase.EditValue(provider, value)
            End If

            Return value

        End Function

        Private Sub SelectedItem(ByVal sender As Object, ByVal e As EventArgs)
            If oEditorService IsNot Nothing Then
                If oList.SelectedValue IsNot Nothing Then oSelectedValue = oList.SelectedValue
                oEditorService.CloseDropDown()
            End If
        End Sub

        Public Class UIListboxDatasource
            Inherits Attribute
            Private oDataSource As Object
            Public Sub New(ByRef Datasource As Object)
                oDataSource = Datasource
            End Sub
            Public ReadOnly Property Value() As Object
                Get
                    Return oDataSource
                End Get
            End Property
        End Class

        Public Class UIListboxValueMember
            Inherits Attribute
            Private sValueMember As String
            Public Sub New(ByVal ValueMember As String)
                sValueMember = ValueMember
            End Sub
            Public Property Value() As String
                Get
                    Return sValueMember
                End Get
                Set(ByVal value As String)
                    sValueMember = value
                End Set
            End Property
        End Class

        Public Class UIListboxDisplayMember
            Inherits Attribute
            Private sDisplayMember As String
            Public Sub New(ByVal DisplayMember As String)
                sDisplayMember = DisplayMember
            End Sub
            Public Property Value() As String
                Get
                    Return sDisplayMember
                End Get
                Set(ByVal value As String)
                    sDisplayMember = value
                End Set
            End Property

        End Class

        Public Class UIListboxIsDropDownResizable
            Inherits Attribute
        End Class

    End Class

End Namespace

