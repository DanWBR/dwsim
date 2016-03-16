Imports System.ComponentModel
Imports System.Windows.Forms
Imports System.Windows.Forms.Design

Namespace PropertyGridEx
    <Serializable()> _
    Public Class CustomChoices
        Inherits ArrayList

        Public Sub New(ByVal array As ArrayList, ByVal IsSorted As Boolean)
            Me.AddRange(array)
            If IsSorted Then Me.Sort()
        End Sub

        Public Sub New(ByVal array As ArrayList)
            Me.AddRange(array)
        End Sub

        Public Sub New(ByVal array() As String, ByVal IsSorted As Boolean)
            Me.AddRange(array)
            If IsSorted Then Me.Sort()
        End Sub

        Public Sub New(ByVal array() As String)
            Me.AddRange(array)
        End Sub

        Public Sub New(ByVal array() As Integer, ByVal IsSorted As Boolean)
            Me.AddRange(array)
            If IsSorted Then Me.Sort()
        End Sub

        Public Sub New(ByVal array() As Integer)
            Me.AddRange(array)
        End Sub

        Public Sub New(ByVal array() As Double, ByVal IsSorted As Boolean)
            Me.AddRange(array)
            If IsSorted Then Me.Sort()
        End Sub

        Public Sub New(ByVal array() As Double)
            Me.AddRange(array)
        End Sub

        Public Sub New(ByVal array() As Object, ByVal IsSorted As Boolean)
            Me.AddRange(array)
            If IsSorted Then Me.Sort()
        End Sub

        Public Sub New(ByVal array() As Object)
            Me.AddRange(array)
        End Sub

        Public ReadOnly Property Items() As ArrayList
            Get
                Return Me
            End Get
        End Property

        Public Class CustomChoicesTypeConverter
            Inherits TypeConverter
            Private oChoices As CustomChoicesAttributeList = Nothing
            Public Overrides Function GetStandardValuesSupported(ByVal context As System.ComponentModel.ITypeDescriptorContext) As Boolean
                Dim Choices As CustomChoicesAttributeList = context.PropertyDescriptor.Attributes(GetType(CustomChoicesAttributeList))
                If Not oChoices Is Nothing Then Return True
                If Not Choices Is Nothing Then
                    oChoices = Choices
                    GetStandardValuesSupported = True
                Else
                    GetStandardValuesSupported = False
                End If
            End Function
            Public Overrides Function GetStandardValuesExclusive(ByVal context As System.ComponentModel.ITypeDescriptorContext) As Boolean
                Dim Choices As CustomChoicesAttributeList = context.PropertyDescriptor.Attributes(GetType(CustomChoicesAttributeList))
                If Not oChoices Is Nothing Then Return True
                If Not Choices Is Nothing Then
                    oChoices = Choices
                    GetStandardValuesExclusive = True
                Else
                    GetStandardValuesExclusive = False
                End If
            End Function
            Public Overrides Function GetStandardValues(ByVal context As System.ComponentModel.ITypeDescriptorContext) As System.ComponentModel.TypeConverter.StandardValuesCollection
                Dim Choices As CustomChoicesAttributeList = context.PropertyDescriptor.Attributes(GetType(CustomChoicesAttributeList))
                If Not oChoices Is Nothing Then
                    Return oChoices.Values
                End If
                Return MyBase.GetStandardValues(context)
            End Function
        End Class

        Public Class CustomChoicesAttributeList
            Inherits Attribute
            Private oList As New ArrayList

            Public ReadOnly Property Item() As ArrayList
                Get
                    Return Me.oList
                End Get
            End Property

            Public ReadOnly Property Values() As TypeConverter.StandardValuesCollection
                Get
                    Return New TypeConverter.StandardValuesCollection(Me.oList)
                End Get
            End Property

            Public Sub New(ByVal List() As String)
                MyBase.New()
                oList.AddRange(List)
            End Sub

            Public Sub New(ByVal List As ArrayList)
                MyBase.New()
                oList.AddRange(List)
            End Sub

            Public Sub New(ByVal List As ListBox.ObjectCollection)
                MyBase.New()
                oList.AddRange(List)
            End Sub
        End Class
    End Class
End Namespace
