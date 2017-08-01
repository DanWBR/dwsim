Imports System.Windows.Forms
Imports System.Drawing
Imports System.IO
Imports System.ComponentModel
Imports System.Reflection

Namespace PropertyGridEx

    <Designer(GetType(System.Windows.Forms.Design.ControlDesigner)), _
     ToolboxBitmap(GetType(PropertyGridEx))> _
    Public Class PropertyGridEx
        Inherits PropertyGrid

#Region "Protected variables and objects"

        ' CustomPropertyCollection assigned to MyBase.SelectedObject
        Protected oCustomPropertyCollection As CustomPropertyCollection
        Protected bShowCustomProperties As Boolean

        ' CustomPropertyCollectionSet assigned to MyBase.SelectedObjects
        Protected oCustomPropertyCollectionSet As CustomPropertyCollectionSet
        Protected bShowCustomPropertiesSet As Boolean

        ' Internal PropertyGrid Controls
        Protected oPropertyGridView As Object
        Protected oHotCommands As Object
        Protected oDocComment As Object
        Protected oToolStrip As ToolStrip

        ' Internal PropertyGrid Fields
        Protected oDocCommentTitle As Label
        Protected oDocCommentDescription As Label
        Protected oPropertyGridEntries As FieldInfo

        ' Properties variables
        Protected bAutoSizeProperties As Boolean
        Protected bDrawFlatToolbar As Boolean

#End Region

#Region "Public Functions"
        Public Sub New()
            ' This call is required by the Windows Form Designer.
            InitializeComponent()

            ' Add any initialization after the InitializeComponent() call.
            Me.DoubleBuffered = True

            ' Initialize collections
            oCustomPropertyCollection = New CustomPropertyCollection
            oCustomPropertyCollectionSet = New CustomPropertyCollectionSet

            If Not Type.GetType("Mono.Runtime") Is Nothing Then
                ' Attach internal controls
                oPropertyGridView = MyBase.GetType.BaseType.InvokeMember("property_grid_view", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                oHotCommands = Nothing 'MyBase.GetType.BaseType.InvokeMember("hotcommands", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                oToolStrip = MyBase.GetType.BaseType.InvokeMember("toolbar", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                oDocComment = MyBase.GetType.BaseType.InvokeMember("help_panel", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)

                ' Attach DocComment internal fields
                If oDocComment IsNot Nothing Then
                    oDocCommentTitle = MyBase.GetType.BaseType.InvokeMember("help_title_label", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                    oDocCommentDescription = MyBase.GetType.BaseType.InvokeMember("help_description_label", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                    'oDocCommentTitle = oDocComment.GetType.InvokeMember("help_title_label", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, oDocComment, Nothing)
                    'oDocCommentDescription = oDocComment.GetType.InvokeMember("help_description_label", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, oDocComment, Nothing)
                End If

                ' Attach PropertyGridView internal fields
                If oPropertyGridView IsNot Nothing Then
                    oPropertyGridEntries = oPropertyGridView.GetType.GetField("grid_items", BindingFlags.NonPublic Or BindingFlags.Instance Or BindingFlags.DeclaredOnly)
                End If
            Else
                ' Attach internal controls
                oPropertyGridView = MyBase.GetType.BaseType.InvokeMember("gridView", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                oHotCommands = MyBase.GetType.BaseType.InvokeMember("hotcommands", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                oToolStrip = MyBase.GetType.BaseType.InvokeMember("toolStrip", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)
                oDocComment = MyBase.GetType.BaseType.InvokeMember("doccomment", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, Me, Nothing)

                ' Attach DocComment internal fields
                If oDocComment IsNot Nothing Then
                    oDocCommentTitle = oDocComment.GetType.InvokeMember("m_labelTitle", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, oDocComment, Nothing)
                    oDocCommentDescription = oDocComment.GetType.InvokeMember("m_labelDesc", BindingFlags.NonPublic Or BindingFlags.GetField Or BindingFlags.Instance, Nothing, oDocComment, Nothing)
                End If

                ' Attach PropertyGridView internal fields
                If oPropertyGridView IsNot Nothing Then
                    oPropertyGridEntries = oPropertyGridView.GetType.GetField("allGridEntries", BindingFlags.NonPublic Or BindingFlags.Instance Or BindingFlags.DeclaredOnly)
                End If
            End If


            ' Apply Toolstrip style
            If oToolStrip IsNot Nothing Then
                ApplyToolStripRenderMode(bDrawFlatToolbar)
            End If

        End Sub

        Public Sub MoveSplitterTo(ByVal x As Integer)
            oPropertyGridView.GetType().InvokeMember("MoveSplitterTo", BindingFlags.InvokeMethod Or BindingFlags.NonPublic Or BindingFlags.Instance, Nothing, oPropertyGridView, New Object() {x})
        End Sub

        Public Overrides Sub Refresh()
            If bShowCustomPropertiesSet Then
                MyBase.SelectedObjects = oCustomPropertyCollectionSet.ToArray
            End If
            MyBase.Refresh()
            If bAutoSizeProperties Then AutoSizeSplitter()
        End Sub

        Public Sub SetComment(ByVal title As String, ByVal description As String)
            oDocComment.SetComment(title, description)
        End Sub

#End Region

#Region "Protected Functions"
        Protected Overrides Sub OnResize(ByVal e As System.EventArgs)
            MyBase.OnResize(e)
            If bAutoSizeProperties Then AutoSizeSplitter()
        End Sub

        Protected Sub AutoSizeSplitter(Optional ByVal RightMargin As Integer = 32)

            Dim oItemCollection As GridItemCollection = oPropertyGridEntries.GetValue(oPropertyGridView)
            If oItemCollection Is Nothing Then Exit Sub
            Dim oGraphics As Drawing.Graphics = Drawing.Graphics.FromHwnd(Me.Handle)
            Dim CurWidth As Integer = 0
            Dim MaxWidth As Integer = 0

            For Each oItem As GridItem In oItemCollection
                If oItem.GridItemType = GridItemType.Property Then
                    CurWidth = oGraphics.MeasureString(oItem.Label, Me.Font).Width + RightMargin
                    If CurWidth > MaxWidth Then
                        MaxWidth = CurWidth
                    End If
                End If
            Next

            MoveSplitterTo(MaxWidth)
        End Sub
        Protected Sub ApplyToolStripRenderMode(ByVal value As Boolean)
            If value Then
                oToolStrip.Renderer = New ToolStripSystemRenderer
            Else
                Dim Renderer As New ToolStripProfessionalRenderer(New CustomColorScheme())
                Renderer.RoundedEdges = False
                oToolStrip.Renderer = Renderer
            End If
        End Sub
#End Region

#Region "Properties"

        <Category("Behavior"), _
         DesignerSerializationVisibility(DesignerSerializationVisibility.Content), _
         DescriptionAttribute("Set the collection of the CustomProperty. Set ShowCustomProperties to True to enable it."), _
         RefreshProperties(RefreshProperties.Repaint)> _
        Public ReadOnly Property Item() As CustomPropertyCollection
            Get
                Return oCustomPropertyCollection
            End Get
        End Property

        <Category("Behavior"), _
         DesignerSerializationVisibility(DesignerSerializationVisibility.Content), _
         DescriptionAttribute("Set the CustomPropertyCollectionSet. Set ShowCustomPropertiesSet to True to enable it."), _
         RefreshProperties(RefreshProperties.Repaint)> _
        Public ReadOnly Property ItemSet() As CustomPropertyCollectionSet
            Get
                Return oCustomPropertyCollectionSet
            End Get
        End Property

        <Category("Behavior"), _
         DefaultValue(False), _
         DescriptionAttribute("Move automatically the splitter to better fit all the properties shown.")> _
        Public Property AutoSizeProperties() As Boolean
            Get
                Return bAutoSizeProperties
            End Get
            Set(ByVal Value As Boolean)
                bAutoSizeProperties = Value
                If Value Then AutoSizeSplitter()
            End Set
        End Property

        <Category("Behavior"), _
         DefaultValue(False), _
         DescriptionAttribute("Use the custom properties collection as SelectedObject."), _
         RefreshProperties(RefreshProperties.All)> _
        Public Property ShowCustomProperties() As Boolean
            Get
                Return bShowCustomProperties
            End Get
            Set(ByVal value As Boolean)
                If value = True Then
                    bShowCustomPropertiesSet = False
                    MyBase.SelectedObject = oCustomPropertyCollection
                End If
                bShowCustomProperties = value
            End Set
        End Property

        <Category("Behavior"), _
         DefaultValue(False), _
         DescriptionAttribute("Use the custom properties collections as SelectedObjects."), _
         RefreshProperties(RefreshProperties.All)> _
        Public Property ShowCustomPropertiesSet() As Boolean
            Get
                Return bShowCustomPropertiesSet
            End Get
            Set(ByVal value As Boolean)
                If value = True Then
                    bShowCustomProperties = False
                    MyBase.SelectedObjects = oCustomPropertyCollectionSet.ToArray
                End If
                bShowCustomPropertiesSet = value
            End Set
        End Property

        <Category("Appearance"), _
         DefaultValue(False), _
         DescriptionAttribute("Draw a flat toolbar")> _
        Public Overloads Property DrawFlatToolbar() As Boolean
            Get
                Return bDrawFlatToolbar
            End Get
            Set(ByVal value As Boolean)
                bDrawFlatToolbar = value
                ApplyToolStripRenderMode(bDrawFlatToolbar)
            End Set
        End Property

        <Category("Appearance"), _
         DisplayName("Toolstrip"), _
         DesignerSerializationVisibility(DesignerSerializationVisibility.Content), _
         DescriptionAttribute("Toolbar object"), _
         Browsable(True)> _
        Public ReadOnly Property ToolStrip() As ToolStrip
            Get
                Return oToolStrip
            End Get
        End Property

        <Category("Appearance"), _
         DisplayName("Help"), _
         DescriptionAttribute("DocComment object. Represent the comments area of the PropertyGrid."), _
         DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden), _
         Browsable(False)> _
        Public ReadOnly Property DocComment() As Control
            Get
                Return oDocComment
            End Get
        End Property

        <Category("Appearance"), _
         DisplayName("HelpTitle"), _
         DesignerSerializationVisibility(DesignerSerializationVisibility.Content), _
         DescriptionAttribute("Help Title Label."), _
         Browsable(True)> _
        Public ReadOnly Property DocCommentTitle() As Label
            Get
                Return oDocCommentTitle
            End Get
        End Property

        <Category("Appearance"), _
         DisplayName("HelpDescription"), _
         DesignerSerializationVisibility(DesignerSerializationVisibility.Content), _
         DescriptionAttribute("Help Description Label."), _
         Browsable(True)> _
        Public ReadOnly Property DocCommentDescription() As Label
            Get
                Return oDocCommentDescription
            End Get
        End Property

        <Category("Appearance"), _
         DisplayName("HelpImageBackground"), _
         DescriptionAttribute("Help Image Background.")> _
        Public Property DocCommentImage() As Image
            Get
                Return oDocComment.BackgroundImage
            End Get
            Set(ByVal value As Image)
                oDocComment.BackgroundImage = value
            End Set
        End Property

#End Region

    End Class

End Namespace

