Option Strict On

Imports System.IO

''' <summary>
''' The TreeViewDataAccess class allows the nodes within a TreeView to be 
''' persisted to xml for later retrevial.
''' </summary>
Public Class TreeViewDataAccess

#Region "Structures"

    ''' <summary>
    ''' TreeViewData structure represents the root node collection of a TreeView
    ''' and provides the PopulateTreeView function to add these nodes to a specified
    ''' TreeView instance.
    ''' </summary>
    <Serializable()> Public Structure TreeViewData

        ''' <summary>Array of TreeNodeData objects representing the root nodes in a TreeView.</summary>
        Public Nodes() As TreeNodeData

        ''' <summary>
        ''' Creates new instance of the TreeViewData structure based from the 
        ''' specified TreeView.
        ''' </summary>
        ''' <param name="treeview">TreeView to build the TreeViewData instance from.</param>
        Public Sub New(ByVal treeview As TreeView)

            'Check to see if there are any root nodes in the TreeView
            If treeview.Nodes.Count = 0 Then Exit Sub

            'Populate the Nodes array with child nodes
            ReDim Nodes(treeview.Nodes.Count - 1)
            For i As Integer = 0 To treeview.Nodes.Count - 1
                Nodes(i) = New TreeNodeData(treeview.Nodes(i))
            Next
        End Sub

        ''' <summary>
        ''' Populates the specified TreeView with the current TreeViewData instance.
        ''' </summary>
        ''' <param name="treeview">TreeView instance to populate.</param>
        Public Sub PopulateTree(ByVal treeview As TreeView)
            'Check to see if there are any root nodes in the TreeViewData
            If Me.Nodes Is Nothing OrElse Me.Nodes.Length = 0 Then Exit Sub

            'Populate the TreeView with child nodes
            treeview.BeginUpdate()
            For i As Integer = 0 To Me.Nodes.Length - 1
                treeview.Nodes.Add(Me.Nodes(i).ToTreeNode)
            Next
            treeview.EndUpdate()
        End Sub

    End Structure

    ''' <summary>
    ''' TreeNodeData structure represents a TreeNode and provides the
    ''' ToTreeNode function to convert the instance to a TreeNode object.
    ''' </summary>
    <Serializable()> Public Structure TreeNodeData

        ''' <summary>String representing the Text property of the TreeNode.</summary>
        Public Text As String
        ''' <summary>String representing the Name property of the TreeNode.</summary>
        Public Name As String
        ''' <summary>Integer representing the ImageIndex property of the TreeNode.</summary>
        Public ImageIndex As Integer
        ''' <summary>Integer representing the SelectedImageIndex property of the TreeNode.</summary>
        Public SelectedImageIndex As Integer
        ''' <summary>Boolean representing the Checked state of the TreeNode.</summary>
        Public Checked As Boolean
        ''' <summary>Boolean representing the Expanded state of the TreeNode.</summary>
        Public Expanded As Boolean
        ''' <summary>Object representing the Tag property of the TreeNode.</summary>
        Public Tag As Object
        ''' <summary>Array of TreeNodeData objects representing the root nodes in a TreeView.</summary>
        Public Nodes() As TreeNodeData

        ''' <summary>
        ''' Creates new instance of the TreeNodeData structure based on the specified TreeNode.
        ''' </summary>
        ''' <param name="node">TreeNode to build the TreeNodeData instance from.</param>
        Public Sub New(ByVal node As TreeNode)
            'Set the basic TreeNode properties
            Me.Text = node.Text
            Me.Name = node.Name
            Me.ImageIndex = node.ImageIndex
            Me.SelectedImageIndex = node.SelectedImageIndex
            Me.Checked = node.Checked
            Me.Expanded = False 'node.IsExpanded

            'See if there is an object in the tag property and if it is serializable
            If (Not node.Tag Is Nothing) AndAlso node.Tag.GetType.IsSerializable Then Me.Tag = node.Tag

            'Check to see if there are any child nodes
            If node.Nodes.Count = 0 Then Exit Sub

            'Recurse through child nodes and add to Nodes array
            ReDim Nodes(node.Nodes.Count - 1)
            For i As Integer = 0 To node.Nodes.Count - 1
                Nodes(i) = New TreeNodeData(node.Nodes(i))
            Next
        End Sub

        ''' <summary>
        ''' Returns as TreeNode built from the instance of the TreeNodeData object.
        ''' </summary>
        Public Function ToTreeNode() As TreeNode
            'Create TreeNode based on instance of TreeNodeData and set basic properties
            ToTreeNode = New TreeNode(Me.Text, Me.ImageIndex, Me.SelectedImageIndex)
            ToTreeNode.Name = Me.Name
            ToTreeNode.Checked = Me.Checked
            ToTreeNode.Tag = Me.Tag
            If Me.Expanded Then ToTreeNode.Expand()

            'Recurse through child nodes adding to Nodes collection
            If Me.Nodes Is Nothing OrElse Me.Nodes.Length = 0 Then Exit Function
            For i As Integer = 0 To Me.Nodes.Length - 1
                ToTreeNode.Nodes.Add(Me.Nodes(i).ToTreeNode)
            Next
        End Function

    End Structure

#End Region

#Region "Public"

    ''' <summary>
    ''' Populates the specified TreeView from the serialized TreeViewData structure file specified.
    ''' </summary>
    ''' <param name="treeView">TreeView instance to populate.</param>
    ''' <param name="path">Serialized Xml representation of TreeViewData</param>
    Public Shared Sub LoadTreeViewData(ByVal treeView As TreeView, ByVal path As String)
        'Create as serializer and get the file to deserialize
        Dim ser As New System.Xml.Serialization.XmlSerializer(GetType(TreeViewData))
        Dim file As New System.IO.FileStream(path, IO.FileMode.Open)
        Dim reader As New System.Xml.XmlTextReader(file)

        'Deserialize the file and populate the treeview
        Dim treeData As TreeViewData = CType(ser.Deserialize(reader), TreeViewData)
        treeData.PopulateTree(treeView)

        'Tidy up
        reader.Close()
        file.Close()
        file = Nothing
    End Sub

    ''' <summary>
    ''' Populates the specified TreeView from the serialized TreeViewData structure file specified.
    ''' </summary>
    ''' <param name="treeView">TreeView instance to populate.</param>
    Public Shared Sub LoadTreeViewData(ByVal treeView As TreeView, ByRef st As MemoryStream)
        'Create as serializer and get the file to deserialize
        Dim ser As New System.Xml.Serialization.XmlSerializer(GetType(TreeViewData))
        Dim reader As New System.Xml.XmlTextReader(st)
        'Deserialize the file and populate the treeview
        Dim treeData As TreeViewData = CType(ser.Deserialize(reader), TreeViewData)
        treeData.PopulateTree(treeView)
        'Tidy up
        reader.Close()
    End Sub

    ''' <summary>
    ''' Saves the specified TreeView in serialized TreeViewData structure file specified.
    ''' </summary>
    ''' <param name="treeView">TreeView instance to save.</param>
    ''' <param name="path">Path to store serialized file.</param>
    Public Shared Sub SaveTreeViewData(ByVal treeView As TreeView, ByVal path As String)
        'Create as serializer and file to save TreeViewData
        Dim ser As New System.Xml.Serialization.XmlSerializer(GetType(TreeViewData))
        Dim file As New System.IO.FileStream(path, IO.FileMode.Create)
        Dim writer As New System.Xml.XmlTextWriter(file, Nothing)

        'Generate TreeViewData from TreeView and serialize the file.
        ser.Serialize(writer, New TreeViewData(treeView))

        'Tidy up
        writer.Close()
        file.Close()
        file = Nothing
    End Sub

    ''' <summary>
    ''' Saves the specified TreeView in serialized TreeViewData structure file specified.
    ''' </summary>
    ''' <param name="treeView">TreeView instance to save.</param>
    Public Shared Sub SaveTreeViewData(ByVal treeView As TreeView, ByRef st As MemoryStream)
        'Create as serializer and file to save TreeViewData
        Dim ser As New System.Xml.Serialization.XmlSerializer(GetType(TreeViewData))
        Dim writer As New System.Xml.XmlTextWriter(st, Nothing)
        'Generate TreeViewData from TreeView and serialize the file.
        ser.Serialize(writer, New TreeViewData(treeView))
        'Tidy up
        writer.Close()
    End Sub

#End Region

End Class



