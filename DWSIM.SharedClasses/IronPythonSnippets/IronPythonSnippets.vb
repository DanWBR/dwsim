Imports System.IO
Imports System.Reflection
Imports System.Xml

Namespace Scripts

    Public Class IronPythonSnippet

        Public Property Name As String = ""

        Public Property Category1 As String = ""

        Public Property Category2 As String = ""

        Public Property Scope As String = ""

        Public Property Snippet As String = ""

    End Class

    Public Class IronPythonSnippets

        Public Sub New()

        End Sub

        Private Function GetSnippetsXML() As XDocument


            Dim xmldoc = New XDocument()

            Using filestr As Stream = Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.SharedClasses.IronPythonSnippets.xml")
                xmldoc = XDocument.Load(filestr)
            End Using

            Return xmldoc

        End Function

        Public Function GetSnippets() As List(Of IronPythonSnippet)

            Dim list As New List(Of IronPythonSnippet)

            Dim xml = GetSnippetsXML()

            For Each node As XElement In xml.Elements.First.Elements

                Dim snippet As New IronPythonSnippet()
                snippet.Name = node.Elements("A").Value
                snippet.Category1 = node.Elements("B").Value
                snippet.Category2 = node.Elements("C").Value
                snippet.Scope = node.Elements("D").Value
                snippet.Snippet = node.Elements("E").Value

                list.Add(snippet)

            Next

            list.Remove(list.First)

            Return list

        End Function

    End Class

End Namespace
