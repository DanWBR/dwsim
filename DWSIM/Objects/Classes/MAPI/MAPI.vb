Imports System.Runtime.InteropServices
Imports System.IO

Namespace SendFileTo
    Class MAPI
        Public Function AddRecipientTo(ByVal email As String) As Boolean
            Return AddRecipient(email, howTo.MAPI_TO)
        End Function

        Public Function AddRecipientCC(ByVal email As String) As Boolean
            Return AddRecipient(email, howTo.MAPI_TO)
        End Function

        Public Function AddRecipientBCC(ByVal email As String) As Boolean
            Return AddRecipient(email, howTo.MAPI_TO)
        End Function

        Public Sub AddAttachment(ByVal strAttachmentFileName As String)
            m_attachments.Add(strAttachmentFileName)
        End Sub

        Public Function SendMailPopup(ByVal strSubject As String, ByVal strBody As String) As Integer
            Return SendMail(strSubject, strBody, MAPI_LOGON_UI Or MAPI_DIALOG)
        End Function

        Public Function SendMailDirect(ByVal strSubject As String, ByVal strBody As String) As Integer
            Return SendMail(strSubject, strBody, MAPI_LOGON_UI)
        End Function

        <DllImport("MAPI32.DLL")> _
         Private Shared Function MAPISendMail(ByVal sess As IntPtr, ByVal hwnd As IntPtr, ByVal message As MapiMessage, ByVal flg As Integer, ByVal rsv As Integer) As Integer
        End Function

        Private Function SendMail(ByVal strSubject As String, ByVal strBody As String, ByVal how As Integer) As Integer
            Dim msg As MapiMessage = New MapiMessage()
            msg.subject = strSubject
            msg.noteText = strBody

            msg.recips = GetRecipients(msg.recipCount)
            msg.files = GetAttachments(msg.fileCount)

            m_lastError = MAPISendMail(New IntPtr(0), New IntPtr(0), msg, how, 0)
            If m_lastError > 1 Then
                MessageBox.Show("MAPISendMail failed! " + GetLastError(), "MAPISendMail")
            End If

            Cleanup(msg)
            Return m_lastError
        End Function

        Private Function AddRecipient(ByVal email As String, ByVal howTo As howTo) As Boolean
            Dim recipient As MapiRecipDesc = New MapiRecipDesc()

            recipient.recipClass = CType(howTo, Integer)
            recipient.name = email
            m_recipients.Add(recipient)

            Return True
        End Function

        Private Function GetRecipients(ByRef recipCount As Integer) As IntPtr
            recipCount = 0
            If m_recipients.Count = 0 Then
                Return 0
            End If

            Dim size As Integer = Marshal.SizeOf(GetType(MapiRecipDesc))
            Dim intPtr As IntPtr = Marshal.AllocHGlobal(m_recipients.Count * size)

            Dim ptr As Integer = CType(intPtr, Integer)
            Dim mapiDesc As MapiRecipDesc
            For Each mapiDesc In m_recipients
                Marshal.StructureToPtr(mapiDesc, CType(ptr, IntPtr), False)
                ptr += size
            Next

            recipCount = m_recipients.Count
            Return intPtr
        End Function

        Private Function GetAttachments(ByRef fileCount As Integer) As IntPtr
            fileCount = 0
            If m_attachments Is Nothing Then
                Return 0
            End If

            If (m_attachments.Count <= 0) Or (m_attachments.Count > maxAttachments) Then
                Return 0
            End If

            Dim size As Integer = Marshal.SizeOf(GetType(MapiFileDesc))
            Dim intPtr As IntPtr = Marshal.AllocHGlobal(m_attachments.Count * size)

            Dim mapiFileDesc As MapiFileDesc = New MapiFileDesc()
            mapiFileDesc.position = -1
            Dim ptr As Integer = CType(intPtr, Integer)

            Dim strAttachment As String
            For Each strAttachment In m_attachments
                mapiFileDesc.name = Path.GetFileName(strAttachment)
                mapiFileDesc.path = strAttachment
                Marshal.StructureToPtr(mapiFileDesc, CType(ptr, IntPtr), False)
                ptr += size
            Next

            fileCount = m_attachments.Count
            Return intPtr
        End Function

        Private Sub Cleanup(ByRef msg As MapiMessage)
            Dim size As Integer = Marshal.SizeOf(GetType(MapiRecipDesc))
            Dim ptr As Integer = 0

            If msg.recips <> IntPtr.Zero Then
                ptr = CType(msg.recips, Integer)
                Dim i As Integer
                For i = 0 To msg.recipCount - 1 Step i + 1
                    Marshal.DestroyStructure(CType(ptr, IntPtr), GetType(MapiRecipDesc))
                    ptr += size
                Next
                Marshal.FreeHGlobal(msg.recips)
            End If

            If msg.files <> IntPtr.Zero Then
                size = Marshal.SizeOf(GetType(MapiFileDesc))

                ptr = CType(msg.files, Integer)
                Dim i As Integer
                For i = 0 To msg.fileCount - 1 Step i + 1
                    Marshal.DestroyStructure(CType(ptr, IntPtr), GetType(MapiFileDesc))
                    ptr += size
                Next
                Marshal.FreeHGlobal(msg.files)
            End If

            m_recipients.Clear()
            m_attachments.Clear()
            m_lastError = 0
        End Sub

        Public Function GetLastError() As String
            If m_lastError <= 26 Then
                Return errors(m_lastError)
            End If
            Return "MAPI error [" + m_lastError.ToString() + "]"
        End Function

        ReadOnly errors() As String = New String() {"OK [0]", "User abort [1]", _
            "General MAPI failure [2]", "MAPI login failure [3]", _
            "Disk full [4]", "Insufficient memory [5]", "Access denied [6]", _
            "-unknown- [7]", "Too many sessions [8]", _
            "Too many files were specified [9]", _
            "Too many recipients were specified [10]", _
            "A specified attachment was not found [11]", _
            "Attachment open failure [12]", "Attachment write failure [13]", _
            "Unknown recipient [14]", "Bad recipient type [15]", _
            "No messages [16]", "Invalid message [17]", "Text too large [18]", _
            "Invalid session [19]", "Type not supported [20]", _
            "A recipient was specified ambiguously [21]", _
            "Message in use [22]", "Network failure [23]", _
            "Invalid edit fields [24]", "Invalid recipients [25]", _
            "Not supported [26]"}

        Dim m_recipients As New List(Of MapiRecipDesc)
        Dim m_attachments As New List(Of String)
        Dim m_lastError As Integer = 0

        Private Const MAPI_LOGON_UI As Integer = &H1
        Private Const MAPI_DIALOG As Integer = &H8
        Private Const maxAttachments As Integer = 20

        Enum howTo
            MAPI_ORIG = 0
            MAPI_TO
            MAPI_CC
            MAPI_BCC
        End Enum

    End Class

    <StructLayout(LayoutKind.Sequential)> _
    Public Class MapiMessage
        Public reserved As Integer
        Public subject As String
        Public noteText As String
        Public messageType As String
        Public dateReceived As String
        Public conversationID As String
        Public flags As Integer
        Public originator As IntPtr
        Public recipCount As Integer
        Public recips As IntPtr
        Public fileCount As Integer
        Public files As IntPtr
    End Class

    <StructLayout(LayoutKind.Sequential)> _
    Public Class MapiFileDesc
        Public reserved As Integer
        Public flags As Integer
        Public position As Integer
        Public path As String
        Public name As String
        Public type As IntPtr
    End Class

    <StructLayout(LayoutKind.Sequential)> _
    Public Class MapiRecipDesc
        Public reserved As Integer
        Public recipClass As Integer
        Public name As String
        Public address As String
        Public eIDSize As Integer
        Public enTryID As IntPtr
    End Class
End Namespace