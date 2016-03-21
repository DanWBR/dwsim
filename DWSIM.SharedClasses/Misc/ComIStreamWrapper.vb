Imports System.Runtime.InteropServices.ComTypes
Imports System.IO
Imports System.Runtime.InteropServices

<System.Serializable()> Public NotInheritable Class ComIStreamWrapper

    Implements IStream

    Private Const STG_E_INVALIDFUNCTION As Integer = &H80030001

    Public ReadOnly baseStream As Stream
    Private position As Long = -1

    Sub New(ByVal stream As Stream)
        baseStream = stream
    End Sub

    Private Sub SetSizeToPosition()
        If position <> -1 Then
            If position > baseStream.Length Then
                baseStream.SetLength(position)
            End If
            baseStream.Position = position
            position = -1
        End If
    End Sub

    Public Sub Read(ByVal pv As Byte(), ByVal cb As Integer, ByVal pcbRead As IntPtr) Implements IStream.Read
        Dim read__1 As Integer = 0

        If cb <> 0 Then
            SetSizeToPosition()
            read__1 = baseStream.Read(pv, 0, cb)
        End If

        If pcbRead <> IntPtr.Zero Then
            Marshal.WriteInt32(pcbRead, read__1)
        End If
    End Sub

    Public Sub Write(ByVal pv As Byte(), ByVal cb As Integer, ByVal pcbWritten As IntPtr) Implements IStream.Write
        If cb <> 0 Then
            SetSizeToPosition()
            baseStream.Write(pv, 0, cb)
        End If

        If pcbWritten <> IntPtr.Zero Then
            Marshal.WriteInt32(pcbWritten, cb)
        End If
    End Sub

    Public Sub Seek(ByVal dlibMove As Long, ByVal dwOrigin As Integer, ByVal plibNewPosition As IntPtr) Implements IStream.Seek
        Dim length As Long = baseStream.Length
        Dim newPosition As Long

        Select Case CType(dwOrigin, SeekOrigin)
            Case SeekOrigin.Begin
                newPosition = dlibMove
                Exit Select
            Case SeekOrigin.Current
                If position = -1 Then
                    newPosition = baseStream.Position + dlibMove
                Else
                    newPosition = position + dlibMove
                End If
                Exit Select
            Case SeekOrigin.[End]
                newPosition = length + dlibMove
                Exit Select
            Case Else
                Throw New ExternalException(Nothing, STG_E_INVALIDFUNCTION)
        End Select

        If newPosition > length Then
            position = newPosition
        Else
            baseStream.Position = newPosition
            position = -1
        End If

        If plibNewPosition <> IntPtr.Zero Then
            Marshal.WriteInt64(plibNewPosition, newPosition)
        End If
    End Sub

    Public Sub SetSize(ByVal libNewSize As Long) Implements IStream.SetSize
        baseStream.SetLength(libNewSize)
    End Sub

    Public Sub CopyTo(ByVal pstm As IStream, ByVal cb As Long, ByVal pcbRead As IntPtr, ByVal pcbWritten As IntPtr) Implements IStream.CopyTo
        Dim buffer As Byte()
        Dim written As Long = 0
        Dim read As Integer
        Dim count As Integer

        If cb <> 0 Then
            If cb < 4096 Then
                count = Convert.ToInt32(cb)
            Else
                count = 4096
            End If
            buffer = New Byte(count - 1) {}
            SetSizeToPosition()
            While True
                If (InlineAssignHelper(read, baseStream.Read(buffer, 0, count))) = 0 Then
                    Exit While
                End If
                pstm.Write(buffer, read, IntPtr.Zero)
                written += read
                If written >= cb Then
                    Exit While
                End If
                If cb - written < 4096 Then
                    count = Convert.ToInt32(cb - written)
                End If
            End While
        End If

        If pcbRead <> IntPtr.Zero Then
            Marshal.WriteInt64(pcbRead, written)
        End If
        If pcbWritten <> IntPtr.Zero Then
            Marshal.WriteInt64(pcbWritten, written)
        End If
    End Sub

    Public Sub Commit(ByVal grfCommitFlags As Integer) Implements IStream.Commit
        baseStream.Flush()
        SetSizeToPosition()
    End Sub

    Public Sub Revert() Implements IStream.Revert
        Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
    End Sub

    Public Sub LockRegion(ByVal libOffset As Long, ByVal cb As Long, ByVal dwLockType As Integer) Implements IStream.LockRegion
        Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
    End Sub

    Public Sub UnlockRegion(ByVal libOffset As Long, ByVal cb As Long, ByVal dwLockType As Integer) Implements IStream.UnlockRegion
        Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
    End Sub

    Public Sub Stat(ByRef pstatstg As System.Runtime.InteropServices.ComTypes.STATSTG, ByVal grfStatFlag As Integer) Implements IStream.Stat
        pstatstg = New System.Runtime.InteropServices.ComTypes.STATSTG()
        pstatstg.cbSize = baseStream.Length
    End Sub

    Public Sub Clone(ByRef ppstm As IStream) Implements IStream.Clone
        ppstm = Nothing
        Throw New Runtime.InteropServices.ExternalException(Nothing, STG_E_INVALIDFUNCTION)
    End Sub

    Private Shared Function InlineAssignHelper(Of T)(ByRef target As T, ByVal value As T) As T
        target = value
        Return value
    End Function

End Class

