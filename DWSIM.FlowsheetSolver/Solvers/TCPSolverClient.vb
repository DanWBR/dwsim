'    DWSIM Network TCP Flowsheet Solver Client & Auxiliary Functions
'    Copyright 2015 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.IO
Imports System.Threading.Tasks
Imports System.Threading
Imports DWSIM.Interfaces
Imports DWSIM.GlobalSettings

Public Class TCPSolverClient

    Public Abort As Boolean = False
    Public ErrorMsg As String = ""
    Public client As TcpComm.Client
    Public lat As TcpComm.Utilities.LargeArrayTransferHelper

    Dim fsheet As Object

    Dim results As Byte()

    Public Sub SolveFlowsheet(fobj As Object)

        fsheet = fobj

        Dim fgui As IFlowsheetGUI = TryCast(fsheet, IFlowsheetGUI)
        Dim fbag As IFlowsheetBag = TryCast(fsheet, IFlowsheetBag)

        Dim errMsg As String = ""

        client = New TcpComm.Client(AddressOf Update, True, 30)

        Dim user, computer As String
        If Type.GetType("Mono.Runtime") Is Nothing Then
            user = My.User.Name
            computer = My.Computer.Name
        Else
            user = "user"
            computer = "computer"
        End If

        If client.Connect(Settings.ServerIPAddress, Settings.ServerPort, user & "@" & computer, errMsg) Then

            fgui.ShowMessage(fgui.GetTranslatedString("ClientConnected"), IFlowsheet.MessageType.Information)

            Dim tmpfile As String = My.Computer.FileSystem.GetTempFileName

            fgui.ShowMessage(fgui.GetTranslatedString("ClientSavingTempFile"), IFlowsheet.MessageType.Information)

            fbag.SaveToXML(tmpfile)

            results = Nothing

            lat = New TcpComm.Utilities.LargeArrayTransferHelper(client)

            Dim uncompressedbytes As Byte() = IO.File.ReadAllBytes(tmpfile)

            File.Delete(tmpfile)

            Using compressedstream As New MemoryStream()
                Using gzs As New BufferedStream(New Compression.GZipStream(compressedstream, Compression.CompressionMode.Compress, True), 64 * 1024)
                    compressedstream.Position = 0
                    gzs.Write(uncompressedbytes, 0, uncompressedbytes.Length)
                    gzs.Close()
                    fgui.ShowMessage(fgui.GetTranslatedString("ClientSendingData") & " " & Math.Round(compressedstream.Length / 1024).ToString & " KB", IFlowsheet.MessageType.Information)
                    If Not lat.SendArray(compressedstream.ToArray, 100, errMsg) Then
                        If errMsg.Trim <> "" Then fgui.ShowMessage(fgui.GetTranslatedString("ClientSendDataError") & ": " & errMsg, IFlowsheet.MessageType.GeneralError)
                    End If
                End Using
            End Using

            fgui.ShowMessage(fgui.GetTranslatedString("ClientSentDataOK"), IFlowsheet.MessageType.Information)

            Dim time As Integer = 0
            Dim sleeptime As Integer = 1
            While results Is Nothing
                Thread.Sleep(sleeptime * 1000)
                time += sleeptime
                If Settings.CalculatorStopRequested = True Then
                    Settings.CalculatorStopRequested = False
                    client.SendText("abort", 3)
                End If
                If Abort Then Throw New Exception(ErrorMsg)
                If time >= Settings.SolverTimeoutSeconds Then Throw New TimeoutException(fgui.GetTranslatedString("SolverTimeout"))
                fgui.ShowMessage(fgui.GetTranslatedString("ClientWaitingForResults"), IFlowsheet.MessageType.Information)
            End While

            Try
                Using ms As New MemoryStream(results)
                    Using decompressedstream As New IO.MemoryStream
                        Using gzs As New IO.BufferedStream(New Compression.GZipStream(ms, Compression.CompressionMode.Decompress, True), 64 * 1024)
                            gzs.CopyTo(decompressedstream)
                            gzs.Close()
                            fgui.ShowMessage(fgui.GetTranslatedString("ClientUpdatingData") & " " & Math.Round(decompressedstream.Length / 1024).ToString & " KB", IFlowsheet.MessageType.Information)
                            decompressedstream.Position = 0
                            Dim xdoc As XDocument = XDocument.Load(decompressedstream)
                            fbag.UpdateProcessData(xdoc)
                            fgui.ShowMessage(fgui.GetTranslatedString("ClientUpdatedDataOK"), IFlowsheet.MessageType.Information)
                        End Using
                    End Using
                End Using
            Catch ex As Exception
                fgui.ShowMessage(fgui.GetTranslatedString("ClientDataProcessingError") & ": " & ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
            End Try

        Else

            Throw New TimeoutException(fgui.GetTranslatedString("ClientConnectingError") & ": " & errMsg)

        End If

    End Sub

    Public Sub Update(ByVal bytes() As Byte, ByVal dataChannel As Byte)

        Dim fgui As IFlowsheetGUI = TryCast(fsheet, IFlowsheetGUI)
        Dim fbag As IFlowsheetBag = TryCast(fsheet, IFlowsheetBag)

        ' Use TcpComm.Utilities.LargeArrayTransferHelper to make it easier to send and receive 
        ' large arrays sent via lat.SendArray()
        ' The LargeArrayTransferHelperb will assemble an incoming large array
        ' on any channel we choose to evaluate, and pass it back to this callback
        ' when it is complete. Returns True if it has handled this incomming packet,
        ' so we exit the callback when it returns true.
        If Not lat Is Nothing AndAlso lat.HandleIncomingBytes(bytes, 100) Then Return

        If dataChannel = 100 Then

            ' This is a large array delivered by LAT. Display it in the 
            ' large transfer viewer form.
            results = bytes

            If client.isClientRunning Then client.Close()

        ElseIf dataChannel = 255 Then

            Dim msg As String = TcpComm.Utilities.BytesToString(bytes)
            If Not fsheet Is Nothing Then fgui.ShowMessage(fgui.GetTranslatedString("ClientMessageFromServer") & ": " & msg, IFlowsheet.MessageType.Information)

        ElseIf dataChannel = 2 Then

            Dim msg As String = TcpComm.Utilities.BytesToString(bytes)
            If Not fsheet Is Nothing Then fgui.ShowMessage(fgui.GetTranslatedString("ClientMessageFromServer") & ": " & msg, IFlowsheet.MessageType.Information)

        ElseIf dataChannel = 3 Then

            Dim msg As String = TcpComm.Utilities.BytesToString(bytes)
            Abort = True
            ErrorMsg = msg

        End If

    End Sub

End Class