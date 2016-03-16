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
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Threading.Tasks
Imports System.Threading

Namespace DWSIM.Flowsheet
    Public Class TCPSolverClient

        Public Abort As Boolean = False
        Public ErrorMsg As String = ""
        Public client As TcpComm.Client
        Public lat As TcpComm.Utilities.LargeArrayTransferHelper

        Dim fsheet As FormFlowsheet

        Dim results As Byte()

        Public Sub SolveFlowsheet(fs As FormFlowsheet)

            fsheet = fs

            Dim errMsg As String = ""

            client = New TcpComm.Client(AddressOf Update, True, 30)

            Dim user, computer As String
            If Not DWSIM.App.IsRunningOnMono Then
                user = My.User.Name
                computer = My.Computer.Name
            Else
                user = "user"
                computer = "computer"""
            End If

            If client.Connect(My.Settings.ServerIPAddress, My.Settings.ServerPort, user & "@" & computer, errMsg) Then

                fs.WriteToLog(DWSIM.App.GetLocalString("ClientConnected"), Color.Brown, FormClasses.TipoAviso.Informacao)

                Dim tmpfile As String = My.Computer.FileSystem.GetTempFileName

                fs.WriteToLog(DWSIM.App.GetLocalString("ClientSavingTempFile"), Color.Brown, FormClasses.TipoAviso.Informacao)

                FormMain.SaveXML(tmpfile, fs)

                results = Nothing

                lat = New TcpComm.Utilities.LargeArrayTransferHelper(client)

                Dim uncompressedbytes As Byte() = IO.File.ReadAllBytes(tmpfile)

                File.Delete(tmpfile)

                Using compressedstream As New MemoryStream()
                    Using gzs As New BufferedStream(New Compression.GZipStream(compressedstream, Compression.CompressionMode.Compress, True), 64 * 1024)
                        compressedstream.Position = 0
                        gzs.Write(uncompressedbytes, 0, uncompressedbytes.Length)
                        gzs.Close()
                        fs.WriteToLog(DWSIM.App.GetLocalString("ClientSendingData") & " " & Math.Round(compressedstream.Length / 1024).ToString & " KB", Color.Brown, FormClasses.TipoAviso.Informacao)
                        If Not lat.SendArray(compressedstream.ToArray, 100, errMsg) Then
                            If errMsg.Trim <> "" Then fs.WriteToLog(DWSIM.App.GetLocalString("ClientSendDataError") & ": " & errMsg, Color.Red, FormClasses.TipoAviso.Erro)
                        End If
                    End Using
                End Using

                fs.WriteToLog(DWSIM.App.GetLocalString("ClientSentDataOK"), Color.Brown, FormClasses.TipoAviso.Informacao)

                Dim time As Integer = 0
                Dim sleeptime As Integer = 1
                While results Is Nothing
                    Thread.Sleep(sleeptime * 1000)
                    Application.DoEvents()
                    time += sleeptime
                    If My.Application.CalculatorStopRequested = True Then
                        My.Application.CalculatorStopRequested = False
                        client.SendText("abort", 3)
                    End If
                    If Abort Then Throw New Exception(ErrorMsg)
                    If time >= My.Settings.SolverTimeoutSeconds Then Throw New TimeoutException(DWSIM.App.GetLocalString("SolverTimeout"))
                    fs.WriteToLog(DWSIM.App.GetLocalString("ClientWaitingForResults"), Color.Brown, FormClasses.TipoAviso.Informacao)
                End While

                Try
                    Using ms As New MemoryStream(results)
                        Using decompressedstream As New IO.MemoryStream
                            Using gzs As New IO.BufferedStream(New Compression.GZipStream(ms, Compression.CompressionMode.Decompress, True), 64 * 1024)
                                gzs.CopyTo(decompressedstream)
                                gzs.Close()
                                fs.WriteToLog(DWSIM.App.GetLocalString("ClientUpdatingData") & " " & Math.Round(decompressedstream.Length / 1024).ToString & " KB", Color.Brown, FormClasses.TipoAviso.Informacao)
                                decompressedstream.Position = 0
                                Dim xdoc As XDocument = XDocument.Load(decompressedstream)
                                DWSIM.SimulationObjects.UnitOps.Flowsheet.UpdateProcessData(fs, xdoc)
                                fs.WriteToLog(DWSIM.App.GetLocalString("ClientUpdatedDataOK"), Color.Brown, FormClasses.TipoAviso.Informacao)
                            End Using
                        End Using
                    End Using
                Catch ex As Exception
                    fs.WriteToLog(DWSIM.App.GetLocalString("ClientDataProcessingError") & ": " & ex.Message.ToString, Color.Red, FormClasses.TipoAviso.Erro)
                End Try

            Else

                Throw New TimeoutException(DWSIM.App.GetLocalString("ClientConnectingError") & ": " & errMsg)

            End If

        End Sub

        Public Sub Update(ByVal bytes() As Byte, ByVal dataChannel As Byte)

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
                If Not fsheet Is Nothing Then fsheet.WriteToLog(DWSIM.App.GetLocalString("ClientMessageFromServer") & ": " & msg, Color.Brown, FormClasses.TipoAviso.Informacao)

            ElseIf dataChannel = 2 Then

                Dim msg As String = TcpComm.Utilities.BytesToString(bytes)
                If Not fsheet Is Nothing Then fsheet.WriteToLog(DWSIM.App.GetLocalString("ClientMessageFromServer") & ": " & msg, Color.Brown, FormClasses.TipoAviso.Informacao)

            ElseIf dataChannel = 3 Then

                Dim msg As String = TcpComm.Utilities.BytesToString(bytes)
                Abort = True
                ErrorMsg = msg

            End If

        End Sub

    End Class

End Namespace
