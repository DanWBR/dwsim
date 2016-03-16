'    DWSIM Flowsheet Solver Client for Microsoft Azure (TM) Service Bus
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

Imports DWSIM
Imports System.IO
Imports System.Threading
Imports System.Threading.Tasks
Imports Microsoft.ServiceBus
Imports Microsoft.ServiceBus.Messaging
Imports System.Linq

Namespace DWSIM.Flowsheet
    Public Class AzureSolverClient

        Private nm As NamespaceManager
        Public qcc, qcs As QueueClient
        Private queueNameS As String = "DWSIMserver"
        Private queueNameC As String = "DWSIMclient"

        Public Sub SolveFlowsheet(fs As FormFlowsheet)

            'gets the service bus connection string 

            Dim connectionString As String = My.Settings.ServiceBusConnectionString
          
            Try

                fs.WriteToLog(DWSIM.App.GetLocalString("AzureClientConnectingtoSB"), Color.Brown, FormClasses.TipoAviso.Informacao)

                'creates the namespace manager instance

                nm = NamespaceManager.CreateFromConnectionString(connectionString)
                If Not nm.QueueExists(queueNameS) Then nm.CreateQueue(queueNameS)
                If Not nm.QueueExists(queueNameC) Then nm.CreateQueue(queueNameC)

                'gets the client and server queues.
                'the client queue is where the client messages come from.
                'the server queue is where the server messages go to.
                'clients listen to messages in the server queue, while the server listens to messages in the client queue.

                qcs = QueueClient.CreateFromConnectionString(connectionString, queueNameS)
                qcc = QueueClient.CreateFromConnectionString(connectionString, queueNameC)

                'process and delete any residual messages in the queues.

                While (qcc.Peek() IsNot Nothing)
                    Dim brokeredMessage = qcc.Receive(New TimeSpan(0, 0, 0))
                    If Not brokeredMessage Is Nothing Then brokeredMessage.Complete()
                End While
                While (qcs.Peek() IsNot Nothing)
                    Dim brokeredMessage = qcs.Receive(New TimeSpan(0, 0, 0))
                    If Not brokeredMessage Is Nothing Then brokeredMessage.Complete()
                End While

                'checks if there is a server listening in the client queue.

                Dim message As BrokeredMessage

                Dim requestID As String = Guid.NewGuid().ToString()

                Dim msg As New BrokeredMessage("")
                msg.Properties.Add("requestID", requestID)
                msg.Properties.Add("type", "connectioncheck")
                msg.Properties.Add("origin", "client")
                qcc.Send(msg)

                fs.WriteToLog(DWSIM.App.GetLocalString("AzureClientCheckingForServerOnEndpoint"), Color.Brown, FormClasses.TipoAviso.Informacao)

                message = qcs.Receive(New TimeSpan(0, 0, 20))
                If message Is Nothing Then
                    Throw New TimeoutException(DWSIM.App.GetLocalString("AzureClientNoServerOnEndpoint"))
                Else
                    fs.WriteToLog(DWSIM.App.GetLocalString("AzureClientServerFoundOnEndpoint"), Color.Brown, FormClasses.TipoAviso.Informacao)
                    message.Complete()
                End If

                If message.Properties("requestID") = requestID And message.Properties("origin") = "server" And message.Properties("type") = "connectioncheck" Then
                    fs.WriteToLog(DWSIM.App.GetLocalString("ClientMessageFromServer") & ": " & message.GetBody(Of String)(), Color.Brown, FormClasses.TipoAviso.Informacao)
                Else
                    Throw New TimeoutException(DWSIM.App.GetLocalString("SolverTimeout"))
                End If

                'send the flowsheet data to be calculated on the server.
            
                Dim tmpfile As String = My.Computer.FileSystem.GetTempFileName
                fs.WriteToLog(DWSIM.App.GetLocalString("ClientSavingTempFile"), Color.Brown, FormClasses.TipoAviso.Informacao)
                FormMain.SaveXML(tmpfile, fs)
                Dim uncompressedbytes As Byte() = IO.File.ReadAllBytes(tmpfile)
                File.Delete(tmpfile)

                'the byte array is compressed before sending to the client queue.
                'due the message size limit of 256 KB, the flowsheet data is divided in pieces and each part is sent to the queue sequentially, if required.  

                Using compressedstream As New MemoryStream()
                    Using gzs As New BufferedStream(New Compression.GZipStream(compressedstream, Compression.CompressionMode.Compress, True), 64 * 1024)
                        compressedstream.Position = 0
                        gzs.Write(uncompressedbytes, 0, uncompressedbytes.Length)
                        gzs.Close()
                        If compressedstream.Length < 220 * 1024 Then
                            fs.WriteToLog(DWSIM.App.GetLocalString("ClientSendingData") & " " & Math.Round(compressedstream.Length / 1024).ToString & " KB, request ID = " & requestID, Color.Brown, FormClasses.TipoAviso.Informacao)
                            msg = New BrokeredMessage(compressedstream.ToArray)
                            msg.Properties.Add("multipart", False)
                            msg.Properties.Add("requestID", requestID)
                            msg.Properties.Add("origin", "client")
                            msg.Properties.Add("type", "data")
                            qcc.Send(msg)
                        Else
                            Dim i, n As Integer
                            Dim bytearray As ArrayList = Split(compressedstream.ToArray, 220)
                            n = bytearray.Count
                            i = 1
                            For Each b As Byte() In bytearray
                                fs.WriteToLog(DWSIM.App.GetLocalString("ClientSendingData") & " " & Math.Round(b.Length / 1024).ToString & " KB, request ID = " & requestID, Color.Brown, FormClasses.TipoAviso.Informacao)
                                msg = New BrokeredMessage(b)
                                msg.Properties.Add("multipart", True)
                                msg.Properties.Add("partnumber", i)
                                msg.Properties.Add("totalparts", n)
                                msg.Properties.Add("requestID", requestID)
                                msg.Properties.Add("origin", "client")
                                msg.Properties.Add("type", "data")
                                qcc.Send(msg)
                                i += 1
                            Next
                        End If
                    End Using
                End Using

                Dim time As Integer = 0
                Dim sleeptime As Integer = 1

                'wait for the results from the server.

                fs.WriteToLog(DWSIM.App.GetLocalString("ClientWaitingForResults"), Color.Brown, FormClasses.TipoAviso.Informacao)

                Dim bytearr As New List(Of Byte())

                While (True)

                    'the user requested the calculation to be cancelled.

                    If My.Application.CalculatorStopRequested = True Then
                        My.Application.CalculatorStopRequested = False
                        msg = New BrokeredMessage("")
                        msg.Properties.Add("requestID", requestID)
                        msg.Properties.Add("type", "abort")
                        msg.Properties.Add("origin", "client")
                        qcc.Send(msg)
                    End If

                    Application.DoEvents()

                    'the server didn't sent an answer within the time limit.

                    Thread.Sleep(1000)
                    time += sleeptime
                    If time >= My.Settings.SolverTimeoutSeconds Then Throw New TimeoutException(DWSIM.App.GetLocalString("SolverTimeout"))

                    'the server sent us something, let's check what it is.

                    message = qcs.Receive(New TimeSpan(0, 0, 0))

                    If Not message Is Nothing Then

                        If message.Properties("requestID") = requestID And message.Properties("origin") = "server" Then

                            If message.Properties("type") = "data" Then

                                'the server sent us the results.

                                Dim bytes As Byte() = message.GetBody(Of Byte())()
                                message.Complete()

                                Dim i, n As Integer

                                If message.Properties("multipart") = False Then

                                    'all data came in a single message.

                                    i = 0
                                    n = 0

                                Else

                                    'the data came in multiple messages.

                                    i = message.Properties("partnumber")
                                    n = message.Properties("totalparts")

                                    If i = 1 Then
                                        bytearr.Clear()
                                        bytearr.Add(bytes)
                                    ElseIf i > 1 And i < n Then
                                        bytearr.Add(bytes)
                                    Else
                                        bytearr.Add(bytes)
                                        bytes = Combine(bytearr.ToArray)
                                    End If

                                End If

                                If i = n Then

                                    'the data was reconstructed/read from server message(s) correctly, let's update the flowsheet data.

                                    Try
                                        Using ms As New MemoryStream(bytes)
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

                                    Exit While

                                End If

                            ElseIf message.Properties("type") = "text" Then

                                'the server sent us a text to be displayed in the log window.

                                message.Complete()
                                fs.WriteToLog(DWSIM.App.GetLocalString("ClientMessageFromServer") & ": " & message.GetBody(Of String)(), Color.Brown, FormClasses.TipoAviso.Informacao)

                            ElseIf message.Properties("type") = "exception" Then

                                'something went wrong on server side.

                                message.Complete()
                                Throw New ServerErrorException(message.GetBody(Of String)())

                                Exit While

                            End If

                        End If

                    End If

                End While

            Catch ex As Exception
                Throw ex
            Finally
                qcs.Close()
                qcc.Close()
            End Try

        End Sub

        Private Function Split(filebytes As Byte(), partsizeKB As Integer) As ArrayList

            partsizeKB *= 1000

            Dim pos As Integer = 0
            Dim remaining As Integer

            Dim result As New ArrayList()

            remaining = filebytes.Length - pos

            While remaining > 0

                Dim block As Byte() = New Byte(Math.Min(remaining, partsizeKB) - 1) {}

                Array.Copy(filebytes, pos, block, 0, block.Length)
                result.Add(block)

                pos += block.Length
                remaining = filebytes.Length - pos

            End While

            Return result

        End Function

        Private Function Combine(ParamArray arrays As Byte()()) As Byte()

            Dim rv As Byte() = New Byte(arrays.Sum(Function(a) a.Length) - 1) {}
            Dim offset As Integer = 0
            For Each array As Byte() In arrays
                System.Buffer.BlockCopy(array, 0, rv, offset, array.Length)
                offset += array.Length
            Next
            Return rv

        End Function

    End Class

End Namespace
