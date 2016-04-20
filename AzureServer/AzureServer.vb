'    DWSIM Flowsheet Solver Server for Microsoft Azure (TM) Service Bus
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
Imports System.Reflection
Imports System.Runtime.Serialization.Formatters.Binary

Module AzureServer

    Private nm As NamespaceManager
    Private qcc, qcs As QueueClient
    Private queueNameS As String = "DWSIMserver"
    Private queueNameC As String = "DWSIMclient"

    Private solutions As Dictionary(Of String, Byte())

    Dim ts As CancellationTokenSource

    Sub Main()

        solutions = New Dictionary(Of String, Byte())

        Console.WriteLine()
        Console.WriteLine("DWSIM - Open Source Process Simulator")
        Console.WriteLine("Microsoft Azure (TM) Service Bus Solver Server")
        Console.WriteLine(My.Application.Info.Copyright)
        Dim dt As DateTime = CType("01/01/2000", DateTime).AddDays(My.Application.Info.Version.Build).AddSeconds(My.Application.Info.Version.Revision * 2)
        Console.WriteLine("Version " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor & _
        ", Build " & My.Application.Info.Version.Build & " (" & Format(dt, "dd/MM/yyyy HH:mm") & ")")
        If Type.GetType("Mono.Runtime") Is Nothing Then
            Console.WriteLine("Microsoft .NET Framework Runtime Version " & System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion.ToString())
        Else
            Dim displayName As MethodInfo = Type.GetType("Mono.Runtime").GetMethod("GetDisplayName", BindingFlags.NonPublic Or BindingFlags.[Static])
            If displayName IsNot Nothing Then
                Console.WriteLine("Mono " + displayName.Invoke(Nothing, Nothing) + " / " + System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion.ToString())
            Else
                Console.WriteLine(System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion.ToString())
            End If
        End If
        Console.WriteLine()

        Dim connectionString As String

        If My.Application.CommandLineArgs.Count > 0 Then
            connectionString = My.Application.CommandLineArgs(0)
        Else
            Console.Write("Please enter the Service Bus connection string: ")
            connectionString = Console.ReadLine()
        End If

        Try

            nm = NamespaceManager.CreateFromConnectionString(connectionString)
            If Not nm.QueueExists(queueNameC) Then nm.CreateQueue(queueNameC)
            If Not nm.QueueExists(queueNameS) Then nm.CreateQueue(queueNameS)

            qcc = QueueClient.CreateFromConnectionString(connectionString, queueNameC)
            qcs = QueueClient.CreateFromConnectionString(connectionString, queueNameS)

            Console.WriteLine("[" & Date.Now.ToString & "] " & "Server is clearing messages on queues...")

            While (qcc.Peek() IsNot Nothing)
                Dim brokeredMessage = qcc.Receive(New TimeSpan(0, 0, 0))
                If Not brokeredMessage Is Nothing Then brokeredMessage.Complete()
            End While
            While (qcs.Peek() IsNot Nothing)
                Dim brokeredMessage = qcs.Receive(New TimeSpan(0, 0, 0))
                If Not brokeredMessage Is Nothing Then brokeredMessage.Complete()
            End While

            Console.WriteLine("[" & Date.Now.ToString & "] " & "Cleared ok")

            Console.WriteLine("[" & Date.Now.ToString & "] " & "Server is running and listening to incoming data on queue '" & queueNameC & "'...")

            Dim bytearr As New List(Of Byte())

            Dim icounter As Integer = 100

            While True

                If ts Is Nothing Then ts = New CancellationTokenSource
                Dim ct As CancellationToken = ts.Token

                Thread.Sleep(1000)

                Dim message As BrokeredMessage

                message = qcc.Receive(New TimeSpan(0, 0, 0))

                If Not message Is Nothing Then

                    If message.Properties("origin") = "client" Then

                        Dim requestID As String = message.Properties("requestID")

                        Try

                            If message.Properties("type") = "abort" Then

                                ts.Cancel()

                            ElseIf message.Properties("type") = "data" Then

                                Dim i, n As Integer

                                Dim bytes As Byte() = message.GetBody(Of Byte())()

                                message.Complete()

                                If message.Properties("multipart") = False Then

                                    i = 0
                                    n = 0

                                Else

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

                                    Console.WriteLine("[" & Date.Now.ToString & "] Data received with Request ID = " & requestID & ", flowsheet solving started!")
                                    Dim msg As New BrokeredMessage("Data received, flowsheet solving started!")
                                    msg.Properties.Add("requestID", requestID)
                                    msg.Properties.Add("type", "text")
                                    msg.Properties.Add("origin", "server")
                                    qcs.Send(msg)
                                    Task.Factory.StartNew(Sub()
                                                              ProcessData(bytes, requestID)
                                                          End Sub,
                                                          TaskCreationOptions.LongRunning,
                                                          ct).ContinueWith(Sub(t)
                                                                               If Not t Is Nothing Then
                                                                                   If Not t.Exception Is Nothing Then
                                                                                       Console.WriteLine("[" & Date.Now.ToString & "] " & t.Exception.Flatten().ToString)
                                                                                       msg = New BrokeredMessage(t.Exception.Flatten().ToString)
                                                                                       msg.Properties.Add("requestID", requestID)
                                                                                       msg.Properties.Add("type", "exception")
                                                                                       msg.Properties.Add("origin", "server")
                                                                                       qcs.Send(msg)
                                                                                   ElseIf t.IsCanceled Then
                                                                                       Console.WriteLine("[" & Date.Now.ToString & "] " & "Calculation aborted.")
                                                                                       msg = New BrokeredMessage("Calculation aborted.")
                                                                                       msg.Properties.Add("requestID", requestID)
                                                                                       msg.Properties.Add("type", "text")
                                                                                       msg.Properties.Add("origin", "server")
                                                                                       qcs.Send(msg)
                                                                                   End If
                                                                               End If
                                                                           End Sub,
                                                                           TaskContinuationOptions.OnlyOnFaulted).ContinueWith(Sub()
                                                                                                                                   ts.Dispose()
                                                                                                                                   ts = Nothing
                                                                                                                               End Sub)

                                End If

                            ElseIf message.Properties("type") = "connectioncheck" Then

                                message.Complete()

                                Dim msg As New BrokeredMessage("Server endpoint '" & My.User.Name & "@" & My.Computer.Name & "' is listening to requests")
                                msg.Properties.Add("requestID", requestID)
                                msg.Properties.Add("type", "connectioncheck")
                                msg.Properties.Add("origin", "server")
                                qcs.Send(msg)

                            End If

                        Catch ex As Exception

                            Dim msg As New BrokeredMessage(ex.ToString)
                            msg.Properties.Add("requestID", requestID)
                            msg.Properties.Add("type", "exception")
                            msg.Properties.Add("origin", "server")
                            qcs.Send(msg)

                        End Try

                    End If

                End If

                icounter += 1

                If Math.IEEERemainder(icounter, 100) = 0 Then

                    Dim binFormatter = New BinaryFormatter()
                    Dim mStream = New MemoryStream()
                    binFormatter.Serialize(mStream, solutions)

                    If mStream.Length > 100 * 1024 * 1024 Then
                        solutions.Clear()
                    End If

                End If

            End While

            qcc.Close()
            qcs.Close()

        Catch ex As Exception

            Console.WriteLine("[" & Date.Now.ToString & "] " & ex.ToString)

        End Try

    End Sub

    Sub ProcessData(bytes As Byte(), requestID As String)
        Using bytestream As New MemoryStream(bytes)
            Using form As FormFlowsheet = DWSIM.UnitOperations.UnitOperations.Flowsheet.InitializeFlowsheet(bytestream)
                If Not solutions.ContainsKey(form.Options.Key) Then
                    FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, 1, ts)
                    Using retbytes As MemoryStream = DWSIM.UnitOperations.UnitOperations.Flowsheet.ReturnProcessData(form)
                        Dim uncompressedbytes As Byte() = retbytes.ToArray
                        Using compressedstream As New MemoryStream()
                            Using gzs As New BufferedStream(New Compression.GZipStream(compressedstream, Compression.CompressionMode.Compress, True), 64 * 1024)
                                gzs.Write(uncompressedbytes, 0, uncompressedbytes.Length)
                                gzs.Close()
                                solutions.Add(form.Options.Key, compressedstream.ToArray)
                            End Using
                        End Using
                    End Using
                End If
                If solutions(form.Options.Key).Length < 220 * 1024 Then
                    Dim msg As New BrokeredMessage(solutions(form.Options.Key))
                    msg.Properties.Add("multipart", False)
                    msg.Properties.Add("requestID", requestID)
                    msg.Properties.Add("origin", "server")
                    msg.Properties.Add("type", "data")
                    qcs.Send(msg)
                    Console.WriteLine("[" & Date.Now.ToString & "] " & "Sent data to the queue: " & solutions(form.Options.Key).Length & " B, Request ID = " & requestID)
                Else
                    Dim i, n As Integer
                    Dim bytearray As ArrayList = Split(solutions(form.Options.Key), 220)
                    n = bytearray.Count
                    i = 1
                    For Each b As Byte() In bytearray
                        Dim msg As New BrokeredMessage(b)
                        msg.Properties.Add("multipart", True)
                        msg.Properties.Add("partnumber", i)
                        msg.Properties.Add("totalparts", n)
                        msg.Properties.Add("type", "data")
                        msg.Properties.Add("requestID", requestID)
                        msg.Properties.Add("origin", "server")
                        qcs.Send(msg)
                        i += 1
                        Console.WriteLine("[" & Date.Now.ToString & "] " & "Sent data to the queue: " & b.Length & " B, Request ID = " & requestID)
                    Next
                End If
            End Using
        End Using
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

End Module
