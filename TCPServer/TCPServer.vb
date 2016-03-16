'    DWSIM Network TCP Flowsheet Solver Server & Auxiliary Functions
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
Imports System.Threading
Imports System.Threading.Tasks
Imports DWSIM
Imports System.Reflection
Imports System.Runtime.Serialization.Formatters.Binary

Module TCPServer

    Private server As TcpComm.Server
    Private lat As TcpComm.Utilities.LargeArrayTransferHelper

    Private solutions As Dictionary(Of String, Byte())

    Dim ts As CancellationTokenSource

    Sub Main()

        solutions = New Dictionary(Of String, Byte())

        Console.WriteLine()
        Console.WriteLine("DWSIM - Open Source Process Simulator")
        Console.WriteLine("Network TCP/IP Solver Server")
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

        server = New TcpComm.Server(AddressOf Process)
        lat = New TcpComm.Utilities.LargeArrayTransferHelper(server)

        Dim port As Integer

        If My.Application.CommandLineArgs.Count > 0 Then
            port = My.Application.CommandLineArgs(0)
        Else
            Console.Write("Please enter the TCP Port Number to listen to: ")
            port = Console.ReadLine()
        End If

        server.Start(port)

        Console.WriteLine("[" & Date.Now.ToString & "] " & "Server IP Addresses:")
        For Each adr In System.Net.Dns.GetHostEntry(System.Net.Dns.GetHostName()).AddressList()
            Console.WriteLine(adr.ToString)
        Next
        Console.WriteLine()
        Console.WriteLine("[" & Date.Now.ToString & "] " & "Server is running and listening to incoming data on port " & port & "...")

        Dim icounter As Integer = 100

        While server.IsRunning

            Threading.Thread.Sleep(1000)

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

    End Sub

    Public Sub Process(ByVal bytes() As Byte, ByVal sessionID As Int32, ByVal dataChannel As Byte)

        ' Use TcpComm.Utilities.LargeArrayTransferHelper to make it easier to send and receive 
        ' large arrays sent via lat.SendArray()
        ' The LargeArrayTransferHelperb will assemble any number of incoming large arrays
        ' on any channel or from any sessionId, and pass them back to this callback
        ' when they are complete. Returns True if it has handled this incomming packet,
        ' so we exit the callback when it returns true.
        If lat.HandleIncomingBytes(bytes, 100, sessionID) Then Return

        If ts Is Nothing Then ts = New CancellationTokenSource

        If dataChannel = 100 Then

            Dim ct As CancellationToken = ts.Token

            Dim errmsg As String = ""

            Console.WriteLine("[" & Date.Now.ToString & "] " & "Data received from " & server.GetSession(sessionID).machineId & ", flowsheet solving started!")
            If Not server.SendText("Data received from " & server.GetSession(sessionID).machineId & ", flowsheet solving started!", 2, sessionID, errmsg) Then
                Console.WriteLine(errmsg)
            End If

            Task.Factory.StartNew(Sub()
                                      ProcessData(bytes, sessionID, dataChannel)
                                  End Sub, ct, TaskCreationOptions.LongRunning).ContinueWith(Sub(t)
                                                                                                 If Not t.Exception Is Nothing Then
                                                                                                     Console.WriteLine("[" & Date.Now.ToString & "] " & "Error solving flowsheet: " & t.Exception.ToString)
                                                                                                     errmsg = ""
                                                                                                     If Not server.SendText("Error solving flowsheet: " & t.Exception.ToString, 3, sessionID, errmsg) Then
                                                                                                         Console.WriteLine(errmsg)
                                                                                                     End If
                                                                                                 ElseIf t.IsCanceled Then
                                                                                                     Console.WriteLine("[" & Date.Now.ToString & "] " & "Calculation aborted.")
                                                                                                     errmsg = ""
                                                                                                     If Not server.SendText("Calculation aborted.", 2, sessionID, errmsg) Then
                                                                                                         Console.WriteLine(errmsg)
                                                                                                     End If
                                                                                                 End If
                                                                                             End Sub,
                                                        TaskContinuationOptions.OnlyOnFaulted).ContinueWith(Sub()
                                                                                                                Console.WriteLine("[" & Date.Now.ToString & "] " & "Closing current session with " & server.GetSession(sessionID).machineId & ".")
                                                                                                                errmsg = ""
                                                                                                                If Not server.SendText("Closing current session with " & server.GetSession(sessionID).machineId & ".", 2, sessionID, errmsg) Then
                                                                                                                    Console.WriteLine(errmsg)
                                                                                                                End If
                                                                                                                ts.Dispose()
                                                                                                                ts = Nothing
                                                                                                                server.GetSession(sessionID).Close()
                                                                                                            End Sub)

        ElseIf dataChannel = 3 Then

            If Not ts Is Nothing Then ts.Cancel()

        ElseIf dataChannel = 255 Then

            Dim tmp = ""
            Dim msg As String = TcpComm.Utilities.BytesToString(bytes)
            ' server has finished sending the bytes you put into sendBytes()
            If msg.Length > 3 Then tmp = msg.Substring(0, 3)
            If tmp = "UBS" Then ' User Bytes Sent.

            End If

        End If

    End Sub

    Sub ProcessData(bytes As Byte(), sessionid As Integer, datachannel As Byte)
        Dim errmsg As String = ""
        Using bytestream As New MemoryStream(bytes)
            Using form As FormFlowsheet = DWSIM.DWSIM.SimulationObjects.UnitOperations.Flowsheet.InitializeFlowsheet(bytestream)
                If Not solutions.ContainsKey(form.Options.Key) Then
                    DWSIM.DWSIM.Flowsheet.FlowsheetSolver.CalculateAll2(form, 1, ts)
                    Dim retbytes As MemoryStream = DWSIM.DWSIM.SimulationObjects.UnitOperations.Flowsheet.ReturnProcessData(form)
                    Using retbytes
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
                lat.SendArray(solutions(form.Options.Key), 100, sessionid, errmsg)
                Console.WriteLine("[" & Date.Now.ToString & "] " & "Byte array length: " & solutions(form.Options.Key).Length)
            End Using
        End Using
    End Sub


End Module
