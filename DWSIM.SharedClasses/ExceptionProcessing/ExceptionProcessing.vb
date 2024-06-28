'    Error (Exception) Processing for User-Friendly Feedback
'    Copyright 2018 Daniel Wagner O. de Medeiros
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

Namespace ExceptionProcessing

    Public Class ExceptionList

        Public Shared Exceptions As New Dictionary(Of String, Exception)

    End Class

    Public Class ProcessedException

        Public Property Name As String = ""

        Public Property OriginalDescription As String = ""

        Public Property DetailedDescription As String = ""

        Public Property CodeLocation As String = ""

        Public Property CodeLocationDetails As String = ""

        Public Property CallingMethod As String = ""

        Public Property UserAction As String = "Please search the Forums for related questions or fill a bug using the Bug Tracking System."

        Public Property ExceptionObject As Exception = Nothing

    End Class

    Public Class ExceptionParser

        Public Shared Function ParseException(ex As Exception) As ProcessedException

            Dim iex As Exception = Nothing

            If TypeOf ex Is AggregateException Then
                iex = GetFirstException(ex)
            Else
                If ex.InnerException IsNot Nothing Then
                    iex = ex.InnerException
                Else
                    iex = ex
                End If
            End If

            Dim pex As New ProcessedException()

            pex.Name = iex.GetType.Name
            pex.OriginalDescription = iex.Message

            If iex.Data.Contains("DetailedDescription") Then
                pex.DetailedDescription = iex.Data("DetailedDescription").ToString()
            Else
                pex.DetailedDescription = pex.OriginalDescription
            End If

            If iex.Data.Contains("UserAction") Then
                pex.UserAction = iex.Data("UserAction").ToString()
            Else
                pex.UserAction = "N/A"
            End If

            pex.ExceptionObject = iex

            If iex.StackTrace IsNot Nothing Then

                Try
                    Dim st As New StackTrace(iex, True)
                    pex.CodeLocation = st.GetFrame(0).GetMethod.Name + " (file '" + st.GetFrame(0).GetFileName + "', line " + st.GetFrame(0).GetFileLineNumber.ToString + ")"
                    pex.CallingMethod = st.GetFrame(1).GetMethod.Name + " (file '" + st.GetFrame(1).GetFileName + "', line " + st.GetFrame(1).GetFileLineNumber.ToString + ")"
                Catch
                End Try

            End If

            Return pex

        End Function

        Public Shared Function GetFirstException(ex As AggregateException) As Exception

            For Each iex In ex.InnerExceptions
                If TypeOf iex Is AggregateException Then
                    For Each iex2 In DirectCast(iex, AggregateException).InnerExceptions
                        If TypeOf iex2 Is AggregateException Then
                            For Each iex3 In DirectCast(iex2, AggregateException).InnerExceptions
                                If TypeOf iex3 Is AggregateException Then
                                    For Each iex4 In DirectCast(iex3, AggregateException).InnerExceptions
                                        If TypeOf iex4 Is AggregateException Then
                                            For Each iex5 In DirectCast(iex4, AggregateException).InnerExceptions
                                                Return iex5.GetBaseException
                                            Next
                                        Else
                                            Return iex3.GetBaseException
                                        End If
                                    Next
                                Else
                                    Return iex3.GetBaseException
                                End If
                            Next
                        Else
                            Return iex2.GetBaseException
                        End If
                    Next
                Else
                    Return iex.GetBaseException
                End If
            Next

            Return ex.GetBaseException

        End Function

        Public Shared Sub ProcessAndDisplayException(fsheet As IFlowsheet, exc As Exception)

            If TypeOf exc Is AggregateException Then
                Dim age = DirectCast(exc, AggregateException)
                Dim baseexception As Exception
                For Each ex In age.Flatten().InnerExceptions
                    Dim euid As String = Guid.NewGuid().ToString()
                    SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.Add(euid, ex)
                    If TypeOf ex Is AggregateException Then
                        baseexception = ex.InnerException
                        For Each iex In DirectCast(ex, AggregateException).Flatten().InnerExceptions
                            While iex.InnerException IsNot Nothing
                                baseexception = iex.InnerException
                            End While
                            fsheet?.ShowMessage(baseexception.Message.ToString, IFlowsheet.MessageType.GeneralError, euid)
                        Next
                    Else
                        baseexception = ex
                        If baseexception.InnerException IsNot Nothing Then
                            While baseexception.InnerException.InnerException IsNot Nothing
                                baseexception = baseexception.InnerException
                                If baseexception Is Nothing Then Exit While
                                If baseexception.InnerException Is Nothing Then Exit While
                            End While
                        End If
                        fsheet?.ShowMessage(baseexception.Message.ToString, IFlowsheet.MessageType.GeneralError, euid)
                    End If
                Next
            Else
                Dim euid As String = Guid.NewGuid().ToString()
                SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.Add(euid, exc)
                fsheet?.ShowMessage(exc.Message.ToString, IFlowsheet.MessageType.GeneralError, euid)
            End If

        End Sub

    End Class

End Namespace

