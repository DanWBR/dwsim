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

    Public Class ProcessedException

        Public Property ID As String = ""

        Public Property Name As String = ""

        Public Property Description As String = ""

        Public Property CodeLocation As String = ""

        Public Property CodeLocationDetails As String = ""

        Public Property UserAction As String = ""

        Public Property ExceptionObject As Exception = Nothing

    End Class

    Public Class ExceptionParser

        Public Shared Function ParseException(ex As Exception) As ProcessedException

            Dim iex As Exception = Nothing

            If TypeOf ex Is AggregateException Then
                iex = GetFirstException(ex)
            Else
                iex = ex
            End If

            Dim pex As New ProcessedException()



            Return pex

        End Function

        Private Shared Function GetFirstException(ex As AggregateException) As Exception

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


    End Class

End Namespace

