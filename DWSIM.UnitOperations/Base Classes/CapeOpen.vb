'    CAPE-OPEN Unit Operation Base Class
'    Copyright 2016 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.Runtime.InteropServices
Imports CapeOpen
Imports DWSIM.Interfaces.Interfaces2
Imports DWSIM.Thermodynamics
Imports System.Windows.Forms
Imports System.Runtime.Serialization.Formatters
Imports System.IO

Namespace UnitOperations.CAPEOPENWrappers

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class CapeOpenBase

        Inherits CapeOpen.CapeUnitBase

        Implements CapeOpen.ICapeUtilities, IPersistStreamInit, ICapeUnit

        Protected _sctxt As Object

        Public Shadows WriteOnly Property simulationContext As Object Implements ICapeUtilities.simulationContext
            Set(value As Object)
                _sctxt = value
            End Set
        End Property

        Public Overridable Shadows Sub Initialize() Implements ICapeUtilities.Initialize

            My.Application.ChangeUICulture("en")

            'handler for unhandled exceptions

            Try
                Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException)
                AddHandler Application.ThreadException, AddressOf UnhandledException
                AddHandler AppDomain.CurrentDomain.UnhandledException, AddressOf UnhandledException2
            Catch ex As Exception

            End Try

        End Sub

        Public Overridable Shadows Sub Terminate() Implements ICapeUtilities.Terminate

            If Not _sctxt Is Nothing Then
                If System.Runtime.InteropServices.Marshal.IsComObject(_sctxt) Then
                    System.Runtime.InteropServices.Marshal.ReleaseComObject(_sctxt)
                End If
            End If

            Me.simulationContext = Nothing

        End Sub

        Public Overrides Sub OnCalculate()

        End Sub

        Public MustOverride Shadows Sub Calculate() Implements CapeOpen.ICapeUnit.Calculate

        Public MustOverride Sub CreateParameters()

#Region "   Error Handling"

        Private Sub UnhandledException(ByVal sender As Object, ByVal e As System.Threading.ThreadExceptionEventArgs)

            Try
                Dim frmEx As New FormUnhandledException
                frmEx.TextBox1.Text = e.Exception.ToString
                frmEx.ex = e.Exception
                frmEx.ShowDialog()
            Finally
            End Try

        End Sub

        Private Sub UnhandledException2(ByVal sender As Object, ByVal e As System.UnhandledExceptionEventArgs)

            Try
                Dim frmEx As New FormUnhandledException
                frmEx.TextBox1.Text = e.ExceptionObject.ToString
                frmEx.ex = e.ExceptionObject
                frmEx.ShowDialog()
            Catch ex As Exception
            End Try

        End Sub


#End Region
  
#Region "   CAPE-OPEN Persistence Implementation"

        Protected m_dirty As Boolean = True

        Public Sub GetClassID(ByRef pClassID As System.Guid) Implements IPersistStreamInit.GetClassID
            pClassID = New Guid(CO_CustomUO.ClassId)
        End Sub

        Public Sub GetSizeMax(ByRef pcbSize As Long) Implements IPersistStreamInit.GetSizeMax
            pcbSize = 1024 * 1024
        End Sub

        Public Overridable Sub InitNew() Implements IPersistStreamInit.InitNew

        End Sub

        Public Function IsDirty() As Integer Implements IPersistStreamInit.IsDirty
            Return m_dirty
        End Function

        Public Sub Load(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream) Implements IPersistStreamInit.Load

            CreateParameters()

            ' Read the length of the string  
            Dim arrLen As Byte() = New [Byte](3) {}
            pStm.Read(arrLen, arrLen.Length, IntPtr.Zero)

            ' Calculate the length  
            Dim cb As Integer = BitConverter.ToInt32(arrLen, 0)

            ' Read the stream to get the string    
            Dim bytes As Byte() = New Byte(cb - 1) {}
            Dim pcb As New IntPtr()
            pStm.Read(bytes, bytes.Length, pcb)
            If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            ' Deserialize byte array    

            Dim memoryStream As New System.IO.MemoryStream(bytes)

            Try

                Dim domain As AppDomain = AppDomain.CurrentDomain
                AddHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

                Dim myarr As ArrayList

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                myarr = mySerializer.Deserialize(memoryStream)

                For i As Integer = 0 To myarr.Count - 1
                    If TypeOf Parameters(i) Is RealParameter Then
                        DirectCast(Parameters(i), RealParameter).SIValue = myarr(i)
                    ElseIf TypeOf Parameters(i) Is OptionParameter Then
                        DirectCast(Parameters(i), OptionParameter).Value = myarr(i)
                    ElseIf TypeOf Parameters(i) Is BooleanParameter Then
                        DirectCast(Parameters(i), BooleanParameter).Value = myarr(i)
                    ElseIf TypeOf Parameters(i) Is IntegerParameter Then
                        DirectCast(Parameters(i), IntegerParameter).Value = myarr(i)
                    End If
                Next

                myarr = Nothing
                mySerializer = Nothing

                RemoveHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            memoryStream.Close()

        End Sub

        Public Sub Save(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream, ByVal fClearDirty As Boolean) Implements IPersistStreamInit.Save

            Dim props As New ArrayList

            With props

                For i As Integer = 0 To Parameters.Count - 1
                    If TypeOf Parameters(i) Is RealParameter Then
                        props.Add(DirectCast(Parameters(i), RealParameter).SIValue)
                    ElseIf TypeOf Parameters(i) Is OptionParameter Then
                        props.Add(DirectCast(Parameters(i), OptionParameter).Value)
                    ElseIf TypeOf Parameters(i) Is BooleanParameter Then
                        props.Add(DirectCast(Parameters(i), BooleanParameter).Value)
                    ElseIf TypeOf Parameters(i) Is IntegerParameter Then
                        props.Add(DirectCast(Parameters(i), IntegerParameter).Value)
                    End If
                Next

            End With

            Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
            Dim mstr As New MemoryStream
            mySerializer.Serialize(mstr, props)
            Dim bytes As Byte() = mstr.ToArray()
            mstr.Close()

            ' construct length (separate into two separate bytes)    

            Dim arrLen As Byte() = BitConverter.GetBytes(bytes.Length)
            Try

                ' Save the array in the stream    
                pStm.Write(arrLen, arrLen.Length, IntPtr.Zero)
                pStm.Write(bytes, bytes.Length, IntPtr.Zero)
                If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            If fClearDirty Then
                m_dirty = False
            End If

        End Sub

        Protected Function MyResolveEventHandler(ByVal sender As Object, ByVal args As ResolveEventArgs) As System.Reflection.Assembly
            Return Me.[GetType]().Assembly
        End Function

#End Region

    End Class

End Namespace
