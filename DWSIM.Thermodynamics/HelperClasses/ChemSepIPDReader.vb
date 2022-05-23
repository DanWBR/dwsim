Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary

Public Class ChemSepIPDReader

    Private CAS_IDs As New Dictionary(Of String, String)

    Sub New()

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.CAS_IDs.txt")
            Using t As New IO.StreamReader(filestr)
                While Not t.EndOfStream
                    Dim line = t.ReadLine().Split(vbTab)
                    CAS_IDs.Add(line(0), line(1))
                End While
            End Using
        End Using

    End Sub

    Public Function ReadNRTLIPD() As List(Of PropertyPackages.Auxiliary.NRTL_IPData)

        Dim nrtlips As New List(Of NRTL_IPData)
        Dim ic = Globalization.CultureInfo.InvariantCulture

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.nrtl.ipd")
            Using t As New IO.StreamReader(filestr)
                Dim i As Integer = 0
                While Not t.EndOfStream
                    Dim line = t.ReadLine()
                    If i >= 16 And line.Length > 0 Then
                        Dim data = line.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)
                        If CAS_IDs.ContainsKey(data(0)) And CAS_IDs.ContainsKey(data(1)) Then
                            Dim ip1 As New NRTL_IPData()
                            ip1.ID1 = CAS_IDs(data(0))
                            ip1.ID2 = CAS_IDs(data(1))
                            ip1.A12 = Double.Parse(data(2), ic)
                            ip1.A21 = Double.Parse(data(3), ic)
                            ip1.alpha12 = Double.Parse(data(4), ic)
                            nrtlips.Add(ip1)
                        End If
                    End If
                    i += 1
                End While
            End Using
        End Using

        Return nrtlips

    End Function

    Public Function ReadUNIQUACIPD() As List(Of UNIQUAC_IPData)

        Dim uniquacips As New List(Of UNIQUAC_IPData)
        Dim ic = Globalization.CultureInfo.InvariantCulture

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.uniquac.ipd")
            Using t As New IO.StreamReader(filestr)
                Dim i As Integer = 0
                While Not t.EndOfStream
                    Dim line = t.ReadLine()
                    If i >= 18 And line.Length > 0 Then
                        Dim data = line.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)
                        If CAS_IDs.ContainsKey(data(0)) And CAS_IDs.ContainsKey(data(1)) Then
                            Dim ip1 As New UNIQUAC_IPData()
                            ip1.Name1 = CAS_IDs(data(0))
                            ip1.Name2 = CAS_IDs(data(1))
                            ip1.A12 = Double.Parse(data(2), ic)
                            ip1.A21 = Double.Parse(data(3), ic)
                            uniquacips.Add(ip1)
                        End If
                    End If
                    i += 1
                End While
            End Using
        End Using

        Return uniquacips

    End Function

End Class
