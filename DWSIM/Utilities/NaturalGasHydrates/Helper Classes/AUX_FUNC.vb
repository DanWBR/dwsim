'    Hydrate Calculation Routines
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports System.Linq

Namespace DWSIM.Utilities.HYD

    Public Class AuxMethods

        Public MAT_UNIFAC(60, 35)
        Public MAT_UNIFAC_ELL(60, 35)
        Public MAT_PROPS(47, 26)
        Public MAT_PROPS2(40, 27)
        Public MAT_KIJ(38, 38)
        Public MAT_KIJ_PRSV(38, 38)
        Public MAT_KAPPA1(37, 1)
        Public MAT_DIEL(37, 2)
        Public MAT_SLOAN1(93, 15)
        Public MAT_SLOAN2(81, 9)
        Public MAT_SLOAN3(53, 7)
        Public MAT_CHENGUO(35, 6)
        Public MAT_KLAUDASANDLER(53, 7)
        Public MAT_VDWP_PP(53, 7)
        Public MAT_MOD_LIFAC(73, 47)
        Public MAT_MOD_LIFAC_BIJ(22, 22)
        Public MAT_MOD_LIFAC_CIJ(22, 22)
        Public MAT_INIB(46, 2)

        Sub New()

            READ_DIEL()
            READ_INIB()
            READ_CHENGUO()
            READ_KLAUDASANDLER()
            READ_VDWP_PP()

        End Sub

        Function INDICE(ByVal name As String)

            INDICE = -1
            Dim i = 0
            Do
                If MAT_INIB(i, 0) = name Then INDICE = i + 1
                i = i + 1
            Loop Until i = 47

        End Function

        Function STRU(ByVal name As String)

            STRU = 0
            Dim i = 0
            Do
                If MAT_INIB(i, 0) = name Then STRU = Convert.ToInt32(MAT_INIB(i, 2))
                i = i + 1
            Loop Until i = 47

        End Function

        Function ISINIB(ByVal name As String)

            ISINIB = 0
            Dim i = 0
            Do
                If MAT_INIB(i, 0) = name Then ISINIB = Convert.ToInt32(MAT_INIB(i, 1))
                i = i + 1
            Loop Until i = 47

        End Function

        Function CHARGE(ByVal id As Integer)

            CHARGE = 0
            If id = 39 Then CHARGE = 1
            If id = 40 Then CHARGE = 1
            If id = 41 Then CHARGE = 2
            If id = 42 Then CHARGE = -1
            If id = 43 Then CHARGE = -1
            If id = 44 Then CHARGE = -1

        End Function

        Function POS(ByVal Vids, ByVal i)

            POS = 0
            Dim j = 0
            Do
                If Vids(j) = i Then POS = j
                j = j + 1
            Loop Until j = UBound(Vids) + 1

        End Function

        Function READ_CHENGUO()


            Dim l, k As Integer
            Dim linha_atual As String() = New String() {}

            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Using filestr As IO.Stream = calculatorassembly.GetManifestResourceStream("DWSIM.Thermodynamics.hid_chenguo.dat")
                Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filestr)
                    MyReader.TextFieldType = FileIO.FieldType.Delimited
                    MyReader.SetDelimiters(";")
                    l = 0
                    While Not MyReader.EndOfData
                        linha_atual = MyReader.ReadFields()
                        k = 0
                        Do
                            MAT_CHENGUO(l, k) = Val(linha_atual(k))
                            k = k + 1
                        Loop Until k = 7
                        l = l + 1
                    End While
                End Using
            End Using


            READ_CHENGUO = 1

        End Function

        Function READ_KLAUDASANDLER()

            Dim l, k As Integer
            Dim linha_atual As String() = New String() {}
            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Using filestr As IO.Stream = calculatorassembly.GetManifestResourceStream("DWSIM.Thermodynamics.hid_klaudasandler.dat")
                Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filestr)
                    MyReader.TextFieldType = FileIO.FieldType.Delimited
                    MyReader.SetDelimiters(";")
                    l = 0
                    While Not MyReader.EndOfData
                        linha_atual = MyReader.ReadFields()
                        k = 0
                        Do
                            MAT_KLAUDASANDLER(l, k) = Val(linha_atual(k))
                            k = k + 1
                        Loop Until k = 8
                        l = l + 1
                    End While
                End Using
            End Using

            READ_KLAUDASANDLER = 1

        End Function

        Function READ_VDWP_PP()

            Dim l, k As Integer
            Dim linha_atual As String() = New String() {}
            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Using filestr As IO.Stream = calculatorassembly.GetManifestResourceStream("DWSIM.Thermodynamics.hid_vdwp_pp.dat")
                Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filestr)
                    MyReader.TextFieldType = FileIO.FieldType.Delimited
                    MyReader.SetDelimiters(";")
                    l = 0
                    While Not MyReader.EndOfData
                        linha_atual = MyReader.ReadFields()
                        k = 0
                        Do
                            MAT_VDWP_PP(l, k) = Val(linha_atual(k))
                            k = k + 1
                        Loop Until k = 8
                        l = l + 1
                    End While
                End Using
            End Using

            READ_VDWP_PP = 1

        End Function

        Function READ_DIEL()

            Dim l, j As Integer
            Dim linha_atual As String() = New String() {}

            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Using filestr As IO.Stream = calculatorassembly.GetManifestResourceStream("DWSIM.Thermodynamics.diel.dat")
                Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filestr)
                    MyReader.TextFieldType = FileIO.FieldType.Delimited
                    MyReader.SetDelimiters(";")
                    l = 0
                    While Not MyReader.EndOfData
                        linha_atual = MyReader.ReadFields()
                        j = 0
                        Do
                            MAT_DIEL(l, j) = Val(linha_atual(j))
                            j = j + 1
                        Loop Until j = 3
                        l = l + 1
                    End While
                End Using
            End Using

            READ_DIEL = 1

        End Function

        Function READ_INIB()

            Dim l, j As Integer
            Dim linha_atual As String() = New String() {}

            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Using filestr As IO.Stream = calculatorassembly.GetManifestResourceStream("DWSIM.Thermodynamics.inib.dat")
                Using MyReader2 As New Microsoft.VisualBasic.FileIO.TextFieldParser(filestr)
                    MyReader2.TextFieldType = FileIO.FieldType.Delimited
                    MyReader2.SetDelimiters(";")
                    l = 0
                    While Not MyReader2.EndOfData
                        linha_atual = MyReader2.ReadFields()
                        j = 0
                        Do
                            MAT_INIB(l, j) = Val(linha_atual(j))
                            j = j + 1
                        Loop Until j = 3
                        l = l + 1
                    End While
                End Using
            End Using

            READ_INIB = 1

        End Function

        Function GET_HS_KS(ByVal id)

            Dim i, tmp(3)
            i = 0

            If id = 1 Then i = 29
            If id = 2 Then i = 30
            If id = 3 Then i = 31
            If id = 4 Then i = 32
            If id = 14 Then i = 33
            If id = 16 Then i = 34
            If id = 15 Then i = 35
            If id = 38 Then i = 36
            Try
                tmp(0) = Convert.ToDouble(MAT_KLAUDASANDLER(i, 1))
                tmp(1) = Convert.ToDouble(MAT_KLAUDASANDLER(i, 2))
                tmp(2) = Convert.ToDouble(MAT_KLAUDASANDLER(i, 3))
                tmp(3) = Convert.ToDouble(MAT_KLAUDASANDLER(i, 4))
            Catch
                tmp(0) = -1.0E+32
                tmp(1) = -1.0E+32
                tmp(2) = -1.0E+32
                tmp(3) = -1.0E+32
            End Try
            GET_HS_KS = tmp

        End Function

        Function RetornarIDsParaCalculoDeHidratos(ByVal nomes As String())

            Dim res As ArrayList = New ArrayList()

            Dim str As String
            For Each str In nomes
                If str = "Metano" Or str = "Methane" Then
                    res.Add(1)
                ElseIf str = "Etano" Or str = "Ethane" Then
                    res.Add(2)
                ElseIf str = "Propano" Or str = "Propane" Then
                    res.Add(3)
                ElseIf str = "nButano" Or str = "N-butane" Then
                    res.Add(5)
                ElseIf str = "iButano" Or str = "Isobutane" Then
                    res.Add(4)
                ElseIf str = "nPentano" Or str = "N-pentane" Then
                    res.Add(7)
                ElseIf str = "iPentano" Or str = "Isopentane" Then
                    res.Add(6)
                ElseIf str = "nHexano" Or str = "N-hexane" Then
                    res.Add(8)
                ElseIf str = "nHeptano" Or str = "N-heptane" Then
                    res.Add(9)
                ElseIf str = "nOctano" Or str = "N-octane" Then
                    res.Add(10)
                ElseIf str = "nNonano" Or str = "N-nonane" Then
                    res.Add(11)
                ElseIf str = "nDecano" Or str = "N-decane" Then
                    res.Add(12)
                ElseIf str = "Oxigenio" Or str = "Oxygen" Then
                    res.Add(17)
                ElseIf str = "Nitrogenio" Or str = "Nitrogen" Then
                    res.Add(16)
                ElseIf str = "Agua" Or str = "Water" Then
                    res.Add(13)
                ElseIf str = "DioxidoDeCarbono" Or str = "Carbon dioxide" Then
                    res.Add(15)
                ElseIf str = "SulfetoDeHidrogenio" Or str = "Hydrogen sulfide" Then
                    res.Add(14)
                ElseIf str.Contains("ol") Then
                    res.Add(100)
                Else
                    res.Add(0)
                End If
            Next

            Return res.ToArray()

        End Function

    End Class

End Namespace
