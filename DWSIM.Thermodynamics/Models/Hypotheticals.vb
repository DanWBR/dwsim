Imports Microsoft.VisualBasic.FileIO
Imports System.IO

'    Hypotheticals Calculation Routines 
'    Copyright 2008/2013 Daniel Wagner O. de Medeiros
'              2013 Gregor Reichert   
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


Namespace Utilities.Hypos.Methods

    <System.Serializable()> Public Class Joback

        Public ElementLines, JOBACKlines, UNIFACLines, MODFACLines As New List(Of String)

        Protected m_Jgroups As System.Collections.Generic.Dictionary(Of Integer, JobackGroup)
        Protected m_JElements As System.Collections.Generic.Dictionary(Of String, Element)
        Sub New()

            Dim pathsep = System.IO.Path.DirectorySeparatorChar
            Dim i As Integer

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.unifac.txt")
                Using parser As New TextFieldParser(filestr)
                    parser.ReadLine()
                    parser.ReadLine()
                    While Not parser.EndOfData
                        UNIFACLines.Add(parser.ReadLine)
                    End While
                End Using
            End Using

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.modfac.txt")
                Using parser As New TextFieldParser(filestr)
                    parser.ReadLine()
                    While Not parser.EndOfData
                        MODFACLines.Add(parser.ReadLine)
                    End While
                End Using
            End Using

            'Load Elements data
            m_JElements = New System.Collections.Generic.Dictionary(Of String, Element)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.Elements.txt")
                Using parser As New TextFieldParser(filestr)
                    While Not parser.EndOfData
                        ElementLines.Add(parser.ReadLine)
                    End While
                End Using
            End Using

            For i = 1 To ElementLines.Count - 1
                With ElementLines(i)
                    Dim El As New Element
                    'ID;Name;Symbol;MW
                    El.ID = .Split(";")(0)
                    El.ElementName = .Split(";")(1)
                    El.ElementSymbol = .Split(";")(2)
                    El.MW = Double.Parse(.Split(";")(3), Globalization.CultureInfo.InvariantCulture)
                    Me.JElements.Add(El.ElementSymbol, El)
                End With
            Next

            'Load Joback-groups data
            m_Jgroups = New System.Collections.Generic.Dictionary(Of Integer, JobackGroup)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.JobackGroups.txt")
                Using parser As New TextFieldParser(filestr)
                    While Not parser.EndOfData
                        JOBACKlines.Add(parser.ReadLine)
                    End While
                End Using
            End Using

            For i = 2 To JOBACKlines.Count - 1
                With JOBACKlines(i)
                    If Not .Split(";")(0) = "X" Then
                        Dim JG As New JobackGroup

                        'ID;Group;Tc;Pc;Vc;Tb;Tm;Hf;Gf;a;b;c;d;Hm;Hv;DVa;DVb;MW;Na;Elements
                        JG.ID = .Split(";")(0)
                        JG.Group = .Split(";")(1)
                        JG.TC = CheckEmptyCell(.Split(";")(2))
                        JG.PC = CheckEmptyCell(.Split(";")(3))
                        JG.VC = CheckEmptyCell(.Split(";")(4))
                        JG.TB = CheckEmptyCell(.Split(";")(5))
                        JG.TF = CheckEmptyCell(.Split(";")(6))
                        JG.DH = CheckEmptyCell(.Split(";")(7))
                        JG.DG = CheckEmptyCell(.Split(";")(8))
                        JG.A = CheckEmptyCell(.Split(";")(9))
                        JG.B = CheckEmptyCell(.Split(";")(10))
                        JG.C = CheckEmptyCell(.Split(";")(11))
                        JG.D = CheckEmptyCell(.Split(";")(12))
                        JG.HF = CheckEmptyCell(.Split(";")(13))
                        JG.NA = CheckEmptyCell(.Split(";")(17))
                        JG.Elements = .Split(";")(18)

                        Me.JGroups.Add(JG.ID, JG)
                    End If
                End With
            Next
        End Sub
        Public ReadOnly Property JGroups() As System.Collections.Generic.Dictionary(Of Integer, JobackGroup)
            Get
                Return m_Jgroups
            End Get
        End Property
        Public ReadOnly Property JElements() As System.Collections.Generic.Dictionary(Of String, Element)
            Get
                Return m_JElements
            End Get
        End Property
        Function CheckEmptyCell(ByVal val As String) As String
            If val = "X" Or val = "" Then
                CheckEmptyCell = Nothing
            Else
                CheckEmptyCell = Double.Parse(val, Globalization.CultureInfo.InvariantCulture)
            End If
        End Function



        Public Function GetACLFromUNIFAC(unifac As Integer()) As Dictionary(Of String, Integer)

            'fill Joback groups table with UNIFAC subgoups
            Dim k, usgc, usgid As Integer
            Dim JSG, JG2 As String, JG As New Dictionary(Of Integer, Integer)

            For Each j As JobackGroup In JGroups.Values
                JG.Add(j.ID, 0)
            Next

            Dim i As Integer = 0
            For Each r As Integer In unifac
                'Joback groups from UNIFAC subgroups
                JG2 = UNIFACLines(i).Split(","c)(8) 'Joback Subgroup List
                For k = 0 To 3
                    JSG = JG2.Split("/"c)(k)
                    If Not JSG = "" Then
                        usgc = CInt(JSG.Split(":"c)(0)) 'Joback subgroup count
                        usgid = CInt(JSG.Split(":"c)(1)) 'Joback subgroup ID
                        JG(usgid) += usgc * r
                    End If
                Next
                i += 1
            Next

            Dim ACL As New Dictionary(Of String, Integer) ' => Atom Count List
            ACL = GetAtomCountList(JG.Values.ToArray) 'Atoms from Joback groups

            Return ACL

        End Function

        Public Function GetJCFromUNIFAC(unifac As Integer()) As Integer()

            'fill Joback groups table with UNIFAC subgoups
            Dim k, usgc, usgid As Integer
            Dim JSG, JG2 As String, JG As New Dictionary(Of Integer, Integer)

            For Each j As JobackGroup In JGroups.Values
                JG.Add(j.ID, 0)
            Next

            Dim i As Integer = 0
            For Each r As Integer In unifac
                'Joback groups from UNIFAC subgroups
                JG2 = UNIFACLines(i).Split(","c)(8) 'Joback Subgroup List
                For k = 0 To 3
                    JSG = JG2.Split("/"c)(k)
                    If Not JSG = "" Then
                        usgc = CInt(JSG.Split(":"c)(0)) 'Joback subgroup count
                        usgid = CInt(JSG.Split(":"c)(1)) 'Joback subgroup ID
                        JG(usgid) += usgc * r
                    End If
                Next
                i += 1
            Next

            Return JG.Values.ToArray

        End Function

        Public Function GetUNIFACList(groups As SortedList) As Integer()

            Dim list(UNIFACLines.Count - 1) As Integer
            Dim i As Integer = 0
            For Each item As String In UNIFACLines
                For Each item2 In groups
                    If item2.Key = CInt(item.Split(","c)(1)) Then
                        list(i) = item2.Value
                        Exit For
                    End If
                Next
                i += 1
            Next

            Return list

        End Function



        Public Function CalcMW(ByVal ACL As System.Collections.Generic.Dictionary(Of String, Integer)) As Double

            Dim sum1 As Double

            If Not ACL Is Nothing Then
                For Each A As String In ACL.Keys
                    sum1 += m_JElements.Item(A).MW * ACL.Item(A)
                Next
            End If

            Dim fval As Double

            fval = sum1

            Return fval 'kg/kmol

        End Function
        Public Function CalcTb(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.TB * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = 198.2 + sum1

            Return fval 'K

        End Function
        Public Function CalcTc(ByVal Tb As Double, ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.TC * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = Tb * (0.584 + 0.965 * sum1 - sum1 ^ 2) ^ -1

            Return fval 'K

        End Function
        Public Function CalcPc(ByVal JC() As Integer) As Double

            Dim sum1, sum2 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.PC * JC(i)
                    sum2 += jg.NA * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = 100000 * (0.113 + 0.0032 * sum2 - sum1) ^ -2

            Return fval 'Pa

        End Function
        Public Function CalcVc(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.VC * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = (17.5 + sum1) / 1000

            Return fval 'm3/kmol

        End Function
        Public Function CalcTf(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.TF * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = 122.5 + sum1

            Return fval 'K

        End Function
        Public Function CalcHf(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.HF * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = (-0.88 + sum1)

            Return fval 'KJ/mol

        End Function
        Public Function CalcDHf(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.DH * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = (68.29 + sum1) * 1000

            Return fval 'kJ/kmol

        End Function
        Public Function CalcDGf(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.DG * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = (53.88 + sum1) * 1000

            Return fval 'kJ/kmol

        End Function
        Public Function CalcCpA(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.A * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = sum1 - 37.93

            Return fval 'for Cp in kJ/kmol.K

        End Function
        Public Function CalcCpB(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.B * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = sum1 + 0.21

            Return fval 'for Cp in kJ/kmol.K

        End Function
        Public Function CalcCpC(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.C * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = sum1 - 0.000391

            Return fval 'for Cp in kJ/kmol.K

        End Function
        Public Function CalcCpD(ByVal JC() As Integer) As Double

            Dim sum1 As Double
            Dim i As Integer

            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    sum1 += jg.D * JC(i)
                    i += 1
                Next
            End If

            Dim fval As Double

            fval = sum1 + 0.000000206

            Return fval 'for Cp in kJ/kmol.K

        End Function
        Public Function GetAtomCountList(ByVal JC() As Integer) As System.Collections.Generic.Dictionary(Of String, Integer)
            'generate atom count list from UNIFAC/Joback groups
            Dim ACL As New System.Collections.Generic.Dictionary(Of String, Integer)
            Dim i, k, n, AtomCount As Integer
            Dim ElemList, AtomTypeCount, AtomName As String
            If Not JC Is Nothing Then
                For Each jg As JobackGroup In Me.JGroups.Values
                    If JC(i) > 0 Then
                        ElemList = jg.Elements ' get element list
                        n = 0
                        For k = 0 To ElemList.Length - 1 'count atom types in group
                            If ElemList.Chars(k) = "/" Then n += 1
                        Next
                        For k = 0 To n 'generate atom count list
                            AtomTypeCount = ElemList.Split("/")(k) 'get type and count of Atom
                            AtomName = AtomTypeCount.Split(":")(0)
                            AtomCount = AtomTypeCount.Split(":")(1) * JC(i)

                            If Not ACL.ContainsKey(AtomName) Then
                                ACL.Add(AtomName, AtomCount)
                            Else
                                ACL.Item(AtomName) = ACL.Item(AtomName) + AtomCount
                            End If
                        Next
                    End If
                    i += 1
                Next
            End If

            Return ACL

        End Function
    End Class
    <System.Serializable()> Public Class Element
        Private _ID As Integer
        Private _ElementName, _Symbol As String
        Private _MW As Double
        Public Property ID() As Integer
            Get
                Return _ID
            End Get
            Set(ByVal value As Integer)
                _ID = value
            End Set
        End Property
        Public Property MW() As Double
            Get
                Return _MW
            End Get
            Set(ByVal value As Double)
                _MW = value
            End Set
        End Property
        Public Property ElementName() As String
            Get
                Return _ElementName
            End Get
            Set(ByVal value As String)
                _ElementName = value
            End Set
        End Property
        Public Property ElementSymbol() As String
            Get
                Return _Symbol
            End Get
            Set(ByVal value As String)
                _Symbol = value
            End Set
        End Property
    End Class

    <System.Serializable()> Public Class JobackGroup

        Private m_group, m_groupType, m_Elements As String
        Private m_groupid As Integer
        Private _a, _b, _c, _d, _dh, _dg, _tc, _pc, _tb, _tf, _na, _vc, _hf As Double

        Sub New()

        End Sub
        Public Property Elements() As String
            Get
                Return m_Elements
            End Get
            Set(ByVal value As String)
                m_Elements = value
            End Set
        End Property
        Public Property TB() As Double
            Get
                Return _tb
            End Get
            Set(ByVal value As Double)
                _tb = value
            End Set
        End Property
        Public Property HF() As Double
            Get
                Return _hf
            End Get
            Set(ByVal value As Double)
                _hf = value
            End Set
        End Property
        Public Property TF() As Double
            Get
                Return _tf
            End Get
            Set(ByVal value As Double)
                _tf = value
            End Set
        End Property
        Public Property NA() As Double
            Get
                Return _na
            End Get
            Set(ByVal value As Double)
                _na = value
            End Set
        End Property
        Public Property VC() As Double
            Get
                Return _vc
            End Get
            Set(ByVal value As Double)
                _vc = value
            End Set
        End Property
        Public Property PC() As Double
            Get
                Return _pc
            End Get
            Set(ByVal value As Double)
                _pc = value
            End Set
        End Property
        Public Property TC() As Double
            Get
                Return _tc
            End Get
            Set(ByVal value As Double)
                _tc = value
            End Set
        End Property
        Public Property DG() As Double
            Get
                Return _dg
            End Get
            Set(ByVal value As Double)
                _dg = value
            End Set
        End Property
        Public Property DH() As Double
            Get
                Return _dh
            End Get
            Set(ByVal value As Double)
                _dh = value
            End Set
        End Property
        Public Property D() As Double
            Get
                Return _d
            End Get
            Set(ByVal value As Double)
                _d = value
            End Set
        End Property
        Public Property C() As Double
            Get
                Return _c
            End Get
            Set(ByVal value As Double)
                _c = value
            End Set
        End Property
        Public Property B() As Double
            Get
                Return _b
            End Get
            Set(ByVal value As Double)
                _b = value
            End Set
        End Property
        Public Property A() As Double
            Get
                Return _a
            End Get
            Set(ByVal value As Double)
                _a = value
            End Set
        End Property
        Public Property ID() As Integer
            Get
                Return m_groupid
            End Get
            Set(ByVal value As Integer)
                m_groupid = value
            End Set
        End Property
        Public Property Group() As String
            Get
                Return m_group
            End Get
            Set(ByVal value As String)
                m_group = value
            End Set
        End Property
        Public Property GroupType() As String
            Get
                Return m_groupType
            End Get
            Set(ByVal value As String)
                m_groupType = value
            End Set
        End Property


    End Class

    <System.Serializable()> Public Class HYP
        Sub New()

        End Sub
        Function Tc_Joback(ByVal Tb As Double, ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                            ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                            ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = 0.0141
            Dim pCH2 As Integer = 0.0189
            Dim pCH As Integer = 0.0164
            Dim pOH As Integer = 0.0741
            Dim pACH As Integer = 0.0082
            Dim pACCH2 As Integer = 0.0256
            Dim pACCH3 As Integer = 0.0284
            Dim pACOH As Integer = 0.0884
            Dim pCH3CO As Integer = 0.0521
            Dim pCH2CO As Integer = 0.0569

            Dim Tc As Double = 0

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Tc = Tb * (0.584 + 0.965 * sum - sum ^ 2) ^ -1

            Return Tc

        End Function
        Function Pc_Joback(ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                            ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                            ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = -0.0012
            Dim pCH2 As Integer = 0
            Dim pCH As Integer = 0.002
            Dim pOH As Integer = 0.0112
            Dim pACH As Integer = 0.0011
            Dim pACCH2 As Integer = -0.002
            Dim pACCH3 As Integer = 0.0004
            Dim pACOH As Integer = 0.012
            Dim pCH3CO As Integer = 0.0019
            Dim pCH2CO As Integer = 0.0031

            Dim Pc As Double = 0

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Dim na = nCH3 * 4 + nCH2 * 3 + nCH * 2 + _
                nOH * 2 + nACH * 2 + nACCH2 * 4 + _
                nACCH3 * 5 + nACOH * 3 + _
                nCH3CO * 6 + nCH2CO * 5

            Pc = (0.113 + 0.0032 * na - sum) ^ -2

            Return Pc * 100000

        End Function
        Function Vc_Joback(ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                            ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                            ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = 65
            Dim pCH2 As Integer = 56
            Dim pCH As Integer = 41
            Dim pOH As Integer = 28
            Dim pACH As Integer = 41
            Dim pACCH2 As Integer = 88
            Dim pACCH3 As Integer = 97
            Dim pACOH As Integer = 60
            Dim pCH3CO As Integer = 127
            Dim pCH2CO As Integer = 118

            Dim Vc As Double = 0

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Vc = 17.5 + sum

            Return Vc * 0.000001 * 1000

        End Function
        Function Tb_Joback(ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                            ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                            ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = 23.58
            Dim pCH2 As Integer = 22.88
            Dim pCH As Integer = 21.74
            Dim pOH As Integer = 92.88
            Dim pACH As Integer = 26.73
            Dim pACCH2 As Integer = 49.19
            Dim pACCH3 As Integer = 54.59
            Dim pACOH As Integer = 123.89
            Dim pCH3CO As Integer = 100.33
            Dim pCH2CO As Integer = 99.63

            Dim Tb As Double = 0

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Tb = 198 + sum

            Return Tb

        End Function
        Function Hf298_Marrero_Gani(ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                                   ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                                   ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = -42.479
            Dim pCH2 As Integer = -20.829
            Dim pCH As Integer = -7.122
            Dim pOH As Integer = -178.36
            Dim pACH As Integer = 12.861
            Dim pACCH2 As Integer = 4.38
            Dim pACCH3 As Integer = -19.258
            Dim pACOH As Integer = -164.191
            Dim pCH3CO As Integer = -180.604
            Dim pCH2CO As Integer = -163.09

            Dim Hf298 As Double = 0

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Hf298 = 5.549 + sum

            Return Hf298 'kJ/mol

        End Function
        Function Gf298_Marrero_Gani(ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                                   ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                                   ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = 2.878
            Dim pCH2 As Integer = 8.064
            Dim pCH As Integer = 8.254
            Dim pOH As Integer = -144.015
            Dim pACH As Integer = 26.732
            Dim pACCH2 As Integer = 31.663
            Dim pACCH3 As Integer = 24.919
            Dim pACOH As Integer = -131.327
            Dim pCH3CO As Integer = -120.667
            Dim pCH2CO As Integer = -120.425

            Dim Gf298 As Double = 0

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Gf298 = -34.967 + sum

            Return Gf298 'kJ/mol

        End Function
        Function Sf298_Marrero_Gani(ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                                   ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                                   ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = -0.1521
            Dim pCH2 As Integer = -0.0969
            Dim pCH As Integer = -0.0516
            Dim pOH As Integer = -0.1152
            Dim pACH As Integer = -0.0465
            Dim pACCH2 As Integer = -0.0915
            Dim pACCH3 As Integer = -0.1482
            Dim pACOH As Integer = -0.1102
            Dim pCH3CO As Integer = -0.201
            Dim pCH2CO As Integer = -0.1431

            Dim Sf298 As Double = 0

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Sf298 = 0.135891 + sum

            Return Sf298 'kJ/mol.K

        End Function
        Function MM_UNIFAC(ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                                   ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                                   ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Dim pCH3 As Integer = 15
            Dim pCH2 As Integer = 14
            Dim pCH As Integer = 13
            Dim pOH As Integer = 17
            Dim pACH As Integer = 13
            Dim pACCH2 As Integer = 26
            Dim pACCH3 As Integer = 27
            Dim pACOH As Integer = 29
            Dim pCH3CO As Integer = 43
            Dim pCH2CO As Integer = 42

            Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                nACCH3 * pACCH3 + nACOH * pACOH + _
                nCH3CO * pCH3CO + nCH2CO * pCH2CO

            Return sum 'kg/kmol

        End Function
        Function DHvb_Vetere(ByVal Tc As Double, ByVal Pc As Double, ByVal Tb As Double) As Double

            Dim R = 8.314
            Dim Tbr = Tb / Tc

            Pc = Pc / 100000

            Return (R * Tc * Tbr * (0.4343 * Math.Log(Pc) - 0.69431 + 0.8954 * Tbr) / (0.37691 - 0.37306 * Tbr + 0.15075 * Pc ^ -1 * Tbr ^ -2))
            'kJ/kmol

        End Function
        Function Element_UNIFAC(ByVal element As String, ByVal nCH3 As Integer, ByVal nCH2 As Integer, ByVal nCH As Integer, ByVal nOH As Integer, _
                                   ByVal nACH As Integer, ByVal nACCH2 As Integer, ByVal nACCH3 As Integer, ByVal nACOH As Integer, _
                                   ByVal nCH3CO As Integer, ByVal nCH2CO As Integer)

            Select Case element
                Case "C"
                    Dim pCH3 As Integer = 1
                    Dim pCH2 As Integer = 1
                    Dim pCH As Integer = 1
                    Dim pOH As Integer = 0
                    Dim pACH As Integer = 1
                    Dim pACCH2 As Integer = 2
                    Dim pACCH3 As Integer = 2
                    Dim pACOH As Integer = 1
                    Dim pCH3CO As Integer = 2
                    Dim pCH2CO As Integer = 2

                    Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                        nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                        nACCH3 * pACCH3 + nACOH * pACOH + _
                        nCH3CO * pCH3CO + nCH2CO * pCH2CO
                    Return sum
                Case "H"
                    Dim pCH3 As Integer = 3
                    Dim pCH2 As Integer = 2
                    Dim pCH As Integer = 1
                    Dim pOH As Integer = 1
                    Dim pACH As Integer = 1
                    Dim pACCH2 As Integer = 2
                    Dim pACCH3 As Integer = 3
                    Dim pACOH As Integer = 1
                    Dim pCH3CO As Integer = 3
                    Dim pCH2CO As Integer = 2

                    Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                        nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                        nACCH3 * pACCH3 + nACOH * pACOH + _
                        nCH3CO * pCH3CO + nCH2CO * pCH2CO

                    Return sum
                Case "O"

                    Dim pCH3 As Integer = 0
                    Dim pCH2 As Integer = 0
                    Dim pCH As Integer = 0
                    Dim pOH As Integer = 1
                    Dim pACH As Integer = 0
                    Dim pACCH2 As Integer = 0
                    Dim pACCH3 As Integer = 0
                    Dim pACOH As Integer = 1
                    Dim pCH3CO As Integer = 1
                    Dim pCH2CO As Integer = 1

                    Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                        nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                        nACCH3 * pACCH3 + nACOH * pACOH + _
                        nCH3CO * pCH3CO + nCH2CO * pCH2CO

                    Return sum
                Case "N"
                    Dim pCH3 As Integer = 0
                    Dim pCH2 As Integer = 0
                    Dim pCH As Integer = 0
                    Dim pOH As Integer = 0
                    Dim pACH As Integer = 0
                    Dim pACCH2 As Integer = 0
                    Dim pACCH3 As Integer = 0
                    Dim pACOH As Integer = 0
                    Dim pCH3CO As Integer = 0
                    Dim pCH2CO As Integer = 0

                    Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                        nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                        nACCH3 * pACCH3 + nACOH * pACOH + _
                        nCH3CO * pCH3CO + nCH2CO * pCH2CO

                    Return sum
                Case "S"
                    Dim pCH3 As Integer = 0
                    Dim pCH2 As Integer = 0
                    Dim pCH As Integer = 0
                    Dim pOH As Integer = 0
                    Dim pACH As Integer = 0
                    Dim pACCH2 As Integer = 0
                    Dim pACCH3 As Integer = 0
                    Dim pACOH As Integer = 0
                    Dim pCH3CO As Integer = 0
                    Dim pCH2CO As Integer = 0

                    Dim sum = nCH3 * pCH3 + nCH2 * pCH2 + nCH * pCH + _
                        nOH * pOH + nACH * pACH + nACCH2 * pACCH2 + _
                        nACCH3 * pACCH3 + nACOH * pACOH + _
                        nCH3CO * pCH3CO + nCH2CO * pCH2CO

                    Return sum
                Case Else
                    Return 0
            End Select

        End Function
    End Class

End Namespace
