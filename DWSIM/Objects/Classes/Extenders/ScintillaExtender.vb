'    scintillaNET Editor extender for code intellisense display
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

Imports ScintillaNET
Imports System.Reflection
Imports System.Xml.Linq
Imports System.Linq

''' <summary>
''' scintillaNET Editor extender for code intellisense display
''' </summary>
''' <remarks>(c) 2015 Daniel Medeiros</remarks>
Module scintillaExtender

    ''' <summary>
    ''' Sets the editor style for Python language.
    ''' </summary>
    ''' <param name="scintilla"></param>
    ''' <param name="fontname">Name of the font to be used.</param>
    ''' <param name="fontsize">Size of the font to be used.</param>
    ''' <param name="viewspaces">Enables or disables whitspace highlighting.</param>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Sub SetEditorStyle(scintilla As scintillaNET.scintilla, fontname As String, fontsize As Integer, viewspaces As Boolean)

        scintilla.StyleResetDefault()
        scintilla.Styles(Style.[Default]).Font = fontname
        scintilla.Styles(Style.[Default]).Size = fontsize
        scintilla.StyleClearAll()

        ' i.e. Apply to all
        ' Set the lexer

        scintilla.Lexer = Lexer.Python

        ' Known lexer properties:
        ' "tab.timmy.whinge.level",
        ' "lexer.python.literals.binary",
        ' "lexer.python.strings.u",
        ' "lexer.python.strings.b",
        ' "lexer.python.strings.over.newline",
        ' "lexer.python.keywords2.no.sub.identifiers",
        ' "fold.quotes.python",
        ' "fold.compact",
        ' "fold"

        ' Some properties we like

        scintilla.SetProperty("tab.timmy.whinge.level", "1")
        scintilla.SetProperty("fold", "1")

        scintilla.Margins(0).Width = 20

        ' Use margin 2 for fold markers

        scintilla.Margins(1).Type = MarginType.Symbol
        scintilla.Margins(1).Mask = Marker.MaskFolders
        scintilla.Margins(1).Sensitive = True
        scintilla.Margins(1).Width = 20

        ' Reset folder markers

        For i As Integer = Marker.FolderEnd To Marker.FolderOpen
            scintilla.Markers(i).SetForeColor(SystemColors.ControlLightLight)
            scintilla.Markers(i).SetBackColor(SystemColors.ControlDark)
        Next

        ' Style the folder markers

        scintilla.Markers(Marker.Folder).Symbol = MarkerSymbol.BoxPlus
        scintilla.Markers(Marker.Folder).SetBackColor(SystemColors.ControlText)
        scintilla.Markers(Marker.FolderOpen).Symbol = MarkerSymbol.BoxMinus
        scintilla.Markers(Marker.FolderEnd).Symbol = MarkerSymbol.BoxPlusConnected
        scintilla.Markers(Marker.FolderEnd).SetBackColor(SystemColors.ControlText)
        scintilla.Markers(Marker.FolderMidTail).Symbol = MarkerSymbol.TCorner
        scintilla.Markers(Marker.FolderOpenMid).Symbol = MarkerSymbol.BoxMinusConnected
        scintilla.Markers(Marker.FolderSub).Symbol = MarkerSymbol.VLine
        scintilla.Markers(Marker.FolderTail).Symbol = MarkerSymbol.LCorner

        ' Enable automatic folding

        scintilla.AutomaticFold = (AutomaticFold.Show Or AutomaticFold.Click Or AutomaticFold.Change)

        ' Set the styles

        scintilla.Styles(Style.Python.[Default]).ForeColor = Color.FromArgb(&H80, &H80, &H80)
        scintilla.Styles(Style.Python.CommentLine).ForeColor = Color.FromArgb(&H0, &H7F, &H0)
        scintilla.Styles(Style.Python.CommentLine).Italic = True
        scintilla.Styles(Style.Python.Number).ForeColor = Color.FromArgb(&H0, &H7F, &H7F)
        scintilla.Styles(Style.Python.[String]).ForeColor = Color.FromArgb(&H7F, &H0, &H7F)
        scintilla.Styles(Style.Python.Character).ForeColor = Color.FromArgb(&H7F, &H0, &H7F)
        scintilla.Styles(Style.Python.Word).ForeColor = Color.FromArgb(&H0, &H0, &H7F)
        scintilla.Styles(Style.Python.Word).Bold = True
        scintilla.Styles(Style.Python.Triple).ForeColor = Color.FromArgb(&H7F, &H0, &H0)
        scintilla.Styles(Style.Python.TripleDouble).ForeColor = Color.FromArgb(&H7F, &H0, &H0)
        scintilla.Styles(Style.Python.ClassName).ForeColor = Color.FromArgb(&H0, &H0, &HFF)
        scintilla.Styles(Style.Python.ClassName).Bold = True
        scintilla.Styles(Style.Python.DefName).ForeColor = Color.FromArgb(&H0, &H7F, &H7F)
        scintilla.Styles(Style.Python.DefName).Bold = True
        scintilla.Styles(Style.Python.[Operator]).Bold = True
        scintilla.Styles(Style.Python.CommentBlock).ForeColor = Color.FromArgb(&H7F, &H7F, &H7F)
        scintilla.Styles(Style.Python.CommentBlock).Italic = True
        scintilla.Styles(Style.Python.StringEol).ForeColor = Color.FromArgb(&H0, &H0, &H0)
        scintilla.Styles(Style.Python.StringEol).BackColor = Color.FromArgb(&HE0, &HC0, &HE0)
        scintilla.Styles(Style.Python.StringEol).FillLine = True

        scintilla.Styles(Style.Python.DefName).ForeColor = Color.Brown
        scintilla.Styles(Style.Python.DefName).Bold = True

        scintilla.Styles(Style.Python.Word2).ForeColor = Color.DarkRed
        scintilla.Styles(Style.Python.Word2).Bold = True

        With scintilla.Styles(Style.CallTip)
            .Font = fontname
            .Size = fontsize - 2
            .ForeColor = Color.FromKnownColor(KnownColor.ActiveCaptionText)
        End With

        If viewspaces Then scintilla.ViewWhitespace = WhitespaceMode.VisibleAfterIndent

        ' Keyword lists:
        ' 0 "Keywords",
        ' 1 "Highlighted identifiers"

        Dim python2 = "and as assert break class continue def del elif else except exec finally for from global if import in is lambda not or pass print raise return try while with yield"
        Dim python3 = "False None True and as assert break class continue def del elif else except finally for from global if import in is lambda nonlocal not or pass raise return try while with yield"

        'add keywords from DWSIM classes properties and methods

        Dim netprops As String = ""

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations,")).FirstOrDefault
        Dim fsolverassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.FlowsheetSolver,")).FirstOrDefault

        Dim props = calculatorassembly.GetType("DWSIM.Thermodynamics.Streams.MaterialStream").GetProperties()
        For Each p In props
            netprops += p.Name + " "
        Next
        Dim methods = calculatorassembly.GetType("DWSIM.Thermodynamics.Streams.MaterialStream").GetMethods()
        For Each m In methods
            netprops += m.Name + " "
        Next
        props = unitopassembly.GetType("DWSIM.UnitOperations.Streams.EnergyStream").GetProperties()
        For Each p In props
            netprops += p.Name + " "
        Next
        methods = unitopassembly.GetType("DWSIM.UnitOperations.Streams.EnergyStream").GetMethods()
        For Each m In methods
            netprops += m.Name + " "
        Next
        props = Type.GetType("DWSIM.FormFlowsheet").GetProperties()
        For Each p In props
            If p.PropertyType.Namespace <> "System.Windows.Forms" Then netprops += p.Name + " "
        Next
        props = calculatorassembly.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage").GetProperties()
        For Each p In props
            If p.PropertyType.Namespace <> "System.Windows.Forms" Then netprops += p.Name + " "
        Next
        methods = calculatorassembly.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage").GetMethods()
        For Each m In methods
            netprops += m.Name + " "
        Next

        Dim objects As String = ""

        If scintilla.Tag = 1 Then
            methods = Type.GetType("DWSIM.FormFlowsheet").GetMethods()
            For Each m In methods
                netprops += m.Name + " "
            Next
            props = Type.GetType("DWSIM.SpreadsheetForm").GetProperties()
            For Each p In props
                If p.PropertyType.Namespace <> "System.Windows.Forms" Then netprops += p.Name + " "
            Next
            methods = Type.GetType("DWSIM.SpreadsheetForm").GetMethods()
            For Each m In methods
                netprops += m.Name + " "
            Next
            'editor is being used at flowsheet level.
            props = fsolverassembly.GetType("DWSIM.FlowsheetSolver.FlowsheetSolver").GetProperties()
            For Each p In props
                If p.PropertyType.Namespace <> "System.Windows.Forms" Then netprops += p.Name + " "
            Next
            methods = fsolverassembly.GetType("DWSIM.FlowsheetSolver.FlowsheetSolver").GetMethods()
            For Each m In methods
                netprops += m.Name + " "
            Next
            objects = "MaterialStream EnergyStream PropertyPackage UnitOp Flowsheet Spreadsheet Plugins Solver DWSIM"
        Else
            'editor is being used at script unit operation level
            objects = "ims1 ims2 ims3 ims4 ims5 ims6 ies1 oms1 oms2 oms3 oms4 oms5 oms6 oes1 Flowsheet Spreadsheet Plugins Solver Me DWSIM"

        End If


        scintilla.SetKeywords(0, python2 + " " + python3)
        scintilla.SetKeywords(1, objects + " " + netprops)

        scintilla.SetColumnMargins()

    End Sub

    ''' <summary>
    ''' Sets the column margins to correctly show line numbers.
    ''' </summary>
    ''' <param name="scintilla"></param>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Sub SetColumnMargins(scintilla As scintillaNET.scintilla)

        Dim maxLineNumberCharLength = scintilla.Lines.Count.ToString().Length

        Const padding As Integer = 2
        scintilla.Margins(0).Width = scintilla.TextWidth(Style.LineNumber, New String("9"c, maxLineNumberCharLength + 1)) + padding

    End Sub

    ''' <summary>
    ''' Show an autocomplete listbox with methods and properties from the object with the entered keyword.
    ''' </summary>
    ''' <param name="scintilla"></param>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Sub ShowAutoComplete(scintilla As scintillaNET.scintilla)

        Dim suggestions As String = ""

        'parses the last keyword (object) (before the ".") and get suggestions for the autocomplete box from its properties and methods

        Dim text = scintilla.getLastWord().Split({".", "(", ")", " ", vbCr, vbLf, vbCrLf}, StringSplitOptions.RemoveEmptyEntries)
        Dim lastchar = Chr(scintilla.GetCharAt(scintilla.CurrentPosition))

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations,")).FirstOrDefault
        Dim fsolverassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.FlowsheetSolver,")).FirstOrDefault

        If text.Length >= 1 Then
            Dim lastkeyword As String = ""
            If text.Length >= 2 Then
                lastkeyword = text(text.Length - 2).Trim
            Else
                lastkeyword = text(text.Length - 1).Trim
            End If
            Select Case lastkeyword
                Case "ims1", "ims2", "ims3", "ims4", "ims5", "ims6", "oms1", "oms2", "oms3", "oms4", "oms5", "MaterialStream"
                    Dim props = calculatorassembly.GetType("DWSIM.Thermodynamics.Streams.MaterialStream").GetProperties()
                    For Each p In props
                        suggestions += (p.Name) + " "
                    Next
                    Dim methods = calculatorassembly.GetType("DWSIM.Thermodynamics.Streams.MaterialStream").GetMethods()
                    For Each m In methods
                        suggestions += (m.Name) + " "
                    Next
                Case "ies1", "oes1", "EnergyStream"
                    Dim props = unitopassembly.GetType("DWSIM.UnitOperations.Streams.EnergyStream").GetProperties()
                    For Each p In props
                        suggestions += (p.Name) + " "
                    Next
                    Dim methods = unitopassembly.GetType("DWSIM.UnitOperations.Streams.EnergyStream").GetMethods()
                    For Each m In methods
                        suggestions += (m.Name) + " "
                    Next
                Case "Flowsheet"
                    Dim props = Type.GetType("DWSIM.FormFlowsheet").GetProperties()
                    For Each p In props
                        If p.PropertyType.Namespace <> "System.Windows.Forms" Then suggestions += (p.Name) + " "
                    Next
                    Dim methods = Type.GetType("DWSIM.FormFlowsheet").GetMethods()
                    For Each m In methods
                        suggestions += (m.Name) + " "
                    Next
                Case "Spreadsheet"
                    Dim props = Type.GetType("DWSIM.SpreadsheetForm").GetProperties()
                    For Each p In props
                        If p.PropertyType.Namespace <> "System.Windows.Forms" Then suggestions += (p.Name) + " "
                    Next
                    Dim methods = Type.GetType("DWSIM.SpreadsheetForm").GetMethods()
                    For Each m In methods
                        suggestions += (m.Name) + " "
                    Next
                Case "PropertyPackage"
                    Dim props = calculatorassembly.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage").GetProperties()
                    For Each p In props
                        suggestions += (p.Name) + " "
                    Next
                    Dim methods = calculatorassembly.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage").GetMethods()
                    For Each m In methods
                        suggestions += (m.Name) + " "
                    Next
                Case "UnitOp", "Me"
                    Dim props = unitopassembly.GetType("DWSIM.SharedClasses.UnitOperations.BaseClass").GetProperties()
                    For Each p In props
                        suggestions += (p.Name) + " "
                    Next
                    Dim methods = unitopassembly.GetType("DWSIM.SharedClasses.UnitOperations.BaseClass").GetMethods()
                    For Each m In methods
                        suggestions += (m.Name) + " "
                    Next
                Case "Solver"
                    Dim methods = fsolverassembly.GetType("DWSIM.FlowsheetSolver.FlowsheetSolver").GetMethods()
                    For Each m In methods
                        suggestions += (m.Name) + " "
                    Next
                Case Else
                    If scintilla.Tag = 1 Then
                        suggestions = "MaterialStream EnergyStream PropertyPackage UnitOp Flowsheet Spreadsheet Plugins Solver DWSIM"
                    Else
                        suggestions = "ims1 ims2 ims3 ims4 ims5 ims6 ies1 oms1 oms2 oms3 oms4 oms5 oms6 oes1 Flowsheet Spreadsheet Plugins Solver Me DWSIM"
                    End If
            End Select
        Else
            If scintilla.Tag = 1 Then
                'editor is being used at flowsheet level.
                suggestions = "MaterialStream EnergyStream PropertyPackage UnitOp Flowsheet Spreadsheet Plugins Solver DWSIM"
            Else
                'editor is being used at script unit operation level
                suggestions = "ims1 ims2 ims3 ims4 ims5 ims6 ies1 oms1 oms2 oms3 oms4 oms5 oms6 oes1 Flowsheet Spreadsheet Plugins Solver Me DWSIM"
            End If
        End If

        Dim currentPos = scintilla.CurrentPosition
        Dim wordStartPos = scintilla.WordStartPosition(currentPos, True)

        ' Display the autocompletion list
        Dim lenEntered = currentPos - wordStartPos

        If lenEntered > 0 And lastchar <> "." Then
            scintilla.AutoCShow(lenEntered, suggestions)
        ElseIf lastchar = "." And text.Length >= 1 Then
            scintilla.AutoCShow(0, suggestions)
        End If

    End Sub

    ''' <summary>
    ''' Show a tooltip with information about the entered object method or property.
    ''' </summary>
    ''' <param name="scintilla"></param>
    ''' <param name="reader">Jolt's XmlDocCommentReader instance, to get and display comments from assembly-generated XML file.</param>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Sub ShowToolTip(scintilla As scintillaNET.scintilla, reader As Jolt.XmlDocCommentReader)

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations,")).FirstOrDefault
        Dim fsolverassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.FlowsheetSolver,")).FirstOrDefault

        'parses the last keyword (object) (before the ".") and get suggestions for the autocomplete box from its properties and methods

        Dim text = scintilla.getLastWord().Split({".", "(", ")", " ", vbCr, vbLf, vbCrLf}, StringSplitOptions.RemoveEmptyEntries)
        Dim lastchar = Chr(scintilla.GetCharAt(scintilla.CurrentPosition))

        Dim helptext As String = ""

        If text.Length >= 2 Then
            Dim lastkeyword = text(text.Length - 1)
            Dim lastobj = text(text.Length - 2).Trim()
            Select Case lastobj
                Case "ims1", "ims2", "ims3", "ims4", "ims5", "ims6", "oms1", "oms2", "oms3", "oms4", "oms5", "MaterialStream"
                    Dim prop = calculatorassembly.GetType("DWSIM.Thermodynamics.Streams.MaterialStream").GetMember(lastkeyword)
                    If prop.Length > 0 Then helptext = scintilla.FormatHelpTip(prop(0), reader)
                Case "ies1", "oes1", "EnergyStream"
                    Dim prop = unitopassembly.GetType("DWSIM.UnitOperations.Streams.EnergyStream").GetMember(lastkeyword)
                    If prop.Length > 0 Then helptext = scintilla.FormatHelpTip(prop(0), reader)
                Case "Flowsheet"
                    Dim prop = Type.GetType("DWSIM.FormFlowsheet").GetMember(lastkeyword)
                    If prop.Length > 0 Then helptext = scintilla.FormatHelpTip(prop(0), reader)
                Case "Spreadsheet"
                    Dim prop = Type.GetType("DWSIM.SpreadsheetForm").GetMember(lastkeyword)
                    If prop.Length > 0 Then helptext = scintilla.FormatHelpTip(prop(0), reader)
                Case "PropertyPackage"
                    Dim prop = calculatorassembly.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage").GetMember(lastkeyword)
                    If prop.Length > 0 Then helptext = scintilla.FormatHelpTip(prop(0), reader)
                Case "UnitOp", "Me"
                    Dim prop = unitopassembly.GetType("DWSIM.SharedClasses.UnitOperations.BaseClass").GetMember(lastkeyword)
                    If prop.Length > 0 Then helptext = scintilla.FormatHelpTip(prop(0), reader)
                Case "Solver"
                    Dim prop = fsolverassembly.GetType("DWSIM.FlowsheetSolver.FlowsheetSolver").GetMember(lastkeyword)
                    If prop.Length > 0 Then helptext = scintilla.FormatHelpTip(prop(0), reader)
            End Select

            'shows the tooltip

            If helptext <> "" Then scintilla.CallTipShow(scintilla.CurrentPosition, helptext) Else scintilla.CallTipCancel()

        Else

            'hides tooltip if visible

            scintilla.CallTipCancel()

        End If

    End Sub

    ''' <summary>
    ''' Formats the text to be displayed in the tooltip using information from the object's member and from the XML documentation, if existing.
    ''' </summary>
    ''' <param name="scintilla"></param>
    ''' <param name="member">Reflected member to display information from.</param>
    ''' <param name="reader">Jolt's XmlDocCommentReader instance, to get and display comments from assembly-generated XML file.</param>
    ''' <returns>The formatted text to display in the tooltip.</returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Private Function FormatHelpTip(scintilla As scintillaNET.scintilla, member As MemberInfo, reader As Jolt.XmlDocCommentReader) As String

        Select Case member.MemberType

            Case MemberTypes.Method

                Dim methods = member.DeclaringType.GetMethods().Where(Function(m) m.Name = member.Name).ToList
                Dim method = methods(0)

                Dim summary As String = ""
                Dim returntype As String = ""
                Dim returndescription As String = ""
                Dim remarks As String = ""

                Dim argumentdescriptions As New Dictionary(Of String, String)

                Dim txthelp As String = method.DeclaringType.Name & " method '" & member.Name & "'" & vbCrLf

                Dim xmlhelp = reader.GetComments(method)

                If Not xmlhelp Is Nothing Then
                    Dim params = xmlhelp.Elements("param").ToList
                    For Each p In params
                        If p.Value.ToString.Length > 70 Then
                            argumentdescriptions.Add(p.Attribute("name"), p.Value.ToString.Substring(0, 70).Trim(vbLf) & " [...]")
                        Else
                            argumentdescriptions.Add(p.Attribute("name"), p.Value.ToString.Trim(vbLf))
                        End If
                    Next
                    If method.ReturnType.Name <> "Void" Then
                        Dim rdesc = xmlhelp.Elements("returns").FirstOrDefault
                        If Not rdesc Is Nothing Then
                            returndescription = rdesc.Value
                        End If
                    End If
                    Dim redesc = xmlhelp.Elements("remarks").FirstOrDefault
                    If Not redesc Is Nothing Then
                        If redesc.Value.Length > 1000 Then
                            remarks = redesc.Value.Substring(0, 1000) & " [...]"
                        Else
                            remarks = redesc.Value
                        End If
                    End If
                    redesc = xmlhelp.Elements("summary").FirstOrDefault
                    If Not redesc Is Nothing Then
                        summary = redesc.Value
                        txthelp += summary & vbCrLf
                    End If
                End If

                If method.GetParameters.Count > 0 Then
                    txthelp += "Parameters:" & vbCrLf & vbCrLf
                    txthelp += "Type".PadRight(18) & "Name".PadRight(15) & "Description" & vbCrLf
                    For Each par In method.GetParameters
                        If argumentdescriptions.ContainsKey(par.Name) Then
                            txthelp += par.ParameterType.Name.PadRight(18) & par.Name.PadRight(15) & argumentdescriptions(par.Name) & vbCrLf
                        Else
                            txthelp += par.ParameterType.Name.PadRight(18) & par.Name.PadRight(15) & vbCrLf
                        End If
                    Next
                    txthelp += vbCrLf
                End If

                txthelp += "Return Type: " & method.ReturnType.ToString
                If returndescription <> "" Then txthelp += vbCrLf & "Return Parameter Description: " & returndescription
                If remarks <> "" Then txthelp += vbCrLf & vbCrLf & "Remarks: " & remarks

                Return txthelp

            Case MemberTypes.Property

                Dim props = member.DeclaringType.GetProperties().Where(Function(p) p.Name = member.Name).ToList
                Dim prop = props(0)

                Dim summary As String = ""
                Dim proptype As String = ""

                Dim txthelp As String = prop.DeclaringType.Name & " property '" & prop.Name & "'" & vbCrLf
                txthelp += "Type: " & prop.PropertyType.ToString

                Dim xmlhelp = reader.GetComments(prop)

                If Not xmlhelp Is Nothing Then
                    Dim redesc = xmlhelp.Elements("summary").FirstOrDefault
                    If Not redesc Is Nothing Then
                        txthelp += vbCrLf & "Description: " & redesc.Value
                    End If
                End If

                Return txthelp

            Case Else

                Return ""

        End Select

    End Function

    ''' <summary>
    ''' Returns the last typed word in the editor.
    ''' </summary>
    ''' <param name="scintilla"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Private Function getLastWord(scintilla As scintillaNET.scintilla) As String

        Dim word As String = ""

        Dim pos As Integer = scintilla.SelectionStart
        If pos > 1 Then

            Dim tmp As String = ""
            Dim f As New Char()
            While f <> " " And pos > 0
                pos -= 1
                tmp = scintilla.Text.Substring(pos, 1)
                f = CChar(tmp(0))
                word += f
            End While

            Dim ca As Char() = word.ToCharArray()
            Array.Reverse(ca)

            word = New [String](ca)
        End If
        Return word.Trim()

    End Function

End Module
