﻿'    Table Graphic Object
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports System.Linq
Imports DWSIM.Interfaces
Imports System.Xml
Imports DWSIM.SharedClasses.Extras
Imports DWSIM.Interfaces.Enums
Imports DWSIM.DrawingTools.Point
Imports DWSIM.ExtensionMethods
Imports s = DWSIM.GlobalSettings.Settings

Namespace GraphicObjects.Tables

    <Serializable()> Public Class FloatingTableGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = Enums.GraphicObjects.ObjectType.GO_FloatingTable
        End Sub

        Public Sub New(ByRef owner As ISimulationObject)
            Me.Owner = owner
            Me.ObjectType = Enums.GraphicObjects.ObjectType.GO_FloatingTable
        End Sub

        Public Sub New(ByRef owner As ISimulationObject, ByVal graphicPosition As Point)
            Me.New(owner)
            Me.SetPosition(graphicPosition.X, graphicPosition.Y)
        End Sub

        Public Sub New(ByRef owner As ISimulationObject, ByVal posX As Integer, ByVal posY As Integer)
            Me.New(owner, New Point(posX, posY))
        End Sub

#End Region

        Property Padding As Double = 6

        Public Property TextColor As SKColor = SKColors.Black

        Public Property BorderColor As SKColor = SKColors.Black

        Public Property HeaderText() As String = ""

        Public Overrides Sub Draw(ByVal g As Object)

            Dim sf = GlobalSettings.Settings.UIScalingFactor

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            FontSize = 11.0

            FontSize *= GlobalSettings.Settings.DpiScale * sf

            Dim zoom As Single = AdditionalInfo

            If zoom = 0.0 Then Exit Sub

            Padding = 5 / zoom * GlobalSettings.Settings.DpiScale * sf

            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = FontSize / zoom
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = If(GlobalSettings.Settings.DarkMode, SKColors.WhiteSmoke, SKColors.SteelBlue)
                .IsStroke = False
                .Typeface = RegularTypeFace
            End With

            Dim tbpaint As New SKPaint()

            With tbpaint
                .TextSize = FontSize / zoom
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = If(GlobalSettings.Settings.DarkMode, SKColors.WhiteSmoke, SKColors.SteelBlue)
                .IsStroke = False
                .Typeface = DefaultTypeFace
            End With

            Dim bpaint, bpaint2 As New SKPaint()

            With bpaint
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = If(GlobalSettings.Settings.DarkMode, SKColors.DimGray.WithAlpha(240), SKColors.White.WithAlpha(240))
                .IsStroke = False
                .StrokeWidth = LineWidth / zoom
            End With

            With bpaint2
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = If(GlobalSettings.Settings.DarkMode, SKColors.WhiteSmoke, SKColors.SteelBlue)
                .IsStroke = True
                .StrokeWidth = LineWidth / zoom
            End With

            Dim spaint As New SKPaint()

            With spaint
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = SKColors.LightGray.WithAlpha(100)
                .IsStroke = False
            End With

            If Not Me.Owner Is Nothing Then

                If Me.Owner.GetFlowsheet IsNot Nothing Then

                    Dim maxL1, maxL2, maxL3, count As Double
                    Dim maxH As Double

                    'determinar comprimento das colunas e altura das linhas

                    maxL1 = 0
                    maxL2 = 0
                    maxL3 = 0
                    maxH = 0
                    count = 1

                    Dim size, size2 As SKSize

                    size = MeasureString(Me.Owner.GraphicObject.Tag.ToUpper, tpaint)
                    If size.Width > maxL1 Then maxL1 = size.Width
                    If size.Height > maxH Then maxH = size.Height

                    Dim fs = Owner.GetFlowsheet
                    Dim props As New List(Of String)
                    Try
                        props = New List(Of String)(fs.FlowsheetOptions.VisibleProperties(Owner.GetType.Name))
                    Catch ex As Exception

                    End Try

                    props.AddRange(DirectCast(Owner.ExtraProperties, IDictionary(Of String, Object)).Keys.ToArray)

                    If Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.CapeOpenUO Or
                        Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.External Then
                        props = Owner.GetProperties(PropertyType.ALL).ToList()
                    End If

                    Dim propstoremove As New List(Of String)

                    If Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream Then
                        For Each p In props
                            If Owner.GetPropertyValue(p).Equals(Double.MinValue) Then
                                propstoremove.Add(p)
                            End If
                        Next
                        For i As Integer = 0 To propstoremove.Count - 1
                            props.Remove(propstoremove(i))
                        Next
                    End If

                    Dim propstring, propval, propunit As String, pval0 As Object

                    For Each prop In props
                        propstring = Owner.GetFlowsheet.GetTranslatedString(prop)
                        pval0 = Owner.GetPropertyValue(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                        If pval0 Is Nothing Then Exit For
                        If TypeOf pval0 Is Double Then
                            propval = Convert.ToDouble(pval0).ToString(Owner.GetFlowsheet.FlowsheetOptions.NumberFormat)
                        Else
                            propval = pval0.ToString
                        End If
                        propunit = Owner.GetPropertyUnit(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                        size = MeasureString(propstring, tpaint)
                        If size.Width > maxL1 Then maxL1 = size.Width
                        If size.Height > maxH Then maxH = size.Height
                        size = MeasureString(propval, tpaint)
                        If size.Width > maxL2 Then maxL2 = size.Width
                        If size.Height > maxH Then maxH = size.Height
                        size = MeasureString(propunit, tpaint)
                        If size.Width > maxL3 Then maxL3 = size.Width
                        If size.Height > maxH Then maxH = size.Height
                        count += 1
                    Next

                    If Not Me.Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream Then

                        If maxH = 0 Then maxH = 20 / zoom

                        Me.Height = (count + 1) * (maxH + 2 * Me.Padding)
                        size = MeasureString(Me.HeaderText, tpaint)
                        size2 = MeasureString(Owner.GetFlowsheet.GetTranslatedString(Me.Owner.GraphicObject.Description), tpaint)

                        If size.Width > size2.Width Then
                            If size.Width > (2 * Me.Padding + maxL1 + maxL2 + maxL3) Then
                                Me.Width = 2 * Me.Padding + size.Width
                            Else
                                Me.Width = 6 * Me.Padding + maxL1 + maxL2 + maxL3
                            End If
                        Else
                            If size2.Width > (2 * Me.Padding + maxL1 + maxL2 + maxL3) Then
                                Me.Width = 2 * Me.Padding + size2.Width
                            Else
                                Me.Width = 6 * Me.Padding + maxL1 + maxL2 + maxL3
                            End If
                        End If

                        Me.Width += 10.0 / zoom

                        maxL1 = maxL1 + 2 * Padding
                        maxL2 = maxL2 + 2 * Padding
                        maxL3 = maxL3 + 2 * Padding

                        maxH = maxH + 2 * Padding

                        'If Width > Owner.GetFlowsheet().GetFlowsheetSurfaceWidth * 2 / 3 Then Exit Sub
                        'If Height > Owner.GetFlowsheet().GetFlowsheetSurfaceHeight * 2 / 3 Then Exit Sub

                        'draw shadow

                        If Not s.DarkMode Then
                            Me.DrawRoundRect(g, X + 4 / zoom, Y + 4 / zoom, Width, Height, 2 / zoom, spaint)
                        End If
                        Dim rect0 As SKRect = GetRect(X + 4 / zoom, Y + 4 / zoom, Width, Height)

                        Dim rect As SKRect = GetRect(X, Y, Width, Height)

                        DrawRoundRect(g, X, Y, Width, Height, 2 / zoom, bpaint)
                        DrawRoundRect(g, X, Y, Width, Height, 2 / zoom, bpaint2)

                        canvas.DrawLine(X + Padding + 3 / zoom, Y + 2 * maxH - Padding, X + Width - Padding - 3 / zoom, Y + 2 * maxH - Padding, bpaint2)

                        size = MeasureString("MEASURE", tpaint)

                        'desenhar textos e retangulos
                        canvas.DrawText(Me.Owner.GraphicObject.Tag.ToUpper, X + Padding + 3 / zoom, Y + Padding + size.Height, tbpaint)
                        canvas.DrawText(Owner.GetFlowsheet.GetTranslatedString(Me.Owner.GraphicObject.Description), X + Padding + 3 / zoom, Y + maxH + size.Height, tpaint)
                        Dim n As Integer = 1
                        For Each prop In props
                            propstring = Owner.GetFlowsheet.GetTranslatedString(prop)
                            pval0 = Owner.GetPropertyValue(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                            If pval0 Is Nothing Then Exit For
                            If TypeOf pval0 Is Double Then
                                propval = Convert.ToDouble(pval0).ToString(Owner.GetFlowsheet.FlowsheetOptions.NumberFormat)
                            Else
                                propval = pval0.ToString
                            End If
                            propunit = Owner.GetPropertyUnit(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                            canvas.DrawText(propstring, X + Padding + 3.0 / zoom, Y + (n + 1) * maxH + Padding + size.Height, tbpaint)
                            canvas.DrawText(propval, (maxL2 - MeasureString(propval, tpaint).Width) + X + maxL1 + 3.0 / zoom, Y + (n + 1) * maxH + Padding + size.Height, tpaint)
                            canvas.DrawText(propunit, X + maxL1 + maxL2 + Padding + 3.0 / zoom, Y + (n + 1) * maxH + Padding + size.Height, tbpaint)
                            n += 1
                        Next

                        props.Clear()
                        props = Nothing

                    Else

                        Dim MSObj = DirectCast(Me.Owner, Interfaces.IMaterialStream)

                        Dim ABasis = MSObj.FloatingTableAmountBasis

                        If ABasis = CompositionBasis.DefaultBasis Then ABasis = MSObj.Flowsheet.FlowsheetOptions.DefaultFloatingTableCompoundAmountBasis

                        Dim maxL4, maxL5, maxL6, maxL7, maxL8, maxL9, count2 As Double
                        Dim maxH2 As Double

                        count2 = 2

                        Dim nf = "G6"

                        Dim compounds = MSObj.Phases(0).Compounds.Select(Function(x) x.Value.Name).ToList

                        Dim p As Interfaces.IPhase

                        size = MeasureString(MSObj.Flowsheet.GetTranslatedString("CompoundsPhases"), tbpaint)
                        If size.Width > maxL4 Then maxL4 = size.Width

                        size = MeasureString(MSObj.Flowsheet.GetTranslatedString("Overall"), tbpaint)
                        If size.Width > maxL5 Then maxL5 = size.Width

                        size = MeasureString(MSObj.Flowsheet.GetTranslatedString("Vapor"), tbpaint)
                        If size.Width > maxL6 Then maxL6 = size.Width

                        size = MeasureString(MSObj.Flowsheet.GetTranslatedString("Liquid1"), tbpaint)
                        If size.Width > maxL7 Then maxL7 = size.Width

                        size = MeasureString(MSObj.Flowsheet.GetTranslatedString("Liquid2"), tbpaint)
                        If size.Width > maxL8 Then maxL8 = size.Width

                        size = MeasureString(MSObj.Flowsheet.GetTranslatedString("Solid"), tbpaint)
                        If size.Width > maxL9 Then maxL9 = size.Width

                        Dim su = MSObj.Flowsheet.FlowsheetOptions.SelectedUnitSystem

                        Dim bprop As String = ""

                        Dim convertfrom As String = ""

                        Select Case ABasis
                            Case CompositionBasis.Mass_Flows
                                bprop = "MassFlow"
                                convertfrom = su.massflow
                            Case CompositionBasis.Mass_Fractions
                                bprop = "MassFraction"
                            Case CompositionBasis.Molar_Flows
                                bprop = "MolarFlow"
                                convertfrom = su.molarflow
                            Case CompositionBasis.Molar_Fractions
                                bprop = "MoleFraction"
                            Case CompositionBasis.Volumetric_Flows
                                bprop = "VolumetricFlow"
                                convertfrom = su.volumetricFlow
                            Case CompositionBasis.Volumetric_Fractions
                                bprop = "VolumetricFraction"
                        End Select

                        Dim bpval As Nullable(Of Double)

                        For Each c In compounds

                            size = MeasureString(c, tpaint)
                            If size.Width > maxL4 Then maxL4 = size.Width
                            If size.Height > maxH2 Then maxH2 = size.Height

                            p = MSObj.GetPhase("Mixture")

                            bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))

                            size = MeasureString(bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf), tpaint)
                            If size.Width > maxL5 Then maxL5 = size.Width
                            If size.Height > maxH2 Then maxH2 = size.Height

                            p = MSObj.GetPhase("Vapor")

                            bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))

                            size = MeasureString(bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf), tpaint)
                            If size.Width > maxL6 Then maxL6 = size.Width
                            If size.Height > maxH2 Then maxH2 = size.Height

                            p = MSObj.GetPhase("Liquid1")

                            bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))

                            size = MeasureString(bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf), tpaint)
                            If size.Width > maxL7 Then maxL7 = size.Width
                            If size.Height > maxH2 Then maxH2 = size.Height

                            p = MSObj.GetPhase("Liquid2")

                            bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))

                            size = MeasureString(bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf), tpaint)
                            If size.Width > maxL8 Then maxL8 = size.Width
                            If size.Height > maxH2 Then maxH2 = size.Height

                            p = MSObj.GetPhase("Solid")

                            bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))

                            size = MeasureString(bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf), tpaint)
                            If size.Width > maxL9 Then maxL9 = size.Width
                            If size.Height > maxH2 Then maxH2 = size.Height

                            count2 += 1

                        Next

                        Dim sumL = maxL1 + maxL2 + maxL3
                        Dim sumL2 = maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + maxL9

                        'If Not Me.AdditionalInfo Is Nothing Then Me.Padding = 3 / Me.AdditionalInfo

                        If maxH = 0 Then maxH = 20.0 / zoom

                        Me.Height = (count + 1) * (maxH + 2 * Me.Padding)

                        Dim Height2 = (count2 + 4) * (maxH2 + 2 * Me.Padding)

                        size = MeasureString(Me.HeaderText, tpaint)
                        size2 = MeasureString(MSObj.Flowsheet.GetTranslatedString(Me.Owner.GraphicObject.Description), tpaint)

                        If size.Width > size2.Width Then
                            If size.Width > (2 * Me.Padding + sumL) Then
                                Me.Width = 2 * Me.Padding + size.Width
                            Else
                                Me.Width = 6 * Me.Padding + sumL
                            End If
                        Else
                            If size2.Width > (2 * Me.Padding + sumL) Then
                                Me.Width = 2 * Me.Padding + size2.Width
                            Else
                                Me.Width = 6 * Me.Padding + sumL
                            End If
                        End If

                        Me.Width += 10 / zoom

                        Dim Width2 = 16 * Me.Padding + sumL2 + 6 / zoom

                        Dim delta = 2 * Padding

                        maxL1 += delta
                        maxL2 += delta
                        maxL3 += delta
                        maxL4 += delta
                        maxL5 += delta
                        maxL6 += delta
                        maxL7 += delta
                        maxL8 += delta
                        maxL9 += delta

                        maxH += delta
                        maxH2 += delta

                        'draw shadow

                        DrawRoundRect(g, X + 4 / zoom, Y + 4 / zoom, Width, Height, 2 / zoom, spaint)
                        DrawRoundRect(g, X, Y, Width, Height, 2 / zoom, bpaint)
                        DrawRoundRect(g, X, Y, Width, Height, 2 / zoom, bpaint2)

                        canvas.DrawLine(X + Padding + 3 / zoom, Y + 2 * maxH - Padding, X + Width - Padding - 3 / zoom, Y + 2 * maxH - Padding, bpaint2)

                        size = MeasureString("MEASURE", tpaint)

                        'desenhar textos e retangulos
                        canvas.DrawText(Me.Owner.GraphicObject.Tag, X + Padding + 3 / zoom, Y + Padding + size.Height, tbpaint)
                        canvas.DrawText(MSObj.Flowsheet.GetTranslatedString(Me.Owner.GraphicObject.Description), X + Padding + 3 / zoom, Y + maxH + size.Height, tpaint)
                        Dim n As Integer = 1
                        For Each prop In props
                            propstring = Owner.GetFlowsheet.GetTranslatedString(prop)
                            pval0 = Owner.GetPropertyValue(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                            If pval0 Is Nothing Then Exit For
                            If TypeOf pval0 Is Double Then
                                propval = Convert.ToDouble(pval0).ToString(Owner.GetFlowsheet.FlowsheetOptions.NumberFormat)
                            Else
                                propval = pval0.ToString
                            End If
                            propunit = Owner.GetPropertyUnit(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                            canvas.DrawText(propstring, X + Padding + 3.0 / zoom, Y + (n + 1) * maxH + Padding + size.Height, tbpaint)
                            canvas.DrawText(propval, (maxL2 - MeasureString(propval, tpaint).Width) + X + maxL1 + 3.0 / zoom, Y + (n + 1) * maxH + Padding + size.Height, tpaint)
                            canvas.DrawText(propunit, X + maxL1 + maxL2 + Padding + 3.0 / zoom, Y + (n + 1) * maxH + Padding + size.Height, tbpaint)
                            n += 1
                        Next

                        Dim DeltaY As Integer = -Height2 - (n + 3) * Padding + size.Height

                        'If MSObj.Flowsheet.FlowsheetOptions.DisplayFloatingTableCompoundAmounts And (Y + DeltaY) > 0 Then

                        If MSObj.Flowsheet.FlowsheetOptions.DisplayFloatingTableCompoundAmounts And (Y + DeltaY) > 0 Then

                            If Width2 > Owner.GetFlowsheet().GetFlowsheetSurfaceWidth * 2 / 3 Then Exit Sub
                            If Height2 > Owner.GetFlowsheet().GetFlowsheetSurfaceHeight * 2 / 3 Then Exit Sub

                            'draw shadow

                            DrawRoundRect(g, X + 4 / zoom, Y + DeltaY + 4 / zoom - size.Height, Width2, Height2, 2 / zoom, spaint)
                            DrawRoundRect(g, X, Y + DeltaY - size.Height, Width2, Height2, 2 / zoom, bpaint)
                            DrawRoundRect(g, X, Y + DeltaY - size.Height, Width2, Height2, 2 / zoom, bpaint2)

                            canvas.DrawLine(X + Padding + 3 / zoom, Y + 3 * maxH2 - 2 * Padding + DeltaY, X + Width2 - Padding - 3 / zoom, Y + 3 * maxH2 - 2 * Padding + DeltaY, bpaint2)

                            Dim atext As String = ""

                            Select Case ABasis
                                Case CompositionBasis.Mass_Flows
                                    atext = MSObj.Flowsheet.GetTranslatedString("Vazomssica") + " (" + su.massflow + ")"
                                Case CompositionBasis.Mass_Fractions
                                    atext = MSObj.Flowsheet.GetTranslatedString("FraoMssica")
                                Case CompositionBasis.Molar_Flows
                                    atext = MSObj.Flowsheet.GetTranslatedString("Vazomolar") + " (" + su.molarflow + ")"
                                Case CompositionBasis.Molar_Fractions
                                    atext = MSObj.Flowsheet.GetTranslatedString("FraoMolar1")
                                Case CompositionBasis.Volumetric_Flows
                                    atext = MSObj.Flowsheet.GetTranslatedString("Vazovolumtrica") + " (" + su.volumetricFlow + ")"
                                Case CompositionBasis.Volumetric_Fractions
                                    atext = MSObj.Flowsheet.GetTranslatedString("VolumetricFraction")
                            End Select

                            canvas.DrawText(Me.HeaderText, X + Padding + 3 / zoom, Y + Padding + DeltaY, tbpaint)
                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("CompoundAmounts") + atext, X + Padding + 3 / zoom, Y + maxH2 + DeltaY, tpaint)
                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("CompoundsPhases"), X + Padding + 3 / zoom, Y + 2 * maxH2 + DeltaY, tbpaint)

                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("Overall"), New SKPoint(X + maxL4 + (maxL5 - MeasureString(MSObj.Flowsheet.GetTranslatedString("Overall"), tpaint).Width) + Padding + 3 / zoom, Y + 2 * maxH2 + DeltaY), tbpaint)
                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("Vapor"), New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(MSObj.Flowsheet.GetTranslatedString("Vapor"), tpaint).Width) + Padding + 3 / zoom, Y + 2 * maxH2 + DeltaY), tbpaint)
                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("Liquid1"), New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(MSObj.Flowsheet.GetTranslatedString("Liquid1"), tpaint).Width) + Padding + 3 / zoom, Y + 2 * maxH2 + DeltaY), tbpaint)
                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("Liquid2"), New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(MSObj.Flowsheet.GetTranslatedString("Liquid2"), tpaint).Width) + Padding + 3 / zoom, Y + 2 * maxH2 + DeltaY), tbpaint)
                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("Solid"), New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(MSObj.Flowsheet.GetTranslatedString("Solid"), tpaint).Width) + Padding + 3 / zoom, Y + 2 * maxH2 + DeltaY), tbpaint)

                            Dim bpval2 As String = ""

                            n = 1
                            For Each c In compounds

                                canvas.DrawText(c, New SKPoint(X + Padding + 3 / zoom, Y + (n + 2) * maxH2 + Padding + DeltaY), tbpaint)

                                p = MSObj.GetPhase("Mixture")

                                bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))
                                bpval2 = bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)

                                canvas.DrawText(bpval2, New SKPoint(X + maxL4 + (maxL5 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 2) * maxH2 + Padding + DeltaY), tpaint)

                                p = MSObj.GetPhase("Vapor")

                                bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))
                                bpval2 = bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)

                                canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 2) * maxH2 + Padding + DeltaY), tpaint)

                                p = MSObj.GetPhase("Liquid1")

                                bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))
                                bpval2 = bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)

                                canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 2) * maxH2 + Padding + DeltaY), tpaint)

                                p = MSObj.GetPhase("Liquid2")

                                bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))
                                bpval2 = bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)

                                canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 2) * maxH2 + Padding + DeltaY), tpaint)

                                p = MSObj.GetPhase("Solid")

                                bpval = p.Compounds(c).GetType.GetProperty(bprop).GetValue(p.Compounds(c))
                                bpval2 = bpval.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)

                                canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 2) * maxH2 + Padding + DeltaY), tpaint)

                                n += 1

                            Next

                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("Fraction"), X + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY, tbpaint)
                            canvas.DrawText(MSObj.Flowsheet.GetTranslatedString("Total"), X + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY, tbpaint)

                            Select Case ABasis
                                Case CompositionBasis.Mass_Flows
                                    p = MSObj.GetPhase("Overall")
                                    bpval2 = p.Properties.massflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + (maxL5 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Vapor")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.massflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid1")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.massflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid2")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.massflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Solid")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.massflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                Case CompositionBasis.Mass_Fractions
                                    atext = MSObj.Flowsheet.GetTranslatedString("FraoMssica")
                                    p = MSObj.GetPhase("Vapor")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid1")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid2")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Solid")
                                    bpval2 = p.Properties.massfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                Case CompositionBasis.Molar_Flows
                                    p = MSObj.GetPhase("Overall")
                                    bpval2 = p.Properties.molarflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + (maxL5 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Vapor")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.molarflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid1")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.molarflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid2")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.molarflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Solid")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    bpval2 = p.Properties.molarflow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                Case CompositionBasis.Molar_Fractions
                                    p = MSObj.GetPhase("Vapor")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid1")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid2")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Solid")
                                    bpval2 = p.Properties.molarfraction.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 3) * maxH2 + Padding + DeltaY), tpaint)
                                Case CompositionBasis.Volumetric_Flows
                                    p = MSObj.GetPhase("Overall")
                                    bpval2 = p.Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + (maxL5 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Vapor")
                                    bpval2 = p.Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + (maxL6 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid1")
                                    bpval2 = p.Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + (maxL7 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Liquid2")
                                    bpval2 = p.Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + (maxL8 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                    p = MSObj.GetPhase("Solid")
                                    bpval2 = p.Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(convertfrom).ToString(nf)
                                    canvas.DrawText(bpval2, New SKPoint(X + maxL4 + maxL5 + maxL6 + maxL7 + maxL8 + (maxL9 - MeasureString(bpval2, tpaint).Width) + Padding + 3 / zoom, Y + (n + 4) * maxH2 + Padding + DeltaY), tpaint)
                                Case CompositionBasis.Volumetric_Fractions
                            End Select

                        End If


                    End If

                End If

            End If

        End Sub

    End Class

End Namespace

