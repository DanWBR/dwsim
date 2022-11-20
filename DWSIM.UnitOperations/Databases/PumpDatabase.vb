'    Copyright 2017 Daniel Wagner O. de Medeiros, Gregor Reichert
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
'
'    Imports DWSIM.SimulationObjects

Imports System.Xml
Imports System.IO
Imports System.Linq
Imports System.Reflection
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter


Namespace Databases

    <System.Serializable()> Public Class PumpDB

        Public Shared Sub AddPump(ByVal Pump As UnitOperations.Pump, xmlstream As Stream, ByVal replace As Boolean)

            If xmlstream.Length = 0 Then
                Dim stream2 As New MemoryStream
                Using writer As New XmlTextWriter(stream2, Text.Encoding.UTF8)
                    With writer
                        .Formatting = Formatting.Indented
                        .WriteStartDocument()
                        .WriteStartElement("Pumps")
                        .WriteEndElement()
                        .WriteEndDocument()
                        .Flush()
                    End With
                    stream2.Position = 0
                    stream2.CopyTo(xmlstream)
                End Using
            End If

            Dim cult As Globalization.CultureInfo = New Globalization.CultureInfo("en-US")
            Dim nf As Globalization.NumberFormatInfo = cult.NumberFormat

            Dim x, y As ArrayList
            Dim xv, yv As Double


            xmlstream.Position = 0

            Dim xmldoc As XmlDocument

            Using reader = XmlReader.Create(xmlstream)

                xmldoc = New XmlDocument()
                xmldoc.Load(reader)

                Dim newnode As XmlNode = xmldoc.CreateNode(XmlNodeType.Element, Pump.PumpType, "")
                With newnode
                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "Speed", "")).InnerText = Pump.ImpellerSpeed.ToString(nf)
                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "Diameter", "")).InnerText = Pump.ImpellerDiameter.ToString(nf)
                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "DiameterUnit", "")).InnerText = Pump.DiameterUnit

                    If Pump.Curves("HEAD").Enabled Then
                        With .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "Curve", ""))
                            .Attributes.Append(xmldoc.CreateAttribute("Y"))
                            .Attributes("Y").Value = Pump.Curves("HEAD").yunit
                            .Attributes.Append(xmldoc.CreateAttribute("X"))
                            .Attributes("X").Value = Pump.Curves("HEAD").xunit
                            .Attributes.Append(xmldoc.CreateAttribute("Type"))
                            .Attributes("Type").Value = "HEAD"

                            x = Pump.Curves("HEAD").x
                            y = Pump.Curves("HEAD").y

                            For i = 0 To x.Count - 1
                                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                                    xv = x(i)
                                    yv = y(i)
                                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "", "Point", ""))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("X"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("X").Value = xv.ToString(nf)
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("Y"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("Y").Value = yv.ToString(nf)
                                End If
                            Next

                        End With
                    End If

                    If Pump.Curves("POWER").Enabled Then
                        With .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "Curve", ""))
                            .Attributes.Append(xmldoc.CreateAttribute("Y"))
                            .Attributes("Y").Value = Pump.Curves("POWER").yunit
                            .Attributes.Append(xmldoc.CreateAttribute("X"))
                            .Attributes("X").Value = Pump.Curves("POWER").xunit
                            .Attributes.Append(xmldoc.CreateAttribute("Type"))
                            .Attributes("Type").Value = "POWER"

                            x = Pump.Curves("POWER").x
                            y = Pump.Curves("POWER").y

                            For i = 0 To x.Count - 1
                                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "", "Point", ""))
                                    xv = x(i)
                                    yv = y(i)
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("X"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("X").Value = xv.ToString(nf)
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("Y"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("Y").Value = yv.ToString(nf)
                                End If
                            Next

                        End With
                    End If

                    If Pump.Curves("EFF").Enabled Then
                        With .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "Curve", ""))
                            .Attributes.Append(xmldoc.CreateAttribute("Y"))
                            .Attributes("Y").Value = Pump.Curves("EFF").yunit
                            .Attributes.Append(xmldoc.CreateAttribute("X"))
                            .Attributes("X").Value = Pump.Curves("EFF").xunit
                            .Attributes.Append(xmldoc.CreateAttribute("Type"))
                            .Attributes("Type").Value = "EFF"

                            x = Pump.Curves("EFF").x
                            y = Pump.Curves("EFF").y

                            For i = 0 To x.Count - 1
                                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                                    xv = x(i)
                                    yv = y(i)
                                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "", "Point", ""))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("X"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("X").Value = xv.ToString(nf)
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("Y"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("Y").Value = yv.ToString(nf)
                                End If
                            Next

                        End With
                    End If

                    If Pump.Curves("NPSH").Enabled Then
                        With .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "Curve", ""))
                            .Attributes.Append(xmldoc.CreateAttribute("Y"))
                            .Attributes("Y").Value = Pump.Curves("NPSH").yunit
                            .Attributes.Append(xmldoc.CreateAttribute("X"))
                            .Attributes("X").Value = Pump.Curves("NPSH").xunit
                            .Attributes.Append(xmldoc.CreateAttribute("Type"))
                            .Attributes("Type").Value = "NPSH"

                            x = Pump.Curves("NPSH").x
                            y = Pump.Curves("NPSH").y

                            For i = 0 To x.Count - 1
                                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                                    xv = x(i)
                                    yv = y(i)
                                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "", "Point", ""))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("X"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("X").Value = xv.ToString(nf)
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("Y"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("Y").Value = yv.ToString(nf)
                                End If
                            Next

                        End With
                    End If

                    If Pump.Curves("SYSTEM").Enabled Then
                        With .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "Curve", ""))
                            .Attributes.Append(xmldoc.CreateAttribute("Y"))
                            .Attributes("Y").Value = Pump.Curves("SYSTEM").yunit
                            .Attributes.Append(xmldoc.CreateAttribute("X"))
                            .Attributes("X").Value = Pump.Curves("SYSTEM").xunit
                            .Attributes.Append(xmldoc.CreateAttribute("Type"))
                            .Attributes("Type").Value = "SYSTEM"

                            x = Pump.Curves("SYSTEM").x
                            y = Pump.Curves("SYSTEM").y

                            For i = 0 To x.Count - 1
                                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                                    xv = x(i)
                                    yv = y(i)
                                    .AppendChild(xmldoc.CreateNode(XmlNodeType.Element, "", "Point", ""))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("X"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("X").Value = xv.ToString(nf)
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes.Append(xmldoc.CreateAttribute("Y"))
                                    .ChildNodes(.ChildNodes.Count - 1).Attributes("Y").Value = yv.ToString(nf)
                                End If
                            Next

                        End With
                    End If

                End With

                Dim OldNode As XmlNode = xmldoc.GetElementsByTagName(Pump.PumpType).ItemOf(0)
                If IsNothing(OldNode) Then
                    xmldoc.ChildNodes(1).AppendChild(newnode)
                Else
                    xmldoc.ChildNodes(1).ReplaceChild(newnode, OldNode)
                End If

                xmldoc.Save(xmlstream)

            End Using

        End Sub

        Public Shared Sub RemovePump(ByVal xmlpath As String, ByVal PumpID As String)

            Dim xmldoc As New XmlDocument
            Dim reader As XmlReader = XmlReader.Create(xmlpath)
            reader.Read()

            xmldoc.Load(reader)
            reader.Close()
            reader = Nothing

            Dim OldNode As XmlNode = xmldoc.GetElementsByTagName(PumpID).ItemOf(0)
            If Not IsNothing(OldNode) Then
                xmldoc.ChildNodes(1).RemoveChild(OldNode)
                xmldoc.Save(xmlpath)
            End If

        End Sub

        Public Shared Function ReadPumpTypes(ByVal xmlstream As Stream) As String()

            Dim xmldoc As XmlDocument

            Using reader As XmlReader = XmlReader.Create(xmlstream)

                reader.Read()

                xmldoc = New XmlDocument
                xmldoc.Load(reader)

                Dim PTa As New List(Of String)

                For Each node As XmlNode In xmldoc.ChildNodes(1)
                    PTa.Add(node.Name)
                Next
                Return PTa.ToArray()

            End Using

        End Function
        Public Shared Sub ReadPumpData(ByVal xmlpath As String, ByVal PumpType As String, ByVal Pump As UnitOperations.Pump)
            Dim cult As Globalization.CultureInfo = New Globalization.CultureInfo("en-US")
            Dim nf As Globalization.NumberFormatInfo = cult.NumberFormat

            Dim xmldoc As XmlDocument
            Dim reader As XmlReader = XmlReader.Create(xmlpath)
            reader.Read()

            xmldoc = New XmlDocument
            xmldoc.Load(reader)
            Dim Node As XmlNode = xmldoc.GetElementsByTagName(PumpType).ItemOf(0)

            Pump.Curves("HEAD").Enabled = False
            Pump.Curves("HEAD").x.Clear()
            Pump.Curves("HEAD").y.Clear()

            Pump.Curves("POWER").Enabled = False
            Pump.Curves("POWER").x.Clear()
            Pump.Curves("POWER").y.Clear()

            Pump.Curves("EFF").Enabled = False
            Pump.Curves("EFF").x.Clear()
            Pump.Curves("EFF").y.Clear()

            Pump.Curves("NPSH").Enabled = False
            Pump.Curves("NPSH").x.Clear()
            Pump.Curves("NPSH").y.Clear()

            Pump.Curves("SYSTEM").Enabled = False
            Pump.Curves("SYSTEM").x.Clear()
            Pump.Curves("SYSTEM").y.Clear()


            For Each node2 As XmlNode In Node.ChildNodes
                With Pump
                    If node2.Name = "Speed" Then Pump.ImpellerSpeed = Double.Parse(node2.InnerText, nf)
                    If node2.Name = "Diameter" Then Pump.ImpellerDiameter = Double.Parse(node2.InnerText, nf)
                    If node2.Name = "DiameterUnit" Then Pump.DiameterUnit = node2.InnerText

                    If node2.Name = "Curve" Then
                        Dim T As String = node2.Attributes.GetNamedItem("Type").Value
                        Dim XUnit As String = node2.Attributes.GetNamedItem("X").Value
                        Dim YUnit As String = node2.Attributes.GetNamedItem("Y").Value
                        Select Case T
                            Case "HEAD"
                                Pump.Curves("HEAD").Enabled = True
                                Pump.Curves("HEAD").xunit = XUnit
                                Pump.Curves("HEAD").yunit = YUnit

                                For i = 0 To node2.ChildNodes.Count - 1
                                    Dim Node3 As XmlNode = node2.ChildNodes(i)
                                    Pump.Curves("HEAD").x.Add(Double.Parse(Node3.Attributes.GetNamedItem("X").Value, nf))
                                    Pump.Curves("HEAD").y.Add(Double.Parse(Node3.Attributes.GetNamedItem("Y").Value, nf))
                                Next
                            Case "POWER"
                                Pump.Curves("POWER").Enabled = True
                                Pump.Curves("POWER").xunit = XUnit
                                Pump.Curves("POWER").yunit = YUnit

                                For i = 0 To node2.ChildNodes.Count - 1
                                    Dim Node3 As XmlNode = node2.ChildNodes(i)
                                    Pump.Curves("POWER").x.Add(Double.Parse(Node3.Attributes.GetNamedItem("X").Value, nf))
                                    Pump.Curves("POWER").y.Add(Double.Parse(Node3.Attributes.GetNamedItem("Y").Value, nf))
                                Next
                            Case "EFF"
                                Pump.Curves("EFF").Enabled = True
                                Pump.Curves("EFF").xunit = XUnit
                                Pump.Curves("EFF").yunit = YUnit

                                For i = 0 To node2.ChildNodes.Count - 1
                                    Dim Node3 As XmlNode = node2.ChildNodes(i)
                                    Pump.Curves("EFF").x.Add(Double.Parse(Node3.Attributes.GetNamedItem("X").Value, nf))
                                    Pump.Curves("EFF").y.Add(Double.Parse(Node3.Attributes.GetNamedItem("Y").Value, nf))
                                Next
                            Case "NPSH"
                                Pump.Curves("NPSH").Enabled = True
                                Pump.Curves("NPSH").xunit = XUnit
                                Pump.Curves("NPSH").yunit = YUnit

                                For i = 0 To node2.ChildNodes.Count - 1
                                    Dim Node3 As XmlNode = node2.ChildNodes(i)
                                    Pump.Curves("NPSH").x.Add(Double.Parse(Node3.Attributes.GetNamedItem("X").Value, nf))
                                    Pump.Curves("NPSH").y.Add(Double.Parse(Node3.Attributes.GetNamedItem("Y").Value, nf))
                                Next
                            Case "SYSTEM"
                                Pump.Curves("SYSTEM").Enabled = True
                                Pump.Curves("SYSTEM").xunit = XUnit
                                Pump.Curves("SYSTEM").yunit = YUnit

                                For i = 0 To node2.ChildNodes.Count - 1
                                    Dim Node3 As XmlNode = node2.ChildNodes(i)
                                    Pump.Curves("SYSTEM").x.Add(Double.Parse(Node3.Attributes.GetNamedItem("X").Value, nf))
                                    Pump.Curves("SYSTEM").y.Add(Double.Parse(Node3.Attributes.GetNamedItem("Y").Value, nf))
                                Next
                        End Select
                    End If
                End With
            Next

            xmldoc = Nothing

            reader.Close()
            reader = Nothing

        End Sub

    End Class

End Namespace
