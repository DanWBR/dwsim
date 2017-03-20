Imports System.Threading.Tasks

Module Export

    Sub Main()

        Dim clist = New ChEDLThermoParser().GetSupportedCompounds()

        Dim collection As New Concurrent.ConcurrentBag(Of BaseClasses.ConstantProperties)

        Parallel.ForEach(clist,
                         New ParallelOptions() With {.MaxDegreeOfParallelism = 12},
                         Sub(c)
                             Dim parser As New ChEDLThermoParser()
                             Dim compound As BaseClasses.ConstantProperties = Nothing
                             Try
                                 compound = parser.GetCompoundData(c.Trim("'"))
                                 compound.Comments = ""
                             Catch ex As Exception
                             End Try
                             If Not compound Is Nothing Then
                                 Try
                                     Dim ddbid As String = DDBStructureLink.DDBStructureParser.GetID(compound.CAS_Number)
                                     If ddbid IsNot Nothing Then
                                         Dim structuredata = DDBStructureLink.DDBStructureParser.GetData(ddbid)
                                         If Not structuredata Is Nothing Then
                                             With compound
                                                 If structuredata.ContainsKey("Original") Then
                                                     If .UNIFACGroups Is Nothing Then .UNIFACGroups = New SortedList
                                                     .UNIFACGroups.Clear()
                                                     For Each item In structuredata("Original")
                                                         .UNIFACGroups.Add(item(1), item(2))
                                                     Next
                                                 End If
                                                 If structuredata.ContainsKey("Modified") Then
                                                     If .MODFACGroups Is Nothing Then .UNIFACGroups = New SortedList
                                                     .MODFACGroups.Clear()
                                                     For Each item In structuredata("Modified")
                                                         .MODFACGroups.Add(item(1), item(2))
                                                     Next
                                                     If .NISTMODFACGroups Is Nothing Then .NISTMODFACGroups = New SortedList
                                                     .NISTMODFACGroups.Clear()
                                                     For Each sg As String In .MODFACGroups.Keys
                                                         .NISTMODFACGroups.Add(sg, .MODFACGroups(sg))
                                                     Next
                                                 End If
                                             End With
                                         End If
                                     End If
                                 Catch ex As Exception
                                 End Try
                             End If
                             parser.KillPythonProcess()
                             parser = Nothing
                             If Not compound Is Nothing Then
                                 collection.Add(compound)
                                 Console.WriteLine("Compounds added: " & collection.Count.ToString)
                             End If
                         End Sub)

        IO.File.WriteAllText("", Newtonsoft.Json.JsonConvert.SerializeObject(collection.ToArray, Newtonsoft.Json.Formatting.Indented))

    End Sub

End Module
