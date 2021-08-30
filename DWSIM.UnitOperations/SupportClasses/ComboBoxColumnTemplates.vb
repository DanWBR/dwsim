Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps
Imports DWSIM.UnitOperations.Streams

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

Namespace UnitOperations.Auxiliary.DGVCBSelectors

    <System.Serializable()> Public Class Templates

        Dim fc As IFlowsheet
        Dim dc As Column

        Sub New(ByVal form As IFlowsheet, ByVal col As Column)

            fc = form
            dc = col
        End Sub

        Function GetMaterialStreamInSelector() As DataGridViewComboBoxCell

            Dim dgcbc As New DataGridViewComboBoxCell
            dgcbc.Sorted = True

            With dgcbc.Items
                .Clear()
                .Add("")
                For Each mstr As MaterialStream In fc.SimulationObjects.Values.Where(Function(x) TypeOf x Is MaterialStream)
                    .Add(mstr.GraphicObject.Tag.ToString)
                Next
            End With

            Return dgcbc

        End Function

        Function GetMaterialStreamOutSelector() As DataGridViewComboBoxCell

            Dim dgcbc As New DataGridViewComboBoxCell
            dgcbc.Sorted = True

            With dgcbc.Items
                .Clear()
                .Add("")
                For Each mstr As MaterialStream In fc.SimulationObjects.Values.Where(Function(x) TypeOf x Is MaterialStream)
                    .Add(mstr.GraphicObject.Tag.ToString)
                    If dc.MaterialStreams.ContainsKey(mstr.Name) Then
                        If dc.MaterialStreams(mstr.Name).StreamBehavior <> StreamInformation.Behavior.Feed Then
                            .Add(mstr.GraphicObject.Tag.ToString)
                        End If
                    End If
                Next
            End With

            Return dgcbc

        End Function

        Function GetEnergyStreamInSelector() As DataGridViewComboBoxCell

            Dim dgcbc As New DataGridViewComboBoxCell
            dgcbc.Sorted = True

            With dgcbc.Items
                .Clear()
                .Add("")
                For Each estr As EnergyStream In fc.SimulationObjects.Values.Where(Function(x) TypeOf x Is EnergyStream)
                    .Add(estr.GraphicObject.Tag.ToString)
                    If dc.EnergyStreams.ContainsKey(estr.Name) Then
                        .Add(estr.GraphicObject.Tag.ToString)
                    End If
                Next
            End With

            Return dgcbc

        End Function

        Function GetEnergyStreamOutSelector() As DataGridViewComboBoxCell

            Dim dgcbc As New DataGridViewComboBoxCell
            dgcbc.Sorted = True

            With dgcbc.Items
                .Clear()
                .Add("")
                For Each estr As EnergyStream In fc.SimulationObjects.Values.Where(Function(x) TypeOf x Is EnergyStream)
                    .Add(estr.GraphicObject.Tag.ToString)
                    If dc.EnergyStreams.ContainsKey(estr.Name) Then
                        .Add(estr.GraphicObject.Tag.ToString)
                    End If
                Next
            End With

            Return dgcbc

        End Function

        Function GetSideDrawTypeSelector() As DataGridViewComboBoxCell

            Dim dgcbc As New DataGridViewComboBoxCell
            dgcbc.Sorted = True

            With dgcbc.Items
                .Clear()
                .Add("L")
                .Add("V")
            End With

            Return dgcbc

        End Function

    End Class

End Namespace
