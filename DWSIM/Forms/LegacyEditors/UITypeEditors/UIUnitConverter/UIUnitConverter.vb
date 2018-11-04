'    UITypeEditor for Input Value Unit Conversion
'    Copyright 2010 Daniel Wagner O. de Medeiros
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

Imports System.Windows.Forms.Design
Imports System.Drawing.Design
Imports DWSIM.SharedClasses.SystemsOfUnits

Namespace DWSIM.Editors.Generic

    <System.Serializable()> Public Class UIUnitConverter

        Inherits System.Drawing.Design.UITypeEditor

        Private editorService As IWindowsFormsEditorService

        Dim loaded As Boolean = False

        Public WithEvents lb As System.Windows.Forms.ComboBox
        Public us As SharedClasses.SystemsOfUnits.Units
        Public nf As String
        Public utype As String
        Public ufrom As String
        Public uto As String
        Public uvalue As String

        Public Overrides Function GetEditStyle(ByVal context As System.ComponentModel.ITypeDescriptorContext) As UITypeEditorEditStyle
            If Not context Is Nothing AndAlso Not context.Instance Is Nothing Then
                Return UITypeEditorEditStyle.DropDown
            End If
            Return UITypeEditorEditStyle.None
        End Function

        Public Overrides Function EditValue(ByVal context As System.ComponentModel.ITypeDescriptorContext, ByVal provider As IServiceProvider, ByVal value As Object) As Object

            If (provider IsNot Nothing) Then
                editorService = CType(provider.GetService(GetType(IWindowsFormsEditorService)),
                IWindowsFormsEditorService)
            End If

            If (editorService IsNot Nothing) Then

                lb = New ComboBox
                With lb
                    .Name = "lb"
                    .DropDownStyle = ComboBoxStyle.Simple
                    .IntegralHeight = True
                    .RightToLeft = RightToLeft.No
                    .Location = New System.Drawing.Point(0, 0)
                    .Height = 130
                End With

                Dim pgrid As PropertyGridEx.PropertyGridEx = My.Application.ActiveSimulation.FormProps.PGEx1

                Dim idx As Integer = pgrid.Item.FindItem(context.PropertyDescriptor.DisplayName)

                With pgrid.Item(idx)
                    nf = .Tag(0)
                    uto = .Tag(1)
                    utype = .Tag(2)
                End With

                Select Case utype
                    Case "T"
                        lb.Items.AddRange(New String() {"K", "R", "C", "F"})
                    Case "P"
                        lb.Items.AddRange(New String() {"Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"})
                    Case "W"
                        lb.Items.AddRange(New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"})
                    Case "M"
                        lb.Items.AddRange(New String() {"mol/s", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "lbmol/h", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"})
                    Case "Q"
                        lb.Items.AddRange(New String() {"m3/s", "ft3/s", "cm3/s", "m3/h", "m3/d", "bbl/h", "bbl/d", "ft3/min", "ft3/d", "gal[UK]/h", "gal[UK]/s", "gal[US]/h", "gal[US]/min", "L/h", "L/min", "L/s"})
                    Case "DP"
                        lb.Items.AddRange(New String() {"Pa", "atm", "lbf/ft2", "kgf/cm2", "kPa", "bar", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi"})
                    Case "E"
                        lb.Items.AddRange(New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"})
                    Case "A"
                        lb.Items.AddRange(New String() {"m2", "cm2", "ft2"})
                End Select

                loaded = True

                editorService.DropDownControl(lb)

                If Not ufrom Is Nothing Then
                    If Double.TryParse(uvalue, New Double) Then
                        value = Converter.ConvertFromSI(uto, Converter.ConvertToSI(ufrom, uvalue))
                    Else
                        MsgBox("Input value is not a valid number.", MsgBoxStyle.OkOnly, "Error")
                    End If
                End If

            End If

            Return value

        End Function

        Private Sub lb_SelectedValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles lb.SelectedValueChanged
            If loaded Then
                ufrom = lb.SelectedItem.ToString
                Me.editorService.CloseDropDown()
            End If
        End Sub

        Private Sub lb_SelectionChangeCommitted(ByVal sender As Object, ByVal e As System.EventArgs) Handles lb.SelectionChangeCommitted
            If Double.TryParse(lb.Text, New Double) Then uvalue = lb.Text
        End Sub

        Private Sub lb_TextUpdate(ByVal sender As Object, ByVal e As System.EventArgs) Handles lb.TextChanged
            If Double.TryParse(lb.Text, New Double) Then uvalue = lb.Text
        End Sub

    End Class

End Namespace



