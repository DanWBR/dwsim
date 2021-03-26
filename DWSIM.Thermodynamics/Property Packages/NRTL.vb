'    NRTL Property Package 
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

Imports DWSIM.Thermodynamics.PropertyPackages

Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(NRTLPropertyPackage.ClassId)> _
   <System.Serializable()> Public Class NRTLPropertyPackage

        Inherits PropertyPackages.ActivityCoefficientPropertyPackage

        Public Shadows Const ClassId As String = "D42F0157-5750-4c89-A94E-634A04701568"

        Public Property m_uni As Auxiliary.NRTL
            Get
                Return m_act
            End Get
            Set(value As Auxiliary.NRTL)
                m_act = m_uni
            End Set
        End Property

        Public Sub New(ByVal comode As Boolean)

            MyBase.New(comode)

            Me.m_act = New Auxiliary.NRTL

        End Sub

        Public Sub New()

            MyBase.New(False)

            Me.m_act = New Auxiliary.NRTL

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.ActivityCoefficient

        End Sub

        Public Overrides Sub DisplayEditingForm()

            If GlobalSettings.Settings.CAPEOPENMode Then
                Dim f As New FormConfigNRTL() With {._pp = Me, ._comps = _selectedcomps.ToDictionary(Of String, Interfaces.ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)}
                f.ShowDialog()
            Else
                Dim f As New FormConfigNRTL() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                f.ShowDialog()
            End If

        End Sub

        Public Overrides Function GetEditingForm() As Form

            Return New FormConfigNRTL() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}

        End Function

        Public Overrides Function CheckMissingInteractionParameters(Vx As Double()) As Boolean

            Dim ipdata(1, 8) As Object

            Dim i1, i2 As Integer
            i1 = 0
            For Each c In CurrentMaterialStream.Phases(0).Compounds.Values
                i2 = 0
                For Each c2 In CurrentMaterialStream.Phases(0).Compounds.Values
                    If c.Name <> c2.Name AndAlso Vx(i1) * Vx(i2) > 0.0 Then
                        ipdata = ExcelAddIn.ExcelIntegration.GetInteractionParameterSet(Me, "NRTL", c.Name, c2.Name)
                        Dim i As Integer, sum As Double
                        sum = 0
                        For i = 2 To 8
                            If ipdata(1, i) IsNot Nothing Then sum += ipdata(1, i)
                        Next
                        If sum = 0.0 Then Throw New Exception(String.Format("NRTL error: missing interaction parameters for binary pair {0}/{1}.", c.Name, c2.Name))
                    End If
                    i2 += 1
                Next
                i1 += 1
            Next

            Return False

        End Function

        Public Overrides Function GetModel() As Object
            Return m_uni
        End Function

    End Class

End Namespace


