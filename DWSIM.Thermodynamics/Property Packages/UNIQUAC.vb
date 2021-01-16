'    UNIQUAC Property Package 
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

Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.Math


Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(UNIQUACPropertyPackage.ClassId)> _
<System.Serializable()> Public Class UNIQUACPropertyPackage

        Inherits PropertyPackages.ActivityCoefficientPropertyPackage

        Public Shadows Const ClassId As String = "5265F953-8825-4a80-9112-A3B68C329E4C"

        Public Property m_uni As Auxiliary.UNIQUAC
            Get
                Return m_act
            End Get
            Set(value As Auxiliary.UNIQUAC)
                m_act = m_uni
            End Set
        End Property

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
            Me.m_act = New Auxiliary.UNIQUAC
        End Sub

        Public Sub New()

            MyBase.New(False)

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.ActivityCoefficient

            Me.m_act = New Auxiliary.UNIQUAC

        End Sub

        Public Overrides Sub DisplayEditingForm()

            Dim f As New FormConfigUNIQUAC() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                            f.ShowDialog()

        End Sub
        Public Overrides Function GetModel() As Object
            Return m_uni
        End Function

#Region "    Auxiliary Functions"

        Function RET_VQ() As Object

            Dim subst As Interfaces.ICompound
            Dim VQ(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VQ(i) = subst.ConstantProperties.UNIQUAC_Q
                i += 1
            Next

            Return VQ

        End Function

        Function RET_VR() As Object

            Dim subst As Interfaces.ICompound
            Dim VR(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VR(i) = subst.ConstantProperties.UNIQUAC_R
                i += 1
            Next

            Return VR

        End Function

        Public Overrides Function CheckMissingInteractionParameters(Vx As Double()) As Boolean

            Dim ipdata(1, 8) As Object

            Dim i1, i2 As Integer
            i1 = 0
            For Each c In CurrentMaterialStream.Phases(0).Compounds.Values
                i2 = 0
                For Each c2 In CurrentMaterialStream.Phases(0).Compounds.Values
                    If c.Name <> c2.Name AndAlso Vx(i1) * Vx(i2) > 0.0 Then
                        ipdata = ExcelAddIn.ExcelIntegration.GetInteractionParameterSet(Me, "UNIQUAC", c.Name, c2.Name)
                        Dim i As Integer, sum As Double
                        sum = 0
                        For i = 2 To 8
                            If ipdata(1, i) IsNot Nothing Then sum += ipdata(1, i)
                        Next
                        If sum = 0.0 Then Throw New Exception(String.Format("UNIQUAC error: missing interaction parameters for binary pair {0}/{1}.", c.Name, c2.Name))
                    End If
                    i2 += 1
                Next
                i1 += 1
            Next

        End Function

#End Region

    End Class

End Namespace


