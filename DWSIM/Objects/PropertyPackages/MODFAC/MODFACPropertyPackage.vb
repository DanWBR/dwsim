'    Modified UNIFAC (Dortmund) Property Package 
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

'Imports CAPEOPEN_PD.CAPEOPEN
'Imports DWSIM.SimulationObjects
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports System.Math
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses

Namespace DWSIM.SimulationObjects.PropertyPackages

    <System.Runtime.InteropServices.Guid(MODFACPropertyPackage.ClassId)> _
     <System.Serializable()> Public Class MODFACPropertyPackage

        Inherits DWSIM.SimulationObjects.PropertyPackages.ActivityCoefficientPropertyPackage

        Public Shadows Const ClassId As String = "519EB917-0B2E-4ac1-9AF2-2D1A2A55011F"

        Public Property m_uni As Auxiliary.Modfac
            Get
                Return m_act
            End Get
            Set(value As Auxiliary.Modfac)
                m_act = m_uni
            End Set
        End Property


        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
            Me.m_act = New Auxiliary.Modfac
        End Sub

        Public Sub New()

            MyBase.New(False)

            Me.IsConfigurable = True
            Me.ConfigForm = New FormConfigPP
            Me._packagetype = PropertyPackages.PackageType.ActivityCoefficient

            Me.m_act = New Auxiliary.Modfac

        End Sub

        Public Overrides Sub ReconfigureConfigForm()
            MyBase.ReconfigureConfigForm()
            Me.ConfigForm = New FormConfigPP
        End Sub

#Region "    Auxiliary Functions"

        Public Function RET_VN(ByVal subst As DWSIM.Thermodynamics.BaseClasses.Compound) As Object

            Return Me.m_uni.RET_VN(subst.ConstantProperties)

        End Function

        Public Function RET_VQ() As Object

            Dim subst As DWSIM.Thermodynamics.BaseClasses.Compound
            Dim VQ(Me.CurrentMaterialStream.Phases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0
            Dim sum As Double = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Componentes.Values
                VQ(i) = Me.m_uni.RET_Qi(Me.RET_VN(subst))
                i += 1
            Next

            Return VQ

        End Function

        Public Function RET_VR() As Object

            Dim subst As DWSIM.Thermodynamics.BaseClasses.Compound
            Dim VR(Me.CurrentMaterialStream.Phases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0
            Dim sum As Double = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Componentes.Values
                VR(i) = Me.m_uni.RET_Ri(Me.RET_VN(subst))
                i += 1
            Next

            Return VR

        End Function

        Public Function RET_VEKI() As List(Of Dictionary(Of Integer, Double))

            Dim subst As DWSIM.Thermodynamics.BaseClasses.Compound
            Dim VEKI As New List(Of Dictionary(Of Integer, Double))
            Dim i As Integer = 0
            Dim sum As Double
            For Each subst In Me.CurrentMaterialStream.Phases(0).Componentes.Values
                sum = 0
                For Each s As String In subst.ConstantProperties.MODFACGroups.Collection.Keys
                    sum += subst.ConstantProperties.MODFACGroups.Collection(s) * Me.m_uni.ModfGroups.Groups(s).Q
                Next
                Dim obj = Me.m_uni.RET_EKI(Me.RET_VN(subst), sum)
                VEKI.Add(obj)
            Next

            Return VEKI

        End Function

#End Region

    End Class

End Namespace


