'    Modified UNIFAC (NIST) Property Package 
'    Copyright 2015 Daniel Wagner O. de Medeiros
'    Copyright 2015 Gregor Reichert
'
'    Based on the paper entitled "New modified UNIFAC parameters using critically 
'    evaluated phase equilibrium data", http://dx.doi.org/10.1016/j.fluid.2014.12.042
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
Imports System.Math


Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(NISTMFACPropertyPackage.ClassId)> _
     <System.Serializable()> Public Class NISTMFACPropertyPackage

        Inherits PropertyPackages.ActivityCoefficientPropertyPackage

        Public Shadows Const ClassId As String = "519EB917-0B2E-4ac1-9AF2-2D1A2A55067F"

        Public Overrides ReadOnly Property DisplayName As String = "Modified UNIFAC (NIST)"

        Public Overrides ReadOnly Property DisplayDescription As String =
            "Uses the Uses the NIST modification of the UNIFAC Model to calculate liquid phase activity coefficients."

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Property m_uni As Auxiliary.NISTMFAC
            Get
                Return m_act
            End Get
            Set(value As Auxiliary.NISTMFAC)
                m_act = m_uni
            End Set
        End Property
    
        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
            Me.m_act = New Auxiliary.NISTMFAC
        End Sub

        Public Sub New()

            MyBase.New(False)
            Me.m_act = New Auxiliary.NISTMFAC

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.ActivityCoefficient

        End Sub
        Public Overrides Function GetModel() As Object
            Return m_uni
        End Function

#Region "    Auxiliary Functions"

        Public Function RET_VN(ByVal subst As Interfaces.ICompound) As Object

            Return Me.m_uni.RET_VN(subst.ConstantProperties)

        End Function

        Public Function RET_VQ() As Object

            Dim subst As Interfaces.ICompound
            Dim VQ(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0
            Dim sum As Double = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VQ(i) = Me.m_uni.RET_Qi(Me.RET_VN(subst))
                i += 1
            Next

            Return VQ

        End Function

        Public Function RET_VR() As Object

            Dim subst As Interfaces.ICompound
            Dim VR(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0
            Dim sum As Double = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VR(i) = Me.m_uni.RET_Ri(Me.RET_VN(subst))
                i += 1
            Next

            Return VR

        End Function

        Public Function RET_VEKI() As List(Of Dictionary(Of Integer, Double))

            Dim subst As Interfaces.ICompound
            Dim VEKI As New List(Of Dictionary(Of Integer, Double))
            Dim i As Integer = 0
            Dim sum As Double
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                sum = 0
                If subst.ConstantProperties.NISTMODFACGroups.Count > 0 Then
                    For Each s As String In subst.ConstantProperties.NISTMODFACGroups.Keys
                        sum += subst.ConstantProperties.NISTMODFACGroups(s) * Me.m_uni.ModfGroups.Groups(s).Q
                    Next
                Else
                    For Each s As String In subst.ConstantProperties.MODFACGroups.Keys
                        sum += subst.ConstantProperties.MODFACGroups(s) * Me.m_uni.ModfGroups.Groups(s).Q
                    Next
                End If
                Dim obj = Me.m_uni.RET_EKI(Me.RET_VN(subst), sum)
                VEKI.Add(obj)
            Next

            Return VEKI

        End Function

        Public Overrides Function CheckMissingInteractionParameters(Vx As Double()) As Boolean
            Return False
        End Function

#End Region

    End Class

End Namespace


