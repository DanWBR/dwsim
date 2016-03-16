'    UNIQUAC Property Package 
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

Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports System.Math
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses

Namespace DWSIM.SimulationObjects.PropertyPackages

    <System.Runtime.InteropServices.Guid(UNIQUACPropertyPackage.ClassId)> _
<System.Serializable()> Public Class UNIQUACPropertyPackage

        Inherits DWSIM.SimulationObjects.PropertyPackages.ActivityCoefficientPropertyPackage

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
            Me.ConfigForm = New FormConfigUNIQUAC
            Me._packagetype = PropertyPackages.PackageType.ActivityCoefficient

            Me.m_act = New Auxiliary.UNIQUAC

        End Sub

        Public Overrides Sub ReconfigureConfigForm()
            MyBase.ReconfigureConfigForm()
            Me.ConfigForm = New FormConfigUNIQUAC
        End Sub

        Public Overrides Sub ConfigParameters()
            m_par = New System.Collections.Generic.Dictionary(Of String, Double)
            With Me.Parameters
                .Clear()
                .Add("PP_PHFILT", 0.001)
                .Add("PP_PSFILT", 0.001)
                .Add("PP_PHFELT", 0.001)
                .Add("PP_PSFELT", 0.001)
                .Add("PP_PHFMEI", 50)
                .Add("PP_PSFMEI", 50)
                .Add("PP_PHFMII", 100)
                .Add("PP_PSFMII", 100)
                .Add("PP_PTFMEI", 100)
                .Add("PP_PTFMII", 100)
                .Add("PP_PTFILT", 0.001)
                .Add("PP_PTFELT", 0.001)
                .Add("PP_FLASHALGORITHM", 2)
                .Add("PP_FLASHALGORITHMFASTMODE", 1)
                .Add("PP_IDEAL_MIXRULE_LIQDENS", 0)
                .Add("PP_USEEXPLIQDENS", 0)
                .Add("PP_USE_EOS_LIQDENS", 0)
                .Add("PP_IDEAL_VAPOR_PHASE_FUG", 1)
                .Add("PP_ENTH_CP_CALC_METHOD", 1)
                .Item("PP_IDEAL_MIXRULE_LIQDENS") = 1
                .Item("PP_USEEXPLIQDENS") = 1
            End With
        End Sub

#Region "    Auxiliary Functions"

        Function RET_VQ() As Object

            Dim subst As DWSIM.Thermodynamics.BaseClasses.Compound
            Dim VQ(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VQ(i) = subst.ConstantProperties.UNIQUAC_Q
                i += 1
            Next

            Return VQ

        End Function

        Function RET_VR() As Object

            Dim subst As DWSIM.Thermodynamics.BaseClasses.Compound
            Dim VR(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VR(i) = subst.ConstantProperties.UNIQUAC_R
                i += 1
            Next

            Return VR

        End Function

#End Region

    End Class

End Namespace


