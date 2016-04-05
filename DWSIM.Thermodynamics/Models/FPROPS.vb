'    FPROPS Native Calls
'    Copyright 2012 Daniel Wagner O. de Medeiros
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

' FPROPS
' Copyright (C) 2011 - Carnegie Mellon University
'
' ASCEND is free software; you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation; either version 2 of the License, or
' (at your option) any later version.
'
' ASCEND is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with ASCEND; if not, write to the Free Software
' Foundation, Inc., 51 Franklin St, Fifth Floor,
' Boston, MA  02110-1301  USA
'/

Namespace PropertyPackages.Auxiliary.FPROPS

    <System.Serializable()> Partial Public Class FPROPS_NativeConstants

        '''FPROPS_H -> 
        '''Error generating expression: Valor nao pode ser nulo.
        '''Nome do parametro: node
        Public Const FPROPS_H As String = ""

        '''FPROPS_RUNDATA_H -> 
        '''Error generating expression: Valor nao pode ser nulo.
        '''Nome do parametro: node
        Public Const FPROPS_RUNDATA_H As String = ""

        '''FPROPS_COMMON_H -> 
        '''Error generating expression: Valor nao pode ser nulo.
        '''Nome do parametro: node
        Public Const FPROPS_COMMON_H As String = ""

        '''Warning: Generation of Method Macros is not supported at this time
        '''FPROPS_NEW -> "(TYPE) ((TYPE *)malloc(sizeof(TYPE)))"
        Public Const FPROPS_NEW As String = "(TYPE) ((TYPE *)malloc(sizeof(TYPE)))"

        '''Warning: Generation of Method Macros is not supported at this time
        '''FPROPS_FREE -> "(PTR) free(PTR)"
        Public Const FPROPS_FREE As String = "(PTR) free(PTR)"

        '''Warning: Generation of Method Macros is not supported at this time
        '''FPROPS_NEW_ARRAY -> "(TYPE,SIZE) ((TYPE*)malloc(sizeof(TYPE)*(SIZE)))"
        Public Const FPROPS_NEW_ARRAY As String = "(TYPE,SIZE) ((TYPE*)malloc(sizeof(TYPE)*(SIZE)))"

        '''Warning: Generation of Method Macros is not supported at this time
        '''FPROPS_ARRAY_COPY -> "(DEST,SRC,TYPE,SIZE) memcpy((void *)(DEST),(void *)(SRC),sizeof(TYPE)*(SIZE));"
        Public Const FPROPS_ARRAY_COPY As String = "(DEST,SRC,TYPE,SIZE) memcpy((void *)(DEST),(void *)(SRC),sizeof(TYPE)*(SIZE));"

        '''Warning: Generation of Method Macros is not supported at this time
        '''SQ -> "(X) ((X)*(X))"
        Public Const SQ As String = "(X) ((X)*(X))"

        '''FPROPS_FILEDATA_H -> 
        '''Error generating expression: Valor nao pode ser nulo.
        '''Nome do parametro: node
        Public Const FPROPS_FILEDATA_H As String = ""

        '''R_UNIVERSAL -> 8314.4621
        Public Const R_UNIVERSAL As Single = 8.314462E+7!
    End Class

    Public Enum FpropsError_enum

        FPROPS_NO_ERROR

        FPROPS_NUMERIC_ERROR

        FPROPS_SAT_CVGC_ERROR

        FPROPS_RANGE_ERROR

        FPROPS_DATA_ERROR

        FPROPS_NOT_IMPLEMENTED
    End Enum

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure CriticalData_struct

        '''double
        Public T As Double

        '''double
        Public rho As Double

        '''double
        Public p As Double
    End Structure

    Public Enum ReferenceStateType

        FPROPS_REF_PHI0

        FPROPS_REF_IIR

        FPROPS_REF_NBP

        FPROPS_REF_TRHS

        FPROPS_REF_TPUS

        FPROPS_REF_TPHS

        FPROPS_REF_TPF

        FPROPS_REF_TPFU
    End Enum

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure ReferenceStatePhi0_struct

        '''double
        Public c As Double

        '''double
        Public m As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure ReferenceStateTRHS_struct

        '''double
        Public T0 As Double

        '''double
        Public rho0 As Double

        '''double
        Public h0 As Double

        '''double
        Public s0 As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure ReferenceStateTPUS_struct

        '''double
        Public T0 As Double

        '''double
        Public p0 As Double

        '''double
        Public u0 As Double

        '''double
        Public s0 As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure ReferenceStateTPHS_struct

        '''double
        Public T0 As Double

        '''double
        Public p0 As Double

        '''double
        Public h0 As Double

        '''double
        Public s0 As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Explicit)> <System.Serializable()> _
    Public Structure Anonymous_db77d62b_3996_4fac_9838_1630d266571b

        '''ReferenceStatePhi0->ReferenceStatePhi0_struct
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public phi0 As ReferenceStatePhi0_struct

        '''ReferenceStateTRHS->ReferenceStateTRHS_struct
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public trhs As ReferenceStateTRHS_struct

        '''ReferenceStateTPUS->ReferenceStateTPUS_struct
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public tpus As ReferenceStateTPUS_struct

        '''ReferenceStateTPHS->ReferenceStateTPHS_struct
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public tphs As ReferenceStateTPHS_struct
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure ReferenceState_struct

        '''ReferenceStateType->Anonymous_121353fe_95a5_4a55_b02d_81271fa54990
        Public type As ReferenceStateType

        '''Anonymous_db77d62b_3996_4fac_9838_1630d266571b
        Public data As Anonymous_db77d62b_3996_4fac_9838_1630d266571b
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Cp0PowTerm_struct

        '''double
        Public c As Double

        '''double
        Public t As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Cp0ExpTerm_struct

        '''double
        Public b As Double

        '''double
        Public beta As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Cp0Data_struct

        '''double
        Public cp0star As Double

        '''double
        Public Tstar As Double

        '''unsigned int
        Public np As UInteger

        '''Cp0PowTerm*
        Public pt As System.IntPtr

        '''unsigned int
        Public ne As UInteger

        '''Cp0ExpTerm*
        Public et As System.IntPtr
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Phi0PowTerm_struct

        '''double
        Public a0 As Double

        '''double
        Public p0 As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Phi0ExpTerm_struct

        '''double
        Public n As Double

        '''double
        Public gamma As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Phi0Data_struct

        '''double
        Public Tstar As Double

        '''unsigned int
        Public np As UInteger

        '''Phi0PowTerm*
        Public pt As System.IntPtr

        '''unsigned int
        Public ne As UInteger

        '''Phi0ExpTerm*
        Public et As System.IntPtr
    End Structure

    Public Enum IdealType_enum

        IDEAL_CP0

        IDEAL_PHI0
    End Enum

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Explicit)> <System.Serializable()> _
    Public Structure Anonymous_266c7d74_8938_41a9_a371_fbedb79d8e49

        '''Cp0Data->Cp0Data_struct
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public cp0 As Cp0Data_struct

        '''Phi0Data->Phi0Data_struct
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public phi0 As Phi0Data_struct
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure IdealData_struct

        '''IdealType->IdealType_enum
        Public type As IdealType_enum

        '''Anonymous_266c7d74_8938_41a9_a371_fbedb79d8e49
        Public data As Anonymous_266c7d74_8938_41a9_a371_fbedb79d8e49
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure IdealFluid_struct

        '''double
        Public M As Double

        '''double
        Public T0 As Double

        '''double
        Public p0 As Double

        '''double
        Public h0 As Double

        '''double
        Public s0 As Double

        '''IdealData->IdealData_struct
        Public data As IdealData_struct
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure HelmholtzPowTerm_struct

        '''double
        Public a As Double

        '''double
        Public t As Double

        '''int
        Public d As Integer

        '''unsigned int
        Public l As UInteger
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure HelmholtzGausTerm_struct

        '''double
        Public n As Double

        '''double
        Public t As Double

        '''double
        Public d As Double

        '''double
        Public alpha As Double

        '''double
        Public beta As Double

        '''double
        Public gamma As Double

        '''double
        Public epsilon As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure HelmholtzCritTerm_struct

        '''double
        Public n As Double

        '''double
        Public a As Double

        '''double
        Public b As Double

        '''double
        Public beta As Double

        '''double
        Public AA As Double

        '''double
        Public BB As Double

        '''double
        Public C As Double

        '''double
        Public D As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure HelmholtzData_struct

        '''double
        Public R As Double

        '''double
        Public M As Double

        '''double
        Public rho_star As Double

        '''double
        Public T_star As Double

        '''double
        Public T_c As Double

        '''double
        Public rho_c As Double

        '''double
        Public T_t As Double

        '''ReferenceState->ReferenceState_struct
        Public ref As ReferenceState_struct

        '''double
        Public omega As Double

        '''IdealData*
        Public ideal As System.IntPtr

        '''unsigned int
        Public np As UInteger

        '''HelmholtzPowTerm*
        Public pt As System.IntPtr

        '''unsigned int
        Public ng As UInteger

        '''HelmholtzGausTerm*
        Public gt As System.IntPtr

        '''unsigned int
        Public nc As UInteger

        '''HelmholtzCritTerm*
        Public ct As System.IntPtr
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure CubicData_struct

        '''double
        Public M As Double

        '''double
        Public T_c As Double

        '''double
        Public p_c As Double

        '''double
        Public rho_c As Double

        '''double
        Public T_t As Double

        '''double
        Public omega As Double

        '''ReferenceState->ReferenceState_struct
        Public ref As ReferenceState_struct

        '''IdealData*
        Public ideal As System.IntPtr
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure MbwrData_struct

        '''double
        Public R As Double

        '''double
        Public rhob_c As Double

        '''double[32]
        <System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.ByValArray, SizeConst:=32, ArraySubType:=System.Runtime.InteropServices.UnmanagedType.R8)> _
        Public beta() As Double
    End Structure

    Public Enum EosType_enum

        '''FPROPS_IDEAL -> 0
        FPROPS_IDEAL = 0

        '''FPROPS_CUBIC -> 1
        FPROPS_CUBIC = 1

        '''FPROPS_PENGROB -> 2
        FPROPS_PENGROB = 2

        '''FPROPS_REDKW -> 3
        FPROPS_REDKW = 3

        '''FPROPS_SOAVE -> 4
        FPROPS_SOAVE = 4

        '''FPROPS_HELMHOLTZ -> 5
        FPROPS_HELMHOLTZ = 5

        '''FPROPS_MBWR -> 6
        FPROPS_MBWR = 6
    End Enum

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Explicit)> <System.Serializable()> _
    Public Structure EosUnion_union

        '''HelmholtzData*
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public helm As System.IntPtr

        '''CubicData*
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public cubic As System.IntPtr

        '''MbwrData*
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public mbwr As System.IntPtr
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure EosData_struct

        '''char*
        <System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> _
        Public name As String

        '''char*
        <System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> _
        Public source As String

        '''char*
        <System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> _
        Public sourceurl As String

        '''double
        Public quality As Double

        '''EosType->EosType_enum
        Public type As EosType_enum

        '''EosUnion->EosUnion_union
        Public data As EosUnion_union
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Cp0RunPowTerm_struct

        '''double
        Public a As Double

        '''double
        Public p As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Cp0RunExpTerm_struct

        '''double
        Public n As Double

        '''double
        Public gamma As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure Phi0RunData_struct

        '''double
        Public c As Double

        '''double
        Public m As Double

        '''unsigned int
        Public np As UInteger

        '''Phi0RunPowTerm*
        Public pt As System.IntPtr

        '''unsigned int
        Public ne As UInteger

        '''Phi0RunExpTerm*
        Public et As System.IntPtr
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure HelmholtzRunData_struct

        '''double
        Public rho_star As Double

        '''double
        Public T_star As Double

        '''unsigned int
        Public np As UInteger

        '''HelmholtzPowTerm*
        Public pt As System.IntPtr

        '''unsigned int
        Public ng As UInteger

        '''HelmholtzGausTerm*
        Public gt As System.IntPtr

        '''unsigned int
        Public nc As UInteger

        '''HelmholtzCritTerm*
        Public ct As System.IntPtr
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure PengrobRunData_struct

        '''double
        Public aTc As Double

        '''double
        Public b As Double

        '''double
        Public kappa As Double
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Explicit)> <System.Serializable()> _
    Public Structure CorrelationUnion_union

        '''HelmholtzRunData*
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public helm As System.IntPtr

        '''PengrobRunData*
        <System.Runtime.InteropServices.FieldOffsetAttribute(0)> _
        Public pengrob As System.IntPtr
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure FluidData_struct

        '''double
        Public R As Double

        '''double
        Public M As Double

        '''double
        Public T_t As Double

        '''double
        Public T_c As Double

        '''double
        Public p_c As Double

        '''double
        Public rho_c As Double

        '''double
        Public omega As Double

        '''Phi0RunData*
        Public cp0 As System.IntPtr

        '''CorrelationUnion->CorrelationUnion_union
        Public corr As CorrelationUnion_union
    End Structure

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure StateData

        '''double
        Public T As Double

        '''double
        Public rho As Double

        '''double
        Public psat As Double

        '''double
        Public rhof As Double

        '''double
        Public rhog As Double

        '''double
        Public dpdT_sat As Double

        '''PureFluid*
        Public fluid As System.IntPtr
    End Structure

    '''Return Type: double
    '''param0: double
    '''param1: double
    '''data: FluidData*
    '''err: FpropsError*
    Public Delegate Function PropEvalFn(ByVal param0 As Double, ByVal param1 As Double, ByRef data As FluidData_struct, ByRef err As FpropsError_enum) As Double

    <System.Runtime.InteropServices.StructLayoutAttribute(System.Runtime.InteropServices.LayoutKind.Sequential)> <System.Serializable()> _
    Public Structure PureFluid_struct

        '''char*
        <System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> _
        Public name As String

        '''EosType->EosType_enum
        Public type As EosType_enum

        '''FluidData*
        Public data As System.IntPtr

        '''PropEvalFn*
        Public p_fn As System.IntPtr

        '''PropEvalFn*
        Public u_fn As System.IntPtr

        '''PropEvalFn*
        Public h_fn As System.IntPtr

        '''PropEvalFn*
        Public s_fn As System.IntPtr

        '''PropEvalFn*
        Public a_fn As System.IntPtr

        '''PropEvalFn*
        Public cv_fn As System.IntPtr

        '''PropEvalFn*
        Public cp_fn As System.IntPtr

        '''PropEvalFn*
        Public w_fn As System.IntPtr

        '''PropEvalFn*
        Public g_fn As System.IntPtr

        '''PropEvalFn*
        Public alphap_fn As System.IntPtr

        '''PropEvalFn*
        Public betap_fn As System.IntPtr

        '''PropEvalFn*
        Public dpdrho_T_fn As System.IntPtr
    End Structure

    <System.Serializable()> Partial Public Class FPROPS

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_p")> _
        Public Shared Function fprops_p(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_u")> _
        Public Shared Function fprops_u(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_h")> _
        Public Shared Function fprops_h(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_s")> _
        Public Shared Function fprops_s(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_a")> _
        Public Shared Function fprops_a(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_cv")> _
        Public Shared Function fprops_cv(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_cp")> _
        Public Shared Function fprops_cp(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_w")> _
        Public Shared Function fprops_w(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_g")> _
        Public Shared Function fprops_g(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_alphap")> _
        Public Shared Function fprops_alphap(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_betap")> _
        Public Shared Function fprops_betap(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_cp0")> _
        Public Shared Function fprops_cp0(ByVal T As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: double
        '''T: double
        '''rho: double
        '''data: PureFluid*
        '''err: FpropsError*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_dpdT_rho")> _
        Public Shared Function fprops_dpdT_rho(ByVal T As Double, ByVal rho As Double, ByRef data As PureFluid_struct, ByRef err As FpropsError_enum) As Double
        End Function

        '''Return Type: PureFluid*
        '''E: EosData*
        '''corrtype: char*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_prepare")> _
        Public Shared Function fprops_prepare(ByRef E As EosData_struct, <System.Runtime.InteropServices.InAttribute(), System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> ByVal corrtype As String) As System.IntPtr
        End Function

        '''Return Type: int
        '''E: EosData*
        '''corrtype: char*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_corr_avail")> _
        Public Shared Function fprops_corr_avail(ByRef E As EosData_struct, <System.Runtime.InteropServices.InAttribute(), System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> ByVal corrtype As String) As Integer
        End Function

        '''Return Type: char*
        '''err: FpropsError->FpropsError_enum
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_error")> _
        Public Shared Function fprops_error(ByVal err As FpropsError_enum) As System.IntPtr
        End Function

        '''Return Type: PureFluid*
        '''name: char*
        '''corrtype: char*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_fluid")> _
        Public Shared Function fprops_fluid(<System.Runtime.InteropServices.InAttribute(), System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> ByVal name As String, <System.Runtime.InteropServices.InAttribute(), System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.LPStr)> ByVal corrtype As String) As System.IntPtr
        End Function

        '''Return Type: int
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_num_fluids")> _
        Public Shared Function fprops_num_fluids() As Integer
        End Function

        '''Return Type: PureFluid*
        '''i: int
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_get_fluid")> _
        Public Shared Function fprops_get_fluid(ByVal i As Integer) As System.IntPtr
        End Function

        '''Return Type: double
        '''z: int
        '''x: int
        '''y: int
        '''T: double
        '''rho: double
        '''fluid: PureFluid*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_deriv")> _
        Public Shared Function fprops_deriv(ByVal z As Integer, ByVal x As Integer, ByVal y As Integer, ByVal T As Double, ByVal rho As Double, ByRef fluid As PureFluid_struct) As Double
        End Function

        '''Return Type: double
        '''x: int
        '''T: double
        '''rho: double
        '''fluid: PureFluid*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_non_dZdv_T")> _
        Public Shared Function fprops_non_dZdv_T(ByVal x As Integer, ByVal T As Double, ByVal rho As Double, ByRef fluid As PureFluid_struct) As Double
        End Function

        '''Return Type: double
        '''x: int
        '''T: double
        '''rho: double
        '''fluid: PureFluid*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_non_dZdT_v")> _
        Public Shared Function fprops_non_dZdT_v(ByVal x As Integer, ByVal T As Double, ByVal rho As Double, ByRef fluid As PureFluid_struct) As Double
        End Function

        '''Return Type: double
        '''z: int
        '''S: StateData*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_sat_dZdT_v")> _
        Public Shared Function fprops_sat_dZdT_v(ByVal z As Integer, ByRef S As StateData) As Double
        End Function

        '''Return Type: double
        '''z: int
        '''S: StateData*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_sat_dZdv_T")> _
        Public Shared Function fprops_sat_dZdv_T(ByVal z As Integer, ByRef S As StateData) As Double
        End Function

        '''Return Type: double
        '''S: StateData*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_drhofdT")> _
        Public Shared Function fprops_drhofdT(ByRef S As StateData) As Double
        End Function

        '''Return Type: double
        '''S: StateData*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_drhogdT")> _
        Public Shared Function fprops_drhogdT(ByRef S As StateData) As Double
        End Function

        '''Return Type: int
        '''T: double
        '''p_sat: double*
        '''rho_f: double*
        '''rho_g: double*
        '''d: PureFluid*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_sat_T")> _
        Public Shared Function fprops_sat_T(ByVal T As Double, ByRef p_sat As Double, ByRef rho_f As Double, ByRef rho_g As Double, ByRef d As PureFluid_struct) As Integer
        End Function

        '''Return Type: int
        '''p: double
        '''T_sat: double*
        '''rho_f: double*
        '''rho_g: double*
        '''d: PureFluid*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_sat_p")> _
        Public Shared Function fprops_sat_p(ByVal p As Double, ByRef T_sat As Double, ByRef rho_f As Double, ByRef rho_g As Double, ByRef d As PureFluid_struct) As Integer
        End Function

        '''Return Type: int
        '''p: double
        '''h: double
        '''T: double*
        '''rho: double*
        '''use_guess: int
        '''fluid: PureFluid*
        <System.Runtime.InteropServices.DllImportAttribute("fprops_ascend", EntryPoint:="fprops_solve_ph")> _
        Public Shared Function fprops_solve_ph(ByVal p As Double, ByVal h As Double, ByRef T As Double, ByRef rho As Double, ByVal use_guess As Integer, ByRef fluid As PureFluid_struct) As Integer
        End Function

    End Class

End Namespace

