Option Strict Off
Option Explicit On

Public Class lpsolve55

    'lpsolve version 5 routines

    Private Declare Function SetEnvironmentVariableA Lib "kernel32" (ByVal lpname As String, ByVal lpValue As String) As Integer
    Private Declare Function GetEnvironmentVariableA Lib "kernel32" (ByVal lpname As String, ByVal lpBuffer As String, ByVal nSize As Integer) As Integer

    '-----------------------------------------------------------------------------------------------------------------------------

    Public Declare Function add_column Lib "lpsolve55.dll" Alias "add_column" (ByVal lp As Integer, ByVal column() As Double) As Boolean
    Public Declare Function add_columnex Lib "lpsolve55.dll" Alias "add_columnex" (ByVal lp As Integer, ByVal count As Integer, ByVal column() As Double, ByVal rowno() As Integer) As Boolean
    Public Declare Function add_constraint Lib "lpsolve55.dll" Alias "add_constraint" (ByVal lp As Integer, ByVal row() As Double, ByVal constr_type As lpsolve_constr_types, ByVal rh As Double) As Boolean
    Public Declare Function add_constraintex Lib "lpsolve55.dll" Alias "add_constraintex" (ByVal lp As Integer, ByVal count As Integer, ByVal row() As Double, ByVal colno() As Integer, ByVal constr_type As lpsolve_constr_types, ByVal rh As Double) As Boolean
    Public Declare Function add_lag_con Lib "lpsolve55.dll" Alias "add_lag_con" (ByVal lp As Integer, ByVal row() As Double, ByVal con_type As lpsolve_constr_types, ByVal rhs As Double) As Boolean
    Public Declare Function add_SOS Lib "lpsolve55.dll" Alias "add_SOS" (ByVal lp As Integer, ByVal name As String, ByVal sostype As Integer, ByVal priority As Integer, ByVal count As Integer, ByVal sosvars() As Integer, ByVal weights() As Double) As Integer
    Public Declare Function column_in_lp Lib "lpsolve55.dll" Alias "column_in_lp" (ByVal lp As Integer, ByVal column() As Double) As Integer
    Public Declare Function copy_lp Lib "lpsolve55.dll" Alias "copy_lp" (ByVal lp As Integer) As Integer
    Public Declare Sub default_basis Lib "lpsolve55.dll" Alias "default_basis" (ByVal lp As Integer)
    Public Declare Function del_column Lib "lpsolve55.dll" Alias "del_column" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function del_constraint Lib "lpsolve55.dll" Alias "del_constraint" (ByVal lp As Integer, ByVal del_row As Integer) As Boolean
    Public Declare Sub delete_lp Lib "lpsolve55.dll" Alias "delete_lp" (ByVal lp As Integer)
    Public Declare Function dualize_lp Lib "lpsolve55.dll" Alias "dualize_lp" (ByVal lp As Integer) As Boolean
    Public Declare Function get_anti_degen Lib "lpsolve55.dll" Alias "get_anti_degen" (ByVal lp As Integer) As lpsolve_anti_degen
    Public Declare Function get_basis Lib "lpsolve55.dll" Alias "get_basis" (ByVal lp As Integer, ByVal bascolumn() As Integer, ByVal nonbasic As Boolean) As Boolean
    Public Declare Function get_basiscrash Lib "lpsolve55.dll" Alias "get_basiscrash" (ByVal lp As Integer) As lpsolve_basiscrash
    Public Declare Function get_bb_depthlimit Lib "lpsolve55.dll" Alias "get_bb_depthlimit" (ByVal lp As Integer) As Integer
    Public Declare Function get_bb_floorfirst Lib "lpsolve55.dll" Alias "get_bb_floorfirst" (ByVal lp As Integer) As lpsolve_branch
    Public Declare Function get_bb_rule Lib "lpsolve55.dll" Alias "get_bb_rule" (ByVal lp As Integer) As lpsolve_BBstrategies
    Public Declare Function get_bounds_tighter Lib "lpsolve55.dll" Alias "get_bounds_tighter" (ByVal lp As Integer) As Boolean
    Public Declare Function get_break_at_value Lib "lpsolve55.dll" Alias "get_break_at_value" (ByVal lp As Integer) As Double
    Public Declare Function get_col_name Lib "lpsolve55.dll" Alias "get_col_name" (ByVal lp As Integer, ByVal column As Integer) As String
    Public Declare Function get_column Lib "lpsolve55.dll" Alias "get_column" (ByVal lp As Integer, ByVal col_nr As Integer, ByVal column() As Double) As Boolean
    Public Declare Function get_columnex Lib "lpsolve55.dll" Alias "get_columnex" (ByVal lp As Integer, ByVal col_nr As Integer, ByVal column() As Double, ByVal nzrow() As Integer) As Integer
    Public Declare Function get_constr_type Lib "lpsolve55.dll" Alias "get_constr_type" (ByVal lp As Integer, ByVal row As Integer) As lpsolve_constr_types
    Public Declare Function get_constr_value Lib "lpsolve55.dll" Alias "get_constr_value" (ByVal lp As Integer, ByVal row As Integer, ByVal count As Integer, ByVal primsolution() As Double, ByVal nzindex() As Integer) As Double
    Public Declare Function get_constraints Lib "lpsolve55.dll" Alias "get_constraints" (ByVal lp As Integer, ByVal constr() As Double) As Boolean
    Public Declare Function get_dual_solution Lib "lpsolve55.dll" Alias "get_dual_solution" (ByVal lp As Integer, ByVal rc() As Double) As Boolean
    Public Declare Function get_epsb Lib "lpsolve55.dll" Alias "get_epsb" (ByVal lp As Integer) As Double
    Public Declare Function get_epsd Lib "lpsolve55.dll" Alias "get_epsd" (ByVal lp As Integer) As Double
    Public Declare Function get_epsel Lib "lpsolve55.dll" Alias "get_epsel" (ByVal lp As Integer) As Double
    Public Declare Function get_epsint Lib "lpsolve55.dll" Alias "get_epsint" (ByVal lp As Integer) As Double
    Public Declare Function get_epsperturb Lib "lpsolve55.dll" Alias "get_epsperturb" (ByVal lp As Integer) As Double
    Public Declare Function get_epspivot Lib "lpsolve55.dll" Alias "get_epspivot" (ByVal lp As Integer) As Double
    Public Declare Function get_improve Lib "lpsolve55.dll" Alias "get_improve" (ByVal lp As Integer) As lpsolve_improves
    Public Declare Function get_infinite Lib "lpsolve55.dll" Alias "get_infinite" (ByVal lp As Integer) As Double
    Public Declare Function get_lambda Lib "lpsolve55.dll" Alias "get_lambda" (ByVal lp As Integer, ByVal lambda() As Double) As Boolean
    Public Declare Function get_lowbo Lib "lpsolve55.dll" Alias "get_lowbo" (ByVal lp As Integer, ByVal column As Integer) As Double
    Public Declare Function get_lp_index Lib "lpsolve55.dll" Alias "get_lp_index" (ByVal lp As Integer, ByVal orig_index As Integer) As Integer
    Public Declare Function get_lp_name Lib "lpsolve55.dll" Alias "get_lp_name" (ByVal lp As Integer) As String
    Public Declare Function get_Lrows Lib "lpsolve55.dll" Alias "get_Lrows" (ByVal lp As Integer) As Integer
    Public Declare Function get_mat Lib "lpsolve55.dll" Alias "get_mat" (ByVal lp As Integer, ByVal row As Integer, ByVal column As Integer) As Double
    Public Declare Function get_max_level Lib "lpsolve55.dll" Alias "get_max_level" (ByVal lp As Integer) As Integer
    Public Declare Function get_maxpivot Lib "lpsolve55.dll" Alias "get_maxpivot" (ByVal lp As Integer) As Integer
    Public Declare Function get_mip_gap Lib "lpsolve55.dll" Alias "get_mip_gap" (ByVal lp As Integer, ByVal absolute As Boolean) As Double
    Public Declare Function get_Ncolumns Lib "lpsolve55.dll" Alias "get_Ncolumns" (ByVal lp As Integer) As Integer
    Public Declare Function get_negrange Lib "lpsolve55.dll" Alias "get_negrange" (ByVal lp As Integer) As Double
    Public Declare Function get_nameindex Lib "lpsolve55.dll" Alias "get_nameindex" (ByVal lp As Integer, ByVal name As String, ByVal isrow As Boolean) As Integer
    Public Declare Function get_nonzeros Lib "lpsolve55.dll" Alias "get_nonzeros" (ByVal lp As Integer) As Integer
    Public Declare Function get_Norig_columns Lib "lpsolve55.dll" Alias "get_Norig_columns" (ByVal lp As Integer) As Integer
    Public Declare Function get_Norig_rows Lib "lpsolve55.dll" Alias "get_Norig_rows" (ByVal lp As Integer) As Integer
    Public Declare Function get_Nrows Lib "lpsolve55.dll" Alias "get_Nrows" (ByVal lp As Integer) As Integer
    Public Declare Function get_obj_bound Lib "lpsolve55.dll" Alias "get_obj_bound" (ByVal lp As Integer) As Double
    Public Declare Function get_objective Lib "lpsolve55.dll" Alias "get_objective" (ByVal lp As Integer) As Double
    Public Declare Function get_orig_index Lib "lpsolve55.dll" Alias "get_orig_index" (ByVal lp As Integer, ByVal lp_index As Integer) As Integer
    Public Declare Function get_origcol_name Lib "lpsolve55.dll" Alias "get_origcol_name" (ByVal lp As Integer, ByVal column As Integer) As String
    Public Declare Function get_origrow_name Lib "lpsolve55.dll" Alias "get_origrow_name" (ByVal lp As Integer, ByVal row As Integer) As String
    Public Declare Function get_pivoting Lib "lpsolve55.dll" Alias "get_pivoting" (ByVal lp As Integer) As lpsolve_piv_rules
    Public Declare Function get_presolve Lib "lpsolve55.dll" Alias "get_presolve" (ByVal lp As Integer) As lpsolve_presolve
    Public Declare Function get_presolveloops Lib "lpsolve55.dll" Alias "get_presolveloops" (ByVal lp As Integer) As Integer
    Public Declare Function get_primal_solution Lib "lpsolve55.dll" Alias "get_primal_solution" (ByVal lp As Integer, ByVal pv_Renamed() As Double) As Boolean
    Public Declare Function get_print_sol Lib "lpsolve55.dll" Alias "get_print_sol" (ByVal lp As Integer) As Integer
    Public Declare Function get_PseudoCosts Lib "lpsolve55.dll" Alias "get_PseudoCosts" (ByVal lp As Integer, ByVal clower() As Double, ByVal cupper() As Double, ByVal updatelimit() As Integer) As Boolean
    Public Declare Function get_rh Lib "lpsolve55.dll" Alias "get_rh" (ByVal lp As Integer, ByVal row As Integer) As Double
    Public Declare Function get_rh_range Lib "lpsolve55.dll" Alias "get_rh_range" (ByVal lp As Integer, ByVal row As Integer) As Double
    Public Declare Function get_row Lib "lpsolve55.dll" Alias "get_row" (ByVal lp As Integer, ByVal row_nr As Integer, ByVal row() As Double) As Boolean
    Public Declare Function get_rowex Lib "lpsolve55.dll" Alias "get_rowex" (ByVal lp As Integer, ByVal row_nr As Integer, ByVal row() As Double, ByVal colno() As Integer) As Integer
    Public Declare Function get_row_name Lib "lpsolve55.dll" Alias "get_row_name" (ByVal lp As Integer, ByVal row As Integer) As String
    Public Declare Function get_scalelimit Lib "lpsolve55.dll" Alias "get_scalelimit" (ByVal lp As Integer) As Double
    Public Declare Function get_scaling Lib "lpsolve55.dll" Alias "get_scaling" (ByVal lp As Integer) As lpsolve_scales
    Public Declare Function get_sensitivity_obj Lib "lpsolve55.dll" Alias "get_sensitivity_obj" (ByVal lp As Integer, ByVal objfrom() As Double, ByVal objtill() As Double) As Boolean
    Public Declare Function get_sensitivity_objex Lib "lpsolve55.dll" Alias "get_sensitivity_objex" (ByVal lp As Integer, ByVal objfrom() As Double, ByVal objtill() As Double, ByVal objfromvalue() As Double, ByVal objtillvalue() As Double) As Boolean
    Public Declare Function get_sensitivity_rhs Lib "lpsolve55.dll" Alias "get_sensitivity_rhs" (ByVal lp As Integer, ByVal duals() As Double, ByVal dualsfrom() As Double, ByVal dualstill() As Double) As Boolean
    Public Declare Function get_simplextype Lib "lpsolve55.dll" Alias "get_simplextype" (ByVal lp As Integer) As lpsolve_simplextypes
    Public Declare Function get_solutioncount Lib "lpsolve55.dll" Alias "get_solutioncount" (ByVal lp As Integer) As Integer
    Public Declare Function get_solutionlimit Lib "lpsolve55.dll" Alias "get_solutionlimit" (ByVal lp As Integer) As Integer
    Public Declare Function get_status Lib "lpsolve55.dll" Alias "get_status" (ByVal lp As Integer) As Integer
    Public Declare Function get_statustext Lib "lpsolve55.dll" Alias "get_statustext" (ByVal lp As Integer, ByVal statuscode As Integer) As String
    Public Declare Function get_timeout Lib "lpsolve55.dll" Alias "get_timeout" (ByVal lp As Integer) As Integer
    Public Declare Function get_total_iter Lib "lpsolve55.dll" Alias "get_total_iter" (ByVal lp As Integer) As Long
    Public Declare Function get_total_nodes Lib "lpsolve55.dll" Alias "get_total_nodes" (ByVal lp As Integer) As Long
    Public Declare Function get_upbo Lib "lpsolve55.dll" Alias "get_upbo" (ByVal lp As Integer, ByVal column As Integer) As Double
    Public Declare Function get_var_branch Lib "lpsolve55.dll" Alias "get_var_branch" (ByVal lp As Integer, ByVal column As Integer) As lpsolve_branch
    Public Declare Function get_var_dualresult Lib "lpsolve55.dll" Alias "get_var_dualresult" (ByVal lp As Integer, ByVal index As Integer) As Double
    Public Declare Function get_var_primalresult Lib "lpsolve55.dll" Alias "get_var_primalresult" (ByVal lp As Integer, ByVal index As Integer) As Double
    Public Declare Function get_var_priority Lib "lpsolve55.dll" Alias "get_var_priority" (ByVal lp As Integer, ByVal column As Integer) As Integer
    Public Declare Function get_variables Lib "lpsolve55.dll" Alias "get_variables" (ByVal lp As Integer, ByVal var() As Double) As Boolean
    Public Declare Function get_verbose Lib "lpsolve55.dll" Alias "get_verbose" (ByVal lp As Integer) As Integer
    Public Declare Function get_working_objective Lib "lpsolve55.dll" Alias "get_working_objective" (ByVal lp As Integer) As Double
    Public Declare Function guess_basis Lib "lpsolve55.dll" Alias "guess_basis" (ByVal lp As Integer, ByVal guessvector() As Double, ByVal basisvector() As Integer) As Boolean
    Public Declare Function has_BFP Lib "lpsolve55.dll" Alias "has_BFP" (ByVal lp As Integer) As Boolean
    Public Declare Function has_XLI Lib "lpsolve55.dll" Alias "has_XLI" (ByVal lp As Integer) As Boolean
    Public Declare Function is_add_rowmode Lib "lpsolve55.dll" Alias "is_add_rowmode" (ByVal lp As Integer) As Boolean
    Public Declare Function is_anti_degen Lib "lpsolve55.dll" Alias "is_anti_degen" (ByVal lp As Integer, ByVal testmask As lpsolve_anti_degen) As Boolean
    Public Declare Function is_binary Lib "lpsolve55.dll" Alias "is_binary" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function is_break_at_first Lib "lpsolve55.dll" Alias "is_break_at_first" (ByVal lp As Integer) As Boolean
    Public Declare Function is_constr_type Lib "lpsolve55.dll" Alias "is_constr_type" (ByVal lp As Integer, ByVal row As Integer, ByVal mask As Integer) As Boolean
    Public Declare Function is_debug Lib "lpsolve55.dll" Alias "is_debug" (ByVal lp As Integer) As Boolean
    Public Declare Function is_feasible Lib "lpsolve55.dll" Alias "is_feasible" (ByVal lp As Integer, ByVal values() As Double, ByVal threshold As Double) As Boolean
    Public Declare Function is_infinite Lib "lpsolve55.dll" Alias "is_infinite" (ByVal lp As Integer, ByVal value As Double) As Boolean
    Public Declare Function is_int Lib "lpsolve55.dll" Alias "is_int" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function is_integerscaling Lib "lpsolve55.dll" Alias "is_integerscaling" (ByVal lp As Integer) As Boolean
    Public Declare Function is_lag_trace Lib "lpsolve55.dll" Alias "is_lag_trace" (ByVal lp As Integer) As Boolean
    Public Declare Function is_maxim Lib "lpsolve55.dll" Alias "is_maxim" (ByVal lp As Integer) As Boolean
    Public Declare Function is_nativeBFP Lib "lpsolve55.dll" Alias "is_nativeBFP" (ByVal lp As Integer) As Boolean
    Public Declare Function is_nativeXLI Lib "lpsolve55.dll" Alias "is_nativeXLI" (ByVal lp As Integer) As Boolean
    Public Declare Function is_negative Lib "lpsolve55.dll" Alias "is_negative" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function is_piv_mode Lib "lpsolve55.dll" Alias "is_piv_mode" (ByVal lp As Integer, ByVal testmask As lpsolve_piv_rules) As Boolean
    Public Declare Function is_piv_rule Lib "lpsolve55.dll" Alias "is_piv_rule" (ByVal lp As Integer, ByVal rule As lpsolve_piv_rules) As Boolean
    Public Declare Function is_presolve Lib "lpsolve55.dll" Alias "is_presolve" (ByVal lp As Integer, ByVal testmask As lpsolve_presolve) As Boolean
    Public Declare Function is_scalemode Lib "lpsolve55.dll" Alias "is_scalemode" (ByVal lp As Integer, ByVal testmask As lpsolve_scales) As Boolean
    Public Declare Function is_scaletype Lib "lpsolve55.dll" Alias "is_scaletype" (ByVal lp As Integer, ByVal scaletype As lpsolve_scales) As Boolean
    Public Declare Function is_semicont Lib "lpsolve55.dll" Alias "is_semicont" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function is_SOS_var Lib "lpsolve55.dll" Alias "is_SOS_var" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function is_trace Lib "lpsolve55.dll" Alias "is_trace" (ByVal lp As Integer) As Boolean
    Public Declare Function is_unbounded Lib "lpsolve55.dll" Alias "is_unbounded" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function is_use_names Lib "lpsolve55.dll" Alias "is_use_names" (ByVal lp As Integer, ByVal isrow As Boolean) As Boolean
    Public Declare Sub version Lib "lpsolve55.dll" Alias "lp_solve_version" (ByRef majorversion As Integer, ByRef minorversion As Integer, ByRef release As Integer, ByRef build As Integer)
    Public Declare Function make_lp Lib "lpsolve55.dll" Alias "make_lp" (ByVal rows As Integer, ByVal columns As Integer) As Integer
    Public Declare Function resize_lp Lib "lpsolve55.dll" Alias "resize_lp" (ByVal lp As Integer, ByVal rows As Integer, ByVal columns As Integer) As Boolean
    Public Declare Sub print_constraints Lib "lpsolve55.dll" Alias "print_constraints" (ByVal lp As Integer, ByVal columns As Integer)
    Public Declare Function print_debugdump Lib "lpsolve55.dll" Alias "print_debugdump" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Sub print_duals Lib "lpsolve55.dll" Alias "print_duals" (ByVal lp As Integer)
    Public Declare Sub print_lp Lib "lpsolve55.dll" Alias "print_lp" (ByVal lp As Integer)
    Public Declare Sub print_objective Lib "lpsolve55.dll" Alias "print_objective" (ByVal lp As Integer)
    Public Declare Sub print_scales Lib "lpsolve55.dll" Alias "print_scales" (ByVal lp As Integer)
    Public Declare Sub print_solution Lib "lpsolve55.dll" Alias "print_solution" (ByVal lp As Integer, ByVal columns As Integer)
    Public Declare Sub print_str Lib "lpsolve55.dll" Alias "print_str" (ByVal lp As Integer, ByVal str_Renamed As String)
    Public Declare Sub print_tableau Lib "lpsolve55.dll" Alias "print_tableau" (ByVal lp As Integer)
    Public Delegate Function abortfunc(ByVal lp As Integer, ByVal userhandle As Integer) As Integer
    Public Declare Sub put_abortfunc Lib "lpsolve55.dll" Alias "put_abortfunc" (ByVal lp As Integer, ByVal newctrlc As abortfunc, ByVal ctrlchandle As Integer)
    Public Delegate Sub logfunc(ByVal lp As Integer, ByVal userhandle As Integer, ByVal buf As String)
    Public Declare Sub put_logfunc Lib "lpsolve55.dll" Alias "put_logfunc" (ByVal lp As Integer, ByVal newlog As logfunc, ByVal loghandle As Integer)
    Public Delegate Sub msgfunc(ByVal lp As Integer, ByVal userhandle As Integer, ByVal message As lpsolve_msgmask)
    Public Declare Sub put_msgfunc Lib "lpsolve55.dll" Alias "put_msgfunc" (ByVal lp As Integer, ByVal newmsg As msgfunc, ByVal msghandle As Integer, ByVal mask As lpsolve_msgmask)
    Public Declare Function read_basis Lib "lpsolve55.dll" Alias "read_basis" (ByVal lp As Integer, ByVal filename As String, ByVal info As String) As Boolean
    Public Declare Function read_freeMPS Lib "lpsolve55.dll" Alias "read_freeMPS" (ByVal filename As String, ByVal options As Integer) As Integer
    Public Declare Function read_LP Lib "lpsolve55.dll" Alias "read_LP" (ByVal filename As String, ByVal verbose As Integer, ByVal lp_name As String) As Integer
    Public Declare Function read_MPS Lib "lpsolve55.dll" Alias "read_MPS" (ByVal filename As String, ByVal options As Integer) As Integer
    Public Declare Function read_XLI Lib "lpsolve55.dll" Alias "read_XLI" (ByVal xliname As String, ByVal modelname As String, ByVal dataname As String, ByVal options As String, ByVal verbose As Integer) As Integer
    Public Declare Function read_params Lib "lpsolve55.dll" Alias "read_params" (ByVal lp As Integer, ByVal filename As String, ByVal options As String) As Boolean
    Public Declare Sub reset_basis Lib "lpsolve55.dll" Alias "reset_basis" (ByVal lp As Integer)
    Public Declare Sub reset_params Lib "lpsolve55.dll" Alias "reset_params" (ByVal lp As Integer)
    Public Declare Function set_add_rowmode Lib "lpsolve55.dll" Alias "set_add_rowmode" (ByVal lp As Integer, ByVal turnon As Boolean) As Boolean
    Public Declare Sub set_anti_degen Lib "lpsolve55.dll" Alias "set_anti_degen" (ByVal lp As Integer, ByVal anti_degen As lpsolve_anti_degen)
    Public Declare Function set_basis Lib "lpsolve55.dll" Alias "set_basis" (ByVal lp As Integer, ByVal bascolumn() As Integer, ByVal nonbasic As Boolean) As Boolean
    Public Declare Sub set_basiscrash Lib "lpsolve55.dll" Alias "set_basiscrash" (ByVal lp As Integer, ByVal mode As lpsolve_basiscrash)
    Public Declare Sub set_basisvar Lib "lpsolve55.dll" Alias "set_basisvar" (ByVal lp As Integer, ByVal basisPos As Integer, ByVal enteringCol As Integer)
    Public Declare Sub set_bb_depthlimit Lib "lpsolve55.dll" Alias "set_bb_depthlimit" (ByVal lp As Integer, ByVal bb_maxlevel As Integer)
    Public Declare Sub set_bb_floorfirst Lib "lpsolve55.dll" Alias "set_bb_floorfirst" (ByVal lp As Integer, ByVal bb_floorfirst As lpsolve_branch)
    Public Declare Sub set_bb_rule Lib "lpsolve55.dll" Alias "set_bb_rule" (ByVal lp As Integer, ByVal bb_rule As lpsolve_BBstrategies)
    Public Declare Function set_BFP Lib "lpsolve55.dll" Alias "set_BFP" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Function set_binary Lib "lpsolve55.dll" Alias "set_binary" (ByVal lp As Integer, ByVal column As Integer, ByVal must_be_bin As Boolean) As Boolean
    Public Declare Function set_bounds Lib "lpsolve55.dll" Alias "set_bounds" (ByVal lp As Integer, ByVal column As Integer, ByVal lower As Double, ByVal upper As Double) As Boolean
    Public Declare Sub set_bounds_tighter Lib "lpsolve55.dll" Alias "set_bounds_tighter" (ByVal lp As Integer, ByVal tighten As Boolean)
    Public Declare Sub set_break_at_first Lib "lpsolve55.dll" Alias "set_break_at_first" (ByVal lp As Integer, ByVal break_at_first As Boolean)
    Public Declare Sub set_break_at_value Lib "lpsolve55.dll" Alias "set_break_at_value" (ByVal lp As Integer, ByVal break_at_value As Double)
    Public Declare Function set_col_name Lib "lpsolve55.dll" Alias "set_col_name" (ByVal lp As Integer, ByVal column As Integer, ByVal new_name As String) As Boolean
    Public Declare Function set_column Lib "lpsolve55.dll" Alias "set_column" (ByVal lp As Integer, ByVal col_no As Integer, ByVal column() As Double) As Boolean
    Public Declare Function set_columnex Lib "lpsolve55.dll" Alias "set_columnex" (ByVal lp As Integer, ByVal col_no As Integer, ByVal count As Integer, ByVal column() As Double, ByVal rowno() As Integer) As Boolean
    Public Declare Function set_constr_type Lib "lpsolve55.dll" Alias "set_constr_type" (ByVal lp As Integer, ByVal row As Integer, ByVal con_type As lpsolve_constr_types) As Boolean
    Public Declare Sub set_debug Lib "lpsolve55.dll" Alias "set_debug" (ByVal lp As Integer, ByVal debug_ As Boolean)
    Public Declare Sub set_epsb Lib "lpsolve55.dll" Alias "set_epsb" (ByVal lp As Integer, ByVal epsb As Double)
    Public Declare Sub set_epsd Lib "lpsolve55.dll" Alias "set_epsd" (ByVal lp As Integer, ByVal epsd As Double)
    Public Declare Sub set_epsel Lib "lpsolve55.dll" Alias "set_epsel" (ByVal lp As Integer, ByVal epsel As Double)
    Public Declare Sub set_epsint Lib "lpsolve55.dll" Alias "set_epsint" (ByVal lp As Integer, ByVal epsint As Double)
    Public Declare Function set_epslevel Lib "lpsolve55.dll" Alias "set_epslevel" (ByVal lp As Integer, ByVal epslevel As Integer) As Boolean
    Public Declare Sub set_epsperturb Lib "lpsolve55.dll" Alias "set_epsperturb" (ByVal lp As Integer, ByVal epsperturb As Double)
    Public Declare Sub set_epspivot Lib "lpsolve55.dll" Alias "set_epspivot" (ByVal lp As Integer, ByVal epspivot As Double)
    Public Declare Sub set_improve Lib "lpsolve55.dll" Alias "set_improve" (ByVal lp As Integer, ByVal improve As lpsolve_improves)
    Public Declare Sub set_infinite Lib "lpsolve55.dll" Alias "set_infinite" (ByVal lp As Integer, ByVal infinite As Double)
    Public Declare Function set_int Lib "lpsolve55.dll" Alias "set_int" (ByVal lp As Integer, ByVal column As Integer, ByVal must_be_int As Boolean) As Boolean
    Public Declare Sub set_lag_trace Lib "lpsolve55.dll" Alias "set_lag_trace" (ByVal lp As Integer, ByVal lag_trace As Boolean)
    Public Declare Function set_lowbo Lib "lpsolve55.dll" Alias "set_lowbo" (ByVal lp As Integer, ByVal column As Integer, ByVal value As Double) As Boolean
    Public Declare Function set_lp_name Lib "lpsolve55.dll" Alias "set_lp_name" (ByVal lp As Integer, ByVal lpname As String) As Boolean
    Public Declare Function set_mat Lib "lpsolve55.dll" Alias "set_mat" (ByVal lp As Integer, ByVal row As Integer, ByVal column As Integer, ByVal value As Double) As Boolean
    Public Declare Sub set_maxim Lib "lpsolve55.dll" Alias "set_maxim" (ByVal lp As Integer)
    Public Declare Sub set_maxpivot Lib "lpsolve55.dll" Alias "set_maxpivot" (ByVal lp As Integer, ByVal max_num_inv As Integer)
    Public Declare Sub set_minim Lib "lpsolve55.dll" Alias "set_minim" (ByVal lp As Integer)
    Public Declare Sub set_mip_gap Lib "lpsolve55.dll" Alias "set_mip_gap" (ByVal lp As Integer, ByVal absolute As Boolean, ByVal mip_gap As Double)
    Public Declare Sub set_negrange Lib "lpsolve55.dll" Alias "set_negrange" (ByVal lp As Integer, ByVal negrange As Double)
    Public Declare Function set_obj Lib "lpsolve55.dll" Alias "set_obj" (ByVal lp As Integer, ByVal column As Integer, ByVal value As Double) As Boolean
    Public Declare Sub set_obj_bound Lib "lpsolve55.dll" Alias "set_obj_bound" (ByVal lp As Integer, ByVal obj_bound As Double)
    Public Declare Function set_obj_fn Lib "lpsolve55.dll" Alias "set_obj_fn" (ByVal lp As Integer, ByVal row() As Double) As Boolean
    Public Declare Function set_obj_fnex Lib "lpsolve55.dll" Alias "set_obj_fnex" (ByVal lp As Integer, ByVal count As Integer, ByVal row() As Double, ByVal colno() As Integer) As Boolean
    Public Declare Function set_outputfile Lib "lpsolve55.dll" Alias "set_outputfile" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Sub set_pivoting Lib "lpsolve55.dll" Alias "set_pivoting" (ByVal lp As Integer, ByVal piv_rule As lpsolve_piv_rules)
    Public Declare Sub set_preferdual Lib "lpsolve55.dll" Alias "set_preferdual" (ByVal lp As Integer, ByVal dodual As Boolean)
    Public Declare Sub set_presolve Lib "lpsolve55.dll" Alias "set_presolve" (ByVal lp As Integer, ByVal do_presolve As lpsolve_presolve, ByVal maxloops As Integer)
    Public Declare Sub set_print_sol Lib "lpsolve55.dll" Alias "set_print_sol" (ByVal lp As Integer, ByVal print_sol As Integer)
    Public Declare Function set_PseudoCosts Lib "lpsolve55.dll" Alias "set_PseudoCosts" (ByVal lp As Integer, ByVal clower() As Double, ByVal cupper() As Double, ByVal updatelimit() As Integer) As Boolean
    Public Declare Function set_rh Lib "lpsolve55.dll" Alias "set_rh" (ByVal lp As Integer, ByVal row As Integer, ByVal value As Double) As Boolean
    Public Declare Function set_rh_range Lib "lpsolve55.dll" Alias "set_rh_range" (ByVal lp As Integer, ByVal row As Integer, ByVal deltavalue As Double) As Boolean
    Public Declare Sub set_rh_vec Lib "lpsolve55.dll" Alias "set_rh_vec" (ByVal lp As Integer, ByVal rh() As Double)
    Public Declare Function set_row Lib "lpsolve55.dll" Alias "set_row" (ByVal lp As Integer, ByVal row_no As Integer, ByVal row() As Double) As Boolean
    Public Declare Function set_row_name Lib "lpsolve55.dll" Alias "set_row_name" (ByVal lp As Integer, ByVal row As Integer, ByVal new_name As String) As Boolean
    Public Declare Function set_rowex Lib "lpsolve55.dll" Alias "set_rowex" (ByVal lp As Integer, ByVal row_no As Integer, ByVal count As Integer, ByVal row() As Double, ByVal colno() As Integer) As Boolean
    Public Declare Sub set_scalelimit Lib "lpsolve55.dll" Alias "set_scalelimit" (ByVal lp As Integer, ByVal scalelimit As Double)
    Public Declare Sub set_scaling Lib "lpsolve55.dll" Alias "set_scaling" (ByVal lp As Integer, ByVal scalemode As lpsolve_scales)
    Public Declare Function set_semicont Lib "lpsolve55.dll" Alias "set_semicont" (ByVal lp As Integer, ByVal column As Integer, ByVal must_be_sc As Boolean) As Boolean
    Public Declare Sub set_sense Lib "lpsolve55.dll" Alias "set_sense" (ByVal lp As Integer, ByVal maximize As Boolean)
    Public Declare Sub set_simplextype Lib "lpsolve55.dll" Alias "set_simplextype" (ByVal lp As Integer, ByVal simplextype As lpsolve_simplextypes)
    Public Declare Sub set_solutionlimit Lib "lpsolve55.dll" Alias "set_solutionlimit" (ByVal lp As Integer, ByVal limit As Integer)
    Public Declare Sub set_timeout Lib "lpsolve55.dll" Alias "set_timeout" (ByVal lp As Integer, ByVal sectimeout As Integer)
    Public Declare Sub set_trace Lib "lpsolve55.dll" Alias "set_trace" (ByVal lp As Integer, ByVal trace As Boolean)
    Public Declare Function set_unbounded Lib "lpsolve55.dll" Alias "set_unbounded" (ByVal lp As Integer, ByVal column As Integer) As Boolean
    Public Declare Function set_upbo Lib "lpsolve55.dll" Alias "set_upbo" (ByVal lp As Integer, ByVal column As Integer, ByVal value As Double) As Boolean
    Public Declare Sub set_use_names Lib "lpsolve55.dll" Alias "set_use_names" (ByVal lp As Integer, ByVal isrow As Boolean, ByVal use_names As Boolean)
    Public Declare Function set_var_branch Lib "lpsolve55.dll" Alias "set_var_branch" (ByVal lp As Integer, ByVal column As Integer, ByVal branch_mode As lpsolve_branch) As Boolean
    Public Declare Function set_var_weights Lib "lpsolve55.dll" Alias "set_var_weights" (ByVal lp As Integer, ByVal weights() As Double) As Boolean
    Public Declare Sub set_verbose Lib "lpsolve55.dll" Alias "set_verbose" (ByVal lp As Integer, ByVal verbose As Integer)
    Public Declare Function set_XLI Lib "lpsolve55.dll" Alias "set_XLI" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Function solve Lib "lpsolve55.dll" Alias "solve" (ByVal lp As Integer) As lpsolve_return
    Public Declare Function str_add_column Lib "lpsolve55.dll" Alias "str_add_column" (ByVal lp As Integer, ByVal col_string As String) As Boolean
    Public Declare Function str_add_constraint Lib "lpsolve55.dll" Alias "str_add_constraint" (ByVal lp As Integer, ByVal row_string As String, ByVal constr_type As lpsolve_constr_types, ByVal rh As Double) As Boolean
    Public Declare Function str_add_lag_con Lib "lpsolve55.dll" Alias "str_add_lag_con" (ByVal lp As Integer, ByVal row_string As String, ByVal con_type As lpsolve_constr_types, ByVal rhs As Double) As Boolean
    Public Declare Function str_set_obj_fn Lib "lpsolve55.dll" Alias "str_set_obj_fn" (ByVal lp As Integer, ByVal row_string As String) As Boolean
    Public Declare Function str_set_rh_vec Lib "lpsolve55.dll" Alias "str_set_rh_vec" (ByVal lp As Integer, ByVal rh_string As String) As Boolean
    Public Declare Function time_elapsed Lib "lpsolve55.dll" Alias "time_elapsed" (ByVal lp As Integer) As Double
    Public Declare Sub unscale Lib "lpsolve55.dll" Alias "unscale" (ByVal lp As Integer)
    Public Declare Function write_basis Lib "lpsolve55.dll" Alias "write_basis" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Function write_freemps Lib "lpsolve55.dll" Alias "write_freemps" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Function write_lp Lib "lpsolve55.dll" Alias "write_lp" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Function write_mps Lib "lpsolve55.dll" Alias "write_mps" (ByVal lp As Integer, ByVal filename As String) As Boolean
    Public Declare Function write_XLI Lib "lpsolve55.dll" Alias "write_XLI" (ByVal lp As Integer, ByVal filename As String, ByVal options As String, ByVal results As Boolean) As Boolean
    Public Declare Function write_params Lib "lpsolve55.dll" Alias "write_params" (ByVal lp As Integer, ByVal filename As String, ByVal options As String) As Boolean

    '-----------------------------------------------------------------------------------------------------------------------------

    'possible type of constraints
    Public Enum lpsolve_constr_types
        LE = 1
        EQ = 3
        GE = 2
        FR = 0
    End Enum

    'Possible Scalings
    Public Enum lpsolve_scales
        SCALE_EXTREME = 1
        SCALE_RANGE = 2
        SCALE_MEAN = 3
        SCALE_GEOMETRIC = 4
        SCALE_CURTISREID = 7
        SCALE_QUADRATIC = 8
        SCALE_LOGARITHMIC = 16
        SCALE_USERWEIGHT = 31
        SCALE_POWER2 = 32
        SCALE_EQUILIBRATE = 64
        SCALE_INTEGERS = 128
    End Enum

    'Possible Improvements
    Public Enum lpsolve_improves
        IMPROVE_NONE = 0
        IMPROVE_SOLUTION = 1
        IMPROVE_DUALFEAS = 2
        IMPROVE_THETAGAP = 4
        IMPROVE_BBSIMPLEX = 8
        IMPROVE_DEFAULT = (IMPROVE_DUALFEAS + IMPROVE_THETAGAP)
        IMPROVE_INVERSE = (IMPROVE_SOLUTION + IMPROVE_THETAGAP)
    End Enum

    Public Enum lpsolve_piv_rules
        PRICER_FIRSTINDEX = 0
        PRICER_DANTZIG = 1
        PRICER_DEVEX = 2
        PRICER_STEEPESTEDGE = 3
        PRICE_PRIMALFALLBACK = 4
        PRICE_MULTIPLE = 8
        PRICE_PARTIAL = 16
        PRICE_ADAPTIVE = 32
        PRICE_HYBRID = 64
        PRICE_RANDOMIZE = 128
        PRICE_AUTOPARTIALCOLS = 256
        PRICE_AUTOPARTIALROWS = 512
        PRICE_LOOPLEFT = 1024
        PRICE_LOOPALTERNATE = 2048
        PRICE_AUTOPARTIAL = lpsolve_piv_rules.PRICE_AUTOPARTIALCOLS + lpsolve_piv_rules.PRICE_AUTOPARTIALROWS
    End Enum

    Public Enum lpsolve_presolve
        PRESOLVE_NONE = 0
        PRESOLVE_ROWS = 1
        PRESOLVE_COLS = 2
        PRESOLVE_LINDEP = 4
        PRESOLVE_SOS = 32
        PRESOLVE_REDUCEMIP = 64
        PRESOLVE_KNAPSACK = 128
        PRESOLVE_ELIMEQ2 = 256
        PRESOLVE_IMPLIEDFREE = 512
        PRESOLVE_REDUCEGCD = 1024
        PRESOLVE_PROBEFIX = 2048
        PRESOLVE_PROBEREDUCE = 4096
        PRESOLVE_ROWDOMINATE = 8192
        PRESOLVE_COLDOMINATE = 16384
        PRESOLVE_MERGEROWS = 32768
        PRESOLVE_IMPLIEDSLK = 65536
        PRESOLVE_COLFIXDUAL = 131072
        PRESOLVE_BOUNDS = 262144
        PRESOLVE_DUALS = 524288
        PRESOLVE_SENSDUALS = 1048576
    End Enum

    Public Enum lpsolve_anti_degen
        ANTIDEGEN_NONE = 0
        ANTIDEGEN_FIXEDVARS = 1
        ANTIDEGEN_COLUMNCHECK = 2
        ANTIDEGEN_STALLING = 4
        ANTIDEGEN_NUMFAILURE = 8
        ANTIDEGEN_LOSTFEAS = 16
        ANTIDEGEN_INFEASIBLE = 32
        ANTIDEGEN_DYNAMIC = 64
        ANTIDEGEN_DURINGBB = 128
        ANTIDEGEN_RHSPERTURB = 256
        ANTIDEGEN_BOUNDFLIP = 512
    End Enum

    Public Enum lpsolve_basiscrash
        CRASH_NOTHING = 0
        CRASH_MOSTFEASIBLE = 2
    End Enum

    Public Enum lpsolve_simplextypes
        SIMPLEX_PRIMAL_PRIMAL = 5
        SIMPLEX_DUAL_PRIMAL = 6
        SIMPLEX_PRIMAL_DUAL = 9
        SIMPLEX_DUAL_DUAL = 10
    End Enum

    'B&B strategies
    Public Enum lpsolve_BBstrategies
        NODE_FIRSTSELECT = 0
        NODE_GAPSELECT = 1
        NODE_RANGESELECT = 2
        NODE_FRACTIONSELECT = 3
        NODE_PSEUDOCOSTSELECT = 4
        NODE_PSEUDONONINTSELECT = 5
        NODE_PSEUDORATIOSELECT = 6
        NODE_USERSELECT = 7
        NODE_WEIGHTREVERSEMODE = 8
        NODE_BRANCHREVERSEMODE = 16
        NODE_GREEDYMODE = 32
        NODE_PSEUDOCOSTMODE = 64
        NODE_DEPTHFIRSTMODE = 128
        NODE_RANDOMIZEMODE = 256
        NODE_GUBMODE = 512
        NODE_DYNAMICMODE = 1024
        NODE_RESTARTMODE = 2048
        NODE_BREADTHFIRSTMODE = 4096
        NODE_AUTOORDER = 8192
        NODE_RCOSTFIXING = 16384
        NODE_STRONGINIT = 32768
    End Enum

    'possible return values of lp solver
    Public Enum lpsolve_return
        NOMEMORY = -2
        OPTIMAL = 0
        SUBOPTIMAL = 1
        INFEASIBLE = 2
        UNBOUNDED = 3
        DEGENERATE = 4
        NUMFAILURE = 5
        USERABORT = 6
        TIMEOUT = 7
        PRESOLVED = 9
        PROCFAIL = 10
        PROCBREAK = 11
        FEASFOUND = 12
        NOFEASFOUND = 13
    End Enum

    'possible branch values
    Public Enum lpsolve_branch
        BRANCH_CEILING = 0
        BRANCH_FLOOR = 1
        BRANCH_AUTOMATIC = 2
        BRANCH_DEFAULT = 3
    End Enum

    'possible message values
    Public Enum lpsolve_msgmask
        MSG_PRESOLVE = 1
        MSG_LPFEASIBLE = 8
        MSG_LPOPTIMAL = 16
        MSG_MILPEQUAL = 32
        MSG_MILPFEASIBLE = 128
        MSG_MILPBETTER = 512
    End Enum

    Private Shared Function SetEnvironmentVariable(ByRef name As String, ByRef value As String) As Boolean

        SetEnvironmentVariable = SetEnvironmentVariableA(name, value)

    End Function

    Private Shared Function GetEnvironmentVariable(ByRef name As String) As String
        Dim l As Integer
        Dim buf As String

        l = GetEnvironmentVariableA(name, vbNullString, 0)
        If l > 0 Then
            buf = Space(l)
            l = GetEnvironmentVariableA(name, buf, Len(buf))
            GetEnvironmentVariable = Mid(buf, 1, l)
        Else
            GetEnvironmentVariable = ""
        End If

    End Function

    Shared Function Init(Optional ByVal dllPath As String = "") As Boolean
        Static bEnvChanged As Boolean
        Dim Path As String
        Dim buf As String

        If Len(dllPath) = 0 Then
            dllPath = CurDir()
        End If
        buf = dllPath
        If Right(buf, 1) <> "\" Then
            buf = buf & "\"
        End If
        buf = buf & "lpsolve55.dll"
        On Error Resume Next
        Init = (Len(Dir(buf, FileAttribute.Normal)) > 0)
        If Init Then
            If Not bEnvChanged Then
                bEnvChanged = True
                Path = GetEnvironmentVariable("PATH")
                If InStr(1, Path & ";", dllPath & ";", CompareMethod.Text) = 0 Then
                    SetEnvironmentVariable("PATH", dllPath & ";" & Path)
                End If
            End If
        End If

    End Function

End Class
