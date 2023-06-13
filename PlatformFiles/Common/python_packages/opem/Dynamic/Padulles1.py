# -*- coding: utf-8 -*-
"""Padulles 1 model functions."""
import math
from opem.Params import Padulles_InputParams as InputParams
from opem.Params import Padulles_Outparams as OutputParams
from opem.Params import R, F, uF, HHV, Padulles_Description, Overall_Params_Max_Description, Overall_Params_Linear_Description, Report_Message
from opem.Static.Amphlett import Power_Calc, Power_Thermal_Calc, Power_Total_Calc, Linear_Aprox_Params_Calc, Max_Params_Calc
import opem.Functions
import os


def Enernst_Calc(E0, N0, T, PH2, PO2):
    """
    Calculate Enernst.

    :param E0: open cell voltage [V]
    :type E0 : float
    :param N0: number of fuel cells in the stack
    :type N0 : int
    :param T: cell operation temperature [K]
    :type T : float
    :param PH2:  partial pressure [atm]
    :type PH2 : float
    :param PO2: partial pressure [atm]
    :type PO2 : float
    :return: Enernest [V] as float
    """
    try:
        result = N0 * (E0 + (R * T / (2 * F)) * math.log(PH2 * ((PO2)**0.5)))
        return result
    except (TypeError, ZeroDivisionError, OverflowError, ValueError):
        print(
            "[Error] Enernst Calculation Failed (E0:%s, N0:%s, T:%s, PH2:%s, PO2:%s)" %
            (str(E0), str(N0), str(T), str(PH2), str(PO2)))


def PH2_Calc(KH2, tH2, Kr, I, qH2):
    """
    Calculate PH2.

    :param KH2: hydrogen valve constant [kmol.s^(-1).atm^(-1)]
    :type KH2 : float
    :param tH2: hydrogen time constant [s]
    :type tH2 : float
    :param Kr: modeling constant [kmol.s^(-1).A^(-1)]
    :type Kr : float
    :param I: cell load current [A]
    :type I : float
    :param qH2: molar flow of hydrogen [kmol.s^(-1)]
    :type qH2 : float
    :return: PH2 [atm] as float
    """
    try:
        result = ((1 / KH2) / (1 + tH2)) * (qH2 - 2 * Kr * I)
        return result
    except (TypeError, ZeroDivisionError):
        print(
            "[Error] PH2 Calculation Failed (KH2:%s, tH2:%s, Kr:%s, I:%s, qH2:%s)" %
            (str(KH2), str(tH2), str(Kr), str(I), str(qH2)))


def PO2_Calc(KO2, tO2, Kr, I, qO2):
    """
    Calculate PO2.

    :param KO2: oxygen valve constant [kmol.s^(-1).atm^(-1)]
    :type KO2 : float
    :param tO2: oxygen time constant [s]
    :type tO2 : float
    :param Kr: modeling constant [kmol.s^(-1).A^(-1)]
    :type Kr : float
    :param I: cell load current [A]
    :type I : float
    :param qO2: molar flow of oxygen [kmol.s^(-1)
    :type qO2 : float
    :return: PO2 [atm] as float
    """
    try:
        result = ((1 / KO2) / (1 + tO2)) * (qO2 - Kr * I)
        return result
    except (TypeError, ZeroDivisionError):
        print(
            "[Error] PO2 Calculation Failed (KO2:%s, tO2:%s, Kr:%s, I:%s, qO2:%s)" %
            (str(KO2), str(tO2), str(Kr), str(I), str(qO2)))


def Kr_Calc(N0):
    """
    Calculate Kr.

    :param N0: number of fuel cells in the stack
    :type N0 : int
    :return: Kr [kmol.s^(-1).A^(-1)] as float
    """
    try:
        result = N0 / (4 * F)
        return result
    except (TypeError, ZeroDivisionError):
        print("[Error] Kr Calculation Failed (N0:%s)" % str(N0))


def Vcell_Calc(Enernst, B, C, I, Rint):
    """
    Calculate Vcell.

    :param Enernst: Enernst [V]
    :type Enernst : float
    :param B: activation voltage constant [V]
    :type B: float
    :param C: constant [A^(-1)
    :type C : float
    :param I: cell load current [A]
    :type I: float
    :param Rint: fuel cell internal resistance [ohm]
    :type Rint : float
    :return: Vcell [V] as float
    """
    try:
        result = Enernst - B * math.log(C * I) - Rint * I
        return result
    except (TypeError, OverflowError, ValueError):
        print(
            "[Error] Vcell Calculation Error (Enernst:%s, B:%s, C:%s, I:%s, Rint:%s)" %
            (str(Enernst), str(B), str(C), str(I), str(Rint)))


def qO2_Calc(qH2, rho):
    """
    Calculate qO2.

    :param qH2: molar flow of hydrogen [kmol.s^(-1)]
    :type qH2 : float
    :param rho: hydrogen-oxygen flow rate
    :type rho : float
    :return: qO2 [kmol.s^(-1)] as float
    """
    try:
        result = (qH2 / rho)
        return result
    except (TypeError, ZeroDivisionError):
        print(
            "[Error] qO2 Calculation Error (qH2:%s, rho:%s)" %
            (str(qH2), str(rho)))


def Efficiency_Calc(Vcell, N):
    """
    Calculate PEM cell efficiency.

    :param Vcell: cell voltage [V]
    :type Vcell:float
    :param N0: number of fuel cells in the stack
    :type N0 : int
    :return: efficiency as float
    """
    try:
        result = (uF * Vcell) / (N * HHV)
        return result
    except (TypeError, ZeroDivisionError):
        print(
            "[Error] PEM Efficiency Calculation Failed (Vcell:%s, N:%s)" %
            (str(Vcell), str(N)))


def Dynamic_Analysis(
        InputMethod=opem.Functions.Get_Input,
        TestMode=False,
        PrintMode=True,
        ReportMode=True):
    """
    Run Padulles I analysis.

    :param InputMethod : input function or input test vector
    :param TestMode : test mode flag
    :type InputMethod : dict or Get_Input function object
    :type TestMode:bool
    :param PrintMode : print mode control flag (True : print outputs)
    :type PrintMode:bool
    :param ReportMode : report mode control flag (True : generate report)
    :type ReportMode: bool
    :return: result as dict
    """
    OutputFile = None
    CSVFile = None
    Warning1 = False
    Warning2 = False
    I_Warning = 0
    Overall_Params_Max = {}
    Overall_Params_Linear = {}
    Simulation_Title = "Padulles-I"
    try:

        if PrintMode:
            print("###########")
            print(Simulation_Title + "-Model Simulation")
            print("###########")
        OutputParamsKeys = sorted(OutputParams.keys())
        Output_Dict = dict(
            zip(OutputParamsKeys, [None] * len(OutputParamsKeys)))
        if not TestMode:
            Input_Dict = InputMethod(InputParams)
        else:
            Input_Dict = InputMethod
        if PrintMode:
            print("Analyzing . . .")
        Name = Input_Dict["Name"]
        if ReportMode:
            OutputFile = opem.Functions.Output_Init(
                Input_Dict, Simulation_Title, Name)
            CSVFile = opem.Functions.CSV_Init(
                OutputParamsKeys,
                OutputParams,
                Simulation_Title,
                Name)
            HTMLFile = opem.Functions.HTML_Init(Simulation_Title, Name)
        IEnd = Input_Dict["i-stop"]
        IStep = Input_Dict["i-step"]
        Precision = opem.Functions.get_precision(IStep)
        [i, IEnd, IStep] = opem.Functions.filter_range(
            Input_Dict["i-start"], IEnd, IStep)
        I_List = []
        Power_List = []
        Vstack_List = []
        Efficiency_List = []
        PH2_List = []
        PO2_List = []
        Power_Thermal_List = []
        Kr = Kr_Calc(Input_Dict["N0"])
        qO2 = qO2_Calc(Input_Dict["qH2"], Input_Dict["rho"])
        while i < IEnd:
            try:
                I_List.append(i)
                Output_Dict["PO2"] = PO2_Calc(
                    Input_Dict["KO2"], Input_Dict["tO2"], Kr, i, qO2)
                Output_Dict["PH2"] = PH2_Calc(
                    Input_Dict["KH2"], Input_Dict["tH2"], Kr, i, Input_Dict["qH2"])
                PH2_List.append(Output_Dict["PH2"])
                PO2_List.append(Output_Dict["PO2"])
                Output_Dict["E"] = Enernst_Calc(
                    Input_Dict["E0"],
                    Input_Dict["N0"],
                    Input_Dict["T"],
                    Output_Dict["PH2"],
                    Output_Dict["PO2"])
                Output_Dict["FC Voltage"] = Vcell_Calc(
                    Output_Dict["E"], Input_Dict["B"], Input_Dict["C"], i, Input_Dict["Rint"])
                [Warning1, I_Warning] = opem.Functions.warning_check_1(
                    Output_Dict["FC Voltage"], I_Warning, i, Warning1)
                Warning2 = opem.Functions.warning_check_2(
                    Vcell=Output_Dict["FC Voltage"],
                    warning_flag=Warning2)
                Vstack_List.append(Output_Dict["FC Voltage"])
                Output_Dict["FC Efficiency"] = Efficiency_Calc(
                    Output_Dict["FC Voltage"], Input_Dict["N0"])
                Efficiency_List.append(Output_Dict["FC Efficiency"])
                Output_Dict["FC Power"] = Power_Calc(
                    Output_Dict["FC Voltage"], i)
                Output_Dict["Power-Thermal"] = Power_Thermal_Calc(
                    VStack=Output_Dict["FC Voltage"], N=Input_Dict["N0"], i=i)
                Power_List.append(Output_Dict["FC Power"])
                Power_Thermal_List.append(Output_Dict["Power-Thermal"])
                if ReportMode:
                    opem.Functions.Output_Save(
                        OutputParamsKeys,
                        Output_Dict,
                        OutputParams,
                        i,
                        OutputFile,
                        PrintMode)
                    opem.Functions.CSV_Save(
                        OutputParamsKeys, Output_Dict, i, CSVFile)
                i = opem.Functions.rounder(i + IStep, Precision)
            except Exception as e:
                print(str(e))
                i = opem.Functions.rounder(i + IStep, Precision)
                if ReportMode:
                    opem.Functions.Output_Save(
                        OutputParamsKeys,
                        Output_Dict,
                        OutputParams,
                        i,
                        OutputFile,
                        PrintMode)
                    opem.Functions.CSV_Save(
                        OutputParamsKeys, Output_Dict, i, CSVFile)
        [Estimated_V, B0, B1] = opem.Functions.linear_plot(
            x=I_List, y=Vstack_List)
        Linear_Approx_Params = Linear_Aprox_Params_Calc(B0, B1)
        Max_Params = Max_Params_Calc(Power_List, Efficiency_List, Vstack_List)
        Power_Total = Power_Total_Calc(Vstack_List, IStep, Input_Dict["N0"])
        Overall_Params_Linear["Pmax(L-Approx)"] = Linear_Approx_Params[0]
        Overall_Params_Linear["V0"] = B0
        Overall_Params_Linear["K"] = B1
        Overall_Params_Linear["VFC|Pmax(L-Approx)"] = Linear_Approx_Params[1]

        Overall_Params_Max["Pmax"] = Max_Params["Max_Power"]
        Overall_Params_Max["VFC|Pmax"] = Max_Params["Max_VStack"]
        Overall_Params_Max["Efficiency|Pmax"] = Max_Params["Max_EFF"]
        Overall_Params_Max["Ptotal(Elec)"] = Power_Total[0]
        Overall_Params_Max["Ptotal(Thermal)"] = Power_Total[1]
        if ReportMode:
            OutputFile.close()
            CSVFile.close()
            if PrintMode:
                print(Report_Message)
            opem.Functions.HTML_Desc(
                Simulation_Title, Padulles_Description, HTMLFile)
            opem.Functions.HTML_Input_Table(
                Input_Dict=Input_Dict,
                Input_Params=InputParams,
                file=HTMLFile)
            opem.Functions.HTML_Overall_Params_Table(
                Overall_Params_Max,
                Overall_Params_Max_Description,
                file=HTMLFile,
                header=True)
            opem.Functions.HTML_Chart(
                x=str(I_List),
                y=str(Power_List),
                color='rgba(255,99,132,1)',
                x_label="I(A)",
                y_label="P(W)",
                chart_name="FC-Power",
                size="600px",
                file=HTMLFile)
            opem.Functions.HTML_Chart(
                x=str(I_List), y=[
                    str(Vstack_List), str(Estimated_V)], color=[
                    'rgba(99,100,255,1)', 'rgb(238, 210, 141)'], x_label="I(A)", y_label="V(V)", chart_name=[
                    "FC-Voltage", "Linear-Apx"], size="600px", file=HTMLFile)
            opem.Functions.HTML_Overall_Params_Table(
                Overall_Params_Linear,
                Overall_Params_Linear_Description,
                file=HTMLFile,
                header=False)
            opem.Functions.HTML_Chart(
                x=str(I_List),
                y=str(Efficiency_List),
                color='rgb(255, 0, 255)',
                x_label="I(A)",
                y_label="EFF",
                chart_name="Efficiency",
                size="600px",
                file=HTMLFile)
            opem.Functions.HTML_Chart(
                x=str(I_List),
                y=str(PO2_List),
                color='	rgb(0, 255, 128)',
                x_label="I(A)",
                y_label="PO2(atm)",
                chart_name="PO2",
                size="600px",
                file=HTMLFile)
            opem.Functions.HTML_Chart(
                x=str(I_List),
                y=str(PH2_List),
                color='	rgb(128, 0, 255)',
                x_label="I(A)",
                y_label="PH2(atm)",
                chart_name="PH2",
                size="600px",
                file=HTMLFile)
            opem.Functions.HTML_Chart(x=str(list(map(opem.Functions.rounder,
                                                     Power_List))),
                                      y=str(Efficiency_List),
                                      color='rgb(238, 210, 141)',
                                      x_label="P(W)",
                                      y_label="EFF",
                                      chart_name="Efficiency vs Power",
                                      size="600px",
                                      file=HTMLFile)
            opem.Functions.HTML_Chart(
                x=str(I_List),
                y=str(Power_Thermal_List),
                color='rgb(255, 0, 255)',
                x_label="I(A)",
                y_label="P(W)",
                chart_name="Power(Thermal)",
                size="600px",
                file=HTMLFile)
            opem.Functions.warning_print(
                warning_flag_1=Warning1,
                warning_flag_2=Warning2,
                I_Warning=I_Warning,
                file=HTMLFile,
                PrintMode=PrintMode)
            opem.Functions.HTML_End(HTMLFile)
            HTMLFile.close()
        if PrintMode:
            print("Done!")
        if not TestMode:
            if PrintMode:
                print(
                    "Result In -->" +
                    os.path.join(
                        os.getcwd(),
                        Simulation_Title))
        else:
            return {
                "Status": True,
                "P": Power_List,
                "I": I_List,
                "V": Vstack_List,
                "EFF": Efficiency_List,
                "PO2": PO2_List,
                "PH2": PH2_List,
                "Ph": Power_Thermal_List,
                "V0": B0,
                "K": B1,
                "VE": Estimated_V}
    except Exception:
        if TestMode:
            return {
                "Status": False,
                "Message": "[Error] " +
                Simulation_Title +
                " Simulation Failed!(Check Your Inputs)"}
        print(
            "[Error] " +
            Simulation_Title +
            " Simulation Failed!(Check Your Inputs)")
