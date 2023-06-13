# -*- coding: utf-8 -*-
"""Amphlett model functions."""
import math, traceback, io
from opem.Params import Amphlett_InputParams as InputParams
from opem.Params import Amphlett_OutputParams as OutputParams
from opem.Params import Amphlett_Params_Default as Defaults
from opem.Params import xi1, xi3, xi4, HHV, uF, R, F, Amphlett_Description, Overall_Params_Max_Description,\
    Overall_Params_Linear_Description, Eth, Report_Message
import opem.Functions
import os


def B_Calc(T, n=2):
    """
    Calculate B (Constant in the mass transfer term).

    :param T: cell operation temperature [K]
    :type T : float
    :param n: number of moles of electrons transferred in the balanced equation occurring in the fuel cell
    :type n: int
    :return: B as float
    """
    try:
        return (R * T) / (n * F)
    except (TypeError, ZeroDivisionError):
        return None


def Power_Thermal_Calc(VStack, N, i):
    """
    Calculate thermal power.

    :param VStack: VStack [V]
    :type VStack : float
    :param N: number of single cells
    :type N : int
    :param i: cell load current [A]
    :type i : float
    :return: thermal power [W]
    """
    try:
        return i * ((N * Eth) - VStack)
    except TypeError:
        return None


def Power_Total_Calc(VStack_List, i_step, N):
    """
    Calculate total elec power and total thermal power by calling integrate function.

    :param VStack_List: Vstack list
    :type VStack_List : list
    :param i_step: cell load current step
    :type i_step : float
    :param N: number of single cells
    :type N : int
    :return: [total elec power,total thermal power] as list
    """
    try:
        Filtered_List = list(filter(lambda x: x is not None, VStack_List))
        Filtered_List_Not = list(map(lambda x: (N * Eth) - x, Filtered_List))
        Total_Elec_Power = opem.Functions.integrate(Filtered_List, i_step)
        Total_Thermal_Power = opem.Functions.integrate(
            Filtered_List_Not, i_step)
        return [Total_Elec_Power, Total_Thermal_Power]
    except Exception:
        return [None, None]


def Linear_Aprox_Params_Calc(B0, B1):
    """
    Calculate linear approximation overall parameters.

    :param B0: intercept
    :type B0 : float
    :param B1: slope
    :type B1 : float
    :return: [Wmax,Vcell_Wmax] as list
    """
    Wmax = 0
    Vcell_Wmax = 0
    try:
        Wmax = (B0**2) / (4 * B1)
    except Exception:
        Wmax = None
    try:
        Vcell_Wmax = (B0 / 2)
    except Exception:
        Vcell_Wmax = None
    if Wmax is not None:
        Wmax = abs(Wmax)
    if Vcell_Wmax is not None:
        Vcell_Wmax = abs(Vcell_Wmax)
    return [Wmax, Vcell_Wmax]


def Max_Params_Calc(Power_List, EFF_List, VStack_List):
    """
    Calculate maximum overall parameters.

    :param Power_List: power list
    :type Power_List : list
    :param EFF_List: efficiency list
    :type EFF_List : list
    :param VStack_List: Vstack list
    :type VStack_List : list
    :return: {max power,max efficiency,max VStack] as list
    """
    Max_Power = max(list(filter(lambda x: x is not None, Power_List)))
    Max_EFF = EFF_List[Power_List.index(Max_Power)]
    Max_VStack = VStack_List[Power_List.index(Max_Power)]
    return {
        "Max_Power": Max_Power,
        "Max_EFF": Max_EFF,
        "Max_VStack": Max_VStack}


def R_Calc(V, i):
    """
    Calculate cell total resistance.

    :param V: cell voltage [V]
    :type V : float
    :param i: cell load current [A]
    :type i : float
    :return: resistance as float [ohm]
    """
    try:
        return V / i
    except (TypeError, ZeroDivisionError):
        print(
            "[Error] R Total Calculation Failed (V:%s ,i:%s)" %
            (str(V), str(i)))


def Enernst_Calc(T, PH2, PO2):
    """
    Calculate Enernst.

    :param T: cell operation temperature [K]
    :type T : float
    :param PH2: partial pressure [atm]
    :type PH2 : float
    :param PO2: partial Pressure [atm]
    :type PO2: float
    :return: Enernst [V} as float
    """
    try:
        result = 1.229 - (8.5 * (10 ** -4)) * (T - 298.15) + (4.308 *
                                                              (10 ** -5)) * T * (math.log(PH2) + 0.5 * math.log(PO2))
        return result
    except (TypeError, OverflowError, ValueError):
        print(
            "[Error] Enernst Calculation Failed (T:%s , PH2:%s, PO2:%s)" %
            (str(T), str(PH2), str(PO2)))


def CH2_Calc(PH2, T):
    """
    Calculate CH2.

    :param PH2: partial pressure [atm]
    :type PH2 : float
    :param T: cell operation temperature [K]
    :type T:float
    :return: CH2 [mol/cm^3] as float
    """
    try:
        result = PH2 / (1.09 * (10 ** 6) * math.exp(77 / T))
        return result
    except (TypeError, ZeroDivisionError, OverflowError, ValueError):
        print(
            "[Error] CH2 Calculation Failed (PH2:%s, T:%s)" %
            (str(PH2), str(T)))


def CO2_Calc(PO2, T):
    """
    Calculate CO2.

    :param PO2: partial pressure [atm]
    :type PO2 : float
    :param T: cell operation temperature [K]
    :type T : float
    :return: CO2 [mol/cm^3] as float
    """
    try:
        result = PO2 / (5.08 * (10 ** 6) * math.exp(-498 / T))
        return result
    except (TypeError, ZeroDivisionError, OverflowError, ValueError):
        print(
            "[Error] CO2 Calculation Failed (PO2:%s, T:%s)" %
            (str(PO2), str(T)))


def Rho_Calc(i, A, T, lambda_param):
    """
    Calculate Rho.

    :param i: cell load current [A]
    :type i : float
    :param A: active area [cm^2]
    :type A:float
    :param T: cell operation temperature [K]
    :type T:float
    :param lambda_param: is an adjustable parameter with a possible maximum value of 23
    :type lambda_param : float
    :return: Rho -- > membrane specific resistivity [ohm.cm] as float
    """
    try:
        result = (181.6 * (1 + 0.03 * (i / A) + 0.062 * ((T / 303) ** 2) * ((i / A) ** 2.5))
                  ) / ((lambda_param - 0.634 - 3 * (i / A)) * math.exp(4.18 * ((T - 303) / T)))
        return result
    except (TypeError, ZeroDivisionError, OverflowError, ValueError):
        print(
            "[Error] Rho Calculation Failed (i:%s, A:%s, T:%s, lambda:%s)" %
            (str(i), str(A), str(T), str(lambda_param)))


def Xi2_Calc(A, PH2, T):
    """
    Calculate Xi2.

    :param A: active area [cm^2]
    :type A : float
    :param PH2: partial pressure [atm]
    :type PH2:float
    :param T: cell operation temperature [K]
    :type T:float
    :return: Xi2 as float
    """
    try:
        CH2 = CH2_Calc(PH2, T)
        result = 0.00286 + 0.0002 * \
            math.log(A) + (4.3 * (10 ** -5)) * math.log(CH2)
        return result
    except (TypeError, OverflowError, ValueError):
        print(
            "[Error] Xi2 Calculation Failed (A:%s, PH2:%s, T:%s)" %
            (str(A), str(PH2), str(T)))


def Eta_Conc_Calc(i, A, B, JMax):
    """
    Calculate Eta concentration.

    :param i: cell load current [A]
    :type i :float
    :param A: active area [cm^2]
    :type A : float
    :return: Eta concentration [V] as float
    """
    try:
        if i != 0:
            J = (i / A)
            result = -B * math.log(1 - (J / JMax))
            return result
        return 0
    except (TypeError, ZeroDivisionError, OverflowError, ValueError):
        print(
            "[Error] Eta Concentration Calculation Failed (i:%s, A:%s, B:%s, JMax:%s)" %
            (str(i), str(A), str(B), str(JMax)))


def Eta_Ohmic_Calc(i, l, A, T, lambda_param, R_elec=None):
    """
    Calculate Eta ohmic.

    :param R_elec: R electronic [ohm]
    :type R_elec:float
    :param i: cell load current [A]
    :type i:float
    :param l: membrane thickness [cm]
    :type l:float
    :param A: active area [cm^2]
    :type A:float
    :param T: cell operation temperature [K]
    :type T:float
    :param lambda_param: is an adjustable parameter with a possible maximum value of 23
    :type lambda_param:float
    :return: Eta ohmic [V] as float
    """
    try:
        if i != 0:
            Rho = Rho_Calc(i, A, T, lambda_param)
            R_prot = (Rho * l) / A
            R_total = R_prot
            if opem.Functions.isfloat(R_elec):
                R_total += R_elec
            result = i * R_total
            return result
        return 0
    except (TypeError, ZeroDivisionError):
        print(
            "[Error] Eta Ohmic Calculation Failed (i:%s, l:%s, A:%s, T:%s, lambda:%s, R_elec:%s)" %
            (str(i), str(l), str(A), str(T), str(lambda_param), str(R_elec)))


def Eta_Act_Calc(T, PO2, PH2, i, A):
    """
    Calculate Eta activation.

    :param T: cell operation temperature [K]
    :type T:float
    :param PO2: partial pressure [atm]
    :type PO2:float
    :param i: cell load current [A]
    :type i:float
    :return:  Eta activation [V] as float
    """
    try:
        if i != 0:
            CO2 = CO2_Calc(PO2, T)
            xi2 = Xi2_Calc(A, PH2, T)
            result = -(xi1 + xi2 * T + xi3 * T *
                       math.log(CO2) + xi4 * T * math.log(i))
            return result
        return 0
    except (TypeError, OverflowError, ValueError):
        print(
            "[Error] Eta Activation Calculation Failed (T:%s, PO2:%s, PH2:%s, i:%s, A:%s)" %
            (str(T), str(PO2), str(PH2), str(i), str(A)))


def Efficiency_Calc(Vcell):
    """
    Calculate PEM cell efficiency.

    :param Vcell: cell voltage [V]
    :type Vcell:float
    :return: efficiency as float
    """
    try:
        result = (uF * Vcell) / HHV
        return result
    except (TypeError, ZeroDivisionError):
        print(
            "[Error] PEM Efficiency Calculation Failed (Vcell:%s)" %
            str(Vcell))


def VStack_Calc(N, Vcell):
    """
    Calculate VStack.

    :param N: number of single cells
    :type N  :int
    :param Vcell: cell voltage [V}
    :type Vcell:float
    :return: VStack [V] as float
    """
    try:
        result = N * (Vcell)
        return result
    except TypeError:
        print(
            "[Error] VStack Calculation Error (N:%s, Vcell:%s)" %
            (str(N), str(Vcell)))


def Loss_Calc(Eta_Act, Eta_Ohmic, Eta_Conc):
    """
    Calculate loss.

    :param Eta_Act: Eta activation [V]
    :type Eta_Act : float
    :param Eta_Ohmic: Eta ohmic [V]
    :type Eta_Ohmic : float
    :param Eta_Conc: Eta concentration [V]
    :type Eta_Conc : float
    :return: loss [V] as float
    """
    try:
        result = Eta_Act + Eta_Ohmic + Eta_Conc
        return result
    except TypeError:
        print(
            "[Error] Loss Calculation Error (Eta_Act:%s, Eta_Ohmic:%s, Eta_Conc:%s)" %
            (str(Eta_Act), str(Eta_Ohmic), str(Eta_Conc)))


def Vcell_Calc(Enernst, Loss):
    """
    Calculate cell voltage.

    :param Enernst:  Enernst [V}
    :type Enernst : float
    :param Loss:  loss [V]
    :type Loss : float
    :return:  cell voltage [V] as float
    """
    try:
        result = Enernst - Loss
        return result
    except TypeError:
        print(
            "[Error] Vcell Calculation Error (Enernst:%s, Loss:%s)" %
            (str(Enernst), str(Loss)))


def Power_Calc(Vcell, i):
    """
    Calculate power.

    :param Vcell: Vcell Voltage [V]
    :type Vcell : float
    :param i: cell load current [A]
    :type i : float
    :return: cell power [W] as float
    """
    try:
        result = Vcell * i
        return result
    except TypeError:
        print(
            "[Error] Power Calculation Error (Vcell:%s, i:%s)" %
            (str(Vcell), str(i)))


def PowerStack_Calc(Power, N):
    """
    Calculate power_stack.

    :param Power: single cell power [W]
    :type Power : float
    :param N: number of single cells
    :type N : int
    :return: power stack [W] as float
    """
    try:
        result = N * Power
        return result
    except TypeError:
        print(
            "[Error] Power Stack Calculation Error (Power:%s, N:%s)" %
            (str(Power), str(N)))


def Static_Analysis(HTMLfilepath, CSVfilepath, OPEMfilepath,
        InputMethod=opem.Functions.Get_Input, 
        TestMode=False,
        PrintMode=True,
        ReportMode=True):
    """
    Run Amphlett static analysis.

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
    Simulation_Title = "Amphlett"
    try:
        if PrintMode:
            print("###########")
            print(Simulation_Title + "-Model Simulation")
            print("###########")
        OutputParamsKeys = sorted(OutputParams.keys())
        Output_Dict = dict(
            zip(OutputParamsKeys, [None] * len(OutputParamsKeys)))
        if not TestMode:
            Input_Dict = InputMethod(InputParams, params_default=Defaults)
        else:
            Input_Dict = InputMethod
            Input_Dict = opem.Functions.filter_default(
                input_dict=Input_Dict, params_default=Defaults)
        Input_Dict = opem.Functions.filter_lambda(Input_Dict)
        if PrintMode:
            print("Analyzing . . .")
        Name = Input_Dict["Name"]
        if ReportMode:
            OutputFile = opem.Functions.Output_Init(
                Input_Dict, Simulation_Title, Name, OPEMfilepath)
            CSVFile = opem.Functions.CSV_Init(
                OutputParamsKeys,
                OutputParams,
                Simulation_Title,
                Name, CSVfilepath)
            HTMLFile = opem.Functions.HTML_Init(Simulation_Title, Name, HTMLfilepath)
        IEndMax = Input_Dict["JMax"] * Input_Dict["A"]
        IEnd = min(IEndMax, Input_Dict["i-stop"])
        IStep = Input_Dict["i-step"]
        Precision = opem.Functions.get_precision(IStep)
        Output_Dict["Enernst"] = Enernst_Calc(
            Input_Dict["T"], Input_Dict["PH2"], Input_Dict["PO2"])
        [i, IEnd, IStep] = opem.Functions.filter_range(
            Input_Dict["i-start"], IEnd, IStep)
        I_List = []
        Efficiency_List = []
        Power_List = []
        Vstack_List = []
        Eta_Ohmic_List = []
        Eta_Conc_List = []
        Eta_Active_List = []
        Power_Thermal_List = []
        B = B_Calc(Input_Dict["T"])
        # R_List=[]
        while i < IEnd:
            try:
                I_List.append(i)
                Output_Dict["Eta Activation"] = Eta_Act_Calc(
                    Input_Dict["T"], Input_Dict["PO2"], Input_Dict["PH2"], i, Input_Dict["A"])
                Eta_Active_List.append(Output_Dict["Eta Activation"])
                Output_Dict["Eta Ohmic"] = Eta_Ohmic_Calc(
                    i,
                    Input_Dict["l"],
                    Input_Dict["A"],
                    Input_Dict["T"],
                    Input_Dict["lambda"],
                    R_elec=Input_Dict["R"])
                Eta_Ohmic_List.append(Output_Dict["Eta Ohmic"])
                Output_Dict["Eta Concentration"] = Eta_Conc_Calc(
                    i, Input_Dict["A"], B, Input_Dict["JMax"])
                Eta_Conc_List.append(Output_Dict["Eta Concentration"])
                Output_Dict["Loss"] = Loss_Calc(
                    Output_Dict["Eta Activation"],
                    Output_Dict["Eta Ohmic"],
                    Output_Dict["Eta Concentration"])
                Output_Dict["Vcell"] = Vcell_Calc(
                    Output_Dict["Enernst"], Output_Dict["Loss"])
                [Warning1, I_Warning] = opem.Functions.warning_check_1(
                    Output_Dict["Vcell"], I_Warning, i, Warning1)
                Warning2 = opem.Functions.warning_check_2(
                    Vcell=Output_Dict["Vcell"], warning_flag=Warning2)
                Output_Dict["PEM Efficiency"] = Efficiency_Calc(
                    Output_Dict["Vcell"])
                Output_Dict["Power"] = Power_Calc(Output_Dict["Vcell"], i)
                Output_Dict["VStack"] = VStack_Calc(
                    Input_Dict["N"], Output_Dict["Vcell"])
                #Output_Dict["R Total"] = R_Calc(Output_Dict["VStack"],i)
                #R_List.append(Output_Dict["R Total"])
                Vstack_List.append(Output_Dict["VStack"])
                Efficiency_List.append(Output_Dict["PEM Efficiency"])
                Output_Dict["Power-Thermal"] = Power_Thermal_Calc(
                    VStack=Output_Dict["VStack"], N=Input_Dict["N"], i=i)
                Output_Dict["Power-Stack"] = PowerStack_Calc(
                    Output_Dict["Power"], Input_Dict["N"])
                Power_List.append(Output_Dict["Power-Stack"])
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
                if opem.Functions.ReporttMode:
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
        Power_Total = Power_Total_Calc(Vstack_List, IStep, Input_Dict["N"])
        Overall_Params_Linear["Pmax(L-Approx)"] = Linear_Approx_Params[0]
        Overall_Params_Linear["V0"] = B0
        Overall_Params_Linear["K"] = B1
        Overall_Params_Linear["VFC|Pmax(L-Approx)"] = Linear_Approx_Params[1]

        Overall_Params_Max["Pmax"] = Max_Params["Max_Power"]
        Overall_Params_Max["VFC|Pmax"] = Max_Params["Max_VStack"]
        Overall_Params_Max["Efficiency|Pmax"] = Max_Params["Max_EFF"]
        Overall_Params_Max["Ptotal(Elec)"] = Power_Total[0]
        Overall_Params_Max["Ptotal(Thermal)"] = Power_Total[1]
        
        HTMLcontent = ""
        CSVcontent = ""
        OPEMcontent = ""
        
        if ReportMode:
            OutputFile.close()
            OutputFile = io.open(OPEMfilepath, 'r')
            OPEMcontent = OutputFile.read()
            OutputFile.close()
            CSVFile.close()
            CSVFile = io.open(CSVfilepath, 'r')
            CSVcontent = CSVFile.read()
            CSVFile.close()
            if PrintMode:
                print(Report_Message)
            opem.Functions.HTML_Desc(
                Simulation_Title, Amphlett_Description, HTMLFile)
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
                chart_name="Power-Stack",
                size="600px",
                file=HTMLFile)
            # HTML_Chart(x=str(I_List), y=str(R_List), color='rgb(159, 82, 71)', x_label="I(A)", y_label="R(ohm)",
            # chart_name="R Total", size="600px", file=HTMLFile)
            opem.Functions.HTML_Chart(
                x=str(I_List), y=[
                    str(Vstack_List), str(Estimated_V)], color=[
                    'rgba(99,100,255,1)', 'rgb(238, 210, 141)'], x_label="I(A)", y_label="V(V)", chart_name=[
                    "Voltage-Stack", "Linear-Apx"], size="600px", file=HTMLFile)
            opem.Functions.HTML_Overall_Params_Table(
                Overall_Params_Linear,
                Overall_Params_Linear_Description,
                file=HTMLFile,
                header=False)
            opem.Functions.HTML_Chart(x=str(I_List),
                                      y=[str(Eta_Active_List),
                                         str(Eta_Conc_List),
                                         str(Eta_Ohmic_List)],
                                      color=['rgba(255,99,132,1)',
                                             'rgba(99,100,255,1)',
                                             'rgb(238, 210, 141)'],
                                      x_label="I(A)",
                                      y_label="V(V)",
                                      chart_name=["Eta Active",
                                                  "Eta Conc",
                                                  "Eta Ohmic"],
                                      size="600px",
                                      file=HTMLFile)
            opem.Functions.HTML_Chart(
                x=str(I_List),
                y=str(Efficiency_List),
                color='rgb(255, 0, 255)',
                x_label="I(A)",
                y_label="EFF",
                chart_name="Efficiency",
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
            HTMLFile = io.open(HTMLfilepath, 'r', encoding="utf-8")
            HTMLcontent = HTMLFile.read()
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
                "Ph": Power_Thermal_List,
                "V0": B0,
                "K": B1,
                "Eta_Active": Eta_Active_List,
                "Eta_Conc": Eta_Conc_List,
                "Eta_Ohmic": Eta_Ohmic_List,
                "VE": Estimated_V,
                "HTML": HTMLcontent,
                "CSV": CSVcontent,
                "OPEM": OPEMcontent}
    except Exception as e:
        if TestMode:
            return {
                "Status": False,
                "Message": "[Error] " +
                Simulation_Title +
                " Simulation Failed!(Check Your Inputs)",
                "Exception": traceback.format_exc()}
        print(
            "[Error] " +
            Simulation_Title +
            " Simulation Failed!(Check Your Inputs)")
