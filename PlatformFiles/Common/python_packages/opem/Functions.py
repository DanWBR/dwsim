# -*- coding: utf-8 -*-
"""OPEM functions."""
import datetime
from art import text2art
import opem.Script
from opem.Params import Version, Website, UpdateUrl, Warning_Message_1, Warning_Message_2
from opem.Params import HTML_Init_Template, HTML_Input_Table_Template1, HTML_Input_Table_Template2, HTML_Overall_Params_Table_Template, HTML_End_Template
import io
import os
import requests
import webbrowser
import sys


def integrate(y_vals, h):
    """
    Calculate integral with Simpson's Rule.

    :param y_vals: output values
    :type y_valS : list
    :param h: interval
    :type h : float
    :return: integrate output as float
    """
    try:
        i = 1
        total = y_vals[0] + y_vals[-1]
        for y in y_vals[1:-1]:
            if i % 2 == 0:
                total += 2 * y
            else:
                total += 4 * y
            i += 1
        return total * (h / 3.0)
    except (TypeError, ZeroDivisionError):
        return None


def linear_plot(x, y):
    """
    Clear input data and call estimate_coef.

    :param x:  x data
    :type x : list
    :param y: y data
    :type y : list
    :return: [estimated_y,intercept,slope] as list
    """
    clear_x = []
    clear_y = []
    estimate_y = []
    none_x = []
    for index, item in enumerate(y):
        if item is not None:
            clear_y.append(item)
            clear_x.append(x[index])
        else:
            none_x.append(x[index])
    [B1, B0] = estimate_coef(clear_x, clear_y)
    for i in x:
        if i not in none_x:
            estimate_y.append(B0 + B1 * i)
        else:
            estimate_y.append(None)
    return [estimate_y, B0, B1]


def estimate_coef(clear_x, clear_y):
    """
    Linear regression function.

    :param clear_x: cleared_x
    :type clear_x : list
    :param clear_y: cleared_y
    :type clear_y : list
    :return: [slope,intercept]
    """
    try:
        n = len(clear_x)
        mean_x = sum(clear_x) / n
        mean_y = sum(clear_y) / n
        SS_xy = 0
        SS_xx = 0
        for index, item in enumerate(clear_x):
            SS_xx += item**2
            SS_xy += item * clear_y[index]
        SS_xx -= n * (mean_x)**2
        SS_xy -= n * mean_x * mean_y
        B1 = SS_xy / SS_xx
        B0 = mean_y - B1 * mean_x
        return [B1, B0]
    except (TypeError, ZeroDivisionError, OverflowError, ValueError):
        return [0, 0]


def line(num=11, char="#"):
    """
    Print line of char.

    :param num: number of character in this line
    :type num : int
    :param char: character
    :type char : str
    :return: None
    """
    print(char * num)


def check_update(Version):
    """
    Check for new opem version in website.

    :return: None
    """
    try:
        update_obj = requests.get(UpdateUrl)
        update_data = update_obj.text
        if float(update_data) > Version:
            line()
            print("New Version (" + update_data + ") Is Available!")
            print("Website : " + Website)
            line()
    except Exception:
        print("Update Check Failed!")


def filter_default(input_dict, params_default):
    """
    Filter input parameters with default params.

    :param input_dict: input parameters
    :type input_dict : dict
    :param params_default: default parameters
    :type params_default : dict
    :return: modified input_dict as dict
    """
    for i in params_default.keys():
        if i not in input_dict.keys():
            input_dict[i] = params_default[i]
    return input_dict


def get_precision(input_number):
    """
    Return precision of input number.

    :param input_number: input number
    :type input_number : float
    :return: precision as int
    """
    input_string = str(input_number)
    if "." in input_string:
        splitted_input = input_string.split(".")
        return len(splitted_input[1])
    return 0


def isfloat(value):
    """
    Check input for float conversion.

    :param value: input value
    :type value:str
    :return: True if input_value is a number and False otherwise
    """
    try:
        float(value)
        return True
    except ValueError:
        return False


def rounder(input_number, digit=2):
    """
    Round input number.

    :param input_number: input number
    :type input_number : anything
    :param digit: precision
    :type digit : int
    :return: round number as float
    """
    try:
        if isfloat(input_number):
            return round(input_number, digit)
        return input_number
    except Exception:
        return None


def input_test(a):
    """
    Injected function for Get_Input testing.

    :param a: input
    :return: "1"
    """
    _ = a
    return "1"


def Get_Input(InputParams, input_item=input, params_default={}):
    """
    Get inputs from users.

    :param InputParams : input parameters  for each  model
    :type InputParams :dict
    :param input_item : input function (this parameter added for Get_Input doctest)
    :return: input dictionary
    """
    try:
        Input_Keys = sorted(InputParams.keys())
        Input_Values = []
        Name = ""
        while(True):
            Name = input_item("Please Enter Simulation Name :")
            if len(Name) != 0:
                break
            else:
                print("[Error] Bad Name Try Again")
        for item in Input_Keys:
            Input_Flag = False
            Input_Item = None
            while not Input_Flag:
                Input_Item = input_item(
                    "Please Enter " + item + "(" + InputParams[item] + ") : ")
                if isfloat(Input_Item):
                    Input_Flag = True
                else:
                    if item in params_default.keys():
                        Input_Item = params_default[item]
                        Input_Flag = True
                    else:
                        print("[Error] Bad Input Try Again")
            Input_Values.append(Input_Item)
        Input_Values = list(map(float, Input_Values))
        Output = dict(zip(Input_Keys, Input_Values))
        Output["Name"] = Name
        return Output
    except Exception:
        print("Bad Input")
        return False


def Output_Save(
        OutputParamsKeys,
        OutputDict,
        OutputParams,
        i,
        file,
        PrintMode):
    """
    Write analysis result in Simulation-Result.opem file.

    :param OutputParamsKeys : output parameters keys
    :type OutputParamsKeys : list
    :param OutputDict: analysis result dictionary
    :type OutputDict: dict
    :param OutputParams : output parameters
    :type OutputParams : dict
    :param i: cell load current [A]
    :type i : float
    :param file : file object
    :return: None
    """
    spliter = "\n"
    if 'win' not in sys.platform:
        spliter = "\r\n"
    file.write("I :" + str(i) + " A " + spliter * 2)
    if PrintMode:
        print("I : " + str(i))
    for key in OutputParamsKeys:
        file.write(key +
                   " : " +
                   str(OutputDict[key]) +
                   " " +
                   OutputParams[key] +
                   spliter)
        if PrintMode:
            print(key +
                  " : " +
                  str(OutputDict[key]) +
                  " " +
                  OutputParams[key])
    file.write("###########" + spliter)
    if PrintMode:
        print("###########")


def Output_Init(InputDict, Title, Name, FilePath):
    """
    Initialize output file.

    :param InputDict: input test vector
    :type InputDict:dict
    :param Title : simulation title
    :type Title :str
    :return: file object
    """
    spliter = "\n"
    if 'win' not in sys.platform:
        spliter = "\r\n"
    Art = text2art("Opem")
    opem_file = open(FilePath, "w")
    opem_file.write(Art)
    opem_file.write("Simulation Date : " +
                    str(datetime.datetime.now()) + spliter)
    opem_file.write("**********" + spliter)
    opem_file.write(Title + " Model" + spliter * 2)
    opem_file.write("**********" + spliter)
    opem_file.write("Simulation Inputs : " + spliter * 2)
    Input_Keys = sorted(InputDict.keys())
    for key in Input_Keys:
        opem_file.write(key + " : " + str(InputDict[key]) + spliter)
    opem_file.write("**********" + spliter)
    return opem_file


def CSV_Init(OutputParamsKeys, OutputParams, Title, Name, FilePath):
    """
    Initialize csv file.

    :param OutputParamsKeys: output parameters Keys
    :type OutputParamsKeys : list
    :param OutputParams : output parameters
    :type OutputParams : dict
    :return: file object
    """
    csv_file = open(FilePath, "w")
    csv_file.write("I (A),")
    for index, item in enumerate(OutputParamsKeys):
        csv_file.write(item + " (" + OutputParams[item] + ")")
        if index < len(OutputParamsKeys) - 1:
            csv_file.write(",")
    csv_file.write("\n")
    return csv_file


def None_Omit(Input_Str):
    """
    Replace None object with "None" string.

    :param Input_Str: input string
    :type Input_Str : str
    :return: modified string as str
    """
    result = Input_Str
    result = result.replace("None", '\"None\"')
    return result


def HTML_Init(Title, Name, FilePath):
    """
    Initialize html file.

    :param Title: simulation title (analysis model)
    :type Title : str
    :param Name: file name
    :type Name : str
    :return: HTML file as file obj
    """
    HTMLFile = io.open(FilePath, "w", encoding="utf-8")
    HTMLFile.write(HTML_Init_Template.format(opem.Script.JS_SCRIPT, Title))
    return HTMLFile


def HTML_Desc(Title, Description, file):
    """
    Write model description in html file.

    :param Title: simulation title (analysis model)
    :type Title : str
    :param Description: model description
    :type Description : str
    :param file: html file object
    :type file : file object
    :return: None
    """
    file.write('<h2 style="color:#ff7600;">What is ' + Title + ' ?</h2>\n')
    file.write(
        '<p style = "text-align:justify;margin:15px;">' +
        Description +
        "</p>\n")


def HTML_Chart(x, y, color, x_label, y_label, chart_name, size, file):
    """
    Write chartjs chart in html file.

    :param x: x data as a string list
    :type x : str
    :param y: y data as string list (or list of y)
    :param color: color code of chart (or list of color)
    :param x_label:x-axis label
    :type x_label : str
    :param y_label:y-axis label
    :type y_label : str
    :param chart_name: chart name (or list of chart_name)
    :param size: chart size in pixel
    :type size : str
    :param file: html file object
    :type file : file object
    :return: None
    """
    chart_data = ""
    chart_title = str(chart_name)
    if " " in chart_title:
        chart_title = chart_title.replace(" ", "-")
    if isinstance(y, list):
        y_data = list(map(None_Omit, y))
        for index, data in enumerate(y_data):
            chart_data += opem.Script.CHART_DATA.format(
                chart_name[index], data, color[index])
            if index != len(y_data) - 1:
                chart_data += ","
            chart_data += "\n"
    else:
        y_data = None_Omit(y)
        chart_data = opem.Script.CHART_DATA.format(chart_name, y_data, color)
    x_data = None_Omit(x)
    file.write(
        opem.Script.LINE_CHART.format(
            x_data,
            y_label,
            x_label,
            chart_title,
            size,
            chart_data))


def HTML_Input_Table(Input_Dict, Input_Params, file):
    """
    Add table to html file.

    :param Input_Dict: input values dictionary
    :type Input_Dict : dict
    :param Input_Params: input parameters dictionary
    :type Input_Params : dict
    :param file: html file object
    :type file : file object
    :return: None
    """
    file.write(HTML_Input_Table_Template1)
    Input_Params_Keys = sorted(Input_Params.keys())
    for key in Input_Params_Keys:
        file.write(
            HTML_Input_Table_Template2.format(
                key, Input_Params[key], str(
                    Input_Dict[key])))
    file.write("</table>\n")


def HTML_Overall_Params_Table(
        Input_Dict,
        Input_Params,
        file,
        header=False):
    """
    Add table to html file.

    :param Input_Dict: input values dictionary
    :type Input_Dict : dict
    :param Input_Params: input parameters dictionary
    :type Input_Params : dict
    :param file: html file object
    :type file : file object
    :return: None
    """
    if header:
        file.write('<h2 style="color:#ff7600;">Overall Parameters</h2>\n')
    file.write(HTML_Overall_Params_Table_Template)
    Input_Params_Keys = sorted(Input_Params.keys())
    for key in Input_Params_Keys:
        file.write(
            HTML_Input_Table_Template2.format(
                key, Input_Params[key], str(
                    Input_Dict[key])))
    file.write("</table>\n")
    if header:
        file.write('<h2 style="color:#ff7600;">Graphs</h2>\n')


def HTML_End(file):
    """
    Add end part of html file.

    :param file: html file object
    :type file : file object
    :return: None
    """
    file.write(HTML_End_Template.format(str(Version)))
    file.write("</body>\n")
    file.write("</html>")


def CSV_Save(OutputParamsKeys, OutputDict, i, file):
    """
    Save parameters in CSV file.

    :param OutputParamsKeys : output parameters keys
    :type OutputParamsKeys : list
    :param OutputDict: analysis result dictionary
    :type OutputDict:dict
    :param i: cell load current [A]
    :type i : float
    :param file : file object
    :return: None
    """
    file.write(str(i) + ",")
    for key in OutputParamsKeys:
        file.write(str(OutputDict[key]))
        if key != OutputParamsKeys[-1]:
            file.write(",")
    file.write("\n")


def filter_lambda(Input_Dict):
    """
    Filter lambda parameter.

    :param Input_Dict: input parameters dictionary
    :type Input_Dict : dict
    :return: modified dictionary
    """
    try:
        if Input_Dict["lambda"] > 23:
            Input_Dict["lambda"] = 23
            print(
                "[Warning] Opem Automatically Set Lambda To Maximum Value (23) ")
        elif Input_Dict["lambda"] < 14:
            Input_Dict["lambda"] = 23
            print(
                "[Warning] Opem Automatically Set Lambda To Minimum Value (14) ")
        return Input_Dict
    except Exception:
        return Input_Dict


def left_justify(words, width):
    """
    Left justify words.

    :param words: list of words
    :type words : list
    :param width: width of each line
    :type width: int
    :return: left justified words as list
    """
    return ' '.join(words).ljust(width)


def justify(words, width):
    """
    Justify input words.

    :param words: list of words
    :type words : list
    :param width: width of each line
    :type width : int
    :return: list of justified words as list
    """
    line = []
    col = 0
    for word in words:
        if line and col + len(word) > width:
            if len(line) == 1:
                yield left_justify(line, width)
            else:
                # After n + 1 spaces are placed between each pair of
                # words, there are r spaces left over; these result in
                # wider spaces at the left.
                n, r = divmod(width - col + 1, len(line) - 1)
                narrow = ' ' * (n + 1)
                if r == 0:
                    yield narrow.join(line)
                else:
                    wide = ' ' * (n + 2)
                    yield wide.join(line[:r] + [narrow.join(line[r:])])
            line, col = [], 0
        line.append(word)
        col += len(word) + 1
    if line:
        yield left_justify(line, width)


def description_print(Analysis_Name, Description_Dict, Width=100):
    """
    Print justified text for overview and each model description in console.

    :param Analysis_Name: analysis model name
    :type Analysis_Name : str
    :param Description_Dict: description dict (in params)
    :type Description_Dict : dict
    :param Width: width of each line (for justify)
    :type Width : int
    :return: None
    """
    line()
    print(Analysis_Name.replace("_", " ") + " : \n")
    if Analysis_Name.find("Padulles") != -1:
        print("\n")
        print(
            "\n".join(
                justify(
                    Description_Dict["General Padulles"].split(),
                    Width)))
    print("\n")
    print(
        "\n".join(
            justify(
                Description_Dict[Analysis_Name].split(),
                Width)))
    print("\n")
    line()


def description_control(
        Analysis_Name,
        Analysis_List,
        User_Input,
        Links_Dict,
        Vectors_Dict):
    """
    Control each analysis description.

    :param Analysis_Name: analysis name
    :type Analysis_Name: str
    :param Analysis_List: analysis list
    :type Analysis_List: list
    :param User_Input: user input
    :type User_Input: str
    :param Links_Dict: documents links
    :type Links_Dict: dict
    :param Vectors_Dict: test vectors
    :type Vectors_Dict: dict
    :return: None
    """
    if User_Input.upper() == "M":
        webbrowser.open_new(Links_Dict[Analysis_Name])
    elif User_Input.upper() == "T":
        line()
        print(Analysis_Name.replace("_", " ") + " Standard Test Vector\n")
        Test_Vector = Vectors_Dict[Analysis_Name]
        for i in Test_Vector.keys():
            print(i + " : " + str(Test_Vector[i]))
        print("\n")
        line()
        input_temp = input("Press any key to continue")
        del input_temp
        Analysis_List[Analysis_Name](
            InputMethod=Test_Vector, TestMode=True)
    else:
        Analysis_List[Analysis_Name]()


def filter_alpha(Input_Dict):
    """
    Filter alpha parameter.

    :param Input_Dict: input parameters dictionary
    :type Input_Dict : dict
    :return: modified dictionary
    """
    try:
        if Input_Dict["alpha"] > 1:
            Input_Dict["alpha"] = 1
            print("[Warning] Opem Automatically Set Alpha To Maximum Value (1) ")
        elif Input_Dict["alpha"] < 0:
            Input_Dict["alpha"] = 0
            print("[Warning] Opem Automatically Set Alpha To Maximum Value (0) ")
        return Input_Dict
    except Exception:
        return Input_Dict


def filter_range(IStart, IEnd, IStep):
    """
    Filter current range.

    :param IStart: current start point
    :type IStart: float
    :param IEnd: current end point
    :type IEnd: float
    :param IStep: current step
    :type IStep: float
    :return: filtered range as list
    """
    temp = None
    IStartO = IStart
    IEndO = IEnd
    IStepO = abs(IStep)
    if IStartO > IEndO:
        temp = IStartO
        IStartO = IEndO
        IEndO = temp
    return [IStartO, IEndO, IStepO]


def warning_check_1(Vcell, I_Warning, I, warning_flag):
    """
    Check Vcell is negative or not.

    :param Vcell: Vcell of FC voltage
    :type Vcell : float
    :param I_Warning: first I of negative range
    :type I_Warning : float
    :param I: test current
    :type I : float
    :param warning_flag: input warning flag
    :type warning_flag : bool
    :return:  update warning_flag and I_Warning [bool,float]
    """
    if not warning_flag:
        try:
            if Vcell < 0:
                return [True, I]
            return [False, I]
        except Exception:
            return [False, I]
    else:
        return [True, I_Warning]


def warning_check_2(Vcell, warning_flag):
    """
    Check Vcell is None or not.

    :param Vcell: Vcell of FC Voltage
    :type Vcell : float
    :param warning_flag: input warning flag
    :type warning_flag : bool
    :return:  update warning_flag as bool
    """
    if not warning_flag:
        if Vcell is None:
            return True
        return False
    return True


def warning_print(
        warning_flag_1,
        warning_flag_2,
        I_Warning,
        file,
        PrintMode):
    """
    Print warning message and write messages to HTML report.

    :param warning_flag_1: first warning message (Vcell <0)
    :type warning_flag_1 : bool
    :param warning_flag_2: second warning message (Vcell==None)
    :type warning_flag_2 : bool
    :param I_Warning: first I of negative range
    :type I_Warning : float
    :param file: html file object
    :type file : file object
    :param PrintMode : print mode control flag (True : print outputs)
    :type PrintMode: bool
    :return: None
    """
    if warning_flag_1:
        file.write(
            '<p style="color:red;font-size:20px;text-align:center;">' +
            Warning_Message_1 .format(
                str(I_Warning)) +
            '</p>\n')
        if PrintMode:
            print(Warning_Message_1.format(str(I_Warning)))
    if warning_flag_2:
        file.write(
            '<p style="color:red;font-size:20px;text-align:center;">' +
            Warning_Message_2 +
            '</p>\n')
        if PrintMode:
            print(Warning_Message_2)
