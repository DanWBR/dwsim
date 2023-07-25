# -*- coding: utf-8 -*-
"""OPEM profile."""
from opem.Static.Amphlett import Static_Analysis as Amphlett_Static_Analysis
Test_Vector = {
    "T": 343.15,
    "PH2": 1,
    "PO2": 1,
    "i-start": 0,
    "i-stop": 100,
    "i-step": 0.005,
    "A": 50.6,
    "l": 0.0178,
    "lambda": 23,
    "N": 1,
    "R": 0,
    "JMax": 1.5,
    "B": 0.016,
    "Name": "Test"}
Result = Amphlett_Static_Analysis(
    InputMethod=Test_Vector,
    TestMode=True,
    PrintMode=False,
    ReportMode=True)

from opem.Static.Chamberline_Kim import Static_Analysis as Chamberline_Kim_Static_Analysis
Test_Vector = {
    "A": 50.0,
    "E0": 0.982,
    "b": 0.0689,
    "R": 0.328,
    "m": 0.000125,
    "n": 9.45,
    "N": 1,
    "i-start": 1,
    "i-stop": 100,
    "i-step": 0.005,
    "Name": "Test"}
Result = Chamberline_Kim_Static_Analysis(
    InputMethod=Test_Vector,
    TestMode=True,
    PrintMode=False,
    ReportMode=True)


from opem.Static.Larminie_Dicks import Static_Analysis as Larminie_Dicks_Static_Analysis
Test_Vector = {
    "A": 0.0587,
    "E0": 1.178,
    "B": 0.0517,
    "RM": 0.0018,
    "i_0": 0.00654,
    "i_L": 100.0,
    "i_n": 0.23,
    "N": 23,
    "i-start": 0.1,
    "i-stop": 80,
    "i-step": 0.005,
    "Name": "Test"}
Result = Larminie_Dicks_Static_Analysis(
    InputMethod=Test_Vector,
    TestMode=True,
    PrintMode=False,
    ReportMode=True)


from opem.Dynamic.Padulles1 import Dynamic_Analysis as Padulles1_Dynamic_Analysis
Test_Vector = {
    "T": 343,
    "E0": 0.6,
    "N0": 88,
    "KO2": 0.0000211,
    "KH2": 0.0000422,
    "tH2": 3.37,
    "tO2": 6.74,
    "B": 0.04777,
    "C": 0.0136,
    "Rint": 0.00303,
    "rho": 1.168,
    "qH2": 0.0004,
    "i-start": 0,
    "i-stop": 100,
    "i-step": 0.005,
    "Name": "Test"}
Result = Padulles1_Dynamic_Analysis(
    InputMethod=Test_Vector,
    TestMode=True,
    PrintMode=False,
    ReportMode=True)


from opem.Dynamic.Padulles2 import Dynamic_Analysis as Padulles2_Dynamic_Analysis
Test_Vector = {
    "T": 343,
    "E0": 0.6,
    "N0": 5,
    "KO2": 0.0000211,
    "KH2": 0.0000422,
    "KH2O": 0.000007716,
    "tH2": 3.37,
    "tO2": 6.74,
    "tH2O": 18.418,
    "B": 0.04777,
    "C": 0.0136,
    "Rint": 0.00303,
    "rho": 1.168,
    "qH2": 0.0004,
    "i-start": 0.1,
    "i-stop": 100,
    "i-step": 0.005,
    "Name": "Test"}
Result = Padulles2_Dynamic_Analysis(
    InputMethod=Test_Vector,
    TestMode=True,
    PrintMode=False,
    ReportMode=True)


from opem.Dynamic.Padulles_Hauer import Dynamic_Analysis as Padulles_Hauer_Dynamic_Analysis
Test_Vector = {
    "T": 343,
    "E0": 0.6,
    "N0": 5,
    "KO2": 0.0000211,
    "KH2": 0.0000422,
    "KH2O": 0.000007716,
    "tH2": 3.37,
    "tO2": 6.74,
    "t1": 2,
    "t2": 2,
    "tH2O": 18.418,
    "B": 0.04777,
    "C": 0.0136,
    "Rint": 0.00303,
    "rho": 1.168,
    "qMethanol": 0.0002,
    "CV": 2,
    "i-start": 0.1,
    "i-stop": 100,
    "i-step": 0.005,
    "Name": "Test"}
Result = Padulles_Hauer_Dynamic_Analysis(
    InputMethod=Test_Vector,
    TestMode=True,
    PrintMode=False,
    ReportMode=True)


from opem.Dynamic.Padulles_Amphlett import Dynamic_Analysis as Padulles_Amphlett_Dynamic_Analysis
Test_Vector = {
    "A": 50.6,
    "l": 0.0178,
    "lambda": 23,
    "JMax": 1.5,
    "T": 343,
    "E0": 1.229,
    "N0": 5,
    "KO2": 0.0000211,
    "KH2": 0.0000422,
    "KH2O": 0.000007716,
    "tH2": 3.37,
    "tO2": 6.74,
    "t1": 2,
    "t2": 2,
    "tH2O": 18.418,
    "B": 0.016,
    "R": 0,
    "rho": 1.168,
    "qMethanol": 0.0002,
    "CV": 2,
    "i-start": 0.1,
    "i-stop": 73,
    "i-step": 0.005,
    "Name": "Test"}
Result = Padulles_Amphlett_Dynamic_Analysis(
    InputMethod=Test_Vector,
    TestMode=True,
    PrintMode=False,
    ReportMode=True)
