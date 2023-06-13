# -*- coding: utf-8 -*-
"""OPEM main."""
import sys
from opem.Static.Amphlett import Static_Analysis as Amphlett_Analysis
from opem.Static.Larminie_Dicks import Static_Analysis as Larminiee_Analysis
from opem.Static.Chamberline_Kim import Static_Analysis as Chamberline_Kim_Analysis
from opem.Dynamic.Padulles1 import Dynamic_Analysis as Padulles1_Analysis
from opem.Dynamic.Padulles2 import Dynamic_Analysis as Padulles2_Analysis
from opem.Dynamic.Padulles_Hauer import Dynamic_Analysis as Padulles_Hauer_Analysis
from opem.Dynamic.Padulles_Amphlett import Dynamic_Analysis as Padulles_Amphlett_Analysis
from opem.Dynamic.Chakraborty import Dynamic_Analysis as Chakraborty_Analysis
from art import tprint
from opem.Params import Version, Description_Menu, Description_Links, Vectors, Mode_Menu
from opem.Functions import check_update, description_print, description_control


if __name__ == "__main__":
    ARGS = sys.argv
    ARGSUP = list(map(str.upper, ARGS))
    ANALYSISLIST = {
        "Amphlett_Analysis (Static)": Amphlett_Analysis,
        "Larminiee_Analysis (Static)": Larminiee_Analysis,
        "Chamberline_Kim_Analysis (Static)": Chamberline_Kim_Analysis,
        "Padulles_Analysis I (Dynamic)": Padulles1_Analysis,
        "Padulles_Analysis II (Dynamic)": Padulles2_Analysis,
        "Padulles_Hauer Analysis (Dynamic)": Padulles_Hauer_Analysis,
        "Padulles_Amphlett Analysis (Dynamic)": Padulles_Amphlett_Analysis,
        "Chakraborty_Analysis (Dynamic)": Chakraborty_Analysis}
    MENU = {
        "(Static)  Amphlett Analysis": "Amphlett_Analysis (Static)",
        "(Static)  Larminiee Analysis": "Larminiee_Analysis (Static)",
        "(Static)  Chamberline Kim Analysis": "Chamberline_Kim_Analysis (Static)",
        "(Dynamic) Padulles Analysis I ": "Padulles_Analysis I (Dynamic)",
        "(Dynamic) Padulles Analysis II": "Padulles_Analysis II (Dynamic)",
        "(Dynamic) Padulles Hauer Analysis": "Padulles_Hauer Analysis (Dynamic)",
        "(Dynamic) Padulles Amphlett Analysis": "Padulles_Amphlett Analysis (Dynamic)",
        "(Dynamic) Chakraborty Analysis": "Chakraborty_Analysis (Dynamic)"}
    MENUKEYS = sorted(MENU.keys())
    EXITFLAG = False
    tprint("OPEM")
    tprint("v" + str(Version))
    print(Description_Menu["Links"])
    description_print("Overview", Description_Menu)
    if "TEST" in ARGSUP:
        sys.exit(0)
    check_update(Version)
    while not EXITFLAG:
        tprint("Models", font="larry3d")
        for i, item in enumerate(MENUKEYS):
            print(str(i + 1) + "-" + item)
        try:
            ANALYSISINDEX = int(input(("\nPlease select a model : ")))
        except Exception:
            ANALYSISINDEX = -1
        if ANALYSISINDEX - 1 in range(len(MENUKEYS)):
            ANALYSISNAME = MENU[MENUKEYS[ANALYSISINDEX - 1]]
            description_print(ANALYSISNAME, Description_Menu)
            USERINPUT = input(Mode_Menu)
            description_control(
                Analysis_Name=ANALYSISNAME,
                Analysis_List=ANALYSISLIST,
                User_Input=USERINPUT,
                Links_Dict=Description_Links,
                Vectors_Dict=Vectors)
            INPUTINDEX = input(
                "Press [R] to restart OPEM or any other key to exit.")
            if INPUTINDEX.upper() != "R":
                EXITFLAG = True
