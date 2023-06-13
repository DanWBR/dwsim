# -*- coding: utf-8 -*-
'''
>>> from opem.Functions import *
>>> data=[i for i in range(100)]
>>> integrate(data,1)
4867.666666666666
>>> data[0]=None
>>> integrate(data,1)
>>> linear_plot([1,2,3],[2,4,6])
[[2.0, 4.0, 6.0], 0.0, 2.0]
>>> linear_plot([1,2,3],[2,4,None])
[[2.0, 4.0, None], 0.0, 2.0]
>>> estimate_coef([0,0,0], [0,0,0])
[0, 0]
>>> get_precision(2)
0
>>> get_precision(2.55)
2
>>> warning_check_1(-0.2, 10, 20, True)
[True, 10]
>>> warning_check_1(-0.2, 10, 20, False)
[True, 20]
>>> rounder(22.2223, digit=2)
22.22
>>> rounder(22, digit=2)
22
>>> rounder("test")
'test'
>>> isfloat("2")
True
>>> isfloat("2.02")
True
>>> isfloat('ss')
False
>>> filter_lambda({"lambda":24})
[Warning] Opem Automatically Set Lambda To Maximum Value (23)
{'lambda': 23}
>>> filter_alpha({"alpha":2})
[Warning] Opem Automatically Set Alpha To Maximum Value (1)
{'alpha': 1}
>>> filter_lambda({"lambda":13})
[Warning] Opem Automatically Set Lambda To Minimum Value (14)
{'lambda': 23}
>>> filter_alpha({"alpha":-0.1})
[Warning] Opem Automatically Set Alpha To Maximum Value (0)
{'alpha': 0}
>>> filter_range(0,100,0.1)
[0, 100, 0.1]
>>> filter_range(500,0,0.1)
[0, 500, 0.1]
>>> filter_range(500,0,-0.1)
[0, 500, 0.1]
>>> Input_dict=Get_Input({"T": "Cell Operation Temperature [K]", "PH2": "Partial Pressure [atm]", "PO2": "Partial Pressure [atm]"},input_item=input_test)
>>> Input_keys=list(Input_dict.keys())
>>> Input_keys.sort()
>>> print(Input_keys)
['Name', 'PH2', 'PO2', 'T']
>>> description_print("Model1",{"Model1":"Test"})
###########
Model1 :
<BLANKLINE>
<BLANKLINE>
<BLANKLINE>
<BLANKLINE>
Test
<BLANKLINE>
<BLANKLINE>
###########
>>> check_update(1.3)
>>> check_update(0.1)
###########
New Version (1.3) Is Available!
Website : http://www.ecsim.ir/opem
###########

'''
