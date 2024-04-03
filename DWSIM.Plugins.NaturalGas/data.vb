'Natural Gas Properties Plugin for DWSIM
'Copyright 2010 Daniel Wagner

Imports FileHelpers

<DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> Public Class datamass

    'this class is equivalent to a row in the mass heating value data file.

    'values are in kJ/kg

    <FieldNullValue(GetType(System.String), "")> Public name As String
    <FieldNullValue(GetType(System.String), "")> Public dbname As String
    <FieldNullValue(GetType(System.Double), "0")> Public sup25 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf25 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup20 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf20 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup15 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf15 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup0 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf0 As Double

End Class

<DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> Public Class datavol

    'values are in MJ/m3

    'this class is equivalent to a row in the volumetric heating value data file.

    'Sup1515	Inf1515	Sup00	Inf00	Sup150	Inf150	Sup250	Inf250	Sup2020	Inf2020	Sup2520	Inf2520

    <FieldNullValue(GetType(System.String), "")> Public name As String
    <FieldNullValue(GetType(System.String), "")> Public dbname As String
    <FieldNullValue(GetType(System.Double), "0")> Public sup1515 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf1515 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup00 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf00 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup150 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf150 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup250 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf250 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup2020 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf2020 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public sup2520 As Double
    <FieldNullValue(GetType(System.Double), "0")> Public inf2520 As Double

End Class
