' based on https://www.chemeo.com/api/v1/
Class SearchResponse
    Public Property Comps As IEnumerable(Of Compound)

End Class


Class Compound
    Public Property Id As String
    Public Property Cas As String
    Public Property Compound As String

End Class

Public Class Af
    Public Property v As Double
    Public Property s As String
End Class

Public Class Affp
    Public Property v As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Basg
    Public Property v As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Chl
    Public Property v As Double
    Public Property e As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Dm
    Public Property v As Double
    Public Property s As String
End Class

Public Class Gf
    Public Property v As Double
    Public Property s As String
End Class

Public Class Gr
    Public Property v As Double
    Public Property e As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Hf
    Public Property v As Double
    Public Property s As String
    Public Property e As Double?
    Public Property c As String
    Public Property r As String
End Class

Public Class Hfl
    Public Property v As Double
    Public Property e As Double
    Public Property s As String
    Public Property r As String
    Public Property c As String
End Class

Public Class Hfu
    Public Property v As Double
    Public Property s As String
    Public Property calc As Boolean
End Class

Public Class Hreac
    Public Property v As Double
    Public Property e As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Hvap
    Public Property v As Double
    Public Property s As String
    Public Property r As String
    Public Property e As Double?
    Public Property c As String
    Public Property o As Boolean?
End Class

Public Class Ie
    Public Property v As Double
    Public Property e As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
    Public Property o As Boolean?
End Class

Public Class Log10ws
    Public Property v As Double
    Public Property s As String
End Class

Public Class Logp
    Public Property v As Double
    Public Property s As String
    Public Property calc As Boolean
End Class

Public Class Mcvol
    Public Property v As Double
    Public Property s As String
    Public Property calc As Boolean
End Class

Public Class Nfpaf
    Public Property v As Double
    Public Property s As String
End Class

Public Class Nfpah
    Public Property v As Double
    Public Property s As String
End Class

Public Class Pc
    Public Property v As Double
    Public Property s As String
    Public Property e As Double?
    Public Property c As String
    Public Property r As String
End Class

Public Class Rhoc
    Public Property v As Double
    Public Property e As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Rinpol
    Public Property v As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
    Public Property o As Boolean?
End Class

Public Class Ripol
    Public Property v As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
    Public Property o As Boolean?
End Class

Public Class Sr
    Public Property v As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Tb
    Public Property v As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
    Public Property e As Double?
    Public Property o As Boolean?
End Class

Public Class Tc
    Public Property v As Double
    Public Property s As String
    Public Property r As String
    Public Property e As Double?
    Public Property c As String
End Class

Public Class Tf
    Public Property v As Double
    Public Property s As String
    Public Property e As Double?
    Public Property c As String
    Public Property r As String
End Class

Public Class Tt
    Public Property v As Double
    Public Property e As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Vc
    Public Property v As Double
    Public Property s As String
End Class

Public Class Zc
    Public Property v As Double
    Public Property s As String
End Class

Public Class Zra
    Public Property v As Double
    Public Property s As String
End Class

Public Class FixedProps
    Public Property af As Af()
    Public Property affp As Affp()
    Public Property basg As Basg()
    Public Property chl As Chl()
    Public Property dm As Dm()
    Public Property gf As Gf()
    Public Property gr As Gr()
    Public Property hf As Hf()
    Public Property hfl As Hfl()
    Public Property hfus As Hfu()
    Public Property hreac As Hreac()
    Public Property hvap As Hvap()
    Public Property ie As Ie()
    Public Property log10ws As Log10ws()
    Public Property logp As Logp()
    Public Property mcvol As Mcvol()
    Public Property nfpaf As Nfpaf()
    Public Property nfpah As Nfpah()
    Public Property pc As Pc()
    Public Property rhoc As Rhoc()
    Public Property rinpol As Rinpol()
    Public Property ripol As Ripol()
    Public Property sr As Sr()
    Public Property tb As Tb()
    Public Property tc As Tc()
    Public Property tf As Tf()
    Public Property tt As Tt()
    Public Property vc As Vc()
    Public Property zc As Zc()
    Public Property zra As Zra()
End Class

Public Class Cpg
    Public Property v As Double
    Public Property t As Double
    Public Property s As String
    Public Property calc As Boolean
End Class

Public Class Hfust
    Public Property v As Double
    Public Property t As Double
    Public Property s As String
    Public Property c As String
    Public Property r As String
End Class

Public Class Hvapt
    Public Property v As Double
    Public Property t As Double
    Public Property s As String
    Public Property r As String
    Public Property c As String
    Public Property e As Double?
End Class

Public Class Pvap
    Public Property v As Double
    Public Property t As Double
    Public Property s As String
End Class

Public Class Rfi
    Public Property v As Double
    Public Property t As Double
    Public Property s As String
    Public Property c As String
End Class

Public Class Rhol
    Public Property v As Double
    Public Property t As Double
    Public Property s As String
End Class

Public Class Srf
    Public Property v As Double
    Public Property t As Double
    Public Property s As String
End Class

Public Class TpdepProps
    Public Property cpg As Cpg()
    Public Property hfust As Hfust()
    Public Property hvapt As Hvapt()
    Public Property pvap As Pvap()
    Public Property rfi As Rfi()
    Public Property rhol As Rhol()
    Public Property srf As Srf()
End Class

Public Class Joback
    Public Property db As String
    Public Property url As String
    Public Property title As String
    Public Property desc As String
End Class

Public Class KdbPure
    Public Property db As String
    Public Property url As String
End Class

Public Class Log10wsAqueousdataset002Xlsx
    Public Property db As String
    Public Property url As String
End Class

Public Class Log10wsCi034243xsi20040112053635Txt
    Public Property db As String
    Public Property url As String
End Class

Public Class Mcgowan
    Public Property db As String
    Public Property url As String
    Public Property title As String
    Public Property desc As String
End Class

Public Class NistWebbook
    Public Property db As String
    Public Property url As String
    Public Property title As String
    Public Property desc As String
End Class

Public Class YawsAntoinePvap
    Public Property db As String
    Public Property url As String
    Public Property title As String
    Public Property desc As String
End Class

Public Class Prop
    Public Property v As Double
    Public Property t As Double
    Public Property calc As Boolean
End Class

Public Class Comp
    Public Property id As String
    Public Property rev As Integer
    Public Property compound As String
    Public Property other_names As String()
    Public Property cas As String
    Public Property inchi As String
    Public Property inchikey As String
    Public Property formula As String
    Public Property type As String
    Public Property mw As Double
    Public Property smiles As String
    Public Property drawing As String
    Public Property mol2d As String
    Public Property n_mixtures As Integer
    Public Property fixed_props As FixedProps
    Public Property tpdep_props As TpdepProps

End Class

Public Class CompoundData
    Public Property comp As Comp
End Class


