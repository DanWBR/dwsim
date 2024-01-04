Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class IAPWS_IF97

        '  water97_v13: A collection of Visual Basic functions
        '  for calculating properties of water and steam.
        '
        '  Source: IAPWS-IF97. For details see
        '  http://www.cheresources.com/iapwsif97.shtml
        '
        '  Version 1.3, 02/10/02:   documentation updated
        '
        '  Version 1.2, 02/06/01:   starting value for iteration in densreg3 for
        '                           supercritical temperatures changed from 500 to 600
        '
        '
        '  Version 1.1, 01/29/01:   mistake in calculation of partial derivatives
        '                           for thermal conductivity corrected
        '
        '  (c) B. Spang, Hamburg, Germany, 2000 - 2002
        '      E-Mail: b.spang@hamburg.de
        '
        '

#Region "Variable declarations"
        Public Const rgas_water As Double = 461.526 'gas constant in J/(kg K)
        Public Const tc_water As Double = 647.096 'critical temperature in K
        Public Const pc_water As Double = 220.64 'critical pressure in bar
        Public Const dc_water As Double = 322.0# 'critical density in kg/m**3
        Public Const tmax As Double = 2000.0

        Private ireg1(34) As Short

        Private jreg1(34) As Short

        Private nreg1(34) As Double

        Private j0reg2(9) As Short

        Private n0reg2(9) As Double

        Private ireg2(43) As Short

        Private jreg2(43) As Short

        Private nreg2(43) As Double

        Private ireg3(40) As Short

        Private jreg3(40) As Short

        Private nreg3(40) As Double

        Private nreg4(10) As Double

        Private nbound(5) As Double
        Private n0visc(3) As Double

        Private ivisc(19) As Short

        Private jvisc(19) As Short

        Private nvisc(19) As Double
        Private n0thcon(3) As Double
        Private nthcon(4, 5) As Double
#End Region

#Region "internal functions"

        Sub InitFieldsreg1()
            '
            '  Initialize coefficients and exponents for region 1
            '
            ireg1(1) = 0
            ireg1(2) = 0
            ireg1(3) = 0
            ireg1(4) = 0
            ireg1(5) = 0
            ireg1(6) = 0
            ireg1(7) = 0
            ireg1(8) = 0
            ireg1(9) = 1
            ireg1(10) = 1
            ireg1(11) = 1
            ireg1(12) = 1
            ireg1(13) = 1
            ireg1(14) = 1
            ireg1(15) = 2
            ireg1(16) = 2
            ireg1(17) = 2
            ireg1(18) = 2
            ireg1(19) = 2
            ireg1(20) = 3
            ireg1(21) = 3
            ireg1(22) = 3
            ireg1(23) = 4
            ireg1(24) = 4
            ireg1(25) = 4
            ireg1(26) = 5
            ireg1(27) = 8
            ireg1(28) = 8
            ireg1(29) = 21
            ireg1(30) = 23
            ireg1(31) = 29
            ireg1(32) = 30
            ireg1(33) = 31
            ireg1(34) = 32
            '
            jreg1(1) = -2
            jreg1(2) = -1
            jreg1(3) = 0
            jreg1(4) = 1
            jreg1(5) = 2
            jreg1(6) = 3
            jreg1(7) = 4
            jreg1(8) = 5
            jreg1(9) = -9
            jreg1(10) = -7
            jreg1(11) = -1
            jreg1(12) = 0
            jreg1(13) = 1
            jreg1(14) = 3
            jreg1(15) = -3
            jreg1(16) = 0
            jreg1(17) = 1
            jreg1(18) = 3
            jreg1(19) = 17
            jreg1(20) = -4
            jreg1(21) = 0
            jreg1(22) = 6
            jreg1(23) = -5
            jreg1(24) = -2
            jreg1(25) = 10
            jreg1(26) = -8
            jreg1(27) = -11
            jreg1(28) = -6
            jreg1(29) = -29
            jreg1(30) = -31
            jreg1(31) = -38
            jreg1(32) = -39
            jreg1(33) = -40
            jreg1(34) = -41
            '
            nreg1(1) = 0.14632971213167
            nreg1(2) = -0.84548187169114
            nreg1(3) = -3.756360367204
            nreg1(4) = 3.3855169168385
            nreg1(5) = -0.95791963387872
            nreg1(6) = 0.15772038513228
            nreg1(7) = -0.016616417199501
            nreg1(8) = 0.00081214629983568
            nreg1(9) = 0.00028319080123804
            nreg1(10) = -0.00060706301565874
            nreg1(11) = -0.018990068218419
            nreg1(12) = -0.032529748770505
            nreg1(13) = -0.021841717175414
            nreg1(14) = -0.00005283835796993
            nreg1(15) = -0.00047184321073267
            nreg1(16) = -0.00030001780793026
            nreg1(17) = 0.000047661393906987
            nreg1(18) = -0.0000044141845330846
            nreg1(19) = -0.00000000000000072694996297594
            nreg1(20) = -0.000031679644845054
            nreg1(21) = -0.0000028270797985312
            nreg1(22) = -0.00000000085205128120103
            nreg1(23) = -0.0000022425281908
            nreg1(24) = -0.00000065171222895601
            nreg1(25) = -0.00000000000014341729937924
            nreg1(26) = -0.00000040516996860117
            nreg1(27) = -0.0000000012734301741641
            nreg1(28) = -0.00000000017424871230634
            nreg1(29) = -6.8762131295531E-19
            nreg1(30) = 1.4478307828521E-20
            nreg1(31) = 2.6335781662795E-23
            nreg1(32) = -1.1947622640071E-23
            nreg1(33) = 1.8228094581404E-24
            nreg1(34) = -9.3537087292458E-26
            ''
        End Sub

        Sub InitFieldsreg2()
            '
            '  Initialize coefficients and exponents for region 2
            '
            j0reg2(1) = 0
            j0reg2(2) = 1
            j0reg2(3) = -5
            j0reg2(4) = -4
            j0reg2(5) = -3
            j0reg2(6) = -2
            j0reg2(7) = -1
            j0reg2(8) = 2
            j0reg2(9) = 3
            '
            n0reg2(1) = -9.6927686500217
            n0reg2(2) = 10.086655968018
            n0reg2(3) = -0.005608791128302
            n0reg2(4) = 0.071452738081455
            n0reg2(5) = -0.40710498223928
            n0reg2(6) = 1.4240819171444
            n0reg2(7) = -4.383951131945
            n0reg2(8) = -0.28408632460772
            n0reg2(9) = 0.021268463753307
            '
            ireg2(1) = 1
            ireg2(2) = 1
            ireg2(3) = 1
            ireg2(4) = 1
            ireg2(5) = 1
            ireg2(6) = 2
            ireg2(7) = 2
            ireg2(8) = 2
            ireg2(9) = 2
            ireg2(10) = 2
            ireg2(11) = 3
            ireg2(12) = 3
            ireg2(13) = 3
            ireg2(14) = 3
            ireg2(15) = 3
            ireg2(16) = 4
            ireg2(17) = 4
            ireg2(18) = 4
            ireg2(19) = 5
            ireg2(20) = 6
            ireg2(21) = 6
            ireg2(22) = 6
            ireg2(23) = 7
            ireg2(24) = 7
            ireg2(25) = 7
            ireg2(26) = 8
            ireg2(27) = 8
            ireg2(28) = 9
            ireg2(29) = 10
            ireg2(30) = 10
            ireg2(31) = 10
            ireg2(32) = 16
            ireg2(33) = 16
            ireg2(34) = 18
            ireg2(35) = 20
            ireg2(36) = 20
            ireg2(37) = 20
            ireg2(38) = 21
            ireg2(39) = 22
            ireg2(40) = 23
            ireg2(41) = 24
            ireg2(42) = 24
            ireg2(43) = 24
            '
            jreg2(1) = 0
            jreg2(2) = 1
            jreg2(3) = 2
            jreg2(4) = 3
            jreg2(5) = 6
            jreg2(6) = 1
            jreg2(7) = 2
            jreg2(8) = 4
            jreg2(9) = 7
            jreg2(10) = 36
            jreg2(11) = 0
            jreg2(12) = 1
            jreg2(13) = 3
            jreg2(14) = 6
            jreg2(15) = 35
            jreg2(16) = 1
            jreg2(17) = 2
            jreg2(18) = 3
            jreg2(19) = 7
            jreg2(20) = 3
            jreg2(21) = 16
            jreg2(22) = 35
            jreg2(23) = 0
            jreg2(24) = 11
            jreg2(25) = 25
            jreg2(26) = 8
            jreg2(27) = 36
            jreg2(28) = 13
            jreg2(29) = 4
            jreg2(30) = 10
            jreg2(31) = 14
            jreg2(32) = 29
            jreg2(33) = 50
            jreg2(34) = 57
            jreg2(35) = 20
            jreg2(36) = 35
            jreg2(37) = 48
            jreg2(38) = 21
            jreg2(39) = 53
            jreg2(40) = 39
            jreg2(41) = 26
            jreg2(42) = 40
            jreg2(43) = 58
            '
            nreg2(1) = -0.0017731742473213
            nreg2(2) = -0.017834862292358
            nreg2(3) = -0.045996013696365
            nreg2(4) = -0.057581259083432
            nreg2(5) = -0.05032527872793
            nreg2(6) = -0.000033032641670203
            nreg2(7) = -0.00018948987516315
            nreg2(8) = -0.0039392777243355
            nreg2(9) = -0.043797295650573
            nreg2(10) = -0.000026674547914087
            nreg2(11) = 0.000000020481737692309
            nreg2(12) = 0.00000043870667284435
            nreg2(13) = -0.00003227767723857
            nreg2(14) = -0.0015033924542148
            nreg2(15) = -0.040668253562649
            nreg2(16) = -0.00000000078847309559367
            nreg2(17) = 0.000000012790717852285
            nreg2(18) = 0.00000048225372718507
            nreg2(19) = 0.0000022922076337661
            nreg2(20) = -0.000000000016714766451061
            nreg2(21) = -0.0021171472321355
            nreg2(22) = -23.895741934104
            nreg2(23) = -5.905956432427E-18
            nreg2(24) = -0.0000012621808899101
            nreg2(25) = -0.038946842435739
            nreg2(26) = 0.000000000011256211360459
            nreg2(27) = -8.2311340897998
            nreg2(28) = 0.000000019809712802088
            nreg2(29) = 1.0406965210174E-19
            nreg2(30) = -0.00000000000010234747095929
            nreg2(31) = -0.0000000010018179379511
            nreg2(32) = -0.000000000080882908646985
            nreg2(33) = 0.10693031879409
            nreg2(34) = -0.33662250574171
            nreg2(35) = 8.9185845355421E-25
            nreg2(36) = 0.00000000000030629316876232
            nreg2(37) = -0.0000042002467698208
            nreg2(38) = -5.9056029685639E-26
            nreg2(39) = 0.0000037826947613457
            nreg2(40) = -0.0000000000000012768608934681
            nreg2(41) = 7.3087610595061E-29
            nreg2(42) = 5.5414715350778E-17
            nreg2(43) = -0.0000009436970724121
            ''
        End Sub

        Sub InitFieldsreg3()
            '
            '  Initialize coefficients and exponents for region 3
            '
            ireg3(1) = 0
            ireg3(2) = 0
            ireg3(3) = 0
            ireg3(4) = 0
            ireg3(5) = 0
            ireg3(6) = 0
            ireg3(7) = 0
            ireg3(8) = 0
            ireg3(9) = 1
            ireg3(10) = 1
            ireg3(11) = 1
            ireg3(12) = 1
            ireg3(13) = 2
            ireg3(14) = 2
            ireg3(15) = 2
            ireg3(16) = 2
            ireg3(17) = 2
            ireg3(18) = 2
            ireg3(19) = 3
            ireg3(20) = 3
            ireg3(21) = 3
            ireg3(22) = 3
            ireg3(23) = 3
            ireg3(24) = 4
            ireg3(25) = 4
            ireg3(26) = 4
            ireg3(27) = 4
            ireg3(28) = 5
            ireg3(29) = 5
            ireg3(30) = 5
            ireg3(31) = 6
            ireg3(32) = 6
            ireg3(33) = 6
            ireg3(34) = 7
            ireg3(35) = 8
            ireg3(36) = 9
            ireg3(37) = 9
            ireg3(38) = 10
            ireg3(39) = 10
            ireg3(40) = 11
            '
            jreg3(1) = 0
            jreg3(2) = 0
            jreg3(3) = 1
            jreg3(4) = 2
            jreg3(5) = 7
            jreg3(6) = 10
            jreg3(7) = 12
            jreg3(8) = 23
            jreg3(9) = 2
            jreg3(10) = 6
            jreg3(11) = 15
            jreg3(12) = 17
            jreg3(13) = 0
            jreg3(14) = 2
            jreg3(15) = 6
            jreg3(16) = 7
            jreg3(17) = 22
            jreg3(18) = 26
            jreg3(19) = 0
            jreg3(20) = 2
            jreg3(21) = 4
            jreg3(22) = 16
            jreg3(23) = 26
            jreg3(24) = 0
            jreg3(25) = 2
            jreg3(26) = 4
            jreg3(27) = 26
            jreg3(28) = 1
            jreg3(29) = 3
            jreg3(30) = 26
            jreg3(31) = 0
            jreg3(32) = 2
            jreg3(33) = 26
            jreg3(34) = 2
            jreg3(35) = 26
            jreg3(36) = 2
            jreg3(37) = 26
            jreg3(38) = 0
            jreg3(39) = 1
            jreg3(40) = 26
            '
            nreg3(1) = 1.0658070028513
            nreg3(2) = -15.732845290239
            nreg3(3) = 20.944396974307
            nreg3(4) = -7.6867707878716
            nreg3(5) = 2.6185947787954
            nreg3(6) = -2.808078114862
            nreg3(7) = 1.2053369696517
            nreg3(8) = -0.0084566812812502
            nreg3(9) = -1.2654315477714
            nreg3(10) = -1.1524407806681
            nreg3(11) = 0.88521043984318
            nreg3(12) = -0.64207765181607
            nreg3(13) = 0.38493460186671
            nreg3(14) = -0.85214708824206
            nreg3(15) = 4.8972281541877
            nreg3(16) = -3.0502617256965
            nreg3(17) = 0.039420536879154
            nreg3(18) = 0.12558408424308
            nreg3(19) = -0.2799932969871
            nreg3(20) = 1.389979956946
            nreg3(21) = -2.018991502357
            nreg3(22) = -0.0082147637173963
            nreg3(23) = -0.47596035734923
            nreg3(24) = 0.0439840744735
            nreg3(25) = -0.44476435428739
            nreg3(26) = 0.90572070719733
            nreg3(27) = 0.70522450087967
            nreg3(28) = 0.10770512626332
            nreg3(29) = -0.32913623258954
            nreg3(30) = -0.50871062041158
            nreg3(31) = -0.022175400873096
            nreg3(32) = 0.094260751665092
            nreg3(33) = 0.16436278447961
            nreg3(34) = -0.013503372241348
            nreg3(35) = -0.014834345352472
            nreg3(36) = 0.00057922953628084
            nreg3(37) = 0.0032308904703711
            nreg3(38) = 0.000080964802996215
            nreg3(39) = -0.00016557679795037
            nreg3(40) = -0.000044923899061815
            ''
        End Sub

        Sub InitFieldsreg4()
            '
            '  Initialize coefficients for region 4
            '
            nreg4(1) = 1167.0521452767
            nreg4(2) = -724213.16703206
            nreg4(3) = -17.073846940092
            nreg4(4) = 12020.82470247
            nreg4(5) = -3232555.0322333
            nreg4(6) = 14.91510861353
            nreg4(7) = -4823.2657361591
            nreg4(8) = 405113.40542057
            nreg4(9) = -0.23855557567849
            nreg4(10) = 650.17534844798
            ''
        End Sub

        Sub InitFieldsbound()
            '
            '  Initialize coefficients for boundary equation
            '
            nbound(1) = 348.05185628969
            nbound(2) = -1.1671859879975
            nbound(3) = 0.0010192970039326
            nbound(4) = 572.54459862746
            nbound(5) = 13.91883977887
            ''
        End Sub

        Sub InitFieldsvisc()
            '
            '  Initialize coefficients and exponents for viscosity
            '
            n0visc(0) = 1.0#
            n0visc(1) = 0.978197
            n0visc(2) = 0.579829
            n0visc(3) = -0.202354
            '
            ivisc(1) = 0
            ivisc(2) = 0
            ivisc(3) = 0
            ivisc(4) = 0
            ivisc(5) = 1
            ivisc(6) = 1
            ivisc(7) = 1
            ivisc(8) = 1
            ivisc(9) = 2
            ivisc(10) = 2
            ivisc(11) = 2
            ivisc(12) = 3
            ivisc(13) = 3
            ivisc(14) = 3
            ivisc(15) = 3
            ivisc(16) = 4
            ivisc(17) = 4
            ivisc(18) = 5
            ivisc(19) = 6
            '
            jvisc(1) = 0
            jvisc(2) = 1
            jvisc(3) = 4
            jvisc(4) = 5
            jvisc(5) = 0
            jvisc(6) = 1
            jvisc(7) = 2
            jvisc(8) = 3
            jvisc(9) = 0
            jvisc(10) = 1
            jvisc(11) = 2
            jvisc(12) = 0
            jvisc(13) = 1
            jvisc(14) = 2
            jvisc(15) = 3
            jvisc(16) = 0
            jvisc(17) = 3
            jvisc(18) = 1
            jvisc(19) = 3
            '
            nvisc(1) = 0.5132047
            nvisc(2) = 0.3205656
            nvisc(3) = -0.7782567
            nvisc(4) = 0.1885447
            nvisc(5) = 0.2151778
            nvisc(6) = 0.7317883
            nvisc(7) = 1.241044
            nvisc(8) = 1.476783
            nvisc(9) = -0.2818107
            nvisc(10) = -1.070786
            nvisc(11) = -1.263184
            nvisc(12) = 0.1778064
            nvisc(13) = 0.460504
            nvisc(14) = 0.2340379
            nvisc(15) = -0.4924179
            nvisc(16) = -0.0417661
            nvisc(17) = 0.1600435
            nvisc(18) = -0.01578386
            nvisc(19) = -0.003629481
            ''
        End Sub

        Sub InitFieldsthcon()
            '
            '  Initialize coefficients and exponents for thermal conductivity
            '
            n0thcon(0) = 1.0#
            n0thcon(1) = 6.978267
            n0thcon(2) = 2.599096
            n0thcon(3) = -0.998254
            '
            nthcon(0, 0) = 1.3293046
            nthcon(0, 1) = -0.40452437
            nthcon(0, 2) = 0.2440949
            nthcon(0, 3) = 0.018660751
            nthcon(0, 4) = -0.12961068
            nthcon(0, 5) = 0.044809953
            nthcon(1, 0) = 1.7018363
            nthcon(1, 1) = -2.2156845
            nthcon(1, 2) = 1.6511057
            nthcon(1, 3) = -0.76736002
            nthcon(1, 4) = 0.37283344
            nthcon(1, 5) = -0.1120316
            nthcon(2, 0) = 5.2246158
            nthcon(2, 1) = -10.124111
            nthcon(2, 2) = 4.9874687
            nthcon(2, 3) = -0.27297694
            nthcon(2, 4) = -0.43083393
            nthcon(2, 5) = 0.13333849
            nthcon(3, 0) = 8.7127675
            nthcon(3, 1) = -9.5000611
            nthcon(3, 2) = 4.3786606
            nthcon(3, 3) = -0.91783782
            nthcon(3, 4) = 0.0#
            nthcon(3, 5) = 0.0#
            nthcon(4, 0) = -1.8525999
            nthcon(4, 1) = 0.9340469
            nthcon(4, 2) = 0.0#
            nthcon(4, 3) = 0.0#
            nthcon(4, 4) = 0.0#
            nthcon(4, 5) = 0.0#
            ''
        End Sub

        Private Function gammareg1(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Fundamental equation for region 1
            '
            Call InitFieldsreg1()

            gammareg1 = 0
            For i = 1 To 34




                gammareg1 = gammareg1 + nreg1(i) * (7.1 - pi) ^ ireg1(i) * (tau - 1.222) ^ jreg1(i)
            Next i
            ''
        End Function

        Private Function gammapireg1(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' First derivative of fundamental equation in pi for region 1
            '
            Call InitFieldsreg1()

            gammapireg1 = 0
            For i = 1 To 34




                gammapireg1 = gammapireg1 - nreg1(i) * ireg1(i) * (7.1 - pi) ^ (ireg1(i) - 1) * (tau - 1.222) ^ jreg1(i)
            Next i
            ''
        End Function

        Private Function gammapipireg1(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Second derivative of fundamental equation in pi for region 1
            '
            Call InitFieldsreg1()

            gammapipireg1 = 0
            For i = 1 To 34




                gammapipireg1 = gammapipireg1 + nreg1(i) * ireg1(i) * (ireg1(i) - 1) * (7.1 - pi) ^ (ireg1(i) - 2) * (tau - 1.222) ^ jreg1(i)
            Next i
            ''
        End Function

        Private Function gammataureg1(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' First derivative of fundamental equation in tau for region 1
            '
            Call InitFieldsreg1()

            gammataureg1 = 0
            For i = 1 To 34




                gammataureg1 = gammataureg1 + nreg1(i) * (7.1 - pi) ^ ireg1(i) * jreg1(i) * (tau - 1.222) ^ (jreg1(i) - 1)
            Next i
            ''
        End Function

        Private Function gammatautaureg1(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Second derivative of fundamental equation in tau for region 1
            '
            Call InitFieldsreg1()

            gammatautaureg1 = 0
            For i = 1 To 34




                gammatautaureg1 = gammatautaureg1 + nreg1(i) * (7.1 - pi) ^ ireg1(i) * jreg1(i) * (jreg1(i) - 1) * (tau - 1.222) ^ (jreg1(i) - 2)
            Next i
            ''
        End Function '

        Private Function gammapitaureg1(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Second derivative of fundamental equation in pi and tau for region 1
            '
            Call InitFieldsreg1()

            gammapitaureg1 = 0
            For i = 1 To 34




                gammapitaureg1 = gammapitaureg1 - nreg1(i) * ireg1(i) * (7.1 - pi) ^ (ireg1(i) - 1) * jreg1(i) * (tau - 1.222) ^ (jreg1(i) - 1)
            Next i
            ''
        End Function

        Private Function gamma0reg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Ideal-gas part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gamma0reg2 = System.Math.Log(pi)
            For i = 1 To 9



                gamma0reg2 = gamma0reg2 + n0reg2(i) * tau ^ j0reg2(i)
            Next i
            ''
        End Function

        Private Function gamma0pireg2(ByRef tau As Object, ByRef pi As Object) As Object
            '
            ' First derivative in pi of ideal-gas part of fundamental equation for region 2
            '


            gamma0pireg2 = 1 / pi
            ''
        End Function

        Private Function gamma0pipireg2(ByRef tau As Object, ByRef pi As Object) As Object
            '
            ' Second derivative in pi of ideal-gas part of fundamental equation for region 2
            '


            gamma0pipireg2 = -1 / pi ^ 2
            ''
        End Function

        Private Function gamma0taureg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' First derivative in tau of ideal-gas part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gamma0taureg2 = 0
            For i = 1 To 9



                gamma0taureg2 = gamma0taureg2 + n0reg2(i) * j0reg2(i) * tau ^ (j0reg2(i) - 1)
            Next i
            ''
        End Function

        Private Function gamma0tautaureg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Second derivative in tau of ideal-gas part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gamma0tautaureg2 = 0
            For i = 1 To 9



                gamma0tautaureg2 = gamma0tautaureg2 + n0reg2(i) * j0reg2(i) * (j0reg2(i) - 1) * tau ^ (j0reg2(i) - 2)
            Next i
            ''
        End Function

        Private Function gamma0pitaureg2(ByRef tau As Object, ByRef pi As Object) As Object
            '
            ' Second derivative in pi and tau of ideal-gas part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gamma0pitaureg2 = 0
            ''
        End Function

        Private Function gammarreg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Residual part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gammarreg2 = 0
            For i = 1 To 43




                gammarreg2 = gammarreg2 + nreg2(i) * pi ^ ireg2(i) * (tau - 0.5) ^ jreg2(i)
            Next i
            ''
        End Function

        Private Function gammarpireg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' First derivative in pi of residual part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gammarpireg2 = 0
            For i = 1 To 43




                gammarpireg2 = gammarpireg2 + nreg2(i) * ireg2(i) * pi ^ (ireg2(i) - 1) * (tau - 0.5) ^ jreg2(i)
            Next i
            ''
        End Function

        Private Function gammarpipireg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Second derivative in pi of residual part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gammarpipireg2 = 0
            For i = 1 To 43




                gammarpipireg2 = gammarpipireg2 + nreg2(i) * ireg2(i) * (ireg2(i) - 1) * pi ^ (ireg2(i) - 2) * (tau - 0.5) ^ jreg2(i)
            Next i
            ''
        End Function

        Private Function gammartaureg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' First derivative in tau of residual part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gammartaureg2 = 0
            For i = 1 To 43




                gammartaureg2 = gammartaureg2 + nreg2(i) * pi ^ ireg2(i) * jreg2(i) * (tau - 0.5) ^ (jreg2(i) - 1)
            Next i
            ''
        End Function

        Private Function gammartautaureg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Second derivative in tau of residual part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gammartautaureg2 = 0
            For i = 1 To 43




                gammartautaureg2 = gammartautaureg2 + nreg2(i) * pi ^ ireg2(i) * jreg2(i) * (jreg2(i) - 1) * (tau - 0.5) ^ (jreg2(i) - 2)
            Next i
            ''
        End Function

        Private Function gammarpitaureg2(ByRef tau As Object, ByRef pi As Object) As Object
            Dim i As Object
            '
            ' Second derivative in pi and tau of residual part of fundamental equation for region 2
            '
            Call InitFieldsreg2()

            gammarpitaureg2 = 0
            For i = 1 To 43




                gammarpitaureg2 = gammarpitaureg2 + nreg2(i) * ireg2(i) * pi ^ (ireg2(i) - 1) * jreg2(i) * (tau - 0.5) ^ (jreg2(i) - 1)
            Next i
            ''
        End Function

        Private Function fireg3(ByRef tau As Object, ByRef delta As Object) As Object
            Dim i As Object
            '
            ' Fundamental equation for region 3
            '
            Call InitFieldsreg3()


            fireg3 = nreg3(1) * System.Math.Log(delta)
            For i = 2 To 40




                fireg3 = fireg3 + nreg3(i) * delta ^ ireg3(i) * tau ^ jreg3(i)
            Next i
            ''
        End Function

        Private Function fideltareg3(ByRef tau As Object, ByRef delta As Object) As Object
            Dim i As Object
            '
            ' First derivative in delta of fundamental equation for region 3
            '
            Call InitFieldsreg3()


            fideltareg3 = nreg3(1) / delta
            For i = 2 To 40




                fideltareg3 = fideltareg3 + nreg3(i) * ireg3(i) * delta ^ (ireg3(i) - 1) * tau ^ jreg3(i)
            Next i
            ''
        End Function

        Private Function fideltadeltareg3(ByRef tau As Object, ByRef delta As Object) As Object
            Dim i As Object
            '
            ' Second derivative in delta of fundamental equation for region 3
            '
            Call InitFieldsreg3()


            fideltadeltareg3 = -nreg3(1) / delta ^ 2
            For i = 2 To 40




                fideltadeltareg3 = fideltadeltareg3 + nreg3(i) * ireg3(i) * (ireg3(i) - 1) * delta ^ (ireg3(i) - 2) * tau ^ jreg3(i)
            Next i
            ''
        End Function

        Private Function fitaureg3(ByRef tau As Object, ByRef delta As Object) As Object
            Dim i As Object
            '
            ' First derivative in tau of fundamental equation for region 3
            '
            Call InitFieldsreg3()

            fitaureg3 = 0
            For i = 2 To 40




                fitaureg3 = fitaureg3 + nreg3(i) * delta ^ ireg3(i) * jreg3(i) * tau ^ (jreg3(i) - 1)
            Next i
            ''
        End Function

        Private Function fitautaureg3(ByRef tau As Object, ByRef delta As Object) As Object
            Dim i As Object
            '
            ' Second derivative in tau of fundamental equation for region 3
            '
            Call InitFieldsreg3()

            fitautaureg3 = 0
            For i = 2 To 40




                fitautaureg3 = fitautaureg3 + nreg3(i) * delta ^ ireg3(i) * jreg3(i) * (jreg3(i) - 1) * tau ^ (jreg3(i) - 2)
            Next i
            ''
        End Function

        Private Function fideltataureg3(ByRef tau As Object, ByRef delta As Object) As Object
            Dim i As Object
            '
            ' Second derivative in delta and tau of fundamental equation for region 3
            '
            Call InitFieldsreg3()

            fideltataureg3 = 0
            For i = 2 To 40




                fideltataureg3 = fideltataureg3 + nreg3(i) * ireg3(i) * delta ^ (ireg3(i) - 1) * jreg3(i) * tau ^ (jreg3(i) - 1)
            Next i
            ''
        End Function

        Private Function psivisc(ByRef tau As Object, ByRef delta As Object) As Object
            Dim i As Object
            Dim psi1 As Object
            Dim psi0 As Object
            '
            ' Reduced dynamic viscosity
            '
            Call InitFieldsvisc()

            psi0 = 0

            psi1 = 0
            For i = 0 To 3



                psi0 = psi0 + n0visc(i) * tau ^ i
            Next i


            psi0 = 1 / (tau ^ 0.5 * psi0)
            For i = 1 To 19




                psi1 = psi1 + nvisc(i) * (delta - 1.0#) ^ ivisc(i) * (tau - 1.0#) ^ jvisc(i)
            Next i


            psi1 = System.Math.Exp(delta * psi1)



            psivisc = psi0 * psi1
            ''
        End Function

        Private Function lambthcon(ByRef temperature As Object, ByRef pressure As Object, ByRef tau As Object, ByRef delta As Object) As Object
            Dim lamb2 As Object
            Dim deltas As Object
            Dim ddeltadpi As Object
            Dim dpidtau As Object
            Dim pis As Object
            Dim taus As Object
            Dim j As Object
            Dim i As Object
            Dim lamb1 As Object
            Dim lamb0 As Object
            '
            ' Reduced thermal conductivity
            '
            Call InitFieldsthcon()

            lamb0 = 0

            lamb1 = 0
            For i = 0 To 3



                lamb0 = lamb0 + n0thcon(i) * tau ^ i
            Next i


            lamb0 = 1 / (tau ^ 0.5 * lamb0)
            For i = 0 To 4
                For j = 0 To 5





                    lamb1 = lamb1 + nthcon(i, j) * (tau - 1.0#) ^ i * (delta - 1.0#) ^ j
                Next j
            Next i


            lamb1 = System.Math.Exp(delta * lamb1)
            '
            ' v1.1: calculation of lamb2 corrected
            '


            If temperature >= 273.15 And temperature <= 623.15 And pressure >= pSatW(temperature) And pressure <= 1000.0# Then
                '  region 1


                taus = 1386.0# / temperature


                pis = pressure / 165.3





                dpidtau = (647.226 * 165.3 * (gammapitaureg1(taus, pis) * 1386.0# - gammapireg1(taus, pis) * temperature)) / (221.15 * temperature ^ 2 * gammapipireg1(taus, pis))




                ddeltadpi = -(22115000.0# * gammapipireg1(taus, pis)) / (317.763 * rgas_water * temperature * gammapireg1(taus, pis) ^ 2)



            ElseIf (temperature >= 273.15 And temperature <= 623.15 And pressure > 0 And pressure <= pSatW(temperature)) Or (temperature >= 623.15 And temperature <= 863.15 And pressure > 0 And pressure <= pBound(temperature)) Or (temperature >= 863.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0#) Then
                '  region 2


                taus = 540.0# / temperature


                pis = pressure / 10.0#








                dpidtau = (647.226 * 10.0# * ((gamma0pitaureg2(taus, pis) + gammarpitaureg2(taus, pis)) * 540.0# - (gamma0pireg2(taus, pis) + gammarpireg2(taus, pis)) * temperature)) / (221.15 * temperature ^ 2 * (gamma0pipireg2(taus, pis) + gammarpipireg2(taus, pis)))






                ddeltadpi = -(22115000.0# * (gamma0pipireg2(taus, pis) + gammarpipireg2(taus, pis))) / (317.763 * rgas_water * temperature * (gamma0pireg2(taus, pis) + gammarpireg2(taus, pis)) ^ 2)




            ElseIf temperature >= 623.15 And temperature <= tBound(pressure) And pressure >= pBound(temperature) And pressure <= 1000.0# Then
                '  region 3


                taus = 647.096 / temperature


                deltas = delta * 317.763 / 322.0#





                dpidtau = (647.226 * rgas_water * (delta * 317.763) ^ 2 * (fideltareg3(taus, deltas) - (647.096 / temperature) * fideltataureg3(taus, deltas))) / (22115000.0# * 322.0#)





                ddeltadpi = (22115000.0# * 322.0#) / (317.763 * delta * 317.763 * rgas_water * temperature * (2 * fideltareg3(taus, deltas) + (delta * 317.763 / 322.0#) * fideltadeltareg3(taus, deltas)))
            Else
                '  outside range

                dpidtau = 0

                ddeltadpi = 0
            End If






            lamb2 = 0.0013848 / psivisc(tau, delta) * (tau * delta) ^ (-2) * dpidtau ^ 2 * (delta * ddeltadpi) ^ 0.4678 * delta ^ 0.5 * System.Math.Exp(-18.66 * (1 / tau - 1) ^ 2 - (delta - 1) ^ 4)




            lambthcon = lamb0 * lamb1 + lamb2
            ''
        End Function

#End Region

        Public Function pSatW(ByRef temperature As Object) As Object
            Dim cco As Object
            Dim bco As Object
            Dim aco As Object
            Dim del As Object
            '
            ' saturation pressure of water
            ' pSatW in bar
            ' temperature in K
            '
            ' pSatW = -1: temperature outside range
            '
            '
            If temperature < 273.15 Or temperature > 647.096 Then

                pSatW = -1.0#
            Else
                Call InitFieldsreg4()
                del = temperature + nreg4(9) / (temperature - nreg4(10))
                aco = del ^ 2 + nreg4(1) * del + nreg4(2)
                bco = nreg4(3) * del ^ 2 + nreg4(4) * del + nreg4(5)
                cco = nreg4(6) * del ^ 2 + nreg4(7) * del + nreg4(8)
                pSatW = (2 * cco / (-bco + (bco ^ 2 - 4 * aco * cco) ^ 0.5)) ^ 4 * 10
            End If
            ''
        End Function

        Public Function tSatW(ByRef pressure As Object) As Object
            Dim dco As Object
            Dim gco As Object
            Dim fco As Object
            Dim eco As Object
            Dim bet As Object
            '
            ' saturation temperature of water
            ' tSatW in K
            ' pressure in bar
            '
            ' tSatW = -1: pressure outside range
            '
            '
            'If pressure < 0.00611213 Or pressure > 220.64 Then
            '    tSatW = -1.0#
            'Else
            Call InitFieldsreg4()
            bet = (0.1 * pressure) ^ 0.25
            eco = bet ^ 2 + nreg4(3) * bet + nreg4(6)
            fco = nreg4(1) * bet ^ 2 + nreg4(4) * bet + nreg4(7)
            gco = nreg4(2) * bet ^ 2 + nreg4(5) * bet + nreg4(8)
            dco = 2 * gco / (-fco - (fco ^ 2 - 4 * eco * gco) ^ 0.5)
            tSatW = 0.5 * (nreg4(10) + dco - ((nreg4(10) + dco) ^ 2 - 4 * (nreg4(9) + nreg4(10) * dco)) ^ 0.5)
            'End If
            ''
        End Function

#Region "private functions"

        Private Function pBound(ByRef temperature As Object) As Object
            '
            ' boundary pressure between regions 2 and 3
            ' pBound in bar
            ' temperature in K
            '
            ' pBound = -1: temperature outside range
            '
            '
            If temperature < 623.15 Or temperature > 863.15 Then

                pBound = -1.0#
            Else
                Call InitFieldsbound()


                pBound = (nbound(1) + nbound(2) * temperature + nbound(3) * temperature ^ 2) * 10.0#
            End If
            ''
        End Function

        Public Function tBound(ByRef pressure As Object) As Object
            '
            ' boundary temperature between regions 2 and 3
            ' tBound in K
            ' pressure in bar
            '
            ' tBound = -1: pressure outside range
            '
            '
            If pressure < 165.292 Or pressure > 1000.0# Then

                tBound = -1.0#
            Else
                Call InitFieldsbound()


                tBound = nbound(4) + ((0.1 * pressure - nbound(5)) / nbound(3)) ^ 0.5
            End If
            ''
        End Function

        Private Function volreg1(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific volume in region 1
            ' volreg1 in m^3/kg
            ' temperature in K
            ' pressure in bar
            '


            tau = 1386.0# / temperature


            pi = 0.1 * pressure / 16.53





            volreg1 = rgas_water * temperature * pi * gammapireg1(tau, pi) / (pressure * 100000.0#)
            ''
        End Function

        Private Function energyreg1(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific internal energy in region 1
            ' energyreg1 in kJ/kg
            ' temperature in K
            ' pressure in bar
            '


            tau = 1386.0# / temperature


            pi = 0.1 * pressure / 16.53






            energyreg1 = 0.001 * rgas_water * temperature * (tau * gammataureg1(tau, pi) - pi * gammapireg1(tau, pi))
            ''
        End Function

        Private Function entropyreg1(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific entropy in region 1
            ' entropyreg1 in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '


            tau = 1386.0# / temperature


            pi = 0.1 * pressure / 16.53




            entropyreg1 = 0.001 * rgas_water * (tau * gammataureg1(tau, pi) - gammareg1(tau, pi))
            ''
        End Function

        Private Function enthalpyreg1(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific enthalpy in region 1
            ' enthalpyreg1 in kJ/kg
            ' temperature in K
            ' pressure in bar
            '


            tau = 1386.0# / temperature


            pi = 0.1 * pressure / 16.53




            enthalpyreg1 = 0.001 * rgas_water * temperature * tau * gammataureg1(tau, pi)
            ''
        End Function

        Private Function cpreg1(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific isobaric heat capacity in region 1
            ' cpreg1 in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '


            tau = 1386.0# / temperature


            pi = 0.1 * pressure / 16.53



            cpreg1 = -0.001 * rgas_water * tau ^ 2 * gammatautaureg1(tau, pi)
            ''
        End Function

        Private Function cvreg1(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific isochoric heat capacity in region 1
            ' cvreg1 in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '


            tau = 1386.0# / temperature


            pi = 0.1 * pressure / 16.53






            cvreg1 = 0.001 * rgas_water * (-tau ^ 2 * gammatautaureg1(tau, pi) + (gammapireg1(tau, pi) - tau * gammapitaureg1(tau, pi)) ^ 2 / gammapipireg1(tau, pi))
            ''
        End Function

        Private Function volreg2(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific volume in region 2
            ' volreg2 in m^3/kg
            ' temperature in K
            ' pressure in bar
            '


            tau = 540.0# / temperature


            pi = 0.1 * pressure






            volreg2 = rgas_water * temperature * pi * (gamma0pireg2(tau, pi) + gammarpireg2(tau, pi)) / (pressure * 100000.0#)
            ''
        End Function

        Private Function energyreg2(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific internal energy in region 2
            ' energyreg2 in kJ/kg
            ' temperature in K
            ' pressure in bar
            '


            tau = 540.0# / temperature


            pi = 0.1 * pressure








            energyreg2 = 0.001 * rgas_water * temperature * (tau * (gamma0taureg2(tau, pi) + gammartaureg2(tau, pi)) - pi * (gamma0pireg2(tau, pi) + gammarpireg2(tau, pi)))
            ''
        End Function

        Private Function entropyreg2(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific entropy in region 2
            ' entropyreg2 in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '


            tau = 540.0# / temperature


            pi = 0.1 * pressure






            entropyreg2 = 0.001 * rgas_water * (tau * (gamma0taureg2(tau, pi) + gammartaureg2(tau, pi)) - (gamma0reg2(tau, pi) + gammarreg2(tau, pi)))
            ''
        End Function

        Private Function enthalpyreg2(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific enthalpy in region 2
            ' enthalpyreg2 in kJ/kg
            ' temperature in K
            ' pressure in bar
            '


            tau = 540.0# / temperature


            pi = 0.1 * pressure





            enthalpyreg2 = 0.001 * rgas_water * temperature * tau * (gamma0taureg2(tau, pi) + gammartaureg2(tau, pi))
            ''
        End Function

        Private Function cpreg2(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific isobaric heat capacity in region 2
            ' cpreg2 in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '


            tau = 540.0# / temperature


            pi = 0.1 * pressure




            cpreg2 = -0.001 * rgas_water * tau ^ 2 * (gamma0tautaureg2(tau, pi) + gammartautaureg2(tau, pi))
            ''
        End Function

        Private Function cvreg2(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim pi As Object
            Dim tau As Object
            '
            ' specific isochoric heat capacity in region 2
            ' cvreg2 in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '


            tau = 540.0# / temperature


            pi = 0.1 * pressure








            cvreg2 = 0.001 * rgas_water * (-tau ^ 2 * (gamma0tautaureg2(tau, pi) + gammartautaureg2(tau, pi)) - (1 + pi * gammarpireg2(tau, pi) - tau * pi * gammarpitaureg2(tau, pi)) ^ 2 / (1 - pi ^ 2 * gammarpipireg2(tau, pi)))
            ''
        End Function

        Private Function pressreg3(ByRef temperature As Object, ByRef density As Object) As Object
            Dim delta As Object
            Dim tau As Object
            '
            ' pressure in region 3
            ' pressreg3 in bar
            ' temperature in K
            ' density in kg/m^3
            '


            tau = tc_water / temperature


            delta = density / dc_water





            pressreg3 = density * rgas_water * temperature * delta * fideltareg3(tau, delta) / 100000.0#
            ''
        End Function

        Private Function energyreg3(ByRef temperature As Object, ByRef density As Object) As Object
            Dim delta As Object
            Dim tau As Object
            '
            ' specific internal energy in region 3
            ' energyreg3 in kJ/kg
            ' temperature in K
            ' density in kg/m^3
            '


            tau = tc_water / temperature


            delta = density / dc_water




            energyreg3 = 0.001 * rgas_water * temperature * tau * fitaureg3(tau, delta)
            ''
        End Function

        Private Function entropyreg3(ByRef temperature As Object, ByRef density As Object) As Object
            Dim delta As Object
            Dim tau As Object
            '
            ' specific entropy in region 3
            ' entropyreg3 in kJ/(kg K)
            ' temperature in K
            ' density in kg/m^3
            '


            tau = tc_water / temperature


            delta = density / dc_water




            entropyreg3 = 0.001 * rgas_water * (tau * fitaureg3(tau, delta) - fireg3(tau, delta))
            ''
        End Function

        Private Function enthalpyreg3(ByRef temperature As Object, ByRef density As Object) As Object
            Dim delta As Object
            Dim tau As Object
            '
            ' specific enthalpy in region 3
            ' enthalpyreg3 in kJ/kg
            ' temperature in K
            ' density in kg/m^3
            '


            tau = tc_water / temperature


            delta = density / dc_water






            enthalpyreg3 = 0.001 * rgas_water * temperature * (tau * fitaureg3(tau, delta) + delta * fideltareg3(tau, delta))
            ''
        End Function

        Private Function cpreg3(ByRef temperature As Object, ByRef density As Object) As Object
            Dim delta As Object
            Dim tau As Object
            '
            ' specific isobaric heat capacity in region 3
            ' cpreg3 in kJ/(kg K)
            ' temperature in K
            ' density in kg/m^3
            '


            tau = tc_water / temperature


            delta = density / dc_water







            cpreg3 = 0.001 * rgas_water * (-tau ^ 2 * fitautaureg3(tau, delta) + (delta * fideltareg3(tau, delta) - delta * tau * fideltataureg3(tau, delta)) ^ 2 / (2 * delta * fideltareg3(tau, delta) + delta ^ 2 * fideltadeltareg3(tau, delta)))
            ''
        End Function

        Private Function cvreg3(ByRef temperature As Object, ByRef density As Object) As Object
            Dim delta As Object
            Dim tau As Object
            '
            ' specific isochoric heat capacity in region 3
            ' cvreg3 in kJ/(kg K)
            ' temperature in K
            ' density in kg/m^3
            '


            tau = tc_water / temperature


            delta = density / dc_water



            cvreg3 = 0.001 * rgas_water * (-tau ^ 2 * fitautaureg3(tau, delta))
            ''
        End Function

        Private Function densreg3(ByRef temperature As Object, ByRef pressure As Object) As Object

            Dim diffdens As Object
            Dim densnew As Object
            Dim derivprho As Object
            Dim delta As Object
            Dim j As Object
            Dim tau As Object
            Dim densold As Object
            '
            ' Determine density in region 3 iteratively using Newton method
            ' densreg3 in kg/m^3
            ' temperature in K
            ' pressure in bar
            '
            ' densreg3 = -2: not converged
            '



            If temperature < tc_water And pressure < pSatW(temperature) Then

                densold = 100.0#
            Else

                densold = 600.0#
            End If


            tau = tc_water / temperature
            '
            For j = 1 To 1000


                delta = densold / dc_water





                derivprho = rgas_water * temperature / dc_water * (2 * densold * fideltareg3(tau, delta) + densold ^ 2 / dc_water * fideltadeltareg3(tau, delta))






                densnew = densold + (pressure * 100000.0# - rgas_water * temperature * densold ^ 2 / dc_water * fideltareg3(tau, delta)) / derivprho



                diffdens = System.Math.Abs(densnew - densold)
                If diffdens < 0.000005 Then


                    densreg3 = densnew
                    Exit Function
                End If


                densold = densnew
            Next j

            densreg3 = -2.0#
            ''
        End Function

#End Region

        Public Function densW(ByRef temperature As Object, ByRef pressure As Object) As Object
            '
            ' density of water or steam
            ' densW in kg/m^3
            ' temperature in K
            ' pressure in bar
            '
            ' densW = -1: temperature and/or pressure outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 And pressure >= pSatW(temperature) And pressure <= 1000.0# Then
                '  region 1
                densW = 1 / volreg1(temperature, pressure)
            ElseIf (temperature >= 273.15 And temperature <= 623.15 And pressure > 0 And pressure <= pSatW(temperature)) Or (temperature >= 623.15 And temperature <= 863.15 And pressure > 0 And pressure <= pBound(temperature)) Or (temperature >= 863.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0#) Then
                '  region 2
                densW = 1 / volreg2(temperature, pressure)
            ElseIf temperature >= 623.15 And temperature <= tBound(pressure) And pressure >= pBound(temperature) And pressure <= 1000.0# Then
                '  region 3
                densW = densreg3(temperature, pressure)
            Else
                '  outside range
                densW = -1.0#
            End If
            ''
        End Function

        Public Function energyW(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim density As Object
            '
            ' specific internal energy of water or steam
            ' energyW in kJ/kg
            ' temperature in K
            ' pressure in bar
            '
            ' energyW = -1: temperature and/or pressure outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 And pressure >= pSatW(temperature) And pressure <= 1000.0# Then
                '  region 1
                energyW = energyreg1(temperature, pressure)
            ElseIf (temperature >= 273.15 And temperature <= 623.15 And pressure > 0 And pressure <= pSatW(temperature)) Or (temperature >= 623.15 And temperature <= 863.15 And pressure > 0 And pressure <= pBound(temperature)) Or (temperature >= 863.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0#) Then
                '  region 2
                energyW = energyreg2(temperature, pressure)
            ElseIf temperature >= 623.15 And temperature <= tBound(pressure) And pressure >= pBound(temperature) And pressure <= 1000.0# Then
                '  region 3
                density = densreg3(temperature, pressure)
                energyW = energyreg3(temperature, density)
            Else
                '  outside range
                energyW = -1.0#
            End If
            ''
        End Function

        Public Function entropyW(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim density As Object
            '
            ' specific entropy of water or steam
            ' entropyW in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '
            ' entropyW = -1: temperature and/or pressure outside range
            '


            If temperature >= 273.15 And temperature <= 623.15 And pressure >= pSatW(temperature) And pressure <= 1000.0# Then
                '  region 1

                entropyW = entropyreg1(temperature, pressure)



            ElseIf (temperature >= 273.15 And temperature <= 623.15 And pressure > 0 And pressure <= pSatW(temperature)) Or (temperature >= 623.15 And temperature <= 863.15 And pressure > 0 And pressure <= pBound(temperature)) Or (temperature >= 863.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0#) Then
                '  region 2

                entropyW = entropyreg2(temperature, pressure)




            ElseIf temperature >= 623.15 And temperature <= tBound(pressure) And pressure >= pBound(temperature) And pressure <= 1000.0# Then
                '  region 3


                density = densreg3(temperature, pressure)

                entropyW = entropyreg3(temperature, density)
            Else
                '  outside range

                entropyW = -1.0#
            End If
            ''
        End Function

        Public Function enthalpyW(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim density As Object
            '
            ' specific enthalpy of water or steam
            ' enthalpyW in kJ/kg
            ' temperature in K
            ' pressure in bar
            '
            ' enthalpyW = -1: temperature and/or pressure outside range
            '


            If temperature >= 273.15 And temperature <= 623.15 And pressure >= pSatW(temperature) And pressure <= 1000.0# Then
                '  region 1

                enthalpyW = enthalpyreg1(temperature, pressure)



            ElseIf (temperature >= 273.15 And temperature <= 623.15 And pressure > 0 And pressure <= pSatW(temperature)) Or (temperature >= 623.15 And temperature <= 863.15 And pressure > 0 And pressure <= pBound(temperature)) Or (temperature >= 863.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0#) Then
                '  region 2

                enthalpyW = enthalpyreg2(temperature, pressure)




            ElseIf temperature >= 623.15 And temperature <= tBound(pressure) And pressure >= pBound(temperature) And pressure <= 1000.0# Then
                '  region 3


                density = densreg3(temperature, pressure)

                enthalpyW = enthalpyreg3(temperature, density)
            Else
                '  outside range

                enthalpyW = -1.0#
            End If
            ''
        End Function

        Public Function cpW(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim density As Object
            '
            ' specific isobaric heat capacity of water or steam
            ' cpW in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '
            ' cpW = -1: temperature and/or pressure outside range
            '


            If temperature >= 273.15 And temperature <= 623.15 And pressure >= pSatW(temperature) And pressure <= 1000.0# Then
                '  region 1

                cpW = cpreg1(temperature, pressure)



            ElseIf (temperature >= 273.15 And temperature <= 623.15 And pressure > 0 And pressure <= pSatW(temperature)) Or (temperature >= 623.15 And temperature <= 863.15 And pressure > 0 And pressure <= pBound(temperature)) Or (temperature >= 863.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0#) Then
                '  region 2

                cpW = cpreg2(temperature, pressure)




            ElseIf temperature >= 623.15 And temperature <= tBound(pressure) And pressure >= pBound(temperature) And pressure <= 1000.0# Then
                '  region 3


                density = densreg3(temperature, pressure)

                cpW = cpreg3(temperature, density)
            Else
                '  outside range

                cpW = -1.0#
            End If
            ''
        End Function

        Public Function cvW(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim density As Object
            '
            ' specific isochoric heat capacity of water or steam
            ' cvW in kJ/(kg K)
            ' temperature in K
            ' pressure in bar
            '
            ' cvW = -1: temperature and/or pressure outside range
            '


            If temperature >= 273.15 And temperature <= 623.15 And pressure >= pSatW(temperature) And pressure <= 1000.0# Then
                '  region 1

                cvW = cvreg1(temperature, pressure)



            ElseIf (temperature >= 273.15 And temperature <= 623.15 And pressure > 0 And pressure <= pSatW(temperature)) Or (temperature >= 623.15 And temperature <= 863.15 And pressure > 0 And pressure <= pBound(temperature)) Or (temperature >= 863.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0#) Then
                '  region 2

                cvW = cvreg2(temperature, pressure)




            ElseIf temperature >= 623.15 And temperature <= tBound(pressure) And pressure >= pBound(temperature) And pressure <= 1000.0# Then
                '  region 3


                density = densreg3(temperature, pressure)

                cvW = cvreg3(temperature, density)
            Else
                '  outside range

                cvW = -1.0#
            End If
            ''
        End Function

        Public Function viscW(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            '
            ' dynamic viscosity of water or steam
            ' viscW in Pa s
            ' temperature in K
            ' pressure in bar
            '
            ' viscW = -1: temperature and/or pressure outside range
            '

            If temperature >= 273.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0# Then


                density = densW(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscW = 0.000055071 * psivisc(tau, delta)
            Else
                '  outside range

                viscW = -1.0#
            End If
            ''
        End Function

        Public Function thconW(ByRef temperature As Object, ByRef pressure As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            '
            ' thermal conductivity of water or steam
            ' thconW in W/(m K)
            ' temperature in K
            ' pressure in bar
            '
            ' thconW = -1: temperature and/or pressure outside range
            '

            If temperature >= 273.15 And temperature <= tmax And pressure > 0 And pressure <= 1000.0# Then


                density = densW(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                thconW = 0.4945 * lambthcon(temperature, pressure, tau, delta)
            Else
                '  outside range

                thconW = -1.0#
            End If
            ''
        End Function

        Public Function densSatLiqTW(ByRef temperature As Object) As Object
            Dim pressure As Object
            '
            ' density of saturated liquid water as a function of temperature
            ' densSatLiqTW in kg/m^3
            ' temperature in K
            '
            ' densSatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)


                densSatLiqTW = 1 / volreg1(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)

                densSatLiqTW = densreg3(temperature, pressure)
            Else
                '  outside range

                densSatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function densSatVapTW(ByRef temperature As Object) As Object
            Dim pressure As Object
            '
            ' density of saturated steam as a function of temperature
            ' densSatVapTW in kg/m^3
            ' temperature in K
            '
            ' densSatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)


                densSatVapTW = 1 / volreg2(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001

                densSatVapTW = densreg3(temperature, pressure)
            Else
                '  outside range

                densSatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function densSatLiqPW(ByRef pressure As Object) As Object
            Dim temperature As Object
            '
            ' density of saturated liquid water as a function of pressure
            ' densSatLiqPW in kg/m^3
            ' pressure in bar
            '
            ' densSatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)


                densSatLiqPW = 1 / volreg1(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001

                densSatLiqPW = densreg3(temperature, pressure)
            Else
                '  outside range

                densSatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function densSatVapPW(ByRef pressure As Object) As Object
            Dim temperature As Object
            '
            ' density of saturated steam as a function of pressure
            ' densSatVapPW in kg/m^3
            ' pressure in bar
            '
            ' densSatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)


                densSatVapPW = 1 / volreg2(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001

                densSatVapPW = densreg3(temperature, pressure)
            Else
                '  outside range

                densSatVapPW = -1.0#
            End If
            ''
        End Function

        Public Function energySatLiqTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific internal energy of saturated liquid water as a function of temperature
            ' energySatLiqTW in kJ/kg
            ' temperature in K
            '
            ' energySatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)

                energySatLiqTW = energyreg1(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)


                density = densreg3(temperature, pressure)

                energySatLiqTW = energyreg3(temperature, density)
            Else
                '  outside range

                energySatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function energySatVapTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific internal energy of saturated steam as a function of temperature
            ' energySatVapTW in kJ/kg
            ' temperature in K
            '
            ' energySatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)

                energySatVapTW = energyreg2(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001


                density = densreg3(temperature, pressure)

                energySatVapTW = energyreg3(temperature, density)
            Else
                '  outside range

                energySatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function energySatLiqPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific internal energy of saturated liquid water as a function of pressure
            ' energySatLiqPW in kJ/kg
            ' pressure in bar
            '
            ' energySatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)

                energySatLiqPW = energyreg1(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001


                density = densreg3(temperature, pressure)

                energySatLiqPW = energyreg3(temperature, density)
            Else
                '  outside range

                energySatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function energySatVapPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific internal energy of saturated steam as a function of pressure
            ' energySatVapPW in kJ/kg
            ' pressure in bar
            '
            ' energySatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)

                energySatVapPW = energyreg2(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001


                density = densreg3(temperature, pressure)

                energySatVapPW = energyreg3(temperature, density)
            Else
                '  outside range

                energySatVapPW = -1.0#
            End If
            ''
        End Function

        Public Function entropySatLiqTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific entropy of saturated liquid water as a function of temperature
            ' entropySatLiqTW in kJ/(kg K)
            ' temperature in K
            '
            ' entropySatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)

                entropySatLiqTW = entropyreg1(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)


                density = densreg3(temperature, pressure)

                entropySatLiqTW = entropyreg3(temperature, density)
            Else
                '  outside range

                entropySatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function entropySatVapTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific entropy of saturated steam as a function of temperature
            ' entropySatVapTW in kJ/(kg K)
            ' temperature in K
            '
            ' entropySatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)

                entropySatVapTW = entropyreg2(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001


                density = densreg3(temperature, pressure)

                entropySatVapTW = entropyreg3(temperature, density)
            Else
                '  outside range

                entropySatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function entropySatLiqPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific entropy of saturated liquid water as a function of pressure
            ' entropySatLiqPW in kJ/(kg K)
            ' pressure in bar
            '
            ' entropySatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)

                entropySatLiqPW = entropyreg1(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001


                density = densreg3(temperature, pressure)

                entropySatLiqPW = entropyreg3(temperature, density)
            Else
                '  outside range

                entropySatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function entropySatVapPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific entropy of saturated steam as a function of pressure
            ' entropySatVapPW in kJ/(kg K)
            ' pressure in bar
            '
            ' entropySatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)

                entropySatVapPW = entropyreg2(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001


                density = densreg3(temperature, pressure)

                entropySatVapPW = entropyreg3(temperature, density)
            Else
                '  outside range

                entropySatVapPW = -1.0#
            End If
            ''
        End Function

        Public Function enthalpySatLiqTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific enthalpy of saturated liquid water as a function of temperature
            ' enthalpySatLiqTW in kJ/kg
            ' temperature in K
            '
            ' enthalpySatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)

                enthalpySatLiqTW = enthalpyreg1(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)


                density = densreg3(temperature, pressure)

                enthalpySatLiqTW = enthalpyreg3(temperature, density)
            Else
                '  outside range

                enthalpySatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function enthalpySatVapTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific enthalpy of saturated steam as a function of temperature
            ' enthalpySatVapTW in kJ/kg
            ' temperature in K
            '
            ' enthalpySatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)

                enthalpySatVapTW = enthalpyreg2(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001


                density = densreg3(temperature, pressure)

                enthalpySatVapTW = enthalpyreg3(temperature, density)
            Else
                '  outside range

                enthalpySatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function enthalpySatLiqPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific enthalpy of saturated liquid water as a function of pressure
            ' enthalpySatLiqPW in kJ/kg
            ' pressure in bar
            '
            ' enthalpySatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)

                enthalpySatLiqPW = enthalpyreg1(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001


                density = densreg3(temperature, pressure)

                enthalpySatLiqPW = enthalpyreg3(temperature, density)
            Else
                '  outside range

                enthalpySatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function enthalpySatVapPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific enthalpy of saturated steam as a function of pressure
            ' enthalpySatVapPW in kJ/kg
            ' pressure in bar
            '
            ' enthalpySatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)

                enthalpySatVapPW = enthalpyreg2(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001


                density = densreg3(temperature, pressure)

                enthalpySatVapPW = enthalpyreg3(temperature, density)
            Else
                '  outside range

                enthalpySatVapPW = -1.0#
            End If
            ''
        End Function

        Public Function cpSatLiqTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific isobaric heat capacity of saturated liquid water as a function of temperature
            ' cpSatLiqTW in kJ/(kg K)
            ' temperature in K
            '
            ' cpSatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)

                cpSatLiqTW = cpreg1(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)


                density = densreg3(temperature, pressure)

                cpSatLiqTW = cpreg3(temperature, density)
            Else
                '  outside range

                cpSatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function cpSatVapTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific isobaric heat capacity of saturated steam as a function of temperature
            ' cpSatVapTW in kJ/(kg K)
            ' temperature in K
            '
            ' cpSatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)

                cpSatVapTW = cpreg2(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001


                density = densreg3(temperature, pressure)

                cpSatVapTW = cpreg3(temperature, density)
            Else
                '  outside range

                cpSatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function cpSatLiqPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific isobaric heat capacity of saturated liquid water as a function of pressure
            ' cpSatLiqPW in kJ/(kg K)
            ' pressure in bar
            '
            ' cpSatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)

                cpSatLiqPW = cpreg1(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001


                density = densreg3(temperature, pressure)

                cpSatLiqPW = cpreg3(temperature, density)
            Else
                '  outside range

                cpSatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function cpSatVapPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific isobaric heat capacity of saturated steam as a function of pressure
            ' cpSatVapPW in kJ/(kg K)
            ' pressure in bar
            '
            ' cpSatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)

                cpSatVapPW = cpreg2(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001


                density = densreg3(temperature, pressure)

                cpSatVapPW = cpreg3(temperature, density)
            Else
                '  outside range

                cpSatVapPW = -1.0#
            End If
            ''
        End Function

        Public Function cvSatLiqTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific isochoric heat capacity of saturated liquid water as a function of temperature
            ' cvSatLiqTW in kJ/(kg K)
            ' temperature in K
            '
            ' cvSatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)

                cvSatLiqTW = cvreg1(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)


                density = densreg3(temperature, pressure)

                cvSatLiqTW = cvreg3(temperature, density)
            Else
                '  outside range

                cvSatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function cvSatVapTW(ByRef temperature As Object) As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' specific isochoric heat capacity of saturated steam as a function of temperature
            ' cvSatVapTW in kJ/(kg K)
            ' temperature in K
            '
            ' cvSatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)

                cvSatVapTW = cvreg2(temperature, pressure)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001


                density = densreg3(temperature, pressure)

                cvSatVapTW = cvreg3(temperature, density)
            Else
                '  outside range

                cvSatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function cvSatLiqPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific isochoric heat capacity of saturated liquid water as a function of pressure
            ' cvSatLiqPW in kJ/(kg K)
            ' pressure in bar
            '
            ' cvSatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)

                cvSatLiqPW = cvreg1(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001


                density = densreg3(temperature, pressure)

                cvSatLiqPW = cvreg3(temperature, density)
            Else
                '  outside range

                cvSatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function cvSatVapPW(ByRef pressure As Object) As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' specific isochoric heat capacity of saturated steam as a function of pressure
            ' cvSatVapPW in kJ/(kg K)
            ' pressure in bar
            '
            ' cvSatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)

                cvSatVapPW = cvreg2(temperature, pressure)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001


                density = densreg3(temperature, pressure)

                cvSatVapPW = cvreg3(temperature, density)
            Else
                '  outside range

                cvSatVapPW = -1.0#
            End If
            ''
        End Function

        Public Function viscSatLiqTW(ByRef temperature As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' dynamic viscosity of saturated liquid water as a function of temperature
            ' viscSatLiqTW in Pa s
            ' temperature in K
            '
            ' viscSatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)


                density = 1 / volreg1(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatLiqTW = 0.000055071 * psivisc(tau, delta)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatLiqTW = 0.000055071 * psivisc(tau, delta)
            Else
                '  outside range

                viscSatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function viscSatVapTW(ByRef temperature As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' dynamic viscosity of saturated steam as a function of temperature
            ' viscSatVapTW in Pa s
            ' temperature in K
            '
            ' viscSatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)


                density = 1 / volreg2(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatVapTW = 0.000055071 * psivisc(tau, delta)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatVapTW = 0.000055071 * psivisc(tau, delta)
            Else
                '  outside range

                viscSatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function viscSatLiqPW(ByRef pressure As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' dynamic viscosity of saturated liquid water as a function of pressure
            ' viscSatLiqPW in Pa s
            ' pressure in bar
            '
            ' viscSatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)


                density = 1 / volreg1(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatLiqPW = 0.000055071 * psivisc(tau, delta)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatLiqPW = 0.000055071 * psivisc(tau, delta)
            Else
                '  outside range

                viscSatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function viscSatVapPW(ByRef pressure As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' dynamic viscosity of saturated steam as a function of pressure
            ' viscSatVapPW in Pa s
            ' pressure in bar
            '
            ' viscSatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)


                density = 1 / volreg2(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatVapPW = 0.000055071 * psivisc(tau, delta)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                viscSatVapPW = 0.000055071 * psivisc(tau, delta)
            Else
                '  outside range

                viscSatVapPW = -1.0#
            End If
            ''
        End Function

        Public Function thconSatLiqTW(ByRef temperature As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' thermal conductivity of saturated liquid water as a function of temperature
            ' thconSatLiqTW in W /(m K)
            ' temperature in K
            '
            ' thconSatLiqTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 1


                pressure = pSatW(temperature)


                density = 1 / volreg1(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                thconSatLiqTW = 0.4945 * lambthcon(temperature, pressure, tau, delta)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature)


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                thconSatLiqTW = 0.4945 * lambthcon(temperature, pressure, tau, delta)
            Else
                '  outside range

                thconSatLiqTW = -1.0#
            End If
            ''
        End Function

        Public Function thconSatVapTW(ByRef temperature As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim pressure As Object
            '
            ' thermal conductivity of saturated steam as a function of temperature
            ' thconSatVapTW in W /(m K)
            ' temperature in K
            '
            ' thconSatVapTW = -1: temperature outside range
            '
            If temperature >= 273.15 And temperature <= 623.15 Then
                '  region 2


                pressure = pSatW(temperature)


                density = 1 / volreg2(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature

                pressure = pressure - 0.0001 * pressure


                thconSatVapTW = 0.4945 * lambthcon(temperature, pressure, tau, delta)

            ElseIf temperature > 623.15 And temperature <= tc_water Then
                '  region 3


                pressure = pSatW(temperature) - 0.00001


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                thconSatVapTW = 0.4945 * lambthcon(temperature, pressure, tau, delta)
            Else
                '  outside range

                thconSatVapTW = -1.0#
            End If
            ''
        End Function

        Public Function thconSatLiqPW(ByRef pressure As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' thermal conductivity of saturated liquid water as a function of pressure
            ' thconSatLiqPW in W /(m K)
            ' pressure in bar
            '
            ' thconSatLiqPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 1


                temperature = tSatW(pressure)


                density = 1 / volreg1(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                thconSatLiqPW = 0.4945 * lambthcon(temperature, pressure, tau, delta)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure + 0.00001


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                thconSatLiqPW = 0.4945 * lambthcon(temperature, pressure, tau, delta)
            Else
                '  outside range

                thconSatLiqPW = -1.0#
            End If
            ''
        End Function

        Public Function thconSatVapPW(ByRef pressure As Object) As Object
            Dim tau As Object
            Dim delta As Object
            Dim density As Object
            Dim temperature As Object
            '
            ' thermal conductivity of saturated steam as a function of pressure
            ' thconSatVapPW in W /(m K)
            ' pressure in bar
            '
            ' thconSatVapPW = -1: pressure outside range
            '


            If pressure >= pSatW(273.15) And pressure <= pSatW(623.15) Then
                '  region 2


                temperature = tSatW(pressure)


                density = 1 / volreg2(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature

                pressure = pressure - 0.0001 * pressure


                thconSatVapPW = 0.4945 * lambthcon(temperature, pressure, tau, delta)


            ElseIf pressure > pSatW(623.15) And pressure <= pc_water Then
                '  region 3


                temperature = tSatW(pressure)

                pressure = pressure - 0.00001


                density = densreg3(temperature, pressure)


                delta = density / 317.763


                tau = 647.226 / temperature


                thconSatVapPW = 0.4945 * lambthcon(temperature, pressure, tau, delta)
            Else
                '  outside range

                thconSatVapPW = -1.0#
            End If
            ''
        End Function

    End Class

End Namespace
