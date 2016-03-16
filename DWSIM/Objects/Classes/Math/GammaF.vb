'************************************************************************
'Cephes Math Library Release 2.8:  June, 2000
'Copyright by Stephen L. Moshier
'
'Contributors:
'    * Sergey Bochkanov (ALGLIB project). Translation from C to
'      pseudocode.
'
'See subroutines comments for additional copyrights.
'
'Redistribution and use in source and binary forms, with or without
'modification, are permitted provided that the following conditions are
'met:
'
'- Redistributions of source code must retain the above copyright
'  notice, this list of conditions and the following disclaimer.
'
'- Redistributions in binary form must reproduce the above copyright
'  notice, this list of conditions and the following disclaimer listed
'  in this license in the documentation and/or other materials
'  provided with the distribution.
'
'- Neither the name of the copyright holders nor the names of its
'  contributors may be used to endorse or promote products derived from
'  this software without specific prior written permission.
'
'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
'A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
'OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
'SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
'LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
'DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
'THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
'(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
'************************************************************************

Imports System

Namespace DWSIM.MathEx.GammaFunctions

    Public Class igammaf

        '************************************************************************
        '    Incomplete gamma integral
        '
        '    The function is defined by
        '
        '                              x
        '                               -
        '                      1       | |  -t  a-1
        '     igam(a,x)  =   -----     |   e   t   dt.
        '                     -      | |
        '                    | (a)    -
        '                              0
        '
        '
        '    In this implementation both arguments must be positive.
        '    The integral is evaluated by either a power series or
        '    continued fraction expansion, depending on the relative
        '    values of a and x.
        '
        '    ACCURACY:
        '
        '                         Relative error:
        '    arithmetic   domain     # trials      peak         rms
        '       IEEE      0,30       200000       3.6e-14     2.9e-15
        '       IEEE      0,100      300000       9.9e-14     1.5e-14
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1985, 1987, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function incompletegamma(ByVal a As Double, ByVal x As Double) As Double
            Dim result As Double = 0
            Dim igammaepsilon As Double = 0
            Dim ans As Double = 0
            Dim ax As Double = 0
            Dim c As Double = 0
            Dim r As Double = 0
            Dim tmp As Double = 0

            igammaepsilon = 0.000000000000001R
            If x <= 0 Or a <= 0 Then
                result = 0
                Return result
            End If
            If x > 1 And x > a Then
                result = 1 - incompletegammac(a, x)
                Return result
            End If
            ax = a * Math.Log(x) - x - gammaf.lngamma(a, tmp)
            If ax < -709.782712893384R Then
                result = 0
                Return result
            End If
            ax = Math.Exp(ax)
            r = a
            c = 1
            ans = 1
            Do
                r = r + 1
                c = c * x / r
                ans = ans + c
            Loop While c / ans > igammaepsilon
            result = ans * ax / a
            Return result
        End Function

        '************************************************************************
        '    Complemented incomplete gamma integral
        '
        '    The function is defined by
        '
        '
        '     igamc(a,x)   =   1 - igam(a,x)
        '
        '                               inf.
        '                                 -
        '                        1       | |  -t  a-1
        '                  =   -----     |   e   t   dt.
        '                       -      | |
        '                      | (a)    -
        '                                x
        '
        '
        '    In this implementation both arguments must be positive.
        '    The integral is evaluated by either a power series or
        '    continued fraction expansion, depending on the relative
        '    values of a and x.
        '
        '    ACCURACY:
        '
        '    Tested at random a, x.
        '                   a         x                      Relative error:
        '    arithmetic   domain   domain     # trials      peak         rms
        '       IEEE     0.5,100   0,100      200000       1.9e-14     1.7e-15
        '       IEEE     0.01,0.5  0,100      200000       1.4e-13     1.6e-15
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1985, 1987, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function incompletegammac(ByVal a As Double, ByVal x As Double) As Double
            Dim result As Double = 0
            Dim igammaepsilon As Double = 0
            Dim igammabignumber As Double = 0
            Dim igammabignumberinv As Double = 0
            Dim ans As Double = 0
            Dim ax As Double = 0
            Dim c As Double = 0
            Dim yc As Double = 0
            Dim r As Double = 0
            Dim t As Double = 0
            Dim y As Double = 0
            Dim z As Double = 0
            Dim pk As Double = 0
            Dim pkm1 As Double = 0
            Dim pkm2 As Double = 0
            Dim qk As Double = 0
            Dim qkm1 As Double = 0
            Dim qkm2 As Double = 0
            Dim tmp As Double = 0

            igammaepsilon = 0.000000000000001R
            igammabignumber = 4.5035996273705E+15
            igammabignumberinv = 2.22044604925031R * 0.0000000000000001R
            If x <= 0 Or a <= 0 Then
                result = 1
                Return result
            End If
            If x < 1 Or x < a Then
                result = 1 - incompletegamma(a, x)
                Return result
            End If
            ax = a * Math.Log(x) - x - gammaf.lngamma(a, tmp)
            If ax < -709.782712893384R Then
                result = 0
                Return result
            End If
            ax = Math.Exp(ax)
            y = 1 - a
            z = x + y + 1
            c = 0
            pkm2 = 1
            qkm2 = x
            pkm1 = x + 1
            qkm1 = z * x
            ans = pkm1 / qkm1
            Do
                c = c + 1
                y = y + 1
                z = z + 2
                yc = y * c
                pk = pkm1 * z - pkm2 * yc
                qk = qkm1 * z - qkm2 * yc
                If qk <> 0 Then
                    r = pk / qk
                    t = Math.Abs((ans - r) / r)
                    ans = r
                Else
                    t = 1
                End If
                pkm2 = pkm1
                pkm1 = pk
                qkm2 = qkm1
                qkm1 = qk
                If Math.Abs(pk) > igammabignumber Then
                    pkm2 = pkm2 * igammabignumberinv
                    pkm1 = pkm1 * igammabignumberinv
                    qkm2 = qkm2 * igammabignumberinv
                    qkm1 = qkm1 * igammabignumberinv
                End If
            Loop While t > igammaepsilon
            result = ans * ax
            Return result
        End Function

        '************************************************************************
        '    Inverse of complemented imcomplete gamma integral
        '
        '    Given p, the function finds x such that
        '
        '     igamc( a, x ) = p.
        '
        '    Starting with the approximate value
        '
        '            3
        '     x = a t
        '
        '     where
        '
        '     t = 1 - d - ndtri(p) sqrt(d)
        '
        '    and
        '
        '     d = 1/9a,
        '
        '    the routine performs up to 10 Newton iterations to find the
        '    root of igamc(a,x) - p = 0.
        '
        '    ACCURACY:
        '
        '    Tested at random a, p in the intervals indicated.
        '
        '                   a        p                      Relative error:
        '    arithmetic   domain   domain     # trials      peak         rms
        '       IEEE     0.5,100   0,0.5       100000       1.0e-14     1.7e-15
        '       IEEE     0.01,0.5  0,0.5       100000       9.0e-14     3.4e-15
        '       IEEE    0.5,10000  0,0.5        20000       2.3e-13     3.8e-14
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function invincompletegammac(ByVal a As Double, ByVal y0 As Double) As Double
            Dim result As Double = 0
            Dim igammaepsilon As Double = 0
            Dim iinvgammabignumber As Double = 0
            Dim x0 As Double = 0
            Dim x1 As Double = 0
            Dim x As Double = 0
            Dim yl As Double = 0
            Dim yh As Double = 0
            Dim y As Double = 0
            Dim d As Double = 0
            Dim lgm As Double = 0
            Dim dithresh As Double = 0
            Dim i As Integer = 0
            Dim dir As Integer = 0
            Dim tmp As Double = 0

            igammaepsilon = 0.000000000000001R
            iinvgammabignumber = 4.5035996273705E+15
            x0 = iinvgammabignumber
            yl = 0
            x1 = 0
            yh = 1
            dithresh = 5 * igammaepsilon
            d = 1 / (9 * a)
            y = 1 - d - normaldistr.invnormaldistribution(y0) * Math.Sqrt(d)
            x = a * y * y * y
            lgm = gammaf.lngamma(a, tmp)
            i = 0
            While i < 10
                If x > x0 Or x < x1 Then
                    d = 0.0625
                    Exit While
                End If
                y = incompletegammac(a, x)
                If y < yl Or y > yh Then
                    d = 0.0625
                    Exit While
                End If
                If y < y0 Then
                    x0 = x
                    yl = y
                Else
                    x1 = x
                    yh = y
                End If
                d = (a - 1) * Math.Log(x) - x - lgm
                If d < -709.782712893384R Then
                    d = 0.0625
                    Exit While
                End If
                d = -Math.Exp(d)
                d = (y - y0) / d
                If Math.Abs(d / x) < igammaepsilon Then
                    result = x
                    Return result
                End If
                x = x - d
                i = i + 1
            End While
            If x0 = iinvgammabignumber Then
                If x <= 0 Then
                    x = 1
                End If
                While x0 = iinvgammabignumber
                    x = (1 + d) * x
                    y = incompletegammac(a, x)
                    If y < y0 Then
                        x0 = x
                        yl = y
                        Exit While
                    End If
                    d = d + d
                End While
            End If
            d = 0.5
            dir = 0
            i = 0
            While i < 400
                x = x1 + d * (x0 - x1)
                y = incompletegammac(a, x)
                lgm = (x0 - x1) / (x1 + x0)
                If Math.Abs(lgm) < dithresh Then
                    Exit While
                End If
                lgm = (y - y0) / y0
                If Math.Abs(lgm) < dithresh Then
                    Exit While
                End If
                If x <= 0.0R Then
                    Exit While
                End If
                If y >= y0 Then
                    x1 = x
                    yh = y
                    If dir < 0 Then
                        dir = 0
                        d = 0.5
                    Else
                        If dir > 1 Then
                            d = 0.5 * d + 0.5
                        Else
                            d = (y0 - yl) / (yh - yl)
                        End If
                    End If
                    dir = dir + 1
                Else
                    x0 = x
                    yl = y
                    If dir > 0 Then
                        dir = 0
                        d = 0.5
                    Else
                        If dir < -1 Then
                            d = 0.5 * d
                        Else
                            d = (y0 - yl) / (yh - yl)
                        End If
                    End If
                    dir = dir - 1
                End If
                i = i + 1
            End While
            result = x
            Return result
        End Function

    End Class

    Public Class gammaf

        '************************************************************************
        '    Gamma function
        '
        '    Input parameters:
        '        X   -   argument
        '
        '    Domain:
        '        0 < X < 171.6
        '        -170 < X < 0, X is not an integer.
        '
        '    Relative error:
        '     arithmetic   domain     # trials      peak         rms
        '        IEEE    -170,-33      20000       2.3e-15     3.3e-16
        '        IEEE     -33,  33     20000       9.4e-16     2.2e-16
        '        IEEE      33, 171.6   20000       2.3e-15     3.2e-16
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Original copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
        '    Translated to AlgoPascal by Bochkanov Sergey (2005, 2006, 2007).
        '    ************************************************************************

        Public Shared Function gamma(ByVal x As Double) As Double
            Dim result As Double = 0
            Dim p As Double = 0
            Dim pp As Double = 0
            Dim q As Double = 0
            Dim qq As Double = 0
            Dim z As Double = 0
            Dim i As Integer = 0
            Dim sgngam As Double = 0

            sgngam = 1
            q = Math.Abs(x)
            If q > 33.0R Then
                If x < 0.0R Then
                    p = Convert.ToInt32(Math.Floor(q))
                    i = Convert.ToInt32(Math.Round(p))
                    If i Mod 2 = 0 Then
                        sgngam = -1
                    End If
                    z = q - p
                    If z > 0.5 Then
                        p = p + 1
                        z = q - p
                    End If
                    z = q * Math.Sin(Math.PI * z)
                    z = Math.Abs(z)
                    z = Math.PI / (z * gammastirf(q))
                Else
                    z = gammastirf(x)
                End If
                result = sgngam * z
                Return result
            End If
            z = 1
            While x >= 3
                x = x - 1
                z = z * x
            End While
            While x < 0
                If x > -0.000000001R Then
                    result = z / ((1 + 0.577215664901533R * x) * x)
                    Return result
                End If
                z = z / x
                x = x + 1
            End While
            While x < 2
                If x < 0.000000001R Then
                    result = z / ((1 + 0.577215664901533R * x) * x)
                    Return result
                End If
                z = z / x
                x = x + 1.0R
            End While
            If x = 2 Then
                result = z
                Return result
            End If
            x = x - 2.0R
            pp = 0.000160119522476752R
            pp = 0.00119135147006586R + x * pp
            pp = 0.0104213797561762R + x * pp
            pp = 0.0476367800457137R + x * pp
            pp = 0.207448227648436R + x * pp
            pp = 0.494214826801497R + x * pp
            pp = 1.0R + x * pp
            qq = -0.000023158187332412R
            qq = 0.000539605580493303R + x * qq
            qq = -0.00445641913851797R + x * qq
            qq = 0.011813978522206R + x * qq
            qq = 0.0358236398605499R + x * qq
            qq = -0.234591795718243R + x * qq
            qq = 0.0714304917030273R + x * qq
            qq = 1.0R + x * qq
            result = z * pp / qq
            Return result
            Return result
        End Function

        '************************************************************************
        '    Natural logarithm of gamma function
        '
        '    Input parameters:
        '        X       -   argument
        '
        '    Result:
        '        logarithm of the absolute value of the Gamma(X).
        '
        '    Output parameters:
        '        SgnGam  -   sign(Gamma(X))
        '
        '    Domain:
        '        0 < X < 2.55e305
        '        -2.55e305 < X < 0, X is not an integer.
        '
        '    ACCURACY:
        '    arithmetic      domain        # trials     peak         rms
        '       IEEE    0, 3                 28000     5.4e-16     1.1e-16
        '       IEEE    2.718, 2.556e305     40000     3.5e-16     8.3e-17
        '    The error criterion was relative when the function magnitude
        '    was greater than one but absolute when it was less than one.
        '
        '    The following test used the relative error criterion, though
        '    at certain points the relative error could be much higher than
        '    indicated.
        '       IEEE    -200, -4             10000     4.8e-16     1.3e-16
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
        '    Translated to AlgoPascal by Bochkanov Sergey (2005, 2006, 2007).
        '    ************************************************************************

        Public Shared Function lngamma(ByVal x As Double, ByRef sgngam As Double) As Double
            Dim result As Double = 0
            Dim a As Double = 0
            Dim b As Double = 0
            Dim c As Double = 0
            Dim p As Double = 0
            Dim q As Double = 0
            Dim u As Double = 0
            Dim w As Double = 0
            Dim z As Double = 0
            Dim i As Integer = 0
            Dim logpi As Double = 0
            Dim ls2pi As Double = 0
            Dim tmp As Double = 0

            sgngam = 1
            logpi = 1.1447298858494R
            ls2pi = 0.918938533204673R
            If x < -34.0R Then
                q = -x
                w = lngamma(q, tmp)
                p = Convert.ToInt32(Math.Floor(q))
                i = Convert.ToInt32(Math.Round(p))
                If i Mod 2 = 0 Then
                    sgngam = -1
                Else
                    sgngam = 1
                End If
                z = q - p
                If z > 0.5 Then
                    p = p + 1
                    z = p - q
                End If
                z = q * Math.Sin(Math.PI * z)
                result = logpi - Math.Log(z) - w
                Return result
            End If
            If x < 13 Then
                z = 1
                p = 0
                u = x
                While u >= 3
                    p = p - 1
                    u = x + p
                    z = z * u
                End While
                While u < 2
                    z = z / u
                    p = p + 1
                    u = x + p
                End While
                If z < 0 Then
                    sgngam = -1
                    z = -z
                Else
                    sgngam = 1
                End If
                If u = 2 Then
                    result = Math.Log(z)
                    Return result
                End If
                p = p - 2
                x = x + p
                b = -1378.25152569121R
                b = -38801.6315134638R + x * b
                b = -331612.992738871R + x * b
                b = -1162370.97492762R + x * b
                b = -1721737.0082084R + x * b
                b = -853555.664245765R + x * b
                c = 1
                c = -351.815701436523R + x * c
                c = -17064.2106651881R + x * c
                c = -220528.590553854R + x * c
                c = -1139334.44367983R + x * c
                c = -2532523.07177583R + x * c
                c = -2018891.41433533R + x * c
                p = x * b / c
                result = Math.Log(z) + p
                Return result
            End If
            q = (x - 0.5) * Math.Log(x) - x + ls2pi
            If x > 100000000 Then
                result = q
                Return result
            End If
            p = 1 / (x * x)
            If x >= 1000.0R Then
                q = q + ((7.93650793650794R * 0.0001 * p - 2.77777777777778R * 0.001) * p + 0.0833333333333333R) / x
            Else
                a = 8.11614167470508R * 0.0001
                a = -(5.95061904284301R * 0.0001) + p * a
                a = 7.93650340457717R * 0.0001 + p * a
                a = -(2.777777777301R * 0.001) + p * a
                a = 8.33333333333332R * 0.01 + p * a
                q = q + a / x
            End If
            result = q
            Return result
        End Function

        Private Shared Function gammastirf(ByVal x As Double) As Double
            Dim result As Double = 0
            Dim y As Double = 0
            Dim w As Double = 0
            Dim v As Double = 0
            Dim stir As Double = 0

            w = 1 / x
            stir = 0.000787311395793094R
            stir = -0.000229549961613378R + w * stir
            stir = -0.00268132617805781R + w * stir
            stir = 0.00347222221605459R + w * stir
            stir = 0.0833333333333482R + w * stir
            w = 1 + w * stir
            y = Math.Exp(x)
            If x > 143.01608 Then
                v = Math.Pow(x, 0.5 * x - 0.25)
                y = v * (v / y)
            Else
                y = Math.Pow(x, x - 0.5) / y
            End If
            result = 2.506628274631R * y * w
            Return result
        End Function

    End Class

    Class normaldistr
        '************************************************************************
        '    Error function
        '
        '    The integral is
        '
        '                              x
        '                               -
        '                    2         | |          2
        '      erf(x)  =  --------     |    exp( - t  ) dt.
        '                 sqrt(pi)   | |
        '                             -
        '                              0
        '
        '    For 0 <= |x| < 1, erf(x) = x * P4(x**2)/Q5(x**2); otherwise
        '    erf(x) = 1 - erfc(x).
        '
        '
        '    ACCURACY:
        '
        '                         Relative error:
        '    arithmetic   domain     # trials      peak         rms
        '       IEEE      0,1         30000       3.7e-16     1.0e-16
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function erf(ByVal x As Double) As Double
            Dim result As Double = 0
            Dim xsq As Double = 0
            Dim s As Double = 0
            Dim p As Double = 0
            Dim q As Double = 0

            s = Math.Sign(x)
            x = Math.Abs(x)
            If x < 0.5 Then
                xsq = x * x
                p = 0.00754772803341863R
                p = 0.288805137207594R + xsq * p
                p = 14.3383842191748R + xsq * p
                p = 38.0140318123903R + xsq * p
                p = 3017.82788536508R + xsq * p
                p = 7404.07142710151R + xsq * p
                p = 80437.363096084R + xsq * p
                q = 0.0R
                q = 1.0R + xsq * q
                q = 38.0190713951939R + xsq * q
                q = 658.07015545924R + xsq * q
                q = 6379.60017324428R + xsq * q
                q = 34216.5257924629R + xsq * q
                q = 80437.363096084R + xsq * q
                result = s * 1.12837916709551R * x * p / q
                Return result
            End If
            If x >= 10 Then
                result = s
                Return result
            End If
            result = s * (1 - erfc(x))
            Return result
        End Function


        '************************************************************************
        '    Complementary error function
        '
        '     1 - erf(x) =
        '
        '                              inf.
        '                                -
        '                     2         | |          2
        '      erfc(x)  =  --------     |    exp( - t  ) dt
        '                  sqrt(pi)   | |
        '                              -
        '                               x
        '
        '
        '    For small x, erfc(x) = 1 - erf(x); otherwise rational
        '    approximations are computed.
        '
        '
        '    ACCURACY:
        '
        '                         Relative error:
        '    arithmetic   domain     # trials      peak         rms
        '       IEEE      0,26.6417   30000       5.7e-14     1.5e-14
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function erfc(ByVal x As Double) As Double
            Dim result As Double = 0
            Dim p As Double = 0
            Dim q As Double = 0

            If x < 0 Then
                result = 2 - erfc(-x)
                Return result
            End If
            If x < 0.5 Then
                result = 1.0R - erf(x)
                Return result
            End If
            If x >= 10 Then
                result = 0
                Return result
            End If
            p = 0.0R
            p = 0.56418778255074R + x * p
            p = 9.67580788298727R + x * p
            p = 77.0816173036843R + x * p
            p = 368.519615471001R + x * p
            p = 1143.26207070389R + x * p
            p = 2320.43959025164R + x * p
            p = 2898.02932921677R + x * p
            p = 1826.33488422951R + x * p
            q = 1.0R
            q = 17.1498094362761R + x * q
            q = 137.125596050062R + x * q
            q = 661.736120710765R + x * q
            q = 2094.38436778954R + x * q
            q = 4429.61280388368R + x * q
            q = 6089.54242327244R + x * q
            q = 4958.82756472114R + x * q
            q = 1826.33488422951R + x * q
            result = Math.Exp(-AP.MathEx.Sqr(x)) * p / q
            Return result
        End Function


        '************************************************************************
        '    Normal distribution function
        '
        '    Returns the area under the Gaussian probability density
        '    function, integrated from minus infinity to x:
        '
        '                               x
        '                                -
        '                      1        | |          2
        '       ndtr(x)  = ---------    |    exp( - t /2 ) dt
        '                  sqrt(2pi)  | |
        '                              -
        '                             -inf.
        '
        '                =  ( 1 + erf(z) ) / 2
        '                =  erfc(z) / 2
        '
        '    where z = x/sqrt(2). Computation is via the functions
        '    erf and erfc.
        '
        '
        '    ACCURACY:
        '
        '                         Relative error:
        '    arithmetic   domain     # trials      peak         rms
        '       IEEE     -13,0        30000       3.4e-14     6.7e-15
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function normaldistribution(ByVal x As Double) As Double
            Dim result As Double = 0

            result = 0.5 * (erf(x / 1.4142135623731R) + 1)
            Return result
        End Function


        '************************************************************************
        '    Inverse of the error function
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function inverf(ByVal e As Double) As Double
            Dim result As Double = 0

            result = invnormaldistribution(0.5 * (e + 1)) / Math.Sqrt(2)
            Return result
        End Function


        '************************************************************************
        '    Inverse of Normal distribution function
        '
        '    Returns the argument, x, for which the area under the
        '    Gaussian probability density function (integrated from
        '    minus infinity to x) is equal to y.
        '
        '
        '    For small arguments 0 < y < exp(-2), the program computes
        '    z = sqrt( -2.0 * log(y) );  then the approximation is
        '    x = z - log(z)/z  - (1/z) P(1/z) / Q(1/z).
        '    There are two rational functions P/Q, one for 0 < y < exp(-32)
        '    and the other for y up to exp(-2).  For larger arguments,
        '    w = y - 0.5, and  x/sqrt(2pi) = w + w**3 R(w**2)/S(w**2)).
        '
        '    ACCURACY:
        '
        '                         Relative error:
        '    arithmetic   domain        # trials      peak         rms
        '       IEEE     0.125, 1        20000       7.2e-16     1.3e-16
        '       IEEE     3e-308, 0.135   50000       4.6e-16     9.8e-17
        '
        '    Cephes Math Library Release 2.8:  June, 2000
        '    Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
        '    ************************************************************************

        Public Shared Function invnormaldistribution(ByVal y0 As Double) As Double
            Dim result As Double = 0
            Dim expm2 As Double = 0
            Dim s2pi As Double = 0
            Dim x As Double = 0
            Dim y As Double = 0
            Dim z As Double = 0
            Dim y2 As Double = 0
            Dim x0 As Double = 0
            Dim x1 As Double = 0
            Dim code As Integer = 0
            Dim p0 As Double = 0
            Dim q0 As Double = 0
            Dim p1 As Double = 0
            Dim q1 As Double = 0
            Dim p2 As Double = 0
            Dim q2 As Double = 0

            expm2 = 0.135335283236613R
            s2pi = 2.506628274631R
            If y0 <= 0 Then
                result = -AP.MathEx.MaxRealNumber
                Return result
            End If
            If y0 >= 1 Then
                result = AP.MathEx.MaxRealNumber
                Return result
            End If
            code = 1
            y = y0
            If y > 1.0R - expm2 Then
                y = 1.0R - y
                code = 0
            End If
            If y > expm2 Then
                y = y - 0.5
                y2 = y * y
                p0 = -59.9633501014108R
                p0 = 98.0010754186R + y2 * p0
                p0 = -56.676285746907R + y2 * p0
                p0 = 13.931260938728R + y2 * p0
                p0 = -1.23916583867381R + y2 * p0
                q0 = 1
                q0 = 1.95448858338142R + y2 * q0
                q0 = 4.67627912898882R + y2 * q0
                q0 = 86.3602421390891R + y2 * q0
                q0 = -225.462687854119R + y2 * q0
                q0 = 200.260212380061R + y2 * q0
                q0 = -82.0372256168333R + y2 * q0
                q0 = 15.9056225126212R + y2 * q0
                q0 = -1.1833162112133R + y2 * q0
                x = y + y * y2 * p0 / q0
                x = x * s2pi
                result = x
                Return result
            End If
            x = Math.Sqrt(-(2.0R * Math.Log(y)))
            x0 = x - Math.Log(x) / x
            z = 1.0R / x
            If x < 8.0R Then
                p1 = 4.05544892305962R
                p1 = 31.5251094599894R + z * p1
                p1 = 57.1628192246421R + z * p1
                p1 = 44.0805073893201R + z * p1
                p1 = 14.6849561928858R + z * p1
                p1 = 2.1866330685079R + z * p1
                p1 = -(1.40256079171355R * 0.1) + z * p1
                p1 = -(3.50424626827848R * 0.01) + z * p1
                p1 = -(8.57456785154685R * 0.0001) + z * p1
                q1 = 1
                q1 = 15.7799883256467R + z * q1
                q1 = 45.3907635128879R + z * q1
                q1 = 41.3172038254672R + z * q1
                q1 = 15.0425385692908R + z * q1
                q1 = 2.50464946208309R + z * q1
                q1 = -(1.42182922854788R * 0.1) + z * q1
                q1 = -(3.80806407691578R * 0.01) + z * q1
                q1 = -(9.33259480895457R * 0.0001) + z * q1
                x1 = z * p1 / q1
            Else
                p2 = 3.23774891776946R
                p2 = 6.91522889068984R + z * p2
                p2 = 3.93881025292474R + z * p2
                p2 = 1.33303460815808R + z * p2
                p2 = 2.01485389549179R * 0.1 + z * p2
                p2 = 1.2371663481782R * 0.01 + z * p2
                p2 = 3.01581553508235R * 0.0001 + z * p2
                p2 = 2.65806974686738R * 0.000001R + z * p2
                p2 = 6.23974539184983R * 0.000000001R + z * p2
                q2 = 1
                q2 = 6.02427039364742R + z * q2
                q2 = 3.67983563856161R + z * q2
                q2 = 1.37702099489081R + z * q2
                q2 = 2.16236993594497R * 0.1 + z * q2
                q2 = 1.34204006088543R * 0.01 + z * q2
                q2 = 3.28014464682128R * 0.0001 + z * q2
                q2 = 2.89247864745381R * 0.000001R + z * q2
                q2 = 6.79019408009981R * 0.000000001R + z * q2
                x1 = z * p2 / q2
            End If
            x = x0 - x1
            If code <> 0 Then
                x = -x
            End If
            result = x
            Return result
        End Function
    End Class


End Namespace
