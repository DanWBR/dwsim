Namespace DWSIM.MathEx.ODESolver

    Public Class bulirschstoer

        Public fc As fdelegate
        Delegate Sub fdelegate(ByVal y As Double(), ByRef dy As Double())

        Sub New()

        End Sub

        Sub DefineFuncDelegate(ByVal func As fdelegate)
            Me.fc = func
        End Sub

        Sub f(ByVal y As Double(), ByRef dy As Double())
            fc.Invoke(y, dy)
        End Sub

        Public Sub solvesystembulirschstoer(ByVal x1 As Double, ByVal x2 As Double, ByRef y As Double(), ByVal n As Integer, ByVal h As Double, ByVal err As Double, ByVal isabserr As Boolean)
            Dim ya As Double() = New Double() {}
            Dim yl As Double() = New Double() {}
            Dim ym As Double() = New Double() {}
            Dim dy As Double() = New Double() {}
            Dim dz As Double() = New Double() {}
            Dim s As Double() = New Double() {}
            Dim dt(,) As Double
            Dim d As Double()
            Dim yg(,) As Double
            Dim yh(,) As Double
            Dim konv As Boolean = False
            Dim bo As Boolean = False
            Dim bh As Boolean = False
            Dim fin As Boolean = False
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim k As Integer = 0
            Dim kk As Integer = 0
            Dim l As Integer = 0
            Dim m As Integer = 0
            Dim jj As Integer = 0
            Dim r As Integer = 0
            Dim sr As Integer = 0
            Dim ta As Double = 0
            Dim xxfin As Double = 0
            Dim a As Double = 0
            Dim b As Double = 0
            Dim c As Double = 0
            Dim b1 As Double = 0
            Dim fc As Double = 0
            Dim g As Double = 0
            Dim ho As Double = 0
            Dim u As Double = 0
            Dim v As Double = 0
            Dim cycleflag As Integer = 0
            ya = New Double((n + 1)) {}
            yl = New Double((n + 1)) {}
            ym = New Double((n + 1)) {}
            dy = New Double((n + 1)) {}
            dz = New Double((n + 1)) {}
            s = New Double((n + 1)) {}
            dt = New Double((n + 1), 8) {}
            d = New Double(8) {}
            yg = New Double(11, (n + 1)) {}
            yh = New Double(111, (n + 1)) {}
            xxfin = x2
            cycleflag = -1
            Do While True
                If ((cycleflag = -1) Or (cycleflag = 22)) Then
                    Me.f(y, (dz))
                    bh = False
                    fin = False
                    i = 1
                    Do While (i <= n)
                        s(i) = 0
                        ya(i) = y(i)
                        k = 1
                        Do While (k <= 7)
                            dt(i, k) = 0
                            k += 1
                        Loop
                        i += 1
                    Loop
                    ho = h
                    If ((x1 + h) >= xxfin) Then
                        ho = (x2 - x1)
                        fin = True
                    End If
                End If
                If (((cycleflag = -1) Or (cycleflag = 20)) Or (cycleflag = 22)) Then
                    a = (ho + x1)
                    If (a = x1) Then
                        a = a
                        Return
                    End If
                    fc = 1.5
                    bo = False
                    m = 1
                    r = 2
                    sr = 3
                    jj = 0
                End If
                cycleflag = -1
                j = 1
                Do While (j <= 10)
                    If bo Then
                        d(2) = 1.7777777777778
                        d(4) = 7.1111111111111
                        d(6) = 28.444444444444
                    Else
                        d(2) = 2.25
                        d(4) = 9
                        d(6) = 36
                    End If
                    konv = False
                    If (j > 3) Then
                        konv = True
                    End If
                    If (j > 7) Then
                        l = 7
                        d(7) = 64
                        fc = (0.6 * fc)
                    Else
                        l = j
                        d(l) = (m * m)
                    End If
                    m = (m * 2)
                    g = (ho / CDbl(m))
                    b = (g * 2)
                    If (bh And (j < 9)) Then
                        i = 1
                        Do While (i <= n)
                            ym(i) = yh(j, i)
                            yl(i) = yg(j, i)
                            i += 1
                        Loop
                    Else
                        kk = ((m - 2) / 2)
                        m -= 1
                        i = 1
                        Do While (i <= n)
                            yl(i) = ya(i)
                            ym(i) = (ya(i) + (g * dz(i)))
                            i += 1
                        Loop
                        k = 1
                        Do While (k <= m)
                            Me.f(ym, (dy))
                            i = 1
                            Do While (i <= n)
                                u = (yl(i) + (b * dy(i)))
                                yl(i) = ym(i)
                                ym(i) = u
                                u = Math.Abs(u)
                                If (u > s(i)) Then
                                    s(i) = u
                                End If
                                i += 1
                            Loop
                            If ((k = kk) And (k <> 2)) Then
                                jj = (1 + jj)
                                i = 1
                                Do While (i <= n)
                                    yh(jj, i) = ym(i)
                                    yg(jj, i) = yl(i)
                                    i += 1
                                Loop
                            End If
                            k += 1
                        Loop
                    End If
                    Me.f(ym, (dy))
                    i = 1
                    Do While (i <= n)
                        v = dt(i, 1)
                        dt(i, 1) = (((ym(i) + yl(i)) + (g * dy(i))) * 0.5)
                        c = dt(i, 1)
                        ta = c
                        If (l >= 2) Then
                            k = 2
                            Do While (k <= l)
                                b1 = (d(k) * v)
                                b = (b1 - c)
                                u = v
                                If (b <> 0) Then
                                    b = ((c - v) / b)
                                    u = (c * b)
                                    c = (b1 * b)
                                End If
                                v = dt(i, k)
                                dt(i, k) = u
                                ta = (u + ta)
                                k += 1
                            Loop
                        End If
                        If (Not isabserr And (Math.Abs(CDbl((y(i) - ta))) > (err * s(i)))) Then
                            konv = False
                        End If
                        If (isabserr And (Math.Abs(CDbl((y(i) - ta))) > err)) Then
                            konv = False
                        End If
                        y(i) = ta
                        i += 1
                    Loop
                    If konv Then
                        x1 = a
                        h = (fc * h)
                        If fin Then
                            Return
                        End If
                        cycleflag = 22
                        Exit Do
                    End If
                    d(3) = 4
                    d(5) = 16
                    bo = Not bo
                    m = r
                    r = sr
                    sr = (m * 2)
                    j += 1
                Loop
                If (cycleflag = -1) Then
                    bh = Not bh
                    ho = (ho * 0.5)
                    h = ho
                    fin = False
                    cycleflag = 20
                End If
            Loop
        End Sub

    End Class

End Namespace