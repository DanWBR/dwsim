Namespace DWSIM.MathEx.LBFGS

    Public Class lbfgs

        Delegate Sub funcgraddelegate(ByVal x As Double(), ByRef f As Double, ByRef g As Double())
        Delegate Sub newiterdelegate(ByRef x As Double(), ByVal f As Double, ByRef g As Double(), ByRef abort As Boolean)
        Public fc As funcgraddelegate
        Public fc2 As newiterdelegate

        Sub New()

        End Sub

        Sub DefineFuncGradDelegate(ByVal fg As funcgraddelegate)
            Me.fc = fg
        End Sub

        Sub funcgrad(ByVal x As Double(), ByRef f As Double, ByRef g As Double())
            fc.Invoke(x, f, g)
        End Sub

        Sub DefineNewIterDelegate(ByVal fg As newiterdelegate)
            Me.fc2 = fg
        End Sub

        Private Sub lbfgsnewiteration(ByRef x As Double(), ByVal f As Double, ByRef g As Double(), ByRef abort As Boolean)
            fc2.Invoke(x, f, g, abort)
        End Sub

        '/*************************************************************************
        '        LIMITED MEMORY BFGS METHOD FOR LARGE SCALE OPTIMIZATION
        '                          JORGE NOCEDAL

        'The subroutine minimizes function F(x) of N arguments by  using  a  quasi-
        'Newton method (LBFGS scheme) which is optimized to use  a  minimum  amount
        'of memory.

        'The subroutine generates the approximation of an inverse Hessian matrix by
        'using information about the last M steps of the algorithm  (instead of N).
        'It lessens a required amount of memory from a value  of  order  N^2  to  a
        'value of order 2*N*M.

        'This subroutine uses the FuncGrad subroutine which calculates the value of
        'the function F and gradient G in point X. The programmer should define the
        'FuncGrad subroutine by himself.  It  should  be  noted that the subroutine
        'doesn't need to waste time for memory allocation of array G,  because  the
        'memory is allocated in calling the subroutine. Setting a dimension of array
        'G  each  time  when  calling  a  subroutine  will excessively slow down an
        'algorithm.

        'The programmer could also redefine the LBFGSNewIteration subroutine  which
        'is called on each new step. The current point X, the function value F  and
        'the  gradient  G  are  passed  into  this  subroutine. It is reasonable to
        'redefine the subroutine for better debugging, for  example,  to  visualize
        'the solution process.

        'Input parameters:
        '    N   -   problem dimension. N>0
        '    M   -   number of corrections in the BFGS scheme of Hessian
        '            approximation update. Recommended value:  3<=M<=7. The smaller
        '            value causes worse convergence, the bigger will  not  cause  a
        '            considerably better convergence, but will cause a fall in  the
        '            performance. M<=N.
        '    X   -   initial solution approximation.
        '            Array whose index ranges from 1 to N.
        '    EpsG -  positive number which  defines  a  precision  of  search.  The
        '            subroutine finishes its work if the condition ||G|| < EpsG  is
        '            satisfied, where ||.|| means Euclidian norm, G - gradient, X -
        '            current approximation.
        '    EpsF -  positive number which  defines  a  precision  of  search.  The
        '            subroutine finishes its work if on iteration  number  k+1  the
        '            condition |F(k+1)-F(k)| <= EpsF*max{|F(k)|, |F(k+1)|, 1}    is
        '            satisfied.
        '    EpsX -  positive number which  defines  a  precision  of  search.  The
        '            subroutine finishes its work if on iteration number k+1    the
        '            condition |X(k+1)-X(k)| <= EpsX is fulfilled.
        '    MaxIts- maximum number of iterations. If MaxIts=0, the number of
        '            iterations is unlimited.

        'Output parameters:
        '    X   -   solution approximation. Array whose index ranges from 1 to N.
        '    Info-   a return code:
        '                    * -1 wrong parameters were specified,
        '                    * 0 interrupted by user,
        '                    * 1 relative function decreasing is less or equal to EpsF,
        '                    * 2 step is less or equal EpsX,
        '                    * 4 gradient norm is less or equal to EpsG,
        '                    * 5 number of iterations exceeds MaxIts.

        'FuncGrad routine description. User-defined.
        'Input parameters:
        '    X   -   array whose index ranges from 1 to N.
        'Output parameters:
        '    F   -   function value at X.
        '    G   -   function gradient.
        '            Array whose index ranges from 1 to N.
        'The memory for array G has already been allocated in the calling subroutine,
        'and it isn't necessary to allocate it in the FuncGrad subroutine.
        '*************************************************************************/

        Private Function lbfgsdotproduct(ByVal n As Integer, ByRef dx As Double(), ByVal sx As Integer, ByRef dy As Double(), ByVal sy As Integer) As Double
            Dim num2 As Double = 0
            Dim num3 As Integer = 0
            Dim index As Integer = 0
            Dim num5 As Integer = 0
            num3 = ((sx + n) - 1)
            num5 = (sy - sx)
            num2 = 0
            index = sx
            Do While (index <= num3)
                num2 = (num2 + (dx(index) * dy((index + num5))))
                index += 1
            Loop
            Return num2
        End Function

        Private Sub lbfgslincomb(ByVal n As Integer, ByVal da As Double, ByRef dx As Double(), ByVal sx As Integer, ByRef dy As Double(), ByVal sy As Integer)
            Dim num As Integer = 0
            Dim index As Integer = 0
            Dim num3 As Integer = 0
            num = ((sy + n) - 1)
            num3 = (sx - sy)
            index = sy
            Do While (index <= num)
                dy(index) = (dy(index) + (da * dx((index + num3))))
                index += 1
            Loop
        End Sub

        Private Sub lbfgsmcsrch(ByVal n As Integer, ByRef x As Double(), ByRef f As Double, ByRef g As Double(), ByRef s As Double(), ByVal sstart As Integer, ByRef stp As Double, ByVal ftol As Double, ByVal xtol As Double, ByVal maxfev As Integer, ByRef info As Integer, ByRef nfev As Integer, ByRef wa As Double(), ByVal gtol As Double, ByVal stpmin As Double, ByVal stpmax As Double)
            Dim num As Integer = 0
            Dim index As Integer = 0
            Dim brackt As Boolean = False
            Dim flag2 As Boolean = False
            Dim num3 As Double = 0
            Dim dp As Double = 0
            Dim num5 As Double = 0
            Dim num6 As Double = 0
            Dim dx As Double = 0
            Dim num8 As Double = 0
            Dim dy As Double = 0
            Dim num10 As Double = 0
            Dim num11 As Double = 0
            Dim num12 As Double = 0
            Dim fp As Double = 0
            Dim fx As Double = 0
            Dim num15 As Double = 0
            Dim fy As Double = 0
            Dim num17 As Double = 0
            Dim num18 As Double = 0
            Dim num19 As Double = 0
            Dim stx As Double = 0
            Dim sty As Double = 0
            Dim stmin As Double = 0
            Dim stmax As Double = 0
            Dim num24 As Double = 0
            Dim num25 As Double = 0
            Dim num26 As Double = 0
            Dim num27 As Double = 0
            Dim num28 As Double = 0
            sstart -= 1
            num18 = 0.5
            num19 = 0.66
            num26 = 4
            num27 = 0
            Me.funcgrad(x, f, g)
            num = 1
            info = 0
            If ((((((((n <= 0) Or (stp <= 0)) Or (ftol < 0)) Or (gtol < num27)) Or (xtol < num27)) Or (stpmin < num27)) Or (stpmax < stpmin)) Or (maxfev <= 0)) Then
                Return
            End If
            num5 = 0
            index = 1
            Do While (index <= n)
                num5 = (num5 + (g(index) * s((index + sstart))))
                index += 1
            Loop
            If (num5 >= 0) Then
                Return
            End If
            brackt = False
            flag2 = True
            nfev = 0
            num11 = f
            num6 = (ftol * num5)
            num24 = (stpmax - stpmin)
            num25 = (num24 / num18)
            index = 1
            Do While (index <= n)
                wa(index) = x(index)
                index += 1
            Loop
            stx = 0
            fx = num11
            dx = num5
            sty = 0
            fy = num11
            dy = num5
            Do While True
                If brackt Then
                    If (stx < sty) Then
                        stmin = stx
                        stmax = sty
                    Else
                        stmin = sty
                        stmax = stx
                    End If
                Else
                    stmin = stx
                    stmax = (stp + (num26 * (stp - stx)))
                End If
                If (stp > stpmax) Then
                    stp = stpmax
                End If
                If (stp < stpmin) Then
                    stp = stpmin
                End If
                If ((((brackt And ((stp <= stmin) Or (stp >= stmax))) Or (nfev >= (maxfev - 1))) Or (num = 0)) Or (brackt And ((stmax - stmin) <= (xtol * stmax)))) Then
                    stp = stx
                End If
                index = 1
                Do While (index <= n)
                    x(index) = (wa(index) + (stp * s((index + sstart))))
                    index += 1
                Loop
                Me.funcgrad(x, f, g)
                info = 0
                nfev += 1
                num3 = 0
                index = 1
                Do While (index <= n)
                    num3 = (num3 + (g(index) * s((index + sstart))))
                    index += 1
                Loop
                num12 = (num11 + (stp * num6))
                If ((brackt And ((stp <= stmin) Or (stp >= stmax))) Or (num = 0)) Then
                    info = 6
                End If
                If (((stp = stpmax) And (f <= num12)) And (num3 <= num6)) Then
                    info = 5
                End If
                If ((stp = stpmin) And ((f > num12) Or (num3 >= num6))) Then
                    info = 4
                End If
                If (nfev >= maxfev) Then
                    info = 3
                End If
                If (brackt And ((stmax - stmin) <= (xtol * stmax))) Then
                    info = 2
                End If
                If ((f <= num12) And (Math.Abs(num3) <= -(gtol * num5))) Then
                    info = 1
                End If
                If (Not info = 0) Then
                    Return
                End If
                num28 = ftol
                If (gtol < ftol) Then
                    num28 = gtol
                End If
                If ((flag2 And (f <= num12)) And (num3 >= (num28 * num5))) Then
                    flag2 = False
                End If
                If ((flag2 And (f <= fx)) And (f > num12)) Then
                    fp = (f - (stp * num6))
                    num15 = (fx - (stx * num6))
                    num17 = (fy - (sty * num6))
                    dp = (num3 - num6)
                    num8 = (dx - num6)
                    num10 = (dy - num6)
                    Me.lbfgsmcstep(stx, num15, num8, sty, num17, num10, stp, fp, dp, brackt, stmin, stmax, num)
                    fx = (num15 + (stx * num6))
                    fy = (num17 + (sty * num6))
                    dx = (num8 + num6)
                    dy = (num10 + num6)
                Else
                    Me.lbfgsmcstep(stx, fx, dx, sty, fy, dy, stp, f, num3, brackt, stmin, stmax, num)
                End If
                If brackt Then
                    If (Math.Abs(CDbl((sty - stx))) >= (num19 * num25)) Then
                        stp = (stx + (num18 * (sty - stx)))
                    End If
                    num25 = num24
                    num24 = Math.Abs(CDbl((sty - stx)))
                End If
            Loop
        End Sub

        Private Sub lbfgsmcstep(ByRef stx As Double, ByRef fx As Double, ByRef dx As Double, ByRef sty As Double, ByRef fy As Double, ByRef dy As Double, ByRef stp As Double, ByVal fp As Double, ByVal dp As Double, ByRef brackt As Boolean, ByVal stmin As Double, ByVal stmax As Double, ByRef info As Integer)
            Dim flag As Boolean = False
            Dim num As Double = 0
            Dim num2 As Double = 0
            Dim num3 As Double = 0
            Dim num4 As Double = 0
            Dim num5 As Double = 0
            Dim num6 As Double = 0
            Dim num7 As Double = 0
            Dim num8 As Double = 0
            Dim num9 As Double = 0
            Dim num10 As Double = 0
            info = 0
            If Not (((brackt And ((stp <= Math.Min(stx, sty)) Or (stp >= Math.Max(stx, sty)))) Or ((dx * (stp - stx)) >= 0)) Or (stmax < stmin)) Then
                num6 = (dp * (dx / Math.Abs(dx)))
                If (fp > fx) Then
                    info = 1
                    flag = True
                    num10 = ((((3 * (fx - fp)) / (stp - stx)) + dx) + dp)
                    num5 = Math.Max(Math.Abs(num10), Math.Max(Math.Abs(dx), Math.Abs(dp)))
                    num = (num5 * Math.Sqrt((Math.Sqrt((num10 / num5)) - ((dx / num5) * (dp / num5)))))
                    If (stp < stx) Then
                        num = -num
                    End If
                    num2 = ((num - dx) + num10)
                    num3 = (((num - dx) + num) + dp)
                    num4 = (num2 / num3)
                    num7 = (stx + (num4 * (stp - stx)))
                    num9 = (stx + (((dx / (((fx - fp) / (stp - stx)) + dx)) / 2) * (stp - stx)))
                    If (Math.Abs(CDbl((num7 - stx))) < Math.Abs(CDbl((num9 - stx)))) Then
                        num8 = num7
                    Else
                        num8 = (num7 + ((num9 - num7) / 2))
                    End If
                    brackt = True
                ElseIf (num6 < 0) Then
                    info = 2
                    flag = False
                    num10 = ((((3 * (fx - fp)) / (stp - stx)) + dx) + dp)
                    num5 = Math.Max(Math.Abs(num10), Math.Max(Math.Abs(dx), Math.Abs(dp)))
                    num = (num5 * Math.Sqrt((Math.Sqrt((num10 / num5)) - ((dx / num5) * (dp / num5)))))
                    If (stp > stx) Then
                        num = -num
                    End If
                    num2 = ((num - dp) + num10)
                    num3 = (((num - dp) + num) + dx)
                    num4 = (num2 / num3)
                    num7 = (stp + (num4 * (stx - stp)))
                    num9 = (stp + ((dp / (dp - dx)) * (stx - stp)))
                    If (Math.Abs(CDbl((num7 - stp))) > Math.Abs(CDbl((num9 - stp)))) Then
                        num8 = num7
                    Else
                        num8 = num9
                    End If
                    brackt = True
                ElseIf (Math.Abs(dp) < Math.Abs(dx)) Then
                    info = 3
                    flag = True
                    num10 = ((((3 * (fx - fp)) / (stp - stx)) + dx) + dp)
                    num5 = Math.Max(Math.Abs(num10), Math.Max(Math.Abs(dx), Math.Abs(dp)))
                    num = (num5 * Math.Sqrt(Math.Max(CDbl(0), CDbl((Math.Sqrt((num10 / num5)) - ((dx / num5) * (dp / num5)))))))
                    If (stp > stx) Then
                        num = -num
                    End If
                    num2 = ((num - dp) + num10)
                    num3 = ((num + (dx - dp)) + num)
                    num4 = (num2 / num3)
                    If ((num4 < 0) And (num <> 0)) Then
                        num7 = (stp + (num4 * (stx - stp)))
                    ElseIf (stp > stx) Then
                        num7 = stmax
                    Else
                        num7 = stmin
                    End If
                    num9 = (stp + ((dp / (dp - dx)) * (stx - stp)))
                    If brackt Then
                        If (Math.Abs(CDbl((stp - num7))) < Math.Abs(CDbl((stp - num9)))) Then
                            num8 = num7
                        Else
                            num8 = num9
                        End If
                    ElseIf (Math.Abs(CDbl((stp - num7))) > Math.Abs(CDbl((stp - num9)))) Then
                        num8 = num7
                    Else
                        num8 = num9
                    End If
                Else
                    info = 4
                    flag = False
                    If brackt Then
                        num10 = ((((3 * (fp - fy)) / (sty - stp)) + dy) + dp)
                        num5 = Math.Max(Math.Abs(num10), Math.Max(Math.Abs(dy), Math.Abs(dp)))
                        num = (num5 * Math.Sqrt((Math.Sqrt((num10 / num5)) - ((dy / num5) * (dp / num5)))))
                        If (stp > sty) Then
                            num = -num
                        End If
                        num2 = ((num - dp) + num10)
                        num3 = (((num - dp) + num) + dy)
                        num4 = (num2 / num3)
                        num7 = (stp + (num4 * (sty - stp)))
                        num8 = num7
                    ElseIf (stp > stx) Then
                        num8 = stmax
                    Else
                        num8 = stmin
                    End If
                End If
                If (fp > fx) Then
                    sty = stp
                    fy = fp
                    dy = dp
                Else
                    If (num6 < 0) Then
                        sty = stx
                        fy = fx
                        dy = dx
                    End If
                    stx = stp
                    fx = fp
                    dx = dp
                End If
                num8 = Math.Min(stmax, num8)
                num8 = Math.Max(stmin, num8)
                stp = num8
                If (brackt And flag) Then
                    If (sty > stx) Then
                        stp = Math.Min((stx + (0.66 * (sty - stx))), stp)
                    Else
                        stp = Math.Max((stx + (0.66 * (sty - stx))), stp)
                    End If
                End If
            End If
        End Sub

        Public Sub lbfgsminimize(ByVal n As Integer, ByVal m As Integer, ByRef x As Double(), ByVal epsg As Double, ByVal epsf As Double, ByVal epsx As Double, ByVal maxits As Integer, ByRef info As Integer)
            Dim dx As Double() = New Double(0 - 1) {}
            Dim abort As Boolean = False
            Dim f As Double = 0
            Dim num2 As Double = 0
            Dim num3 As Double = 0
            Dim num4 As Double = 0
            Dim numArray2 As Double()
            Dim numArray3 As Double()
            Dim g As Double()
            Dim wa As Double()
            Dim num6 As Double = 0
            Dim num7 As Double = 0
            Dim ftol As Double = 0
            Dim stp As Double = 0
            Dim num10 As Double = 0
            Dim num11 As Double = 0
            Dim num12 As Double = 0
            Dim num13 As Double = 0
            Dim da As Double = 0
            Dim num16 As Integer = 0
            Dim num17 As Integer = 0
            Dim num18 As Integer = 0
            Dim num19 As Integer = 0
            Dim num20 As Integer = 0
            Dim maxfev As Integer = 0
            Dim num22 As Integer = 0
            Dim num23 As Integer = 0
            Dim num24 As Integer = 0
            Dim index As Integer = 0
            Dim nfev As Integer = 0
            Dim num27 As Integer = 0
            Dim num28 As Integer = 0
            Dim num29 As Integer = 0
            Dim xtol As Double = 0
            Dim gtol As Double = 0
            Dim stpmin As Double = 0
            Dim stpmax As Double = 0
            Dim num34 As Integer = 0
            dx = New Double((((n * ((2 * m) + 1)) + (2 * m)) + 1) - 1) {}
            g = New Double((n + 1) - 1) {}
            numArray2 = New Double((n + 1) - 1) {}
            numArray3 = New Double((n + 1) - 1) {}
            wa = New Double((n + 1) - 1) {}
            Me.funcgrad(x, f, g)
            num2 = f
            num16 = 0
            info = 0
            If (((((((n <= 0) Or (m <= 0)) Or (m > n)) Or (epsg < 0)) Or (epsf < 0)) Or (epsx < 0)) Or (maxits < 0)) Then
                info = -1
                Return
            End If
            num17 = 1
            num18 = 0
            index = 1
            Do While (index <= n)
                wa(index) = 1
                index += 1
            Loop
            xtol = 0.00000000000005
            gtol = 0.9
            stpmin = Math.Pow(10, -20)
            stpmax = Math.Pow(10, 20)
            num19 = (n + (2 * m))
            num20 = (num19 + (n * m))
            index = 1
            Do While (index <= n)
                dx((num19 + index)) = -(g(index) * wa(index))
                index += 1
            Loop
            num6 = Math.Sqrt(Me.lbfgsdotproduct(n, g, 1, g, 1))
            num7 = (1 / num6)
            ftol = 0.0001
            maxfev = 20
Label_0289:
            num34 = 1
            Do While (num34 <= n)
                numArray2(num34) = x(num34)
                num34 += 1
            Loop
            num16 += 1
            info = 0
            num22 = (num16 - 1)
            If (num16 <> 1) Then
                If (num16 > m) Then
                    num22 = m
                End If
                num10 = Me.lbfgsdotproduct(n, dx, ((num20 + num23) + 1), dx, ((num19 + num23) + 1))
                num11 = Me.lbfgsdotproduct(n, dx, ((num20 + num23) + 1), dx, ((num20 + num23) + 1))
                index = 1
                Do While (index <= n)
                    wa(index) = (num10 / num11)
                    index += 1
                Loop
                num24 = num18
                If (num18 = 0) Then
                    num24 = m
                End If
                dx((n + num24)) = (1 / num10)
                index = 1
                Do While (index <= n)
                    dx(index) = -g(index)
                    index += 1
                Loop
                num24 = num18
                index = 1
                Do While (index <= num22)
                    num24 -= 1
                    If (num24 = -1) Then
                        num24 = (m - 1)
                    End If
                    num12 = Me.lbfgsdotproduct(n, dx, ((num19 + (num24 * n)) + 1), dx, 1)
                    num27 = (((n + m) + num24) + 1)
                    num28 = (num20 + (num24 * n))
                    dx(num27) = (dx(((n + num24) + 1)) * num12)
                    Me.lbfgslincomb(n, -dx(num27), dx, (num28 + 1), dx, 1)
                    index += 1
                Loop
                index = 1
                Do While (index <= n)
                    dx(index) = (wa(index) * dx(index))
                    index += 1
                Loop
                index = 1
                Do While (index <= num22)
                    num13 = Me.lbfgsdotproduct(n, dx, ((num20 + (num24 * n)) + 1), dx, 1)
                    da = (dx(((n + num24) + 1)) * num13)
                    num27 = (((n + m) + num24) + 1)
                    da = (dx(num27) - da)
                    num29 = (num19 + (num24 * n))
                    Me.lbfgslincomb(n, da, dx, (num29 + 1), dx, 1)
                    num24 += 1
                    If (num24 = m) Then
                        num24 = 0
                    End If
                    index += 1
                Loop
                index = 1
                Do While (index <= n)
                    dx(((num19 + (num18 * n)) + index)) = dx(index)
                    index += 1
                Loop
            End If
            nfev = 0
            stp = 1
            If (num16 = 1) Then
                stp = num7
            End If
            index = 1
            Do While (index <= n)
                dx(index) = g(index)
                index += 1
            Loop
            Me.lbfgsmcsrch(n, x, f, g, dx, ((num19 + (num18 * n)) + 1), stp, ftol, xtol, maxfev, info, nfev, wa, gtol, stpmin, stpmax)
            If ((Not info = 1) AndAlso (info = 0)) Then
                info = -1
            Else
                num17 = (num17 + nfev)
                num23 = (num18 * n)
                index = 1
                Do While (index <= n)
                    dx(((num19 + num23) + index)) = (stp * dx(((num19 + num23) + index)))
                    dx(((num20 + num23) + index)) = (g(index) - dx(index))
                    index += 1
                Loop
                num18 += 1
                If (num18 = m) Then
                    num18 = 0
                End If
                If ((num16 > maxits) And (maxits > 0)) Then
                    info = 5
                Else
                    Me.lbfgsnewiteration(x, f, g, abort)
                    If abort Then Exit Sub
                    If (Math.Sqrt(Me.lbfgsdotproduct(n, g, 1, g, 1)) <= epsg) Then
                        info = 4
                    Else
                        num3 = Math.Max(Math.Abs(num2), Math.Max(Math.Abs(f), 1))
                        If ((num2 - f) <= (epsf * num3)) Then
                            info = 1
                        Else
                            num34 = 1
                            Do While (num34 <= n)
                                numArray3(num34) = numArray2(num34)
                                num34 += 1
                            Loop
                            num34 = 1
                            Do While (num34 <= n)
                                numArray3(num34) = (numArray3(num34) - x(num34))
                                num34 += 1
                            Loop
                            num4 = Math.Max(Math.Max(Math.Sqrt(Me.lbfgsdotproduct(n, x, 1, x, 1)), Math.Sqrt(Me.lbfgsdotproduct(n, numArray2, 1, numArray2, 1))), 1)
                            If (Math.Sqrt(Me.lbfgsdotproduct(n, numArray3, 1, numArray3, 1)) <= epsx) Then
                                info = 2
                            Else
                                num2 = f
                                num34 = 1
                                Do While (num34 <= n)
                                    numArray2(num34) = x(num34)
                                    num34 += 1
                                Loop
                                GoTo Label_0289
                            End If
                        End If
                    End If
                End If
            End If
        End Sub


    End Class

End Namespace
