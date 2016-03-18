Namespace MathEx.LBFGSB

    '/*************************************************************************
    '   NEOS, November 1994. (Latest revision June 1996.)
    '   Optimization Technology Center.
    '   Argonne National Laboratory and Northwestern University.

    '   Written by Ciyou Zhu in collaboration with
    '   R.H. Byrd, P. Lu-Chen and J. Nocedal.

    '   Contributors:
    '       * Sergey Bochkanov (ALGLIB project). Translation from FORTRAN to
    '         pseudocode.

    '   This software is freely available, but we  expect  that  all  publications
    '   describing  work using this software, or all commercial products using it,
    '   quote at least one of the references given below:
    '       * R. H. Byrd, P. Lu and J. Nocedal.  A Limited  Memory  Algorithm  for
    '         Bound Constrained Optimization, (1995), SIAM Journal  on  Scientific
    '         and Statistical Computing , 16, 5, pp. 1190-1208.
    '       * C. Zhu, R.H. Byrd and J. Nocedal. L-BFGS-B: Algorithm 778: L-BFGS-B,
    '         FORTRAN routines for  large  scale  bound  constrained  optimization
    '         (1997), ACM Transactions on Mathematical Software,  Vol 23,  Num. 4,
    '         pp. 550 - 560.
    '   *************************************************************************/

    Public Class lbfgsb

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

        Private Sub lbfgsbnewiteration(ByRef x As Double(), ByVal f As Double, ByRef g As Double(), ByRef abort As Boolean)
            fc2.Invoke(x, f, g, abort)
        End Sub

        '/*************************************************************************
        'The  subroutine  minimizes  the  function  F(x) of N arguments with simple
        'constraints using a quasi-Newton method (LBFGS scheme) which is  optimized
        'to use a minimum amount of memory.

        'The subroutine generates the approximation of an inverse Hessian matrix by
        'using information about the last M steps of the algorithm (instead  of N).
        'It lessens a required amount of memory from a value  of  order  N^2  to  a
        'value of order 2*N*M.

        'This subroutine uses the FuncGrad subroutine which calculates the value of
        'the function F and gradient G in point X. The programmer should define the
        'FuncGrad subroutine by himself.  It should be noted  that  the  subroutine
        'doesn't need to waste  time for memory allocation of array G, because  the
        'memory is allocated in calling the  subroutine.  Setting  a  dimension  of
        'array G each time when calling a subroutine will excessively slow down  an
        'algorithm.

        'The programmer could also redefine the LBFGSNewIteration subroutine  which
        'is called on each new step. The current point X, the function value F  and
        'the gradient G are passed  into  this  subroutine.  It  is  reasonable  to
        'redefine the subroutine for better debugging, for  example,  to  visualize
        'the solution process.

        'Input parameters:
        '    N       -   problem dimension. N>0
        '    M       -   number of  corrections  in  the  BFGS  scheme  of  Hessian
        '                approximation  update.  Recommended value:  3<=M<=7.   The
        '                smaller value causes worse convergence,  the  bigger  will
        '                not  cause  a  considerably  better  convergence, but will
        '                cause a fall in the performance. M<=N.
        '    X       -   initial solution approximation.
        '                Array whose index ranges from 1 to N.
        '    EpsG    -   positive number which defines a precision of  search.  The
        '                subroutine finishes its work if the condition ||G|| < EpsG
        '                is satisfied, where ||.|| means Euclidian norm, G - gradient
        '                projection onto a feasible set, X - current approximation.
        '    EpsF    -   positive number which defines a precision of  search.  The
        '                subroutine  finishes  its  work if on iteration number k+1
        '                the condition |F(k+1)-F(k)| <= EpsF*max{|F(k)|, |F(k+1)|, 1}
        '                is satisfied.
        '    EpsX    -   positive number which defines a precision of  search.  The
        '                subroutine  finishes  its  work if on iteration number k+1
        '                the condition |X(k+1)-X(k)| <= EpsX is satisfied.
        '    MaxIts  -   maximum number of iterations.
        '                If MaxIts=0, the number of iterations is unlimited.
        '    NBD     -   constraint type. If NBD(i) is equal to:
        '                * 0, X(i) has no constraints,
        '                * 1, X(i) has only lower boundary,
        '                * 2, X(i) has both lower and upper boundaries,
        '                * 3, X(i) has only upper boundary,
        '                Array whose index ranges from 1 to N.
        '    L       -   lower boundaries of X(i) variables.
        '                Array whose index ranges from 1 to N.
        '    U       -   upper boundaries of X(i) variables.
        '                Array whose index ranges from 1 to N.

        'Output parameters:
        '    X       -   solution approximation.
        'Array whose index ranges from 1 to N.
        '    Info    -   a return code:
        '                    * -2 unknown internal error,
        '                    * -1 wrong parameters were specified,
        '                    * 0 interrupted by user,
        '                    * 1 relative function decreasing is less or equal to EpsF,
        '                    * 2 step is less or equal to EpsX,
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

        '    NEOS, November 1994. (Latest revision June 1996.)
        '    Optimization Technology Center.
        '    Argonne National Laboratory and Northwestern University.

        '    Written by Ciyou Zhu in collaboration with
        '    R.H. Byrd, P. Lu-Chen and J. Nocedal.
        '*************************************************************************/

        ' Methods
        Private Function additionallbfgsbstoppingcriterion(ByVal iter As Integer, ByRef x As Double(), ByVal f As Double, ByRef g As Double()) As Boolean
            Return False
        End Function

        Private Sub lbfgsbactive(ByVal n As Integer, ByRef l As Double(), ByRef u As Double(), ByRef nbd As Integer(), ByRef x As Double(), ByRef iwhere As Integer(), ByRef prjctd As Boolean, ByRef cnstnd As Boolean, ByRef boxed As Boolean)
            Dim num As Integer = 0
            Dim index As Integer = 0
            num = 0
            prjctd = False
            cnstnd = False
            boxed = True
            index = 1
            Do While (index <= n)
                If (nbd(index) > 0) Then
                    If ((nbd(index) <= 2) And (x(index) <= l(index))) Then
                        If (x(index) < l(index)) Then
                            prjctd = True
                            x(index) = l(index)
                        End If
                        num += 1
                    ElseIf ((nbd(index) >= 2) And (x(index) >= u(index))) Then
                        If (x(index) > u(index)) Then
                            prjctd = True
                            x(index) = u(index)
                        End If
                        num += 1
                    End If
                End If
                index += 1
            Loop
            index = 1
            Do While (index <= n)
                If (nbd(index) <> 2) Then
                    boxed = False
                End If
                If (nbd(index) = 0) Then
                    iwhere(index) = -1
                Else
                    cnstnd = True
                    If ((nbd(index) = 2) And ((u(index) - l(index)) <= 0)) Then
                        iwhere(index) = 3
                    Else
                        iwhere(index) = 0
                    End If
                End If
                index += 1
            Loop
        End Sub

        Private Sub lbfgsbbmv(ByVal m As Integer, ByRef sy As Double(,), ByRef wt As Double(,), ByVal col As Integer, ByRef v As Double(), ByRef p As Double(), ByRef info As Integer, ByRef workvec As Double())
            Dim index As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Double = 0
            Dim num5 As Integer = 0
            Dim num6 As Integer = 0
            If (col <> 0) Then
                p((col + 1)) = v((col + 1))
                index = 2
                Do While (index <= col)
                    num3 = (col + index)
                    num4 = 0
                    num2 = 1
                    Do While (num2 <= (index - 1))
                        num4 = (num4 + ((sy(index, num2) * v(num2)) / sy(num2, num2)))
                        num2 += 1
                    Loop
                    p(num3) = (v(num3) + num4)
                    index += 1
                Loop
                num6 = ((col + 1) - 1)
                num5 = 1
                Do While (num5 <= col)
                    workvec(num5) = p((num5 + num6))
                    num5 += 1
                Loop
                Me.lbfgsbdtrsl(wt, col, workvec, 11, info)
                num6 = (1 - (col + 1))
                num5 = (col + 1)
                Do While (num5 <= (col + col))
                    p(num5) = workvec((num5 + num6))
                    num5 += 1
                Loop
                If (info = 0) Then
                    index = 1
                    Do While (index <= col)
                        p(index) = (v(index) / Math.Sqrt(sy(index, index)))
                        index += 1
                    Loop
                    num6 = ((col + 1) - 1)
                    num5 = 1
                    Do While (num5 <= col)
                        workvec(num5) = p((num5 + num6))
                        num5 += 1
                    Loop
                    Me.lbfgsbdtrsl(wt, col, workvec, 1, info)
                    num6 = (1 - (col + 1))
                    num5 = (col + 1)
                    Do While (num5 <= (col + col))
                        p(num5) = workvec((num5 + num6))
                        num5 += 1
                    Loop
                    If (info = 0) Then
                        index = 1
                        Do While (index <= col)
                            p(index) = -(p(index) / Math.Sqrt(sy(index, index)))
                            index += 1
                        Loop
                        index = 1
                        Do While (index <= col)
                            num4 = 0
                            num2 = (index + 1)
                            Do While (num2 <= col)
                                num4 = (num4 + ((sy(num2, index) * p((col + num2))) / sy(index, index)))
                                num2 += 1
                            Loop
                            p(index) = (p(index) + num4)
                            index += 1
                        Loop
                    End If
                End If
            End If
        End Sub

        Private Sub lbfgsbcauchy(ByVal n As Integer, ByRef x As Double(), ByRef l As Double(), ByRef u As Double(), ByRef nbd As Integer(), ByRef g As Double(), ByRef iorder As Integer(), ByRef iwhere As Integer(), ByRef t As Double(), ByRef d As Double(), ByRef xcp As Double(), ByVal m As Integer, ByRef wy As Double(,), ByRef ws As Double(,), ByRef sy As Double(,), ByRef wt As Double(,), ByVal theta As Double, ByVal col As Integer, ByVal head As Integer, ByRef p As Double(), ByRef c As Double(), ByRef wbp As Double(), ByRef v As Double(), ByRef nint As Integer, ByRef sg As Double(), ByRef yg As Double(), ByVal sbgnrm As Double, ByRef info As Integer, ByRef workvec As Double())
            Dim flag As Boolean = False
            Dim flag2 As Boolean = False
            Dim flag3 As Boolean = False
            Dim index As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Integer = 0
            Dim num6 As Integer = 0
            Dim num7 As Integer = 0
            Dim num8 As Integer = 0
            Dim num9 As Integer = 0
            Dim num10 As Integer = 0
            Dim num11 As Double = 0
            Dim num12 As Double = 0
            Dim num13 As Double = 0
            Dim num14 As Double = 0
            Dim num15 As Double = 0
            Dim num16 As Double = 0
            Dim num17 As Double = 0
            Dim num18 As Double = 0
            Dim num19 As Double = 0
            Dim num20 As Double = 0
            Dim num21 As Double = 0
            Dim num22 As Double = 0
            Dim num23 As Double = 0
            Dim num24 As Double = 0
            Dim num25 As Double = 0
            Dim num26 As Double = 0
            Dim num27 As Double = 0
            Dim num28 As Double = 0
            Dim num29 As Double = 0
            Dim num30 As Integer = 0
            If (sbgnrm <= 0) Then
                num30 = 1
                Do While (num30 <= n)
                    xcp(num30) = x(num30)
                    num30 += 1
                Loop
                Return
            End If
            flag3 = True
            num4 = (n + 1)
            num5 = 0
            num9 = 0
            num19 = 0
            num3 = (2 * col)
            num11 = 0
            index = 1
            Do While (index <= num3)
                p(index) = 0
                index += 1
            Loop
            index = 1
            Do While (index <= n)
                num27 = -g(index)
                If ((iwhere(index) <> 3) And (iwhere(index) <> -1)) Then
                    num21 = 0
                    num20 = 0
                    If (nbd(index) <= 2) Then
                        num21 = (x(index) - l(index))
                    End If
                    If (nbd(index) >= 2) Then
                        num20 = (u(index) - x(index))
                    End If
                    flag = ((nbd(index) <= 2) And (num21 <= 0))
                    flag2 = ((nbd(index) >= 2) And (num20 <= 0))
                    iwhere(index) = 0
                    If flag Then
                        If (num27 <= 0) Then
                            iwhere(index) = 1
                        End If
                    ElseIf flag2 Then
                        If (num27 >= 0) Then
                            iwhere(index) = 2
                        End If
                    ElseIf (Math.Abs(num27) <= 0) Then
                        iwhere(index) = -3
                    End If
                End If
                num6 = head
                If ((iwhere(index) <> 0) And (iwhere(index) <> -1)) Then
                    d(index) = 0
                Else
                    d(index) = num27
                    num11 = (num11 - (num27 * num27))
                    num2 = 1
                    Do While (num2 <= col)
                        p(num2) = (p(num2) + (wy(index, num6) * num27))
                        p((col + num2)) = (p((col + num2)) + (ws(index, num6) * num27))
                        num6 = ((num6 Mod m) + 1)
                        num2 += 1
                    Loop
                    If (((nbd(index) <= 2) And (nbd(index) <> 0)) And (num27 < 0)) Then
                        num5 += 1
                        iorder(num5) = index
                        t(num5) = (num21 / -num27)
                        If ((num5 = 1) Or (t(num5) < num19)) Then
                            num19 = t(num5)
                            num9 = num5
                        End If
                    ElseIf ((nbd(index) >= 2) And (num27 > 0)) Then
                        num5 += 1
                        iorder(num5) = index
                        t(num5) = (num20 / num27)
                        If ((num5 = 1) Or (t(num5) < num19)) Then
                            num19 = t(num5)
                            num9 = num5
                        End If
                    Else
                        num4 -= 1
                        iorder(num4) = index
                        If (Math.Abs(num27) > 0) Then
                            flag3 = False
                        End If
                    End If
                End If
                index += 1
            Loop
            If (theta <> 1) Then
                num30 = (col + 1)
                Do While (num30 <= (col + col))
                    p(num30) = (theta * p(num30))
                    num30 += 1
                Loop
            End If
            num30 = 1
            Do While (num30 <= n)
                xcp(num30) = x(num30)
                num30 += 1
            Loop
            If ((num5 = 0) And (num4 = (n + 1))) Then
                Return
            End If
            num2 = 1
            Do While (num2 <= num3)
                c(num2) = 0
                num2 += 1
            Loop
            num12 = -(theta * num11)
            num28 = num12
            If (col > 0) Then
                Me.lbfgsbbmv(m, sy, wt, col, p, v, info, workvec)
                If (Not info = 0) Then
                    Return
                End If
                num29 = 0
                num30 = 1
                Do While (num30 <= num3)
                    num29 = (num29 + (v(num30) * p(num30)))
                    num30 += 1
                Loop
                num12 = (num12 - num29)
            End If
            num14 = -(num11 / num12)
            num15 = 0
            nint = 1
            If (num5 = 0) Then
                GoTo Label_07E7
            End If
            num8 = num5
            num10 = 1
            num25 = 0
Label_04DC:
            num26 = num25
            If (num10 = 1) Then
                num25 = num19
                num7 = iorder(num9)
            Else
                If ((num10 = 2) AndAlso (num9 <> num5)) Then
                    t(num9) = t(num5)
                    iorder(num9) = iorder(num5)
                End If
                Me.lbfgsbhpsolb(num8, t, iorder, num10 - 2)
                num25 = t(num8)
                num7 = iorder(num8)
            End If
            num13 = (num25 - num26)
            If (num14 >= num13) Then
                num15 = (num15 + num13)
                num8 -= 1
                num10 += 1
                num16 = d(num7)
                d(num7) = 0
                If (num16 > 0) Then
                    num17 = (u(num7) - x(num7))
                    xcp(num7) = u(num7)
                    iwhere(num7) = 2
                Else
                    num17 = (l(num7) - x(num7))
                    xcp(num7) = l(num7)
                    iwhere(num7) = 1
                End If
                If ((num8 = 0) And (num5 = n)) Then
                    num14 = num13
                    If (col > 0) Then
                        num30 = 1
                        Do While (num30 <= num3)
                            c(num30) = (c(num30) + (num14 * p(num30)))
                            num30 += 1
                        Loop
                    End If
                    Return
                End If
                nint += 1
                num18 = Math.Sqrt(num16)
                num11 = (((num11 + (num13 * num12)) + num18) - ((theta * num16) * num17))
                num12 = (num12 - (theta * num18))
                If (col > 0) Then
                    num30 = 1
                    Do While (num30 <= num3)
                        c(num30) = (c(num30) + (num13 * p(num30)))
                        num30 += 1
                    Loop
                    num6 = head
                    num2 = 1
                    Do While (num2 <= col)
                        wbp(num2) = wy(num7, num6)
                        wbp((col + num2)) = (theta * ws(num7, num6))
                        num6 = ((num6 Mod m) + 1)
                        num2 += 1
                    Loop
                    Me.lbfgsbbmv(m, sy, wt, col, wbp, v, info, workvec)
                    If (Not info = 0) Then
                        Return
                    End If
                    num22 = 0
                    num30 = 1
                    Do While (num30 <= num3)
                        num22 = (num22 + (c(num30) * v(num30)))
                        num30 += 1
                    Loop
                    num23 = 0
                    num30 = 1
                    Do While (num30 <= num3)
                        num23 = (num23 + (p(num30) * v(num30)))
                        num30 += 1
                    Loop
                    num24 = 0
                    num30 = 1
                    Do While (num30 <= num3)
                        num24 = (num24 + (wbp(num30) * v(num30)))
                        num30 += 1
                    Loop
                    num30 = 1
                    Do While (num30 <= num3)
                        p(num30) = (p(num30) - (num16 * wbp(num30)))
                        num30 += 1
                    Loop
                    num11 = (num11 + (num16 * num22))
                    num12 = ((num12 + ((2 * num16) * num23)) - (num18 * num24))
                End If
                num12 = Math.Max((0.0000000000000005 * num28), num12)
                If (num8 > 0) Then
                    num14 = -(num11 / num12)
                    GoTo Label_04DC
                End If
                If flag3 Then
                    num11 = 0
                    num12 = 0
                    num14 = 0
                Else
                    num14 = -(num11 / num12)
                End If
            End If
Label_07E7:
            If (num14 <= 0) Then
                num14 = 0
            End If
            num15 = (num15 + num14)
            num30 = 1
            Do While (num30 <= n)
                xcp(num30) = (xcp(num30) + (num15 * d(num30)))
                num30 += 1
            Loop
            If (col > 0) Then
                num30 = 1
                Do While (num30 <= num3)
                    c(num30) = (c(num30) + (num14 * p(num30)))
                    num30 += 1
                Loop
            End If
        End Sub

        Private Sub lbfgsbcmprlb(ByVal n As Integer, ByVal m As Integer, ByRef x As Double(), ByRef g As Double(), ByRef ws As Double(,), ByRef wy As Double(,), ByRef sy As Double(,), ByRef wt As Double(,), ByRef z As Double(), ByRef r As Double(), ByRef wa As Double(), ByRef index As Integer(), ByVal theta As Double, ByVal col As Integer, ByVal head As Integer, ByVal nfree As Integer, ByVal cnstnd As Boolean, ByRef info As Integer, ByRef workvec As Double(), ByRef workvec2 As Double())
            Dim num As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Double = 0
            Dim num6 As Double = 0
            Dim num7 As Integer = 0
            Dim num8 As Integer = 0
            If (Not cnstnd And (col > 0)) Then
                num = 1
                Do While (num <= n)
                    r(num) = -g(num)
                    num += 1
                Loop
            Else
                num = 1
                Do While (num <= nfree)
                    num3 = index(num)
                    r(num) = (-(theta * (z(num3) - x(num3))) - g(num3))
                    num += 1
                Loop
                num8 = (((2 * m) + 1) - 1)
                num7 = 1
                Do While (num7 <= (2 * m))
                    workvec2(num7) = wa((num7 + num8))
                    num7 += 1
                Loop
                Me.lbfgsbbmv(m, sy, wt, col, workvec2, wa, info, workvec)
                num8 = (1 - ((2 * m) + 1))
                num7 = ((2 * m) + 1)
                Do While (num7 <= (4 * m))
                    wa(num7) = workvec2((num7 + num8))
                    num7 += 1
                Loop
                If (Not info = 0) Then
                    info = -8
                Else
                    num4 = head
                    num2 = 1
                    Do While (num2 <= col)
                        num5 = wa(num2)
                        num6 = (theta * wa((col + num2)))
                        num = 1
                        Do While (num <= nfree)
                            num3 = index(num)
                            r(num) = ((r(num) + (wy(num3, num4) * num5)) + (ws(num3, num4) * num6))
                            num += 1
                        Loop
                        num4 = ((num4 Mod m) + 1)
                        num2 += 1
                    Loop
                End If
            End If
        End Sub

        Private Sub lbfgsbdcsrch(ByVal f As Double, ByVal g As Double, ByRef stp As Double, ByVal ftol As Double, ByVal gtol As Double, ByVal xtol As Double, ByVal stpmin As Double, ByVal stpmax As Double, ByRef task As Integer, ByRef isave As Integer(), ByRef dsave As Double(), ByRef addinfo As Integer)
            Dim brackt As Boolean = False
            Dim num As Integer = 0
            Dim num2 As Double = 0
            Dim num3 As Double = 0
            Dim fp As Double = 0
            Dim fx As Double = 0
            Dim num6 As Double = 0
            Dim fy As Double = 0
            Dim num8 As Double = 0
            Dim num9 As Double = 0
            Dim num10 As Double = 0
            Dim dp As Double = 0
            Dim dx As Double = 0
            Dim num13 As Double = 0
            Dim dy As Double = 0
            Dim num15 As Double = 0
            Dim stx As Double = 0
            Dim sty As Double = 0
            Dim num18 As Double = 0
            Dim num19 As Double = 0
            Dim num20 As Double = 0
            Dim num21 As Double = 0
            Dim num22 As Double = 0
            Dim num23 As Double = 0
            num22 = 1.1
            num23 = 4
            If (task = 0) Then
                If (stp < stpmin) Then
                    task = 2
                    addinfo = 0
                End If
                If (stp > stpmax) Then
                    task = 2
                    addinfo = 0
                End If
                If (g >= 0) Then
                    task = 2
                    addinfo = 0
                End If
                If (ftol < 0) Then
                    task = 2
                    addinfo = 0
                End If
                If (gtol < 0) Then
                    task = 2
                    addinfo = 0
                End If
                If (xtol < 0) Then
                    task = 2
                    addinfo = 0
                End If
                If (stpmin < 0) Then
                    task = 2
                    addinfo = 0
                End If
                If (stpmax < stpmin) Then
                    task = 2
                    addinfo = 0
                End If
                If (task = 2) Then
                    Return
                End If
                brackt = False
                num = 1
                num2 = f
                num9 = g
                num10 = (ftol * num9)
                num20 = (stpmax - stpmin)
                num21 = (num20 / 0.5)
                stx = 0
                fx = num2
                dx = num9
                sty = 0
                fy = num2
                dy = num9
                num18 = 0
                num19 = (stp + (num23 * stp))
                task = 1
            Else
                If (isave(1) = 1) Then
                    brackt = True
                Else
                    brackt = False
                End If
                num = isave(2)
                num9 = dsave(1)
                num10 = dsave(2)
                dx = dsave(3)
                dy = dsave(4)
                num2 = dsave(5)
                fx = dsave(6)
                fy = dsave(7)
                stx = dsave(8)
                sty = dsave(9)
                num18 = dsave(10)
                num19 = dsave(11)
                num20 = dsave(12)
                num21 = dsave(13)
                num3 = (num2 + (stp * num10))
                If (((num = 1) And (f <= num3)) And (g >= 0)) Then
                    num = 2
                End If
                If (brackt And ((stp <= num18) Or (stp >= num19))) Then
                    task = 3
                    addinfo = 1
                End If
                If (brackt And ((num19 - num18) <= (xtol * num19))) Then
                    task = 3
                    addinfo = 2
                End If
                If (((stp = stpmax) And (f <= num3)) And (g <= num10)) Then
                    task = 3
                    addinfo = 3
                End If
                If ((stp = stpmin) And ((f > num3) Or (g >= num10))) Then
                    task = 3
                    addinfo = 4
                End If
                If ((f <= num3) And (Math.Abs(g) <= (gtol * -num9))) Then
                    task = 4
                    addinfo = -1
                End If
                If Not ((task = 3) Or (task = 4)) Then
                    If (((num = 1) And (f <= fx)) And (f > num3)) Then
                        fp = (f - (stp * num10))
                        num6 = (fx - (stx * num10))
                        num8 = (fy - (sty * num10))
                        dp = (g - num10)
                        num13 = (dx - num10)
                        num15 = (dy - num10)
                        Me.lbfgsbdcstep(stx, num6, num13, sty, num8, num15, stp, fp, dp, brackt, num18, num19)
                        fx = (num6 + (stx * num10))
                        fy = (num8 + (sty * num10))
                        dx = (num13 + num10)
                        dy = (num15 + num10)
                    Else
                        Me.lbfgsbdcstep(stx, fx, dx, sty, fy, dy, stp, f, g, brackt, num18, num19)
                    End If
                    If brackt Then
                        If (Math.Abs(Convert.ToDouble((sty - stx))) >= (0.66 * num21)) Then
                            stp = (stx + (0.5 * (sty - stx)))
                        End If
                        num21 = num20
                        num20 = Math.Abs(Convert.ToDouble((sty - stx)))
                    End If
                    If brackt Then
                        num18 = Math.Min(stx, sty)
                        num19 = Math.Max(stx, sty)
                    Else
                        num18 = (stp + (num22 * (stp - stx)))
                        num19 = (stp + (num23 * (stp - stx)))
                    End If
                    stp = Math.Max(stp, stpmin)
                    stp = Math.Min(stp, stpmax)
                    If ((brackt And ((stp <= num18) Or (stp >= num19))) Or (brackt And ((num19 - num18) <= (xtol * num19)))) Then
                        stp = stx
                    End If
                    task = 1
                End If
            End If
            If brackt Then
                isave(1) = 1
            Else
                isave(1) = 0
            End If
            isave(2) = num
            dsave(1) = num9
            dsave(2) = num10
            dsave(3) = dx
            dsave(4) = dy
            dsave(5) = num2
            dsave(6) = fx
            dsave(7) = fy
            dsave(8) = stx
            dsave(9) = sty
            dsave(10) = num18
            dsave(11) = num19
            dsave(12) = num20
            dsave(13) = num21
        End Sub

        Private Sub lbfgsbdcstep(ByRef stx As Double, ByRef fx As Double, ByRef dx As Double, ByRef sty As Double, ByRef fy As Double, ByRef dy As Double, ByRef stp As Double, ByVal fp As Double, ByVal dp As Double, ByRef brackt As Boolean, ByVal stpmin As Double, ByVal stpmax As Double)
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
            num6 = (dp * (dx / Math.Abs(dx)))
            If (fp > fx) Then
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
                If (Math.Abs(Convert.ToDouble((num7 - stx))) < Math.Abs(Convert.ToDouble((num9 - stx)))) Then
                    num8 = num7
                Else
                    num8 = (num7 + ((num9 - num7) / 2))
                End If
                brackt = True
            ElseIf (num6 < 0) Then
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
                If (Math.Abs(Convert.ToDouble((num7 - stp))) > Math.Abs(Convert.ToDouble((num9 - stp)))) Then
                    num8 = num7
                Else
                    num8 = num9
                End If
                brackt = True
            ElseIf (Math.Abs(dp) < Math.Abs(dx)) Then
                num10 = ((((3 * (fx - fp)) / (stp - stx)) + dx) + dp)
                num5 = Math.Max(Math.Abs(num10), Math.Max(Math.Abs(dx), Math.Abs(dp)))
                num = (num5 * Math.Sqrt(Math.Max(Convert.ToDouble(0), Convert.ToDouble((Math.Sqrt((num10 / num5)) - ((dx / num5) * (dp / num5)))))))
                If (stp > stx) Then
                    num = -num
                End If
                num2 = ((num - dp) + num10)
                num3 = ((num + (dx - dp)) + num)
                num4 = (num2 / num3)
                If ((num4 < 0) And (num <> 0)) Then
                    num7 = (stp + (num4 * (stx - stp)))
                ElseIf (stp > stx) Then
                    num7 = stpmax
                Else
                    num7 = stpmin
                End If
                num9 = (stp + ((dp / (dp - dx)) * (stx - stp)))
                If brackt Then
                    If (Math.Abs(Convert.ToDouble((num7 - stp))) < Math.Abs(Convert.ToDouble((num9 - stp)))) Then
                        num8 = num7
                    Else
                        num8 = num9
                    End If
                    If (stp > stx) Then
                        num8 = Math.Min((stp + (0.66 * (sty - stp))), num8)
                    Else
                        num8 = Math.Max((stp + (0.66 * (sty - stp))), num8)
                    End If
                Else
                    If (Math.Abs(Convert.ToDouble((num7 - stp))) > Math.Abs(Convert.ToDouble((num9 - stp)))) Then
                        num8 = num7
                    Else
                        num8 = num9
                    End If
                    num8 = Math.Min(stpmax, num8)
                    num8 = Math.Max(stpmin, num8)
                End If
            ElseIf brackt Then
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
                num8 = stpmax
            Else
                num8 = stpmin
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
            stp = num8
        End Sub

        Private Function lbfgsbdpofa(ByRef a As Double(,), ByVal n As Integer) As Boolean
            Dim num As Double = 0
            Dim d As Double = 0
            Dim num3 As Double = 0
            Dim num4 As Integer = 0
            Dim num5 As Integer = 0
            Dim num6 As Integer = 0
            Dim num7 As Integer = 0
            num4 = 1
            Do While (num4 <= n)
                d = 0
                num5 = (num4 - 1)
                If (num5 >= 1) Then
                    num6 = 1
                    Do While (num6 <= num5)
                        num3 = 0
                        num7 = 1
                        Do While (num7 <= (num6 - 1))
                            num3 = (num3 + (a(num7, num6) * a(num7, num4)))
                            num7 += 1
                        Loop
                        num = (a(num6, num4) - num3)
                        num = (num / a(num6, num6))
                        a(num6, num4) = num
                        d = (d + (num * num))
                        num6 += 1
                    Loop
                End If
                d = (a(num4, num4) - d)
                If (d <= 0) Then
                    Return False
                End If
                a(num4, num4) = Math.Sqrt(d)
                num4 += 1
            Loop
            Return True
        End Function

        Private Sub lbfgsbdtrsl(ByRef t As Double(,), ByVal n As Integer, ByRef b As Double(), ByVal job As Integer, ByRef info As Integer)
            Dim num As Double = 0
            Dim num2 As Double = 0
            Dim num3 As Integer = 0
            Dim index As Integer = 0
            Dim num5 As Integer = 0
            Dim num6 As Integer = 0
            index = 1
            Do While (index <= n)
                If (t(index, index) = 0) Then
                    info = index
                    Return
                End If
                index += 1
            Loop
            info = 0
            num3 = 1
            If ((job Mod 10) <> 0) Then
                num3 = 2
            End If
            If (((job Mod 100) / 10) <> 0) Then
                num3 = (num3 + 2)
            End If
            If (num3 = 1) Then
                b(1) = (b(1) / t(1, 1))
                If (n >= 2) Then
                    index = 2
                    Do While (index <= n)
                        num = -b((index - 1))
                        num6 = index
                        Do While (num6 <= n)
                            b(num6) = (b(num6) + (num * t(num6, (index - 1))))
                            num6 += 1
                        Loop
                        b(index) = (b(index) / t(index, index))
                        index += 1
                    Loop
                End If
            ElseIf (num3 = 2) Then
                b(n) = (b(n) / t(n, n))
                If (n >= 2) Then
                    num5 = 2
                    Do While (num5 <= n)
                        index = ((n - num5) + 1)
                        num = -b((index + 1))
                        num6 = 1
                        Do While (num6 <= index)
                            b(num6) = (b(num6) + (num * t(num6, (index + 1))))
                            num6 += 1
                        Loop
                        b(index) = (b(index) / t(index, index))
                        num5 += 1
                    Loop
                End If
            ElseIf (num3 = 3) Then
                b(n) = (b(n) / t(n, n))
                If (n >= 2) Then
                    num5 = 2
                    Do While (num5 <= n)
                        index = ((n - num5) + 1)
                        num2 = 0
                        num6 = (index + 1)
                        Do While (num6 <= ((((index + 1) + num5) - 1) - 1))
                            num2 = (num2 + (t(num6, index) * b(num6)))
                            num6 += 1
                        Loop
                        b(index) = (b(index) - num2)
                        b(index) = (b(index) / t(index, index))
                        num5 += 1
                    Loop
                End If
            ElseIf (num3 = 4) Then
                b(1) = (b(1) / t(1, 1))
                If (n >= 2) Then
                    index = 2
                    Do While (index <= n)
                        num2 = 0
                        num6 = 1
                        Do While (num6 <= (index - 1))
                            num2 = (num2 + (t(num6, index) * b(num6)))
                            num6 += 1
                        Loop
                        b(index) = (b(index) - num2)
                        b(index) = (b(index) / t(index, index))
                        index += 1
                    Loop
                End If
            End If
        End Sub

        Private Sub lbfgsberrclb(ByVal n As Integer, ByVal m As Integer, ByVal factr As Double, ByRef l As Double(), ByRef u As Double(), ByRef nbd As Integer(), ByRef task As Integer, ByRef info As Integer, ByRef k As Integer)
            Dim index As Integer = 0
            If (n <= 0) Then
                task = 2
            End If
            If (m <= 0) Then
                task = 2
            End If
            If (m > n) Then
                task = 2
            End If
            If (factr < 0) Then
                task = 2
            End If
            index = 1
            Do While (index <= n)
                If ((nbd(index) < 0) Or (nbd(index) > 3)) Then
                    task = 2
                    info = -6
                    k = index
                End If
                If ((nbd(index) = 2) AndAlso (l(index) > u(index))) Then
                    task = 2
                    info = -7
                    k = index
                End If
                index += 1
            Loop
        End Sub

        Private Sub lbfgsbformk(ByVal n As Integer, ByVal nsub As Integer, ByRef ind As Integer(), ByVal nenter As Integer, ByVal ileave As Integer, ByRef indx2 As Integer(), ByVal iupdat As Integer, ByVal updatd As Boolean, ByRef wn As Double(,), ByRef wn1 As Double(,), ByVal m As Integer, ByRef ws As Double(,), ByRef wy As Double(,), ByRef sy As Double(,), ByVal theta As Double, ByVal col As Integer, ByVal head As Integer, ByRef info As Integer, ByRef workvec As Double(), ByRef workmat As Double(,))
            Dim num As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Integer = 0
            Dim num6 As Integer = 0
            Dim num7 As Integer = 0
            Dim num8 As Integer = 0
            Dim num9 As Integer = 0
            Dim num10 As Integer = 0
            Dim index As Integer = 0
            Dim num12 As Integer = 0
            Dim num13 As Integer = 0
            Dim num14 As Integer = 0
            Dim num15 As Integer = 0
            Dim num16 As Integer = 0
            Dim num17 As Integer = 0
            Dim num18 As Double = 0
            Dim num19 As Double = 0
            Dim num20 As Double = 0
            Dim num21 As Double = 0
            Dim num22 As Double = 0
            Dim num23 As Integer = 0
            Dim num24 As Integer = 0
            Dim num25 As Integer = 0
            If updatd Then
                If (iupdat > m) Then
                    num5 = 1
                    Do While (num5 <= (m - 1))
                        num6 = (m + num5)
                        num25 = ((num5 + 1) - num5)
                        num24 = num5
                        Do While (num24 <= (m - 1))
                            wn1(num24, num5) = wn1((num24 + num25), (num5 + 1))
                            num24 += 1
                        Loop
                        num25 = ((num6 + 1) - num6)
                        num24 = num6
                        Do While (num24 <= (((num6 + m) - num5) - 1))
                            wn1(num24, num6) = wn1((num24 + num25), (num6 + 1))
                            num24 += 1
                        Loop
                        num25 = ((m + 2) - (m + 1))
                        num24 = (m + 1)
                        Do While (num24 <= ((m + m) - 1))
                            wn1(num24, num5) = wn1((num24 + num25), (num5 + 1))
                            num24 += 1
                        Loop
                        num5 += 1
                    Loop
                End If
                num13 = 1
                num14 = nsub
                num15 = (nsub + 1)
                num16 = n
                num3 = col
                num4 = (m + col)
                num = ((head + col) - 1)
                If (num > m) Then
                    num = (num - m)
                End If
                num2 = head
                num5 = 1
                Do While (num5 <= col)
                    num6 = (m + num5)
                    num18 = 0
                    num19 = 0
                    num20 = 0
                    index = num13
                    Do While (index <= num14)
                        num9 = ind(index)
                        num18 = (num18 + (wy(num9, num) * wy(num9, num2)))
                        index += 1
                    Loop
                    index = num15
                    Do While (index <= num16)
                        num9 = ind(index)
                        num19 = (num19 + (ws(num9, num) * ws(num9, num2)))
                        num20 = (num20 + (ws(num9, num) * wy(num9, num2)))
                        index += 1
                    Loop
                    wn1(num3, num5) = num18
                    wn1(num4, num6) = num19
                    wn1(num4, num5) = num20
                    num2 = ((num2 Mod m) + 1)
                    num5 += 1
                Loop
                num5 = col
                num2 = ((head + col) - 1)
                If (num2 > m) Then
                    num2 = (num2 - m)
                End If
                num = head
                num10 = 1
                Do While (num10 <= col)
                    num4 = (m + num10)
                    num20 = 0
                    index = num13
                    Do While (index <= num14)
                        num9 = ind(index)
                        num20 = (num20 + (ws(num9, num) * wy(num9, num2)))
                        index += 1
                    Loop
                    num = ((num Mod m) + 1)
                    wn1(num4, num5) = num20
                    num10 += 1
                Loop
                num17 = (col - 1)
            Else
                num17 = col
            End If
            num = head
            num3 = 1
            Do While (num3 <= num17)
                num4 = (m + num3)
                num2 = head
                num5 = 1
                Do While (num5 <= num3)
                    num6 = (m + num5)
                    num18 = 0
                    num19 = 0
                    num20 = 0
                    num21 = 0
                    index = 1
                    Do While (index <= nenter)
                        num9 = indx2(index)
                        num18 = (num18 + (wy(num9, num) * wy(num9, num2)))
                        num19 = (num19 + (ws(num9, num) * ws(num9, num2)))
                        index += 1
                    Loop
                    index = ileave
                    Do While (index <= n)
                        num9 = indx2(index)
                        num20 = (num20 + (wy(num9, num) * wy(num9, num2)))
                        num21 = (num21 + (ws(num9, num) * ws(num9, num2)))
                        index += 1
                    Loop
                    wn1(num3, num5) = ((wn1(num3, num5) + num18) - num20)
                    wn1(num4, num6) = ((wn1(num4, num6) - num19) + num21)
                    num2 = ((num2 Mod m) + 1)
                    num5 += 1
                Loop
                num = ((num Mod m) + 1)
                num3 += 1
            Loop
            num = head
            num4 = (m + 1)
            Do While (num4 <= (m + num17))
                num2 = head
                num5 = 1
                Do While (num5 <= num17)
                    num18 = 0
                    num20 = 0
                    index = 1
                    Do While (index <= nenter)
                        num9 = indx2(index)
                        num18 = (num18 + (ws(num9, num) * wy(num9, num2)))
                        index += 1
                    Loop
                    index = ileave
                    Do While (index <= n)
                        num9 = indx2(index)
                        num20 = (num20 + (ws(num9, num) * wy(num9, num2)))
                        index += 1
                    Loop
                    If (num4 <= (num5 + m)) Then
                        wn1(num4, num5) = ((wn1(num4, num5) + num18) - num20)
                    Else
                        wn1(num4, num5) = ((wn1(num4, num5) - num18) + num20)
                    End If
                    num2 = ((num2 Mod m) + 1)
                    num5 += 1
                Loop
                num = ((num Mod m) + 1)
                num4 += 1
            Loop
            num3 = 1
            Do While (num3 <= col)
                num4 = (col + num3)
                num7 = (m + num3)
                num5 = 1
                Do While (num5 <= num3)
                    num6 = (col + num5)
                    num8 = (m + num5)
                    wn(num5, num3) = (wn1(num3, num5) / theta)
                    wn(num6, num4) = (wn1(num7, num8) * theta)
                    num5 += 1
                Loop
                num5 = 1
                Do While (num5 <= (num3 - 1))
                    wn(num5, num4) = -wn1(num7, num5)
                    num5 += 1
                Loop
                num5 = num3
                Do While (num5 <= col)
                    wn(num5, num4) = wn1(num7, num5)
                    num5 += 1
                Loop
                wn(num3, num3) = (wn(num3, num3) + sy(num3, num3))
                num3 += 1
            Loop
            info = 0
            If Not Me.lbfgsbdpofa(wn, col) Then
                info = -1
            Else
                num12 = (2 * col)
                num6 = (col + 1)
                Do While (num6 <= num12)
                    num24 = 1
                    Do While (num24 <= col)
                        workvec(num24) = wn(num24, num6)
                        num24 += 1
                    Loop
                    Me.lbfgsbdtrsl(wn, col, workvec, 11, info)
                    num24 = 1
                    Do While (num24 <= col)
                        wn(num24, num6) = workvec(num24)
                        num24 += 1
                    Loop
                    num6 += 1
                Loop
                num4 = (col + 1)
                Do While (num4 <= num12)
                    num6 = num4
                    Do While (num6 <= num12)
                        num22 = 0
                        num24 = 1
                        Do While (num24 <= col)
                            num22 = (num22 + (wn(num24, num4) * wn(num24, num6)))
                            num24 += 1
                        Loop
                        wn(num4, num6) = (wn(num4, num6) + num22)
                        num6 += 1
                    Loop
                    num4 += 1
                Loop
                num23 = 1
                Do While (num23 <= col)
                    num25 = ((col + 1) - 1)
                    num24 = 1
                    Do While (num24 <= col)
                        workmat(num23, num24) = wn((col + num23), (num24 + num25))
                        num24 += 1
                    Loop
                    num23 += 1
                Loop
                info = 0
                If Not Me.lbfgsbdpofa(workmat, col) Then
                    info = -2
                Else
                    num23 = 1
                    Do While (num23 <= col)
                        num25 = (1 - (col + 1))
                        num24 = (col + 1)
                        Do While (num24 <= (col + col))
                            wn((col + num23), num24) = workmat(num23, (num24 + num25))
                            num24 += 1
                        Loop
                        num23 += 1
                    Loop

                End If
            End If
        End Sub

        Private Sub lbfgsbformt(ByVal m As Integer, ByRef wt As Double(,), ByRef sy As Double(,), ByRef ss As Double(,), ByVal col As Integer, ByVal theta As Double, ByRef info As Integer)
            Dim num As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Double = 0
            num2 = 1
            Do While (num2 <= col)
                wt(1, num2) = (theta * ss(1, num2))
                num2 += 1
            Loop
            num = 2
            Do While (num <= col)
                num2 = num
                Do While (num2 <= col)
                    num4 = (Math.Min(num, num2) - 1)
                    num5 = 0
                    num3 = 1
                    Do While (num3 <= num4)
                        num5 = (num5 + ((sy(num, num3) * sy(num2, num3)) / sy(num3, num3)))
                        num3 += 1
                    Loop
                    wt(num, num2) = (num5 + (theta * ss(num, num2)))
                    num2 += 1
                Loop
                num += 1
            Loop
            info = 0
            If Not Me.lbfgsbdpofa(wt, col) Then
                info = -3
            End If
        End Sub

        Private Sub lbfgsbfreev(ByVal n As Integer, ByRef nfree As Integer, ByRef index As Integer(), ByRef nenter As Integer, ByRef ileave As Integer, ByRef indx2 As Integer(), ByRef iwhere As Integer(), ByRef wrk As Boolean, ByVal updatd As Boolean, ByVal cnstnd As Boolean, ByVal iter As Integer)
            Dim num As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            nenter = 0
            ileave = (n + 1)
            If ((iter > 0) And cnstnd) Then
                num2 = 1
                Do While (num2 <= nfree)
                    num3 = index(num2)
                    If (iwhere(num3) > 0) Then
                        ileave -= 1
                        indx2(ileave) = num3
                    End If
                    num2 += 1
                Loop
                num2 = (1 + nfree)
                Do While (num2 <= n)
                    num3 = index(num2)
                    If (iwhere(num3) <= 0) Then
                        nenter += 1
                        indx2(nenter) = num3
                    End If
                    num2 += 1
                Loop
            End If
            wrk = (((ileave < (n + 1)) Or (nenter > 0)) Or updatd)
            nfree = 0
            num = (n + 1)
            num2 = 1
            Do While (num2 <= n)
                If (iwhere(num2) <= 0) Then
                    nfree += 1
                    index(nfree) = num2
                Else
                    num -= 1
                    index(num) = num2
                End If
                num2 += 1
            Loop
        End Sub

        Private Sub lbfgsbhpsolb(ByVal n As Integer, ByRef t As Double(), ByRef iorder As Integer(), ByVal iheap As Integer)
            Dim index As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Integer = 0
            Dim num6 As Double = 0
            Dim num7 As Double = 0
            If (iheap = 0) Then
                num3 = 2
                Do While (num3 <= n)
                    num6 = t(num3)
                    num4 = iorder(num3)
                    index = num3
                    Do While True
                        If (index <= 1) Then
                            Exit Do
                        End If
                        num2 = (index / 2)
                        If (num6 >= t(num2)) Then
                            Exit Do
                        End If
                        t(index) = t(num2)
                        iorder(index) = iorder(num2)
                        index = num2
                    Loop
                    t(index) = num6
                    iorder(index) = num4
                    num3 += 1
                Loop
            End If
            If (n <= 1) Then
                Return
            End If
            index = 1
            num7 = t(1)
            num5 = iorder(1)
            num6 = t(n)
            num4 = iorder(n)
Label_0089:
            num2 = (index + index)
            If (num2 <= (n - 1)) Then
                If (t((num2 + 1)) < t(num2)) Then
                    num2 += 1
                End If
                If (t(num2) < num6) Then
                    t(index) = t(num2)
                    iorder(index) = iorder(num2)
                    index = num2
                    GoTo Label_0089
                End If
            End If
            t(index) = num6
            iorder(index) = num4
            t(n) = num7
            iorder(n) = num5
        End Sub

        Private Sub lbfgsblnsrlb(ByVal n As Integer, ByRef l As Double(), ByRef u As Double(), ByRef nbd As Integer(), ByRef x As Double(), ByVal f As Double, ByRef fold As Double, ByRef gd As Double, ByRef gdold As Double, ByRef g As Double(), ByRef d As Double(), ByRef r As Double(), ByRef t As Double(), ByRef z As Double(), ByRef stp As Double, ByRef dnrm As Double, ByRef dtd As Double, ByRef xstep As Double, ByRef stpmx As Double, ByVal iter As Integer, ByRef ifun As Integer, ByRef iback As Integer, ByRef nfgv As Integer, ByRef info As Integer, ByRef task As Integer, ByVal boxed As Boolean, ByVal cnstnd As Boolean, ByRef csave As Integer, ByRef isave As Integer(), ByRef dsave As Double())
            Dim index As Integer = 0
            Dim num2 As Double = 0
            Dim num3 As Double = 0
            Dim num4 As Double = 0
            Dim ftol As Double = 0
            Dim gtol As Double = 0
            Dim xtol As Double = 0
            Dim num8 As Double = 0
            Dim addinfo As Integer = 0
            Dim num10 As Integer = 0
            addinfo = 0
            num8 = 10000000000
            ftol = 0.001
            gtol = 0.9
            xtol = 0.1
            If (Not task = 1) Then
                num4 = 0
                num10 = 1
                Do While (num10 <= n)
                    num4 = (num4 + (d(num10) * d(num10)))
                    num10 += 1
                Loop
                dtd = num4
                dnrm = Math.Sqrt(dtd)
                stpmx = num8
                If cnstnd Then
                    If (iter = 0) Then
                        stpmx = 1
                    Else
                        index = 1
                        Do While (index <= n)
                            num2 = d(index)
                            If (nbd(index) <> 0) Then
                                If ((num2 < 0) And (nbd(index) <= 2)) Then
                                    num3 = (l(index) - x(index))
                                    If (num3 >= 0) Then
                                        stpmx = 0
                                    ElseIf ((num2 * stpmx) < num3) Then
                                        stpmx = (num3 / num2)
                                    End If
                                ElseIf ((num2 > 0) And (nbd(index) >= 2)) Then
                                    num3 = (u(index) - x(index))
                                    If (num3 <= 0) Then
                                        stpmx = 0
                                    ElseIf ((num2 * stpmx) > num3) Then
                                        stpmx = (num3 / num2)
                                    End If
                                End If
                            End If
                            index += 1
                        Loop
                    End If
                End If
                If ((iter = 0) And Not boxed) Then
                    stp = Math.Min((1 / dnrm), stpmx)
                Else
                    stp = 1
                End If
                num10 = 1
                Do While (num10 <= n)
                    t(num10) = x(num10)
                    num10 += 1
                Loop
                num10 = 1
                Do While (num10 <= n)
                    r(num10) = g(num10)
                    num10 += 1
                Loop
                fold = f
                ifun = 0
                iback = 0
                csave = 0
            End If
            num4 = 0
            num10 = 1
            Do While (num10 <= n)
                num4 = (num4 + (g(num10) * d(num10)))
                num10 += 1
            Loop
            gd = num4
            If (ifun = 0) Then
                gdold = gd
                If (gd >= 0) Then
                    info = -4
                    Return
                End If
            End If
            Me.lbfgsbdcsrch(f, gd, stp, ftol, gtol, xtol, 0, stpmx, csave, isave, dsave, addinfo)
            xstep = (stp * dnrm)
            If ((Not csave = 4) And (Not csave = 3)) Then
                task = 1
                ifun += 1
                nfgv += 1
                iback = (ifun - 1)
                If (stp = 1) Then
                    num10 = 1
                    Do While (num10 <= n)
                        x(num10) = z(num10)
                        num10 += 1
                    Loop
                Else
                    index = 1
                    Do While (index <= n)
                        x(index) = ((stp * d(index)) + t(index))
                        index += 1
                    Loop
                End If
            Else
                task = 5
            End If
        End Sub

        Private Sub lbfgsbmatupd(ByVal n As Integer, ByVal m As Integer, ByRef ws As Double(,), ByRef wy As Double(,), ByRef sy As Double(,), ByRef ss As Double(,), ByRef d As Double(), ByRef r As Double(), ByRef itail As Integer, ByVal iupdat As Integer, ByRef col As Integer, ByRef head As Integer, ByRef theta As Double, ByVal rr As Double, ByVal dr As Double, ByVal stp As Double, ByVal dtd As Double)
            Dim num As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Double = 0
            Dim index As Integer = 0
            Dim num5 As Integer = 0
            If (iupdat <= m) Then
                col = iupdat
                itail = ((((head + iupdat) - 2) Mod m) + 1)
            Else
                itail = ((itail Mod m) + 1)
                head = ((head Mod m) + 1)
            End If
            index = 1
            Do While (index <= n)
                ws(index, itail) = d(index)
                index += 1
            Loop
            index = 1
            Do While (index <= n)
                wy(index, itail) = r(index)
                index += 1
            Loop
            theta = (rr / dr)
            If (iupdat > m) Then
                num = 1
                Do While (num <= (col - 1))
                    num5 = 1
                    index = 1
                    Do While (index <= num)
                        ss(index, num) = ss((index + num5), (num + 1))
                        index += 1
                    Loop
                    num5 = ((num + 1) - num)
                    index = num
                    Do While (index <= (col - 1))
                        sy(index, num) = sy((index + num5), (num + 1))
                        index += 1
                    Loop
                    num += 1
                Loop
            End If
            num2 = head
            num = 1
            Do While (num <= (col - 1))
                num3 = 0
                index = 1
                Do While (index <= n)
                    num3 = (num3 + (d(index) * wy(index, num2)))
                    index += 1
                Loop
                sy(col, num) = num3
                num3 = 0
                index = 1
                Do While (index <= n)
                    num3 = (num3 + (ws(index, num2) * d(index)))
                    index += 1
                Loop
                ss(num, col) = num3
                num2 = ((num2 Mod m) + 1)
                num += 1
            Loop
            If (stp = 1) Then
                ss(col, col) = dtd
            Else
                ss(col, col) = ((stp * stp) * dtd)
            End If
            sy(col, col) = dr
        End Sub

        Public Sub lbfgsbminimize(ByVal n As Integer, ByVal m As Integer, ByRef x As Double(), ByVal epsg As Double, ByVal epsf As Double, ByVal epsx As Double, ByVal maxits As Integer, ByRef nbd As Integer(), ByRef l As Double(), ByRef u As Double(), ByRef info As Integer)
            Dim f As Double = 0
            Dim g As Double()
            Dim abort As Boolean = False
            Dim numArray2 As Double()
            Dim numArray3 As Double()
            Dim ws(,) As Double
            Dim wy(,) As Double
            Dim sy(,) As Double
            Dim ss(,) As Double
            Dim wt(,) As Double
            Dim wn(,) As Double
            Dim numArray10(,) As Double
            Dim xcp As Double()
            Dim r As Double()
            Dim d As Double()
            Dim t As Double()
            Dim wa As Double()
            Dim sg As Double()
            Dim yg As Double()
            Dim index As Integer()
            Dim iwhere As Integer()
            Dim iorder As Integer()
            Dim csave As Integer = 0
            Dim task As Integer = 0
            Dim prjctd As Boolean = False
            Dim cnstnd As Boolean = False
            Dim boxed As Boolean = False
            Dim updatd As Boolean = False
            Dim wrk As Boolean = False
            Dim num4 As Integer = 0
            Dim k As Integer = 0
            Dim num6 As Integer = 0
            Dim iback As Integer = 0
            Dim num8 As Integer = 0
            Dim head As Integer = 0
            Dim col As Integer = 0
            Dim iter As Integer = 0
            Dim itail As Integer = 0
            Dim iupdat As Integer = 0
            Dim nint As Integer = 0
            Dim nfgv As Integer = 0
            Dim num16 As Integer = 0
            Dim ifun As Integer = 0
            Dim iword As Integer = 0
            Dim nfree As Integer = 0
            Dim ileave As Integer = 0
            Dim nenter As Integer = 0
            Dim theta As Double = 0
            Dim fold As Double = 0
            Dim dr As Double = 0
            Dim rr As Double = 0
            Dim dnrm As Double = 0
            Dim xstep As Double = 0
            Dim sbgnrm As Double = 0
            Dim num29 As Double = 0
            Dim dtd As Double = 0
            Dim gd As Double = 0
            Dim gdold As Double = 0
            Dim stp As Double = 0
            Dim stpmx As Double = 0
            Dim num35 As Double = 0
            Dim workvec As Double()
            Dim numArray22 As Double()
            Dim dsave As Double()
            Dim p As Double()
            Dim c As Double()
            Dim wbp As Double()
            Dim v As Double()
            Dim workmat(,) As Double
            Dim isave As Integer()
            Dim num36 As Integer = 0
            Dim num37 As Integer = 0
            workvec = New Double((m + 1) - 1) {}
            numArray22 = New Double(((2 * m) + 1) - 1) {}
            workmat = New Double((m + 1) - 1, (m + 1) - 1) {}
            isave = New Integer(3 - 1) {}
            dsave = New Double(14 - 1) {}
            p = New Double(((2 * m) + 1) - 1) {}
            c = New Double(((2 * m) + 1) - 1) {}
            wbp = New Double(((2 * m) + 1) - 1) {}
            v = New Double(((2 * m) + 1) - 1) {}
            g = New Double((n + 1) - 1) {}
            numArray2 = New Double((n + 1) - 1) {}
            numArray3 = New Double((n + 1) - 1) {}
            ws = New Double((n + 1) - 1, (m + 1) - 1) {}
            wy = New Double((n + 1) - 1, (m + 1) - 1) {}
            sy = New Double((m + 1) - 1, (m + 1) - 1) {}
            ss = New Double((m + 1) - 1, (m + 1) - 1) {}
            wt = New Double((m + 1) - 1, (m + 1) - 1) {}
            wn = New Double(((2 * m) + 1) - 1, ((2 * m) + 1) - 1) {}
            numArray10 = New Double(((2 * m) + 1) - 1, ((2 * m) + 1) - 1) {}
            xcp = New Double((n + 1) - 1) {}
            r = New Double((n + 1) - 1) {}
            d = New Double((n + 1) - 1) {}
            t = New Double((n + 1) - 1) {}
            wa = New Double(((8 * m) + 1) - 1) {}
            sg = New Double((m + 1) - 1) {}
            yg = New Double((m + 1) - 1) {}
            index = New Integer((n + 1) - 1) {}
            iwhere = New Integer((n + 1) - 1) {}
            iorder = New Integer((n + 1) - 1) {}
            col = 0
            head = 1
            theta = 1
            iupdat = 0
            updatd = False
            iter = 0
            nfgv = 0
            nint = 0
            num6 = 0
            num8 = 0
            nfree = n
            num16 = 0
            Me.lbfgsberrclb(n, m, epsf, l, u, nbd, task, num16, k)
            If ((((task = 2) Or (maxits < 0)) Or (epsg < 0)) Or (epsx < 0)) Then
                info = -1
                Return
            End If
            Me.lbfgsbactive(n, l, u, nbd, x, iwhere, prjctd, cnstnd, boxed)
            num36 = 1
            Do While (num36 <= n)
                numArray2(num36) = x(num36)
                num36 += 1
            Loop
            Me.funcgrad(x, f, g)
            nfgv = 1
            Me.lbfgsbprojgr(n, l, u, nbd, x, g, sbgnrm)
            If (sbgnrm <= epsg) Then
                info = 4
                Return
            End If
Label_03EE:
            iword = -1
            If (Not cnstnd And (col > 0)) Then
                num36 = 1
                Do While (num36 <= n)
                    xcp(num36) = x(num36)
                    num36 += 1
                Loop
                wrk = updatd
                nint = 0
            Else
                num36 = 1
                Do While (num36 <= (2 * m))
                    p(num36) = wa(num36)
                    num36 += 1
                Loop
                num37 = (((2 * m) + 1) - 1)
                num36 = 1
                Do While (num36 <= (2 * m))
                    c(num36) = wa((num36 + num37))
                    num36 += 1
                Loop
                num37 = (((4 * m) + 1) - 1)
                num36 = 1
                Do While (num36 <= (2 * m))
                    wbp(num36) = wa((num36 + num37))
                    num36 += 1
                Loop
                num37 = (((6 * m) + 1) - 1)
                num36 = 1
                Do While (num36 <= (2 * m))
                    v(num36) = wa((num36 + num37))
                    num36 += 1
                Loop
                Me.lbfgsbcauchy(n, x, l, u, nbd, g, iorder, iwhere, t, d, xcp, m, wy, ws, sy, wt, theta, col, head, p, c, wbp, v, nint, sg, yg, sbgnrm, num16, workvec)
                num36 = 1
                Do While (num36 <= (2 * m))
                    wa(num36) = p(num36)
                    num36 += 1
                Loop
                num37 = (1 - ((2 * m) + 1))
                num36 = ((2 * m) + 1)
                Do While (num36 <= (4 * m))
                    wa(num36) = c((num36 + num37))
                    num36 += 1
                Loop
                num37 = (1 - ((4 * m) + 1))
                num36 = ((4 * m) + 1)
                Do While (num36 <= (6 * m))
                    wa(num36) = wbp((num36 + num37))
                    num36 += 1
                Loop
                num37 = (1 - ((6 * m) + 1))
                num36 = ((6 * m) + 1)
                Do While (num36 <= (8 * m))
                    wa(num36) = v((num36 + num37))
                    num36 += 1
                Loop
                If (num16 <> 0) Then
                    num16 = 0
                    col = 0
                    head = 1
                    theta = 1
                    iupdat = 0
                    updatd = False
                    GoTo Label_03EE
                End If
                num6 = (num6 + nint)
                Me.lbfgsbfreev(n, nfree, index, nenter, ileave, iorder, iwhere, wrk, updatd, cnstnd, iter)
            End If
            If ((nfree <> 0) And (col <> 0)) Then
                If wrk Then
                    Me.lbfgsbformk(n, nfree, index, nenter, ileave, iorder, iupdat, updatd, wn, numArray10, m, ws, wy, sy, theta, col, head, num16, workvec, workmat)
                End If
                If (num16 <> 0) Then
                    num16 = 0
                    col = 0
                    head = 1
                    theta = 1
                    iupdat = 0
                    updatd = False
                    GoTo Label_03EE
                End If
                Me.lbfgsbcmprlb(n, m, x, g, ws, wy, sy, wt, xcp, r, wa, index, theta, col, head, nfree, cnstnd, num16, workvec, numArray22)
                If (num16 = 0) Then
                    Me.lbfgsbsubsm(n, m, nfree, index, l, u, nbd, xcp, r, ws, wy, theta, col, head, iword, wa, wn, num16)
                End If
                If (num16 <> 0) Then
                    num16 = 0
                    col = 0
                    head = 1
                    theta = 1
                    iupdat = 0
                    updatd = False
                    GoTo Label_03EE
                End If
            End If
            num4 = 1
            Do While (num4 <= n)
                d(num4) = (xcp(num4) - x(num4))
                num4 += 1
            Loop
            task = 0
            Do While True
                Me.lbfgsblnsrlb(n, l, u, nbd, x, f, fold, gd, gdold, g, d, r, t, xcp, stp, dnrm, dtd, xstep, stpmx, iter, ifun, iback, nfgv, num16, task, boxed, cnstnd, csave, isave, dsave)
                If (((num16 <> 0) Or (iback >= 20)) Or (task <> 1)) Then
                    Exit Do
                End If
                Me.funcgrad(x, f, g)
                If Double.IsNaN(x(1)) Then Exit Do
            Loop
            If (num16 <> 0) Then
                num36 = 1
                Do While (num36 <= n)
                    x(num36) = t(num36)
                    num36 += 1
                Loop
                num36 = 1
                Do While (num36 <= n)
                    g(num36) = r(num36)
                    num36 += 1
                Loop
                f = fold
                If (col = 0) Then
                    If (num16 = 0) Then
                        num16 = -9
                        nfgv -= 1
                        ifun -= 1
                        iback -= 1
                    End If
                    task = 2
                    iter += 1
                    info = -2
                    Return
                End If
                If (num16 = 0) Then
                    nfgv -= 1
                End If
                num16 = 0
                col = 0
                head = 1
                theta = 1
                iupdat = 0
                updatd = False
                GoTo Label_03EE
            End If
            iter += 1
            Me.lbfgsbnewiteration(x, f, g, abort)
            If abort Then Exit Sub
            Me.lbfgsbprojgr(n, l, u, nbd, x, g, sbgnrm)
            If (sbgnrm <= epsg) Then
                info = 4
            Else
                num36 = 1
                Do While (num36 <= n)
                    numArray3(num36) = numArray2(num36)
                    num36 += 1
                Loop
                num36 = 1
                Do While (num36 <= n)
                    numArray3(num36) = (numArray3(num36) - x(num36))
                    num36 += 1
                Loop
                num35 = 0
                num36 = 1
                Do While (num36 <= n)
                    num35 = (num35 + (numArray3(num36) * numArray3(num36)))
                    num36 += 1
                Loop
                If (Math.Sqrt(num35) <= epsx) Then
                    info = 2
                Else
                    num29 = Math.Max(Math.Abs(fold), Math.Max(Math.Abs(f), 1))
                    If ((fold - f) <= (epsf * num29)) Then
                        info = 1
                    ElseIf ((iter > maxits) And (maxits > 0)) Then
                        info = 5
                    ElseIf Me.additionallbfgsbstoppingcriterion(iter, x, f, g) Then
                        info = 0
                    Else
                        num36 = 1
                        Do While (num36 <= n)
                            numArray2(num36) = x(num36)
                            num36 += 1
                        Loop
                        num4 = 1
                        Do While (num4 <= n)
                            r(num4) = (g(num4) - r(num4))
                            num4 += 1
                        Loop
                        rr = 0
                        num36 = 1
                        Do While (num36 <= n)
                            rr = (rr + (r(num36) * r(num36)))
                            num36 += 1
                        Loop
                        If (stp = 1) Then
                            dr = (gd - gdold)
                            num29 = -gdold
                        Else
                            dr = ((gd - gdold) * stp)
                            num36 = 1
                            Do While (num36 <= n)
                                d(num36) = (stp * d(num36))
                                num36 += 1
                            Loop
                            num29 = -(gdold * stp)
                        End If
                        If (dr <= (0.0000000000000005 * num29)) Then
                            num8 += 1
                            updatd = False
                        Else
                            updatd = True
                            iupdat += 1
                            Me.lbfgsbmatupd(n, m, ws, wy, sy, ss, d, r, itail, iupdat, col, head, theta, rr, dr, stp, dtd)
                            Me.lbfgsbformt(m, wt, sy, ss, col, theta, num16)
                            If (num16 <> 0) Then
                                num16 = 0
                                col = 0
                                head = 1
                                theta = 1
                                iupdat = 0
                                updatd = False
                            End If
                        End If
                        GoTo Label_03EE
                    End If
                End If
            End If
        End Sub

        Private Sub lbfgsbprojgr(ByVal n As Integer, ByRef l As Double(), ByRef u As Double(), ByRef nbd As Integer(), ByRef x As Double(), ByRef g As Double(), ByRef sbgnrm As Double)
            Dim index As Integer = 0
            Dim num2 As Double = 0
            sbgnrm = 0
            index = 1
            Do While (index <= n)
                num2 = g(index)
                If (nbd(index) <> 0) Then
                    If (num2 < 0) Then
                        If (nbd(index) >= 2) Then
                            num2 = Math.Max((x(index) - u(index)), num2)
                        End If
                    ElseIf (nbd(index) <= 2) Then
                        num2 = Math.Min((x(index) - l(index)), num2)
                    End If
                End If
                sbgnrm = Math.Max(sbgnrm, Math.Abs(num2))
                index += 1
            Loop
        End Sub

        Private Sub lbfgsbsubsm(ByVal n As Integer, ByVal m As Integer, ByVal nsub As Integer, ByRef ind As Integer(), ByRef l As Double(), ByRef u As Double(), ByRef nbd As Integer(), ByRef x As Double(), ByRef d As Double(), ByRef ws As Double(,), ByRef wy As Double(,), ByVal theta As Double, ByVal col As Integer, ByVal head As Integer, ByRef iword As Integer, ByRef wv As Double(), ByRef wn As Double(,), ByRef info As Integer)
            Dim num As Integer = 0
            Dim num2 As Integer = 0
            Dim index As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Integer = 0
            Dim num6 As Integer = 0
            Dim num7 As Integer = 0
            Dim num8 As Integer = 0
            Dim num9 As Double = 0
            Dim num10 As Double = 0
            Dim num11 As Double = 0
            Dim num12 As Double = 0
            If (nsub > 0) Then
                num = head
                num6 = 1
                Do While (num6 <= col)
                    num11 = 0
                    num12 = 0
                    num7 = 1
                    Do While (num7 <= nsub)
                        num8 = ind(num7)
                        num11 = (num11 + (wy(num8, num) * d(num7)))
                        num12 = (num12 + (ws(num8, num) * d(num7)))
                        num7 += 1
                    Loop
                    wv(num6) = num11
                    wv((col + num6)) = (theta * num12)
                    num = ((num Mod m) + 1)
                    num6 += 1
                Loop
                num2 = (2 * col)
                Me.lbfgsbdtrsl(wn, num2, wv, 11, info)
                If (info = 0) Then
                    num6 = 1
                    Do While (num6 <= col)
                        wv(num6) = -wv(num6)
                        num6 += 1
                    Loop
                    Me.lbfgsbdtrsl(wn, num2, wv, 1, info)
                    If (info = 0) Then
                        num = head
                        num4 = 1
                        Do While (num4 <= col)
                            num5 = (col + num4)
                            num6 = 1
                            Do While (num6 <= nsub)
                                num8 = ind(num6)
                                d(num6) = ((d(num6) + ((wy(num8, num) * wv(num4)) / theta)) + (ws(num8, num) * wv(num5)))
                                num6 += 1
                            Loop
                            num = ((num Mod m) + 1)
                            num4 += 1
                        Loop
                        num6 = 1
                        Do While (num6 <= nsub)
                            d(num6) = (d(num6) / theta)
                            num6 += 1
                        Loop
                        num9 = 1
                        num11 = num9
                        num6 = 1
                        Do While (num6 <= nsub)
                            num8 = ind(num6)
                            num10 = d(num6)
                            If (nbd(num8) <> 0) Then
                                If ((num10 < 0) And (nbd(num8) <= 2)) Then
                                    num12 = (l(num8) - x(num8))
                                    If (num12 >= 0) Then
                                        num11 = 0
                                    ElseIf ((num10 * num9) < num12) Then
                                        num11 = (num12 / num10)
                                    End If
                                ElseIf ((num10 > 0) And (nbd(num8) >= 2)) Then
                                    num12 = (u(num8) - x(num8))
                                    If (num12 <= 0) Then
                                        num11 = 0
                                    ElseIf ((num10 * num9) > num12) Then
                                        num11 = (num12 / num10)
                                    End If
                                End If
                                If (num11 < num9) Then
                                    num9 = num11
                                    index = num6
                                End If
                            End If
                            num6 += 1
                        Loop
                        If (num9 < 1) Then
                            num10 = d(index)
                            num8 = ind(index)
                            If (num10 > 0) Then
                                x(num8) = u(num8)
                                d(index) = 0
                            ElseIf (num10 < 0) Then
                                x(num8) = l(num8)
                                d(index) = 0
                            End If
                        End If
                        num6 = 1
                        Do While (num6 <= nsub)
                            num8 = ind(num6)
                            x(num8) = (x(num8) + (num9 * d(num6)))
                            num6 += 1
                        Loop
                        If (num9 < 1) Then
                            iword = 1
                        Else
                            iword = 0
                        End If
                    End If
                End If
            End If
        End Sub

    End Class

End Namespace
