Namespace DWSIM.MathEx.LM

    Public Class levenbergmarquardt

        '************************************************************************
        'Minpack Copyright Notice (1999) University of Chicago.  All rights reserved
        '
        'Redistribution and use in source and binary forms, with or
        'without modification, are permitted provided that the
        'following conditions are met:
        '
        '1. Redistributions of source code must retain the above
        'copyright notice, this list of conditions and the following
        'disclaimer.
        '
        '2. Redistributions in binary form must reproduce the above
        'copyright notice, this list of conditions and the following
        'disclaimer in the documentation and/or other materials
        'provided with the distribution.
        '
        '3. The end-user documentation included with the
        'redistribution, if any, must include the following
        'acknowledgment:
        '
        '   "This product includes software developed by the
        '   University of Chicago, as Operator of Argonne National
        '   Laboratory.
        '
        'Alternately, this acknowledgment may appear in the software
        'itself, if and wherever such third-party acknowledgments
        'normally appear.
        '
        '4. WARRANTY DISCLAIMER. THE SOFTWARE IS SUPPLIED "AS IS"
        'WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT HOLDER, THE
        'UNITED STATES, THE UNITED STATES DEPARTMENT OF ENERGY, AND
        'THEIR EMPLOYEES: (1) DISCLAIM ANY WARRANTIES, EXPRESS OR
        'IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES
        'OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE
        'OR NON-INFRINGEMENT, (2) DO NOT ASSUME ANY LEGAL LIABILITY
        'OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR
        'USEFULNESS OF THE SOFTWARE, (3) DO NOT REPRESENT THAT USE OF
        'THE SOFTWARE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS, (4)
        'DO NOT WARRANT THAT THE SOFTWARE WILL FUNCTION
        'UNINTERRUPTED, THAT IT IS ERROR-FREE OR THAT ANY ERRORS WILL
        'BE CORRECTED.
        '
        '5. LIMITATION OF LIABILITY. IN NO EVENT WILL THE COPYRIGHT
        'HOLDER, THE UNITED STATES, THE UNITED STATES DEPARTMENT OF
        'ENERGY, OR THEIR EMPLOYEES: BE LIABLE FOR ANY INDIRECT,
        'INCIDENTAL, CONSEQUENTIAL, SPECIAL OR PUNITIVE DAMAGES OF
        'ANY KIND OR NATURE, INCLUDING BUT NOT LIMITED TO LOSS OF
        'PROFITS OR LOSS OF DATA, FOR ANY REASON WHATSOEVER, WHETHER
        'SUCH LIABILITY IS ASSERTED ON THE BASIS OF CONTRACT, TORT
        '(INCLUDING NEGLIGENCE OR STRICT LIABILITY), OR OTHERWISE,
        'EVEN IF ANY OF SAID PARTIES HAS BEEN WARNED OF THE
        'POSSIBILITY OF SUCH LOSS OR DAMAGES.
        '************************************************************************

        '
        '    This members must be defined by you:
        '    static void funcvecjac(ref double[] x,
        '        ref double[] fvec,
        '        ref double[,] fjac,
        '        ref int iflag)
        '    
        Public fv As funcvecjacdelegate
        Delegate Sub funcvecjacdelegate(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

        Sub New()

        End Sub

        Sub DefineFuncGradDelegate(ByVal fvj As funcvecjacdelegate)
            Me.fv = fvj
        End Sub

        Public Sub funcvecjac(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)
            fv.Invoke(x, fvec, fjac, iflag)
        End Sub

        '************************************************************************
        '    The subroutine minimizes the sum of squares  of  M nonlinear finctions  of
        '    N  arguments  with  Levenberg-Marquardt  algorithm  using  Jacobian    and
        '    information about function values.
        '
        '    Programmer  should  redefine  FuncVecJac  subroutine  which  takes array X
        '    (argument)  whose  index  ranges  from  1 to N as an input and if variable
        '    IFlag is equal to:
        '        * 1, returns vector of function values in array FVec (in elements from
        '          1 to M), not changing FJac.
        '        * 2,  returns  Jacobian  in  array FJac (in elements [1..M,1..N]), not
        '          changing FVec.
        '    The subroutine can change the IFlag parameter by setting it into a negative
        '    number. It will terminate program.
        '
        '    Programmer  can  also  redefine  LevenbergMarquardtNewIteration subroutine
        '    which is called on each new step.   Current  point  X  is  passed into the
        '    subroutine.   It  is  reasonable  to  redefine  the  subroutine for better
        '    debugging, for example, to visualize the solution process.
        '
        '    The AdditionalLevenbergMarquardtStoppingCriterion could  be  redefined  to
        '    modify stopping conditions.
        '
        '    Input parameters:
        '        N       –   number of unknowns, N>0.
        '        M       –   number of summable functions, M>=N.
        '        X       –   initial solution approximation.
        '                    Array whose index ranges from 1 to N.
        '        EpsG    –   stopping criterion. Iterations are stopped, if  cosine  of
        '                    the angle between vector of function values  and  each  of
        '                    the  Jacobian  columns  if  less or equal EpsG by absolute
        '                    value. In fact this value defines stopping condition which
        '                    is based on the function gradient smallness.
        '        EpsF    –   stopping criterion. Iterations are  stopped,  if  relative
        '                    decreasing of sum of function values squares (real and
        '                    predicted on the base of extrapolation)  is  less or equal
        '                    EpsF.
        '        EpsX    –   stopping criterion. Iterations are  stopped,  if  relative
        '                    change of solution is less or equal EpsX.
        '        MaxIts  –   stopping  criterion.  Iterations  are  stopped,  if  their
        '                    number exceeds MaxIts.
        '
        '    Output parameters:
        '        X       –   solution
        '                    Array whose index ranges from 1 to N.
        '        Info    –   a reason of a program completion:
        '                        * -1 wrong parameters were specified,
        '                        * 0 interrupted by user,
        '                        * 1 relative decrease of sum of function values
        '                            squares (real and predicted on the base  of
        '                            extrapolation) is less or equal EpsF.
        '                        * 2 relative change of solution is less or equal
        '                            EpsX.
        '                        * 3 conditions (1) and (2) are fulfilled.
        '                        * 4 cosine of the angle between vector of function
        '                            values and each of the Jacobian columns is less
        '                            or equal EpsG by absolute value.
        '                        * 5 number of iterations exceeds MaxIts.
        '                        * 6 EpsF is too small.
        '                            It is impossible to get a better result.
        '                        * 7 EpsX is too small.
        '                            It is impossible to get a better result.
        '                        * 8 EpsG is too small. Vector of functions is
        '                            orthogonal to Jacobian columns with near-machine
        '                            precision.
        '    argonne national laboratory. minpack project. march 1980.
        '    burton s. garbow, kenneth e. hillstrom, jorge j. more
        '
        '    Contributors:
        '        * Sergey Bochkanov (ALGLIB project). Translation from FORTRAN to
        '          pseudocode.
        '    ************************************************************************

        Public Sub levenbergmarquardtminimize(ByVal n As Integer, ByVal m As Integer, ByRef x As Double(), ByVal epsg As Double, ByVal epsf As Double, ByVal epsx As Double, _
         ByVal maxits As Integer, ByRef info As Integer)
            Dim fvec As Double() = New Double(-1) {}
            Dim qtf As Double() = New Double(-1) {}
            Dim ipvt As Integer() = New Integer(-1) {}
            Dim fjac As Double(,) = New Double(-1, -1) {}
            Dim w2 As Double(,) = New Double(-1, -1) {}
            Dim wa1 As Double() = New Double(-1) {}
            Dim wa2 As Double() = New Double(-1) {}
            Dim wa3 As Double() = New Double(-1) {}
            Dim wa4 As Double() = New Double(-1) {}
            Dim diag As Double() = New Double(-1) {}
            Dim mode As Integer = 0
            Dim nfev As Integer = 0
            Dim njev As Integer = 0
            Dim factor As Double = 0
            Dim i As Integer = 0
            Dim iflag As Integer = 0
            Dim iter As Integer = 0
            Dim j As Integer = 0
            Dim l As Integer = 0
            Dim actred As Double = 0
            Dim delta As Double = 0
            Dim dirder As Double = 0
            Dim fnorm As Double = 0
            Dim fnorm1 As Double = 0
            Dim gnorm As Double = 0
            Dim par As Double = 0
            Dim pnorm As Double = 0
            Dim prered As Double = 0
            Dim ratio As Double = 0
            Dim sum As Double = 0
            Dim temp As Double = 0
            Dim temp1 As Double = 0
            Dim temp2 As Double = 0
            Dim xnorm As Double = 0
            Dim p1 As Double = 0
            Dim p5 As Double = 0
            Dim p25 As Double = 0
            Dim p75 As Double = 0
            Dim p0001 As Double = 0
            Dim i_ As Integer = 0


            '
            ' Factor is a positive input variable used in determining the
            ' initial step bound. This bound is set to the product of
            ' factor and the euclidean norm of diag*x if nonzero, or else
            ' to factor itself. in most cases factor should lie in the
            ' interval (.1,100.).
            ' 100.0 is a generally recommended value.
            '
            factor = 100.0R

            '
            ' mode is an integer input variable. if mode = 1, the
            ' variables will be scaled internally. if mode = 2,
            ' the scaling is specified by the input diag. other
            ' values of mode are equivalent to mode = 1.
            '
            mode = 1

            '
            ' diag is an array of length n. if mode = 1
            ' diag is internally set. if mode = 2, diag
            ' must contain positive entries that serve as
            ' multiplicative scale factors for the variables.
            '
            diag = New Double(n) {}

            '
            ' Initialization
            '
            qtf = New Double(n) {}
            fvec = New Double(m) {}
            fjac = New Double(m, n) {}
            w2 = New Double(n, m) {}
            ipvt = New Integer(n) {}
            wa1 = New Double(n) {}
            wa2 = New Double(n) {}
            wa3 = New Double(n) {}
            wa4 = New Double(m) {}
            p1 = 0.1R
            p5 = 0.5R
            p25 = 0.25R
            p75 = 0.75R
            p0001 = 0.0001
            info = 0
            iflag = 0
            nfev = 0
            njev = 0

            '
            ' check the input parameters for errors.
            '
            If n <= 0 Or m < n Then
                info = -1
                Exit Sub
            End If
            If epsf < 0 Or epsx < 0 Or epsg < 0 Then
                info = -1
                Exit Sub
            End If
            If factor <= 0 Then
                info = -1
                Exit Sub
            End If
            If mode = 2 Then
                For j = 1 To n
                    If diag(j) <= 0 Then
                        info = -1
                        Exit Sub
                    End If
                Next
            End If

            '
            ' evaluate the function at the starting point
            ' and calculate its norm.
            '
            iflag = 1
            funcvecjac(x, fvec, fjac, iflag)
            nfev = 1
            If iflag < 0 Then
                info = 0
                Exit Sub
            End If
            fnorm = 0.0R
            For i_ = 1 To m
                fnorm += fvec(i_) * fvec(i_)
            Next
            fnorm = Math.Sqrt(fnorm)

            '
            ' initialize levenberg-marquardt parameter and iteration counter.
            '
            par = 0
            iter = 1

            '
            ' beginning of the outer loop.
            '
            While True

                '
                ' New iteration
                '
                levenbergmarquardtnewiteration(x)

                '
                ' calculate the jacobian matrix.
                '
                iflag = 2
                funcvecjac(x, fvec, fjac, iflag)
                njev = njev + 1
                If iflag < 0 Then
                    info = 0
                    Exit Sub
                End If

                '
                ' compute the qr factorization of the jacobian.
                '
                levenbergmarquardtqrfac(m, n, fjac, True, ipvt, wa1, _
                 wa2, wa3, w2)

                '
                ' on the first iteration and if mode is 1, scale according
                ' to the norms of the columns of the initial jacobian.
                '
                If iter = 1 Then
                    If mode <> 2 Then
                        For j = 1 To n
                            diag(j) = wa2(j)
                            If wa2(j) = 0 Then
                                diag(j) = 1
                            End If
                        Next
                    End If

                    '
                    ' on the first iteration, calculate the norm of the scaled x
                    ' and initialize the step bound delta.
                    '
                    For j = 1 To n
                        wa3(j) = diag(j) * x(j)
                    Next
                    xnorm = 0.0R
                    For i_ = 1 To n
                        xnorm += wa3(i_) * wa3(i_)
                    Next
                    xnorm = Math.Sqrt(xnorm)
                    delta = factor * xnorm
                    If delta = 0 Then
                        delta = factor
                    End If
                End If

                '
                ' form (q transpose)*fvec and store the first n components in
                ' qtf.
                '
                For i = 1 To m
                    wa4(i) = fvec(i)
                Next
                For j = 1 To n
                    If fjac(j, j) <> 0 Then
                        sum = 0
                        For i = j To m
                            sum = sum + fjac(i, j) * wa4(i)
                        Next
                        temp = -(sum / fjac(j, j))
                        For i = j To m
                            wa4(i) = wa4(i) + fjac(i, j) * temp
                        Next
                    End If
                    fjac(j, j) = wa1(j)
                    qtf(j) = wa4(j)
                Next

                '
                ' compute the norm of the scaled gradient.
                '
                gnorm = 0
                If fnorm <> 0 Then
                    For j = 1 To n
                        l = ipvt(j)
                        If wa2(l) <> 0 Then
                            sum = 0
                            For i = 1 To j
                                sum = sum + fjac(i, j) * (qtf(i) / fnorm)
                            Next
                            gnorm = Math.Max(gnorm, Math.Abs(sum / wa2(l)))
                        End If
                    Next
                End If

                '
                ' test for convergence of the gradient norm.
                '
                If gnorm <= epsg Then
                    info = 4
                End If
                If info <> 0 Then
                    Exit Sub
                End If

                '
                ' rescale if necessary.
                '
                If mode <> 2 Then
                    For j = 1 To n
                        diag(j) = Math.Max(diag(j), wa2(j))
                    Next
                End If

                '
                ' beginning of the inner loop.
                '
                While True

                    '
                    ' determine the levenberg-marquardt parameter.
                    '
                    levenbergmarquardtpar(n, fjac, ipvt, diag, qtf, delta, _
                     par, wa1, wa2, wa3, wa4)

                    '
                    ' store the direction p and x + p. calculate the norm of p.
                    '
                    For j = 1 To n
                        wa1(j) = -wa1(j)
                        wa2(j) = x(j) + wa1(j)
                        wa3(j) = diag(j) * wa1(j)
                    Next
                    pnorm = 0.0R
                    For i_ = 1 To n
                        pnorm += wa3(i_) * wa3(i_)
                    Next
                    pnorm = Math.Sqrt(pnorm)

                    '
                    ' on the first iteration, adjust the initial step bound.
                    '
                    If iter = 1 Then
                        delta = Math.Min(delta, pnorm)
                    End If

                    '
                    ' evaluate the function at x + p and calculate its norm.
                    '
                    iflag = 1
                    funcvecjac(wa2, wa4, fjac, iflag)
                    nfev = nfev + 1
                    If iflag < 0 Then
                        info = 0
                        Exit Sub
                    End If
                    fnorm1 = 0.0R
                    For i_ = 1 To m
                        fnorm1 += wa4(i_) * wa4(i_)
                    Next
                    fnorm1 = Math.Sqrt(fnorm1)

                    '
                    ' compute the scaled actual reduction.
                    '
                    actred = -1
                    If p1 * fnorm1 < fnorm Then
                        actred = 1 - AP.MathEx.Sqr(fnorm1 / fnorm)
                    End If

                    '
                    ' compute the scaled predicted reduction and
                    ' the scaled directional derivative.
                    '
                    For j = 1 To n
                        wa3(j) = 0
                        l = ipvt(j)
                        temp = wa1(l)
                        For i = 1 To j
                            wa3(i) = wa3(i) + fjac(i, j) * temp
                        Next
                    Next
                    temp1 = 0.0R
                    For i_ = 1 To n
                        temp1 += wa3(i_) * wa3(i_)
                    Next
                    temp1 = Math.Sqrt(temp1) / fnorm
                    temp2 = Math.Sqrt(par) * pnorm / fnorm
                    prered = AP.MathEx.Sqr(temp1) + AP.MathEx.Sqr(temp2) / p5
                    dirder = -(AP.MathEx.Sqr(temp1) + AP.MathEx.Sqr(temp2))

                    '
                    ' compute the ratio of the actual to the predicted
                    ' reduction.
                    '
                    ratio = 0
                    If prered <> 0 Then
                        ratio = actred / prered
                    End If

                    '
                    ' update the step bound.
                    '
                    If ratio > p25 Then
                        If par = 0 Or ratio >= p75 Then
                            delta = pnorm / p5
                            par = p5 * par
                        End If
                    Else
                        If actred >= 0 Then
                            temp = p5
                        End If
                        If actred < 0 Then
                            temp = p5 * dirder / (dirder + p5 * actred)
                        End If
                        If p1 * fnorm1 >= fnorm Or temp < p1 Then
                            temp = p1
                        End If
                        delta = temp * Math.Min(delta, pnorm / p1)
                        par = par / temp
                    End If

                    '
                    ' test for successful iteration.
                    '
                    If ratio >= p0001 Then

                        '
                        ' successful iteration. update x, fvec, and their norms.
                        '
                        For j = 1 To n
                            x(j) = wa2(j)
                            wa2(j) = diag(j) * x(j)
                        Next
                        For i = 1 To m
                            fvec(i) = wa4(i)
                        Next
                        xnorm = 0.0R
                        For i_ = 1 To n
                            xnorm += wa2(i_) * wa2(i_)
                        Next
                        xnorm = Math.Sqrt(xnorm)
                        fnorm = fnorm1
                        iter = iter + 1
                    End If

                    '
                    ' tests for convergence.
                    '
                    If Math.Abs(actred) <= epsf And prered <= epsf And p5 * ratio <= 1 Then
                        info = 1
                    End If
                    If delta <= epsx * xnorm Then
                        info = 2
                    End If
                    If Math.Abs(actred) <= epsf And prered <= epsf And p5 * ratio <= 1 And info = 2 Then
                        info = 3
                    End If
                    If info <> 0 Then
                        Exit Sub
                    End If

                    '
                    ' tests for termination and stringent tolerances.
                    '
                    If iter >= maxits And maxits > 0 Then
                        info = 5
                    End If
                    If Math.Abs(actred) <= AP.MathEx.MachineEpsilon And prered <= AP.MathEx.MachineEpsilon And p5 * ratio <= 1 Then
                        info = 6
                    End If
                    If delta <= AP.MathEx.MachineEpsilon * xnorm Then
                        info = 7
                    End If
                    If gnorm <= AP.MathEx.MachineEpsilon Then
                        info = 8
                    End If
                    If info <> 0 Then
                        Exit Sub
                    End If

                    '
                    ' end of the inner loop. repeat if iteration unsuccessful.
                    '
                    If ratio < p0001 Then
                        Continue While
                    End If
                    Exit While
                End While

                '
                ' Termination criterion
                '
                If additionallevenbergmarquardtstoppingcriterion(iter) Then
                    info = 0
                    Exit Sub
                End If

                '
                ' end of the outer loop.
                '
            End While
        End Sub


        Private Shared Sub levenbergmarquardtqrfac(ByVal m As Integer, ByVal n As Integer, ByRef a As Double(,), ByVal pivot As Boolean, ByRef ipvt As Integer(), ByRef rdiag As Double(), _
         ByRef acnorm As Double(), ByRef wa As Double(), ByRef w2 As Double(,))
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim jp1 As Integer = 0
            Dim k As Integer = 0
            Dim kmax As Integer = 0
            Dim minmn As Integer = 0
            Dim ajnorm As Double = 0
            Dim sum As Double = 0
            Dim temp As Double = 0
            Dim v As Double = 0
            Dim i_ As Integer = 0


            '
            ' Copy from a to w2 and transpose
            '
            For i = 1 To m
                For i_ = 1 To n
                    w2(i_, i) = a(i, i_)
                Next
            Next

            '
            ' compute the initial column norms and initialize several arrays.
            '
            For j = 1 To n
                v = 0.0R
                For i_ = 1 To m
                    v += w2(j, i_) * w2(j, i_)
                Next
                acnorm(j) = Math.Sqrt(v)
                rdiag(j) = acnorm(j)
                wa(j) = rdiag(j)
                If pivot Then
                    ipvt(j) = j
                End If
            Next

            '
            ' reduce a to r with householder transformations.
            '
            minmn = Math.Min(m, n)
            For j = 1 To minmn
                If pivot Then

                    '
                    ' bring the column of largest norm into the pivot position.
                    '
                    kmax = j
                    For k = j To n
                        If rdiag(k) > rdiag(kmax) Then
                            kmax = k
                        End If
                    Next
                    If kmax <> j Then
                        For i = 1 To m
                            temp = w2(j, i)
                            w2(j, i) = w2(kmax, i)
                            w2(kmax, i) = temp
                        Next
                        rdiag(kmax) = rdiag(j)
                        wa(kmax) = wa(j)
                        k = ipvt(j)
                        ipvt(j) = ipvt(kmax)
                        ipvt(kmax) = k
                    End If
                End If

                '
                ' compute the householder transformation to reduce the
                ' j-th column of a to a multiple of the j-th unit vector.
                '
                v = 0.0R
                For i_ = j To m
                    v += w2(j, i_) * w2(j, i_)
                Next
                ajnorm = Math.Sqrt(v)
                If ajnorm <> 0 Then
                    If w2(j, j) < 0 Then
                        ajnorm = -ajnorm
                    End If
                    v = 1 / ajnorm
                    For i_ = j To m
                        w2(j, i_) = v * w2(j, i_)
                    Next
                    w2(j, j) = w2(j, j) + 1.0R

                    '
                    ' apply the transformation to the remaining columns
                    ' and update the norms.
                    '
                    jp1 = j + 1
                    If n >= jp1 Then
                        For k = jp1 To n
                            sum = 0.0R
                            For i_ = j To m
                                sum += w2(j, i_) * w2(k, i_)
                            Next
                            temp = sum / w2(j, j)
                            For i_ = j To m
                                w2(k, i_) = w2(k, i_) - temp * w2(j, i_)
                            Next
                            If pivot And rdiag(k) <> 0 Then
                                temp = w2(k, j) / rdiag(k)
                                rdiag(k) = rdiag(k) * Math.Sqrt(Math.Max(0, 1 - AP.MathEx.Sqr(temp)))
                                If 0.05 * AP.MathEx.Sqr(rdiag(k) / wa(k)) <= AP.MathEx.MachineEpsilon Then
                                    v = 0.0R
                                    For i_ = jp1 To jp1 + m - j - 1
                                        v += w2(k, i_) * w2(k, i_)
                                    Next
                                    rdiag(k) = Math.Sqrt(v)
                                    wa(k) = rdiag(k)
                                End If
                            End If
                        Next
                    End If
                End If
                rdiag(j) = -ajnorm
            Next

            '
            ' Copy from w2 to a and transpose
            '
            For i = 1 To m
                For i_ = 1 To n
                    a(i, i_) = w2(i_, i)
                Next
            Next
        End Sub


        Private Shared Sub levenbergmarquardtqrsolv(ByVal n As Integer, ByRef r As Double(,), ByRef ipvt As Integer(), ByRef diag As Double(), ByRef qtb As Double(), ByRef x As Double(), _
         ByRef sdiag As Double(), ByRef wa As Double())
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim jp1 As Integer = 0
            Dim k As Integer = 0
            Dim kp1 As Integer = 0
            Dim l As Integer = 0
            Dim nsing As Integer = 0
            Dim cs As Double = 0
            Dim ct As Double = 0
            Dim qtbpj As Double = 0
            Dim sn As Double = 0
            Dim sum As Double = 0
            Dim t As Double = 0
            Dim temp As Double = 0


            '
            ' copy r and (q transpose)*b to preserve input and initialize s.
            ' in particular, save the diagonal elements of r in x.
            '
            For j = 1 To n
                For i = j To n
                    r(i, j) = r(j, i)
                Next
                x(j) = r(j, j)
                wa(j) = qtb(j)
            Next

            '
            ' eliminate the diagonal matrix d using a givens rotation.
            '
            For j = 1 To n

                '
                ' prepare the row of d to be eliminated, locating the
                ' diagonal element using p from the qr factorization.
                '
                l = ipvt(j)
                If diag(l) <> 0 Then
                    For k = j To n
                        sdiag(k) = 0
                    Next
                    sdiag(j) = diag(l)

                    '
                    ' the transformations to eliminate the row of d
                    ' modify only a single element of (q transpose)*b
                    ' beyond the first n, which is initially zero.
                    '
                    qtbpj = 0
                    For k = j To n

                        '
                        ' determine a givens rotation which eliminates the
                        ' appropriate element in the current row of d.
                        '
                        If sdiag(k) <> 0 Then
                            If Math.Abs(r(k, k)) >= Math.Abs(sdiag(k)) Then
                                t = sdiag(k) / r(k, k)
                                cs = 0.5 / Math.Sqrt(0.25 + 0.25 * AP.MathEx.Sqr(t))
                                sn = cs * t
                            Else
                                ct = r(k, k) / sdiag(k)
                                sn = 0.5 / Math.Sqrt(0.25 + 0.25 * AP.MathEx.Sqr(ct))
                                cs = sn * ct
                            End If

                            '
                            ' compute the modified diagonal element of r and
                            ' the modified element of ((q transpose)*b,0).
                            '
                            r(k, k) = cs * r(k, k) + sn * sdiag(k)
                            temp = cs * wa(k) + sn * qtbpj
                            qtbpj = -(sn * wa(k)) + cs * qtbpj
                            wa(k) = temp

                            '
                            ' accumulate the tranformation in the row of s.
                            '
                            kp1 = k + 1
                            If n >= kp1 Then
                                For i = kp1 To n
                                    temp = cs * r(i, k) + sn * sdiag(i)
                                    sdiag(i) = -(sn * r(i, k)) + cs * sdiag(i)
                                    r(i, k) = temp
                                Next
                            End If
                        End If
                    Next
                End If

                '
                ' store the diagonal element of s and restore
                ' the corresponding diagonal element of r.
                '
                sdiag(j) = r(j, j)
                r(j, j) = x(j)
            Next

            '
            ' solve the triangular system for z. if the system is
            ' singular, then obtain a least squares solution.
            '
            nsing = n
            For j = 1 To n
                If sdiag(j) = 0 And nsing = n Then
                    nsing = j - 1
                End If
                If nsing < n Then
                    wa(j) = 0
                End If
            Next
            If nsing >= 1 Then
                For k = 1 To nsing
                    j = nsing - k + 1
                    sum = 0
                    jp1 = j + 1
                    If nsing >= jp1 Then
                        For i = jp1 To nsing
                            sum = sum + r(i, j) * wa(i)
                        Next
                    End If
                    wa(j) = (wa(j) - sum) / sdiag(j)
                Next
            End If

            '
            ' permute the components of z back to components of x.
            '
            For j = 1 To n
                l = ipvt(j)
                x(l) = wa(j)
            Next
        End Sub


        Private Shared Sub levenbergmarquardtpar(ByVal n As Integer, ByRef r As Double(,), ByRef ipvt As Integer(), ByRef diag As Double(), ByRef qtb As Double(), ByVal delta As Double, _
         ByRef par As Double, ByRef x As Double(), ByRef sdiag As Double(), ByRef wa1 As Double(), ByRef wa2 As Double())
            Dim i As Integer = 0
            Dim iter As Integer = 0
            Dim j As Integer = 0
            Dim jm1 As Integer = 0
            Dim jp1 As Integer = 0
            Dim k As Integer = 0
            Dim l As Integer = 0
            Dim nsing As Integer = 0
            Dim dxnorm As Double = 0
            Dim dwarf As Double = 0
            Dim fp As Double = 0
            Dim gnorm As Double = 0
            Dim parc As Double = 0
            Dim parl As Double = 0
            Dim paru As Double = 0
            Dim sum As Double = 0
            Dim temp As Double = 0
            Dim v As Double = 0
            Dim i_ As Integer = 0

            dwarf = AP.MathEx.MinRealNumber

            '
            ' compute and store in x the gauss-newton direction. if the
            ' jacobian is rank-deficient, obtain a least squares solution.
            '
            nsing = n
            For j = 1 To n
                wa1(j) = qtb(j)
                If r(j, j) = 0 And nsing = n Then
                    nsing = j - 1
                End If
                If nsing < n Then
                    wa1(j) = 0
                End If
            Next
            If nsing >= 1 Then
                For k = 1 To nsing
                    j = nsing - k + 1
                    wa1(j) = wa1(j) / r(j, j)
                    temp = wa1(j)
                    jm1 = j - 1
                    If jm1 >= 1 Then
                        For i = 1 To jm1
                            wa1(i) = wa1(i) - r(i, j) * temp
                        Next
                    End If
                Next
            End If
            For j = 1 To n
                l = ipvt(j)
                x(l) = wa1(j)
            Next

            '
            ' initialize the iteration counter.
            ' evaluate the function at the origin, and test
            ' for acceptance of the gauss-newton direction.
            '
            iter = 0
            For j = 1 To n
                wa2(j) = diag(j) * x(j)
            Next
            v = 0.0R
            For i_ = 1 To n
                v += wa2(i_) * wa2(i_)
            Next
            dxnorm = Math.Sqrt(v)
            fp = dxnorm - delta
            If fp <= 0.1 * delta Then

                '
                ' termination.
                '
                If iter = 0 Then
                    par = 0
                End If
                Exit Sub
            End If

            '
            ' if the jacobian is not rank deficient, the newton
            ' step provides a lower bound, parl, for the zero of
            ' the function. otherwise set this bound to zero.
            '
            parl = 0
            If nsing >= n Then
                For j = 1 To n
                    l = ipvt(j)
                    wa1(j) = diag(l) * (wa2(l) / dxnorm)
                Next
                For j = 1 To n
                    sum = 0
                    jm1 = j - 1
                    If jm1 >= 1 Then
                        For i = 1 To jm1
                            sum = sum + r(i, j) * wa1(i)
                        Next
                    End If
                    wa1(j) = (wa1(j) - sum) / r(j, j)
                Next
                v = 0.0R
                For i_ = 1 To n
                    v += wa1(i_) * wa1(i_)
                Next
                temp = Math.Sqrt(v)
                parl = fp / delta / temp / temp
            End If

            '
            ' calculate an upper bound, paru, for the zero of the function.
            '
            For j = 1 To n
                sum = 0
                For i = 1 To j
                    sum = sum + r(i, j) * qtb(i)
                Next
                l = ipvt(j)
                wa1(j) = sum / diag(l)
            Next
            v = 0.0R
            For i_ = 1 To n
                v += wa1(i_) * wa1(i_)
            Next
            gnorm = Math.Sqrt(v)
            paru = gnorm / delta
            If paru = 0 Then
                paru = dwarf / Math.Min(delta, 0.1)
            End If

            '
            ' if the input par lies outside of the interval (parl,paru),
            ' set par to the closer endpoint.
            '
            par = Math.Max(par, parl)
            par = Math.Min(par, paru)
            If par = 0 Then
                par = gnorm / dxnorm
            End If

            '
            ' beginning of an iteration.
            '
            While True
                iter = iter + 1

                '
                ' evaluate the function at the current value of par.
                '
                If par = 0 Then
                    par = Math.Max(dwarf, 0.001 * paru)
                End If
                temp = Math.Sqrt(par)
                For j = 1 To n
                    wa1(j) = temp * diag(j)
                Next
                levenbergmarquardtqrsolv(n, r, ipvt, wa1, qtb, x, _
                 sdiag, wa2)
                For j = 1 To n
                    wa2(j) = diag(j) * x(j)
                Next
                v = 0.0R
                For i_ = 1 To n
                    v += wa2(i_) * wa2(i_)
                Next
                dxnorm = Math.Sqrt(v)
                temp = fp
                fp = dxnorm - delta

                '
                ' if the function is small enough, accept the current value
                ' of par. also test for the exceptional cases where parl
                ' is zero or the number of iterations has reached 10.
                '
                If Math.Abs(fp) <= 0.1 * delta Or parl = 0 And fp <= temp And temp < 0 Or iter = 10 Then
                    Exit While
                End If

                '
                ' compute the newton correction.
                '
                For j = 1 To n
                    l = ipvt(j)
                    wa1(j) = diag(l) * (wa2(l) / dxnorm)
                Next
                For j = 1 To n
                    wa1(j) = wa1(j) / sdiag(j)
                    temp = wa1(j)
                    jp1 = j + 1
                    If n >= jp1 Then
                        For i = jp1 To n
                            wa1(i) = wa1(i) - r(i, j) * temp
                        Next
                    End If
                Next
                v = 0.0R
                For i_ = 1 To n
                    v += wa1(i_) * wa1(i_)
                Next
                temp = Math.Sqrt(v)
                parc = fp / delta / temp / temp

                '
                ' depending on the sign of the function, update parl or paru.
                '
                If fp > 0 Then
                    parl = Math.Max(parl, par)
                End If
                If fp < 0 Then
                    paru = Math.Min(paru, par)
                End If

                '
                ' compute an improved estimate for par.
                '
                par = Math.Max(parl, par + parc)

                '
                ' end of an iteration.
                '
            End While

            '
            ' termination.
            '
            If iter = 0 Then
                par = 0
            End If
        End Sub


        Private Shared Sub levenbergmarquardtnewiteration(ByRef x As Double())
        End Sub


        Private Shared Function additionallevenbergmarquardtstoppingcriterion(ByVal iter As Integer) As Boolean
            Dim result As New Boolean()

            result = False
            Return result
        End Function
    End Class


End Namespace
