Imports Mapack

Namespace DWSIM.MathEx.AccelMethods

    Public Class GDEM

        Public Shared Function Promote(ByVal currX As Double(), ByVal dXvec As ArrayList, ByVal nu As Integer) As Double()

            If nu = 0 Then Return currX

            Dim n As Integer = currX.Length - 1
            Dim m As Integer = dXvec.Count - 1

            Dim newX(n) As Double

            Dim mu0 As Mapack.Matrix = New Mapack.Matrix(nu - 1, 1)
            Dim mu As Mapack.Matrix = New Mapack.Matrix(nu, 1)
            Dim W As Mapack.Matrix = New Mapack.Matrix(n + 1, n + 1)
            Dim prod As Mapack.Matrix = New Mapack.Matrix(n + 1, n + 1)
            Dim m1 As Mapack.Matrix = New Mapack.Matrix(n + 1, 1)
            Dim m2 As Mapack.Matrix = New Mapack.Matrix(1, n + 1)
            Dim b As Mapack.Matrix = New Mapack.Matrix(nu - 1, nu - 1)
            Dim b1 As Mapack.Matrix = New Mapack.Matrix(nu - 1, 1)

            Dim i, j, k As Integer
            Dim value, currvalue As Double

            i = 0
            Do
                j = 0
                Do
                    If i <> j Then
                        W(i, j) = 0
                    Else
                        value = 0.0001
                        currvalue = dXvec(m)(i, 0)
                        If value < 1 / currvalue ^ 2 Then
                            W(i, j) = value
                        Else
                            W(i, j) = 1 / currvalue ^ 2
                        End If
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            If nu > 1 Then

                j = 1
                Do
                    m1 = dXvec(m - j)
                    k = 1
                    Do
                        m2 = dXvec(m - k)
                        b(j - 1, k - 1) = (m1.Transpose.Multiply(W).Multiply(m2)).FrobeniusNorm ^ 2
                        k = k + 1
                    Loop Until k = nu
                    j = j + 1
                Loop Until j = nu

                j = 0
                Do
                    m1 = dXvec(m - j)
                    m2 = dXvec(m - 1)
                    b1(j, 0) = (m1.Transpose.Multiply(W).Multiply(m2)).FrobeniusNorm ^ 2
                    j = j + 1
                Loop Until j = nu - 1


                'Console.WriteLine(b.ToString)
                'Console.ReadKey()

                If b.Determinant = 0 Then Return currX Else mu0 = b.Solve(b1)

                i = 0
                Do
                    mu(i + 1, 0) = mu0(i, 0)
                    i = i + 1
                Loop Until i = nu - 1
                mu(0, 0) = 1

                Dim sum1 As Double = 0
                i = 0
                Do
                    sum1 += mu(i, 0)
                    i = i + 1
                Loop Until i = nu

                Dim sum2 As Double = 0, sum3 As Double = 0

                k = 0
                Do
                    sum3 = 0
                    i = 0
                    Do
                        sum2 = 0
                        j = i + 1
                        Do
                            sum2 += mu(j, 0)
                            j = j + 1
                        Loop Until j = nu
                        sum3 += dXvec(m - i)(k, 0) * sum2
                        i = i + 1
                    Loop Until i = nu - 1
                    newX(k) = currX(k) - sum3 / sum1
                    k = k + 1
                Loop Until k = n + 1

                Return newX

            Else

                m1 = dXvec(m)
                m2 = dXvec(m - 1)

                mu(0, 0) = -(m1.Transpose.Multiply(W).Multiply(m2)).FrobeniusNorm ^ 2

                m1 = dXvec(m - 1)
                m2 = dXvec(m - 1)

                mu(0, 0) = mu(0, 0) / (m1.Transpose.Multiply(W).Multiply(m2)).FrobeniusNorm ^ 2

                k = 0
                Do
                    newX(k) = currX(k) + dXvec(m)(k, 0) / (1 + mu(0, 0))
                    k = k + 1
                Loop Until k = n + 1

                Return newX

            End If

        End Function

    End Class

End Namespace
