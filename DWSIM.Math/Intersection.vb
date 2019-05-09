Imports DWSIM.DrawingTools.Point
Imports System.Linq

Namespace MathEx

    Public Class Intersection

        Public Shared Function FindIntersection(ByVal curve1 As List(Of Point), curve2 As List(Of Point), curve1fit As LMFit.FitType, curve2fit As LMFit.FitType, tolerance As Double, xmin As Double, xmax As Double, npoints As Integer) As List(Of Point)

            Dim polyfit As New DWSIM.MathOps.MathEx.LMFit()

            Dim ie1, ie2 As Double()

            Select Case curve1fit
                Case LMFit.FitType.Linear
                    ie1 = New Double() {1.0, 1.0}
                Case LMFit.FitType.SecondDegreePoly
                    ie1 = New Double() {1.0, 1.0, 1.0}
                Case LMFit.FitType.ThirdDegreePoly
                    ie1 = New Double() {1.0, 1.0, 1.0, 1.0}
                Case LMFit.FitType.FourthDegreePoly
                    ie1 = New Double() {1.0, 1.0, 1.0, 1.0, 1.0}
                Case Else
                    ie1 = New Double() {}
            End Select

            Select Case curve2fit
                Case LMFit.FitType.Linear
                    ie2 = New Double() {1.0, 1.0}
                Case LMFit.FitType.SecondDegreePoly
                    ie2 = New Double() {1.0, 1.0, 1.0}
                Case LMFit.FitType.ThirdDegreePoly
                    ie2 = New Double() {1.0, 1.0, 1.0, 1.0}
                Case LMFit.FitType.FourthDegreePoly
                    ie2 = New Double() {1.0, 1.0, 1.0, 1.0, 1.0}
                Case Else
                    ie2 = New Double() {}
            End Select

            Dim c1 = polyfit.GetCoeffs(curve1.Select(Function(p) p.X).ToArray, curve1.Select(Function(p) p.Y).ToArray, ie1, curve1fit, 0.000001, 0.000001, 0.000001, 1000)

            Dim c2 = polyfit.GetCoeffs(curve2.Select(Function(p) p.X).ToArray, curve2.Select(Function(p) p.Y).ToArray, ie2, curve2fit, 0.000001, 0.000001, 0.000001, 1000)

            Dim c1coeffs = c1.Item1

            Dim c2coeffs = c2.Item1

            Dim intersections As New List(Of Point)

            Dim c1v, c2v As Double

            For x As Double = xmin To xmax Step (xmax - xmin) / npoints

                Select Case curve1fit
                    Case LMFit.FitType.Linear
                        c1v = c1coeffs(0) + c1coeffs(1) * x
                    Case LMFit.FitType.SecondDegreePoly
                        c1v = c1coeffs(0) + c1coeffs(1) * x + c1coeffs(2) * x ^ 2
                    Case LMFit.FitType.ThirdDegreePoly
                        c1v = c1coeffs(0) + c1coeffs(1) * x + c1coeffs(2) * x ^ 2 + c1coeffs(3) * x ^ 3
                    Case LMFit.FitType.FourthDegreePoly
                        c1v = c1coeffs(0) + c1coeffs(1) * x + c1coeffs(2) * x ^ 2 + c1coeffs(3) * x ^ 3 + c1coeffs(4) * x ^ 4
                End Select

                Select Case curve2fit
                    Case LMFit.FitType.Linear
                        c2v = c2coeffs(0) + c2coeffs(1) * x
                    Case LMFit.FitType.SecondDegreePoly
                        c2v = c2coeffs(0) + c2coeffs(1) * x + c2coeffs(2) * x ^ 2
                    Case LMFit.FitType.ThirdDegreePoly
                        c2v = c2coeffs(0) + c2coeffs(1) * x + c2coeffs(2) * x ^ 2 + c2coeffs(3) * x ^ 3
                    Case LMFit.FitType.FourthDegreePoly
                        c2v = c2coeffs(0) + c2coeffs(1) * x + c2coeffs(2) * x ^ 2 + c2coeffs(3) * x ^ 3 + c2coeffs(4) * x ^ 4
                End Select

                If Math.Abs(c1v - c2v) < tolerance Then

                    intersections.Add(New Point(x, c1v))

                End If

            Next

            Return intersections

        End Function

    End Class

End Namespace


