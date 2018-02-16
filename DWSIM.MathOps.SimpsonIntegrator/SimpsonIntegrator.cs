using System;

/// <summary>
/// Simpson's rule with the number of integration steps determined dynamically.
/// Used for integrating a smooth function over a finite interval.
/// The number of integration points is doubled at each step, while avoiding redundant function evaluations, until tolerance is met.
/// </summary>
public class SimpsonIntegrator
{
    /// <summary>
    /// Simplest interface, returning no diagnostic data
    /// </summary>
    /// <param name="f">integrand</param>
    /// <param name="a">left limit of integration</param>
    /// <param name="b">right limit of integration</param>
    /// <param name="desiredRelativeError">relative error tolerance</param>
    /// <returns>Integral value</returns>
    static public double Integrate(Func<double, double> f, double a, double b, double desiredRelativeError)
    {
        int log2MaxFunctionEvals = 20;
        int functionEvalsUsed;
        double estimatedError;

        return Integrate(f, a, b, desiredRelativeError, log2MaxFunctionEvals, out functionEvalsUsed, out estimatedError);
    }

    /// <summary>
    /// Full interface. Gives a posteriori information, namely the mumber of function evaluations and a conservative estimate of the error.
    /// </summary>
    /// <param name="f">integrand</param>
    /// <param name="a">left limit of integration</param>
    /// <param name="b">right limit of integration</param>
    /// <param name="desiredRelativeError">relative error tolerance</param>
    /// <param name="log2MaxFunctionEvals">If this value is N, the number of function evaluations will be less than 2^n + 1</param>
    /// <param name="functionEvalsUsed">Actual number of function evaluations used</param>
    /// <param name="estimatedError">Conservative error estimate</param>
    /// <returns>Integral value</returns>
    static public double Integrate(Func<double, double> f, double a, double b, double relativeErrorTolerance, int log2MaxFunctionEvals, out int functionEvalsUsed, out double estimatedError)
    {
        double integral = 0.0;
        double mostRecentContribution = 0.0;
        double previousContribution = 0.0;
        double previousIntegral = 0.0;
        double sum = 0.0;

        functionEvalsUsed = 0;
        estimatedError = double.MaxValue;

        for (int stage = 0; stage <= log2MaxFunctionEvals; stage++)
        {
            if (stage == 0)
            {
                sum = f(a) + f(b);
                functionEvalsUsed = 2;
                integral = sum * 0.5 * (b - a);
            }
            else
            {
                // Pattern of Simpson's rule coefficients:
                //
                // 1               1
                // 1       4       1
                // 1   4   2   4   1
                // 1 4 2 4 2 4 2 4 1
                // ...
                //
                // Each row multiplies new function evaluations by 4, and the evalutations from the previous step by 2.

                int numNewPts = 1 << (stage - 1);
                mostRecentContribution = 0.0;
                double h = (b - a) / numNewPts;
                double x = a + 0.5 * h;
                for (int i = 0; i < numNewPts; i++)
                    mostRecentContribution += f(x + i * h);
                functionEvalsUsed += numNewPts;
                mostRecentContribution *= 4.0;
                sum += mostRecentContribution - 0.5 * previousContribution;

                integral = sum * (b - a) / ((1 << stage) * 3.0);

                // Require at least five stages to reduce the risk of incorrectly declaring convergence too soon.
                // Note that you can specify fewer stages, but the early termination rule below will not be used.
                if (stage >= 5)
                {
                    estimatedError = Math.Abs(integral - previousIntegral); // conservative
                    if (estimatedError <= relativeErrorTolerance * Math.Abs(previousIntegral))
                        return integral;
                }
                previousContribution = mostRecentContribution;
                previousIntegral = integral;
            }
        }
        return integral;
    }
}