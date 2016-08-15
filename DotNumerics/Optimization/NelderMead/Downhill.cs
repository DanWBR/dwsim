using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization.NelderMead
{
    internal class Downhill
    {
        #region Fields
        private double _alpha = -1.0d; 
        private double _beta = 0.5;
        private double _gamma = 2.0;
        private double _tiny = 1.0e-10;
        private int _nmax = 3000;
        private int _nfun;
        #endregion

        #region Constructor
        public Downhill()
        {
        }
        #endregion



        //public void Run(double[] start, double Step, double ftol, ref bool Stop, OptMultivariateFunction funk)
        //{
        //    int ndim = start.Length;
        //    int mpts = ndim + 1;
        //    double[,] p = new double[mpts, ndim];
        //    double[] y = new double[mpts];

        //    this.InitializeInternal(start, Step, ref p, ref y, funk);

        //    this.amoeba(start, p, y, ftol, ref Stop, funk);
        //}

        public int EvaluationCount
        {
            get { return _nfun; }
        }

        public void Run(double[] Point, double[,] p, double[] y, double ftol, ref bool Stop, OptMultivariateFunction fun, ref int nfun)
        {

            _nmax = nfun;
            nfun = 0;

            int ndim = Point.Length;
            int mpts = ndim + 1;

            //int i;
            int indexFMax;  //Index para el punto donde la fun es maxima 
            int indexFMin;  //Index para el punto donde la fun es minima
            int indexFMax2; //
            int j;
            double rtol, sum, swap, ysave, ytry;
            double[] psum = new Double[ndim];  // 

            //nfunk = 0;
            for (j = 0; j < ndim; j++)
            {
                sum = 0.0;
                for (int i = 0; i < mpts; i++) sum += p[i, j];
                psum[j] = sum;
            }

            bool conti = true;
            while (conti)
            {
                #region Maximo, segundo maximo y Minimo de F  (ihi, inhi, ilo)
                indexFMin = 0;
                if (y[0] > y[1])
                {
                    indexFMax2 = 1;
                    indexFMax = 0;
                }
                else
                {
                    indexFMax2 = 0;
                    indexFMax = 1;
                }
                for (int i = 0; i < mpts; i++)
                {
                    if (y[i] <= y[indexFMin]) indexFMin = i;     //Se encuentra el minimo de la funcion(punto ilo)
                    if (y[i] > y[indexFMax])
                    {
                        indexFMax2 = indexFMax;                //Se encuentra el segundo valor maximo de la funcion(punto inhi)         
                        indexFMax = i;                  //Se encuentra el maximo de la funcion(punto ihi)
                    }
                    else if ((y[i] > y[indexFMax2]) && (i != indexFMax)) indexFMax2 = i;
                }
                #endregion
                rtol = 2.0 * Math.Abs(y[indexFMax] - y[indexFMin]) / (Math.Abs(y[indexFMax]) + Math.Abs(y[indexFMin]) + _tiny);
                if (rtol < ftol || Stop == true || nfun >= _nmax)
                {
                    #region Mejor valor en Y[0] y P[0,i]
                    //Se pone en Y[0] y p[0,i] el mejor valor
                    swap = y[0];
                    y[0] = y[indexFMin];
                    y[indexFMin] = swap;
                    for (int i = 0; i < ndim; i++)
                    {
                        swap = p[0, i];
                        p[0, i] = p[indexFMin, i];
                        p[indexFMin, i] = swap;
                        Point[i] = p[indexFMin, i];
                    }
                    #endregion
                    //if (nfunk >= NMAX)
                    //{
                    //    nfunk = 0;
                    //    this.InitializeInternal(start, Step, ref p, ref y, funk);  //Nunca termina
                    //}
                    //else
                    //{
                    //    break;
                    //}
                    conti = false;
                    break;
                }
                //if (nfunk >= NMAX) try { throw new Exception(); }
                //    catch (Exception)
                //    {

                //        //MessageBox.Show("NMAX exceeded",
                //        //    "Invalid method", MessageBoxButtons.OK, MessageBoxIcon.Error);
                //    }
                nfun += 2;
                ytry = this.Extrapolate(p, y, psum, ndim, fun, indexFMax, _alpha);
                if (ytry <= y[indexFMin]) ytry = this.Extrapolate(p, y, psum, ndim, fun, indexFMax, _gamma);
                else if (ytry >= y[indexFMax2])
                {
                    ysave = y[indexFMax];
                    ytry = this.Extrapolate(p, y, psum, ndim, fun, indexFMax, _beta);
                    if (ytry >= ysave)
                    {
                        for (int i = 0; i < mpts; i++)
                        {
                            if (i != indexFMin)
                            {
                                for (j = 0; j < ndim; j++) p[i, j] = psum[j] = 0.5 * (p[i, j] + p[indexFMin, j]);
                                y[i] = fun(psum);
                            }
                        }
                        nfun += ndim;
                        for (j = 0; j < ndim; j++)
                        {
                            sum = 0.0;
                            for (int i = 0; i < mpts; i++) sum += p[i, j];
                            psum[j] = sum;
                        }
                    }
                }
                else --nfun;
            }
        }

        private double Extrapolate(double[,] p, double[] y, double[] psum, int ndim, OptMultivariateFunction fun, int indexFMax, double fac)
        {
            int j;
            double fac1, fac2, ytry;
            double[] ptry = new Double[ndim];

            fac1 = (1.0 - fac) / (double)ndim;
            fac2 = fac1 - fac;
            for (j = 0; j < ndim; j++) ptry[j] = psum[j] * fac1 - p[indexFMax, j] * fac2;
            ytry = fun(ptry);
            if (ytry < y[indexFMax])
            {
                y[indexFMax] = ytry;
                for (j = 0; j < ndim; j++)
                {
                    psum[j] += ptry[j] - p[indexFMax, j];
                    p[indexFMax, j] = ptry[j];
                }
            }
            return ytry;
        }

    }
}
