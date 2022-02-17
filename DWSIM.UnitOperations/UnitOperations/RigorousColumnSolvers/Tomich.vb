'    Rigorous Columns (Distillation and Absorption) Solvers
'    Copyright 2008-2022 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Namespace UnitOperations.Auxiliary.SepOps.SolvingMethods

    <System.Serializable()> Public Class Tomich

        Public Shared Function TDMASolve(ByVal a As Double(), ByVal b As Double(), ByVal c As Double(), ByVal d As Double()) As Double()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "TDMASolve", "Tridiagonal Matrix Algorithm", "Tomich TDM Solver", True)

            IObj?.Paragraphs.Add("The key to the BP and SR tearing procedures is the tridiagonal matrix, which results from a modified form of the M equations, when they are torn from the other equations by selecting Tj and Vj as the tear variables, leaving the modified M equations linear in the unknown liquid mole fractions.")

            IObj?.Paragraphs.Add("This set of equations, one for each component, is solved by a modified Gaussian–elimination algorithm due to Thomas as applied by Wang and Henke. Equations for calculating y and L are partitioned from the other equations. The result for each component, i, and each stage, j, is as follows, where the i subscripts have been dropped from the B, C, and D terms.")

            IObj?.Paragraphs.Add("<math>A_jx_{i,j-1}+B_jx_{i,j}+C_jx_{i,j+1}=D_j</math>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<math>A_j = V_j+\sum\limits_{m=1}^{j-1} (F_m-W_m-U_m)-V_1</math>")

            IObj?.Paragraphs.Add("<math>B_j=-[V_{j+1}+\sum\limits_{m+1}^j(F_m-W_m-U_m)-V_1+U_j+(V_j+W_j)K_{i,j}], 1\leq j\leq N</math>")

            IObj?.Paragraphs.Add("<math>C_j=V_{j+1}K_{i,j+1}, 1 \leq j \leq N-1</math>")

            IObj?.Paragraphs.Add("<math>D_j=-F_jz_{i,j}, 1 \leq j \leq N</math>")

            IObj?.Paragraphs.Add("with <mi>x_{i,0}=0</mi>; <mi>V_{N+1}=0</mi>; <mi>W_1 = 0</mi>, and <mi>U_N=0</mi>, as indicated in Figure 10.3. If the modified M equations are grouped by component, they can be partitioned by writing them as a series of separate tridiagonal-matrix equations, one for each component, where the output variable for each matrix equation is xi over the entire N-stage cascade.")

            IObj?.Paragraphs.Add("Constants Bj and Cj for each component depend only on tear variables T and V if K-values are composition-independent. If not, previous iteration compositions may be used to estimate K-values.")

            IObj?.Paragraphs.Add("The Thomas algorithm for solving the linearized equation set is a Gaussian–elimination procedure involving forward elimination starting from stage 1 and working toward stage N to finally isolate <mi>x_{i,N}</mi>. Other values of <mi>x_{i,j}</mi> are then obtained, starting with <mi>x_{i,N-1}</mi> by backward substitution.")

            IObj?.Paragraphs.Add("The Thomas algorithm avoids buildup of computer truncation errors because none of the steps involves subtraction of nearly equal quantities. Furthermore, computed values of xi,j are almost always positive. The algorithmis superior to alternative matrix-inversion routines. A modified Thomas algorithm for difficult cases is given by Boston and Sullivan [9]. Such cases can occur for columns with large numbers of equilibrium stages and components whose absorption factors, <mi>A=L/KV</mi>, are less than unity in one section and greater than unity in another.")

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")

            IObj?.Paragraphs.Add(String.Format("A: {0}", a.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("B: {0}", b.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("C: {0}", c.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("D: {0}", d.ToMathArrayString))

            ' Warning: will modify c and d!

            Dim n As Integer = d.Length
            ' All arrays should be the same length
            Dim x As Double() = New Double(n - 1) {}
            Dim id As Double

            ' Modify the coefficients.

            c(0) /= b(0)
            ' Division by zero risk.
            d(0) /= b(0)
            ' Division by zero would imply a singular matrix.
            For i As Integer = 1 To n - 1
                id = b(i) - c(i - 1) * a(i)
                c(i) /= id
                ' This calculation during the last iteration is redundant.
                d(i) = (d(i) - d(i - 1) * a(i)) / id
            Next

            ' Now back substitute.

            x(n - 1) = d(n - 1)
            For i As Integer = n - 2 To 0 Step -1
                x(i) = d(i) - c(i) * x(i + 1)
            Next

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("x: {0}", x.ToMathArrayString))

            IObj?.Close()

            Return x

        End Function

    End Class

End Namespace