Imports DWSIM.DWSIM.SimulationObjects

Imports Ciloci.Flee

Public Class UI_SpecControlPanelForm

    Inherits System.Windows.Forms.Form

    Public formC As FormFlowsheet
    Public status As String = ""

    Public mySPEC As SpecialOps.Spec
    
    Public su As DWSIM.SistemasDeUnidades.Unidades
    Public cv As New DWSIM.SistemasDeUnidades.Conversor
    Public nf As String

    Private Sub UI_SpecControlPanelForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        formC = My.Application.ActiveSimulation

        Me.su = formC.Options.SelectedUnitSystem
        Me.nf = formC.Options.NumberFormat

        mySPEC = formC.Collections.CLCS_SpecCollection(formC.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

        With mySPEC
            Me.tbSVID.Text = .SourceObjectData.m_Name & " (" & .SourceObjectData.m_Type & ") / " & DWSIM.App.GetPropertyName(.SourceObjectData.m_Property)
            Me.tbTVID.Text = .TargetObjectData.m_Name & " (" & .TargetObjectData.m_Type & ") / " & DWSIM.App.GetPropertyName(.TargetObjectData.m_Property)
            Me.tbSVVal.Text = Format(Double.Parse(.GetSourceVarValue), nf) & " " & .GetSourceVarUnit
            Me.tbTVVal.Text = Format(Double.Parse(.GetTargetVarValue), nf) & " " & .GetTargetVarUnit
            Me.tbExp.Text = .Expression
        End With

        Me.Text = mySPEC.GraphicObject.Tag & " - " & DWSIM.App.GetLocalString("PaineldeControle")

    End Sub

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Me.Close()
    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click

        With mySPEC

            .ExpContext = New Ciloci.Flee.ExpressionContext
            .ExpContext.Imports.AddType(GetType(System.Math))
            '// Define an int variable
            'context.Variables.DefineVariable(DWSIM.App.GetLocalString("a"), typeof(int));
            'context.Variables.SetVariableValue(DWSIM.App.GetLocalString("a"), 100);
            .ExpContext.Variables.Add("X", Double.Parse(.GetSourceVarValue))
            .ExpContext.Variables.Add("Y", Double.Parse(.GetTargetVarValue))

            '// Create a dynamic expression that evaluates to an Object
            'IDynamicExpression eDynamic = ExpressionFactory.CreateDynamic("sqrt(a) + 1", context);
            '// Create a generic expression that evaluates to a double
            'IGenericExpression<double> eGeneric = ExpressionFactory.CreateGeneric<double>("sqrt(a) + 1", context);
            Try
                .Expr = .ExpContext.CompileGeneric(Of Double)(Me.tbExp.Text)
                Me.lblExpRes.Text = DWSIM.App.GetLocalString("ExpressãoOKResultado") & Format(.Expr.Evaluate, nf)
                .Expression = Me.tbExp.Text
            Catch ex As ExpressionCompileException
                Select Case ex.Reason
                    Case CompileExceptionReason.SyntaxError
                        Me.lblExpRes.Text = DWSIM.App.GetLocalString("Erronasintaxedaexpre")
                    Case CompileExceptionReason.UndefinedName
                        Me.lblExpRes.Text = DWSIM.App.GetLocalString("ErronaexpressoVerifi")
                    Case Else
                        Me.lblExpRes.Text = ex.ToString
                End Select
            End Try

            '// Evaluate the expressions
            'double result = (double)eDynamic.Evaluate();
            'result = eGeneric.Evaluate();

            '// Update the value of our variable
            'context.Variables.SetVariableValue(DWSIM.App.GetLocalString("a"), 144);
            '// Evaluate again to get the updated result
            'result = eGeneric.Evaluate();

        End With



    End Sub

    Private Sub tbExp_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tbExp.TextChanged
        Me.lblExpRes.Text = ""
        mySPEC.Expression = Me.tbExp.Text
    End Sub

End Class