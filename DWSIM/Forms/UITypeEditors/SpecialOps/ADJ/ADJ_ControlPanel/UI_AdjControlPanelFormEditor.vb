Imports System.Windows.Forms.Design
Imports System.Drawing.Design
Imports System.ComponentModel

Namespace DWSIM.Editors.SpecialOps.Adjust

    <System.Serializable()> Public Class UI_AdjControlPanelFormEditor

        Inherits System.Drawing.Design.UITypeEditor

        Private editorService As IWindowsFormsEditorService

        Public Overrides Function GetEditStyle(ByVal context As System.ComponentModel.ITypeDescriptorContext) As UITypeEditorEditStyle
            Return UITypeEditorEditStyle.Modal
        End Function

        Public Overrides Function EditValue(ByVal context As System.ComponentModel.ITypeDescriptorContext, ByVal provider As IServiceProvider, ByVal value As Object) As Object

            If (provider IsNot Nothing) Then
                editorService = CType(provider.GetService(GetType(IWindowsFormsEditorService)), _
                IWindowsFormsEditorService)
            End If

            If (editorService IsNot Nothing) Then

                Dim selectionControl As New UI_AdjControlPanelForm
                Dim form As FormFlowsheet = My.Application.ActiveSimulation

                editorService.ShowDialog(selectionControl)

                value = selectionControl.status

            End If

            Return value

        End Function

    End Class

End Namespace