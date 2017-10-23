Imports Eto.Forms
Imports DWSIM.ExtensionMethods
Imports DWSIM.UI.Shared
Imports DWSIM.Interfaces

Public Class EtoForm

    'flowsheet reference
    Public fsheet As DWSIM.FlowsheetBase.FlowsheetBase

    Sub New(fs As IFlowsheet)
        fsheet = CType(fs, DWSIM.FlowsheetBase.FlowsheetBase)
    End Sub

    Function GetForm() As Eto.Forms.Form

        Dim container = DWSIM.UI.Shared.Common.GetDefaultContainer()
        Dim f = DWSIM.UI.Shared.Common.GetDefaultEditorForm("Natural Gas Properties Plugin", 450, 700, container)

        Dim p As New Populate()
        p.Populate(fsheet, f)

        Return f

    End Function

End Class
