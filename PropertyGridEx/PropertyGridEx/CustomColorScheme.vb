Imports System.Windows.Forms
Imports System.Drawing

Namespace PropertyGridEx
    Public Class CustomColorScheme
        Inherits ProfessionalColorTable

        Public Overrides ReadOnly Property ButtonCheckedGradientBegin() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonCheckedGradientEnd() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonCheckedGradientMiddle() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonPressedBorder() As Color
            Get
                Return Color.FromArgb(&H31, &H6A, &HC5)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonPressedGradientBegin() As Color
            Get
                Return Color.FromArgb(&H98, &HB5, &HE2)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonPressedGradientEnd() As Color
            Get
                Return Color.FromArgb(&H98, &HB5, &HE2)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonPressedGradientMiddle() As Color
            Get
                Return Color.FromArgb(&H98, &HB5, &HE2)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonSelectedBorder() As Color
            Get
                Return MyBase.ButtonSelectedBorder
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonSelectedGradientBegin() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonSelectedGradientEnd() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property ButtonSelectedGradientMiddle() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property CheckBackground() As Color
            Get
                Return Color.FromArgb(&HE1, 230, &HE8)
            End Get
        End Property

        Public Overrides ReadOnly Property CheckPressedBackground() As Color
            Get
                Return Color.FromArgb(&H31, &H6A, &HC5)
            End Get
        End Property

        Public Overrides ReadOnly Property CheckSelectedBackground() As Color
            Get
                Return Color.FromArgb(&H31, &H6A, &HC5)
            End Get
        End Property

        Public Overrides ReadOnly Property GripDark() As Color
            Get
                Return Color.FromArgb(&HC1, 190, &HB3)
            End Get
        End Property

        Public Overrides ReadOnly Property GripLight() As Color
            Get
                Return Color.FromArgb(&HFF, &HFF, &HFF)
            End Get
        End Property

        Public Overrides ReadOnly Property ImageMarginGradientBegin() As Color
            Get
                Return Color.FromArgb(251, 250, 247)
            End Get
        End Property

        Public Overrides ReadOnly Property ImageMarginGradientEnd() As Color
            Get
                Return Color.FromArgb(&HBD, &HBD, &HA3)
            End Get
        End Property

        Public Overrides ReadOnly Property ImageMarginGradientMiddle() As Color
            Get
                Return Color.FromArgb(&HEC, &HE7, &HE0)
            End Get
        End Property

        Public Overrides ReadOnly Property ImageMarginRevealedGradientBegin() As Color
            Get
                Return Color.FromArgb(&HF7, &HF6, &HEF)
            End Get
        End Property

        Public Overrides ReadOnly Property ImageMarginRevealedGradientEnd() As Color
            Get
                Return Color.FromArgb(230, &HE3, 210)
            End Get
        End Property

        Public Overrides ReadOnly Property ImageMarginRevealedGradientMiddle() As Color
            Get
                Return Color.FromArgb(&HF2, 240, &HE4)
            End Get
        End Property

        Public Overrides ReadOnly Property MenuBorder() As Color
            Get
                Return Color.FromArgb(&H8A, &H86, &H7A)
            End Get
        End Property

        Public Overrides ReadOnly Property MenuItemBorder() As Color
            Get
                Return Color.FromArgb(&H31, &H6A, &HC5)
            End Get
        End Property

        Public Overrides ReadOnly Property MenuItemPressedGradientBegin() As Color
            Get
                Return MyBase.MenuItemPressedGradientBegin
            End Get
        End Property

        Public Overrides ReadOnly Property MenuItemPressedGradientEnd() As Color
            Get
                Return MyBase.MenuItemPressedGradientEnd
            End Get
        End Property

        Public Overrides ReadOnly Property MenuItemPressedGradientMiddle() As Color
            Get
                Return MyBase.MenuItemPressedGradientMiddle
            End Get
        End Property

        Public Overrides ReadOnly Property MenuItemSelected() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property MenuItemSelectedGradientBegin() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property MenuItemSelectedGradientEnd() As Color
            Get
                Return Color.FromArgb(&HC1, 210, &HEE)
            End Get
        End Property

        Public Overrides ReadOnly Property MenuStripGradientBegin() As Color
            Get
                Return Color.FromArgb(&HE5, &HE5, &HD7)
            End Get
        End Property

        Public Overrides ReadOnly Property MenuStripGradientEnd() As Color
            Get
                Return Color.FromArgb(&HF4, &HF2, &HE8)
            End Get
        End Property

        Public Overrides ReadOnly Property OverflowButtonGradientBegin() As Color
            Get
                Return Color.FromArgb(&HF3, &HF2, 240)
            End Get
        End Property

        Public Overrides ReadOnly Property OverflowButtonGradientEnd() As Color
            Get
                Return Color.FromArgb(&H92, &H92, &H76)
            End Get
        End Property

        Public Overrides ReadOnly Property OverflowButtonGradientMiddle() As Color
            Get
                Return Color.FromArgb(&HE2, &HE1, &HDB)
            End Get
        End Property

        Public Overrides ReadOnly Property RaftingContainerGradientBegin() As Color
            Get
                Return Color.FromArgb(&HE5, &HE5, &HD7)
            End Get
        End Property

        Public Overrides ReadOnly Property RaftingContainerGradientEnd() As Color
            Get
                Return Color.FromArgb(&HF4, &HF2, &HE8)
            End Get
        End Property

        Public Overrides ReadOnly Property SeparatorDark() As Color
            Get
                Return Color.FromArgb(&HC5, &HC2, &HB8)
            End Get
        End Property

        Public Overrides ReadOnly Property SeparatorLight() As Color
            Get
                Return Color.FromArgb(&HFF, &HFF, &HFF)
            End Get
        End Property

        Public Overrides ReadOnly Property ToolStripBorder() As Color
            Get
                Return Color.FromArgb(&HA3, &HA3, &H7C)
            End Get
        End Property

        Public Overrides ReadOnly Property ToolStripDropDownBackground() As Color
            Get
                Return Color.FromArgb(&HFC, &HFC, &HF9)
            End Get
        End Property

        Public Overrides ReadOnly Property ToolStripGradientBegin() As Color
            Get
                Return Color.FromArgb(&HF7, &HF6, &HEF)
            End Get
        End Property

        Public Overrides ReadOnly Property ToolStripGradientEnd() As Color
            Get
                Return Color.FromArgb(192, 192, 168)
            End Get
        End Property

        Public Overrides ReadOnly Property ToolStripGradientMiddle() As Color
            Get
                Return Color.FromArgb(&HF2, 240, &HE4)
            End Get
        End Property

        Public Overrides ReadOnly Property ToolStripPanelGradientBegin() As System.Drawing.Color
            Get
                Return Color.FromArgb(&HE5, &HE5, &HD7)
            End Get
        End Property

        Public Overrides ReadOnly Property ToolStripPanelGradientEnd() As System.Drawing.Color
            Get
                Return Color.FromArgb(&HF4, &HF2, &HE8)
            End Get
        End Property
    End Class
End Namespace
