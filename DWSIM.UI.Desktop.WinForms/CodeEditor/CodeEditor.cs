using DWSIM.UI.Controls;
using System.Windows.Forms;
using ScintillaNET;
using System.Drawing;
using System;
using System.Collections.Generic;

namespace DWSIM.UI.Desktop.WinForms
{

    public class CodeEditorControlHandler : Eto.WinForms.Forms.WindowsControl<Control, CodeEditorControl, CodeEditorControl.ICallback>, CodeEditorControl.ICodeEditor
    {

        ScintillaNET.Scintilla te;
        TextBox te2;
        Type t;

        public CodeEditorControlHandler()
        {

            t = Type.GetType("Mono.Runtime");
            if (t != null)
            {
                te2 = new TextBox();
                te2.Font = new System.Drawing.Font(FontFamily.GenericMonospace, 9.0f);
                te2.Multiline = true;
                te2.ScrollBars = ScrollBars.Both;
                this.Control = te2;
            }
            else {
                te = new Scintilla();
                te.AnnotationVisible = ScintillaNET.Annotation.Standard;
                te.AutoCChooseSingle = true;
                te.AutoCMaxHeight = 10;
                te.AutoCOrder = ScintillaNET.Order.PerformSort;
                te.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
                te.Lexer = ScintillaNET.Lexer.Python;
                te.UseTabs = false;
                SetEditorStyle(te);
                this.Control = te;
            }

        }

        public override Eto.Drawing.Color BackgroundColor
        {
            get
            {
                return Eto.Drawing.Colors.White;
            }

            set
            {
                return;
            }
        }

        public List<int> GetBookmarks()
        {
            List<int> bookmarks = new List<int>();
            return bookmarks;
        }

        public override string Text
        {
            get
            {
                if (t != null)
                {
                    return te2.Text;
                }
                else
                {
                    return te.Text;
                }
            }

            set
            {
                if (t != null)
                {
                    te2.Text = value;
                }
                else
                {
                    te.Text = value;
                }
            }
        }

        public static void SetEditorStyle(ScintillaNET.Scintilla scintilla)
        {
            scintilla.StyleResetDefault();
            scintilla.Styles[Style.Default].Font = "Consolas";
            scintilla.Styles[Style.Default].Size = 10;
            scintilla.StyleClearAll();

            // Set the lexer

            scintilla.Lexer = Lexer.Python;

            // Some properties we like

            scintilla.SetProperty("tab.timmy.whinge.level", "1");
            scintilla.SetProperty("fold", "1");

            scintilla.Margins[0].Width = 30;

            // Use margin 2 for fold markers

            scintilla.Margins[1].Type = MarginType.Symbol;
            scintilla.Margins[1].Mask = Marker.MaskFolders;
            scintilla.Margins[1].Sensitive = true;
            scintilla.Margins[1].Width = 20;

            // Reset folder markers

            for (int i = Marker.FolderEnd; i <= Marker.FolderOpen; i++)
            {
                scintilla.Markers[i].SetForeColor(SystemColors.ControlLightLight);
                scintilla.Markers[i].SetBackColor(SystemColors.ControlDark);
            }

            // Style the folder markers

            scintilla.Markers[Marker.Folder].Symbol = MarkerSymbol.BoxPlus;
            scintilla.Markers[Marker.Folder].SetBackColor(SystemColors.ControlText);
            scintilla.Markers[Marker.FolderOpen].Symbol = MarkerSymbol.BoxMinus;
            scintilla.Markers[Marker.FolderEnd].Symbol = MarkerSymbol.BoxPlusConnected;
            scintilla.Markers[Marker.FolderEnd].SetBackColor(SystemColors.ControlText);
            scintilla.Markers[Marker.FolderMidTail].Symbol = MarkerSymbol.TCorner;
            scintilla.Markers[Marker.FolderOpenMid].Symbol = MarkerSymbol.BoxMinusConnected;
            scintilla.Markers[Marker.FolderSub].Symbol = MarkerSymbol.VLine;
            scintilla.Markers[Marker.FolderTail].Symbol = MarkerSymbol.LCorner;

            // Enable automatic folding

            scintilla.AutomaticFold = (AutomaticFold.Show | AutomaticFold.Click | AutomaticFold.Change);

            // Set the styles

            scintilla.Styles[Style.Python.Default].ForeColor = Color.FromArgb(0x80, 0x80, 0x80);
            scintilla.Styles[Style.Python.CommentLine].ForeColor = Color.FromArgb(0x0, 0x7f, 0x0);
            scintilla.Styles[Style.Python.CommentLine].Italic = true;
            scintilla.Styles[Style.Python.Number].ForeColor = Color.FromArgb(0x0, 0x7f, 0x7f);
            scintilla.Styles[Style.Python.String].ForeColor = Color.FromArgb(0x7f, 0x0, 0x7f);
            scintilla.Styles[Style.Python.Character].ForeColor = Color.FromArgb(0x7f, 0x0, 0x7f);
            scintilla.Styles[Style.Python.Word].ForeColor = Color.FromArgb(0x0, 0x0, 0x7f);
            scintilla.Styles[Style.Python.Word].Bold = true;
            scintilla.Styles[Style.Python.Triple].ForeColor = Color.FromArgb(0x7f, 0x0, 0x0);
            scintilla.Styles[Style.Python.TripleDouble].ForeColor = Color.FromArgb(0x7f, 0x0, 0x0);
            scintilla.Styles[Style.Python.ClassName].ForeColor = Color.FromArgb(0x0, 0x0, 0xff);
            scintilla.Styles[Style.Python.ClassName].Bold = true;
            scintilla.Styles[Style.Python.DefName].ForeColor = Color.FromArgb(0x0, 0x7f, 0x7f);
            scintilla.Styles[Style.Python.DefName].Bold = true;
            scintilla.Styles[Style.Python.Operator].Bold = true;
            scintilla.Styles[Style.Python.CommentBlock].ForeColor = Color.FromArgb(0x7f, 0x7f, 0x7f);
            scintilla.Styles[Style.Python.CommentBlock].Italic = true;
            scintilla.Styles[Style.Python.StringEol].ForeColor = Color.FromArgb(0x0, 0x0, 0x0);
            scintilla.Styles[Style.Python.StringEol].BackColor = Color.FromArgb(0xe0, 0xc0, 0xe0);
            scintilla.Styles[Style.Python.StringEol].FillLine = true;

            scintilla.Styles[Style.Python.DefName].ForeColor = Color.Brown;
            scintilla.Styles[Style.Python.DefName].Bold = true;

            scintilla.Styles[Style.Python.Word2].ForeColor = Color.DarkRed;
            scintilla.Styles[Style.Python.Word2].Bold = true;

            // Keyword lists:
            // 0 "Keywords",
            // 1 "Highlighted identifiers"

            dynamic python2 = "and as assert break class continue def del elif else except exec finally for from global if import in is lambda not or pass print raise return try while with yield";
            dynamic python3 = "False None True and as assert break class continue def del elif else except finally for from global if import in is lambda nonlocal not or pass raise return try while with yield";

            //add keywords from DWSIM classes properties and methods
            
            scintilla.SetKeywords(0, python2 + " " + python3);

        }
    }


}
