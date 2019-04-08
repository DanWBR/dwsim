using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SkiaSharp;
using SkiaSharp.Views.Desktop;
using System.Windows.Media;
using System.Windows;
using DWSIM.Drawing.SkiaSharp;
using DWSIM.Interfaces;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using System.Windows.Input;
using DWSIM.UI.Controls;
using Eto.Drawing;
using ScintillaNET;

namespace DWSIM.UI.Desktop.WPF
{

    public class CodeEditorControlHandler : Eto.Wpf.Forms.WpfFrameworkElement<FrameworkElement, CodeEditorControl, CodeEditorControl.ICallback>, CodeEditorControl.ICodeEditor
    {

        CodeEditor_WPF te;

        public CodeEditorControlHandler()
        {
            te = new CodeEditor_WPF();
            this.Control = te;
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

        public string Text
        {
            get
            {
                return te.WinFormsControl.Text;
            }

            set
            {
                te.WinFormsControl.Text = value;
            }
        }
    }

    public class CodeEditor_WPF : System.Windows.Controls.Grid
    {
       
        public Scintilla WinFormsControl;

        public CodeEditor_WPF()
        {
            
            WinFormsControl = new Scintilla();
            WinFormsControl.AnnotationVisible = ScintillaNET.Annotation.Standard;
            WinFormsControl.AutoCChooseSingle = true;
            WinFormsControl.AutoCMaxHeight = 10;
            WinFormsControl.AutoCOrder = ScintillaNET.Order.PerformSort;
            WinFormsControl.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            WinFormsControl.Lexer = ScintillaNET.Lexer.Python;
            WinFormsControl.UseTabs = false;
            SetEditorStyle(WinFormsControl);

            this.Loaded += Window_Loaded;
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {

            // Create the interop host control.
            System.Windows.Forms.Integration.WindowsFormsHost host =
                new System.Windows.Forms.Integration.WindowsFormsHost();

            // Assign the winforms control as the host control's child.
            host.Child = WinFormsControl;
            
            // Add the interop host control to the Grid
            // control's collection of child controls.
            this.Children.Add(host);

        }

        protected override void OnRender(DrawingContext drawingContext)
        {
            WinFormsControl.Invalidate();
        }

        public static void SetEditorStyle(ScintillaNET.Scintilla scintilla)
        {
            scintilla.StyleResetDefault();
            scintilla.Styles[ScintillaNET.Style.Default].Font = "Consolas";
            scintilla.Styles[ScintillaNET.Style.Default].Size = 10;
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
                scintilla.Markers[i].SetForeColor(System.Drawing.SystemColors.ControlLightLight);
                scintilla.Markers[i].SetBackColor(System.Drawing.SystemColors.ControlDark);
            }

            // Style the folder markers

            scintilla.Markers[Marker.Folder].Symbol = MarkerSymbol.BoxPlus;
            scintilla.Markers[Marker.Folder].SetBackColor(System.Drawing.SystemColors.ControlText);
            scintilla.Markers[Marker.FolderOpen].Symbol = MarkerSymbol.BoxMinus;
            scintilla.Markers[Marker.FolderEnd].Symbol = MarkerSymbol.BoxPlusConnected;
            scintilla.Markers[Marker.FolderEnd].SetBackColor(System.Drawing.SystemColors.ControlText);
            scintilla.Markers[Marker.FolderMidTail].Symbol = MarkerSymbol.TCorner;
            scintilla.Markers[Marker.FolderOpenMid].Symbol = MarkerSymbol.BoxMinusConnected;
            scintilla.Markers[Marker.FolderSub].Symbol = MarkerSymbol.VLine;
            scintilla.Markers[Marker.FolderTail].Symbol = MarkerSymbol.LCorner;

            // Enable automatic folding

            scintilla.AutomaticFold = (AutomaticFold.Show | AutomaticFold.Click | AutomaticFold.Change);

            // Set the styles

            scintilla.Styles[ScintillaNET.Style.Python.Default].ForeColor = System.Drawing.Color.FromArgb(0x80, 0x80, 0x80);
            scintilla.Styles[ScintillaNET.Style.Python.CommentLine].ForeColor = System.Drawing.Color.FromArgb(0x0, 0x7f, 0x0);
            scintilla.Styles[ScintillaNET.Style.Python.CommentLine].Italic = true;
            scintilla.Styles[ScintillaNET.Style.Python.Number].ForeColor = System.Drawing.Color.FromArgb(0x0, 0x7f, 0x7f);
            scintilla.Styles[ScintillaNET.Style.Python.String].ForeColor = System.Drawing.Color.FromArgb(0x7f, 0x0, 0x7f);
            scintilla.Styles[ScintillaNET.Style.Python.Character].ForeColor = System.Drawing.Color.FromArgb(0x7f, 0x0, 0x7f);
            scintilla.Styles[ScintillaNET.Style.Python.Word].ForeColor = System.Drawing.Color.FromArgb(0x0, 0x0, 0x7f);
            scintilla.Styles[ScintillaNET.Style.Python.Word].Bold = true;
            scintilla.Styles[ScintillaNET.Style.Python.Triple].ForeColor = System.Drawing.Color.FromArgb(0x7f, 0x0, 0x0);
            scintilla.Styles[ScintillaNET.Style.Python.TripleDouble].ForeColor = System.Drawing.Color.FromArgb(0x7f, 0x0, 0x0);
            scintilla.Styles[ScintillaNET.Style.Python.ClassName].ForeColor = System.Drawing.Color.FromArgb(0x0, 0x0, 0xff);
            scintilla.Styles[ScintillaNET.Style.Python.ClassName].Bold = true;
            scintilla.Styles[ScintillaNET.Style.Python.DefName].ForeColor = System.Drawing.Color.FromArgb(0x0, 0x7f, 0x7f);
            scintilla.Styles[ScintillaNET.Style.Python.DefName].Bold = true;
            scintilla.Styles[ScintillaNET.Style.Python.Operator].Bold = true;
            scintilla.Styles[ScintillaNET.Style.Python.CommentBlock].ForeColor = System.Drawing.Color.FromArgb(0x7f, 0x7f, 0x7f);
            scintilla.Styles[ScintillaNET.Style.Python.CommentBlock].Italic = true;
            scintilla.Styles[ScintillaNET.Style.Python.StringEol].ForeColor = System.Drawing.Color.FromArgb(0x0, 0x0, 0x0);
            scintilla.Styles[ScintillaNET.Style.Python.StringEol].BackColor = System.Drawing.Color.FromArgb(0xe0, 0xc0, 0xe0);
            scintilla.Styles[ScintillaNET.Style.Python.StringEol].FillLine = true;

            scintilla.Styles[ScintillaNET.Style.Python.DefName].ForeColor = System.Drawing.Color.Brown;
            scintilla.Styles[ScintillaNET.Style.Python.DefName].Bold = true;

            scintilla.Styles[ScintillaNET.Style.Python.Word2].ForeColor = System.Drawing.Color.DarkRed;
            scintilla.Styles[ScintillaNET.Style.Python.Word2].Bold = true;

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
