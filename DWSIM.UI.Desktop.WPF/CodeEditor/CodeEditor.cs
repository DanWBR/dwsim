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

namespace DWSIM.UI.Desktop.WPF
{

    public class CodeEditorControlHandler : Eto.Wpf.Forms.WpfFrameworkElement<FrameworkElement, CodeEditorControl, CodeEditorControl.ICallback>, CodeEditorControl.ICodeEditor
    {

        ICSharpCode.AvalonEdit.TextEditor te;

        public CodeEditorControlHandler()
        {

            te = new ICSharpCode.AvalonEdit.TextEditor();

            te.FontFamily = new System.Windows.Media.FontFamily("Consolas");
            te.ShowLineNumbers = true;
            te.SyntaxHighlighting = ICSharpCode.AvalonEdit.Highlighting.HighlightingManager.Instance.GetDefinition("C#");

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
                return te.Text;
            }

            set
            {
                te.Text = value;
            }
        }
    }


}
