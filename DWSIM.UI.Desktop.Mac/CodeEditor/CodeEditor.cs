using DWSIM.Drawing.SkiaSharp;
using MonoMac.AppKit;
using MonoMac.CoreGraphics;
using SkiaSharp;
using System;
using DWSIM.UI.Controls;

namespace DWSIM.UI.Desktop.Mac
{
    public class CodeEditorControlHandler : Eto.Mac.Forms.MacView<NSView, CodeEditorControl, CodeEditorControl.ICallback>, CodeEditorControl.ICodeEditor
    {

        DWSIM.UI.Controls.Mac.TextEditor te;

        public CodeEditorControlHandler()
        {
            te = new Controls.Mac.TextEditor();
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

        public override NSView ContainerControl
        {
            get
            {
                return Control;
            }
        }

        public override bool Enabled { get; set; }

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
