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
        NSScrollView sv;

        public CodeEditorControlHandler()
        {
            te = new Controls.Mac.TextEditor();
            te.Font = NSFont.FromFontName("Menlo", 11.0f);
            te.Editable = true;
            te.Selectable = true;
            te.AutoresizingMask = NSViewResizingMask.WidthSizable;
            te.MaxSize = new CGSize(1000, 10000000);
            sv = new NSScrollView {AutoresizesSubviews = true, BorderType = NSBorderType.NoBorder, HasVerticalScroller = true, HasHorizontalScroller = true, AutoresizingMask = NSViewResizingMask.WidthSizable };
            var cv = new NSClipView { AutoresizesSubviews = true};
            cv.DocumentView = te;
            sv.ContentView = cv;
            this.Control = sv;
            te.BecomeFirstResponder();
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
                te.Text = "";
                te.Text = value;
            }
        }
    }

}
