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
using System.Windows.Forms;

namespace DWSIM.UI.Desktop.WPF
{

    public class FlowsheetSurfaceControlHandler_OpenGL : Eto.Wpf.Forms.WpfFrameworkElement<FrameworkElement, FlowsheetSurfaceControl_OpenGL, FlowsheetSurfaceControl_OpenGL.ICallback>, FlowsheetSurfaceControl_OpenGL.IFlowsheetSurface_OpenGL
    {

        private FlowsheetSurface_WPF_OpenGL nativecontrol;

        public FlowsheetSurfaceControlHandler_OpenGL()
        {
            nativecontrol = new FlowsheetSurface_WPF_OpenGL();

            // Create the winforms control.
            nativecontrol.WinFormsControl = new WinForms.FlowsheetSurface_WinForms_OpenGL();

            this.Control = nativecontrol;

            nativecontrol.WinFormsControl.WPFMouseDown = ((e) => {
               this.Callback.OnMouseDown(Widget, e);
            });

            nativecontrol.WinFormsControl.WPFMouseUp = ((e) =>
            {
                this.Callback.OnMouseUp(Widget, e);
            });

            nativecontrol.WinFormsControl.WPFMouseDoubleClick = ((e) =>
            {
                this.Callback.OnMouseDoubleClick(Widget, e);
            });
        }

        protected override void Initialize()
        {
            base.Initialize();

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

        public GraphicsSurface FlowsheetSurface
        {
            get
            {
                return ((FlowsheetSurface_WPF_OpenGL)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_WPF_OpenGL)this.Control).fsurface = value;
            }
        }

        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_WPF_OpenGL)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_WPF_OpenGL)this.Control).fbase = value;
            }
        }

    }

    public class FlowsheetSurface_WPF_OpenGL : System.Windows.Controls.Grid
    {

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private float _lastTouchX;
        private float _lastTouchY;

        public DWSIM.UI.Desktop.WinForms.FlowsheetSurface_WinForms_OpenGL WinFormsControl;

        public FlowsheetSurface_WPF_OpenGL()
        {
            this.Loaded += Window_Loaded;

        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {

            // Create the interop host control.
            System.Windows.Forms.Integration.WindowsFormsHost host =
                new System.Windows.Forms.Integration.WindowsFormsHost();

            WinFormsControl.fbase = fbase;
            WinFormsControl.fsurface = fsurface;

            WinFormsControl.WPFHost = true;

            // Assign the winforms control as the host control's child.
            host.Child = WinFormsControl;

            // Add the interop host control to the Grid
            // control's collection of child controls.
            this.Children.Add(host);

        }

    }

}
