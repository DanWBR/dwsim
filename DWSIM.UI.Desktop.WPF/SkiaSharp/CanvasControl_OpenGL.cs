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

    public class FlowsheetSurfaceControlHandler_OpenGL : Eto.Wpf.Forms.WpfFrameworkElement<System.Windows.Controls.Grid, FlowsheetSurfaceControl_OpenGL, FlowsheetSurfaceControl_OpenGL.ICallback>, FlowsheetSurfaceControl_OpenGL.IFlowsheetSurface_OpenGL
    {
        public FlowsheetSurfaceControlHandler_OpenGL()
        {
            this.Control = new FlowsheetSurface_WPF_OpenGL();
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

        public DWSIM.UI.Desktop.WinForms.FlowsheetSurface_WinForms_OpenGL surface;

        public FlowsheetSurface_WPF_OpenGL()
        {
            this.Loaded += Window_Loaded;
        }

        private void Window_Loaded(object sender, RoutedEventArgs e) 
        {

            // Create the interop host control.
            System.Windows.Forms.Integration.WindowsFormsHost host =
                new System.Windows.Forms.Integration.WindowsFormsHost();

            // Create the MaskedTextBox control.
            surface = new  WinForms.FlowsheetSurface_WinForms_OpenGL();

            surface.fbase = fbase;
            surface.fsurface = fsurface;

            // Assign the MaskedTextBox control as the host control's child.
            host.Child = surface;

            // Add the interop host control to the Grid
            // control's collection of child controls.
            this.Children.Add(host);

           
        }

        protected override void OnRender(DrawingContext drawingContext)
        {

            base.OnRender(drawingContext);

            if (surface != null) surface.Invalidate();

        }

        protected override void OnMouseDown(System.Windows.Input.MouseButtonEventArgs e)
        {
            _lastTouchX = (int)e.GetPosition(this).X;
            _lastTouchY = (int)e.GetPosition(this).Y;
            fsurface.InputPress((int)_lastTouchX, (int)_lastTouchY);
            this.InvalidateVisual();
        }

        protected override void OnMouseUp(System.Windows.Input.MouseButtonEventArgs e)
        {
            fsurface.InputRelease();
            this.InvalidateVisual();
        }

        protected override void OnMouseMove(System.Windows.Input.MouseEventArgs e)
        {
            _lastTouchX = (int)e.GetPosition(this).X;
            _lastTouchY = (int)e.GetPosition(this).Y;
            fsurface.InputMove((int)_lastTouchX, (int)_lastTouchY);
            this.InvalidateVisual();
        }

        protected override void OnMouseLeftButtonDown(System.Windows.Input.MouseButtonEventArgs e)
        {
            //if (e.ClickCount == 2) { 
            //    fsurface.ZoomAll((int)this.ActualWidth, (int)this.ActualHeight); 
            //    this.InvalidateVisual(); 
            //} 
        }

        protected override void OnMouseWheel(System.Windows.Input.MouseWheelEventArgs e)
        {

            fsurface.Zoom += e.Delta / 4 / 100.0f;
            this.InvalidateVisual();
        }

    }

}
