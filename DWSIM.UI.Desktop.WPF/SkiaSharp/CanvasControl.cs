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

namespace DWSIM.UI.Desktop.WPF
{

    public class FlowsheetSurfaceControlHandler : Eto.Wpf.Forms.WpfFrameworkElement<FrameworkElement, FlowsheetSurfaceControl, FlowsheetSurfaceControl.ICallback>, FlowsheetSurfaceControl.IFlowsheetSurface
    {
        
        private FlowsheetSurface_WPF nativecontrol;

        public FlowsheetSurfaceControlHandler()
        {
            nativecontrol = new FlowsheetSurface_WPF();
            this.Control = nativecontrol;
        }

        public override void OnLoadComplete(EventArgs e)
        {
            base.OnLoadComplete(e);
            nativecontrol.fbase = this.Widget.FlowsheetObject;
            nativecontrol.fsurface = this.Widget.FlowsheetSurface;
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
                return ((FlowsheetSurface_WPF)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_WPF)this.Control).fsurface = value;
            }
        }

        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_WPF)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_WPF)this.Control).fbase = value;
            }
        }

    }

    public class FlowsheetSurface_WPF : SkiaSharp.Views.WPF.SKElement
    {
        
        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;
        private WriteableBitmap bitmap;

        private float _lastTouchX;
        private float _lastTouchY;

        private GestureDetector _gestureDetector;

        public FlowsheetSurface_WPF()
        {
            _gestureDetector = new GestureDetector(this);
            this.AllowDrop = true;
        }

        protected override void OnRender(DrawingContext drawingContext)
        {
          
            base.OnRender(drawingContext);
            
            int width, height;
            double dpiX = 1.0;
            double dpiY = 1.0;
            if (IgnorePixelScaling)
            {
                width = (int)ActualWidth;
                height = (int)ActualHeight;
            }
            else
            {
                var m = PresentationSource.FromVisual(this).CompositionTarget.TransformToDevice;
                dpiX = m.M11;
                dpiY = m.M22;
                width = (int)(ActualWidth * dpiX);
                height = (int)(ActualHeight * dpiY);
            }

            if (width == 0) return;
            if (height == 0) return;

            var info = new SKImageInfo(width, height, SKImageInfo.PlatformColorType, SKAlphaType.Premul);

            // reset the bitmap if the size has changed
            if (bitmap == null || info.Width != bitmap.PixelWidth || info.Height != bitmap.PixelHeight)
            {
                bitmap = new WriteableBitmap(width, height, dpiX, dpiY, PixelFormats.Pbgra32, null);
            }

            // draw on the bitmap
            bitmap.Lock();
            using (var surface = SKSurface.Create(info, bitmap.BackBuffer, bitmap.BackBufferStride))
            {
                if (fsurface != null) fsurface.UpdateSurface(surface);
                OnPaintSurface(new SKPaintSurfaceEventArgs(surface, info));
                surface.Canvas.Flush();
            }

            // draw the bitmap to the screen
            bitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
            bitmap.Unlock();
            drawingContext.DrawImage(bitmap, new Rect(0, 0, ActualWidth, ActualHeight));
        }

        protected override void OnMouseDown(System.Windows.Input.MouseButtonEventArgs e)
        {
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            var m = PresentationSource.FromVisual(this).CompositionTarget.TransformToDevice;
            _lastTouchX = (int)e.GetPosition(this).X * (float)m.M11;
            _lastTouchY = (int)e.GetPosition(this).Y * (float)m.M22;
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
            var m = PresentationSource.FromVisual(this).CompositionTarget.TransformToDevice;
            _lastTouchX = (int)e.GetPosition(this).X*(float)m.M11;
            _lastTouchY = (int)e.GetPosition(this).Y* (float)m.M22;
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
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            var oldzoom = fsurface.Zoom;
            fsurface.Zoom += e.Delta / 4 / 100.0f;
            if (fsurface.Zoom < 0.05) fsurface.Zoom = 0.05f;
            var m = PresentationSource.FromVisual(this).CompositionTarget.TransformToDevice;
            var X = (int)e.GetPosition(this).X*(int)m.M11;
            var Y = (int)e.GetPosition(this).Y* (int)m.M22;
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            fsurface.CenterTo(oldzoom, X, Y, (int)Width, (int)Height);
            this.InvalidateVisual();
        }

        protected override void OnManipulationDelta(ManipulationDeltaEventArgs e)
        {
            {
                if (_gestureDetector.IsPanningAllowed)
                {
                    // Pan
                }
            }

            {
                if (_gestureDetector.IsScalingAllowed)
                {
                    // Scale
                }
            }

            {
                if (_gestureDetector.IsRotatingAllowed)
                {
                    // Rotate
                }
            }
        }

    }

    public class GestureDetector
    {
        private readonly uint _pixelPerCm = 38;
        private bool _isGestureDetected = false;

        public bool IsPanningAllowed { get; private set; }
        public bool IsScalingAllowed { get; private set; }
        public bool IsRotatingAllowed { get; private set; }

        public GestureDetector(FrameworkElement uiElement)
        {
            IsPanningAllowed = false;
            IsScalingAllowed = false;
            IsRotatingAllowed = false;

            uiElement.ManipulationStarted += (sender, args) =>
            {
                IsPanningAllowed = true;
            };

            double scale = 0.0d;
            double rot = 0.0d;

            uiElement.ManipulationDelta += (sender, args) =>
            {
                const double MIN_SCALE_TRIGGER = 0.05;
                const int MIN_ROTATIONANGLE_TRIGGER_DEGREE = 10;
                const int MIN_FINGER_DISTANCE_FOR_ROTATION_CM = 2;

                var manipulatorBounds = Rect.Empty;
                foreach (var manipulator in args.Manipulators)
                {
                    manipulatorBounds.Union(manipulator.GetPosition(sender as IInputElement));
                }

                var distance = (manipulatorBounds.TopLeft - manipulatorBounds.BottomRight).Length;
                var distanceInCm = distance / _pixelPerCm;

                scale += 1 - (args.DeltaManipulation.Scale.Length / Math.Sqrt(2));

                rot += args.DeltaManipulation.Rotation;

                if (Math.Abs(scale) > MIN_SCALE_TRIGGER && Math.Abs(rot) < MIN_ROTATIONANGLE_TRIGGER_DEGREE)
                {
                    ApplyScaleMode();
                }

                if (Math.Abs(rot) >= MIN_ROTATIONANGLE_TRIGGER_DEGREE && distanceInCm > MIN_FINGER_DISTANCE_FOR_ROTATION_CM)
                {
                    ApplyRotationMode();
                }
            };

            uiElement.ManipulationCompleted += (sender, args) =>
            {
                scale = 0.0d;
                rot = 0.0d;
                IsPanningAllowed = false;
                IsScalingAllowed = false;
                IsRotatingAllowed = false;
                _isGestureDetected = false;
            };
        }

        private void ApplyScaleMode()
        {
            if (!_isGestureDetected)
            {
                _isGestureDetected = true;
                IsPanningAllowed = true;
                IsScalingAllowed = true;
                IsRotatingAllowed = false;
            }
        }

        private void ApplyRotationMode()
        {
            if (!_isGestureDetected)
            {
                _isGestureDetected = true;
                IsPanningAllowed = true;
                IsScalingAllowed = true;
                IsRotatingAllowed = true;
            }
        }
    }

}
