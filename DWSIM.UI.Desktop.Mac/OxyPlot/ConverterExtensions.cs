// --------------------------------------------------------------------------------------------------------------------
// <copyright file="ConverterExtensions.cs" company="OxyPlot">
//   Copyright (c) 2014 OxyPlot contributors
// </copyright>
// <summary>
//   Provides extension methods that converts between MonoTouch and OxyPlot types.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace DWSIM.UI.Desktop.Mac
{
    using System;

    using AppKit;
    using CoreGraphics;

    using OxyPlot;

    /// <summary>
    /// Provides extension methods that converts between MonoTouch and OxyPlot types.
    /// </summary>
    public static class ConverterExtensions
    {
        /// <summary>
        /// Converts a <see cref="System.Drawing.PointF" /> to a <see cref="ScreenPoint" />.
        /// </summary>
        /// <param name="p">The point to convert.</param>
        /// <returns>The converted point.</returns>
        public static ScreenPoint ToScreenPoint(this CGPoint p)
        {
            return new ScreenPoint(p.X, p.Y);
        }

        public static ScreenPoint PositionAsScreenPointRelativeToPlotView(this NSEvent p, PlotView plotView)
        {
            var relativePoint = plotView.ConvertPointFromView(p.LocationInWindow, null);
            return new ScreenPoint(relativePoint.X, relativePoint.Y);
        }

        /// <summary>
        /// Converts a <see cref="OxyColor" /> to a <see cref="CGColor" />.
        /// </summary>
        /// <param name="c">The color to convert.</param>
        /// <returns>The converted color.</returns>
        // ReSharper disable once InconsistentNaming
        public static CGColor ToCGColor(this OxyColor c)
        {
            return new CGColor(c.R / 255f, c.G / 255f, c.B / 255f, c.A / 255f);
        }

        /// <summary>
        /// Converts a <see cref="LineJoin" /> to a <see cref="CGLineCap" />.
        /// </summary>
        /// <param name="lineJoin">The line join.</param>
        /// <returns>The converted join.</returns>
        public static CGLineJoin Convert(this LineJoin lineJoin)
        {
            switch (lineJoin)
            {
                case LineJoin.Bevel:
                    return CGLineJoin.Bevel;
                case LineJoin.Miter:
                    return CGLineJoin.Miter;
                case LineJoin.Round:
                    return CGLineJoin.Round;
                default:
                    throw new InvalidOperationException("Invalid join type.");
            }
        }

        /// <summary>
        /// Converts a <see cref="ScreenPoint" /> to a <see cref="CGPoint" />.
        /// </summary>
        /// <param name="p">The point to convert.</param>
        /// <returns>The converted point.</returns>
        public static CGPoint Convert(this ScreenPoint p)
        {
            return new CGPoint((float)p.X, (float)p.Y);
        }

        /// <summary>
        /// Converts a <see cref="ScreenPoint" /> to a pixel center aligned <see cref="CGPoint" />.
        /// </summary>
        /// <param name="p">The point to convert.</param>
        /// <returns>The converted point.</returns>
        public static CGPoint ConvertAliased(this ScreenPoint p)
        {
            return new CGPoint(0.5f + (float)Math.Round(p.X), 0.5f + (float)Math.Round(p.Y));
        }

        /// <summary>
        /// Converts a <see cref="OxyRect" /> to a pixel center aligned <see cref="CGRect" />.
        /// </summary>
        /// <param name="rect">The rectangle to convert.</param>
        /// <returns>The converted rectangle.</returns>
        public static CGRect ConvertAliased(this OxyRect rect)
        {
            float x = 0.5f + (float)Math.Round(rect.Left);
            float y = 0.5f + (float)Math.Round(rect.Top);
            float w = 0.5f + (float)Math.Round(rect.Right) - x;
            float h = 0.5f + (float)Math.Round(rect.Bottom) - y;
            return new CGRect(x, y, w, h);
        }

        /// <summary>
        /// Converts a <see cref="OxyRect" /> to a <see cref="CGRect" />.
        /// </summary>
        /// <param name="rect">The rectangle to convert.</param>
        /// <returns>The converted rectangle.</returns>
        public static CGRect Convert(this OxyRect rect)
        {
            return new CGRect((float)rect.Left, (float)rect.Top, (float)(rect.Right - rect.Left), (float)(rect.Bottom - rect.Top));
        }

        /// <summary>
        /// Converts a <see cref="NSEventType" /> to a <see cref="OxyMouseButton" />.
        /// </summary>
        /// <param name="theType">The event type to convert.</param>
        /// <returns>The converted value.</returns>
        public static OxyMouseButton ToButton(this NSEventType theType)
        {
            switch (theType)
            {
                case NSEventType.LeftMouseDown:
                    return OxyMouseButton.Left;
                case NSEventType.RightMouseDown:
                    return OxyMouseButton.Right;
                case NSEventType.OtherMouseDown:
                    return OxyMouseButton.Middle;
                default:
                    return OxyMouseButton.None;
            }
        }

        /// <summary>
        /// Converts a <see cref="NSEventModifierMask" /> to a <see cref="OxyModifierKeys" />.
        /// </summary>
        /// <param name="theMask">The mask to convert.</param>
        /// <returns>The converted value.</returns>
        public static OxyModifierKeys ToModifierKeys(this NSEventModifierMask theMask)
        {
            var keys = OxyModifierKeys.None;
            if ((theMask & NSEventModifierMask.ShiftKeyMask) == NSEventModifierMask.ShiftKeyMask)
                keys |= OxyModifierKeys.Shift;
            if ((theMask & NSEventModifierMask.ControlKeyMask) == NSEventModifierMask.ControlKeyMask)
                keys |= OxyModifierKeys.Control;
            if ((theMask & NSEventModifierMask.AlternateKeyMask) == NSEventModifierMask.AlternateKeyMask)
                keys |= OxyModifierKeys.Alt;

            // TODO
            if ((theMask & NSEventModifierMask.CommandKeyMask) == NSEventModifierMask.CommandKeyMask)
                keys |= OxyModifierKeys.Control;

            return keys;
        }

        public static OxyMouseDownEventArgs ToMouseDownEventArgs(this NSEvent theEvent, PlotView view)
        {
            // https://developer.apple.com/library/mac/documentation/Cocoa/Reference/ApplicationKit/Classes/NSEvent_Class/Reference/Reference.html
            return new OxyMouseDownEventArgs
            {
                Position = theEvent.PositionAsScreenPointRelativeToPlotView(view),
                ChangedButton = theEvent.Type.ToButton(),
                ModifierKeys = theEvent.ModifierFlags.ToModifierKeys(),
                ClickCount = (int)theEvent.ClickCount,
            };
        }

        public static OxyMouseEventArgs ToMouseEventArgs(this NSEvent theEvent, PlotView view)
        {
            return new OxyMouseEventArgs
            {
                Position = theEvent.PositionAsScreenPointRelativeToPlotView(view),
                ModifierKeys = theEvent.ModifierFlags.ToModifierKeys(),
            };
        }

        /// <summary>
        /// Converts a <see cref="NSEvent" /> to a <see cref="OxyMouseWheelEventArgs" />.
        /// </summary>
        /// <param name="theEvent">The event to convert.</param>
        /// <param name="bounds">The bounds of the window.</param> 
        /// <returns>The converted event arguments.</returns>
        public static OxyMouseWheelEventArgs ToMouseWheelEventArgs(this NSEvent theEvent, PlotView view)
        {
            return new OxyMouseWheelEventArgs
            {
                Delta = (int)theEvent.ScrollingDeltaY,
                Position = theEvent.PositionAsScreenPointRelativeToPlotView(view),
                ModifierKeys = theEvent.ModifierFlags.ToModifierKeys(),
            };
        }

        /// <summary>
        /// Converts a <see cref="NSEvent" /> to a <see cref="OxyKeyEventArgs" />.
        /// </summary>
        /// <param name="theEvent">The event to convert.</param>
        /// <returns>The converted event arguments.</returns>
        public static OxyKeyEventArgs ToKeyEventArgs(this NSEvent theEvent)
        {
            return new OxyKeyEventArgs
            {
                Key = theEvent.KeyCode.ToKey(),
                ModifierKeys = theEvent.ModifierFlags.ToModifierKeys(),
            };
        }

        /// <summary>
        /// Converts a key code to a <see cref="OxyKey" />.
        /// </summary>
        /// <param name="keycode">The key code to convert.</param>
        /// <returns>The converted key.</returns>
        public static OxyKey ToKey(this ushort keycode)
        {
            // TODO
            return OxyKey.A;
        }
    }
}