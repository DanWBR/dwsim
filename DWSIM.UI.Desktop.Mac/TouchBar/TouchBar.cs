using System;
using AppKit;
using Eto.Drawing;
using Eto.Mac;
using Foundation;
using ObjCRuntime;

namespace DWSIM.UI.Desktop.Mac.TouchBar
{
    public class FlowsheetTouchBarDelegate : NSTouchBarDelegate
    {

        public DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;

        string imgprefix = "DWSIM.UI.Desktop.Mac.Icons.";

        NSSegmentedControl seg1, seg2, seg3, seg4;

        public override NSTouchBarItem MakeItem(NSTouchBar touchBar, string identifier)
        {
            NSCustomTouchBarItem item = new NSCustomTouchBarItem(identifier);
            switch (int.Parse(identifier))
            {
                case 0:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-save.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "icons8-save_as.png").ToNS(20);
                        seg1 = NSSegmentedControl.FromImages(new NSImage[] { img1, img2 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction1(); });
                        seg1.SetTag(0, 0);
                        seg1.SetTag(1, 1);
                        item.View = seg1;
                        return item;
                    }
                case 1:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-thin_test_tube.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "icons8-math.png").ToNS(20);
                        var img3 = Bitmap.FromResource(imgprefix + "icons8-sorting_options.png").ToNS(20);
                        seg2 = NSSegmentedControl.FromImages(new NSImage[] { img1, img2, img3 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction2(); });
                        seg2.SetTag(2, 0);
                        seg2.SetTag(3, 1);
                        seg2.SetTag(4, 2);
                        item.View = seg2;
                        return item;
                    }
                case 2:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-play.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "Checked_96px.png").ToNS(20);
                        seg3= NSSegmentedControl.FromImages(new NSImage[] { img1, img2 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction3(); });
                        seg3.SetTag(5, 0);
                        seg3.SetTag(6, 1);
                        item.View = seg3;
                        return item;
                    }
                case 3:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-zoom_out_filled.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "icons8-zoom_in_filled.png").ToNS(20);
                        var img3 = Bitmap.FromResource(imgprefix + "icons8-fit_to_page_filled.png").ToNS(20);
                        seg4 = NSSegmentedControl.FromImages(new NSImage[] { img1, img2, img3 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction4(); });
                        seg4.SetTag(7, 0);
                        seg4.SetTag(8, 1);
                        seg4.SetTag(9, 2);
                        item.View = seg4;
                        return item;
                    }
            }
            return null;
        }

        private void SegmentAction1()
        {
            switch (seg1.Cell.GetTag(seg1.SelectedSegment))
            {
                case 0:
                    Flowsheet.ActSave.Invoke();
                    break;
                case 1:
                    Flowsheet.ActSaveAs.Invoke();
                    break;
                default:
                    break;
            }


        }

        private void SegmentAction2()
        {
            switch (seg2.Cell.GetTag(seg2.SelectedSegment))
            {
                case 2:
                    Flowsheet.ActComps.Invoke();
                    break;
                case 3:
                    Flowsheet.ActBasis.Invoke();
                    break;
                case 4:
                    Flowsheet.ActOptions.Invoke();
                    break;
                default:
                    break;
            }


        }

        private void SegmentAction3()
        {
            switch (seg3.Cell.GetTag(seg3.SelectedSegment))
            {
                case 5:
                    Flowsheet.HighLevelSolve.Invoke();
                    break;
                case 6:
                    Flowsheet.ActSimultAdjustSolver.Invoke();
                    break;
                default:
                    break;
            }


        }

        private void SegmentAction4()
        {
            switch (seg4.Cell.GetTag(seg4.SelectedSegment))
            {
                case 7:
                    Flowsheet.ActZoomOut.Invoke();
                    break;
                case 8:
                    Flowsheet.ActZoomIn.Invoke();
                    break;
                case 9:
                    Flowsheet.ActZoomFit.Invoke();
                    break;
                default:
                    break;
            }


        }

    }
}



