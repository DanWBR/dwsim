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

        NSSegmentedControl seg1, seg2, seg3, seg4, seg5, seg6;

        public override NSTouchBarItem MakeItem(NSTouchBar touchBar, string identifier)
        {
            NSCustomTouchBarItem item = new NSCustomTouchBarItem(identifier);
            switch (int.Parse(identifier))
            {
                case 0:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-save.png").ToNS(20);
                        //var img2 = Bitmap.FromResource(imgprefix + "icons8-save_as.png").ToNS(20);
                        seg1 = NSSegmentedControl.FromImages(new NSImage[] { img1 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction1(); });
                        seg1.SegmentDistribution = NSSegmentDistribution.Fill;
                        seg1.SetWidth(26, 0);
                        //seg1.SetWidth(26, 1);
                        seg1.SetTag(0, 0);
                        //seg1.SetTag(1, 1);
                        item.View = seg1;
                        return item;
                    }
                case 1:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-thin_test_tube.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "icons8-math.png").ToNS(20);
                        var img3 = Bitmap.FromResource(imgprefix + "icons8-sorting_options.png").ToNS(20);
                        seg2 = NSSegmentedControl.FromImages(new NSImage[] { img1, img2, img3 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction2(); });
                        seg2.SegmentDistribution = NSSegmentDistribution.Fill;
                        seg2.SetWidth(26, 0);
                        seg2.SetWidth(26, 1);
                        seg2.SetWidth(26, 2);
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
                        seg3.SegmentDistribution = NSSegmentDistribution.Fill;
                        seg3.SetWidth(26, 0);
                        seg3.SetWidth(26, 1);
                        seg3.SetTag(5, 0);
                        seg3.SetTag(6, 1);
                        item.View = seg3;
                        return item;
                    }
                case 3:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-zoom_out_filled.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "icons8-zoom_in_filled.png").ToNS(20);
                        var img3 = Bitmap.FromResource(imgprefix + "icons8-zoom_to_extents.png").ToNS(20);
                        var img4 = Bitmap.FromResource(imgprefix + "icons8-zoom_to_actual_size_filled.png").ToNS(20);
                        seg4 = NSSegmentedControl.FromImages(new NSImage[] { img1, img2, img3, img4 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction4(); });
                        seg4.SegmentDistribution = NSSegmentDistribution.Fill;
                        seg4.SetWidth(26, 0);
                        seg4.SetWidth(26, 1);
                        seg4.SetWidth(26, 2);
                        seg4.SetWidth(26, 3);
                        seg4.SetTag(7, 0);
                        seg4.SetTag(8, 1);
                        seg4.SetTag(9, 2);
                        seg4.SetTag(10, 3);
                        item.View = seg4;
                        return item;
                    }
                case 4:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "icons8-grid.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "icons8-grid_filled.png").ToNS(20);
                        seg5 = NSSegmentedControl.FromImages(new NSImage[] { img1, img2 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction5(); });
                        seg5.SegmentDistribution = NSSegmentDistribution.Fill;
                        seg5.SetWidth(26, 0);
                        seg5.SetWidth(26, 1);
                        seg5.SetTag(11, 0);
                        seg5.SetTag(12, 1);
                        item.View = seg5;
                        return item;
                    }
                case 5:
                    {
                        var img1 = Bitmap.FromResource(imgprefix + "shape_group.png").ToNS(20);
                        var img2 = Bitmap.FromResource(imgprefix + "shape_align_left.png").ToNS(20);
                        var img3 = Bitmap.FromResource(imgprefix + "shape_align_center.png").ToNS(20);
                        var img4 = Bitmap.FromResource(imgprefix + "shape_align_right.png").ToNS(20);
                        var img5 = Bitmap.FromResource(imgprefix + "shape_align_top.png").ToNS(20);
                        var img6 = Bitmap.FromResource(imgprefix + "shape_align_middle.png").ToNS(20);
                        var img7 = Bitmap.FromResource(imgprefix + "shape_align_bottom.png").ToNS(20);
                        var img8 = Bitmap.FromResource(imgprefix + "shape_align_center1.png").ToNS(20);
                        var img9 = Bitmap.FromResource(imgprefix + "shape_align_middle1.png").ToNS(20);
                        seg6 = NSSegmentedControl.FromImages(new NSImage[] { img1, img2, img3, img4,img5,img6,img7,img8,img9 }, NSSegmentSwitchTracking.Momentary, () => { SegmentAction6(); });
                        seg6.SegmentDistribution = NSSegmentDistribution.Fill;
                        seg6.SetWidth(26, 0);
                        seg6.SetWidth(26, 1);
                        seg6.SetWidth(26, 2);
                        seg6.SetWidth(26, 3);
                        seg6.SetWidth(26, 4);
                        seg6.SetWidth(26, 5);
                        seg6.SetWidth(26, 6);
                        seg6.SetWidth(26, 7);
                        seg6.SetWidth(26, 8);
                        seg6.SetTag(13, 0);
                        seg6.SetTag(14, 1);
                        seg6.SetTag(15, 2);
                        seg6.SetTag(16, 3);
                        seg6.SetTag(17, 4);
                        seg6.SetTag(18, 5);
                        seg6.SetTag(19, 6);
                        seg6.SetTag(20, 7);
                        seg6.SetTag(21, 8);
                        item.View = seg6;
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
                case 10:
                    Flowsheet.ActZoomDefault.Invoke();
                    break;
                default:
                    break;
            }
        }

        private void SegmentAction5()
        {
            switch (seg5.Cell.GetTag(seg5.SelectedSegment))
            {
                case 11:
                    Flowsheet.ActDrawGrid.Invoke();
                    break;
                case 12:
                    Flowsheet.ActSnapToGrid.Invoke();
                    break;
                default:
                    break;
            }
        }

        private void SegmentAction6()
        {
            switch (seg6.Cell.GetTag(seg6.SelectedSegment))
            {
                case 13:
                    Flowsheet.ActMultiSelect.Invoke();
                    break;
                case 14:
                    Flowsheet.ActAlignLefts.Invoke();
                    break;
                case 15:
                    Flowsheet.ActAlignCenters.Invoke();
                    break;
                case 16:
                    Flowsheet.ActAlignRights.Invoke();
                    break;
                case 17:
                    Flowsheet.ActAlignTops.Invoke();
                    break;
                case 18:
                    Flowsheet.ActAlignMiddles.Invoke();
                    break;
                case 19:
                    Flowsheet.ActAlignBottoms.Invoke();
                    break;
                case 20:
                    Flowsheet.ActVertAlign.Invoke();
                    break;
                case 21:
                    Flowsheet.ActHorizAlign.Invoke();
                    break;
                default:
                    break;
            }
        }


    }
}



