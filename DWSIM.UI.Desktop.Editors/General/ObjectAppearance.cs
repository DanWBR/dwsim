using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.ExtensionMethods;
using Eto.Forms;
using Eto.Drawing;
using s = DWSIM.UI.Shared.Common;
using DWSIM.UI.Shared;
using System.Reflection;
using System.IO;

namespace DWSIM.UI.Desktop.Editors
{
    public class ObjectAppearanceEditorView : DynamicLayout
    {

        public IFlowsheet flowsheet;
        public Drawing.SkiaSharp.GraphicObjects.ShapeGraphic gobj;

        public ObjectAppearanceEditorView(IFlowsheet fs, Drawing.SkiaSharp.GraphicObjects.ShapeGraphic gobject)
        {
            flowsheet = fs;
            gobj = gobject;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            this.CreateAndAddLabelRow("Font");
            this.CreateAndAddDropDownRow("Font Style", gobj.FontStyle.GetEnumNames(), (int)gobj.FontStyle,
                (sender, e) => { gobj.FontStyle = sender.SelectedIndex.ToEnum<Interfaces.Enums.GraphicObjects.FontStyle>(); flowsheet.UpdateInterface(); });
            this.CreateAndAddTextBoxRow("N0", "Font Size", gobj.FontSize,
                (sender, e) => { if (sender.Text.IsValidDouble()) gobj.FontSize = (int)sender.Text.ToDoubleFromCurrent(); flowsheet.UpdateInterface(); });
            this.CreateAndAddLabelRow("Dimensions");
            this.CreateAndAddTextBoxRow("N0", "Width", gobj.Width,
                (sender, e) => { if (sender.Text.IsValidDouble()) gobj.Width = (int)sender.Text.ToDoubleFromCurrent(); });
            this.CreateAndAddTextBoxRow("N0", "Height", gobj.Height,
                (sender, e) => { if (sender.Text.IsValidDouble()) gobj.Height = (int)sender.Text.ToDoubleFromCurrent(); });
            this.CreateAndAddLabelRow("Transform");
            this.CreateAndAddNumericEditorRow2("Rotation", gobj.Rotation, 0, 360, 0,
                (sender, e) => { gobj.Rotation = (int)sender.Text.ToDoubleFromCurrent(); });
            this.CreateAndAddCheckBoxRow("Flip Horizontally", gobj.FlippedH, (sender, e) => gobj.FlippedH = sender.Checked.GetValueOrDefault());
            this.CreateAndAddCheckBoxRow("Flip Vertically", gobj.FlippedV, (sender, e) => gobj.FlippedV = sender.Checked.GetValueOrDefault());
            this.CreateAndAddLabelRow("Border/Fill");
            this.CreateAndAddNumericEditorRow2("Border Width", gobj.LineWidth, 1, 10, 0,
                (sender, e) => { gobj.LineWidth = (int)sender.Text.ToDoubleFromCurrent(); });
            this.CreateAndAddCheckBoxRow("Override Color", gobj.OverrideColors, (sender, e) => gobj.OverrideColors = sender.Checked.GetValueOrDefault());
            this.CreateAndAddColorPickerRow("Color", Color.Parse(gobj.LineColor.ToString()),
                (sender, e) => {
                    gobj.LineColor = SkiaSharp.SKColor.Parse(sender.Value.ToHex());
                });
        }
    }
}
