using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;
using System.Xml;
using System.Xml.Linq;

namespace DWSIM.UI.Forms.Forms
{
    partial class Flowsheet : Form
    {

        public Desktop.Shared.Flowsheet FlowsheetObject;

        void InitializeComponent()
        {

            FlowsheetObject = new Desktop.Shared.Flowsheet() { FlowsheetForm = this };

            Title = "Flowsheet";

            var fsc = new DWSIM.UI.Controls.FlowsheetSurfaceControl();

            fsc.FlowsheetSurface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)FlowsheetObject.GetSurface();

            fsc.FlowsheetSurface.BackgroundColor = SkiaSharp.SKColors.White;

            FlowsheetObject.LoadFromXML(XDocument.Load("C:\\Users\\ptc0\\Desktop\\cavett.dwxml"));

            Content = fsc;

            ClientSize = new Size(1024, 768);

        }
    }
}
