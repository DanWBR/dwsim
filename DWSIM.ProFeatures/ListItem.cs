using System;
using System.Drawing;
using System.Windows.Forms;

namespace DWSIM.ProFeatures
{

    public partial class ListItem
    {

        public Type ObjectTypeInfo { get; set; } = null;

        public ListItem()
        {
            InitializeComponent();
            lblName = _lblName;
            _lblName.Name = "lblName";
        }

        private void ListItem_GiveFeedback(object sender, GiveFeedbackEventArgs e)
        {
            e.UseDefaultCursors = false;
            var bmp = new Bitmap(Image.Width + 30, Image.Height + 30);
            Image.DrawToBitmap(bmp, new Rectangle(new Point(30, 30), bmp.Size));
            bmp.MakeTransparent(Color.White);
            var g = Graphics.FromImage(bmp);
            g.CompositingMode = System.Drawing.Drawing2D.CompositingMode.SourceOver;
            var arrow = My.Resources.Resources.cursor;
            g.DrawImage(arrow, new Point(25, 25));
            var cur = new Cursor(bmp.GetHicon());
            Cursor.Current = cur;
        }

        private void ListItem_MouseDown(object sender, MouseEventArgs e)
        {
            DoDragDrop(new object[] { ObjectTypeInfo, Tag, lblName.Text, lblName.Tag }, DragDropEffects.All);
        }

        private void ListItem_MouseUp(object sender, MouseEventArgs e)
        {
            // Me.BackColor = Color.White
        }

    }
}