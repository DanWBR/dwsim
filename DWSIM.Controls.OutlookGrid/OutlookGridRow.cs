// Copyright 2006 Herre Kuijpers - <herre@xs4all.nl>
//
// This source file(s) may be redistributed, altered and customized
// by any means PROVIDING the authors name and all copyright
// notices remain intact.
// THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED. USE IT AT YOUR OWN RISK. THE AUTHOR ACCEPTS NO
// LIABILITY FOR ANY DATA DAMAGE/LOSS THAT THIS PRODUCT MAY CAUSE.
//-----------------------------------------------------------------------
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;


namespace OutlookStyleControls
{

    #region OutlookGridRow - subclasses the DataGridView's DataGridViewRow class
    /// <summary>
    /// In order to support grouping with the same look & feel as Outlook, the behaviour
    /// of the DataGridViewRow is overridden by the OutlookGridRow.
    /// The OutlookGridRow has 2 main additional properties: the Group it belongs to and
    /// a the IsRowGroup flag that indicates whether the OutlookGridRow object behaves like
    /// a regular row (with data) or should behave like a Group row.
    /// 
    /// </summary>
    public class OutlookGridRow : DataGridViewRow
    {
        private bool isGroupRow;
        private IOutlookGridGroup group;

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public IOutlookGridGroup Group
        {
            get { return group; }
            set { group = value; }
        }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public bool IsGroupRow
        {
            get { return isGroupRow; }
            set { isGroupRow = value; }
        }

        public OutlookGridRow() : this(null, false)
        {
        }

        public OutlookGridRow(IOutlookGridGroup group)
            : this(group, false)
        {
        }

        public OutlookGridRow(IOutlookGridGroup group, bool isGroupRow) : base()
        {
            this.group = group;
            this.isGroupRow = isGroupRow;
        }

        public override DataGridViewElementStates GetState(int rowIndex)
        {
            if (!IsGroupRow && group != null && group.Collapsed)
            {
                return base.GetState(rowIndex) & DataGridViewElementStates.Selected;   
            }

            return base.GetState(rowIndex);
        }

        /// <summary>
        /// the main difference with a Group row and a regular row is the way it is painted on the control.
        /// the Paint method is therefore overridden and specifies how the Group row is painted.
        /// Note: this method is not implemented optimally. It is merely used for demonstration purposes
        /// </summary>
        /// <param name="graphics"></param>
        /// <param name="clipBounds"></param>
        /// <param name="rowBounds"></param>
        /// <param name="rowIndex"></param>
        /// <param name="rowState"></param>
        /// <param name="isFirstDisplayedRow"></param>
        /// <param name="isLastVisibleRow"></param>
        protected override void Paint(System.Drawing.Graphics graphics, System.Drawing.Rectangle clipBounds, System.Drawing.Rectangle rowBounds, int rowIndex, DataGridViewElementStates rowState, bool isFirstDisplayedRow, bool isLastVisibleRow)
        {
            if (this.isGroupRow)
            {
                var dpi = graphics.DpiX/96.0;

                OutlookGrid grid = (OutlookGrid)this.DataGridView;
                int rowHeadersWidth = grid.RowHeadersVisible ? grid.RowHeadersWidth : 0;

                // this can be optimized
                Brush brush = new SolidBrush(grid.DefaultCellStyle.BackColor);
                Brush brush2 = new SolidBrush(Color.FromKnownColor(KnownColor.GradientActiveCaption));

                int gridwidth = grid.Columns.GetColumnsWidth(DataGridViewElementStates.Displayed);
                Rectangle rowBounds2 = grid.GetRowDisplayRectangle(this.Index, true);

                // draw the background
                graphics.FillRectangle(brush, rowBounds.Left + rowHeadersWidth - grid.HorizontalScrollingOffset, rowBounds.Top, gridwidth, rowBounds.Height - 1);
                
                // draw text, using the current grid font
                graphics.DrawString(group.Text, grid.Font, Brushes.Black, rowHeadersWidth - grid.HorizontalScrollingOffset + (int)(23.0*dpi), rowBounds.Bottom - (int)(18.0*dpi));
                
                //draw bottom line
                graphics.FillRectangle(brush2, rowBounds.Left + rowHeadersWidth - grid.HorizontalScrollingOffset, rowBounds.Bottom - (int)(2.0*dpi), gridwidth - 1, (int)(2.0*dpi));
                
                // draw right vertical bar
                if (grid.CellBorderStyle == DataGridViewCellBorderStyle.SingleVertical || grid.CellBorderStyle == DataGridViewCellBorderStyle.Single)
                    graphics.FillRectangle(brush2, rowBounds.Left + rowHeadersWidth - grid.HorizontalScrollingOffset + gridwidth - 1, rowBounds.Top, 1, rowBounds.Height);

                if (group.Collapsed)
                {
                    if (grid.ExpandIcon != null)
                        graphics.DrawImage(grid.ExpandIcon, rowBounds.Left + rowHeadersWidth - grid.HorizontalScrollingOffset + (int)(4.0*dpi), rowBounds.Bottom - (int)(18.0*dpi), (int)(11.0*dpi), (int)(11.0*dpi));
                }
                else
                {
                    if (grid.CollapseIcon != null)
                        graphics.DrawImage(grid.CollapseIcon, rowBounds.Left + rowHeadersWidth - grid.HorizontalScrollingOffset + (int)(4.0*dpi), rowBounds.Bottom - (int)(18.0*dpi), (int)(11.0*dpi), (int)(11.0*dpi));
                }
                brush.Dispose();
                brush2.Dispose();
                brush2.Dispose();
            }
            base.Paint(graphics, clipBounds, rowBounds, rowIndex, rowState, isFirstDisplayedRow, isLastVisibleRow);

        }

        protected override void PaintCells(System.Drawing.Graphics graphics, System.Drawing.Rectangle clipBounds, System.Drawing.Rectangle rowBounds, int rowIndex, DataGridViewElementStates rowState, bool isFirstDisplayedRow, bool isLastVisibleRow, DataGridViewPaintParts paintParts)
        {
            if (!this.isGroupRow)
                base.PaintCells(graphics, clipBounds, rowBounds, rowIndex, rowState, isFirstDisplayedRow, isLastVisibleRow, paintParts);
        }


        /// <summary>
        /// this function checks if the user hit the expand (+) or collapse (-) icon.
        /// if it was hit it will return true
        /// </summary>
        /// <param name="e">mouse click event arguments</param>
        /// <returns>returns true if the icon was hit, false otherwise</returns>
        internal bool IsIconHit(DataGridViewCellMouseEventArgs e)
        {
            if (e.ColumnIndex < 0) return false;

            OutlookGrid grid = (OutlookGrid)this.DataGridView;
            Rectangle rowBounds = grid.GetRowDisplayRectangle(this.Index, false);
            int x = e.X;

            DataGridViewColumn c = grid.Columns[e.ColumnIndex];
            if (this.isGroupRow &&
                (c.DisplayIndex == 0) &&
                (x > rowBounds.Left + 4) &&
                (x < rowBounds.Left + 16) &&
                (e.Y > rowBounds.Height - 18) &&
                (e.Y < rowBounds.Height - 7))
                return true;

            return false;


            //System.Diagnostics.Debug.WriteLine(e.ColumnIndex);
        }
    }
    #endregion OutlookGridRow - subclasses the DataGridView's DataGridViewRow class

    #region OutlookGridRowComparer implementation
    /// <summary>
    /// the OutlookGridRowComparer object is used to sort unbound data in the OutlookGrid.
    /// currently the comparison is only done for string values. 
    /// therefore dates or numbers may not be sorted correctly.
    /// Note: this class is not implemented optimally. It is merely used for demonstration purposes
    /// </summary>
    internal class OutlookGridRowComparer : IComparer
    {
        ListSortDirection direction;
        int columnIndex;

        public OutlookGridRowComparer(int columnIndex, ListSortDirection direction)
        {
            this.columnIndex = columnIndex;
            this.direction = direction;
        }

        #region IComparer Members

        public int Compare(object x, object y)
        {
            OutlookGridRow obj1 = (OutlookGridRow)x;
            OutlookGridRow obj2 = (OutlookGridRow)y;
            return string.Compare(obj1.Cells[this.columnIndex].Value.ToString(), obj2.Cells[this.columnIndex].Value.ToString()) * (direction == ListSortDirection.Ascending ? 1 : -1);
        }
        #endregion
    }
    #endregion OutlookGridRowComparer implementation

}
