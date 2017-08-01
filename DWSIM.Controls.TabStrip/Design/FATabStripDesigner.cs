using System;
using System.Windows.Forms.Design;
using System.ComponentModel.Design;
using System.Drawing;
using System.Collections;
using System.Windows.Forms;
using FarsiLibrary.Win;

namespace FarsiLibrary.Win.Design
{
    public class FATabStripDesigner : ParentControlDesigner
    {
        #region Fields

        IComponentChangeService changeService;

        #endregion

        #region Initialize & Dispose

        public override void Initialize(System.ComponentModel.IComponent component)
        {
            base.Initialize(component);
            
            //Design services
            changeService = (IComponentChangeService)GetService(typeof(IComponentChangeService));

            //Bind design events
            changeService.ComponentRemoving += new ComponentEventHandler(OnRemoving);

            Verbs.Add(new DesignerVerb("Add TabStrip", new EventHandler(OnAddTabStrip)));
            Verbs.Add(new DesignerVerb("Remove TabStrip", new EventHandler(OnRemoveTabStrip)));
        }

        protected override void Dispose(bool disposing)
        {
            changeService.ComponentRemoving -= new ComponentEventHandler(OnRemoving);

            base.Dispose(disposing);
        }

        #endregion

        #region Private Methods

        private void OnRemoving(object sender, ComponentEventArgs e)
        {
            IDesignerHost host = (IDesignerHost) GetService(typeof (IDesignerHost));

            //Removing a button
            if (e.Component is FATabStripItem)
            {
                FATabStripItem itm = e.Component as FATabStripItem;
                if (Control.Items.Contains(itm))
                {
                    changeService.OnComponentChanging(Control, null);
                    Control.RemoveTab(itm);
                    changeService.OnComponentChanged(Control, null, null, null);
                    return;
                }
            }

            if (e.Component is FATabStrip)
            {
                for (int i = Control.Items.Count - 1; i >= 0; i--)
                {
                    FATabStripItem itm = Control.Items[i];
                    changeService.OnComponentChanging(Control, null);
                    Control.RemoveTab(itm);
                    host.DestroyComponent(itm);
                    changeService.OnComponentChanged(Control, null, null, null);
                }
            }
        }

        private void OnAddTabStrip(object sender, EventArgs e)
        {
            IDesignerHost host = (IDesignerHost)GetService(typeof(IDesignerHost));
            DesignerTransaction transaction = host.CreateTransaction("Add TabStrip");
            FATabStripItem itm = (FATabStripItem)host.CreateComponent(typeof(FATabStripItem));
            changeService.OnComponentChanging(Control, null);
            Control.AddTab(itm);
            int indx = Control.Items.IndexOf(itm) + 1;
            itm.Title = "TabStrip Page " + indx.ToString();
            Control.SelectItem(itm);
            changeService.OnComponentChanged(Control, null, null, null);
            transaction.Commit();
        }

        private void OnRemoveTabStrip(object sender, EventArgs e)
        {
            IDesignerHost host = (IDesignerHost)GetService(typeof(IDesignerHost));
            DesignerTransaction transaction = host.CreateTransaction("Remove Button");
            changeService.OnComponentChanging(Control, null);
            FATabStripItem itm = Control.Items[Control.Items.Count - 1];
            Control.UnSelectItem(itm);
            Control.Items.Remove(itm);
            changeService.OnComponentChanged(Control, null, null, null);
            transaction.Commit();
        }

        #endregion

        #region Overrides

        /// <summary>
        /// Check HitTest on <see cref="FATabStrip"/> control and
        /// let the user click on close and menu buttons.
        /// </summary>
        /// <param name="point"></param>
        /// <returns></returns>
        protected override bool GetHitTest(Point point)
        {
            HitTestResult result = Control.HitTest(point);
            if(result == HitTestResult.CloseButton || result == HitTestResult.MenuGlyph)
                return true;
            
            return false;
        }

        protected override void PreFilterProperties(IDictionary properties)
        {
            base.PreFilterProperties(properties);

            properties.Remove("DockPadding");
            properties.Remove("DrawGrid");
            properties.Remove("Margin");
            properties.Remove("Padding");
            properties.Remove("BorderStyle");
            properties.Remove("ForeColor");
            properties.Remove("BackColor");
            properties.Remove("BackgroundImage");
            properties.Remove("BackgroundImageLayout");
            properties.Remove("GridSize");
            properties.Remove("ImeMode");
        }

        protected override void WndProc(ref Message msg)
        {
            if (msg.Msg == 0x201)
            {
                Point pt = Control.PointToClient(Cursor.Position);
                FATabStripItem itm = Control.GetTabItemByPoint(pt);
                if (itm != null)
                {
                    Control.SelectedItem = itm;
                    ArrayList selection = new ArrayList();
                    selection.Add(itm);
                    ISelectionService selectionService = (ISelectionService)GetService(typeof(ISelectionService));
                    selectionService.SetSelectedComponents(selection);
                }
            }

            base.WndProc(ref msg);
        }

        public override ICollection AssociatedComponents
        {
            get
            {
                return Control.Items;
            }
        }

        public new virtual FATabStrip Control
        {
            get
            {
                return base.Control as FATabStrip;
            }
        }

        #endregion
    }
}
