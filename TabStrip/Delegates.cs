using System;

namespace FarsiLibrary.Win
{
    #region TabStripItemClosingEventArgs

    public class TabStripItemClosingEventArgs : EventArgs
    {
        public TabStripItemClosingEventArgs(FATabStripItem item)
        {
            _item = item;
        }

        private bool _cancel = false;
        private FATabStripItem _item;

        public FATabStripItem Item
        {
            get { return _item; }
            set { _item = value; }
        }

        public bool Cancel
        {
            get { return _cancel; }
            set { _cancel = value; }
        }

    }

    #endregion

    #region TabStripItemChangedEventArgs

    public class TabStripItemChangedEventArgs : EventArgs
    {
        FATabStripItem itm;
        FATabStripItemChangeTypes changeType;

        public TabStripItemChangedEventArgs(FATabStripItem item, FATabStripItemChangeTypes type)
        {
            changeType = type;
            itm = item;
        }

        public FATabStripItemChangeTypes ChangeType
        {
            get { return changeType; }
        }

        public FATabStripItem Item
        {
            get { return itm; }
        }
    }

    #endregion

    public delegate void TabStripItemChangedHandler(TabStripItemChangedEventArgs e);
    public delegate void TabStripItemClosingHandler(TabStripItemClosingEventArgs e);

}
