// *********************************************************************
// PLEASE DO NOT REMOVE THIS DISCLAIMER
//
// WpfPropertyGrid - By Jaime Olivares
// Article site: http://www.codeproject.com/KB/grid/WpfPropertyGrid.aspx
// Author site: www.jaimeolivares.com
// License: Code Project Open License (CPOL)
//
// *********************************************************************

using System.Activities;
using System.Activities.Presentation;
using System.Collections.Generic;
using System.ComponentModel;
using System.Reflection;
using System.Windows.Controls;
using System.Linq;

namespace System.Windows.Control
{
    /// <summary>
    /// WPF Native PropertyGrid class, taken from Workflow Foundation Designer
    /// </summary>
    public class WpfPropertyGrid : Grid
    {
        #region Private fields
        private WorkflowDesigner Designer;
        private DummyActivity UniqueActivity;
        private MethodInfo RefreshMethod;
        private TextBlock SelectionTypeLabel;
        #endregion

        #region Public properties
        /// <summary>
        /// Facade for the dummy activity's SelectedObject
        /// </summary>
        public object SelectedObject
        {
            get
            {
                return this.UniqueActivity.SelectedObject;
            }
            set
            {
                this.UniqueActivity.SelectedObject = value;
                this.RefreshPropertyList();

                this.SelectionTypeLabel.Text = value == null ? string.Empty : value.GetType().Name;
            }
        }

        /// <summary>
        /// XAML information with PropertyGrid's font and color information
        /// </summary>
        /// <seealso>Documentation for WorkflowDesigner.PropertyInspectorFontAndColorData</seealso>
        public string FontAndColorData
        {
            set { Designer.PropertyInspectorFontAndColorData = value; }
        }
        #endregion

        /// <summary>
        /// Default constructor, creates a hidden designer view and a property inspector
        /// </summary>
        public WpfPropertyGrid()
        {
            this.UniqueActivity = new DummyActivity();

            this.Designer = new WorkflowDesigner();
            Designer.PropertyInspectorView.Visibility = Visibility.Visible;
            Designer.View.Visibility = Windows.Visibility.Hidden;

            this.Children.Add(Designer.PropertyInspectorView);
            this.Children.Add(Designer.View);

            Designer.Load(this.UniqueActivity);

            this.RefreshMethod = Designer.PropertyInspectorView.GetType().GetMethod("RefreshPropertyList", Reflection.BindingFlags.NonPublic | Reflection.BindingFlags.Instance | Reflection.BindingFlags.DeclaredOnly);

            this.SelectionTypeLabel = this.Designer.PropertyInspectorView.GetType().GetMethod("get_SelectionTypeLabel",
                Reflection.BindingFlags.Public | Reflection.BindingFlags.NonPublic | Reflection.BindingFlags.Instance
                | Reflection.BindingFlags.DeclaredOnly).Invoke(Designer.PropertyInspectorView, new object[0]) as TextBlock;

            this.SelectionTypeLabel.Text = string.Empty;
        }

        /// <summary>
        /// Updates the PropertyGrid's properties
        /// </summary>
        public void RefreshPropertyList()
        {
            RefreshMethod.Invoke(Designer.PropertyInspectorView, new object[] { false });
        }

        /// <summary>
        /// Dummy activity, will contain the selected object. Will be exposed in the PropertyGrid
        /// </summary>
        public sealed class DummyActivity : CodeActivity, ICustomTypeDescriptor
        {
            /// <summary>
            /// Storage of the Selected Object
            /// </summary>
            [Browsable(false)]
            public object SelectedObject { get; set; }

            /// <summary>
            /// Override to hide this property
            /// </summary>
            [Browsable(false)]
            public new string DisplayName { get; set; }

            /// <summary>
            /// Not used, since a workflow instance is never created
            /// </summary>
            /// <param name="context">Not used</param>
            protected override void Execute(CodeActivityContext context)
            {
                // Do nothing
            }

            /// <summary>
            /// Default constructor
            /// </summary>
            public DummyActivity()
            {
            }

            /// <summary>
            /// Type descriptor methods. All them will be redirected to expose SelectedObject's type description
            /// </summary>
            /// <returns></returns>
            #region ICustomTypeDescriptor Members
            public string GetClassName()
            {
                return SelectedObject == null ? null : TypeDescriptor.GetClassName(SelectedObject);
            }
            public AttributeCollection GetAttributes()
            {
                return SelectedObject == null ? null : TypeDescriptor.GetAttributes(SelectedObject);
            }
            public string GetComponentName()
            {
                return SelectedObject == null ? null : TypeDescriptor.GetComponentName(SelectedObject);
            }
            public TypeConverter GetConverter()
            {
                return SelectedObject == null ? null : TypeDescriptor.GetConverter(SelectedObject);
            }
            public EventDescriptor GetDefaultEvent()
            {
                return SelectedObject == null ? null : TypeDescriptor.GetDefaultEvent(SelectedObject);
            }
            public PropertyDescriptor GetDefaultProperty()
            {
                return SelectedObject == null ? null : TypeDescriptor.GetDefaultProperty(SelectedObject);
            }
            public object GetEditor(Type editorBaseType)
            {
                return SelectedObject == null ? null : TypeDescriptor.GetEditor(SelectedObject, editorBaseType);
            }
            public EventDescriptorCollection GetEvents(Attribute[] attributes)
            {
                return SelectedObject == null ? null : TypeDescriptor.GetEvents(SelectedObject, attributes);
            }
            public EventDescriptorCollection GetEvents()
            {
                return SelectedObject == null ? null : TypeDescriptor.GetEvents(SelectedObject);
            }
            public object GetPropertyOwner(PropertyDescriptor pd)
            {
                return SelectedObject == null ? null : this;
            }
            public PropertyDescriptorCollection GetProperties(Attribute[] attributes)
            {
                return GetProperties();
            }

            // returns the properties of the selected object, instead of the activity itself
            public PropertyDescriptorCollection GetProperties()
            {
                if (SelectedObject == null)
                {
                    return TypeDescriptor.GetProperties(this, new Attribute[] { new BrowsableAttribute(false) }, true);
                }

                var props = new PropertyDescriptorCollection(null);
                if (SelectedObject is ICustomTypeDescriptor)
                {
                    foreach (PropertyDescriptor prop in (SelectedObject as ICustomTypeDescriptor).GetProperties())
                    {
                        props.Add(new WPG_PropertyDescriptor(this, prop.Name));
                    }
                }
                else
                {
                    foreach (var prop in SelectedObject.GetType().GetProperties())
                    {
                        props.Add(new WPG_PropertyDescriptor(this, prop.Name));
                    }
                }
                return props;
            }
            #endregion
        }

        /// <summary>
        /// Property descriptor for the SelectedObject. Will expose SelectedObject properties as if they were DummyActivity properties
        /// </summary>
        public class WPG_PropertyDescriptor : PropertyDescriptor
        {
            #region Private fields
            private DummyActivity OwnerActivity = null;
            private string PropName = null;
            private PropertyInfo PropInfo = null;
            #endregion

            public WPG_PropertyDescriptor(DummyActivity owner, string name)
                : base(name, null)
            {
                this.OwnerActivity = owner;
                this.PropName = name;
                this.PropInfo = OwnerActivity.SelectedObject.GetType().GetProperty(name);
            }

            public override AttributeCollection Attributes
            {
                get
                {
                    List<Attribute> attrs = new List<Attribute>();
                    foreach (var attrInfo in PropInfo.GetCustomAttributes(true))
                    {
                        attrs.Add(attrInfo as Attribute);
                    }
                    return new AttributeCollection(attrs.ToArray());
                }
            }
            public override bool CanResetValue(object component)
            {
                return false;
            }
            public override Type ComponentType
            {
                get { return this.OwnerActivity.GetType(); }
            }
            public override string DisplayName
            {
                get { return this.PropName; }
            }
            public override string Description
            {
                get { return this.PropName; }
            }
            public override bool IsReadOnly
            {
                get
                {
                    return false;
                }
            }
            public override string Name
            {
                get
                {
                    return this.PropName;
                }
            }
            public override Type PropertyType
            {
                get
                {
                    return PropInfo.PropertyType;
                }
            }
            public override void ResetValue(object component)
            {
            }
            public override bool ShouldSerializeValue(object component)
            {
                return true;
            }
            public override object GetValue(object component)
            {
                return PropInfo.GetValue(this.OwnerActivity.SelectedObject, null);
            }
            public override void SetValue(object component, object value)
            {
                PropInfo.SetValue(this.OwnerActivity.SelectedObject, value, null);
            }
        }
    }
}
