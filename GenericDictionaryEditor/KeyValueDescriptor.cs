using System;
using System.ComponentModel;
using System.Drawing.Design;

namespace Wexman.Design
{
    internal class KeyValueDescriptor : PropertyDescriptor
    {
        private PropertyDescriptor _pd;

        private Type m_ConverterType;
        private Type m_EditorType;
        private Type m_AttributeProviderType;
        private string m_DisplayName;

        public KeyValueDescriptor(PropertyDescriptor pd, Type converterType, Type editorType, Type attributeProviderType, string displayName)
            : base(pd)
        {
            this._pd = pd;

            m_ConverterType = converterType;
            m_EditorType = editorType;
            m_AttributeProviderType = attributeProviderType;
            m_DisplayName = displayName;
        }

        public override string DisplayName
        {
            get
            {
                return m_DisplayName;
            }
        }

        public override bool CanResetValue(object component)
        {
            return _pd.CanResetValue(component);
        }

        public override Type ComponentType
        {
            get { return _pd.ComponentType; }
        }

        public override object GetValue(object component)
        {
            return _pd.GetValue(component);
        }

        public override bool IsReadOnly
        {
            get { return _pd.IsReadOnly; }
        }

        public override Type PropertyType
        {
            get { return _pd.PropertyType; }
        }

        public override void ResetValue(object component)
        {
            _pd.ResetValue(component);
        }

        public override void SetValue(object component, object value)
        {
            _pd.SetValue(component, value);
        }

        public override bool ShouldSerializeValue(object component)
        {
            return _pd.ShouldSerializeValue(component);
        }

        public override TypeConverter Converter
        {
            get
            {
                if (m_ConverterType != null)
                    return Activator.CreateInstance(m_ConverterType) as TypeConverter;
                else
                {
                    return TypeDescriptor.GetConverter(PropertyType);
                }
            }
        }

        public override object GetEditor(Type editorBaseType)
        {
            if (m_EditorType != null)
                return Activator.CreateInstance(m_EditorType) as UITypeEditor;
            else
                return TypeDescriptor.GetEditor(PropertyType, typeof(UITypeEditor));
        }

        public override AttributeCollection Attributes
        {
            get
            {
                if (m_AttributeProviderType != null)
                {
                    return (Activator.CreateInstance(m_AttributeProviderType) as AttributeProvider).GetAttributes(this.PropertyType);
                }
                else
                {
                    return TypeDescriptor.GetAttributes(PropertyType);
                }
            }
        }
    }
}
