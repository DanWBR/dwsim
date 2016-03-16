using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace Wexman.Design
{
    internal class EditableKeyValuePair<TKey, TValue> : CustomTypeDescriptor
    {

        public TKey Key { get; set; }
        public TValue Value { get; set; }
  
        public EditableKeyValuePair(TKey key, TValue value, GenericDictionaryEditorAttribute editorAttribute)
        {
            this.Key = key;
            this.Value = value;

            if (editorAttribute == null)
                throw new ArgumentNullException("editorAttribute");

            m_EditorAttribute = editorAttribute;
        }

        private GenericDictionaryEditorAttribute m_EditorAttribute;
        public GenericDictionaryEditorAttribute EditorAttribute
        {
            get { return m_EditorAttribute; }
            set { m_EditorAttribute = value; }
        }

        public override PropertyDescriptorCollection GetProperties(Attribute[] attributes)
        {
            return GetProperties();
        }

        public override PropertyDescriptorCollection GetProperties()
        {
            List<PropertyDescriptor> properties = new List<PropertyDescriptor>();

            KeyValueDescriptor KeyDescriptor = new KeyValueDescriptor(TypeDescriptor.CreateProperty(this.GetType(), "Key", typeof(TKey)),  this.EditorAttribute.KeyConverterType, this.EditorAttribute.KeyEditorType, this.EditorAttribute.KeyAttributeProviderType, this.EditorAttribute.KeyDisplayName);
            properties.Add(KeyDescriptor);

            KeyValueDescriptor ValueDescriptor = new KeyValueDescriptor(TypeDescriptor.CreateProperty(this.GetType(), "Value", typeof(TValue)), this.EditorAttribute.ValueConverterType, this.EditorAttribute.ValueEditorType, this.EditorAttribute.ValueAttributeProviderType, this.EditorAttribute.ValueDisplayName);
            properties.Add(ValueDescriptor);

            return new PropertyDescriptorCollection(properties.ToArray());
        }

        public override object GetPropertyOwner(PropertyDescriptor pd)
        {
            return this;
        }
    }

}
