Namespace UnitOperations.Auxiliary.Pipe

    <System.Serializable()> Public Class ThermalEditorDefinitions

        Implements Interfaces.ICustomXMLSerialization

        Protected m_type As ThermalProfileType = ThermalProfileType.Definir_CGTC
        Protected m_cgtc_definido, m_temp_amb_definir, m_calor_trocado, m_temp_amb_estimar, _
                    m_condtermica, m_espessura, m_velocidade As Double
        Protected m_material As Integer = 4
        Protected m_meio As Integer = 0
        Protected m_incluir_paredes, m_incluir_cti, m_incluir_cte, m_incluir_isolamento As Boolean

        Public Sub New()
            With Me
                .m_temp_amb_definir = 298.15
                .m_temp_amb_estimar = 298.15
            End With
        End Sub

        Public Property AmbientTemperatureGradient As Double = 0.0#

        Public Property AmbientTemperatureGradient_EstimateHTC As Double = 0.0#

        Public Property Incluir_isolamento() As Boolean
            Get
                Return m_incluir_isolamento
            End Get
            Set(ByVal value As Boolean)
                m_incluir_isolamento = value
            End Set
        End Property

        Public Property Incluir_cte() As Boolean
            Get
                Return m_incluir_cte
            End Get
            Set(ByVal value As Boolean)
                m_incluir_cte = value
            End Set
        End Property

        Public Property Incluir_cti() As Boolean
            Get
                Return m_incluir_cti
            End Get
            Set(ByVal value As Boolean)
                m_incluir_cti = value
            End Set
        End Property

        Public Property Incluir_paredes() As Boolean
            Get
                Return m_incluir_paredes
            End Get
            Set(ByVal value As Boolean)
                m_incluir_paredes = value
            End Set
        End Property

        Public Property Meio() As Integer
            Get
                Return m_meio
            End Get
            Set(ByVal value As Integer)
                m_meio = value
            End Set
        End Property

        Public Property Material() As Integer
            Get
                Return m_material
            End Get
            Set(ByVal value As Integer)
                m_material = value
            End Set
        End Property

        Public Property Velocidade() As Double
            Get
                Return m_velocidade
            End Get
            Set(ByVal value As Double)
                m_velocidade = value
            End Set
        End Property


        Public Property Espessura() As Double
            Get
                Return m_espessura
            End Get
            Set(ByVal value As Double)
                m_espessura = value
            End Set
        End Property

        Public Property Condtermica() As Double
            Get
                Return m_condtermica
            End Get
            Set(ByVal value As Double)
                m_condtermica = value
            End Set
        End Property

        Public Property Temp_amb_estimar() As Double
            Get
                Return m_temp_amb_estimar
            End Get
            Set(ByVal value As Double)
                m_temp_amb_estimar = value
            End Set
        End Property

        Public Property Calor_trocado() As Double
            Get
                Return m_calor_trocado
            End Get
            Set(ByVal value As Double)
                m_calor_trocado = value
            End Set
        End Property

        Public Property Temp_amb_definir() As Double
            Get
                Return m_temp_amb_definir
            End Get
            Set(ByVal value As Double)
                m_temp_amb_definir = value
            End Set
        End Property

        Public Property CGTC_Definido() As Double
            Get
                Return m_cgtc_definido
            End Get
            Set(ByVal value As Double)
                m_cgtc_definido = value
            End Set
        End Property

        Public Property TipoPerfil() As ThermalProfileType
            Get
                Return m_type
            End Get
            Set(ByVal value As ThermalProfileType)
                m_type = value
            End Set
        End Property

        Public Enum ThermalProfileType
            Definir_CGTC = 0
            Definir_Q = 1
            Estimar_CGTC = 2
        End Enum

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)

            Return elements

        End Function

    End Class

End Namespace

