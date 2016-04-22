<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
 <xsl:output omit-xml-declaration="no" indent="yes"/>

   <xsl:key name="groups" match="Nome" use="."/>

   <xsl:template match="/DocumentElement">
      <Objects>
         <xsl:apply-templates select="Results/Nome[generate-id() = generate-id(key('groups', .)[1])]"/>
      </Objects>
   </xsl:template>

   <xsl:template match="Nome">
      <xsl:variable name="currentGroup" select="."/>
      <Object>
         <xsl:attribute name="name">
            <xsl:value-of select="$currentGroup"/>
         </xsl:attribute>
         <xsl:attribute name="type">
            <xsl:value-of select="../Tipo"/>
         </xsl:attribute>
         <xsl:for-each select="key('groups', $currentGroup)">
               <Property>
                  <xsl:attribute name="name">
                     <xsl:value-of select="../Propriedade"/>
                  </xsl:attribute>
                  <xsl:attribute name="value">
                     <xsl:value-of select="../Valor"/>
                  </xsl:attribute>
                  <xsl:attribute name="units">
                     <xsl:value-of select="../Unidade"/>
                  </xsl:attribute>
               </Property>
         </xsl:for-each>
      </Object>
   </xsl:template>

</xsl:stylesheet>