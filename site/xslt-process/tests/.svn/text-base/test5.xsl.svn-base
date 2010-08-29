<xsl:transform
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0"
>

<xsl:variable name="g1" select="'AMA'"/>
<xsl:variable name="g2">this is a string</xsl:variable>

<xsl:template match="page">
 <xsl:apply-templates select="child::*"/>
</xsl:template>

<xsl:template match="h1">
 <xsl:variable name="title" select="."/>
 <xsl:variable name="str" select="'Title 123'"/>
 <xsl:variable name="bool" select="true()"/>
 <xsl:variable name="number" select="string-length($g1)"/>
 <xsl:variable name="number1" select="string-length(.)"/>
 <xsl:variable name="vendor" select="system-property('xsl:vendor')" />
 <xsl:if test="starts-with($vendor, 'SAXON')" >
   <xsl:variable name="sdate" select="Date:new()" xmlns:Date="java:java.util.Date"/>
   <xsl:variable name="snumber" select="string-length($sdate)"/>
   <xsl:value-of select="."/> as of <xsl:value-of select="$sdate"/>
   <xsl:text>.
</xsl:text>    
 </xsl:if>
 <xsl:if test="starts-with($vendor, 'Apache')" >
   <xsl:variable name="xdate" select="Date:new()" xmlns:Date="xalan://java.util.Date"/>
   <xsl:variable name="xnumber" select="string-length($xdate)"/> 
   <xsl:value-of select="."/> as of <xsl:value-of select="$xdate"/>
   <xsl:text>.
</xsl:text>          
 </xsl:if>
 <xsl:variable name="a1" select="."/>
 <xsl:value-of select="."/> as of <xsl:value-of select="'now'"/>
</xsl:template>

</xsl:transform>
