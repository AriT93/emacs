<?xml version="1.0"?>

<xsl:transform
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0"
>

<xsl:template match="page">
 <xsl:variable name="a" select="h1"/>
 <xsl:variable name="b" select="'Title 123'"/>
 <xsl:apply-templates select="child::*"/>
</xsl:template>

<xsl:template match="h1">
 <xsl:variable name="a1" select="."/>
 <!-- this is an intentional mistake, xsl:variable above is already closed -->
 </xsl:variable>
</xsl:template>

</xsl:transform>
