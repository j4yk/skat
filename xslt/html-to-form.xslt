<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:html="http://www.w3.org/1999/xhtml"
	xmlns="jabber:x:data">
<xsl:output type="xml" indent="yes" encoding="utf-8" />

<xsl:template match="/">
	<x type="submit">
		<xsl:apply-templates select="/html:div/html:div[@id!='']" />
	</x>
</xsl:template>

<xsl:template match="html:tr">
	<field>
		<xsl:attribute name="var"><xsl:value-of select="@id" /></xsl:attribute>
		<xsl:apply-templates select="html:textarea|html:input|html:select" />
	</field>
</xsl:template>

<xsl:template match="html:textarea">
	<!-- TODO: find out which node must be selected to find out the current text -->
	<xsl:call-template name="value-line" />
</xsl:template>

<!-- take each line in the element as a <value> -->
<xsl:template name="value-line">
	<xsl:param name="text" select="@value" />
	<xsl:variable name="contains-newline" select="contains($text, '&#xA;')" />
	<value>
		<xsl:choose>
			<xsl:when test="$contains-newline">
				<!-- only take one line at a time -->
				<xsl:value-of select="substring-before($text, '&#xA;')" />
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$text" />
			</xsl:otherwise>
		</xsl:choose>
	</value>
	<xsl:if test="$contains-newline">
		<!-- recurse if there are more lines to come -->
		<xsl:call-template name="value-line">
			<xsl:with-param name="text">
				<xsl:value-of select="substring-after($text, '&#xA;')" />
			</xsl:with-param>
		</xsl:call-template>
	</xsl:if>
</xsl:template>

<xsl:template match="html:input[@type='checkbox']">
	<value>
		<xsl:choose>
			<xsl:when test="@checked='checked'">
				<xsl:text>1</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>0</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</value>
</xsl:template>

<xsl:template match="html:input[@type='text' or @type='password']">
	<value>
		<xsl:value-of select="@value" />
	</value>
</xsl:template>

<xsl:template match="html:select">
	<xsl:apply-templates select="html:option[@selected='selected']" />
</xsl:template>

<xsl:template match="html:option">
	<value><xsl:value-of select="." /></value>
</xsl:template>


</xsl:stylesheet>
