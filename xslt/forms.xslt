<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml"
	xmlns:data="jabber:x:data">

<xsl:template match="/">
	<div>
		<xsl:apply-templates select="//data:field[@type!='hidden']" />
	</div>
</xsl:template>

<xsl:template match="data:field">
	<div>
		<xsl:if test="@var">
			<xsl:attribute name="id">
				<xsl:value-of select="@var" />
			</xsl:attribute>
		</xsl:if>
		<xsl:if test="@label">
			<label><xsl:value-of select="@label" /></label>
		</xsl:if>
		<xsl:choose>
			<xsl:when test="@type='text-single'">
				<xsl:call-template name="input"><xsl:with-param name="type">text</xsl:with-param></xsl:call-template>
			</xsl:when>
			<xsl:when test="@type='text-private'">
				<xsl:call-template name="input"><xsl:with-param name="type">password</xsl:with-param></xsl:call-template>
			</xsl:when>
			<xsl:when test="@type='text-multi'">
				<xsl:call-template name="textarea" />
			</xsl:when>
			<xsl:when test="@type='jid-single'">
				<xsl:call-template name="input"><xsl:with-param name="type">text</xsl:with-param></xsl:call-template>
			</xsl:when>
			<xsl:when test="@type='jid-multi'">
				<xsl:call-template name="textarea" />
			</xsl:when>
			<xsl:when test="@type='boolean'">
				<xsl:call-template name="input"><xsl:with-param name="type">checkbox</xsl:with-param></xsl:call-template>
			</xsl:when>
			<xsl:when test="@type='list-single'">
				<select>
					<xsl:attribute name="id"><xsl:value-of select="@var" /></xsl:attribute>
					<xsl:apply-templates select="data:option" />
				</select>
			</xsl:when>
			<xsl:when test="@type='list-multi'">
				<select multiple="multiple">
					<xsl:attribute name="id"><xsl:value-of select="@var" /></xsl:attribute>
					<xsl:apply-templates select="data:option" />
				</select>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="data:value" />
			</xsl:otherwise>
		</xsl:choose>
	</div>
</xsl:template>

<xsl:template name="textarea">
	<textarea><xsl:attribute name="name"><xsl:value-of select="@var" /></xsl:attribute></textarea>
</xsl:template>

<xsl:template name="input">
	<xsl:param name="type" />
	<input>
		<xsl:attribute name="name"><xsl:value-of select="@var" /></xsl:attribute>
		<xsl:attribute name="type"><xsl:value-of select="$type" /></xsl:attribute>
	</input>
</xsl:template>

<xsl:template match="data:option">
	<option>
		<xsl:attribute name="value"><xsl:value-of select="data:value" /></xsl:attribute>
		<xsl:if test="../data:value=data:value">
			<xsl:attribute name="selected">selected</xsl:attribute>
		</xsl:if>
		<xsl:value-of select="@label" />
	</option>
</xsl:template>

</xsl:stylesheet>
