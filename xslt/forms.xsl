<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml"
	xmlns:data="jabber:x:data">
<!-- <xsl:output type="html" indent="yes" /> -->

<xsl:template match="/">
	<table>
		<xsl:apply-templates select="//data:field[@type!='hidden']" />
	</table>
</xsl:template>

<xsl:template match="data:field">
	<tr>
		<xsl:if test="@var">
			<xsl:attribute name="id">
				<xsl:value-of select="@var" />
			</xsl:attribute>
		</xsl:if>
		<td>
			<xsl:if test="@label">
				<label><xsl:value-of select="@label" /></label>
			</xsl:if>
		</td>
		<td>
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
		</td>
	</tr>
</xsl:template>

<xsl:template name="textarea">
	<textarea>
		<xsl:attribute name="name"><xsl:value-of select="@var" /></xsl:attribute>
		<xsl:for-each select="data:value">
			<xsl:value-of select="." />
			<xsl:if test="not(position()=count(../data:value))">
				<!-- all but the last item are followed by a newline -->
				<xsl:text>
</xsl:text>
			</xsl:if>
		</xsl:for-each>
	</textarea>
</xsl:template>

<xsl:template name="input">
	<xsl:param name="type" />
	<input>
		<xsl:attribute name="name"><xsl:value-of select="@var" /></xsl:attribute>
		<xsl:attribute name="type"><xsl:value-of select="$type" /></xsl:attribute>
		<xsl:choose>
			<xsl:when test="$type='checkbox'">
				<xsl:if test="data:value=1">
					<xsl:attribute name="checked">checked</xsl:attribute>
				</xsl:if>
			</xsl:when>
			<xsl:when test="$type='text'">
				<xsl:if test="data:value">
					<xsl:attribute name="value"><xsl:value-of select="data:value" /></xsl:attribute>
				</xsl:if>
			</xsl:when>
		</xsl:choose>
	</input>
</xsl:template>

<xsl:template match="data:option">
	<xsl:variable name="value"><xsl:value-of select="normalize-space(string(.))" /></xsl:variable>
	<option>
		<xsl:attribute name="value"><xsl:value-of select="$value" /></xsl:attribute>
		<xsl:if test="../data:value[text()=$value]">
			<xsl:attribute name="selected">selected</xsl:attribute>
		</xsl:if>
		<xsl:if test="not(@label)">
			<xsl:value-of select="." />
		</xsl:if>
		<xsl:value-of select="@label" />
	</option>
</xsl:template>

</xsl:stylesheet>
