describe('XSLT helper functions', function () {
	it("basically works", function () {
		var xslt = DOM.parse('<xsl:stylesheet' +
			' xmlns:xsl="http://www.w3.org/1999/XSL/Transform"' +
			' xmlns="http://www.w3.org/1999/xhtml"' +
			'>' +
			'<xsl:template match="div"><xsl:copy-of select="." /></xsl:template>' +
			'</xsl:stylesheet>').documentElement;
		var input = DOM.parse("<div><a href='.'>Text</a></div>");
		var output = $(XSLT.transform(input, xslt).firstChild);
		expect(output[0].nodeName.toUpperCase()).toEqual("DIV");
		expect(output.children()[0].nodeName.toUpperCase()).toEqual("A");
		expect($('a', output).attr('href')).toEqual(".");
	});
});

