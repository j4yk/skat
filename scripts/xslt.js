XSLT = {
	// source: http://www.w3schools.com/xsl/xsl_transformation.asp
	transform: function (xml, xslt) {
		if (window.ActiveXObject) {
			return xml.transformNode(xslt);
		} else if (document.implementation && document.implementation.createDocument) {
			xsltProcessor = new XSLTProcessor();
			xsltProcessor.importStylesheet(xslt);
			return xsltProcessor.transformToFragment(xml, document);
		}
	}
}

DOM = {
	parse: function (str) {
		if (window.DOMParser) {
			parser = new DOMParser();
			return parser.parseFromString(str, "text/xml");
		} else {
			var doc = new ActiveXObject("Microsoft.XMLDOM");
			doc.async = "false";
			doc.loadXML(text);
			return doc;
		}
	}
}
