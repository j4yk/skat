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
			// Internet Explorer
			var doc = new ActiveXObject("Microsoft.XMLDOM");
			doc.async = "false";
			doc.loadXML(str);
			return doc;
		}
	},

	str2xml: function (str, targetDOM) {
		if (targetDOM) {
			return targetDOM.ownerDocument.importNode(DOM.parse(str).documentElement, true);
		} else {
			return DOM.parse(str).documentElement;
		}
	},

	treeEquals: function (node1, node2) {
		//return Strophe.serialize(node1) == Strophe.serialize(node2);
		// checks if two DOM trees are semantically equal
		if (node1 === node2) return true; // same node
		// make sure both nodes are defined
		if (node1 === undefined) {
			return node2 === undefined;
		} else if (node2 === undefined) {
			return node1 === undefined;
		}
		var result = null;
		$.map(["nodeName", "nodeValue", "textContent", "childElementCount"], function (property) {
			if (node1[property] !== node2[property]) {
				result = false;
				return false;
			}
		});
		if (result === false) return result;
		// check attributes
		var attributes1 = node1.attributes;
		var attributes2 = node2.attributes;
		if (attributes1 === null) {
			if (attributes2 !== null) return false;
		} else if (attributes2 === null) {
			return false;
		} else {
			if (attributes1.length !== attributes2.length) return false;
			for (var i = 0; i < attributes1.length; i++) {
				if (!DOM.treeEquals(attributes1[i], attributes2[i])) return false;
			}
		}
		// check children nodes
		var children1 = node1.childNodes;
		var children2 = node2.childNodes;
		if (children1 === null) {
			if (children2 !== null) return false;
		} else {
			if (children1.length !== children2.length) return false;
			for (var i = 0; i < children1.length; i++) {
				if (!DOM.treeEquals(children1[i], children2[i])) return false;
			}
		}
		return true;
	},

	differenceNode: function (node1, node2) {
		// returns the nodes that are different from node1 and node2
		if (node1 === node2) return null; // same node
		// make sure both nodes are defined
		if (node1 === undefined) {
			return node2 === undefined ? null : [node1, node2];
		} else if (node2 === undefined) {
			return node1 === undefined ? null : [node1, node2];
		}
		var result = null;
		$.map(["nodeName", "nodeValue", "textContent", "childElementCount"], function (property) {
			// check atomic properties (nodeValue, nodeName etc.)
			if (node1[property] !== node2[property]) {
				result = [node1, node2, property];
				return;
			}
		});
		if (result) return result;
		// check attributes
		var attributes1 = node1.attributes;
		var attributes2 = node2.attributes;
		if (attributes1 === null) {
			if (attributes2 !== null) return [node1, node2, 'attributes'];
		} else if (attributes2 === null) {
			return [node1, node2, 'attributes'];
		} else {
			if (attributes1.length !== attributes2.length)
				return [node1, node2, 'attributes'];
			for (var i = 0; i < attributes1.length; i++) {
				if (!DOM.treeEquals(attributes1[i], attributes2[i]))
					return [attributes1[i], attributes2[i], node1];
			}
		}
		// check children nodes
		var children1 = node1.childNodes;
		var children2 = node2.childNodes;
		if (children1 === null) {
			if (children2 !== null) return [node1, node2, 'childNodes'];
		} else {
			if (children1.length !== children2.length)
				return [node1, node2, 'childNodes'];
			for (var i = 0; i < children1.length; i++) {
				var diff = DOM.differenceNode(children1[i], children2[i]);
				if (diff)
					return diff;
			}
		}
		return null;
	},

	build: function (name, ns, attrs) {
		return new DOM.Builder(name, ns, attrs);
	}
}

DOM.Builder = function (name, ns, attrs) {
	this.doc = document.implementation.createDocument("", "DOM.Builder", null);
	attrs = arguments.length === 2 && typeof arguments[1] === "object" ? ns : attrs;
	var xmlns = attrs && attrs.xmlns || ns;
	var newnode = this.doc.createElementNS(xmlns, name);
	this.node = newnode;
	for (k in attrs) {
		if (attrs.hasOwnProperty(k)) {
			this.attr(k, attrs[k]);
		}
	}
	this.nodeTree = this.node;
}

DOM.Builder.prototype = {
	c: function (name, ns, attrs) {
		attrs = arguments.length === 2 && typeof arguments[1] === "object" ? ns : attrs;
		var xmlns = attrs && attrs.xmlns || ns;
		var newnode = this.doc.createElementNS(xmlns || this.node.namespaceURI, name);
		this.node.appendChild(newnode);
		this.node = newnode;
		for (k in attrs) {
			if (attrs.hasOwnProperty(k)) {
				this.attr(k, attrs[k]);
			}
		}
		return this;
	},

	t: function (text) {
		var txt = this.doc.createTextNode(text);
		this.node.appendChild(txt);
		return this;
	},

	attr: function (name, ns_or_value, value) {
		arguments.length === 3 && this.node.setAttributeNS(ns_or_value, name, value) || this.node.setAttribute(name, ns_or_value);
		return this;
	},

	up: function () {
		this.node = this.node.parentNode;
		return this;
	},

	tree: function () {
		return this.nodeTree;
	}
}
