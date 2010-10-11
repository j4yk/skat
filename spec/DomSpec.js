describe('DOM helper functions', function () {
	it("can parse XML from string", function () {
		var str = "<root xmlns='urn:xml:test'><child attr='foo'><subchild /></child><child2><subchild attr='bla'/></child2></root>";
		var xml = DOM.parse(str);
		expect($('root', xml).children().length).toBe(2);
		expect($('root', xml).attr('xmlns')).toEqual('urn:xml:test');
		expect($('child', xml).attr('attr')).toEqual('foo');
		expect($('child2 subchild', xml).attr('attr')).toEqual('bla');
		expect($('root > child > subchild', xml).length).toBe(1);
	});

	it("can build DOM trees", function () {
		var tree = new DOM.Builder("foo", "urn:test", {bar: "bla"}).c("value").t("x").tree();
		expect(tree.localName).toEqual('foo');
		expect(tree.namespaceURI).toEqual('urn:test');
		expect(tree.attributes.length).toBe(1);
		expect(tree.attributes[0]).toBeDefined();
		expect(tree.attributes[0].nodeName).toEqual('bar');
		expect(tree.attributes[0].nodeValue).toEqual('bla');
		expect(tree.childNodes.length).toBe(1);
		expect(tree.childNodes[0].localName).toEqual('value');
		expect(tree.childNodes[0].namespaceURI).toEqual('urn:test');
	});

	it("doesn't confuse xml and html", function () {
		var str = "<html xmlns='urn:xml:something'><input><testelement /></input></html>";
		var xml = DOM.parse(str);
		expect($('input', xml).children().length).toBe(1);
	});

	it('can tell DOM trees to be equal', function () {
		var dom1 = DOM.parse("<foo bla='bar'><element /><element2>hallo</element2>bar</foo>");
		var dom2 = DOM.parse("<foo bla='bar'><element /><element2>hallo</element2>bar</foo>");
		expect(DOM.treeEquals(dom1, dom2)).toBeTruthy();
	});

	it('can tell differing DOM trees not to be equal', function () {
		var dom1 = DOM.parse("<foo xmlns='x' />");
		var dom2 = DOM.parse("<foo xmlns='y' />");
		expect(DOM.treeEquals(dom1, dom2)).toBeFalsy();
		var dom3 = DOM.parse("<bar xmlns='x' />");
		expect(DOM.treeEquals(dom1, dom3)).toBeFalsy();
		var dom4 = DOM.parse("<foo xmlns='x'><bar /></foo>");
		expect(DOM.treeEquals(dom1, dom4)).toBeFalsy();
		expect(DOM.treeEquals(dom3, dom4)).toBeFalsy();
		var dom5 = DOM.parse("<foo xmlns='x'><baz /></foo>");
		expect(DOM.treeEquals(dom4, dom5)).toBeFalsy();
		var dom6 = DOM.parse("<foo xmlns='x'>hallo</foo>");
		expect(DOM.treeEquals(dom1, dom6)).toBeFalsy();
		expect(DOM.treeEquals(dom4, dom6)).toBeFalsy();
		var dom7 = DOM.parse("<foo xmlns='x'><bar>hallo</bar></foo>");
		expect(DOM.treeEquals(dom4, dom7)).toBeFalsy();
		expect(DOM.treeEquals(dom6, dom7)).toBeFalsy();
		var dom8 = DOM.parse("<foo xmlns='x'><bar>hallo </bar></foo>");
		expect(DOM.treeEquals(dom7, dom8)).toBeFalsy();
		expect(DOM.treeEquals($('<input type="checkbox" checked="checked">')[0], 
			$('<input type="checkbox">')[0])).toBeFalsy();
		expect(DOM.treeEquals($('<select><option label="a">A</option></select>')[0], 
			$('<select multiple="multiple"><option label="a">A</option></select>')[0])).toBeFalsy(); 
	});

	it('can find attribute differences', function () {
		var elm1 = DOM.parse('<foo var="x" />').documentElement;
		var elm2 = DOM.parse('<foo var="y" />').documentElement;
		var diff = DOM.differenceNode(elm1, elm2);
		expect(diff[0]).toBe(elm1.attributes['var']);
		expect(diff[1]).toBe(elm2.attributes['var']);
		expect(diff[2]).toBe(elm1);
	});

	it('can find property differences in DOM trees', function () {
		var elm1 = DOM.parse('<foo>hallo</foo>').documentElement;
		var elm2 = DOM.parse('<foo>tschüss</foo>').documentElement;
		var diff = DOM.differenceNode(elm1, elm2);
		expect(diff[0]).toBe(elm1);
		expect(diff[1]).toBe(elm2);
		expect(diff[2]).toBe('textContent');

		elm1 = DOM.parse('<foo xmlns="x" />').documentElement;
		elm2 = DOM.parse('<foo xmlns="y" />').documentElement;
		diff = DOM.differenceNode(elm1, elm2);
		expect(diff[0]).toBe(elm1);
		expect(diff[1]).toBe(elm2);
		expect(diff[2]).toBe('namespaceURI');
	});

	it('can find children node differnces', function () {
		var elm1 = DOM.parse('<foo><bar /></foo>').documentElement;
		var elm2 = DOM.parse('<foo><baz /></foo>').documentElement;
		var diff = DOM.differenceNode(elm1, elm2);
		expect(diff[0]).toBe(elm1.childNodes[0]);
		expect(diff[1]).toBe(elm2.childNodes[0]);
		expect(diff[2]).toBe('localName');

		var elm3 = DOM.parse('<foo />').documentElement;
		var elm4 = DOM.parse('<foo><bar /><baz /></foo>').documentElement;
		diff = DOM.differenceNode(elm1, elm3);
		expect(diff[0]).toBe(elm1);
		expect(diff[1]).toBe(elm3);
		expect(diff[2]).toBe('childElementCount');
		diff = DOM.differenceNode(elm1, elm4);
		expect(diff[0]).toBe(elm1);
		expect(diff[1]).toBe(elm4);
		expect(diff[2]).toBe('childElementCount');
	});
});
