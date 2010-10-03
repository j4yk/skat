describe('XSLT helper functions', function () {
});

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

	it("doesn't confuse xml and html", function () {
		var str = "<html xmlns='urn:xml:something'><input><testelement /></input></html>";
		var xml = DOM.parse(str);
		document.__thenode = xml;
		expect($('input', xml).children().length).toBe(1);
	});
});
