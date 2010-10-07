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
		expect($('input', xml).children().length).toBe(1);
	});
});

describe('XSLT helper functions', function () {
	it("basically works", function () {
		var xslt = DOM.parse("<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform' xmlns='http://www.w3.org/1999/xhtml'>" +
			"<xsl:template match='div'><xsl:copy-of select='.' /></xsl:template>" +
			"</xsl:stylesheet>");
		var input = DOM.parse("<div><a href='.'>Text</a></div>");
		var output = $(XSLT.transform(input, xslt).firstChild);
		expect(output[0].nodeName.toUpperCase()).toEqual("DIV");
		expect(output.children()[0].nodeName.toUpperCase()).toEqual("A");
		expect($('a', output).attr('href')).toEqual(".");
	});

	it('transforms XMPP data forms to html', function () {
		var form = DOM.parse('<x xmlns="jabber:x:data" type="query"><field var="a" label="Variable a" type="text-single"><value>foo</value></field>' +
			'<field var="b" type="text-multi"><value>First</value><value>Second</value></field>' +
			'<field var="c" type="boolean"><value>1</value></field>' +
			'<field var="d" type="jid-multi" label="Enter multiple jids here:" />' +
			'<field var="e" type="list-single" label="single list"><option>A</option><option>B</option><value>B</value></field>' +
			'<field var="f" type="list-multi" label="multi list"><option label="A">a</option><option label="B">b</option><option>C</option><value>a</value><value>C</value></field>' +
			'<field var="g" type="text-private" label="Password" />' + 
			'</x>');
		waitsFor(function () {
			return Skat.Comm.form2html_xslt.DONE !== 4
		}, "", 10000);
		runs(function () {
			var html = XSLT.transform(form, Skat.Comm.form2html_xslt);
			expect(html).not.toBeNull();
			expect(html.childNodes[0].nodeName.toUpperCase()).toEqual("TABLE");
			html = $(html.firstChild);
			expect(html.find('tr').length).toBe(7);
			expect($('tr#a input', html).attr('type')).toEqual('text');
			expect($('tr#a label', html)[0].innerHTML).toEqual('Variable a');
			expect($('tr#b textarea', html).length).toBe(1);
			expect($('tr#b textarea', html).val()).toEqual('First\nSecond');
			expect($('tr#c input', html).attr('type')).toEqual('checkbox');
			expect($('tr#c input', html).attr('checked')).toBeTruthy();
			expect($('tr#d textarea', html).val()).toEqual('');
			expect($('tr#d label', html)[0].innerHTML).toEqual('Enter multiple jids here:');
			expect($('tr#e select', html).attr('multiple')).toBeFalsy();
			expect($('tr#e label', html)[0].innerHTML).toEqual('single list');
			var eoptions = $('tr#e select option', html);
			expect(eoptions[0].value).toEqual('A');
			expect(eoptions[0].text).toEqual('A');
			expect(eoptions[0].selected).toBeFalsy();
			expect(eoptions[1].value).toEqual('B');
			expect(eoptions[1].selected).toBeTruthy();
			expect($('tr#f select', html).attr('multiple')).toBeTruthy();
			var foptions = $('tr#f select option', html);
			expect(foptions[0].value).toEqual('a');
			expect(foptions[0].text).toEqual('A');
			expect(foptions[0].selected).toBeTruthy();
			expect(foptions[1].value).toEqual('b');
			expect(foptions[1].text).toEqual('B');
			expect(foptions[1].selected).toBeFalsy();
			expect(foptions[2].value).toEqual('C');
			expect(foptions[2].text).toEqual('C');
			expect(foptions[2].selected).toBeTruthy();
			expect($('tr#g input', html).attr('type')).toEqual('password');
			expect($('tr#g label', html)[0].innerHTML).toEqual('Password');
		});
	});

});

