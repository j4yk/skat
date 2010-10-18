/* Tests for the handling of data forms */

describe('Data Form handling', function () {
	it('transforms forms to html', function () {
		var form = DOM.parse('<x xmlns="jabber:x:data" type="query"><field var="a" label="Variable a" type="text-single"><value>foo</value></field>' +
			'<field var="b" type="text-multi"><value>First</value><value>Second</value></field>' +
			'<field var="c" type="boolean"><value>1</value></field>' +
			'<field var="d" type="jid-multi" label="Enter multiple jids here:" />' +
			'<field var="e" type="list-single" label="single list"><option>A</option><option>B</option><value>B</value></field>' +
			'<field var="f" type="list-multi" label="multi list"><option label="A">a</option><option label="B">b</option><option>C</option><value>a</value><value>C</value></field>' +
			'<field var="g" type="text-private" label="Password" />' + 
			'</x>');
		// make DataForm
		form = new Skat.Comm.XMPP.DataForm(form);
		// wait for XSLT
		waitsFor(Skat.Comm.XMPP.DataForm.prototype.ready, "XSLT files loaded", 10000);

		runs(function () {
			var html = form.to_html();
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

	var test_form = new Skat.Comm.XMPP.DataForm(DOM.parse('<x xmlns="jabber:x:data" type="form">' +
		'<field var="a" type="boolean"><value>0</value></field>' +
		'<field var="b" type="text-single"><value>foo</value></field>' +
		'<field var="c" type="text-multi"><value>1</value><value>2</value></field>' +
		'</x>').documentElement);

	it('has correctly initialized properties', function () {
		expect(test_form.form_dom).toBeDefined();
		expect(test_form.submit_dom).toBeDefined();
		expect(DOM.treeEquals(test_form.query_dom, test_form.submit_dom));
	});

	it('can set fields in a form', function () {
		var expdom = DOM.parse('<x xmlns="jabber:x:data" type="form">' +
			'<field var="a" type="boolean"><value>1</value></field>' +
			'<field var="b" type="text-single"><value>bla</value></field>' +
			'<field var="c" type="text-multi"><value>A</value><value>B</value><value>C</value></field>' +
			'</x>');
		jasmine.log($('value', test_form.submit_dom)[1].namespaceURI);
		test_form.set('a', true);
		test_form.set('b', 'bla');
		test_form.set('c', ['A', 'B', 'C']);

		expect(DOM.treeEquals(expdom.documentElement, test_form.submit_dom)).toBeTruthy();
		var diff = DOM.differenceNode(expdom.documentElement, test_form.submit_dom);
		if (diff) {
			jasmine.log(diff[0].localName);
			jasmine.log(diff[2]);
			if (typeof diff[2] === "string")
				jasmine.log([diff[0][diff[2]], diff[1][diff[2]]].join(" !== "));
		}
	});

	it('can set multiple fields in a form', function () {
		var expdom = new Strophe.Builder("x", {xmlns: "jabber:x:data", type: "form"})
		.c("field", {"var": "a", "type": "boolean"}).c("value").t("1").up().up()
		.c("field", {"var": "b", "type": "text-single"}).c("value").t("bla").up().up()
		.c("field", {"var": "c", "type": "text-multi"}).c("value").t("A").up().c("value").t("B").tree();
		test_form.set({
			a: true,
			b: "bla",
			c: ['A', 'B']
		});
		expect(DOM.treeEquals(expdom, test_form.submit_dom)).toBeTruthy();
		var diff = DOM.differenceNode(expdom, test_form.submit_dom);
		if (diff) {
			jasmine.log(diff[0].localName);
			jasmine.log(diff[2]);
			if (typeof diff[2] === "string")
				jasmine.log([diff[0][diff[2]], diff[1][diff[2]]].join(" !== "));
		}
	});
});
