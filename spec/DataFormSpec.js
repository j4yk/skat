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
			'</x>').documentElement;
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

	beforeEach(function () {
		test_form = new Skat.Comm.XMPP.DataForm(DOM.parse('<x xmlns="jabber:x:data" type="form">' +
			'<field var="a" type="boolean"><value>0</value></field>' +
			'<field var="b" type="text-single"><value>foo</value></field>' +
			'<field var="c" type="text-multi"><value>1</value><value>2</value></field>' +
			'</x>').documentElement);
	});

	it('has correctly initialized properties', function () {
		expect(test_form.form_dom).toBeDefined();
		expect(test_form.submit_dom).toBeDefined();
		expect(DOM.treeEquals(test_form.query_dom, test_form.submit_dom));
	});

	describe('field method', function () {
		it('allows to read fields', function () {
			expect(test_form.field('a')).toBe('0');
			expect(test_form.field('b')).toBe('foo');
			expect(test_form.field('c')).toEqual(['1', '2']);
		});

		it('returns undefined on undefined fields', function () {
			expect(test_form.field('d')).not.toBeDefined();
		});
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

	it('can set multiple fields in a form at once', function () {
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

	it('can filter form fields (new forms are created)', function () {
		var newform = test_form.filter(function () { return $(this).attr('var') === "a" || $(this).attr('type') === "text-multi"; });
		expect(test_form.submit_dom.childElementCount).toBe(3); // original untouched
		var expdom = new Strophe.Builder("x", {xmlns: "jabber:x:data", type: "form"})
		.c('field', {'var': 'a', type: 'boolean'}).c('value').t('0').up().up()
		.c('field', {'var': 'c', type: 'text-multi'}).c('value').t('1').up().c('value').t('2').tree();
		expect(DOM.treeEquals(newform.submit_dom, expdom)).toBeTruthy();
		var diff = DOM.differenceNode(expdom, newform.submit_dom);
		if (diff) {
			jasmine.log(diff[0].nodeName);
			jasmine.log(diff[2]);
			if (typeof diff[2] === "string")
				jasmine.log([diff[0][diff[2]], diff[1][diff[2]]].join(" !== "));
		}
	});

	it('can merge two forms together without overwriting', function () {
		var secondform = new Skat.Comm.XMPP.DataForm(new Strophe.Builder('x', {xmlns: 'jabber:x:data', type: 'form'})
		.c('field', {'var': 'd', type: 'text-private'}).c('value').t('foo').up().up()
		.c('field', {'var': 'c', type: 'text-single'}).c('value').t('bla').tree());
		test_form.merge(secondform);
		expect(test_form.field('a')).toBeTruthy();
		expect(test_form.field('b')).toBe('foo');
		expect($(test_form.submit_dom).find('field[var="c"]').attr('type')).toBe('text-multi');
		expect(test_form.field('c')).toEqual(['1', '2']);
		expect(test_form.field('d')).toBe('foo');
	});

	it('can merge two forms together with overwriting', function () {
		var secondform = new Skat.Comm.XMPP.DataForm(new Strophe.Builder('x', {xmlns: 'jabber:x:data', type: 'form'})
		.c('field', {'var': 'd', type: 'text-private'}).c('value').t('foo').up().up()
		.c('field', {'var': 'c', type: 'text-single'}).c('value').t('bla').tree());
		test_form.merge(secondform, true);
		expect(test_form.field('a')).toBeTruthy();
		expect(test_form.field('b')).toBe('foo');
		expect($(test_form.submit_dom).find('field[var="c"]').attr('type')).toBe('text-single');
		expect(test_form.field('c')).toBe('bla');
		expect(test_form.field('d')).toBe('foo');
	});

	it('can strip down a form for submission', function () {
		var sf = test_form.submit();
		expect(sf).toBeDefined();
		expect(jasmine.isDomNode(sf)).toBeTruthy();
		expect(sf.nodeName).toBe('x');
		expect($(sf).attr('xmlns')).toBe('jabber:x:data');
		expect($(sf).attr('type')).toBe('submit');
		// label and type attributes removed
		expect($('field[label]', sf).length).toBe(0);
		expect($('field[type]', sf).length).toBe(0);
		// no more options
		expect($('field option', sf).length).toBe(0);
		// all fields must have a var attribute
		expect($('field[var]', sf).length).toBe($('field', sf).length);
		// test_form unchanged
		expect($('field[type]', test_form.submit_dom).length).toBe(3);
	});
});