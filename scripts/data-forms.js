/* Skat.Comm.XMPP.DataForm objects
   provide actions (transformations) for XMPP data forms
*/
Skat.Comm.XMPP.DataForm = function (form_dom) {
	this.form_dom = form_dom;
	// working copy
	this.submit_dom = $(form_dom).clone().get(0);
}

Skat.Comm.XMPP.DataForm.prototype = {
	// get XSLT dom trees for data form transformation to and from html
	html_xslt: $.get("xslt/forms.xslt", function (xslt) {
		Skat.Comm.XMPP.DataForm.prototype.html_xslt = DOM.parse(xslt);
		Skat.Comm.XMPP.DataForm.prototype.html_xslt_ready = true;
	}),
	html_xslt_ready: false,

	form_xslt: $.get('xslt/html-to-form.xslt', function (xslt) {
		Skat.Comm.XMPP.DataForm.prototype.form_xslt = DOM.parse(xslt);
		Skat.Comm.XMPP.DataForm.prototype.form_xslt_ready = true;
	}),
	form_xslt_ready: false,

	ready: function () {
		// return true if all XSLT files are loaded
		var This = Skat.Comm.XMPP.DataForm.prototype;
		return This.html_xslt_ready && This.form_xslt_ready;
	},

	to_html: function () {
		// transform the form into an html table
		return XSLT.transform(this.form_dom, Skat.Comm.XMPP.DataForm.prototype.html_xslt);
	},

	set: function (varname, value) {
		// set the value of a form field
		var field = $(this.submit_dom).find('field[var="' + varname + '"]');
		if (field.length > 0) {
			if (typeof value === "object") { // probably array
				field.find('value').remove();
				$.map(value, function (value) {
					var elm = DOM.build('value', 'jabber:x:data').t(value).tree();
					field.append(field[0].ownerDocument.adoptNode(elm));
				});
			} else if (field.find('value').length > 0) {
				if (typeof value === "boolean") {
					field.find('value').text(value ? '1' : '0');
				} else {
					field.find('value').text(value);
				}
			} else {
				// insert value
				var val = DOM.build('value', 'jabber:x:data').t(value).tree();
				field.append(field[0].ownerDocument.adoptNode(val));
			}
		}
		field.data('done', true);
		// field.attr('done', 'done');
		return this;
	},

	fill_dialog: function (dialog) {
		$(dialog).append(this.to_html());
		$(dialog).data('form-dom', this.form_dom);
	}
}
