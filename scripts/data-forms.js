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
	html_xslt: $.get("xslt/forms.xsl", function (xslt) {
		Skat.Comm.XMPP.DataForm.prototype.html_xslt = xslt;
		Skat.Comm.XMPP.DataForm.prototype.html_xslt_ready = true;
	}, "xml"),
	html_xslt_ready: false,

	form_xslt: $.get('xslt/html-to-form.xsl', function (xslt) {
		Skat.Comm.XMPP.DataForm.prototype.form_xslt = xslt;
		Skat.Comm.XMPP.DataForm.prototype.form_xslt_ready = true;
	}, "xml"),
	form_xslt_ready: false,

	ready: function () {
		// return true if all XSLT files are loaded
		var This = Skat.Comm.XMPP.DataForm.prototype;
		return This.html_xslt_ready && This.form_xslt_ready;
	},

	to_html: function () {
		// transform the form into an html table
		return XSLT.transform(this.form_dom.ownerDocument, Skat.Comm.XMPP.DataForm.prototype.html_xslt);
	},

	set: function (varname_or_object, value) {
		if (typeof varname_or_object === "string") {
			var varname = varname_or_object;
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
		} else if (typeof varname_or_object === "object") {
			var values = varname_or_object;
			// values contains an object which shall be used as key-value store
			for (var k in values) {
				if (values.hasOwnProperty(k)) {
					// set the values one after another
					this.set(k, values[k]);
				}
			}
			return this;
		}
	},

	field: function (varname, value) {
		if (value !== undefined) {
			this.set(varname, value);
		} else {
			// get this field
			var field = $(this.submit_dom).find('field[var="' + varname + '"]');
			if (field.length > 0) {
				var values = field.find('value');
				if (values.length === 1) {
					return values.text();
				} else if (values.length > 1) {
					return $.map(values, function (value) { return $(value).text(); });
				} else {
					return null;
				}
			}
		}
	},

	fill_dialog: function (dialog) {
		$(dialog).append(this.to_html());
		$(dialog).data('form-dom', this.form_dom);
	},

	add_field: function (name, type, value) {
		if ($(this.submit_dom).find('field[var="' + name + '"]').length === 0) {
			var field = new Strophe.Builder("field", {"var": name, type: type});
			if (typeof value === "object") {
				$.map(value, function (value) {
					field.c("value").t("" + value).up();
				});
			} else {
				field.c("value").t("" + value).up();
			}
			$(this.submit_dom).find('x[xmlns="jabber:x:data"]').append(field.toString());
		} else {
			throw "Skat.Comm.XMPP.DataForm.add_field: field exists.";
		}
		return this;
	},

	/// Return a form which contains a subset of the fields in this form
	filter: function (filter) {
		if (typeof filter === "function") {
			var newform = new Skat.Comm.XMPP.DataForm($(this.submit_dom).clone()[0]);
			$(newform.submit_dom).find('field[var]').filter(function (index) {
				return !filter.call(this, index);
			}).remove();
			return newform;
		} else {
			throw "Skat.Comm.XMPP.DataForm.filter:  not implemented for typeof filter !== function"
		}
	},

	/// Insert fields from other form into this form
	merge: function (otherform, overwrite) {
		var thisform = $(this.submit_dom);
		$(otherform.submit_dom).find('field[var]').each(function () {
			var thisfield = thisform.find('field[var="' + $(this).attr('var') + '"]');
			if (thisfield.length > 0) {
				if (overwrite) {
					thisfield.remove();
					thisform.append($(this).clone());
				}
			} else {
				thisform.append($(this).clone());
			}
		});
	}
}
