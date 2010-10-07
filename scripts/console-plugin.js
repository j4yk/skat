Console = {
	connection: null,

	inputHistory: new Ring(),

	log: function (body) {
		var log = $('#log').get(0);
		var atBottom = log.scrollTop >= log.scrollHeight - log.clientHeight;
		$('#log').append(body);
		if (atBottom) {
			log.scrollTop = log.scrollHeight;
		}
	},

	prettyXml: function (xml, level) {
		var i, j;
		var result = [];
		if (!level) {
			level = 0;
		}

		result.push("<div class='xml_level" + level + "'>");
		result.push("<span class='xml_punc'>&lt;</span>");
		result.push("<span class='xml_tag'>");
		result.push(xml.tagName);
		result.push("</span>");

		var attrs = xml.attributes;
		var attr_lead = [];
		for (i = 0; i < xml.tagName.length + 1; i++) {
			attr_lead.push("&nbsp;");
		}
		attr_lead = attr_lead.join("");

		for (i = 0; i < attrs.length; i++) {
			result.push(" <span class='xml_aname'>");
			result.push(attrs[i].nodeName);
			result.push("</span><span class='xml_punc'>='</span>");
			result.push("<span class='xml_avalue'>");
			result.push(attrs[i].nodeValue);
			result.push("</span><span class='xml_punc'>'</span>");

			if (i !== attrs.length - 1) {
				result.push("</div><div class='xml_level" + level + "'>");
				result.push(attr_lead);
			}
		}
		
		if (xml.childNodes.length === 0) {
			result.push("<span class='xml_punc'>/&gt;</span></div>");
		} else {
			result.push("<span class='xml_punc'>&gt;</span></div>");

			$.each(xml.childNodes, function () {
				if (this.nodeType === 1) {
					result.push(Console.prettyXml(this, level + 1));
				} else if (this.nodeType === 3) {
					result.push("<div class='xml_text xml_level" + (level + 1) + "'>");
					result.push(this.nodeValue);
					result.push("</div>");
				}
			});

			result.push("<div class='xml xml_level" + level + "'>");
			result.push("<span class='xml_punc'>&lt;/</span>");
			result.push("<span class='xml_tag'>");
			result.push(xml.tagName);
			result.push("</span>");
			result.push("<span class='xml_punc'>&gt;</span></div>");
		}

		return result.join("");
	},

	show_traffic: function (body, class) {
		$.each(body.childNodes, function () {
			Console.log("<div class='" + class + "'>" + Console.prettyXml(this) + "</div>");
		});
	},

	text_to_xml: function (text) {
		var doc = null;
		if (window['DOMParser']) {
			var parser = new DOMParser();
			doc = parser.parseFromString(text, 'text/xml');
		} else if (window['ActiveXObject']) {
			doc = new ActiveXObject("MSXML2.DOMDocument");
			doc.async = false;
			doc.loadXML(text);
		} else {
			throw {
				type: 'ConsoleError',
				message: 'No DOMParser object found.'
			}
		}

		var elem = doc.documentElement;
		if ($(elem).filter('parsererror').length > 0) {
			return null;
		}
		return elem;
	}
}

$(function () {
	$(document).bind('connection-created', function (ev, conn) {
		Console.connection = conn;
		conn.xmlInput = function (body) {
			Console.show_traffic(body, 'incoming');
		};
		conn.xmlOutput = function (body) {
			Console.show_traffic(body, 'outgoing');
		};
	});

	$(document).bind('connection-status-changed', function (ev, status) {
		Console.log('<p>Status: ' + status + '</p>');
	});

	$(document).bind('disconnect', function () {
		$('#input, #button-bar button').attr('disabled', 'disabled');
	});

	$(document).bind('connected', function (ev, connection) {
		$('#input, #button-bar button').removeAttr('disabled');
		$('#input').focus();
	});

	$(document).bind('console-send', function (ev, send_input) {
		var input = send_input;
		var customInput = true;
		if (!input) {
			customInput = false;
			input = $('#input').val();
			$('#input').css({backgroundColor: "#fff"});
		}
		var error = false;
		if (input.length > 0) {
			if (input[0] === '<') {
				var xml = Console.text_to_xml(input);
				if (xml) {
					Console.connection.send(xml); 
					if (!customInput) { $('#input').val(''); }
				} else {
					error = true;
				}
			} else {
				try {
					var builder = eval(input);
					Console.connection.send(builder);
					if (!customInput) { $('#input').val(''); }
				} catch (e) {
					error = true;
				}
			}
		}

		if (error) {
			if (customInput) {
				alert('Konnte "' + input + '" nicht senden!');
			} else {
				$('#input').animate({backgroundColor: "#faa"});
			}
		} else if (!customInput) {
			Console.inputHistory.push(input);
		}
	});
});

$(document).bind('console-loaded', function () {
	$('#send-button').click(function () {
		$(document).trigger('console-send');
	});

	$('#input').keypress(function (ev) {
		if (ev.which === 10 && ev.ctrlKey) {
			$(document).trigger('console-send');
			ev.preventDefault();
		}
	});

	$('#input').keydown(function (ev) {
		if (ev.which === 38) {
			$('#input').val(Console.inputHistory.prev());
			ev.preventDefault();
		} else if (ev.which === 40) {
			$('#input').val(Console.inputHistory.next());
			ev.preventDefault();
		}
	});

	$('#disconnect-button').click(function () {
		Console.connection.send($pres({type: "unavailable"}));
		$(document).trigger('disconnect');
		setTimeout(function () { Console.connection.disconnect(); }, 2000);
	});

	$('#log > div').live('click', function () {
		$(this).toggleClass('folded');
	});
});
