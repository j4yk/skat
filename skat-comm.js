Strophe.addNamespace("skat", "urn:xmpp:skat");
Strophe.addNamespace("lobby", "urn:xmpp:skat:lobby");
//Skat.NS = "urn:xmpp:skat";
//Skat.NS_LOBBY = "urn:xmpp:skat:lobby";

Skat.Comm = {
	Room: null,
	XMPP: {},   // scripts/data-forms.js

	connection: null,

	lobby: null,
	lobby_jid: "skatlobby@conference.draugr.de/tester",
	game: null,
	game_jid: null,
	game_room: null,

	is_game_owner: false,

	lobby_roster: new Object(),
	game_roster: new Object(),

	get_room: function (room_jid) {
		return Strophe.getBareJidFromJid(room_jid);
	},

	get_nick: function (room_jid) {
		return Strophe.getResourceFromJid(room_jid);
	},

	join_game_pres: function (room_jid) {
		return $pres({to: room_jid}).c("x", {xmlns: Strophe.NS.MUC}).up().c("x", {xmlns: Strophe.NS.skat}).up();
	},

	join_lobby_pres: function (room_jid) {
		return $pres({to: room_jid}).c("x", {xmlns: Strophe.NS.MUC}).up().c("x", {xmlns: Strophe.NS.lobby}).up();
	},

	enter_lobby: function (lobby_address, nick) {
		Skat.Comm.lobby = new Skat.Comm.Room();
		var lobby = $(Skat.Comm.lobby);
		lobby.bind('error-while-joining', function (ev, presence) {
			$(Skat.Comm).trigger('error-joining-lobby', presence);
		});
		lobby.bind('joined', function (ev, presence) {
			$(Skat).trigger('lobby-joined');
			if ($(presence).find('status[code="201"]').length > 0) {
				// ask if the lobby room should be created
				var dlg = $('<div id="#confirm-lobby-create-dialog">The lobby at ' + 
					Strophe.getBareJidFromJid($(presence).attr('from')) + 
					' does not exist yet.  Create it?</div>');
				dlg.dialog({
					autoOpen: true,
					title: 'Lobby does not exist',
					buttons: {
						'Create lobby': function () {
							Skat.Comm.lobby.configure();
							$(this).dialog('close');
						},
						'Abort': function () {
							Skat.Comm.lobby.leave();
							$(this).dialog('close');
						}
					}
				});
			}
		});
		lobby.bind('user-joined', function (ev, presence) {
			$(Skat).trigger('user-joined-lobby', {name: nick, status: $(presence).find('status').text()});
		});
		Skat.Comm.lobby.join(lobby_address + "/" + nick, null, Skat.Comm.join_lobby_pres(lobby_address + "/" + nick)); 
	},


	create_game: function (game_jid) {
		// attempt to open a new MUC room for a fresh game
		Skat.Comm.game_jid = game_jid;
		Skat.Comm.game_room = Skat.Comm.get_room(game_jid);
		Skat.Comm.is_game_owner = true;
		// add handler for presence from this room
		var gamePresenceHandler = Skat.Comm.connection.addHandler(Skat.Comm.on_game_presence, null, "presence", null, null, game_jid, {matchBare: true});
		// send presence to game room
		var pres = Skat.Comm.join_game_pres(game_jid);
		Skat.Comm.connection.send(pres);
	},

	set_form_field: function (form, fieldvar, value) {
		form.set(fieldvar, value);
		/*var field = form.find('field[var="' + fieldvar + '"]');
		if (field.length > 0) {
			if (typeof value === "object") { // probably array
				field.find('value').remove();
				$.map(value, function (value) {
					field.append('<value>' + value + '</value>');
				});
			} else if (field.find('value').length > 0) {
				field.find('value').text(value);
			} else {
				// insert value
				field.append('<value>' + value + '</value>');
			}
		}
		field.data('done', true);
		field.attr('done', 'done');
		return field;*/
	},

	set_roomconfig_form_field: function (form, varname, value) {
		Skat.Comm.set_form_field(form, 'muc#roomconfig_' + varname, value);
	},

	// fills the dialog element with elements that can be used to fill out the form
	// @form:  DOM node that contains the fields as children
	build_dialog_from_fields: function (dialog, form, form_dom) {
		new Skat.Comm.XMPP.DataForm(form_dom || form).fill_dialog(dialog);
	},

	configure_room: function (options) {
		// fill in some options automatically and present the rest to the user
		// finally send the configuration form
		var options_jquery = options;
		var options = new Skat.Comm.XMPP.DataForm(options);
		$.map([['allowinvites', 0], ['changesubject', 0], ['enablelogging', 0], ['getmemberlist', ['moderator', 'participant']],
			['pubsub', ''], ['moderatedroom', 0], ['passwordprotectedroom', 0], ['persistentroom', 0], 
			['presencebroadcast', ['moderator', 'participant', 'visitor']], ['publicroom', 0], ['roomadmins', [Skat.Comm.connection.jid]],
			['roomdesc', 'Skat game'], ['roomowners', [Skat.Comm.connection.jid]], ['roomsecret', ''], ['whois', []]],
			function (option_value_pair) {
				Skat.Comm.set_roomconfig_form_field(options, option_value_pair[0], option_value_pair[1]);
			});
		var config_dialog = $('#game-config-dialog');
		config_dialog.empty();
		var dataform = options;
		options = options_jquery;
		// insert the option for members-only access manually
		if (options.find('field[var="muc#roomconfig_membersonly"]').length > 0) {
			options.find('field[var="muc#roomconfig_membersonly"]').data('done', true);
		}
		// and the rest of the options automatically
		var fields = options.find('field[var][type!="hidden"]').select(function () {
			return !$(this).data('done'); // return true if done is false or undefined
		});
		var form = $('<x xmlns="jabber:x:data" type="query"></x>').append(fields);
		Skat.Comm.build_dialog_from_fields(config_dialog, form.get(0), //options);
			dataform);
		// now prepend the members-only option
		if (options.find('field[var="muc#roomconfig_membersonly"]').length > 0) {
			config_dialog.find('tr:first').before('<tr><td><label>Require others to be approved by you</label></td><td><input id="muc#roomconfig_membersonly" type="checkbox"></td></tr>');
		}
		// show the configuration dialog
		config_dialog.dialog('open');
	},

	register_with_game: function (game_jid) {
		// attempt to join the MUC room of this game
		Skat.Comm.game_jid = game_jid;
		Skat.Comm.game_room = Skat.Comm.get_room(game_jid);
		Skat.Comm.is_game_owner = false;
		// add handler for presence from this room
		var gamePresenceHandler = Skat.Comm.connection.addHandler(Skat.Comm.on_game_presence, null, "presence", null, null, game_jid, {matchBare: true});
		// discover if room needs registration
		Skat.Comm.connection.sendIQ($iq({to: game_room, type: 'get'}).c('query', {xmlns: Strophe.NS.DISCO_INFO}), function (iq) {
			// reply
			if (iq.find("feature[var='muc_membersonly']").length > 0) {
				Skat.Comm.send_registration_request(game_room);
			} else {
				Skat.Comm.connection.send(Skat.Comm.join_game_pres(game_jid));
			}
		}, function () {
			// timeout
			$(Skat).trigger('error-joining-game', {type: 'error', reason: 'timeout', stage: 'query'});
		}, 10000);
	},

	send_registration_request: function (game_jid, timeout) {
		// try to register with the room and thus become a member
		Skat.Comm.connection.sendIQ($iq({to: Strophe.getBareJidFromJid(game_jid), type: 'get'}).c('query', {xmlns: 'jabber:iq:register'}), function (iq) {
			if ($(iq).attr('type') === 'error') {
				// error IQ result
				$(Skat.Comm).trigger('error-joining-game', iq);
			} else if ($(iq).attr('type') === 'result') {
				if ($(iq).find('register').length > 0) {
					// already registered, we should never really get here
					Skat.Comm.connection.send(Skat.Comm.join_game_pres(game_jid));
				} else if ($(iq).find('query').length > 0) {
					// registration form
					// TODO: let user fill out registration form or do it automatically
					// TODO: send the filled in form and process the result
					var form = $(iq).find('x[xmlns="jabber:x:data"][type="form"]').clone();
					Skat.Comm.process_registration_form(form);
				}
			}
		}, function () {
			// timeout
			$(Skat.Comm).trigger('error-joining-game', {type: 'error', reason: 'timeout', stage: 'registration-request'});
		}, timeout || 60000);
	},

	process_registration_form: function (form, nickname) {
		$.map([['email', ''], ['faqentry', ''], ['first', nickname], ['last', ''], ['roomnick', nickname], ['url', '']],
			function (option_value_pair) {
				var field = Skat.Comm.set_form_field(form, 'muc#register_' + option_value_pair[0], option_value_pair[1]);
				field.attr('done', 'done');
			});
		var morefields = form.find('field[done!="done"]');
		if (morefields.length > 0) {
			// build a dialog from more fields
			var dlg = $('#register-options-dialog');
			dlg.empty();
			Skat.Comm.build_dialog_from_fields(dlg, morefields, form);
			dlg.dialog('open');
		}
	},

	on_game_presence: function (pres) {
		var from = $(pres).attr('from');
		var nick = Strophe.getResourceFromJid(from);

		if ($(pres).attr('type') === 'error' && !Skat.game_joined) {
			// error joining game
			$(Skat.Comm).trigger('error-joining-game', pres);
		} else if (!Skat.Comm.game_roster[nick] && $(pres).attr('type') !== 'unavailable') {
			// add user to game roster
			Skat.Comm.game_roster[nick] = true;
			$(Skat).trigger('user-joined-game', {name: nick});
		}

		// check for our own presence
		if (from === Skat.Comm.game_jid) {
			if ($(pres).attr('type') === 'unavailable') {
				// we left the room...
				return false;  // don't need this handler anymore
			}
		}
		if ($(pres).attr('type') !== 'error' && !Skat.game_joined) {
			// this is indicated by status 110 or 201 if the room is freshly created
			if ($(pres).find('status[code="110"], status[code="201"]').length > 0) {
				// check if the server changed our nick
				if ($(pres).find("status[code='210']").length > 0) {
					Skat.Comm.lobby_jid = from;
				}

				if ($(pres).find("status[code='201']").length > 0) {
					// room created
					// game successfully joined
					$(Skat).trigger('game-joined');
				} else {
					// room did already exist apparently
					$(Skat.Comm).trigger('error-joining-game', {type: 'error', reason: 'room-exists', stage: 'joined'});
				}
			}
		}

		return true;
	}
}


$(function () {
	// load the xmpp console
	$('#console').load('console.html #console', function () { $(document).trigger('console-loaded'); });

	$(document).bind('connect', function (ev, login_data) {
		// create the connection
		conn = new Strophe.Connection("https://www.draugr.de/http-bind");
		$(document).trigger('connection-created', conn);
		conn.connect(login_data.jid, login_data.password, function (status) {
			$(document).trigger('connection-status-changed', status);
		});
	});

	$(document).bind('connection-status-changed', function (ev, status) {
		if (status === Strophe.Status.CONNECTED) {
			$(document).trigger('connected', conn);
		} else if (status === Strophe.Status.DISCONNECTED) {
			$(document).trigger('disconnected', conn);
		}
	});

	$(document).bind('connected', function (ev, conn) {
		// connection established and useable
		Skat.Comm.connection = conn;
		conn.send($pres().c('priority').t('-1'));
	});

	$(Skat).bind('game-joined', function () {
		// received own presence in the game MUC room
		if (Skat.Comm.is_game_owner) {
			// configure the room
			Skat.Comm.connection.sendIQ($iq({to: Skat.Comm.game_room, type: 'get'}).c('query', {xmlns: Strophe.NS.MUC + '#owner'}), function (iq) {
				// configuration form
				if ($(iq).attr('type') === 'result' && $(iq).find("x[xmlns='jabber:x:data'][type='form']").length > 0) {
					// reuse the form elements to fill out the form
					Skat.Comm.configure_room($(iq).find("x[xmlns='jabber:x:data'][type='form']").clone());
				} else {
					$(Skat.Comm).trigger('error-creating-game', {type: 'error', reason: 'no-creation-form', stage: 'room-creation'});
				}
			}, function () {
				// timeout
				$(Skat.Comm).trigger('error-creating-game', {type: 'error', reason: 'timeout', stage: 'room-creation'});
			}, 10000);
		} else {
			// TODO: something to do here?
		}
	});
	
	$(Skat).bind('game-config-approved', function () {
		// the user accepted the options for MUC room creation for his own game
		// continue the process of game creation
		// clean the data-form and submit it
		options = $('#game-creation-dialog').data('form-dom'); // data-form DOM
		options.find('field :not(value)').remove();  // remove everything except values
		options.find('field').removeAttr('type').removeAttr('label');
		options.attr('type', 'submit');
		// send the form
		Skat.Comm.connection.sendIQ(
			$($iq({to: Skat.Comm.game_room}).c('query', {xmlns: Strophe.NS.MUC + '#owner'}).tree()).append(options),
			function (iq) {
				if ($(iq).attr('type') !== 'result') {
					$(Skat.Comm).trigger('error-creating-game', iq);
				}
			}, function () {
				// timeout
				$(Skat.Comm).trigger('error-creating-game', {type: 'error', reason: 'timeout', stage: 'send-room-config'});
			}, 10000);
	});

	$(Skat).bind('game-creation-aborted', function () {
		// cancel the room configuration form
		Skat.Comm.connection.send($iq({to: Skat.Comm.game_room, type: 'set'}).c('query', {xmlns: Strophe.NS.MUC + '#owner'}).c('x', {xmlns: 'jabber:x:data', type: 'cancel'}));
		Skat.Comm.connection.send($pres({to: Skat.Comm.game_room, type: 'unavailable'}));
		// TODO: change presence in lobby?
	});

	$(Skat).bind('registration-options-approved', function () {
		// TODO: fill the dialog values into the form
		// TODO: submit the form
	});

	$(Skat).bind('registration-cancelled', function () {
		// TODO: cancel the registration attempt
		// TODO: return to lobby
	});

	// prepare login-dialog
	var tbl = $('<table />');	
	var jidr = $('<tr><td><label>JID: </label></td><td><input type="text" name="jid"></td></tr>');
	var pwr = $('<tr><td><label>Password: </label></td><td><input type="password" name="password"></td></tr>');
	tbl.append(jidr).append(pwr).appendTo('#login-dialog');

	$('#login-dialog').dialog({
		autoOpen: false,
		width: "700px",
		buttons: {
			"Login": function () {
				$(document).trigger('connect', {
					jid: $(this).find('input[name="jid"]').val(),
					password: $(this).find('input[name="password"]').val()
				});
				$(this).dialog('close');
			}
		}
	});

	$(document).bind('console-loaded', function () {
		setTimeout(function () { $('#login-dialog').dialog('open'); }, 200);
	});
});
