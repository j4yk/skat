/* Skat.Comm.Room
   implements generic MUC room actions

   triggers the following events on itself:
   - created               created the room
   - joined                successfully joined the room
   - left                  left the room
   - nick-changed          nickname has changed
   - error                 an error was send back
   - error-while-joining   an error occurred when trying to join
   - presence              some presence arrived
   - user-joined           a user joined the room
   - user-left             a user left the room
   - room-configured       successfully configured the room
   - error-while-configuring   an error was send back when the configuration form should be received
*/
Skat.Comm.Room = function () {
	this.room_jid = null;
	this.nick = null;
	this.my_jid = null;

	this.joined = false;
	this.roster = {};
};

Skat.Comm.Room.prototype.join = function (my_jid, password, initial_presence) {
	/// attempts to join a room by sending initial presence
	/// sets room_jid, nick and my_jid
	this.room_jid = Strophe.getBareJidFromJid(my_jid);
	this.nick = Strophe.getResourceFromJid(my_jid);
	this.my_jid = my_jid;
	var room = this;
	Skat.Comm.connection.addHandler(function (presence) {
		return room.presence_handler(presence);
	}, null, 'presence', null, null, this.room_jid, {matchBare: true});
	var pres = null;
	if (!initial_presence) {
		pres = $pres({to: this.my_jid}).c("x", {xmlns: Strophe.NS.MUC});
		if (password) {
			pres.c("password").t(password); // append password element to x element
		}
	}
	Skat.Comm.connection.send(initial_presence || pres);
};

Skat.Comm.Room.prototype.leave = function () {
	// leave the room by sending unavailable presence
	Skat.Comm.connection.send($pres({to: this.my_jid, type: 'unavailable'}));
};

Skat.Comm.Room.prototype.configure = function (enforced_values, default_values, create_instant_room) {
	// request the room configuration form
	if (create_instant_room) {
		// send an empty form
		Skat.Comm.connection.sendIQ($iq({to: this.room_jid, type: 'set'}).c('query', {xmlns: Strophe.NS.MUC + "#owner"}).
			c('x', {xmlns: 'jabber:x:data', type: 'submit'}), function (iq) {
			// success
			$(this).trigger('room-configured');
		}, function (iq) {
			// error
			$(this).trigger('error-while-configuring', { reason: iq === null ? 'timeout' : iq });
		}, 10000);
	} else {
		// get the configuration form
		Skat.Comm.connection.sendIQ($iq({to: this.room_jid, type: 'get'}).c('query', {xmlns: Strophe.NS.MUC + "#owner"}), function (iq) {
			// form retrieved
			if ($(iq).attr('type' === 'result') && $(iq).find('x[xmlns="jabber:x:data"][type="form"]').length > 0) {
				// prefill form and display the rest to the user
			} else {
				$(this).trigger('error-while-configuring', { reason: 'no configuration form' });
			}
		}, function (iq) {
			// error
			$(this).trigger('error-while-configuring', { reason: iq === null ? 'timeout' : iq });
		}, 10000);
	}
};

Skat.Comm.Room.prototype.presence_handler = function (presence) {
	/// this is the default presence handler for MUC rooms that are managed by Skat.Comm.Room
	var from = $(presence).attr('from');
	var type = $(presence).attr('type');

	if ($(presence).find('status[code="210"]').length > 0) {
		// server changed our nick
		this.my_jid = from;
		$(this).trigger('nick-changed', presence);
	}

	if (type === 'error') {
		if (!this.joined) {
			$(this).trigger('error-while-joining', presence);
		} else {
			$(this).trigger('error', presence);
		}
	} else if (from === this.my_jid || $(presence).find('status[code="110"]').length > 0) {
		// our own presence
		if (type === 'unavailable') {
			// officially logged out
			this.joined = false;
			$(this).trigger('left', presence);
			return false; // hence, this handler is obsolete
		} else if ($(presence).find('status[code="110"], status[code="201"]').length > 0) {
			if (!this.joined) {
				this.joined = true;
				if ($(presence).find('status[code="201"]').length > 0) {
					$(this).trigger('created', presence);
				} else {
					$(this).trigger('joined', presence);
				}
			}
		}
	} else {
		// someone else's presence
		var nick = Strophe.getResourceFromJid(from);
		if (type === 'unavailable') {
			delete this.roster[nick];
			$(this).trigger('user-left', {nick: nick, jid: from, presence: presence});
		} else {
			if (!this.roster[nick]) {
				this.roster[nick] = presence;
				$(this).trigger('user-joined', presence);
			} else {
				this.roster[nick] = presence;
				$(this).trigger('user-presence', presence);
			}
		}
	}
	return true;
};
