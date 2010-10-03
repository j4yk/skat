Skat = {
	Comm: null,

	lobby_entered: false,
	game_joined: false
}

$(function () {
	$(document).bind('disconnected', function () {
		$('#login-dialog').dialog('open');
	});

	$('#game-config-dialog').dialog({
		autoOpen: false,
		title: 'Game configuration',
		width: '800px',
		buttons: {
			'Create game': function () {
				$(Skat).trigger('game-config-approved');
				$(this).dialog('close');
			},
			'Return to lobby': function () {
				$(Skat).trigger('game-creation-aborted');
				$(this).dialog('close');
			}
		}
	});

	$('#register-options-dialog').dialog({
		autoOpen: false,
		title: 'Join options',
		width: '800px',
		buttons: {
			'Join': function () {
				$(Skat).trigger('registration-options-approved');
			},
			'Return to lobby': function () {
				$(Skat).trigger('registration-cancelled');
			}
		}
	});
});
