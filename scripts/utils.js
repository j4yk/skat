// Ring class

function Ring() {
	this.delim = Math.random();
	this.items = [this.delim];
}

Ring.prototype = {
	delim: null,
	items: [null],

	_move_next: function () {
		var x = this.items.shift();
		this.items.push(x);
	},

	_move_prev: function () {
		var x = this.items.pop();
		this.items.unshift(x);
	},

	next: function () {
		this._move_next();
		var l = this.items.length;
		var c = 1;
		while (c < l && this.items[0] === this.delim) {
			this._move_next();
			c++;
		}
		return this.items[0] === this.delim ? null : this.items[0];
	},

	prev: function () {
		this._move_prev();
		var l = this.items.length;
		var c = 1;
		while (c < l && this.items[0] === this.delim) {
			this._move_prev();
			c++;
		}
		return this.items[0] === this.delim ? null : this.items[0];
	},

	/// Rewinds the ring until the delimter is first, 
	/// i. e. the last pushed element will be returned upon next()
	rewind: function () {
		var l = this.items.length;
		var c = 0;
		while (c < l && this.items[0] !== this.delim) {
			this._move_prev();
		}
		// delim is at [0] now
	},

	push: function (x) {
		this.rewind();
		// insert x AFTER the delimter
		this.items.splice(1, 0, x);
		return this;
	}
}

// Functional programming

Function.prototype.curry = function () {
	var fn = this;
	var args = Array.prototype.slice.apply(arguments);
	return function () {
		return fn.apply(this, args.concat(Array.prototype.slice.apply(arguments)));
	}
}

Array.prototype.select = function (condition) {
	return SkatUtils.select(this, condition);
}

jQuery.prototype.select = function (condition) {
	return $(SkatUtils.select(this, condition));
}

SkatUtils = {
	select: function (collection, condition) {
		if (!condition || typeof condition !== "function") {
			throw('No condition function supplied to Skat.Utils.select()');
		}
		if (!collection.length) {
			throw('No valid collection (needs a length attribute) supplied to Skat.Utils.select()');
		}
		var r = [];
		for (var i = 0; i < collection.length; i++) {
			if (condition.call(collection[i])) {
				r.push(collection[i]);
			}
		}
		return r;
	}
}
