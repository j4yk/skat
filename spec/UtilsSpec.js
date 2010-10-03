describe('Skat Utils', function () {
	describe('Ring class', function () {
		var ring = null;
		var firstObj = {item: 1};
		var secondObj = {item: 2};

		beforeEach(function () {
			ring = new Ring();
			ring.push(firstObj);
			ring.push(secondObj);
		});

		it('pushes elements next to the Ring start', function () {
			ring.push('new');
			expect(ring.next()).toEqual('new');
		});

		it('supports function chaining with push', function () {
			expect(ring.push('new')).toBe(ring);
		});

		it('can cycle around', function () {
			expect(ring.next()).toBe(secondObj);
			expect(ring.next()).toBe(firstObj);
			expect(ring.next()).toBe(secondObj);
			expect(ring.prev()).toBe(firstObj);
			expect(ring.prev()).toBe(secondObj);
		});

		it('can hold null and undefined', function () {
			ring.push(null);
			ring.push(undefined);
			expect(ring.next()).not.toBeDefined();
			expect(ring.next()).toBeNull();
		});
	});

	describe('Currying', function () {
		var addition = function () {
			var sum = 0;
			for (var i = 0; i < arguments.length; i++) {
				sum += arguments[i];
			}
			return sum;
		};

		it('works', function () {
			var f = addition.curry(5, 2);
			expect(f(3, 4)).toEqual(5+2+3+4);
		});
	});

	describe('select (collection filtering)', function () {
		var set = [1, 2, 3, 4, 5, 6, {name: 'foo', type: 1}, {name: 'bar', type: 1}];
		var evenp = function () {
			return this % 2 === 0;
		}
		var isObject = function () {
			return typeof this === "object";
		}
		var isOfType1 = function () {
			return isObject(this) && this.type === 1;
		}

		it('implements select as a static function of SkatUtils', function () {
			expect(SkatUtils.select(set, evenp)).toEqual([2, 4, 6]);
			expect(SkatUtils.select(set, isObject)).toEqual(set);
			expect(SkatUtils.select(set, isOfType1)).toEqual([set[6], set[7]]);
		});

		it('adds a select method to arrays', function () {
			expect(set.select(evenp)).toEqual([2, 4, 6]);
			expect(set.select(isObject)).toEqual(set);
			expect(set.select(isOfType1)).toEqual([set[6], set[7]]);
		});

		it('adds a select method to jQuery objects which returns jQuery objects', function () {
			var jq = $(set);
			expect(jq.select(evenp)).toEqual($([2, 4, 6]));
			expect(jq.select(isObject)).toEqual($(set));
			expect(jq.select(isOfType1)).toEqual($([set[6], set[7]]));
		});
	});
});
