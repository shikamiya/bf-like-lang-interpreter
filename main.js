(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.cz.az === region.cU.az)
	{
		return 'on line ' + region.cz.az;
	}
	return 'on lines ' + region.cz.az + ' through ' + region.cU.az;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**_UNUSED/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.dT,
		impl.d3,
		impl.d$,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		G: func(record.G),
		cB: record.cB,
		cw: record.cw
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.G;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.cB;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.cw) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.dT,
		impl.d3,
		impl.d$,
		function(sendToApp, initialModel) {
			var view = impl.d6;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.dT,
		impl.d3,
		impl.d$,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aD && impl.aD(sendToApp)
			var view = impl.d6;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.dH);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.d2) && (_VirtualDom_doc.title = title = doc.d2);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.dX;
	var onUrlRequest = impl.dY;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aD: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.dn === next.dn
							&& curr.cZ === next.cZ
							&& curr.di.a === next.di.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		dT: function(flags)
		{
			return A3(impl.dT, flags, _Browser_getUrl(), key);
		},
		d6: impl.d6,
		d3: impl.d3,
		d$: impl.d$
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { dQ: 'hidden', dI: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { dQ: 'mozHidden', dI: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { dQ: 'msHidden', dI: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { dQ: 'webkitHidden', dI: 'webkitvisibilitychange' }
		: { dQ: 'hidden', dI: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		du: _Browser_getScene(),
		dE: {
			cd: _Browser_window.pageXOffset,
			ce: _Browser_window.pageYOffset,
			ao: _Browser_doc.documentElement.clientWidth,
			ae: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		ao: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		ae: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			du: {
				ao: node.scrollWidth,
				ae: node.scrollHeight
			},
			dE: {
				cd: node.scrollLeft,
				ce: node.scrollTop,
				ao: node.clientWidth,
				ae: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			du: _Browser_getScene(),
			dE: {
				cd: x,
				ce: y,
				ao: _Browser_doc.documentElement.clientWidth,
				ae: _Browser_doc.documentElement.clientHeight
			},
			dM: {
				cd: x + rect.left,
				ce: y + rect.top,
				ao: rect.width,
				ae: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}var author$project$Main$ParseTokens = {$: 19};
var author$project$BFTypes$NextCommand = 0;
var author$project$BFTypes$NotRunning = 0;
var author$project$BFTypes$BFTape = elm$core$Basics$identity;
var author$project$BFTypes$tapePages = 128;
var elm$core$Basics$EQ = 1;
var elm$core$Basics$GT = 2;
var elm$core$Basics$LT = 0;
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$mul = _Basics_mul;
var author$project$BFTypes$tapeSize = (16 * 16) * author$project$BFTypes$tapePages;
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.f) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.h),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.h);
		} else {
			var treeLen = builder.f * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.i) : builder.i;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.f);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.h) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.h);
		}
	});
var elm$core$Basics$False = 1;
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{i: nodeList, f: (len / elm$core$Array$branchFactor) | 0, h: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			elm$core$Array$initialize,
			n,
			function (_n0) {
				return e;
			});
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var author$project$BFTypes$initialBFTape = A2(elm$core$Array$repeat, author$project$BFTypes$tapeSize, 0);
var elm$core$Basics$True = 0;
var elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, list);
			var jsArray = _n0.a;
			var remainingItems = _n0.b;
			if (_Utils_cmp(
				elm$core$Elm$JsArray$length(jsArray),
				elm$core$Array$branchFactor) < 0) {
				return A2(
					elm$core$Array$builderToArray,
					true,
					{i: nodeList, f: nodeListSize, h: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					elm$core$List$cons,
					elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return elm$core$Array$empty;
	} else {
		return A3(elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var elm$core$Maybe$Nothing = {$: 1};
var author$project$BFTypes$initialExecutorParams = {
	a7: elm$core$Array$fromList(_List_Nil),
	a8: _List_Nil,
	cQ: 0,
	bd: elm$core$Maybe$Nothing,
	dU: '',
	aw: 0,
	S: 0,
	de: _List_Nil,
	z: 0,
	A: author$project$BFTypes$initialBFTape,
	w: 0
};
var author$project$BFTypes$NoOp = 0;
var author$project$BFTypes$DecreasePointer = 6;
var author$project$BFTypes$DecreaseValue = 4;
var author$project$BFTypes$IncreasePointer = 5;
var author$project$BFTypes$IncreaseValue = 3;
var author$project$BFTypes$LoopEnd = 2;
var author$project$BFTypes$LoopStart = 1;
var author$project$BFTypes$PrintOutput = 8;
var author$project$BFTypes$ReadInput = 7;
var elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var author$project$BFTypes$tokenKindFromInt = function (value) {
	switch (value) {
		case 0:
			return elm$core$Maybe$Just(0);
		case 1:
			return elm$core$Maybe$Just(5);
		case 2:
			return elm$core$Maybe$Just(6);
		case 3:
			return elm$core$Maybe$Just(3);
		case 4:
			return elm$core$Maybe$Just(4);
		case 5:
			return elm$core$Maybe$Just(8);
		case 6:
			return elm$core$Maybe$Just(7);
		case 7:
			return elm$core$Maybe$Just(1);
		case 8:
			return elm$core$Maybe$Just(2);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 1) {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$json$Json$Decode$list = _Json_decodeList;
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Main$decodeTokenTable = A3(
	elm$json$Json$Decode$map2,
	elm$core$Tuple$pair,
	A2(
		elm$json$Json$Decode$field,
		'tokens',
		elm$json$Json$Decode$list(
			A3(
				elm$json$Json$Decode$map2,
				elm$core$Tuple$pair,
				A2(
					elm$json$Json$Decode$field,
					'kind',
					A2(
						elm$json$Json$Decode$map,
						A2(
							elm$core$Basics$composeL,
							elm$core$Maybe$withDefault(0),
							author$project$BFTypes$tokenKindFromInt),
						elm$json$Json$Decode$int)),
				A2(elm$json$Json$Decode$field, 'value', elm$json$Json$Decode$string)))),
	A2(elm$json$Json$Decode$field, 'name', elm$json$Json$Decode$string));
var author$project$Language$BF$table = _Utils_Tuple2(
	_List_fromArray(
		[
			_Utils_Tuple2(1, '['),
			_Utils_Tuple2(2, ']'),
			_Utils_Tuple2(3, '+'),
			_Utils_Tuple2(4, '-'),
			_Utils_Tuple2(5, '>'),
			_Utils_Tuple2(6, '<'),
			_Utils_Tuple2(7, ','),
			_Utils_Tuple2(8, '.')
		]),
	'BF');
var author$project$Main$defaultTokenTable = author$project$Language$BF$table;
var author$project$Language$HogyLang$table = _Utils_Tuple2(
	_List_fromArray(
		[
			_Utils_Tuple2(1, 'ﾎｷﾞｨ！ﾎｷﾞｨ〜'),
			_Utils_Tuple2(2, 'ﾎｷﾞｨ〜ﾎｷﾞｨ！'),
			_Utils_Tuple2(3, 'ﾎｷﾞｨ…ﾎｷﾞｨ…'),
			_Utils_Tuple2(4, 'ﾎｷﾞｨ！ﾎｷﾞｨ！'),
			_Utils_Tuple2(5, 'ﾎｷﾞｨ…ﾎｷﾞｨ〜'),
			_Utils_Tuple2(6, 'ﾎｷﾞｨ〜ﾎｷﾞｨ…'),
			_Utils_Tuple2(7, 'ﾎｷﾞｨ…ﾎｷﾞｨ！'),
			_Utils_Tuple2(8, 'ﾎｷﾞｨ！ﾎｷﾞｨ…')
		]),
	'Hogy Lang');
var author$project$Main$ShowBFTapeAsInt = 0;
var rundis$elm_bootstrap$Bootstrap$Popover$State = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Popover$initialState = {
	ck: {
		cs: 0,
		bv: 0,
		cx: {ae: 0, Q: 0, X: 0, ao: 0}
	},
	ag: false
};
var author$project$Main$initialCommandPopoverState = {bK: _List_Nil, U: rundis$elm_bootstrap$Bootstrap$Popover$initialState};
var rundis$elm_bootstrap$Bootstrap$Dropdown$Closed = 2;
var rundis$elm_bootstrap$Bootstrap$Dropdown$State = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$Area = F4(
	function (top, left, width, height) {
		return {ae: height, Q: left, X: top, ao: width};
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$initialState = {
	aA: A4(rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$Area, 0, 0, 0, 0),
	r: 2,
	b3: A4(rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$Area, 0, 0, 0, 0)
};
var author$project$Main$initialTokenTableState = {N: rundis$elm_bootstrap$Bootstrap$Dropdown$initialState, p: author$project$Main$defaultTokenTable};
var rundis$elm_bootstrap$Bootstrap$Modal$Hide = 3;
var rundis$elm_bootstrap$Bootstrap$Modal$hidden = 3;
var rundis$elm_bootstrap$Bootstrap$Tab$Showing = 2;
var rundis$elm_bootstrap$Bootstrap$Tab$State = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Tab$initialState = {aT: elm$core$Maybe$Nothing, j: 2};
var author$project$Main$initialModel = {
	aU: rundis$elm_bootstrap$Bootstrap$Modal$hidden,
	x: author$project$Main$initialCommandPopoverState,
	E: _List_fromArray(
		[author$project$Language$HogyLang$table]),
	M: true,
	q: author$project$Main$initialTokenTableState,
	e: author$project$BFTypes$initialExecutorParams,
	v: author$project$Main$initialTokenTableState,
	V: '',
	bW: rundis$elm_bootstrap$Bootstrap$Modal$hidden,
	W: 0,
	b1: rundis$elm_bootstrap$Bootstrap$Tab$initialState,
	s: elm$core$Array$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(1, '['),
				_Utils_Tuple2(2, ']'),
				_Utils_Tuple2(3, '+'),
				_Utils_Tuple2(4, '-'),
				_Utils_Tuple2(5, '>'),
				_Utils_Tuple2(6, '<'),
				_Utils_Tuple2(7, ','),
				_Utils_Tuple2(8, '.')
			])),
	aJ: rundis$elm_bootstrap$Bootstrap$Dropdown$initialState,
	aK: 'New Language'
};
var elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var elm$json$Json$Decode$decodeValue = _Json_run;
var author$project$Main$decodeModel = function (value) {
	var cacheStr = A2(
		elm$core$Result$withDefault,
		'',
		A2(elm$json$Json$Decode$decodeValue, elm$json$Json$Decode$string, value));
	var customTokenTableList = A2(
		elm$core$Result$withDefault,
		author$project$Main$initialModel.E,
		A2(
			elm$json$Json$Decode$decodeString,
			A2(
				elm$json$Json$Decode$field,
				'customLanguages',
				elm$json$Json$Decode$list(author$project$Main$decodeTokenTable)),
			cacheStr));
	var displayTokenTable = A2(
		elm$core$Result$withDefault,
		author$project$Main$defaultTokenTable,
		A2(
			elm$json$Json$Decode$decodeString,
			A2(elm$json$Json$Decode$field, 'displayTokenTable', author$project$Main$decodeTokenTable),
			cacheStr));
	var input = A2(
		elm$core$Result$withDefault,
		'',
		A2(
			elm$json$Json$Decode$decodeString,
			A2(elm$json$Json$Decode$field, 'input', elm$json$Json$Decode$string),
			cacheStr));
	var parserTokenTable = A2(
		elm$core$Result$withDefault,
		author$project$Main$defaultTokenTable,
		A2(
			elm$json$Json$Decode$decodeString,
			A2(elm$json$Json$Decode$field, 'parserTokenTable', author$project$Main$decodeTokenTable),
			cacheStr));
	var programContent = A2(
		elm$core$Result$withDefault,
		'',
		A2(
			elm$json$Json$Decode$decodeString,
			A2(elm$json$Json$Decode$field, 'programContent', elm$json$Json$Decode$string),
			cacheStr));
	return _Utils_update(
		author$project$Main$initialModel,
		{
			E: customTokenTableList,
			q: _Utils_update(
				author$project$Main$initialTokenTableState,
				{p: displayTokenTable}),
			e: _Utils_update(
				author$project$BFTypes$initialExecutorParams,
				{dU: input}),
			v: _Utils_update(
				author$project$Main$initialTokenTableState,
				{p: parserTokenTable}),
			V: programContent
		});
};
var elm$core$Elm$JsArray$map = _JsArray_map;
var elm$core$Array$map = F2(
	function (func, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = function (node) {
			if (!node.$) {
				var subTree = node.a;
				return elm$core$Array$SubTree(
					A2(elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return elm$core$Array$Leaf(
					A2(elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2(elm$core$Elm$JsArray$map, helper, tree),
			A2(elm$core$Elm$JsArray$map, func, tail));
	});
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var author$project$BFParser$convertBFCommandToString = F3(
	function (table, isCopyingComment, command) {
		if (!command.$) {
			var token = command.a;
			return A2(
				elm$core$Maybe$withDefault,
				_Utils_Tuple2(
					0,
					isCopyingComment ? token.d5 : ''),
				elm$core$List$head(
					A2(
						elm$core$List$filter,
						A2(
							elm$core$Basics$composeL,
							elm$core$Basics$eq(token.c6),
							elm$core$Tuple$first),
						table.a))).b;
		} else {
			var commands = command.a;
			var funcStr = A3(author$project$BFParser$convertBFCommandsToString, table, isCopyingComment, commands);
			return isCopyingComment ? elm$core$String$concat(
				_List_fromArray(
					['\n', funcStr, '\n'])) : funcStr;
		}
	});
var author$project$BFParser$convertBFCommandsToString = F3(
	function (table, isCopyingComment, commands) {
		return elm$core$String$concat(
			elm$core$Array$toList(
				A2(
					elm$core$Array$map,
					A2(author$project$BFParser$convertBFCommandToString, table, isCopyingComment),
					commands)));
	});
var author$project$BFParser$BFCommandList = elm$core$Basics$identity;
var author$project$BFParser$BFCommandStack = elm$core$Basics$identity;
var author$project$BFParser$addCommandIntoList = F2(
	function (_n0, cmd) {
		var commands = _n0;
		return A2(elm$core$List$cons, cmd, commands);
	});
var author$project$BFParser$addCommandIntoCurrentList = F2(
	function (stack, cmd) {
		var _n0 = function () {
			if (stack.b) {
				var _n2 = stack;
				var x = _n2.a;
				var xs = _n2.b;
				return _Utils_Tuple2(x, xs);
			} else {
				return _Utils_Tuple2(_List_Nil, _List_Nil);
			}
		}();
		var current = _n0.a;
		var ancestors = _n0.b;
		return A2(
			elm$core$List$cons,
			A2(author$project$BFParser$addCommandIntoList, current, cmd),
			ancestors);
	});
var elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var author$project$BFParser$beginNewLoopCommand = F2(
	function (_n0, cmd) {
		var stackList = _n0;
		return A2(
			elm$core$List$cons,
			elm$core$List$singleton(cmd),
			stackList);
	});
var author$project$BFParser$reverseCommandList = function (_n0) {
	var commands = _n0;
	return elm$core$Array$fromList(
		elm$core$List$reverse(commands));
};
var author$project$BFTypes$BFLoopFunc = function (a) {
	return {$: 1, a: a};
};
var author$project$BFParser$finalizeLoopCommand = F2(
	function (stack, cmd) {
		var _n0 = function () {
			if (stack.b) {
				var _n2 = stack;
				var x = _n2.a;
				var xs = _n2.b;
				return _Utils_Tuple2(x, xs);
			} else {
				return _Utils_Tuple2(_List_Nil, _List_Nil);
			}
		}();
		var current = _n0.a;
		var ancestors = _n0.b;
		var commands = author$project$BFParser$reverseCommandList(
			A2(author$project$BFParser$addCommandIntoList, current, cmd));
		return A2(
			author$project$BFParser$addCommandIntoCurrentList,
			ancestors,
			author$project$BFTypes$BFLoopFunc(commands));
	});
var author$project$BFParser$getBFCommandListLength = function (_n0) {
	var commands = _n0;
	return elm$core$List$length(commands);
};
var author$project$BFTypes$BFCommand = function (a) {
	return {$: 0, a: a};
};
var author$project$BFTypes$BFToken = F3(
	function (kind, value, error) {
		return {bd: error, c6: kind, d5: value};
	});
var author$project$BFTypes$InsufficientLoopEnd = 1;
var author$project$BFParser$finalizer = F2(
	function (list, current) {
		if (!author$project$BFParser$getBFCommandListLength(current)) {
			return list;
		} else {
			var _n0 = A2(
				author$project$BFParser$finalizeLoopCommand,
				_List_fromArray(
					[current, list]),
				author$project$BFTypes$BFCommand(
					A3(
						author$project$BFTypes$BFToken,
						2,
						'(Loop wasn\'t closed)',
						elm$core$Maybe$Just(1))));
			var innerStackList = _n0;
			return A2(
				elm$core$Maybe$withDefault,
				_List_Nil,
				elm$core$List$head(innerStackList));
		}
	});
var author$project$BFParser$getBFStackLength = function (_n0) {
	var commands = _n0;
	return elm$core$List$length(commands);
};
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var elm$parser$Parser$UnexpectedChar = {$: 11};
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var elm$parser$Parser$Advanced$Parser = elm$core$Basics$identity;
var elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {cO: col, dL: contextStack, dk: problem, ds: row};
	});
var elm$parser$Parser$Advanced$Empty = {$: 0};
var elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$DeadEnd, s.ds, s.cO, x, s.c));
	});
var elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return function (s) {
			var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, s.b, s.a);
			return _Utils_eq(newOffset, -1) ? A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
				elm$parser$Parser$Advanced$Good,
				true,
				0,
				{cO: 1, c: s.c, d: s.d, b: s.b + 1, ds: s.ds + 1, a: s.a}) : A3(
				elm$parser$Parser$Advanced$Good,
				true,
				0,
				{cO: s.cO + 1, c: s.c, d: s.d, b: newOffset, ds: s.ds, a: s.a}));
		};
	});
var elm$parser$Parser$chompIf = function (isGood) {
	return A2(elm$parser$Parser$Advanced$chompIf, isGood, elm$parser$Parser$UnexpectedChar);
};
var elm$core$String$slice = _String_slice;
var elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _n0) {
		var parse = _n0;
		return function (s0) {
			var _n1 = parse(s0);
			if (_n1.$ === 1) {
				var p = _n1.a;
				var x = _n1.b;
				return A2(elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p = _n1.a;
				var a = _n1.b;
				var s1 = _n1.c;
				return A3(
					elm$parser$Parser$Advanced$Good,
					p,
					A2(
						func,
						A3(elm$core$String$slice, s0.b, s1.b, s0.a),
						a),
					s1);
			}
		};
	});
var elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2(elm$parser$Parser$Advanced$mapChompedString, elm$core$Basics$always, parser);
};
var elm$parser$Parser$getChompedString = elm$parser$Parser$Advanced$getChompedString;
var elm$parser$Parser$Advanced$map = F2(
	function (func, _n0) {
		var parse = _n0;
		return function (s0) {
			var _n1 = parse(s0);
			if (!_n1.$) {
				var p = _n1.a;
				var a = _n1.b;
				var s1 = _n1.c;
				return A3(
					elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _n1.a;
				var x = _n1.b;
				return A2(elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var elm$parser$Parser$map = elm$parser$Parser$Advanced$map;
var author$project$BFParser$parseNoOpToken = A2(
	elm$parser$Parser$map,
	function (value) {
		return A3(author$project$BFTypes$BFToken, 0, value, elm$core$Maybe$Nothing);
	},
	elm$parser$Parser$getChompedString(
		elm$parser$Parser$chompIf(
			elm$core$Basics$always(true))));
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$parser$Parser$Advanced$map2 = F3(
	function (func, _n0, _n1) {
		var parseA = _n0;
		var parseB = _n1;
		return function (s0) {
			var _n2 = parseA(s0);
			if (_n2.$ === 1) {
				var p = _n2.a;
				var x = _n2.b;
				return A2(elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _n2.a;
				var a = _n2.b;
				var s1 = _n2.c;
				var _n3 = parseB(s1);
				if (_n3.$ === 1) {
					var p2 = _n3.a;
					var x = _n3.b;
					return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _n3.a;
					var b = _n3.b;
					var s2 = _n3.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$always, keepParser, ignoreParser);
	});
var elm$parser$Parser$ignorer = elm$parser$Parser$Advanced$ignorer;
var elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2(elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _n1 = parse(s0);
				if (!_n1.$) {
					var step = _n1;
					return step;
				} else {
					var step = _n1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2(elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3(elm$parser$Parser$Advanced$oneOfHelp, s, elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var elm$parser$Parser$oneOf = elm$parser$Parser$Advanced$oneOf;
var elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3(elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var elm$parser$Parser$succeed = elm$parser$Parser$Advanced$succeed;
var elm$parser$Parser$Expecting = function (a) {
	return {$: 0, a: a};
};
var elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$parser$Parser$toToken = function (str) {
	return A2(
		elm$parser$Parser$Advanced$Token,
		str,
		elm$parser$Parser$Expecting(str));
};
var elm$core$Basics$not = _Basics_not;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var elm$parser$Parser$Advanced$token = function (_n0) {
	var str = _n0.a;
	var expecting = _n0.b;
	var progress = !elm$core$String$isEmpty(str);
	return function (s) {
		var _n1 = A5(elm$parser$Parser$Advanced$isSubString, str, s.b, s.ds, s.cO, s.a);
		var newOffset = _n1.a;
		var newRow = _n1.b;
		var newCol = _n1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			elm$parser$Parser$Advanced$Bad,
			false,
			A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{cO: newCol, c: s.c, d: s.d, b: newOffset, ds: newRow, a: s.a});
	};
};
var elm$parser$Parser$token = function (str) {
	return elm$parser$Parser$Advanced$token(
		elm$parser$Parser$toToken(str));
};
var author$project$BFParser$parseTokenByTable = function (table) {
	return elm$parser$Parser$oneOf(
		A2(
			elm$core$List$map,
			function (x) {
				var _n0 = x;
				var kind = _n0.a;
				var value = _n0.b;
				return A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$succeed(
						A3(author$project$BFTypes$BFToken, kind, value, elm$core$Maybe$Nothing)),
					elm$parser$Parser$token(value));
			},
			table.a));
};
var author$project$BFTypes$TooManyLoopEnd = 0;
var elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var elm$parser$Parser$ExpectingEnd = {$: 10};
var elm$core$String$length = _String_length;
var elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			elm$core$String$length(s.a),
			s.b) ? A3(elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			elm$parser$Parser$Advanced$Bad,
			false,
			A2(elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var elm$parser$Parser$end = elm$parser$Parser$Advanced$end(elm$parser$Parser$ExpectingEnd);
var elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var elm$parser$Parser$toAdvancedStep = function (step) {
	if (!step.$) {
		var s = step.a;
		return elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return elm$parser$Parser$Advanced$Done(a);
	}
};
var elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _n0 = callback(state);
			var parse = _n0;
			var _n1 = parse(s0);
			if (!_n1.$) {
				var p1 = _n1.a;
				var step = _n1.b;
				var s1 = _n1.c;
				if (!step.$) {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3(elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _n1.a;
				var x = _n1.b;
				return A2(elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4(elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					elm$parser$Parser$map,
					elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var author$project$BFParser$parseTokensHelper = function (cmdTable) {
	return A2(
		elm$parser$Parser$loop,
		_List_fromArray(
			[_List_Nil]),
		function (memo) {
			return elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						elm$parser$Parser$map,
						function (token) {
							return elm$parser$Parser$Loop(
								function () {
									var _n0 = token.c6;
									switch (_n0) {
										case 1:
											return A2(
												author$project$BFParser$beginNewLoopCommand,
												memo,
												author$project$BFTypes$BFCommand(token));
										case 2:
											return (author$project$BFParser$getBFStackLength(memo) <= 1) ? A2(
												author$project$BFParser$addCommandIntoCurrentList,
												memo,
												author$project$BFTypes$BFCommand(
													_Utils_update(
														token,
														{
															bd: elm$core$Maybe$Just(0),
															c6: 0
														}))) : A2(
												author$project$BFParser$finalizeLoopCommand,
												memo,
												author$project$BFTypes$BFCommand(token));
										default:
											return A2(
												author$project$BFParser$addCommandIntoCurrentList,
												memo,
												author$project$BFTypes$BFCommand(token));
									}
								}());
						},
						author$project$BFParser$parseTokenByTable(cmdTable)),
						A2(
						elm$parser$Parser$map,
						A2(
							elm$core$Basics$composeL,
							A2(
								elm$core$Basics$composeL,
								elm$parser$Parser$Loop,
								author$project$BFParser$addCommandIntoCurrentList(memo)),
							author$project$BFTypes$BFCommand),
						author$project$BFParser$parseNoOpToken),
						A2(
						elm$parser$Parser$map,
						function (_n1) {
							var _n2 = memo;
							var stackList = _n2;
							return elm$parser$Parser$Done(
								author$project$BFParser$reverseCommandList(
									A3(elm$core$List$foldl, author$project$BFParser$finalizer, _List_Nil, stackList)));
						},
						elm$parser$Parser$end)
					]));
		});
};
var elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {cO: col, dk: problem, ds: row};
	});
var elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3(elm$parser$Parser$DeadEnd, p.ds, p.cO, p.dk);
};
var elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var elm$parser$Parser$Advanced$run = F2(
	function (_n0, src) {
		var parse = _n0;
		var _n1 = parse(
			{cO: 1, c: _List_Nil, d: 1, b: 0, ds: 1, a: src});
		if (!_n1.$) {
			var value = _n1.b;
			return elm$core$Result$Ok(value);
		} else {
			var bag = _n1.b;
			return elm$core$Result$Err(
				A2(elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var elm$parser$Parser$run = F2(
	function (parser, source) {
		var _n0 = A2(elm$parser$Parser$Advanced$run, parser, source);
		if (!_n0.$) {
			var a = _n0.a;
			return elm$core$Result$Ok(a);
		} else {
			var problems = _n0.a;
			return elm$core$Result$Err(
				A2(elm$core$List$map, elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var author$project$BFParser$parseTokens = function (table) {
	return elm$parser$Parser$run(
		author$project$BFParser$parseTokensHelper(table));
};
var author$project$Main$ChangeProgramContent = function (a) {
	return {$: 0, a: a};
};
var author$project$Main$UpdateAddCustomTokenTableModalState = function (a) {
	return {$: 5, a: a};
};
var author$project$Main$UpdateCommandPopoverIndices = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$UpdateCommandPopoverState = function (a) {
	return {$: 15, a: a};
};
var author$project$Main$UpdateCommandPopoversState = function (a) {
	return {$: 0, a: a};
};
var author$project$Main$UpdateExecutorParams = function (a) {
	return {$: 20, a: a};
};
var author$project$Main$UpdateProgramContent = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$UpdateTokens = function (a) {
	return {$: 0, a: a};
};
var author$project$Main$createTokenTable = function (model) {
	var tokenTable = elm$core$Array$toList(model.s);
	return elm$core$List$singleton(
		_Utils_Tuple2(tokenTable, model.aK));
};
var author$project$Main$updateCommandPopoverState = F2(
	function (msg, popoverState) {
		if (!msg.$) {
			var state = msg.a;
			return _Utils_update(
				popoverState,
				{U: state});
		} else {
			var pos = msg.a;
			return _Utils_update(
				popoverState,
				{bK: pos});
		}
	});
var author$project$BFExecutor$decreaseTapePointer = function (current) {
	return (0 < current) ? elm$core$Result$Ok(current - 1) : elm$core$Result$Err('Tape Pointer Under Limit Exceed');
};
var author$project$BFTypes$extractBFTape = function (tape) {
	var _n0 = tape;
	var tapeArray = _n0;
	return tapeArray;
};
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$core$Array$bitMask = 4294967295 >>> (32 - elm$core$Array$shiftStep);
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = elm$core$Array$bitMask & (index >>> shift);
			var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_n0.$) {
				var subTree = _n0.a;
				var $temp$shift = shift - elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _n0.a;
				return A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, values);
			}
		}
	});
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Array$get = F2(
	function (index, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? elm$core$Maybe$Just(
			A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, tail)) : elm$core$Maybe$Just(
			A3(elm$core$Array$getHelp, startShift, index, tree)));
	});
var author$project$BFExecutor$getMaybeTapeValue = F2(
	function (tape, pos) {
		return A2(
			elm$core$Array$get,
			pos,
			author$project$BFTypes$extractBFTape(tape));
	});
var author$project$BFExecutor$getTapeValue = F2(
	function (tape, pos) {
		return A2(
			elm$core$Maybe$withDefault,
			0,
			A2(author$project$BFExecutor$getMaybeTapeValue, tape, pos));
	});
var elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (!_n0.$) {
			var subTree = _n0.a;
			var newSub = A4(elm$core$Array$setHelp, shift - elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _n0.a;
			var newLeaf = A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, values);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, tail)) : A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4(elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var elm$core$Basics$modBy = _Basics_modBy;
var author$project$BFExecutor$setTapeValue = F3(
	function (tape, pos, value) {
		var modValue = A2(elm$core$Basics$modBy, 256, value);
		return A3(
			elm$core$Array$set,
			pos,
			modValue,
			author$project$BFTypes$extractBFTape(tape));
	});
var author$project$BFExecutor$decreaseTapeValue = F2(
	function (tape, pos) {
		return A3(
			author$project$BFExecutor$setTapeValue,
			tape,
			pos,
			A2(author$project$BFExecutor$getTapeValue, tape, pos) - 1);
	});
var author$project$BFExecutor$getBFCommandByRevIndices = F2(
	function (cmds, pos) {
		getBFCommandByRevIndices:
		while (true) {
			if (pos.b) {
				var currentPos = pos.a;
				var remainingPos = pos.b;
				var _n1 = A2(elm$core$Array$get, currentPos, cmds);
				if (!_n1.$) {
					if (_n1.a.$ === 1) {
						var newCmds = _n1.a.a;
						var $temp$cmds = newCmds,
							$temp$pos = remainingPos;
						cmds = $temp$cmds;
						pos = $temp$pos;
						continue getBFCommandByRevIndices;
					} else {
						var cmd = _n1.a;
						return elm$core$Maybe$Just(cmd);
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			} else {
				return elm$core$Maybe$Just(
					author$project$BFTypes$BFLoopFunc(cmds));
			}
		}
	});
var author$project$BFExecutor$getBFCommandByIndices = F2(
	function (cmds, pos) {
		return A2(
			author$project$BFExecutor$getBFCommandByRevIndices,
			cmds,
			elm$core$List$reverse(pos));
	});
var author$project$BFExecutor$increaseBFCommandIndices = function (pos) {
	if (pos.b) {
		var head = pos.a;
		var tail = pos.b;
		return A2(elm$core$List$cons, head + 1, tail);
	} else {
		return _List_fromArray(
			[0]);
	}
};
var author$project$BFExecutor$getNextIndices = F2(
	function (cmds, pos) {
		return A2(
			author$project$BFExecutor$skipUntilNextBFCommand,
			cmds,
			author$project$BFExecutor$increaseBFCommandIndices(pos));
	});
var author$project$BFExecutor$skipUntilNextBFCommand = F2(
	function (cmds, pos) {
		var cmd = A2(author$project$BFExecutor$getBFCommandByIndices, cmds, pos);
		if (cmd.$ === 1) {
			if (pos.b && pos.b.b) {
				var parentPos = pos;
				var _n2 = parentPos.b;
				return A2(author$project$BFExecutor$getNextIndices, cmds, parentPos);
			} else {
				return pos;
			}
		} else {
			if (!cmd.a.$) {
				var token = cmd.a.a;
				var _n3 = token.c6;
				if (!_n3) {
					return A2(author$project$BFExecutor$getNextIndices, cmds, pos);
				} else {
					return pos;
				}
			} else {
				return A2(
					author$project$BFExecutor$skipUntilNextBFCommand,
					cmds,
					A2(elm$core$List$cons, 0, pos));
			}
		}
	});
var author$project$BFExecutor$increaseTapePointer = function (current) {
	return (_Utils_cmp(current + 1, author$project$BFTypes$tapeSize) < 0) ? elm$core$Result$Ok(current + 1) : elm$core$Result$Err('Tape Pointer Limit Exceed');
};
var author$project$BFExecutor$increaseTapeValue = F2(
	function (tape, pos) {
		return A3(
			author$project$BFExecutor$setTapeValue,
			tape,
			pos,
			A2(author$project$BFExecutor$getTapeValue, tape, pos) + 1);
	});
var author$project$BFTypes$ContinueLoop = 1;
var author$project$BFTypes$LeaveLoop = 2;
var author$project$BFTypes$Pausing = 6;
var author$project$BFTypes$bfParseErrorToString = function (error) {
	if (!error) {
		return 'Loop End shouldn\'t be here';
	} else {
		return 'Loop End should be here';
	}
};
var elm$core$Char$fromCode = _Char_fromCode;
var elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(xs);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var author$project$BFExecutor$runBFCommandByStep = function (oldState) {
	runBFCommandByStep:
	while (true) {
		var indices = function () {
			var _n8 = oldState.S;
			switch (_n8) {
				case 0:
					return A2(author$project$BFExecutor$getNextIndices, oldState.a7, oldState.a8);
				case 1:
					return A2(
						author$project$BFExecutor$getNextIndices,
						oldState.a7,
						A2(
							elm$core$List$cons,
							0,
							A2(
								elm$core$Maybe$withDefault,
								_List_Nil,
								elm$core$List$tail(oldState.a8))));
				default:
					return A2(
						author$project$BFExecutor$getNextIndices,
						oldState.a7,
						A2(
							elm$core$Maybe$withDefault,
							_List_Nil,
							elm$core$List$tail(oldState.a8)));
			}
		}();
		var state = _Utils_update(
			oldState,
			{a8: indices, S: 0});
		var cmd = A2(author$project$BFExecutor$getBFCommandByIndices, oldState.a7, indices);
		if (!cmd.$) {
			if (!cmd.a.$) {
				var token = cmd.a.a;
				var _n7 = token.bd;
				if (!_n7.$) {
					var err = _n7.a;
					return _Utils_update(
						state,
						{
							bd: elm$core$Maybe$Just(
								'Parse Error: ' + author$project$BFTypes$bfParseErrorToString(err)),
							z: 6
						});
				} else {
					return A2(author$project$BFExecutor$runBFCommandByStepInternal, token, state);
				}
			} else {
				var $temp$oldState = state;
				oldState = $temp$oldState;
				continue runBFCommandByStep;
			}
		} else {
			return _Utils_update(
				state,
				{z: 0});
		}
	}
};
var author$project$BFExecutor$runBFCommandByStepInternal = F2(
	function (token, state) {
		var _n0 = token.c6;
		switch (_n0) {
			case 0:
				return author$project$BFExecutor$runBFCommandByStep(state);
			case 1:
				var _n1 = A2(author$project$BFExecutor$getTapeValue, state.A, state.w);
				if (!_n1) {
					return _Utils_update(
						state,
						{S: 2});
				} else {
					return state;
				}
			case 2:
				var _n2 = A2(author$project$BFExecutor$getTapeValue, state.A, state.w);
				if (!_n2) {
					return _Utils_update(
						state,
						{S: 2});
				} else {
					return _Utils_update(
						state,
						{S: 1});
				}
			case 5:
				var _n3 = author$project$BFExecutor$increaseTapePointer(state.w);
				if (!_n3.$) {
					var ptr = _n3.a;
					return _Utils_update(
						state,
						{cQ: (ptr / (16 * 16)) | 0, w: ptr});
				} else {
					var error = _n3.a;
					return _Utils_update(
						state,
						{
							bd: elm$core$Maybe$Just(error),
							z: 6
						});
				}
			case 6:
				var _n4 = author$project$BFExecutor$decreaseTapePointer(state.w);
				if (!_n4.$) {
					var ptr = _n4.a;
					return _Utils_update(
						state,
						{cQ: (ptr / (16 * 16)) | 0, w: ptr});
				} else {
					var error = _n4.a;
					return _Utils_update(
						state,
						{
							bd: elm$core$Maybe$Just(error),
							z: 6
						});
				}
			case 3:
				return _Utils_update(
					state,
					{
						A: A2(author$project$BFExecutor$increaseTapeValue, state.A, state.w)
					});
			case 4:
				return _Utils_update(
					state,
					{
						A: A2(author$project$BFExecutor$decreaseTapeValue, state.A, state.w)
					});
			case 7:
				var maybeInput = A2(
					elm$core$Maybe$map,
					elm$core$Tuple$first,
					elm$core$String$uncons(
						A2(elm$core$String$dropLeft, state.aw, state.dU)));
				if (!maybeInput.$) {
					var input = maybeInput.a;
					return _Utils_update(
						state,
						{
							aw: state.aw + 1,
							A: A3(
								author$project$BFExecutor$setTapeValue,
								state.A,
								state.w,
								elm$core$Char$toCode(input))
						});
				} else {
					return _Utils_update(
						state,
						{aw: state.aw + 1});
				}
			default:
				var outputChar = elm$core$Char$fromCode(
					A2(author$project$BFExecutor$getTapeValue, state.A, state.w));
				var output = A2(elm$core$List$cons, outputChar, state.de);
				return _Utils_update(
					state,
					{de: output});
		}
	});
var author$project$BFExecutor$runBFCommandInternal = function (state) {
	var _n0 = state.z;
	switch (_n0) {
		case 0:
			return state;
		case 6:
			return state;
		case 1:
			return author$project$BFExecutor$runBFCommandByStep(state);
		case 2:
			return author$project$BFExecutor$runBFCommandByStep(state);
		case 3:
			var newState = author$project$BFExecutor$runBFCommandByStep(state);
			return _Utils_update(
				newState,
				{z: 6});
		case 4:
			var newState = author$project$BFExecutor$runBFCommandByStep(state);
			var _n1 = newState.S;
			if (!_n1) {
				return newState;
			} else {
				return _Utils_update(
					newState,
					{z: 6});
			}
		default:
			var newState = author$project$BFExecutor$runBFCommandByStep(state);
			var _n2 = newState.S;
			if (_n2 === 2) {
				return _Utils_update(
					newState,
					{z: 6});
			} else {
				return newState;
			}
	}
};
var author$project$BFExecutor$runBFCommands = function (state) {
	runBFCommands:
	while (true) {
		var newState = author$project$BFExecutor$runBFCommandInternal(state);
		var _n0 = newState.z;
		switch (_n0) {
			case 1:
				var $temp$state = newState;
				state = $temp$state;
				continue runBFCommands;
			case 2:
				var $temp$state = newState;
				state = $temp$state;
				continue runBFCommands;
			case 4:
				var $temp$state = newState;
				state = $temp$state;
				continue runBFCommands;
			case 5:
				var $temp$state = newState;
				state = $temp$state;
				continue runBFCommands;
			default:
				return newState;
		}
	}
};
var author$project$BFTypes$Running = 1;
var author$project$Main$StopExecution = {$: 4};
var author$project$Main$updateExecutorParams = F2(
	function (msg, state) {
		switch (msg.$) {
			case 0:
				var commands = msg.a;
				return _Utils_update(
					state,
					{a7: commands});
			case 1:
				var input = msg.a;
				return _Utils_update(
					state,
					{dU: input});
			case 2:
				var page = msg.a;
				var newPage = A2(elm$core$Basics$modBy, author$project$BFTypes$tapePages, page);
				return _Utils_update(
					state,
					{cQ: newPage});
			case 3:
				var runningState = msg.a;
				var newState = (runningState === 1) ? A2(author$project$Main$updateExecutorParams, author$project$Main$StopExecution, state) : state;
				return author$project$BFExecutor$runBFCommands(
					_Utils_update(
						newState,
						{z: runningState}));
			default:
				return _Utils_update(
					author$project$BFTypes$initialExecutorParams,
					{a7: state.a7, dU: state.dU});
		}
	});
var author$project$Main$updateTokenTableState = F2(
	function (msg, tokenTableDropdown) {
		if (!msg.$) {
			var state = msg.a;
			return _Utils_update(
				tokenTableDropdown,
				{N: state});
		} else {
			var tokenTable = msg.a;
			return _Utils_update(
				tokenTableDropdown,
				{p: tokenTable});
		}
	});
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Cacher$cache = _Platform_outgoingPort('cache', elm$json$Json$Encode$string);
var author$project$BFTypes$tokenKindToInt = function (kind) {
	switch (kind) {
		case 0:
			return 0;
		case 5:
			return 1;
		case 6:
			return 2;
		case 3:
			return 3;
		case 4:
			return 4;
		case 8:
			return 5;
		case 7:
			return 6;
		case 1:
			return 7;
		default:
			return 8;
	}
};
var elm$json$Json$Encode$int = _Json_wrap;
var elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var author$project$Main$encodeTokenTable = function (_n0) {
	var tableList = _n0.a;
	var name = _n0.b;
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tokens',
				A2(
					elm$json$Json$Encode$list,
					function (_n1) {
						var kind = _n1.a;
						var value = _n1.b;
						return elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'kind',
									elm$json$Json$Encode$int(
										author$project$BFTypes$tokenKindToInt(kind))),
									_Utils_Tuple2(
									'value',
									elm$json$Json$Encode$string(value))
								]));
					},
					tableList)),
				_Utils_Tuple2(
				'name',
				elm$json$Json$Encode$string(name))
			]));
};
var author$project$Main$encodeModel = function (model) {
	return A2(
		elm$json$Json$Encode$encode,
		0,
		elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'programContent',
					elm$json$Json$Encode$string(model.V)),
					_Utils_Tuple2(
					'input',
					elm$json$Json$Encode$string(model.e.dU)),
					_Utils_Tuple2(
					'customLanguages',
					A2(elm$json$Json$Encode$list, author$project$Main$encodeTokenTable, model.E)),
					_Utils_Tuple2(
					'parserTokenTable',
					author$project$Main$encodeTokenTable(model.v.p)),
					_Utils_Tuple2(
					'displayTokenTable',
					author$project$Main$encodeTokenTable(model.q.p))
				])));
};
var elm$core$Platform$Cmd$batch = _Platform_batch;
var author$project$Main$withCacheCmd = function (model) {
	return _Utils_Tuple2(
		model,
		elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					author$project$Cacher$cache(
					author$project$Main$encodeModel(model))
				])));
};
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Main$withCmdNone = function (model) {
	return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
};
var elm$core$Array$filter = F2(
	function (isGood, array) {
		return elm$core$Array$fromList(
			A3(
				elm$core$Array$foldr,
				F2(
					function (x, xs) {
						return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
					}),
				_List_Nil,
				array));
	});
var elm$core$Elm$JsArray$push = _JsArray_push;
var elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					elm$core$Elm$JsArray$push,
					elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, elm$core$Elm$JsArray$empty));
				return A2(elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!value.$) {
				var subTree = value.a;
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, subTree));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(
						elm$core$Array$insertTailInTree,
						shift - elm$core$Array$shiftStep,
						index,
						tail,
						elm$core$Elm$JsArray$singleton(value)));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		var originalTailLen = elm$core$Elm$JsArray$length(tail);
		var newTailLen = elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + elm$core$Array$shiftStep;
				var newTree = A4(
					elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					elm$core$Elm$JsArray$singleton(
						elm$core$Array$SubTree(tree)));
				return A4(elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4(elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4(elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			elm$core$Array$unsafeReplaceTail,
			A2(elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var author$project$Main$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 0:
					var programContent = msg.a;
					return author$project$Main$withCacheCmd(
						A2(
							author$project$Main$update,
							author$project$Main$ParseTokens,
							A2(
								author$project$Main$update,
								author$project$Main$UpdateProgramContent(programContent),
								model).a).a);
				case 1:
					var programContent = msg.a;
					return author$project$Main$withCacheCmd(
						_Utils_update(
							model,
							{V: programContent}));
				case 2:
					var state = msg.a;
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{b1: state}));
				case 3:
					var visibility = msg.a;
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{M: visibility}));
				case 4:
					var customTokenTableList = A2(
						elm$core$List$append,
						model.E,
						author$project$Main$createTokenTable(model));
					return author$project$Main$withCacheCmd(
						A2(
							author$project$Main$update,
							author$project$Main$UpdateAddCustomTokenTableModalState(rundis$elm_bootstrap$Bootstrap$Modal$hidden),
							_Utils_update(
								model,
								{E: customTokenTableList})).a);
				case 5:
					var state = msg.a;
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{aU: state}));
				case 6:
					var pos = msg.a;
					var value = msg.b;
					var kind = A2(
						elm$core$Maybe$withDefault,
						0,
						A2(
							elm$core$Maybe$map,
							elm$core$Tuple$first,
							A2(elm$core$Array$get, pos, model.s)));
					var upComingCustomTokenTable = A3(
						elm$core$Array$set,
						pos,
						_Utils_Tuple2(kind, value),
						model.s);
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{s: upComingCustomTokenTable}));
				case 7:
					var kind = msg.a;
					var upComingCustomTokenTable = A2(
						elm$core$Array$push,
						_Utils_Tuple2(kind, ''),
						model.s);
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{s: upComingCustomTokenTable}));
				case 8:
					var pos = msg.a;
					var upComingCustomTokenTable = A2(
						elm$core$Array$filter,
						function (_n1) {
							var kind = _n1.a;
							return kind;
						},
						A3(
							elm$core$Array$set,
							pos,
							_Utils_Tuple2(0, ''),
							model.s));
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{s: upComingCustomTokenTable}));
				case 9:
					var name = msg.a;
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{aK: name}));
				case 10:
					var state = msg.a;
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{aJ: state}));
				case 11:
					var state = msg.a;
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{W: state}));
				case 12:
					var $temp$msg = author$project$Main$ChangeProgramContent(
						A3(author$project$BFParser$convertBFCommandsToString, model.q.p, model.M, model.e.a7)),
						$temp$model = model;
					msg = $temp$msg;
					model = $temp$model;
					continue update;
				case 13:
					return author$project$Main$withCacheCmd(author$project$Main$initialModel);
				case 14:
					var state = msg.a;
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{bW: state}));
				case 15:
					var popoverStateMsg = msg.a;
					var state = A2(author$project$Main$updateCommandPopoverState, popoverStateMsg, model.x);
					return author$project$Main$withCmdNone(
						_Utils_update(
							model,
							{x: state}));
				case 16:
					var pos = msg.a;
					var state = msg.b;
					return author$project$Main$withCmdNone(
						A2(
							author$project$Main$update,
							author$project$Main$UpdateCommandPopoverState(
								author$project$Main$UpdateCommandPopoversState(state)),
							A2(
								author$project$Main$update,
								author$project$Main$UpdateCommandPopoverState(
									author$project$Main$UpdateCommandPopoverIndices(pos)),
								model).a).a);
				case 17:
					var tokenTableStateMsg = msg.a;
					var state = A2(author$project$Main$updateTokenTableState, tokenTableStateMsg, model.v);
					return author$project$Main$withCacheCmd(
						A2(
							author$project$Main$update,
							author$project$Main$ParseTokens,
							_Utils_update(
								model,
								{v: state})).a);
				case 18:
					var tokenTableStateMsg = msg.a;
					var state = A2(author$project$Main$updateTokenTableState, tokenTableStateMsg, model.q);
					return author$project$Main$withCacheCmd(
						_Utils_update(
							model,
							{q: state}));
				case 19:
					var commands = A2(
						elm$core$Result$withDefault,
						model.e.a7,
						A2(author$project$BFParser$parseTokens, model.v.p, model.V));
					return author$project$Main$withCmdNone(
						A2(
							author$project$Main$update,
							author$project$Main$UpdateExecutorParams(
								author$project$Main$UpdateTokens(commands)),
							model).a);
				default:
					var executorParamsMsg = msg.a;
					var state = A2(author$project$Main$updateExecutorParams, executorParamsMsg, model.e);
					return author$project$Main$withCacheCmd(
						_Utils_update(
							model,
							{e: state}));
			}
		}
	});
var author$project$Main$init = function (flags) {
	return author$project$Main$withCmdNone(
		A2(
			author$project$Main$update,
			author$project$Main$ParseTokens,
			author$project$Main$decodeModel(flags)).a);
};
var author$project$Main$UpdateDisplayTokenTableState = function (a) {
	return {$: 18, a: a};
};
var author$project$Main$UpdateParserTokenTableState = function (a) {
	return {$: 17, a: a};
};
var author$project$Main$UpdateTokenTableDropdownState = function (a) {
	return {$: 0, a: a};
};
var author$project$Main$UpdateUpComingCustomTokenTableDropdown = function (a) {
	return {$: 10, a: a};
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {ct: oldTime, dq: request, dy: subs};
	});
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$browser$Browser$AnimationManager$init = elm$core$Task$succeed(
	A3(elm$browser$Browser$AnimationManager$State, _List_Nil, elm$core$Maybe$Nothing, 0));
var elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$Dom$NotFound = elm$core$Basics$identity;
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = elm$core$Basics$identity;
var elm$core$Task$init = elm$core$Task$succeed(0);
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return 0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0;
		return A2(elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			A2(elm$core$Task$map, toMessage, task));
	});
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = 0;
var elm$url$Url$Https = 1;
var elm$core$String$indexes = _String_indexes;
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {cY: fragment, cZ: host, dg: path, di: port_, dn: protocol, $7: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 1) {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		0,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		1,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$AnimationManager$now = _Browser_now(0);
var elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(0);
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _n0) {
		var request = _n0.dq;
		var oldTime = _n0.ct;
		var _n1 = _Utils_Tuple2(request, subs);
		if (_n1.a.$ === 1) {
			if (!_n1.b.b) {
				var _n2 = _n1.a;
				return elm$browser$Browser$AnimationManager$init;
			} else {
				var _n4 = _n1.a;
				return A2(
					elm$core$Task$andThen,
					function (pid) {
						return A2(
							elm$core$Task$andThen,
							function (time) {
								return elm$core$Task$succeed(
									A3(
										elm$browser$Browser$AnimationManager$State,
										subs,
										elm$core$Maybe$Just(pid),
										time));
							},
							elm$browser$Browser$AnimationManager$now);
					},
					elm$core$Process$spawn(
						A2(
							elm$core$Task$andThen,
							elm$core$Platform$sendToSelf(router),
							elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_n1.b.b) {
				var pid = _n1.a.a;
				return A2(
					elm$core$Task$andThen,
					function (_n3) {
						return elm$browser$Browser$AnimationManager$init;
					},
					elm$core$Process$kill(pid));
			} else {
				return elm$core$Task$succeed(
					A3(elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var elm$time$Time$Posix = elm$core$Basics$identity;
var elm$time$Time$millisToPosix = elm$core$Basics$identity;
var elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _n0) {
		var subs = _n0.dy;
		var oldTime = _n0.ct;
		var send = function (sub) {
			if (!sub.$) {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(
						elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			elm$core$Task$andThen,
			function (pid) {
				return A2(
					elm$core$Task$andThen,
					function (_n1) {
						return elm$core$Task$succeed(
							A3(
								elm$browser$Browser$AnimationManager$State,
								subs,
								elm$core$Maybe$Just(pid),
								newTime));
					},
					elm$core$Task$sequence(
						A2(elm$core$List$map, send, subs)));
			},
			elm$core$Process$spawn(
				A2(
					elm$core$Task$andThen,
					elm$core$Platform$sendToSelf(router),
					elm$browser$Browser$AnimationManager$rAF)));
	});
var elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (!sub.$) {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Time(
				A2(elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Delta(
				A2(elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager(elm$browser$Browser$AnimationManager$init, elm$browser$Browser$AnimationManager$onEffects, elm$browser$Browser$AnimationManager$onSelfMsg, 0, elm$browser$Browser$AnimationManager$subMap);
var elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return elm$browser$Browser$AnimationManager$subscription(
		elm$browser$Browser$AnimationManager$Time(tagger));
};
var elm$browser$Browser$Events$onAnimationFrame = elm$browser$Browser$AnimationManager$onAnimationFrame;
var elm$browser$Browser$Events$Document = 0;
var elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {dh: pids, dy: subs};
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$browser$Browser$Events$init = elm$core$Task$succeed(
	A2(elm$browser$Browser$Events$State, _List_Nil, elm$core$Dict$empty));
var elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {cV: event, c5: key};
	});
var elm$browser$Browser$Events$spawn = F3(
	function (router, key, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						elm$core$Platform$sendToSelf,
						router,
						A2(elm$browser$Browser$Events$Event, key, event));
				}));
	});
var elm$core$Dict$Black = 1;
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = 0;
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1) {
				case 0:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _n6) {
				var deads = _n6.a;
				var lives = _n6.b;
				var news = _n6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						elm$core$List$cons,
						A3(elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_n4, pid, _n5) {
				var deads = _n5.a;
				var lives = _n5.b;
				var news = _n5.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _n2, _n3) {
				var deads = _n3.a;
				var lives = _n3.b;
				var news = _n3.c;
				return _Utils_Tuple3(
					deads,
					A3(elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2(elm$core$List$map, elm$browser$Browser$Events$addKey, subs);
		var _n0 = A6(
			elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.dh,
			elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, elm$core$Dict$empty, _List_Nil));
		var deadPids = _n0.a;
		var livePids = _n0.b;
		var makeNewPids = _n0.c;
		return A2(
			elm$core$Task$andThen,
			function (pids) {
				return elm$core$Task$succeed(
					A2(
						elm$browser$Browser$Events$State,
						newSubs,
						A2(
							elm$core$Dict$union,
							livePids,
							elm$core$Dict$fromList(pids))));
			},
			A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$sequence(makeNewPids);
				},
				elm$core$Task$sequence(
					A2(elm$core$List$map, elm$core$Process$kill, deadPids))));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (!_n0.$) {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _n0, state) {
		var key = _n0.c5;
		var event = _n0.cV;
		var toMessage = function (_n2) {
			var subKey = _n2.a;
			var _n3 = _n2.b;
			var node = _n3.a;
			var name = _n3.b;
			var decoder = _n3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : elm$core$Maybe$Nothing;
		};
		var messages = A2(elm$core$List$filterMap, toMessage, state.dy);
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Platform$sendToApp(router),
					messages)));
	});
var elm$browser$Browser$Events$subMap = F2(
	function (func, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var decoder = _n0.c;
		return A3(
			elm$browser$Browser$Events$MySub,
			node,
			name,
			A2(elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager(elm$browser$Browser$Events$init, elm$browser$Browser$Events$onEffects, elm$browser$Browser$Events$onSelfMsg, 0, elm$browser$Browser$Events$subMap);
var elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return elm$browser$Browser$Events$subscription(
			A3(elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var elm$browser$Browser$Events$onClick = A2(elm$browser$Browser$Events$on, 0, 'click');
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var rundis$elm_bootstrap$Bootstrap$Dropdown$ListenClicks = 1;
var rundis$elm_bootstrap$Bootstrap$Dropdown$updateStatus = F2(
	function (status, _n0) {
		var stateRec = _n0;
		return _Utils_update(
			stateRec,
			{r: status});
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$subscriptions = F2(
	function (state, toMsg) {
		var status = state.r;
		switch (status) {
			case 0:
				return elm$browser$Browser$Events$onAnimationFrame(
					function (_n1) {
						return toMsg(
							A2(rundis$elm_bootstrap$Bootstrap$Dropdown$updateStatus, 1, state));
					});
			case 1:
				return elm$browser$Browser$Events$onClick(
					elm$json$Json$Decode$succeed(
						toMsg(
							A2(rundis$elm_bootstrap$Bootstrap$Dropdown$updateStatus, 2, state))));
			default:
				return elm$core$Platform$Sub$none;
		}
	});
var author$project$Main$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Dropdown$subscriptions,
				model.v.N,
				A2(elm$core$Basics$composeL, author$project$Main$UpdateParserTokenTableState, author$project$Main$UpdateTokenTableDropdownState)),
				A2(
				rundis$elm_bootstrap$Bootstrap$Dropdown$subscriptions,
				model.q.N,
				A2(elm$core$Basics$composeL, author$project$Main$UpdateDisplayTokenTableState, author$project$Main$UpdateTokenTableDropdownState)),
				A2(rundis$elm_bootstrap$Bootstrap$Dropdown$subscriptions, model.aJ, author$project$Main$UpdateUpComingCustomTokenTableDropdown)
			]));
};
var author$project$Main$UpdateTabState = function (a) {
	return {$: 2, a: a};
};
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$button = _VirtualDom_node('button');
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$html$Html$Attributes$classList = function (classes) {
	return elm$html$Html$Attributes$class(
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$map,
				elm$core$Tuple$first,
				A2(elm$core$List$filter, elm$core$Tuple$second, classes))));
};
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption = function (size) {
	switch (size) {
		case 0:
			return elm$core$Maybe$Nothing;
		case 1:
			return elm$core$Maybe$Just('sm');
		case 2:
			return elm$core$Maybe$Just('md');
		case 3:
			return elm$core$Maybe$Just('lg');
		default:
			return elm$core$Maybe$Just('xl');
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 0:
				var size = modifier.a;
				return _Utils_update(
					options,
					{
						b$: elm$core$Maybe$Just(size)
					});
			case 1:
				var coloring = modifier.a;
				return _Utils_update(
					options,
					{
						D: elm$core$Maybe$Just(coloring)
					});
			case 2:
				return _Utils_update(
					options,
					{a2: true});
			case 3:
				var val = modifier.a;
				return _Utils_update(
					options,
					{bc: val});
			default:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions = {a0: _List_Nil, a2: false, D: elm$core$Maybe$Nothing, bc: false, b$: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass = function (role) {
	switch (role) {
		case 0:
			return 'primary';
		case 1:
			return 'secondary';
		case 2:
			return 'success';
		case 3:
			return 'info';
		case 4:
			return 'warning';
		case 5:
			return 'danger';
		case 6:
			return 'dark';
		case 7:
			return 'light';
		default:
			return 'link';
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier, rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('btn', true),
						_Utils_Tuple2('btn-block', options.a2),
						_Utils_Tuple2('disabled', options.bc)
					])),
				elm$html$Html$Attributes$disabled(options.bc)
			]),
		_Utils_ap(
			function () {
				var _n0 = A2(elm$core$Maybe$andThen, rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption, options.b$);
				if (!_n0.$) {
					var s = _n0.a;
					return _List_fromArray(
						[
							elm$html$Html$Attributes$class('btn-' + s)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _n1 = options.D;
					if (!_n1.$) {
						if (!_n1.a.$) {
							var role = _n1.a.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'btn-' + rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						} else {
							var role = _n1.a.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'btn-outline-' + rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						}
					} else {
						return _List_Nil;
					}
				}(),
				options.a0)));
};
var rundis$elm_bootstrap$Bootstrap$Button$button = F2(
	function (options, children) {
		return A2(
			elm$html$Html$button,
			rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options),
			children);
	});
var elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs = function (a) {
	return {$: 4, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Button$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs(attrs_);
};
var rundis$elm_bootstrap$Bootstrap$Button$onClick = function (message) {
	return rundis$elm_bootstrap$Bootstrap$Button$attrs(
		_List_fromArray(
			[
				A2(
				elm$html$Html$Events$preventDefaultOn,
				'click',
				elm$json$Json$Decode$succeed(
					_Utils_Tuple2(message, true)))
			]));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Column = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$col = F2(
	function (options, children) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Column(
			{cN: children, bC: options});
	});
var elm$html$Html$div = _VirtualDom_node('div');
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$Keyed$node = elm$virtual_dom$VirtualDom$keyedNode;
var rundis$elm_bootstrap$Bootstrap$General$Internal$XS = 0;
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col = 0;
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width = F2(
	function (screenSize, columnCount) {
		return {cP: columnCount, dv: screenSize};
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign = F2(
	function (align_, options) {
		var _n0 = align_.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						aZ: elm$core$Maybe$Just(align_)
					});
			case 1:
				return _Utils_update(
					options,
					{
						aX: elm$core$Maybe$Just(align_)
					});
			case 2:
				return _Utils_update(
					options,
					{
						aW: elm$core$Maybe$Just(align_)
					});
			case 3:
				return _Utils_update(
					options,
					{
						aV: elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						aY: elm$core$Maybe$Just(align_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset = F2(
	function (offset_, options) {
		var _n0 = offset_.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						bx: elm$core$Maybe$Just(offset_)
					});
			case 1:
				return _Utils_update(
					options,
					{
						bu: elm$core$Maybe$Just(offset_)
					});
			case 2:
				return _Utils_update(
					options,
					{
						bt: elm$core$Maybe$Just(offset_)
					});
			case 3:
				return _Utils_update(
					options,
					{
						bs: elm$core$Maybe$Just(offset_)
					});
			default:
				return _Utils_update(
					options,
					{
						bw: elm$core$Maybe$Just(offset_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder = F2(
	function (order_, options) {
		var _n0 = order_.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						bI: elm$core$Maybe$Just(order_)
					});
			case 1:
				return _Utils_update(
					options,
					{
						bG: elm$core$Maybe$Just(order_)
					});
			case 2:
				return _Utils_update(
					options,
					{
						bF: elm$core$Maybe$Just(order_)
					});
			case 3:
				return _Utils_update(
					options,
					{
						bE: elm$core$Maybe$Just(order_)
					});
			default:
				return _Utils_update(
					options,
					{
						bH: elm$core$Maybe$Just(order_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull = F2(
	function (pull_, options) {
		var _n0 = pull_.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						bP: elm$core$Maybe$Just(pull_)
					});
			case 1:
				return _Utils_update(
					options,
					{
						bN: elm$core$Maybe$Just(pull_)
					});
			case 2:
				return _Utils_update(
					options,
					{
						bM: elm$core$Maybe$Just(pull_)
					});
			case 3:
				return _Utils_update(
					options,
					{
						bL: elm$core$Maybe$Just(pull_)
					});
			default:
				return _Utils_update(
					options,
					{
						bO: elm$core$Maybe$Just(pull_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush = F2(
	function (push_, options) {
		var _n0 = push_.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						bU: elm$core$Maybe$Just(push_)
					});
			case 1:
				return _Utils_update(
					options,
					{
						bS: elm$core$Maybe$Just(push_)
					});
			case 2:
				return _Utils_update(
					options,
					{
						bR: elm$core$Maybe$Just(push_)
					});
			case 3:
				return _Utils_update(
					options,
					{
						bQ: elm$core$Maybe$Just(push_)
					});
			default:
				return _Utils_update(
					options,
					{
						bT: elm$core$Maybe$Just(push_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth = F2(
	function (width_, options) {
		var _n0 = width_.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						aP: elm$core$Maybe$Just(width_)
					});
			case 1:
				return _Utils_update(
					options,
					{
						aN: elm$core$Maybe$Just(width_)
					});
			case 2:
				return _Utils_update(
					options,
					{
						aM: elm$core$Maybe$Just(width_)
					});
			case 3:
				return _Utils_update(
					options,
					{
						aL: elm$core$Maybe$Just(width_)
					});
			default:
				return _Utils_update(
					options,
					{
						aO: elm$core$Maybe$Just(width_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 6:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs)
					});
			case 0:
				var width_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth, width_, options);
			case 1:
				var offset_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset, offset_, options);
			case 2:
				var pull_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull, pull_, options);
			case 3:
				var push_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush, push_, options);
			case 4:
				var order_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder, order_, options);
			case 5:
				var align = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign, align, options);
			default:
				var align = modifier.a;
				return _Utils_update(
					options,
					{
						b2: elm$core$Maybe$Just(align)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption = function (size) {
	switch (size) {
		case 0:
			return elm$core$Maybe$Nothing;
		case 1:
			return elm$core$Maybe$Just('1');
		case 2:
			return elm$core$Maybe$Just('2');
		case 3:
			return elm$core$Maybe$Just('3');
		case 4:
			return elm$core$Maybe$Just('4');
		case 5:
			return elm$core$Maybe$Just('5');
		case 6:
			return elm$core$Maybe$Just('6');
		case 7:
			return elm$core$Maybe$Just('7');
		case 8:
			return elm$core$Maybe$Just('8');
		case 9:
			return elm$core$Maybe$Just('9');
		case 10:
			return elm$core$Maybe$Just('10');
		case 11:
			return elm$core$Maybe$Just('11');
		case 12:
			return elm$core$Maybe$Just('12');
		default:
			return elm$core$Maybe$Just('auto');
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass = function (_n0) {
	var screenSize = _n0.dv;
	var columnCount = _n0.cP;
	return elm$html$Html$Attributes$class(
		'col' + (A2(
			elm$core$Maybe$withDefault,
			'',
			A2(
				elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + A2(
			elm$core$Maybe$withDefault,
			'',
			A2(
				elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption(columnCount)))));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes = function (widths) {
	var width_ = function (w) {
		return A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass, w);
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, width_, widths));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions = {aV: elm$core$Maybe$Nothing, aW: elm$core$Maybe$Nothing, aX: elm$core$Maybe$Nothing, aY: elm$core$Maybe$Nothing, aZ: elm$core$Maybe$Nothing, a0: _List_Nil, bs: elm$core$Maybe$Nothing, bt: elm$core$Maybe$Nothing, bu: elm$core$Maybe$Nothing, bw: elm$core$Maybe$Nothing, bx: elm$core$Maybe$Nothing, bE: elm$core$Maybe$Nothing, bF: elm$core$Maybe$Nothing, bG: elm$core$Maybe$Nothing, bH: elm$core$Maybe$Nothing, bI: elm$core$Maybe$Nothing, bL: elm$core$Maybe$Nothing, bM: elm$core$Maybe$Nothing, bN: elm$core$Maybe$Nothing, bO: elm$core$Maybe$Nothing, bP: elm$core$Maybe$Nothing, bQ: elm$core$Maybe$Nothing, bR: elm$core$Maybe$Nothing, bS: elm$core$Maybe$Nothing, bT: elm$core$Maybe$Nothing, bU: elm$core$Maybe$Nothing, b2: elm$core$Maybe$Nothing, aL: elm$core$Maybe$Nothing, aM: elm$core$Maybe$Nothing, aN: elm$core$Maybe$Nothing, aO: elm$core$Maybe$Nothing, aP: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption = function (size) {
	switch (size) {
		case 0:
			return '0';
		case 1:
			return '1';
		case 2:
			return '2';
		case 3:
			return '3';
		case 4:
			return '4';
		case 5:
			return '5';
		case 6:
			return '6';
		case 7:
			return '7';
		case 8:
			return '8';
		case 9:
			return '9';
		case 10:
			return '10';
		default:
			return '11';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString = function (screenSize) {
	var _n0 = rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize);
	if (!_n0.$) {
		var s = _n0.a;
		return '-' + (s + '-');
	} else {
		return '-';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass = function (_n0) {
	var screenSize = _n0.dv;
	var offsetCount = _n0.dd;
	return elm$html$Html$Attributes$class(
		'offset' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption(offsetCount)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes = function (offsets) {
	var offset_ = function (m) {
		return A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass, m);
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, offset_, offsets));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption = function (size) {
	switch (size) {
		case 0:
			return 'first';
		case 1:
			return '1';
		case 2:
			return '2';
		case 3:
			return '3';
		case 4:
			return '4';
		case 5:
			return '5';
		case 6:
			return '6';
		case 7:
			return '7';
		case 8:
			return '8';
		case 9:
			return '9';
		case 10:
			return '10';
		case 11:
			return '11';
		case 12:
			return '12';
		default:
			return 'last';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes = function (orders) {
	var order_ = function (m) {
		if (!m.$) {
			var screenSize = m.a.dv;
			var moveCount = m.a.aj;
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class(
					'order' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption(moveCount))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, order_, orders));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption = function (size) {
	switch (size) {
		case 0:
			return '0';
		case 1:
			return '1';
		case 2:
			return '2';
		case 3:
			return '3';
		case 4:
			return '4';
		case 5:
			return '5';
		case 6:
			return '6';
		case 7:
			return '7';
		case 8:
			return '8';
		case 9:
			return '9';
		case 10:
			return '10';
		case 11:
			return '11';
		default:
			return '12';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes = function (pulls) {
	var pull_ = function (m) {
		if (!m.$) {
			var screenSize = m.a.dv;
			var moveCount = m.a.aj;
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class(
					'pull' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, pull_, pulls));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes = function (pushes) {
	var push_ = function (m) {
		if (!m.$) {
			var screenSize = m.a.dv;
			var moveCount = m.a.aj;
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class(
					'push' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, push_, pushes));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption = function (align) {
	switch (align) {
		case 0:
			return 'start';
		case 1:
			return 'center';
		default:
			return 'end';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass = F2(
	function (prefix, _n0) {
		var align = _n0.cJ;
		var screenSize = _n0.dv;
		return elm$html$Html$Attributes$class(
			_Utils_ap(
				prefix,
				_Utils_ap(
					A2(
						elm$core$Maybe$withDefault,
						'',
						A2(
							elm$core$Maybe$map,
							function (v) {
								return v + '-';
							},
							rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))),
					rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption(align))));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes = F2(
	function (prefix, aligns) {
		var align = function (a) {
			return A2(
				elm$core$Maybe$map,
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass(prefix),
				a);
		};
		return A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			A2(elm$core$List$map, align, aligns));
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption = function (dir) {
	switch (dir) {
		case 1:
			return 'center';
		case 0:
			return 'left';
		default:
			return 'right';
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass = function (_n0) {
	var dir = _n0.cS;
	var size = _n0.b$;
	return elm$html$Html$Attributes$class(
		'text' + (A2(
			elm$core$Maybe$withDefault,
			'-',
			A2(
				elm$core$Maybe$map,
				function (s) {
					return '-' + (s + '-');
				},
				rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size))) + rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption(dir)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption, rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions, modifiers);
	var shouldAddDefaultXs = !elm$core$List$length(
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[options.aP, options.aN, options.aM, options.aL, options.aO])));
	return _Utils_ap(
		rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes(
			_List_fromArray(
				[
					shouldAddDefaultXs ? elm$core$Maybe$Just(
					A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, 0, 0)) : options.aP,
					options.aN,
					options.aM,
					options.aL,
					options.aO
				])),
		_Utils_ap(
			rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes(
				_List_fromArray(
					[options.bx, options.bu, options.bt, options.bs, options.bw])),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes(
					_List_fromArray(
						[options.bP, options.bN, options.bM, options.bL, options.bO])),
				_Utils_ap(
					rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes(
						_List_fromArray(
							[options.bU, options.bS, options.bR, options.bQ, options.bT])),
					_Utils_ap(
						rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes(
							_List_fromArray(
								[options.bI, options.bG, options.bF, options.bE, options.bH])),
						_Utils_ap(
							A2(
								rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
								'align-self-',
								_List_fromArray(
									[options.aZ, options.aX, options.aW, options.aV, options.aY])),
							_Utils_ap(
								function () {
									var _n0 = options.b2;
									if (!_n0.$) {
										var a = _n0.a;
										return _List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(a)
											]);
									} else {
										return _List_Nil;
									}
								}(),
								options.a0)))))));
};
var rundis$elm_bootstrap$Bootstrap$Grid$renderCol = function (column) {
	switch (column.$) {
		case 0:
			var options = column.a.bC;
			var children = column.a.cN;
			return A2(
				elm$html$Html$div,
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
		case 1:
			var e = column.a;
			return e;
		default:
			var options = column.a.bC;
			var children = column.a.cN;
			return A3(
				elm$html$Html$Keyed$node,
				'div',
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign = F2(
	function (align, options) {
		var _n0 = align.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						bj: elm$core$Maybe$Just(align)
					});
			case 1:
				return _Utils_update(
					options,
					{
						bh: elm$core$Maybe$Just(align)
					});
			case 2:
				return _Utils_update(
					options,
					{
						bg: elm$core$Maybe$Just(align)
					});
			case 3:
				return _Utils_update(
					options,
					{
						bf: elm$core$Maybe$Just(align)
					});
			default:
				return _Utils_update(
					options,
					{
						bi: elm$core$Maybe$Just(align)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign = F2(
	function (align_, options) {
		var _n0 = align_.dv;
		switch (_n0) {
			case 0:
				return _Utils_update(
					options,
					{
						ca: elm$core$Maybe$Just(align_)
					});
			case 1:
				return _Utils_update(
					options,
					{
						b8: elm$core$Maybe$Just(align_)
					});
			case 2:
				return _Utils_update(
					options,
					{
						b7: elm$core$Maybe$Just(align_)
					});
			case 3:
				return _Utils_update(
					options,
					{
						b6: elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						b9: elm$core$Maybe$Just(align_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 2:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs)
					});
			case 0:
				var align = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign, align, options);
			default:
				var align = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign, align, options);
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions = {a0: _List_Nil, bf: elm$core$Maybe$Nothing, bg: elm$core$Maybe$Nothing, bh: elm$core$Maybe$Nothing, bi: elm$core$Maybe$Nothing, bj: elm$core$Maybe$Nothing, b6: elm$core$Maybe$Nothing, b7: elm$core$Maybe$Nothing, b8: elm$core$Maybe$Nothing, b9: elm$core$Maybe$Nothing, ca: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption = function (align) {
	switch (align) {
		case 0:
			return 'start';
		case 1:
			return 'center';
		case 2:
			return 'end';
		case 3:
			return 'around';
		default:
			return 'between';
	}
};
var rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass = function (_n0) {
	var align = _n0.cJ;
	var screenSize = _n0.dv;
	return elm$html$Html$Attributes$class(
		'justify-content-' + (A2(
			elm$core$Maybe$withDefault,
			'',
			A2(
				elm$core$Maybe$map,
				function (v) {
					return v + '-';
				},
				rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption(align)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes = function (aligns) {
	var align = function (a) {
		return A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass, a);
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, align, aligns));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption, rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('row')
			]),
		_Utils_ap(
			A2(
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
				'align-items-',
				_List_fromArray(
					[options.ca, options.b8, options.b7, options.b6, options.b9])),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes(
					_List_fromArray(
						[options.bj, options.bh, options.bg, options.bf, options.bi])),
				options.a0)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$row = F2(
	function (options, cols) {
		return A2(
			elm$html$Html$div,
			rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes(options),
			A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Grid$renderCol, cols));
	});
var rundis$elm_bootstrap$Bootstrap$Tab$Item = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Tab$item = function (rec) {
	return {c0: rec.c0, c9: rec.c9, df: rec.df};
};
var rundis$elm_bootstrap$Bootstrap$Tab$Link = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Tab$link = F2(
	function (attributes, children) {
		return {a0: attributes, cN: children};
	});
var rundis$elm_bootstrap$Bootstrap$Tab$Pane = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Tab$pane = F2(
	function (attributes, children) {
		return {a0: attributes, cN: children};
	});
var rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt3 = elm$html$Html$Attributes$class('mt-3');
var author$project$Main$viewOfDebugTabItem = function (_n0) {
	return rundis$elm_bootstrap$Bootstrap$Tab$item(
		{
			c0: 'debugTabItem',
			c9: A2(
				rundis$elm_bootstrap$Bootstrap$Tab$link,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text('Debug')
					])),
			df: A2(
				rundis$elm_bootstrap$Bootstrap$Tab$pane,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt3]),
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$ChangeProgramContent('++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.'))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('Set BF Hello world program')
											]))
									]))
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$ChangeProgramContent('+++++++++++++[>+++++++++++++++++>++++++++++>+++++++++++++>++++++++++++<<<<-]>++++++.>-.>>---------.<<<.>+.>>.<<<.>-.>++.<<.>.>----------.<<.>.>++++++++++++++.<<.>-.+.<+.>>+++++++++.>+++.<<<+++.>>>-.---------.<<<----.>-.++.'))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('Set BF こんにちは世界 program')
											]))
									]))
							]))
					]))
		});
};
var author$project$BFTypes$RunningStep = 3;
var author$project$BFTypes$RunningUntilEndingLoop = 4;
var author$project$BFTypes$RunningUntilLeavingLoop = 5;
var author$project$BFTypes$tokenKindToString = function (kind) {
	switch (kind) {
		case 0:
			return 'NoOp';
		case 1:
			return 'LoopStart';
		case 2:
			return 'LoopEnd';
		case 3:
			return 'IncreaseValue';
		case 4:
			return 'DecreaseValue';
		case 5:
			return 'IncreasePointer';
		case 6:
			return 'DecreasePointer';
		case 7:
			return 'ReadInput';
		default:
			return 'PrintOutput';
	}
};
var author$project$Main$AddCustomTokenTable = {$: 4};
var author$project$Main$ChangeNoOpCommandVisibility = function (a) {
	return {$: 3, a: a};
};
var author$project$Main$CopyConvertedProgram = {$: 12};
var author$project$Main$ExecuteWithNewRunningState = function (a) {
	return {$: 3, a: a};
};
var author$project$Main$PushUpComingCustomTokenTable = function (a) {
	return {$: 7, a: a};
};
var author$project$Main$RemoveUpComingCustomTokenTable = function (a) {
	return {$: 8, a: a};
};
var author$project$Main$ResetAll = {$: 13};
var author$project$Main$ShowBFTapeAsHex = 1;
var author$project$Main$ShowBFTapeAsStr = 2;
var author$project$Main$UpdateCurrentTapePage = function (a) {
	return {$: 2, a: a};
};
var author$project$Main$UpdateHowShowBFTapeAs = function (a) {
	return {$: 11, a: a};
};
var author$project$Main$UpdateInput = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$UpdateResetConfirmationModalState = function (a) {
	return {$: 14, a: a};
};
var author$project$Main$UpdateTokenTable = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$UpdateUpComingCustomTokenTable = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var author$project$Main$UpdateUpComingCustomTokenTableName = function (a) {
	return {$: 9, a: a};
};
var author$project$Language$Ook$table = _Utils_Tuple2(
	_List_fromArray(
		[
			_Utils_Tuple2(5, 'Ook. Ook?'),
			_Utils_Tuple2(6, 'Ook? Ook.'),
			_Utils_Tuple2(3, 'Ook. Ook.'),
			_Utils_Tuple2(4, 'Ook! Ook!'),
			_Utils_Tuple2(7, 'Ook. Ook!'),
			_Utils_Tuple2(8, 'Ook! Ook.'),
			_Utils_Tuple2(1, 'Ook! Ook?'),
			_Utils_Tuple2(2, 'Ook? Ook!')
		]),
	'Ook!');
var author$project$Main$bfDefaultTokenTableList = _List_fromArray(
	[author$project$Language$BF$table, author$project$Language$Ook$table]);
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var author$project$Main$bfTokenTableList = function (model) {
	return elm$core$List$concat(
		_List_fromArray(
			[author$project$Main$bfDefaultTokenTableList, model.E]));
};
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var author$project$Main$convertIntIntoHexChar = function (value) {
	return ((0 <= value) && (value < 10)) ? A2(
		elm$core$Maybe$withDefault,
		'0',
		elm$core$List$head(
			elm$core$String$toList(
				elm$core$String$fromInt(value)))) : elm$core$Char$fromCode(
		(elm$core$Char$toCode('A') + value) - 10);
};
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var author$project$Main$convertIntIntoHexString = function (value) {
	var upperValue = (value / 16) | 0;
	var upperStr = (!upperValue) ? '' : author$project$Main$convertIntIntoHexString(upperValue);
	var lowerValue = A2(elm$core$Basics$modBy, 16, value);
	var lowerStr = elm$core$String$fromChar(
		author$project$Main$convertIntIntoHexChar(lowerValue));
	return _Utils_ap(upperStr, lowerStr);
};
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)),
			string);
	});
var author$project$Main$convertCharIntoHexString = function (value) {
	return A3(
		elm$core$String$padLeft,
		2,
		'0',
		author$project$Main$convertIntIntoHexString(value));
};
var author$project$Main$convertTapeValue = F2(
	function (showAs, value) {
		switch (showAs) {
			case 0:
				return elm$core$String$fromInt(value);
			case 1:
				return author$project$Main$convertCharIntoHexString(value);
			default:
				return elm$core$String$fromChar(
					elm$core$Char$fromCode(value));
		}
	});
var rundis$elm_bootstrap$Bootstrap$Table$CellAttr = function (a) {
	return {$: 2, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$cellAttr = function (attr_) {
	return rundis$elm_bootstrap$Bootstrap$Table$CellAttr(attr_);
};
var rundis$elm_bootstrap$Bootstrap$Table$THead = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Table$thead = F2(
	function (options, rows) {
		return {bC: options, bY: rows};
	});
var rundis$elm_bootstrap$Bootstrap$Table$Row = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$tr = F2(
	function (options, cells) {
		return rundis$elm_bootstrap$Bootstrap$Table$Row(
			{k: cells, bC: options});
	});
var rundis$elm_bootstrap$Bootstrap$Table$simpleThead = function (cells) {
	return A2(
		rundis$elm_bootstrap$Bootstrap$Table$thead,
		_List_Nil,
		_List_fromArray(
			[
				A2(rundis$elm_bootstrap$Bootstrap$Table$tr, _List_Nil, cells)
			]));
};
var rundis$elm_bootstrap$Bootstrap$Table$Small = {$: 4};
var rundis$elm_bootstrap$Bootstrap$Table$small = rundis$elm_bootstrap$Bootstrap$Table$Small;
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var elm$html$Html$table = _VirtualDom_node('table');
var rundis$elm_bootstrap$Bootstrap$Table$Inversed = {$: 0};
var rundis$elm_bootstrap$Bootstrap$Table$isResponsive = function (option) {
	if (option.$ === 5) {
		return true;
	} else {
		return false;
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$KeyedTBody = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$TBody = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$InversedRow = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$KeyedRow = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$InversedCell = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$Td = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$Th = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell = function (cell) {
	var inverseOptions = function (options) {
		return A2(
			elm$core$List$map,
			function (opt) {
				if (!opt.$) {
					var role = opt.a;
					return rundis$elm_bootstrap$Bootstrap$Table$InversedCell(role);
				} else {
					return opt;
				}
			},
			options);
	};
	if (cell.$ === 1) {
		var cellCfg = cell.a;
		return rundis$elm_bootstrap$Bootstrap$Table$Th(
			_Utils_update(
				cellCfg,
				{
					bC: inverseOptions(cellCfg.bC)
				}));
	} else {
		var cellCfg = cell.a;
		return rundis$elm_bootstrap$Bootstrap$Table$Td(
			_Utils_update(
				cellCfg,
				{
					bC: inverseOptions(cellCfg.bC)
				}));
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow = function (row) {
	var inversedOptions = function (options) {
		return A2(
			elm$core$List$map,
			function (opt) {
				if (!opt.$) {
					var role = opt.a;
					return rundis$elm_bootstrap$Bootstrap$Table$InversedRow(role);
				} else {
					return opt;
				}
			},
			options);
	};
	if (!row.$) {
		var options = row.a.bC;
		var cells = row.a.k;
		return rundis$elm_bootstrap$Bootstrap$Table$Row(
			{
				k: A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell, cells),
				bC: inversedOptions(options)
			});
	} else {
		var options = row.a.bC;
		var cells = row.a.k;
		return rundis$elm_bootstrap$Bootstrap$Table$KeyedRow(
			{
				k: A2(
					elm$core$List$map,
					function (_n1) {
						var key = _n1.a;
						var cell = _n1.b;
						return _Utils_Tuple2(
							key,
							rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell(cell));
					},
					cells),
				bC: inversedOptions(options)
			});
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTBody = F2(
	function (isTableInversed, tbody_) {
		var _n0 = _Utils_Tuple2(isTableInversed, tbody_);
		if (!_n0.a) {
			return tbody_;
		} else {
			if (!_n0.b.$) {
				var body = _n0.b.a;
				return rundis$elm_bootstrap$Bootstrap$Table$TBody(
					_Utils_update(
						body,
						{
							bY: A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow, body.bY)
						}));
			} else {
				var keyedBody = _n0.b.a;
				return rundis$elm_bootstrap$Bootstrap$Table$KeyedTBody(
					_Utils_update(
						keyedBody,
						{
							bY: A2(
								elm$core$List$map,
								function (_n1) {
									var key = _n1.a;
									var row = _n1.b;
									return _Utils_Tuple2(
										key,
										rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow(row));
								},
								keyedBody.bY)
						}));
			}
		}
	});
var rundis$elm_bootstrap$Bootstrap$Table$InversedHead = {$: 0};
var rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTHead = F2(
	function (isTableInversed, _n0) {
		var thead_ = _n0;
		var isHeadInversed = A2(
			elm$core$List$any,
			function (opt) {
				return _Utils_eq(opt, rundis$elm_bootstrap$Bootstrap$Table$InversedHead);
			},
			thead_.bC);
		return (isTableInversed || isHeadInversed) ? _Utils_update(
			thead_,
			{
				bY: A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow, thead_.bY)
			}) : thead_;
	});
var rundis$elm_bootstrap$Bootstrap$Table$maybeWrapResponsive = F2(
	function (options, table_) {
		var responsiveClass = elm$html$Html$Attributes$class(
			'table-responsive' + A2(
				elm$core$Maybe$withDefault,
				'',
				A2(
					elm$core$Maybe$map,
					function (v) {
						return '-' + v;
					},
					A2(
						elm$core$Maybe$andThen,
						rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption,
						A2(
							elm$core$Maybe$andThen,
							function (opt) {
								if (opt.$ === 5) {
									var val = opt.a;
									return val;
								} else {
									return elm$core$Maybe$Nothing;
								}
							},
							elm$core$List$head(
								A2(elm$core$List$filter, rundis$elm_bootstrap$Bootstrap$Table$isResponsive, options)))))));
		return A2(elm$core$List$any, rundis$elm_bootstrap$Bootstrap$Table$isResponsive, options) ? A2(
			elm$html$Html$div,
			_List_fromArray(
				[responsiveClass]),
			_List_fromArray(
				[table_])) : table_;
	});
var elm$html$Html$tbody = _VirtualDom_node('tbody');
var elm$html$Html$Attributes$scope = elm$html$Html$Attributes$stringProperty('scope');
var rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh = function (cell) {
	if (cell.$ === 1) {
		var cellConfig = cell.a;
		return rundis$elm_bootstrap$Bootstrap$Table$Th(
			_Utils_update(
				cellConfig,
				{
					bC: A2(
						elm$core$List$cons,
						rundis$elm_bootstrap$Bootstrap$Table$cellAttr(
							elm$html$Html$Attributes$scope('row')),
						cellConfig.bC)
				}));
	} else {
		return cell;
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell = function (row) {
	if (!row.$) {
		var options = row.a.bC;
		var cells = row.a.k;
		if (!cells.b) {
			return row;
		} else {
			var first = cells.a;
			var rest = cells.b;
			return rundis$elm_bootstrap$Bootstrap$Table$Row(
				{
					k: A2(
						elm$core$List$cons,
						rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh(first),
						rest),
					bC: options
				});
		}
	} else {
		var options = row.a.bC;
		var cells = row.a.k;
		if (!cells.b) {
			return row;
		} else {
			var _n3 = cells.a;
			var firstKey = _n3.a;
			var first = _n3.b;
			var rest = cells.b;
			return rundis$elm_bootstrap$Bootstrap$Table$KeyedRow(
				{
					k: A2(
						elm$core$List$cons,
						_Utils_Tuple2(
							firstKey,
							rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh(first)),
						rest),
					bC: options
				});
		}
	}
};
var elm$html$Html$tr = _VirtualDom_node('tr');
var elm$html$Html$td = _VirtualDom_node('td');
var elm$html$Html$th = _VirtualDom_node('th');
var rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass = F2(
	function (prefix, role) {
		return elm$html$Html$Attributes$class(
			prefix + ('-' + function () {
				switch (role) {
					case 0:
						return 'primary';
					case 1:
						return 'secondary';
					case 2:
						return 'success';
					case 3:
						return 'info';
					case 4:
						return 'warning';
					case 5:
						return 'danger';
					case 6:
						return 'light';
					default:
						return 'dark';
				}
			}()));
	});
var rundis$elm_bootstrap$Bootstrap$Table$cellAttribute = function (option) {
	switch (option.$) {
		case 0:
			if (!option.a.$) {
				var role = option.a.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'table', role);
			} else {
				var _n1 = option.a;
				return elm$html$Html$Attributes$class('table-active');
			}
		case 1:
			if (!option.a.$) {
				var role = option.a.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg-', role);
			} else {
				var _n2 = option.a;
				return elm$html$Html$Attributes$class('bg-active');
			}
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$cellAttributes = function (options) {
	return A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$cellAttribute, options);
};
var rundis$elm_bootstrap$Bootstrap$Table$renderCell = function (cell) {
	if (!cell.$) {
		var options = cell.a.bC;
		var children = cell.a.cN;
		return A2(
			elm$html$Html$td,
			rundis$elm_bootstrap$Bootstrap$Table$cellAttributes(options),
			children);
	} else {
		var options = cell.a.bC;
		var children = cell.a.cN;
		return A2(
			elm$html$Html$th,
			rundis$elm_bootstrap$Bootstrap$Table$cellAttributes(options),
			children);
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$rowClass = function (option) {
	switch (option.$) {
		case 0:
			if (!option.a.$) {
				var role_ = option.a.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'table', role_);
			} else {
				var _n1 = option.a;
				return elm$html$Html$Attributes$class('table-active');
			}
		case 1:
			if (!option.a.$) {
				var role_ = option.a.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role_);
			} else {
				var _n2 = option.a;
				return elm$html$Html$Attributes$class('bg-active');
			}
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$rowAttributes = function (options) {
	return A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$rowClass, options);
};
var rundis$elm_bootstrap$Bootstrap$Table$renderRow = function (row) {
	if (!row.$) {
		var options = row.a.bC;
		var cells = row.a.k;
		return A2(
			elm$html$Html$tr,
			rundis$elm_bootstrap$Bootstrap$Table$rowAttributes(options),
			A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$renderCell, cells));
	} else {
		var options = row.a.bC;
		var cells = row.a.k;
		return A3(
			elm$html$Html$Keyed$node,
			'tr',
			rundis$elm_bootstrap$Bootstrap$Table$rowAttributes(options),
			A2(
				elm$core$List$map,
				function (_n1) {
					var key = _n1.a;
					var cell = _n1.b;
					return _Utils_Tuple2(
						key,
						rundis$elm_bootstrap$Bootstrap$Table$renderCell(cell));
				},
				cells));
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$renderTBody = function (body) {
	if (!body.$) {
		var attributes = body.a.a0;
		var rows = body.a.bY;
		return A2(
			elm$html$Html$tbody,
			attributes,
			A2(
				elm$core$List$map,
				function (row) {
					return rundis$elm_bootstrap$Bootstrap$Table$renderRow(
						rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell(row));
				},
				rows));
	} else {
		var attributes = body.a.a0;
		var rows = body.a.bY;
		return A3(
			elm$html$Html$Keyed$node,
			'tbody',
			attributes,
			A2(
				elm$core$List$map,
				function (_n1) {
					var key = _n1.a;
					var row = _n1.b;
					return _Utils_Tuple2(
						key,
						rundis$elm_bootstrap$Bootstrap$Table$renderRow(
							rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell(row)));
				},
				rows));
	}
};
var elm$html$Html$thead = _VirtualDom_node('thead');
var rundis$elm_bootstrap$Bootstrap$Table$theadAttribute = function (option) {
	switch (option.$) {
		case 0:
			return elm$html$Html$Attributes$class('thead-dark');
		case 1:
			return elm$html$Html$Attributes$class('thead-default');
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$theadAttributes = function (options) {
	return A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$theadAttribute, options);
};
var rundis$elm_bootstrap$Bootstrap$Table$renderTHead = function (_n0) {
	var options = _n0.bC;
	var rows = _n0.bY;
	return A2(
		elm$html$Html$thead,
		rundis$elm_bootstrap$Bootstrap$Table$theadAttributes(options),
		A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$renderRow, rows));
};
var rundis$elm_bootstrap$Bootstrap$Table$tableClass = function (option) {
	switch (option.$) {
		case 0:
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class('table-dark'));
		case 1:
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class('table-striped'));
		case 2:
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class('table-bordered'));
		case 3:
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class('table-hover'));
		case 4:
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class('table-sm'));
		case 5:
			return elm$core$Maybe$Nothing;
		case 6:
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class('table-reflow'));
		default:
			var attr_ = option.a;
			return elm$core$Maybe$Just(attr_);
	}
};
var rundis$elm_bootstrap$Bootstrap$Table$tableAttributes = function (options) {
	return A2(
		elm$core$List$cons,
		elm$html$Html$Attributes$class('table'),
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Table$tableClass, options)));
};
var rundis$elm_bootstrap$Bootstrap$Table$table = function (rec) {
	var isInversed = A2(
		elm$core$List$any,
		function (opt) {
			return _Utils_eq(opt, rundis$elm_bootstrap$Bootstrap$Table$Inversed);
		},
		rec.bC);
	var classOptions = A2(
		elm$core$List$filter,
		function (opt) {
			return !rundis$elm_bootstrap$Bootstrap$Table$isResponsive(opt);
		},
		rec.bC);
	return A2(
		rundis$elm_bootstrap$Bootstrap$Table$maybeWrapResponsive,
		rec.bC,
		A2(
			elm$html$Html$table,
			rundis$elm_bootstrap$Bootstrap$Table$tableAttributes(classOptions),
			_List_fromArray(
				[
					rundis$elm_bootstrap$Bootstrap$Table$renderTHead(
					A2(rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTHead, isInversed, rec.d1)),
					rundis$elm_bootstrap$Bootstrap$Table$renderTBody(
					A2(rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTBody, isInversed, rec.d0))
				])));
};
var rundis$elm_bootstrap$Bootstrap$Table$tbody = F2(
	function (attributes, rows) {
		return rundis$elm_bootstrap$Bootstrap$Table$TBody(
			{a0: attributes, bY: rows});
	});
var rundis$elm_bootstrap$Bootstrap$Table$td = F2(
	function (options, children) {
		return rundis$elm_bootstrap$Bootstrap$Table$Td(
			{cN: children, bC: options});
	});
var rundis$elm_bootstrap$Bootstrap$Table$th = F2(
	function (options, children) {
		return rundis$elm_bootstrap$Bootstrap$Table$Th(
			{cN: children, bC: options});
	});
var author$project$Main$tableViewOfTapeLine = F2(
	function (model, line) {
		var list = A2(elm$core$List$range, 0, 15);
		var header = A2(
			elm$core$List$map,
			function (idx) {
				var address = (16 * line) + idx;
				var addressStr = (_Utils_cmp(address, author$project$BFTypes$tapeSize) < 0) ? elm$core$String$fromInt(address) : '';
				var isCurrentAddress = _Utils_eq(model.e.w, address);
				return A2(
					rundis$elm_bootstrap$Bootstrap$Table$th,
					_List_fromArray(
						[
							rundis$elm_bootstrap$Bootstrap$Table$cellAttr(
							elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('text-success', isCurrentAddress)
									])))
						]),
					_List_fromArray(
						[
							elm$html$Html$text(addressStr)
						]));
			},
			list);
		var body = A2(
			elm$core$List$map,
			function (idx) {
				var address = (16 * line) + idx;
				var value = A2(
					elm$core$Maybe$withDefault,
					'',
					A2(
						elm$core$Maybe$map,
						author$project$Main$convertTapeValue(model.W),
						A2(author$project$BFExecutor$getMaybeTapeValue, model.e.A, address)));
				return A2(
					rundis$elm_bootstrap$Bootstrap$Table$td,
					_List_Nil,
					_List_fromArray(
						[
							elm$html$Html$text(value)
						]));
			},
			list);
		return rundis$elm_bootstrap$Bootstrap$Table$table(
			{
				bC: _List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Table$small]),
				d0: A2(
					rundis$elm_bootstrap$Bootstrap$Table$tbody,
					_List_Nil,
					_List_fromArray(
						[
							A2(rundis$elm_bootstrap$Bootstrap$Table$tr, _List_Nil, body)
						])),
				d1: rundis$elm_bootstrap$Bootstrap$Table$simpleThead(header)
			});
	});
var author$project$Main$ChangeCommandPopoverState = F2(
	function (a, b) {
		return {$: 16, a: a, b: b};
	});
var elm$html$Html$br = _VirtualDom_node('br');
var rundis$elm_bootstrap$Bootstrap$Popover$Config = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Popover$Content = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Popover$content = F3(
	function (attributes, children, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{
				dK: elm$core$Maybe$Just(
					A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class('popover-body'),
							attributes),
						children))
			});
	});
var elm$html$Html$h4 = _VirtualDom_node('h4');
var rundis$elm_bootstrap$Bootstrap$Popover$Title = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Popover$titlePrivate = F4(
	function (elemFn, attributes, children, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{
				d2: elm$core$Maybe$Just(
					A2(
						elemFn,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class('popover-header'),
							attributes),
						children))
			});
	});
var rundis$elm_bootstrap$Bootstrap$Popover$titleH4 = rundis$elm_bootstrap$Bootstrap$Popover$titlePrivate(elm$html$Html$h4);
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var elm$core$String$fromFloat = _String_fromNumber;
var rundis$elm_bootstrap$Bootstrap$Popover$calculatePos = F2(
	function (pos, _n0) {
		var rect = _n0.cx;
		var offsetWidth = _n0.bv;
		var offsetHeight = _n0.cs;
		switch (pos) {
			case 3:
				return {
					aa: elm$core$Maybe$Nothing,
					ab: elm$core$Maybe$Just((offsetHeight / 2) - 12),
					Q: (-offsetWidth) - 10,
					X: (rect.ae / 2) - (offsetHeight / 2)
				};
			case 1:
				return {
					aa: elm$core$Maybe$Nothing,
					ab: elm$core$Maybe$Just((offsetHeight / 2) - 12),
					Q: rect.ao,
					X: (rect.ae / 2) - (offsetHeight / 2)
				};
			case 0:
				return {
					aa: elm$core$Maybe$Just((offsetWidth / 2) - 12),
					ab: elm$core$Maybe$Nothing,
					Q: (rect.ao / 2) - (offsetWidth / 2),
					X: (-offsetHeight) - 10
				};
			default:
				return {
					aa: elm$core$Maybe$Just((offsetWidth / 2) - 12),
					ab: elm$core$Maybe$Nothing,
					Q: (rect.ao / 2) - (offsetWidth / 2),
					X: rect.ae
				};
		}
	});
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var rundis$elm_bootstrap$Bootstrap$Popover$directionAttr = function (position) {
	return A2(
		elm$html$Html$Attributes$attribute,
		'x-placement',
		function () {
			switch (position) {
				case 3:
					return 'left';
				case 1:
					return 'right';
				case 0:
					return 'top';
				default:
					return 'bottom';
			}
		}());
};
var rundis$elm_bootstrap$Bootstrap$Popover$positionClass = function (position) {
	switch (position) {
		case 3:
			return _Utils_Tuple2('bs-popover-left', true);
		case 1:
			return _Utils_Tuple2('bs-popover-right', true);
		case 0:
			return _Utils_Tuple2('bs-popover-top', true);
		default:
			return _Utils_Tuple2('bs-popover-bottom', true);
	}
};
var rundis$elm_bootstrap$Bootstrap$Popover$popoverView = F2(
	function (_n0, _n1) {
		var isActive = _n0.ag;
		var domState = _n0.ck;
		var conf = _n1;
		var px = function (f) {
			return elm$core$String$fromFloat(f) + 'px';
		};
		var pos = A2(rundis$elm_bootstrap$Bootstrap$Popover$calculatePos, conf.F, domState);
		var styles = isActive ? _List_fromArray(
			[
				A2(
				elm$html$Html$Attributes$style,
				'left',
				px(pos.Q)),
				A2(
				elm$html$Html$Attributes$style,
				'top',
				px(pos.X)),
				A2(elm$html$Html$Attributes$style, 'display', 'inline-block'),
				A2(
				elm$html$Html$Attributes$style,
				'width',
				px(domState.bv))
			]) : _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'left', '-5000px'),
				A2(elm$html$Html$Attributes$style, 'top', '-5000px')
			]);
		var arrowStyles = A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					A2(
					elm$core$Maybe$map,
					function (t) {
						return A2(
							elm$html$Html$Attributes$style,
							'top',
							px(t));
					},
					pos.ab),
					A2(
					elm$core$Maybe$map,
					function (l) {
						return A2(
							elm$html$Html$Attributes$style,
							'left',
							px(l));
					},
					pos.aa)
				]));
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('popover', true),
								_Utils_Tuple2('fade', true),
								_Utils_Tuple2('show', isActive),
								rundis$elm_bootstrap$Bootstrap$Popover$positionClass(conf.F)
							])),
						rundis$elm_bootstrap$Bootstrap$Popover$directionAttr(conf.F)
					]),
				styles),
			A2(
				elm$core$List$filterMap,
				elm$core$Basics$identity,
				_List_fromArray(
					[
						elm$core$Maybe$Just(
						A2(
							elm$html$Html$div,
							A2(
								elm$core$List$cons,
								elm$html$Html$Attributes$class('arrow'),
								arrowStyles),
							_List_Nil)),
						A2(
						elm$core$Maybe$map,
						function (_n2) {
							var t = _n2;
							return t;
						},
						conf.d2),
						A2(
						elm$core$Maybe$map,
						function (_n3) {
							var c = _n3;
							return c;
						},
						conf.dK)
					])));
	});
var rundis$elm_bootstrap$Bootstrap$Popover$view = F2(
	function (state, conf) {
		var triggerElement = conf.dD;
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'position', 'relative'),
					A2(elm$html$Html$Attributes$style, 'display', 'inline-block')
				]),
			_List_fromArray(
				[
					triggerElement,
					A2(rundis$elm_bootstrap$Bootstrap$Popover$popoverView, state, conf)
				]));
	});
var author$project$Main$commandPopoverView = F4(
	function (token, state, pos, config) {
		return elm$core$List$singleton(
			A2(
				rundis$elm_bootstrap$Bootstrap$Popover$view,
				state,
				A3(
					rundis$elm_bootstrap$Bootstrap$Popover$content,
					_List_Nil,
					elm$core$List$concat(
						_List_fromArray(
							[
								_List_fromArray(
								[
									elm$html$Html$text('Value: '),
									elm$html$Html$text(token.d5),
									A2(elm$html$Html$br, _List_Nil, _List_Nil),
									elm$html$Html$text('Position: '),
									elm$html$Html$text(
									A2(
										elm$core$String$join,
										', ',
										A2(
											elm$core$List$map,
											elm$core$String$fromInt,
											elm$core$List$reverse(pos))))
								]),
								function () {
								var _n0 = token.bd;
								if (_n0.$ === 1) {
									return _List_Nil;
								} else {
									var error = _n0.a;
									return _List_fromArray(
										[
											A2(elm$html$Html$br, _List_Nil, _List_Nil),
											elm$html$Html$text('Parse Error: '),
											elm$html$Html$text(
											author$project$BFTypes$bfParseErrorToString(error))
										]);
								}
							}()
							])),
					A3(
						rundis$elm_bootstrap$Bootstrap$Popover$titleH4,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text('Command: '),
								elm$html$Html$text(
								author$project$BFTypes$tokenKindToString(token.c6))
							]),
						config))));
	});
var elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var elm$core$Array$indexedMap = F2(
	function (func, _n0) {
		var len = _n0.a;
		var tree = _n0.c;
		var tail = _n0.d;
		var initialBuilder = {
			i: _List_Nil,
			f: 0,
			h: A3(
				elm$core$Elm$JsArray$indexedMap,
				func,
				elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (!node.$) {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.f * elm$core$Array$branchFactor;
					var mappedLeaf = elm$core$Array$Leaf(
						A3(elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						i: A2(elm$core$List$cons, mappedLeaf, builder.i),
						f: builder.f + 1,
						h: builder.h
					};
				}
			});
		return A2(
			elm$core$Array$builderToArray,
			true,
			A3(elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2(elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var elm$core$List$repeat = F2(
	function (n, value) {
		return A3(elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var elm$html$Html$span = _VirtualDom_node('span');
var rundis$elm_bootstrap$Bootstrap$Popover$Top = 0;
var rundis$elm_bootstrap$Bootstrap$Popover$config = function (triggerElement) {
	return {dK: elm$core$Maybe$Nothing, F: 0, d2: elm$core$Maybe$Nothing, dD: triggerElement};
};
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$map3 = _Json_map3;
var rundis$elm_bootstrap$Bootstrap$Popover$DOMState = F3(
	function (rect, offsetWidth, offsetHeight) {
		return {cs: offsetHeight, bv: offsetWidth, cx: rect};
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$json$Json$Decode$fail = _Json_fail;
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['className']),
	elm$json$Json$Decode$string);
var rundis$elm_bootstrap$Bootstrap$Popover$isPopover = A2(
	elm$json$Json$Decode$andThen,
	function (_class) {
		return A2(elm$core$String$contains, 'popover', _class) ? elm$json$Json$Decode$succeed(true) : elm$json$Json$Decode$succeed(false);
	},
	rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className);
var rundis$elm_bootstrap$Bootstrap$Popover$popper = F2(
	function (path, decoder) {
		return elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					elm$json$Json$Decode$andThen,
					function (res) {
						return res ? A2(
							elm$json$Json$Decode$at,
							_Utils_ap(
								path,
								_List_fromArray(
									['nextSibling'])),
							decoder) : elm$json$Json$Decode$fail('');
					},
					A2(
						elm$json$Json$Decode$at,
						_Utils_ap(
							path,
							_List_fromArray(
								['nextSibling'])),
						rundis$elm_bootstrap$Bootstrap$Popover$isPopover)),
					A2(
					elm$json$Json$Decode$andThen,
					function (_n0) {
						return A2(
							rundis$elm_bootstrap$Bootstrap$Popover$popper,
							_Utils_ap(
								path,
								_List_fromArray(
									['parentElement'])),
							decoder);
					},
					A2(
						elm$json$Json$Decode$at,
						_Utils_ap(
							path,
							_List_fromArray(
								['parentElement'])),
						rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className)),
					elm$json$Json$Decode$fail('No popover found')
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Popover$isTrigger = A2(
	elm$json$Json$Decode$andThen,
	function (_class) {
		return A2(elm$core$String$contains, 'popover-trigger', _class) ? elm$json$Json$Decode$succeed(true) : elm$json$Json$Decode$succeed(false);
	},
	rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className);
var elm$json$Json$Decode$float = _Json_decodeFloat;
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetHeight = A2(elm$json$Json$Decode$field, 'offsetHeight', elm$json$Json$Decode$float);
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetWidth = A2(elm$json$Json$Decode$field, 'offsetWidth', elm$json$Json$Decode$float);
var elm$json$Json$Decode$map4 = _Json_map4;
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetLeft = A2(elm$json$Json$Decode$field, 'offsetLeft', elm$json$Json$Decode$float);
var elm$json$Json$Decode$null = _Json_decodeNull;
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetParent = F2(
	function (x, decoder) {
		return elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					elm$json$Json$Decode$field,
					'offsetParent',
					elm$json$Json$Decode$null(x)),
					A2(elm$json$Json$Decode$field, 'offsetParent', decoder)
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetTop = A2(elm$json$Json$Decode$field, 'offsetTop', elm$json$Json$Decode$float);
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollLeft = A2(elm$json$Json$Decode$field, 'scrollLeft', elm$json$Json$Decode$float);
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollTop = A2(elm$json$Json$Decode$field, 'scrollTop', elm$json$Json$Decode$float);
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$position = F2(
	function (x, y) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (_n0) {
				var x_ = _n0.a;
				var y_ = _n0.b;
				return A2(
					rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetParent,
					_Utils_Tuple2(x_, y_),
					A2(rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$position, x_, y_));
			},
			A5(
				elm$json$Json$Decode$map4,
				F4(
					function (scrollLeft_, scrollTop_, offsetLeft_, offsetTop_) {
						return _Utils_Tuple2((x + offsetLeft_) - scrollLeft_, (y + offsetTop_) - scrollTop_);
					}),
				rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollLeft,
				rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollTop,
				rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetLeft,
				rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetTop));
	});
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$boundingArea = A4(
	elm$json$Json$Decode$map3,
	F3(
		function (_n0, width, height) {
			var x = _n0.a;
			var y = _n0.b;
			return {ae: height, Q: x, X: y, ao: width};
		}),
	A2(rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$position, 0, 0),
	rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetWidth,
	rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetHeight);
var rundis$elm_bootstrap$Bootstrap$Popover$trigger = function (path) {
	return elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				elm$json$Json$Decode$andThen,
				function (res) {
					return res ? A2(elm$json$Json$Decode$at, path, rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$boundingArea) : elm$json$Json$Decode$fail('');
				},
				A2(elm$json$Json$Decode$at, path, rundis$elm_bootstrap$Bootstrap$Popover$isTrigger)),
				A2(
				elm$json$Json$Decode$andThen,
				function (_n0) {
					return rundis$elm_bootstrap$Bootstrap$Popover$trigger(
						_Utils_ap(
							path,
							_List_fromArray(
								['parentElement'])));
				},
				A2(
					elm$json$Json$Decode$at,
					_Utils_ap(
						path,
						_List_fromArray(
							['parentElement'])),
					rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className)),
				elm$json$Json$Decode$fail('No trigger found')
			]));
};
var rundis$elm_bootstrap$Bootstrap$Popover$stateDecoder = A4(
	elm$json$Json$Decode$map3,
	rundis$elm_bootstrap$Bootstrap$Popover$DOMState,
	rundis$elm_bootstrap$Bootstrap$Popover$trigger(
		_List_fromArray(
			['target'])),
	A2(
		rundis$elm_bootstrap$Bootstrap$Popover$popper,
		_List_fromArray(
			['target']),
		rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetWidth),
	A2(
		rundis$elm_bootstrap$Bootstrap$Popover$popper,
		_List_fromArray(
			['target']),
		rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetHeight));
var rundis$elm_bootstrap$Bootstrap$Popover$toggleState = F2(
	function (_n0, toMsg) {
		var state = _n0;
		var isActive = state.ag;
		return A2(
			elm$json$Json$Decode$andThen,
			function (v) {
				return elm$json$Json$Decode$succeed(
					toMsg(
						(!isActive) ? {ck: v, ag: true} : _Utils_update(
							state,
							{ag: false})));
			},
			rundis$elm_bootstrap$Bootstrap$Popover$stateDecoder);
	});
var rundis$elm_bootstrap$Bootstrap$Popover$onClick = F2(
	function (state, toMsg) {
		return _List_fromArray(
			[
				elm$html$Html$Attributes$class('popover-trigger'),
				A2(
				elm$html$Html$Events$on,
				'click',
				A2(rundis$elm_bootstrap$Bootstrap$Popover$toggleState, state, toMsg))
			]);
	});
var author$project$Main$viewOfBFCommand = F3(
	function (model, pos, cmd) {
		var spacing = A2(
			elm$html$Html$span,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('ml-2')
				]),
			_List_Nil);
		var isCurrentPopoverCommand = _Utils_eq(model.x.bK, pos);
		var isCurrentCommand = _Utils_eq(model.e.a8, pos);
		var depth = elm$core$List$length(pos) - 1;
		var brWithSpacings = function (indent) {
			return A2(
				elm$core$List$cons,
				A2(elm$html$Html$br, _List_Nil, _List_Nil),
				A2(elm$core$List$repeat, indent, spacing));
		};
		if (!cmd.$) {
			var token = cmd.a;
			var isError = function () {
				var _n2 = token.bd;
				if (!_n2.$) {
					return true;
				} else {
					return false;
				}
			}();
			var _n1 = token.c6;
			if (!_n1) {
				var visible = isError || model.M;
				return ((token.d5 === '\n') && visible) ? brWithSpacings(depth) : elm$core$List$singleton(
					A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('my-hidden-popover', !isCurrentPopoverCommand),
										_Utils_Tuple2('d-inline-block', true)
									]))
							]),
						A4(
							author$project$Main$commandPopoverView,
							token,
							model.x.U,
							pos,
							rundis$elm_bootstrap$Bootstrap$Popover$config(
								A2(
									elm$html$Html$span,
									A2(
										elm$core$List$cons,
										elm$html$Html$Attributes$classList(
											_List_fromArray(
												[
													_Utils_Tuple2('text-muted', !isError),
													_Utils_Tuple2('text-danger', isError),
													_Utils_Tuple2('font-weight-bold', isError),
													_Utils_Tuple2('d-none', !visible)
												])),
										A2(
											rundis$elm_bootstrap$Bootstrap$Popover$onClick,
											model.x.U,
											author$project$Main$ChangeCommandPopoverState(pos))),
									_List_fromArray(
										[
											elm$html$Html$text(token.d5)
										]))))));
			} else {
				var displayValue = A2(
					elm$core$Maybe$withDefault,
					_Utils_Tuple2(token.c6, token.d5),
					elm$core$List$head(
						A2(
							elm$core$List$filter,
							function (table) {
								return _Utils_eq(token.c6, table.a);
							},
							model.q.p.a))).b;
				return elm$core$List$singleton(
					A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('my-hidden-popover', !isCurrentPopoverCommand),
										_Utils_Tuple2('d-inline-block', true)
									]))
							]),
						A4(
							author$project$Main$commandPopoverView,
							token,
							model.x.U,
							pos,
							rundis$elm_bootstrap$Bootstrap$Popover$config(
								A2(
									elm$html$Html$span,
									A2(
										elm$core$List$cons,
										elm$html$Html$Attributes$classList(
											_List_fromArray(
												[
													_Utils_Tuple2('text-dark', (!isCurrentCommand) && (!isError)),
													_Utils_Tuple2('text-success', isCurrentCommand),
													_Utils_Tuple2('text-danger', isError),
													_Utils_Tuple2('font-weight-bold', true)
												])),
										A2(
											rundis$elm_bootstrap$Bootstrap$Popover$onClick,
											model.x.U,
											author$project$Main$ChangeCommandPopoverState(pos))),
									_List_fromArray(
										[
											elm$html$Html$text(displayValue)
										]))))));
			}
		} else {
			var commands = cmd.a;
			var children = A3(author$project$Main$viewOfBFCommands, model, pos, commands);
			return elm$core$List$concat(
				_List_fromArray(
					[
						brWithSpacings(depth + 1),
						children,
						brWithSpacings(depth)
					]));
		}
	});
var author$project$Main$viewOfBFCommands = F3(
	function (model, pos, cmds) {
		return elm$core$List$concat(
			elm$core$Array$toList(
				A2(
					elm$core$Array$indexedMap,
					function (idx) {
						return A2(
							author$project$Main$viewOfBFCommand,
							model,
							A2(elm$core$List$cons, idx, pos));
					},
					cmds)));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var rundis$elm_bootstrap$Bootstrap$Dropdown$DropdownItem = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Dropdown$buttonItem = F2(
	function (attributes, children) {
		return A2(
			elm$html$Html$button,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$type_('button'),
						elm$html$Html$Attributes$class('dropdown-item')
					]),
				attributes),
			children);
	});
var author$project$Main$viewOfBFTokenTableItem = F2(
	function (msg, table) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Dropdown$buttonItem,
			_List_fromArray(
				[
					elm$html$Html$Events$onClick(
					msg(table))
				]),
			_List_fromArray(
				[
					elm$html$Html$text(table.b)
				]));
	});
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						elm$core$List$cons,
						sep,
						A2(elm$core$List$cons, x, rest));
				});
			var spersed = A3(elm$core$List$foldr, step, _List_Nil, tl);
			return A2(elm$core$List$cons, hd, spersed);
		}
	});
var elm$html$Html$p = _VirtualDom_node('p');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var elm$url$Url$percentDecode = _Url_percentDecode;
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Danger = 5;
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Button$danger = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled(5));
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Outlined = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary = 0;
var rundis$elm_bootstrap$Bootstrap$Button$outlinePrimary = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Outlined(0));
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Secondary = 1;
var rundis$elm_bootstrap$Bootstrap$Button$outlineSecondary = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Outlined(1));
var rundis$elm_bootstrap$Bootstrap$Button$primary = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled(0));
var rundis$elm_bootstrap$Bootstrap$Button$secondary = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled(1));
var rundis$elm_bootstrap$Bootstrap$General$Internal$SM = 1;
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Size = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Button$small = rundis$elm_bootstrap$Bootstrap$Internal$Button$Size(1);
var elm$html$Html$input = _VirtualDom_node('input');
var elm$html$Html$label = _VirtualDom_node('label');
var elm$html$Html$Attributes$autocomplete = function (bool) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var elm$html$Html$Attributes$checked = elm$html$Html$Attributes$boolProperty('checked');
var rundis$elm_bootstrap$Bootstrap$Button$radioButton = F3(
	function (checked, options, children) {
		var hideRadio = A2(elm$html$Html$Attributes$attribute, 'data-toggle', 'button');
		return A2(
			elm$html$Html$label,
			A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('active', checked)
						])),
				A2(
					elm$core$List$cons,
					hideRadio,
					rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options))),
			A2(
				elm$core$List$cons,
				A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$type_('radio'),
							elm$html$Html$Attributes$checked(checked),
							elm$html$Html$Attributes$autocomplete(false)
						]),
					_List_Nil),
				children));
	});
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$RadioButtonItem = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton = F3(
	function (checked, options, children) {
		return A3(rundis$elm_bootstrap$Bootstrap$Button$radioButton, checked, options, children);
	});
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$GroupItem = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 0:
				var size = modifier.a;
				return _Utils_update(
					options,
					{
						b$: elm$core$Maybe$Just(size)
					});
			case 1:
				return _Utils_update(
					options,
					{cc: true});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$defaultOptions = {a0: _List_Nil, b$: elm$core$Maybe$Nothing, cc: false};
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$groupAttributes = F2(
	function (toggle, modifiers) {
		var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$ButtonGroup$applyModifier, rundis$elm_bootstrap$Bootstrap$ButtonGroup$defaultOptions, modifiers);
		return _Utils_ap(
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$attribute, 'role', 'group'),
					elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('btn-group', true),
							_Utils_Tuple2('btn-group-toggle', toggle),
							_Utils_Tuple2('btn-group-vertical', options.cc)
						])),
					A2(elm$html$Html$Attributes$attribute, 'data-toggle', 'buttons')
				]),
			_Utils_ap(
				function () {
					var _n0 = A2(elm$core$Maybe$andThen, rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption, options.b$);
					if (!_n0.$) {
						var s = _n0.a;
						return _List_fromArray(
							[
								elm$html$Html$Attributes$class('btn-group-' + s)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				options.a0));
	});
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroupItem = F2(
	function (options, items) {
		return A2(
			elm$html$Html$div,
			A2(rundis$elm_bootstrap$Bootstrap$ButtonGroup$groupAttributes, true, options),
			A2(
				elm$core$List$map,
				function (_n0) {
					var elem = _n0;
					return elem;
				},
				items));
	});
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$renderGroup = function (_n0) {
	var elem = _n0;
	return elem;
};
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroup = F2(
	function (options, items) {
		return rundis$elm_bootstrap$Bootstrap$ButtonGroup$renderGroup(
			A2(rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroupItem, options, items));
	});
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$Size = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$ButtonGroup$small = rundis$elm_bootstrap$Bootstrap$ButtonGroup$Size(1);
var rundis$elm_bootstrap$Bootstrap$Card$Internal$Attrs = function (a) {
	return {$: 3, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Card$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Card$Internal$Attrs(attrs_);
};
var rundis$elm_bootstrap$Bootstrap$Card$Config = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Card$Internal$CardBlock = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$applyBlockModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 0:
				var align = option.a;
				return _Utils_update(
					options,
					{
						B: elm$core$Maybe$Just(align)
					});
			case 1:
				var role = option.a;
				return _Utils_update(
					options,
					{
						D: elm$core$Maybe$Just(role)
					});
			case 2:
				var color = option.a;
				return _Utils_update(
					options,
					{
						I: elm$core$Maybe$Just(color)
					});
			default:
				var attrs = option.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultBlockOptions = {B: elm$core$Maybe$Nothing, a0: _List_Nil, D: elm$core$Maybe$Nothing, I: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textColorClass = function (color) {
	if (color.$ === 1) {
		return elm$html$Html$Attributes$class('text-white');
	} else {
		var role = color.a;
		return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'text', role);
	}
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$blockAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Card$Internal$applyBlockModifier, rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultBlockOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('card-body')
			]),
		_Utils_ap(
			function () {
				var _n0 = options.B;
				if (!_n0.$) {
					var align = _n0.a;
					return _List_fromArray(
						[
							rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(align)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _n1 = options.D;
					if (!_n1.$) {
						var role = _n1.a;
						return _List_fromArray(
							[
								A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _n2 = options.I;
						if (!_n2.$) {
							var color = _n2.a;
							return _List_fromArray(
								[
									rundis$elm_bootstrap$Bootstrap$Internal$Text$textColorClass(color)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.a0))));
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$block = F2(
	function (options, items) {
		return rundis$elm_bootstrap$Bootstrap$Card$Internal$CardBlock(
			A2(
				elm$html$Html$div,
				rundis$elm_bootstrap$Bootstrap$Card$Internal$blockAttributes(options),
				A2(
					elm$core$List$map,
					function (_n0) {
						var e = _n0;
						return e;
					},
					items)));
	});
var rundis$elm_bootstrap$Bootstrap$Card$block = F3(
	function (options, items, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{
				cg: _Utils_ap(
					conf.cg,
					_List_fromArray(
						[
							A2(rundis$elm_bootstrap$Bootstrap$Card$Internal$block, options, items)
						]))
			});
	});
var rundis$elm_bootstrap$Bootstrap$Card$config = function (options) {
	return {cg: _List_Nil, cl: elm$core$Maybe$Nothing, bl: elm$core$Maybe$Nothing, cn: elm$core$Maybe$Nothing, co: elm$core$Maybe$Nothing, bC: options};
};
var rundis$elm_bootstrap$Bootstrap$Card$Header = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Card$headerPrivate = F4(
	function (elemFn, attributes, children, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{
				bl: elm$core$Maybe$Just(
					A2(
						elemFn,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class('card-header'),
							attributes),
						children))
			});
	});
var rundis$elm_bootstrap$Bootstrap$Card$header = rundis$elm_bootstrap$Bootstrap$Card$headerPrivate(elm$html$Html$div);
var rundis$elm_bootstrap$Bootstrap$Card$Internal$applyModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 0:
				var align = option.a;
				return _Utils_update(
					options,
					{
						B: elm$core$Maybe$Just(align)
					});
			case 1:
				var coloring = option.a;
				return _Utils_update(
					options,
					{
						D: elm$core$Maybe$Just(coloring)
					});
			case 2:
				var coloring = option.a;
				return _Utils_update(
					options,
					{
						I: elm$core$Maybe$Just(coloring)
					});
			default:
				var attrs = option.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultOptions = {B: elm$core$Maybe$Nothing, a0: _List_Nil, D: elm$core$Maybe$Nothing, I: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$cardAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Card$Internal$applyModifier, rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('card')
			]),
		_Utils_ap(
			function () {
				var _n0 = options.D;
				if (!_n0.$) {
					if (!_n0.a.$) {
						var role = _n0.a.a;
						return _List_fromArray(
							[
								A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role)
							]);
					} else {
						var role = _n0.a.a;
						return _List_fromArray(
							[
								A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'border', role)
							]);
					}
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _n1 = options.I;
					if (!_n1.$) {
						var color = _n1.a;
						return _List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$Internal$Text$textColorClass(color)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _n2 = options.B;
						if (!_n2.$) {
							var align = _n2.a;
							return _List_fromArray(
								[
									rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(align)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.a0))));
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$renderBlocks = function (blocks) {
	return A2(
		elm$core$List$map,
		function (block_) {
			if (!block_.$) {
				var e = block_.a;
				return e;
			} else {
				var e = block_.a;
				return e;
			}
		},
		blocks);
};
var rundis$elm_bootstrap$Bootstrap$Card$view = function (_n0) {
	var conf = _n0;
	return A2(
		elm$html$Html$div,
		rundis$elm_bootstrap$Bootstrap$Card$Internal$cardAttributes(conf.bC),
		_Utils_ap(
			A2(
				elm$core$List$filterMap,
				elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						elm$core$Maybe$map,
						function (_n1) {
							var e = _n1;
							return e;
						},
						conf.bl),
						A2(
						elm$core$Maybe$map,
						function (_n2) {
							var e = _n2;
							return e;
						},
						conf.co)
					])),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Card$Internal$renderBlocks(conf.cg),
				A2(
					elm$core$List$filterMap,
					elm$core$Basics$identity,
					_List_fromArray(
						[
							A2(
							elm$core$Maybe$map,
							function (_n3) {
								var e = _n3;
								return e;
							},
							conf.cl),
							A2(
							elm$core$Maybe$map,
							function (_n4) {
								var e = _n4;
								return e;
							},
							conf.cn)
						])))));
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$BlockItem = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Card$Block$custom = function (element) {
	return element;
};
var rundis$elm_bootstrap$Bootstrap$Dropdown$dropDir = function (maybeDir) {
	var toAttrs = function (dir) {
		return _List_fromArray(
			[
				elm$html$Html$Attributes$class(
				'drop' + function () {
					if (!dir) {
						return 'left';
					} else {
						return 'right';
					}
				}())
			]);
	};
	return A2(
		elm$core$Maybe$withDefault,
		_List_Nil,
		A2(elm$core$Maybe$map, toAttrs, maybeDir));
};
var rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownAttributes = F2(
	function (status, config) {
		return _Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('btn-group', true),
							_Utils_Tuple2('show', status !== 2),
							_Utils_Tuple2('dropup', config.ax)
						]))
				]),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Dropdown$dropDir(config.as),
				config.a0));
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$menuStyles = F2(
	function (_n0, config) {
		var status = _n0.r;
		var toggleSize = _n0.b3;
		var menuSize = _n0.aA;
		var px = function (n) {
			return elm$core$String$fromFloat(n) + 'px';
		};
		var translate = F3(
			function (x, y, z) {
				return 'translate3d(' + (px(x) + (',' + (px(y) + (',' + (px(z) + ')')))));
			});
		var _default = _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'top', '0'),
				A2(elm$html$Html$Attributes$style, 'left', '0')
			]);
		var _n1 = _Utils_Tuple2(config.ax, config.as);
		_n1$0:
		while (true) {
			if (!_n1.b.$) {
				if (_n1.b.a === 1) {
					if (_n1.a) {
						break _n1$0;
					} else {
						var _n2 = _n1.b.a;
						return _default;
					}
				} else {
					if (_n1.a) {
						break _n1$0;
					} else {
						var _n3 = _n1.b.a;
						return _Utils_ap(
							_default,
							_List_fromArray(
								[
									A2(
									elm$html$Html$Attributes$style,
									'transform',
									A3(translate, (-toggleSize.ao) - menuSize.ao, 0, 0))
								]));
					}
				}
			} else {
				if (_n1.a) {
					break _n1$0;
				} else {
					return _Utils_ap(
						_default,
						_List_fromArray(
							[
								A2(
								elm$html$Html$Attributes$style,
								'transform',
								A3(translate, -toggleSize.ao, toggleSize.ae, 0))
							]));
				}
			}
		}
		return _Utils_ap(
			_default,
			_List_fromArray(
				[
					A2(
					elm$html$Html$Attributes$style,
					'transform',
					A3(translate, -toggleSize.ao, -menuSize.ae, 0))
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownMenu = F3(
	function (state, config, items) {
		var status = state.r;
		var menuSize = state.aA;
		var wrapperStyles = (status === 2) ? _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'height', '0'),
				A2(elm$html$Html$Attributes$style, 'overflow', 'hidden'),
				A2(elm$html$Html$Attributes$style, 'position', 'relative')
			]) : _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'position', 'relative')
			]);
		return A2(
			elm$html$Html$div,
			wrapperStyles,
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_Utils_ap(
						_List_fromArray(
							[
								elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('dropdown-menu', true),
										_Utils_Tuple2('dropdown-menu-right', config.bk),
										_Utils_Tuple2('show', true)
									]))
							]),
						_Utils_ap(
							A2(rundis$elm_bootstrap$Bootstrap$Dropdown$menuStyles, state, config),
							config.bq)),
					A2(
						elm$core$List$map,
						function (_n0) {
							var x = _n0;
							return x;
						},
						items))
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$applyModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 1:
				return _Utils_update(
					options,
					{bk: true});
			case 0:
				return _Utils_update(
					options,
					{ax: true});
			case 4:
				var attrs_ = option.a;
				return _Utils_update(
					options,
					{a0: attrs_});
			case 2:
				var dir = option.a;
				return _Utils_update(
					options,
					{
						as: elm$core$Maybe$Just(dir)
					});
			default:
				var attrs_ = option.a;
				return _Utils_update(
					options,
					{bq: attrs_});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$defaultOptions = {a0: _List_Nil, as: elm$core$Maybe$Nothing, bk: false, ax: false, bq: _List_Nil};
var rundis$elm_bootstrap$Bootstrap$Dropdown$toConfig = function (options) {
	return A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Dropdown$applyModifier, rundis$elm_bootstrap$Bootstrap$Dropdown$defaultOptions, options);
};
var rundis$elm_bootstrap$Bootstrap$Dropdown$dropdown = F2(
	function (state, _n0) {
		var status = state.r;
		var toggleMsg = _n0.cG;
		var toggleButton = _n0.cF;
		var items = _n0.cq;
		var options = _n0.bC;
		var config = rundis$elm_bootstrap$Bootstrap$Dropdown$toConfig(options);
		var _n1 = toggleButton;
		var buttonFn = _n1;
		return A2(
			elm$html$Html$div,
			A2(rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownAttributes, status, config),
			_List_fromArray(
				[
					A2(buttonFn, toggleMsg, state),
					A3(rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownMenu, state, config, items)
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$DropdownToggle = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Dropdown$Open = 0;
var rundis$elm_bootstrap$Bootstrap$Dropdown$nextStatus = function (status) {
	switch (status) {
		case 0:
			return 2;
		case 1:
			return 2;
		default:
			return 0;
	}
};
var rundis$elm_bootstrap$Bootstrap$Dropdown$isToggle = A2(
	elm$json$Json$Decode$andThen,
	function (_class) {
		return A2(elm$core$String$contains, 'dropdown-toggle', _class) ? elm$json$Json$Decode$succeed(true) : elm$json$Json$Decode$succeed(false);
	},
	rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className);
var rundis$elm_bootstrap$Bootstrap$Dropdown$toggler = F2(
	function (path, decoder) {
		return elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					elm$json$Json$Decode$andThen,
					function (res) {
						return res ? A2(elm$json$Json$Decode$at, path, decoder) : elm$json$Json$Decode$fail('');
					},
					A2(elm$json$Json$Decode$at, path, rundis$elm_bootstrap$Bootstrap$Dropdown$isToggle)),
					A2(
					elm$json$Json$Decode$andThen,
					function (_n0) {
						return A2(
							rundis$elm_bootstrap$Bootstrap$Dropdown$toggler,
							_Utils_ap(
								path,
								_List_fromArray(
									['parentElement'])),
							decoder);
					},
					A2(
						elm$json$Json$Decode$at,
						_Utils_ap(
							path,
							_List_fromArray(
								['parentElement'])),
						rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className)),
					elm$json$Json$Decode$fail('No toggler found')
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$childNode = function (idx) {
	return elm$json$Json$Decode$at(
		_List_fromArray(
			[
				'childNodes',
				elm$core$String$fromInt(idx)
			]));
};
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$nextSibling = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'nextSibling', decoder);
};
var rundis$elm_bootstrap$Bootstrap$Dropdown$sizeDecoder = A3(
	elm$json$Json$Decode$map2,
	elm$core$Tuple$pair,
	A2(
		rundis$elm_bootstrap$Bootstrap$Dropdown$toggler,
		_List_fromArray(
			['target']),
		rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$boundingArea),
	A2(
		rundis$elm_bootstrap$Bootstrap$Dropdown$toggler,
		_List_fromArray(
			['target']),
		rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$nextSibling(
			A2(rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$childNode, 0, rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$boundingArea))));
var rundis$elm_bootstrap$Bootstrap$Dropdown$clickHandler = F2(
	function (toMsg, state) {
		var status = state.r;
		return A2(
			elm$json$Json$Decode$andThen,
			function (_n0) {
				var b = _n0.a;
				var m = _n0.b;
				return elm$json$Json$Decode$succeed(
					toMsg(
						{
							aA: m,
							r: rundis$elm_bootstrap$Bootstrap$Dropdown$nextStatus(status),
							b3: b
						}));
			},
			rundis$elm_bootstrap$Bootstrap$Dropdown$sizeDecoder);
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$togglePrivate = F4(
	function (buttonOptions, children, toggleMsg, state) {
		return A2(
			elm$html$Html$button,
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(buttonOptions),
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('dropdown-toggle'),
						elm$html$Html$Attributes$type_('button'),
						A2(
						elm$html$Html$Events$on,
						'click',
						A2(rundis$elm_bootstrap$Bootstrap$Dropdown$clickHandler, toggleMsg, state))
					])),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Dropdown$toggle = F2(
	function (buttonOptions, children) {
		return A2(rundis$elm_bootstrap$Bootstrap$Dropdown$togglePrivate, buttonOptions, children);
	});
var rundis$elm_bootstrap$Bootstrap$Form$Input$Attrs = function (a) {
	return {$: 9, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Form$Input$Attrs(attrs_);
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$OnInput = function (a) {
	return {$: 5, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$onInput = function (toMsg) {
	return rundis$elm_bootstrap$Bootstrap$Form$Input$OnInput(toMsg);
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$Value = function (a) {
	return {$: 4, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$value = function (value_) {
	return rundis$elm_bootstrap$Bootstrap$Form$Input$Value(value_);
};
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Addon = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$button = F2(
	function (options, children) {
		return A2(rundis$elm_bootstrap$Bootstrap$Button$button, options, children);
	});
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Config = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$config = function (input_) {
	return {a0: _List_Nil, dU: input_, cv: _List_Nil, b$: elm$core$Maybe$Nothing, cD: _List_Nil};
};
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$predecessors = F2(
	function (addons, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{cv: addons});
	});
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$small = function (_n0) {
	var conf = _n0;
	return _Utils_update(
		conf,
		{
			b$: elm$core$Maybe$Just(1)
		});
};
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$span = F2(
	function (attributes, children) {
		return A2(
			elm$html$Html$span,
			A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$class('input-group-text'),
				attributes),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$successors = F2(
	function (addons, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{cD: addons});
	});
var rundis$elm_bootstrap$Bootstrap$Form$Input$Text = 0;
var rundis$elm_bootstrap$Bootstrap$Form$Input$Input = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Form$Input$Type = function (a) {
	return {$: 2, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$create = F2(
	function (tipe, options) {
		return {
			bC: A2(
				elm$core$List$cons,
				rundis$elm_bootstrap$Bootstrap$Form$Input$Type(tipe),
				options)
		};
	});
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$html$Html$Attributes$placeholder = elm$html$Html$Attributes$stringProperty('placeholder');
var elm$html$Html$Attributes$readonly = elm$html$Html$Attributes$boolProperty('readOnly');
var rundis$elm_bootstrap$Bootstrap$Form$Input$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 0:
				var size_ = modifier.a;
				return _Utils_update(
					options,
					{
						b$: elm$core$Maybe$Just(size_)
					});
			case 1:
				var id_ = modifier.a;
				return _Utils_update(
					options,
					{
						c0: elm$core$Maybe$Just(id_)
					});
			case 2:
				var tipe = modifier.a;
				return _Utils_update(
					options,
					{aF: tipe});
			case 3:
				var val = modifier.a;
				return _Utils_update(
					options,
					{bc: val});
			case 4:
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						d5: elm$core$Maybe$Just(value_)
					});
			case 7:
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						bJ: elm$core$Maybe$Just(value_)
					});
			case 5:
				var onInput_ = modifier.a;
				return _Utils_update(
					options,
					{
						bB: elm$core$Maybe$Just(onInput_)
					});
			case 6:
				var validation_ = modifier.a;
				return _Utils_update(
					options,
					{
						cb: elm$core$Maybe$Just(validation_)
					});
			case 8:
				var val = modifier.a;
				return _Utils_update(
					options,
					{bV: val});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Form$Input$defaultOptions = {a0: _List_Nil, bc: false, c0: elm$core$Maybe$Nothing, bB: elm$core$Maybe$Nothing, bJ: elm$core$Maybe$Nothing, bV: false, b$: elm$core$Maybe$Nothing, aF: 0, cb: elm$core$Maybe$Nothing, d5: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Form$Input$sizeAttribute = function (size) {
	return A2(
		elm$core$Maybe$map,
		function (s) {
			return elm$html$Html$Attributes$class('form-control-' + s);
		},
		rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size));
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$typeAttribute = function (inputType) {
	return elm$html$Html$Attributes$type_(
		function () {
			switch (inputType) {
				case 0:
					return 'text';
				case 1:
					return 'password';
				case 2:
					return 'datetime-local';
				case 3:
					return 'date';
				case 4:
					return 'month';
				case 5:
					return 'time';
				case 6:
					return 'week';
				case 7:
					return 'number';
				case 8:
					return 'email';
				case 9:
					return 'url';
				case 10:
					return 'search';
				case 11:
					return 'tel';
				default:
					return 'color';
			}
		}());
};
var rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString = function (validation) {
	if (!validation) {
		return 'is-valid';
	} else {
		return 'is-invalid';
	}
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$validationAttribute = function (validation) {
	return elm$html$Html$Attributes$class(
		rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(validation));
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$toAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Form$Input$applyModifier, rundis$elm_bootstrap$Bootstrap$Form$Input$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('form-control'),
				elm$html$Html$Attributes$disabled(options.bc),
				elm$html$Html$Attributes$readonly(options.bV),
				rundis$elm_bootstrap$Bootstrap$Form$Input$typeAttribute(options.aF)
			]),
		_Utils_ap(
			A2(
				elm$core$List$filterMap,
				elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(elm$core$Maybe$map, elm$html$Html$Attributes$id, options.c0),
						A2(elm$core$Maybe$andThen, rundis$elm_bootstrap$Bootstrap$Form$Input$sizeAttribute, options.b$),
						A2(elm$core$Maybe$map, elm$html$Html$Attributes$value, options.d5),
						A2(elm$core$Maybe$map, elm$html$Html$Attributes$placeholder, options.bJ),
						A2(elm$core$Maybe$map, elm$html$Html$Events$onInput, options.bB),
						A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$Form$Input$validationAttribute, options.cb)
					])),
			options.a0));
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$view = function (_n0) {
	var options = _n0.bC;
	return A2(
		elm$html$Html$input,
		rundis$elm_bootstrap$Bootstrap$Form$Input$toAttributes(options),
		_List_Nil);
};
var rundis$elm_bootstrap$Bootstrap$Form$Input$input = F2(
	function (tipe, options) {
		return rundis$elm_bootstrap$Bootstrap$Form$Input$view(
			A2(rundis$elm_bootstrap$Bootstrap$Form$Input$create, tipe, options));
	});
var rundis$elm_bootstrap$Bootstrap$Form$Input$text = rundis$elm_bootstrap$Bootstrap$Form$Input$input(0);
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Input = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$input = F2(
	function (inputFn, options) {
		return inputFn(options);
	});
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$text = rundis$elm_bootstrap$Bootstrap$Form$InputGroup$input(rundis$elm_bootstrap$Bootstrap$Form$Input$text);
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$sizeAttribute = function (size) {
	return A2(
		elm$core$Maybe$map,
		function (s) {
			return elm$html$Html$Attributes$class('input-group-' + s);
		},
		rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size));
};
var rundis$elm_bootstrap$Bootstrap$Form$InputGroup$view = function (_n0) {
	var conf = _n0;
	var _n1 = conf.dU;
	var input_ = _n1;
	return A2(
		elm$html$Html$div,
		_Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('input-group')
				]),
			_Utils_ap(
				A2(
					elm$core$List$filterMap,
					elm$core$Basics$identity,
					_List_fromArray(
						[
							A2(elm$core$Maybe$andThen, rundis$elm_bootstrap$Bootstrap$Form$InputGroup$sizeAttribute, conf.b$)
						])),
				conf.a0)),
		_Utils_ap(
			A2(
				elm$core$List$map,
				function (_n2) {
					var e = _n2;
					return A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('input-group-prepend')
							]),
						_List_fromArray(
							[e]));
				},
				conf.cv),
			_Utils_ap(
				_List_fromArray(
					[input_]),
				A2(
					elm$core$List$map,
					function (_n3) {
						var e = _n3;
						return A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class('input-group-append')
								]),
							_List_fromArray(
								[e]));
					},
					conf.cD))));
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$OnInput = function (a) {
	return {$: 4, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$onInput = function (toMsg) {
	return rundis$elm_bootstrap$Bootstrap$Form$Textarea$OnInput(toMsg);
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$Rows = function (a) {
	return {$: 1, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$rows = function (rows_) {
	return rundis$elm_bootstrap$Bootstrap$Form$Textarea$Rows(rows_);
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$Textarea = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$create = function (options) {
	return {bC: options};
};
var elm$html$Html$textarea = _VirtualDom_node('textarea');
var elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		elm$core$String$fromInt(n));
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 0:
				var id_ = modifier.a;
				return _Utils_update(
					options,
					{
						c0: elm$core$Maybe$Just(id_)
					});
			case 1:
				var rows_ = modifier.a;
				return _Utils_update(
					options,
					{
						bY: elm$core$Maybe$Just(rows_)
					});
			case 2:
				return _Utils_update(
					options,
					{bc: true});
			case 3:
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						d5: elm$core$Maybe$Just(value_)
					});
			case 4:
				var onInput_ = modifier.a;
				return _Utils_update(
					options,
					{
						bB: elm$core$Maybe$Just(onInput_)
					});
			case 5:
				var validation = modifier.a;
				return _Utils_update(
					options,
					{
						cb: elm$core$Maybe$Just(validation)
					});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						a0: _Utils_ap(options.a0, attrs_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$defaultOptions = {a0: _List_Nil, bc: false, c0: elm$core$Maybe$Nothing, bB: elm$core$Maybe$Nothing, bY: elm$core$Maybe$Nothing, cb: elm$core$Maybe$Nothing, d5: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$validationAttribute = function (validation) {
	return elm$html$Html$Attributes$class(
		rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(validation));
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$toAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Form$Textarea$applyModifier, rundis$elm_bootstrap$Bootstrap$Form$Textarea$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('form-control'),
				elm$html$Html$Attributes$disabled(options.bc)
			]),
		_Utils_ap(
			A2(
				elm$core$List$filterMap,
				elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(elm$core$Maybe$map, elm$html$Html$Attributes$id, options.c0),
						A2(elm$core$Maybe$map, elm$html$Html$Attributes$rows, options.bY),
						A2(elm$core$Maybe$map, elm$html$Html$Attributes$value, options.d5),
						A2(elm$core$Maybe$map, elm$html$Html$Events$onInput, options.bB),
						A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$Form$Textarea$validationAttribute, options.cb)
					])),
			options.a0));
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$view = function (_n0) {
	var options = _n0.bC;
	return A2(
		elm$html$Html$textarea,
		rundis$elm_bootstrap$Bootstrap$Form$Textarea$toAttributes(options),
		_List_Nil);
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$textarea = A2(elm$core$Basics$composeL, rundis$elm_bootstrap$Bootstrap$Form$Textarea$view, rundis$elm_bootstrap$Bootstrap$Form$Textarea$create);
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$Value = function (a) {
	return {$: 3, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Form$Textarea$value = function (value_) {
	return rundis$elm_bootstrap$Bootstrap$Form$Textarea$Value(value_);
};
var rundis$elm_bootstrap$Bootstrap$General$Internal$LG = 3;
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col3 = 3;
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth = function (a) {
	return {$: 0, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$width = F2(
	function (size, count) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth(
			A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, size, count));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Col$lg3 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, 3, 3);
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col6 = 6;
var rundis$elm_bootstrap$Bootstrap$Grid$Col$lg6 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, 3, 6);
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col2 = 2;
var rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, 1, 2);
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col4 = 4;
var rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, 1, 4);
var rundis$elm_bootstrap$Bootstrap$Grid$Col$sm6 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, 1, 6);
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowAttrs = function (a) {
	return {$: 2, a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowAttrs(attrs_);
};
var rundis$elm_bootstrap$Bootstrap$Modal$Body = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Modal$Config = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Modal$body = F3(
	function (attributes, children, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{
				dH: elm$core$Maybe$Just(
					{a0: attributes, cN: children})
			});
	});
var rundis$elm_bootstrap$Bootstrap$Modal$config = function (closeMsg) {
	return {
		dH: elm$core$Maybe$Nothing,
		ar: closeMsg,
		cl: elm$core$Maybe$Nothing,
		bl: elm$core$Maybe$Nothing,
		bC: {ci: true, at: true, aB: elm$core$Maybe$Nothing},
		Z: elm$core$Maybe$Nothing
	};
};
var rundis$elm_bootstrap$Bootstrap$Modal$Footer = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Modal$footer = F3(
	function (attributes, children, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{
				cl: elm$core$Maybe$Just(
					{a0: attributes, cN: children})
			});
	});
var elm$html$Html$h3 = _VirtualDom_node('h3');
var rundis$elm_bootstrap$Bootstrap$Modal$Header = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Modal$header = F3(
	function (attributes, children, _n0) {
		var conf = _n0;
		return _Utils_update(
			conf,
			{
				bl: elm$core$Maybe$Just(
					{a0: attributes, cN: children})
			});
	});
var rundis$elm_bootstrap$Bootstrap$Modal$titledHeader = F3(
	function (itemFn, attributes, children) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Modal$header,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					itemFn,
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$class('modal-title'),
						attributes),
					children)
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Modal$h3 = rundis$elm_bootstrap$Bootstrap$Modal$titledHeader(elm$html$Html$h3);
var rundis$elm_bootstrap$Bootstrap$Modal$hideOnBackdropClick = F2(
	function (hide, _n0) {
		var conf = _n0;
		var options = conf.bC;
		return _Utils_update(
			conf,
			{
				bC: _Utils_update(
					options,
					{at: hide})
			});
	});
var rundis$elm_bootstrap$Bootstrap$Modal$Show = 0;
var rundis$elm_bootstrap$Bootstrap$Modal$shown = 0;
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var rundis$elm_bootstrap$Bootstrap$Modal$StartClose = 1;
var rundis$elm_bootstrap$Bootstrap$Modal$getCloseMsg = function (config_) {
	var _n0 = config_.Z;
	if (!_n0.$) {
		var animationMsg = _n0.a;
		return animationMsg(1);
	} else {
		return config_.ar;
	}
};
var rundis$elm_bootstrap$Bootstrap$Modal$isFade = function (conf) {
	return A2(
		elm$core$Maybe$withDefault,
		false,
		A2(
			elm$core$Maybe$map,
			function (_n0) {
				return true;
			},
			conf.Z));
};
var rundis$elm_bootstrap$Bootstrap$Modal$backdrop = F2(
	function (visibility, conf) {
		var attributes = function () {
			switch (visibility) {
				case 0:
					return _Utils_ap(
						_List_fromArray(
							[
								elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('modal-backdrop', true),
										_Utils_Tuple2(
										'fade',
										rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
										_Utils_Tuple2('show', true)
									]))
							]),
						conf.bC.at ? _List_fromArray(
							[
								elm$html$Html$Events$onClick(
								rundis$elm_bootstrap$Bootstrap$Modal$getCloseMsg(conf))
							]) : _List_Nil);
				case 1:
					return _List_fromArray(
						[
							elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('modal-backdrop', true),
									_Utils_Tuple2('fade', true),
									_Utils_Tuple2('show', true)
								]))
						]);
				case 2:
					return _List_fromArray(
						[
							elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('modal-backdrop', true),
									_Utils_Tuple2('fade', true),
									_Utils_Tuple2('show', false)
								]))
						]);
				default:
					return _List_fromArray(
						[
							elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('modal-backdrop', false),
									_Utils_Tuple2(
									'fade',
									rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
									_Utils_Tuple2('show', false)
								]))
						]);
			}
		}();
		return _List_fromArray(
			[
				A2(elm$html$Html$div, attributes, _List_Nil)
			]);
	});
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'target', decoder);
};
var rundis$elm_bootstrap$Bootstrap$Modal$containerClickDecoder = function (closeMsg) {
	return A2(
		elm$json$Json$Decode$andThen,
		function (c) {
			return A2(elm$core$String$contains, 'elm-bootstrap-modal', c) ? elm$json$Json$Decode$succeed(closeMsg) : elm$json$Json$Decode$fail('ignoring');
		},
		rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target(rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className));
};
var rundis$elm_bootstrap$Bootstrap$Modal$display = F2(
	function (visibility, conf) {
		switch (visibility) {
			case 0:
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'pointer-events', 'none'),
						A2(elm$html$Html$Attributes$style, 'display', 'block'),
						elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2(
								'fade',
								rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
								_Utils_Tuple2('show', true)
							]))
					]);
			case 1:
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'pointer-events', 'none'),
						A2(elm$html$Html$Attributes$style, 'display', 'block'),
						elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2('fade', true),
								_Utils_Tuple2('show', true)
							]))
					]);
			case 2:
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'pointer-events', 'none'),
						A2(elm$html$Html$Attributes$style, 'display', 'block'),
						elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2('fade', true),
								_Utils_Tuple2('show', false)
							])),
						A2(
						elm$html$Html$Events$on,
						'transitionend',
						elm$json$Json$Decode$succeed(conf.ar))
					]);
			default:
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'height', '0px'),
						A2(elm$html$Html$Attributes$style, 'display', 'block'),
						elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2(
								'fade',
								rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
								_Utils_Tuple2('show', false)
							]))
					]);
		}
	});
var rundis$elm_bootstrap$Bootstrap$Modal$modalClass = function (size) {
	var _n0 = rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size);
	if (!_n0.$) {
		var s = _n0.a;
		return _List_fromArray(
			[
				elm$html$Html$Attributes$class('modal-' + s)
			]);
	} else {
		return _List_Nil;
	}
};
var rundis$elm_bootstrap$Bootstrap$Modal$modalAttributes = function (options) {
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('modal-dialog', true),
						_Utils_Tuple2('modal-dialog-centered', options.ci)
					])),
				A2(elm$html$Html$Attributes$style, 'pointer-events', 'auto')
			]),
		A2(
			elm$core$Maybe$withDefault,
			_List_Nil,
			A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$Modal$modalClass, options.aB)));
};
var rundis$elm_bootstrap$Bootstrap$Modal$renderBody = function (maybeBody) {
	if (!maybeBody.$) {
		var cfg = maybeBody.a;
		return elm$core$Maybe$Just(
			A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$class('modal-body'),
					cfg.a0),
				cfg.cN));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var rundis$elm_bootstrap$Bootstrap$Modal$renderFooter = function (maybeFooter) {
	if (!maybeFooter.$) {
		var cfg = maybeFooter.a;
		return elm$core$Maybe$Just(
			A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$class('modal-footer'),
					cfg.a0),
				cfg.cN));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var rundis$elm_bootstrap$Bootstrap$Modal$closeButton = function (closeMsg) {
	return A2(
		elm$html$Html$button,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('close'),
				elm$html$Html$Events$onClick(closeMsg)
			]),
		_List_fromArray(
			[
				elm$html$Html$text('×')
			]));
};
var rundis$elm_bootstrap$Bootstrap$Modal$renderHeader = function (conf_) {
	var _n0 = conf_.bl;
	if (!_n0.$) {
		var cfg = _n0.a;
		return elm$core$Maybe$Just(
			A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$class('modal-header'),
					cfg.a0),
				_Utils_ap(
					cfg.cN,
					_List_fromArray(
						[
							rundis$elm_bootstrap$Bootstrap$Modal$closeButton(
							rundis$elm_bootstrap$Bootstrap$Modal$getCloseMsg(conf_))
						]))));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var rundis$elm_bootstrap$Bootstrap$Modal$view = F2(
	function (visibility, _n0) {
		var conf = _n0;
		return A2(
			elm$html$Html$div,
			_List_Nil,
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_Utils_ap(
							_List_fromArray(
								[
									elm$html$Html$Attributes$tabindex(-1)
								]),
							A2(rundis$elm_bootstrap$Bootstrap$Modal$display, visibility, conf)),
						_List_fromArray(
							[
								A2(
								elm$html$Html$div,
								_Utils_ap(
									_List_fromArray(
										[
											A2(elm$html$Html$Attributes$attribute, 'role', 'document'),
											elm$html$Html$Attributes$class('elm-bootstrap-modal')
										]),
									_Utils_ap(
										rundis$elm_bootstrap$Bootstrap$Modal$modalAttributes(conf.bC),
										conf.bC.at ? _List_fromArray(
											[
												A2(
												elm$html$Html$Events$on,
												'click',
												rundis$elm_bootstrap$Bootstrap$Modal$containerClickDecoder(conf.ar))
											]) : _List_Nil)),
								_List_fromArray(
									[
										A2(
										elm$html$Html$div,
										_List_fromArray(
											[
												elm$html$Html$Attributes$class('modal-content')
											]),
										A2(
											elm$core$List$filterMap,
											elm$core$Basics$identity,
											_List_fromArray(
												[
													rundis$elm_bootstrap$Bootstrap$Modal$renderHeader(conf),
													rundis$elm_bootstrap$Bootstrap$Modal$renderBody(conf.dH),
													rundis$elm_bootstrap$Bootstrap$Modal$renderFooter(conf.cl)
												])))
									]))
							]))
					]),
				A2(rundis$elm_bootstrap$Bootstrap$Modal$backdrop, visibility, conf)));
	});
var rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt2 = elm$html$Html$Attributes$class('mt-2');
var author$project$Main$viewOfMainTabItem = function (model) {
	return rundis$elm_bootstrap$Bootstrap$Tab$item(
		{
			c0: 'mainTabItem',
			c9: A2(
				rundis$elm_bootstrap$Bootstrap$Tab$link,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text('Executor')
					])),
			df: A2(
				rundis$elm_bootstrap$Bootstrap$Tab$pane,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt3]),
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[rundis$elm_bootstrap$Bootstrap$Grid$Col$lg6]),
								_List_fromArray(
									[
										rundis$elm_bootstrap$Bootstrap$Card$view(
										A3(
											rundis$elm_bootstrap$Bootstrap$Card$block,
											_List_Nil,
											_List_fromArray(
												[
													rundis$elm_bootstrap$Bootstrap$Card$Block$custom(
													rundis$elm_bootstrap$Bootstrap$Form$Textarea$textarea(
														_List_fromArray(
															[
																rundis$elm_bootstrap$Bootstrap$Form$Textarea$rows(15),
																rundis$elm_bootstrap$Bootstrap$Form$Textarea$onInput(author$project$Main$ChangeProgramContent),
																rundis$elm_bootstrap$Bootstrap$Form$Textarea$value(model.V)
															])))
												]),
											A3(
												rundis$elm_bootstrap$Bootstrap$Card$header,
												_List_Nil,
												_List_fromArray(
													[
														elm$html$Html$text('Program Input : Parse as '),
														A2(
														rundis$elm_bootstrap$Bootstrap$Dropdown$dropdown,
														model.v.N,
														{
															cq: A2(
																elm$core$List$map,
																author$project$Main$viewOfBFTokenTableItem(
																	A2(elm$core$Basics$composeL, author$project$Main$UpdateParserTokenTableState, author$project$Main$UpdateTokenTable)),
																author$project$Main$bfTokenTableList(model)),
															bC: _List_Nil,
															cF: A2(
																rundis$elm_bootstrap$Bootstrap$Dropdown$toggle,
																_List_fromArray(
																	[rundis$elm_bootstrap$Bootstrap$Button$primary, rundis$elm_bootstrap$Bootstrap$Button$small]),
																_List_fromArray(
																	[
																		elm$html$Html$text(model.v.p.b)
																	])),
															cG: A2(elm$core$Basics$composeL, author$project$Main$UpdateParserTokenTableState, author$project$Main$UpdateTokenTableDropdownState)
														})
													]),
												rundis$elm_bootstrap$Bootstrap$Card$config(_List_Nil))))
									])),
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[rundis$elm_bootstrap$Bootstrap$Grid$Col$lg6]),
								_List_fromArray(
									[
										A2(
										rundis$elm_bootstrap$Bootstrap$Grid$row,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												rundis$elm_bootstrap$Bootstrap$Grid$col,
												_List_Nil,
												_List_fromArray(
													[
														rundis$elm_bootstrap$Bootstrap$Card$view(
														A3(
															rundis$elm_bootstrap$Bootstrap$Card$block,
															_List_Nil,
															_List_fromArray(
																[
																	rundis$elm_bootstrap$Bootstrap$Card$Block$custom(
																	rundis$elm_bootstrap$Bootstrap$Form$Textarea$textarea(
																		_List_fromArray(
																			[
																				rundis$elm_bootstrap$Bootstrap$Form$Textarea$rows(5),
																				rundis$elm_bootstrap$Bootstrap$Form$Textarea$onInput(
																				A2(elm$core$Basics$composeL, author$project$Main$UpdateExecutorParams, author$project$Main$UpdateInput)),
																				rundis$elm_bootstrap$Bootstrap$Form$Textarea$value(model.e.dU)
																			])))
																]),
															A3(
																rundis$elm_bootstrap$Bootstrap$Card$header,
																_List_Nil,
																_List_fromArray(
																	[
																		elm$html$Html$text('Input')
																	]),
																rundis$elm_bootstrap$Bootstrap$Card$config(_List_Nil))))
													]))
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Grid$row,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												rundis$elm_bootstrap$Bootstrap$Grid$col,
												_List_Nil,
												_List_fromArray(
													[
														rundis$elm_bootstrap$Bootstrap$Card$view(
														A3(
															rundis$elm_bootstrap$Bootstrap$Card$block,
															_List_Nil,
															_List_fromArray(
																[
																	rundis$elm_bootstrap$Bootstrap$Card$Block$custom(
																	A2(
																		elm$html$Html$p,
																		_List_Nil,
																		function (x) {
																			return A2(
																				elm$core$List$append,
																				x,
																				_List_fromArray(
																					[
																						A2(
																						elm$html$Html$span,
																						_List_fromArray(
																							[
																								elm$html$Html$Attributes$class('text-danger')
																							]),
																						_List_fromArray(
																							[
																								elm$html$Html$text(
																								A2(elm$core$Maybe$withDefault, '', model.e.bd))
																							]))
																					]));
																		}(
																			A2(
																				elm$core$List$intersperse,
																				A2(elm$html$Html$br, _List_Nil, _List_Nil),
																				A2(
																					elm$core$List$map,
																					elm$html$Html$text,
																					A2(
																						elm$core$String$split,
																						'\n',
																						A2(
																							elm$core$Maybe$withDefault,
																							'',
																							elm$url$Url$percentDecode(
																								elm$core$String$concat(
																									A2(
																										elm$core$List$map,
																										A2(
																											elm$core$Basics$composeR,
																											elm$core$Char$toCode,
																											A2(
																												elm$core$Basics$composeR,
																												author$project$Main$convertCharIntoHexString,
																												elm$core$Basics$append('%'))),
																										elm$core$List$reverse(model.e.de)))))))))))
																]),
															A3(
																rundis$elm_bootstrap$Bootstrap$Card$header,
																_List_Nil,
																_List_fromArray(
																	[
																		elm$html$Html$text('Output')
																	]),
																rundis$elm_bootstrap$Bootstrap$Card$config(
																	_List_fromArray(
																		[
																			rundis$elm_bootstrap$Bootstrap$Card$attrs(
																			_List_fromArray(
																				[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt2]))
																		])))))
													]))
											]))
									]))
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs(
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('my-3')
									]))
							]),
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$UpdateExecutorParams(
													author$project$Main$ExecuteWithNewRunningState(1)))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('Run')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$UpdateExecutorParams(
													author$project$Main$ExecuteWithNewRunningState(3)))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('StepRun')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$UpdateExecutorParams(
													author$project$Main$ExecuteWithNewRunningState(4)))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('SkipLoopOnce')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$UpdateExecutorParams(
													author$project$Main$ExecuteWithNewRunningState(5)))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('SkipEntireLoop')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$UpdateExecutorParams(author$project$Main$StopExecution))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('ResetStepRunPosition')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(
												author$project$Main$UpdateResetConfirmationModalState(rundis$elm_bootstrap$Bootstrap$Modal$shown))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('ResetAll')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$onClick(author$project$Main$CopyConvertedProgram)
											]),
										_List_fromArray(
											[
												elm$html$Html$text('CopyParsedProgramIntoProgramInput')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Button$button,
										_List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Button$attrs(
												_List_fromArray(
													[
														elm$html$Html$Events$onClick(
														author$project$Main$UpdateAddCustomTokenTableModalState(rundis$elm_bootstrap$Bootstrap$Modal$shown))
													]))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('Add new Language')
											])),
										A2(
										rundis$elm_bootstrap$Bootstrap$Modal$view,
										model.bW,
										A3(
											rundis$elm_bootstrap$Bootstrap$Modal$footer,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													rundis$elm_bootstrap$Bootstrap$Button$button,
													_List_fromArray(
														[
															rundis$elm_bootstrap$Bootstrap$Button$onClick(
															author$project$Main$UpdateResetConfirmationModalState(rundis$elm_bootstrap$Bootstrap$Modal$hidden))
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Cancel')
														])),
													A2(
													rundis$elm_bootstrap$Bootstrap$Button$button,
													_List_fromArray(
														[
															rundis$elm_bootstrap$Bootstrap$Button$onClick(author$project$Main$ResetAll),
															rundis$elm_bootstrap$Bootstrap$Button$danger
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Reset EVERYTHING')
														]))
												]),
											A3(
												rundis$elm_bootstrap$Bootstrap$Modal$body,
												_List_Nil,
												_List_fromArray(
													[
														elm$html$Html$text('Resetting everything can\'t be undone.')
													]),
												A3(
													rundis$elm_bootstrap$Bootstrap$Modal$h3,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Are you sure?')
														]),
													A2(
														rundis$elm_bootstrap$Bootstrap$Modal$hideOnBackdropClick,
														true,
														rundis$elm_bootstrap$Bootstrap$Modal$config(
															author$project$Main$UpdateResetConfirmationModalState(rundis$elm_bootstrap$Bootstrap$Modal$hidden))))))),
										A2(
										rundis$elm_bootstrap$Bootstrap$Modal$view,
										model.aU,
										A3(
											rundis$elm_bootstrap$Bootstrap$Modal$footer,
											_List_Nil,
											_List_fromArray(
												[
													rundis$elm_bootstrap$Bootstrap$Form$InputGroup$view(
													A2(
														rundis$elm_bootstrap$Bootstrap$Form$InputGroup$successors,
														_List_fromArray(
															[
																A2(
																rundis$elm_bootstrap$Bootstrap$Form$InputGroup$button,
																_List_fromArray(
																	[
																		rundis$elm_bootstrap$Bootstrap$Button$outlinePrimary,
																		rundis$elm_bootstrap$Bootstrap$Button$attrs(
																		_List_fromArray(
																			[
																				elm$html$Html$Events$onClick(author$project$Main$AddCustomTokenTable)
																			]))
																	]),
																_List_fromArray(
																	[
																		elm$html$Html$text('Add')
																	]))
															]),
														A2(
															rundis$elm_bootstrap$Bootstrap$Form$InputGroup$predecessors,
															_List_fromArray(
																[
																	A2(
																	rundis$elm_bootstrap$Bootstrap$Form$InputGroup$span,
																	_List_Nil,
																	_List_fromArray(
																		[
																			elm$html$Html$text('Language Name')
																		]))
																]),
															rundis$elm_bootstrap$Bootstrap$Form$InputGroup$config(
																rundis$elm_bootstrap$Bootstrap$Form$InputGroup$text(
																	_List_fromArray(
																		[
																			rundis$elm_bootstrap$Bootstrap$Form$Input$attrs(
																			_List_fromArray(
																				[
																					elm$html$Html$Events$onInput(author$project$Main$UpdateUpComingCustomTokenTableName),
																					elm$html$Html$Attributes$value(model.aK)
																				]))
																		]))))))
												]),
											A3(
												rundis$elm_bootstrap$Bootstrap$Modal$body,
												_List_Nil,
												_Utils_ap(
													elm$core$Array$toList(
														A2(
															elm$core$Array$indexedMap,
															F2(
																function (idx, _n0) {
																	var kind = _n0.a;
																	var value = _n0.b;
																	return A2(
																		rundis$elm_bootstrap$Bootstrap$Grid$row,
																		_List_Nil,
																		_List_fromArray(
																			[
																				A2(
																				rundis$elm_bootstrap$Bootstrap$Grid$col,
																				_List_Nil,
																				_List_fromArray(
																					[
																						rundis$elm_bootstrap$Bootstrap$Form$InputGroup$view(
																						A2(
																							rundis$elm_bootstrap$Bootstrap$Form$InputGroup$successors,
																							_List_fromArray(
																								[
																									A2(
																									rundis$elm_bootstrap$Bootstrap$Form$InputGroup$button,
																									_List_fromArray(
																										[
																											rundis$elm_bootstrap$Bootstrap$Button$danger,
																											rundis$elm_bootstrap$Bootstrap$Button$attrs(
																											_List_fromArray(
																												[
																													elm$html$Html$Events$onClick(
																													author$project$Main$RemoveUpComingCustomTokenTable(idx))
																												]))
																										]),
																									_List_fromArray(
																										[
																											elm$html$Html$text('Delete')
																										]))
																								]),
																							A2(
																								rundis$elm_bootstrap$Bootstrap$Form$InputGroup$predecessors,
																								_List_fromArray(
																									[
																										A2(
																										rundis$elm_bootstrap$Bootstrap$Form$InputGroup$span,
																										_List_Nil,
																										_List_fromArray(
																											[
																												elm$html$Html$text(
																												author$project$BFTypes$tokenKindToString(kind))
																											]))
																									]),
																								rundis$elm_bootstrap$Bootstrap$Form$InputGroup$config(
																									rundis$elm_bootstrap$Bootstrap$Form$InputGroup$text(
																										_List_fromArray(
																											[
																												rundis$elm_bootstrap$Bootstrap$Form$Input$attrs(
																												_List_fromArray(
																													[
																														elm$html$Html$Events$onInput(
																														author$project$Main$UpdateUpComingCustomTokenTable(idx)),
																														elm$html$Html$Attributes$value(value)
																													]))
																											]))))))
																					]))
																			]));
																}),
															model.s)),
													_List_fromArray(
														[
															A2(
															rundis$elm_bootstrap$Bootstrap$Grid$row,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	rundis$elm_bootstrap$Bootstrap$Grid$col,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A2(
																			rundis$elm_bootstrap$Bootstrap$Dropdown$dropdown,
																			model.aJ,
																			{
																				cq: A2(
																					elm$core$List$map,
																					function (k) {
																						return A2(
																							rundis$elm_bootstrap$Bootstrap$Dropdown$buttonItem,
																							_List_fromArray(
																								[
																									elm$html$Html$Events$onClick(
																									author$project$Main$PushUpComingCustomTokenTable(k))
																								]),
																							_List_fromArray(
																								[
																									elm$html$Html$text(
																									author$project$BFTypes$tokenKindToString(k))
																								]));
																					},
																					_List_fromArray(
																						[1, 2, 3, 4, 5, 6, 7, 8])),
																				bC: _List_Nil,
																				cF: A2(
																					rundis$elm_bootstrap$Bootstrap$Dropdown$toggle,
																					_List_fromArray(
																						[rundis$elm_bootstrap$Bootstrap$Button$outlineSecondary]),
																					_List_fromArray(
																						[
																							elm$html$Html$text('Add new')
																						])),
																				cG: author$project$Main$UpdateUpComingCustomTokenTableDropdown
																			})
																		]))
																]))
														])),
												A3(
													rundis$elm_bootstrap$Bootstrap$Modal$h3,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('New Language')
														]),
													A2(
														rundis$elm_bootstrap$Bootstrap$Modal$hideOnBackdropClick,
														true,
														rundis$elm_bootstrap$Bootstrap$Modal$config(
															author$project$Main$UpdateAddCustomTokenTableModalState(rundis$elm_bootstrap$Bootstrap$Modal$hidden)))))))
									]))
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[rundis$elm_bootstrap$Bootstrap$Grid$Col$lg6]),
								_List_fromArray(
									[
										rundis$elm_bootstrap$Bootstrap$Card$view(
										A3(
											rundis$elm_bootstrap$Bootstrap$Card$block,
											_List_Nil,
											_List_fromArray(
												[
													rundis$elm_bootstrap$Bootstrap$Card$Block$custom(
													A2(
														elm$html$Html$p,
														_List_Nil,
														A3(author$project$Main$viewOfBFCommands, model, _List_Nil, model.e.a7)))
												]),
											A3(
												rundis$elm_bootstrap$Bootstrap$Card$header,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														rundis$elm_bootstrap$Bootstrap$Grid$row,
														_List_Nil,
														_List_fromArray(
															[
																A2(
																rundis$elm_bootstrap$Bootstrap$Grid$col,
																_List_Nil,
																_List_fromArray(
																	[
																		elm$html$Html$text('Parsed Program : Display as '),
																		A2(
																		rundis$elm_bootstrap$Bootstrap$Dropdown$dropdown,
																		model.q.N,
																		{
																			cq: A2(
																				elm$core$List$map,
																				author$project$Main$viewOfBFTokenTableItem(
																					A2(elm$core$Basics$composeL, author$project$Main$UpdateDisplayTokenTableState, author$project$Main$UpdateTokenTable)),
																				author$project$Main$bfTokenTableList(model)),
																			bC: _List_Nil,
																			cF: A2(
																				rundis$elm_bootstrap$Bootstrap$Dropdown$toggle,
																				_List_fromArray(
																					[rundis$elm_bootstrap$Bootstrap$Button$primary, rundis$elm_bootstrap$Bootstrap$Button$small]),
																				_List_fromArray(
																					[
																						elm$html$Html$text(model.q.p.b)
																					])),
																			cG: A2(elm$core$Basics$composeL, author$project$Main$UpdateDisplayTokenTableState, author$project$Main$UpdateTokenTableDropdownState)
																		})
																	])),
																A2(
																rundis$elm_bootstrap$Bootstrap$Grid$col,
																_List_Nil,
																_List_fromArray(
																	[
																		elm$html$Html$text(' Comments: '),
																		A2(
																		rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroup,
																		_List_fromArray(
																			[rundis$elm_bootstrap$Bootstrap$ButtonGroup$small]),
																		_List_fromArray(
																			[
																				A3(
																				rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
																				model.M,
																				_List_fromArray(
																					[
																						rundis$elm_bootstrap$Bootstrap$Button$primary,
																						rundis$elm_bootstrap$Bootstrap$Button$onClick(
																						author$project$Main$ChangeNoOpCommandVisibility(true))
																					]),
																				_List_fromArray(
																					[
																						elm$html$Html$text('Show')
																					])),
																				A3(
																				rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
																				!model.M,
																				_List_fromArray(
																					[
																						rundis$elm_bootstrap$Bootstrap$Button$primary,
																						rundis$elm_bootstrap$Bootstrap$Button$onClick(
																						author$project$Main$ChangeNoOpCommandVisibility(false))
																					]),
																				_List_fromArray(
																					[
																						elm$html$Html$text('Hide')
																					]))
																			]))
																	]))
															]))
													]),
												rundis$elm_bootstrap$Bootstrap$Card$config(
													_List_fromArray(
														[
															rundis$elm_bootstrap$Bootstrap$Card$attrs(
															_List_fromArray(
																[
																	elm$html$Html$Attributes$class('h-100')
																]))
														])))))
									])),
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[rundis$elm_bootstrap$Bootstrap$Grid$Col$lg6]),
								_List_fromArray(
									[
										rundis$elm_bootstrap$Bootstrap$Card$view(
										A3(
											rundis$elm_bootstrap$Bootstrap$Card$block,
											_List_Nil,
											_List_fromArray(
												[
													rundis$elm_bootstrap$Bootstrap$Card$Block$custom(
													A2(
														elm$html$Html$div,
														_List_Nil,
														A2(
															elm$core$List$map,
															function (idx) {
																var line = (model.e.cQ * 16) + idx;
																return A2(
																	rundis$elm_bootstrap$Bootstrap$Grid$row,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A2(
																			rundis$elm_bootstrap$Bootstrap$Grid$col,
																			_List_Nil,
																			_List_fromArray(
																				[
																					A2(author$project$Main$tableViewOfTapeLine, model, line)
																				]))
																		]));
															},
															A2(elm$core$List$range, 0, 15))))
												]),
											A3(
												rundis$elm_bootstrap$Bootstrap$Card$header,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														rundis$elm_bootstrap$Bootstrap$Grid$row,
														_List_Nil,
														_List_fromArray(
															[
																A2(
																rundis$elm_bootstrap$Bootstrap$Grid$col,
																_List_fromArray(
																	[rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2, rundis$elm_bootstrap$Bootstrap$Grid$Col$lg3]),
																_List_fromArray(
																	[
																		elm$html$Html$text('Tape Status')
																	])),
																A2(
																rundis$elm_bootstrap$Bootstrap$Grid$col,
																_List_fromArray(
																	[rundis$elm_bootstrap$Bootstrap$Grid$Col$sm6]),
																_List_fromArray(
																	[
																		elm$html$Html$text(' Display value as: '),
																		A2(
																		rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroup,
																		_List_fromArray(
																			[rundis$elm_bootstrap$Bootstrap$ButtonGroup$small]),
																		_List_fromArray(
																			[
																				A3(
																				rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
																				!model.W,
																				_List_fromArray(
																					[
																						rundis$elm_bootstrap$Bootstrap$Button$primary,
																						rundis$elm_bootstrap$Bootstrap$Button$onClick(
																						author$project$Main$UpdateHowShowBFTapeAs(0))
																					]),
																				_List_fromArray(
																					[
																						elm$html$Html$text('Int')
																					])),
																				A3(
																				rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
																				model.W === 1,
																				_List_fromArray(
																					[
																						rundis$elm_bootstrap$Bootstrap$Button$primary,
																						rundis$elm_bootstrap$Bootstrap$Button$onClick(
																						author$project$Main$UpdateHowShowBFTapeAs(1))
																					]),
																				_List_fromArray(
																					[
																						elm$html$Html$text('Hex')
																					])),
																				A3(
																				rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
																				model.W === 2,
																				_List_fromArray(
																					[
																						rundis$elm_bootstrap$Bootstrap$Button$primary,
																						rundis$elm_bootstrap$Bootstrap$Button$onClick(
																						author$project$Main$UpdateHowShowBFTapeAs(2))
																					]),
																				_List_fromArray(
																					[
																						elm$html$Html$text('Char')
																					]))
																			]))
																	])),
																A2(
																rundis$elm_bootstrap$Bootstrap$Grid$col,
																_List_fromArray(
																	[rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4, rundis$elm_bootstrap$Bootstrap$Grid$Col$lg3]),
																_List_fromArray(
																	[
																		rundis$elm_bootstrap$Bootstrap$Form$InputGroup$view(
																		A2(
																			rundis$elm_bootstrap$Bootstrap$Form$InputGroup$successors,
																			_List_fromArray(
																				[
																					A2(
																					rundis$elm_bootstrap$Bootstrap$Form$InputGroup$span,
																					_List_Nil,
																					_List_fromArray(
																						[
																							elm$html$Html$text('/ '),
																							elm$html$Html$text(
																							elm$core$String$fromInt(author$project$BFTypes$tapePages - 1))
																						])),
																					A2(
																					rundis$elm_bootstrap$Bootstrap$Form$InputGroup$button,
																					_List_fromArray(
																						[
																							rundis$elm_bootstrap$Bootstrap$Button$secondary,
																							rundis$elm_bootstrap$Bootstrap$Button$onClick(
																							author$project$Main$UpdateExecutorParams(
																								author$project$Main$UpdateCurrentTapePage(model.e.cQ + 1)))
																						]),
																					_List_fromArray(
																						[
																							elm$html$Html$text('>')
																						]))
																				]),
																			A2(
																				rundis$elm_bootstrap$Bootstrap$Form$InputGroup$predecessors,
																				_List_fromArray(
																					[
																						A2(
																						rundis$elm_bootstrap$Bootstrap$Form$InputGroup$button,
																						_List_fromArray(
																							[
																								rundis$elm_bootstrap$Bootstrap$Button$secondary,
																								rundis$elm_bootstrap$Bootstrap$Button$onClick(
																								author$project$Main$UpdateExecutorParams(
																									author$project$Main$UpdateCurrentTapePage(model.e.cQ - 1)))
																							]),
																						_List_fromArray(
																							[
																								elm$html$Html$text('<')
																							]))
																					]),
																				rundis$elm_bootstrap$Bootstrap$Form$InputGroup$small(
																					rundis$elm_bootstrap$Bootstrap$Form$InputGroup$config(
																						rundis$elm_bootstrap$Bootstrap$Form$InputGroup$text(
																							_List_fromArray(
																								[
																									rundis$elm_bootstrap$Bootstrap$Form$Input$value(
																									elm$core$String$fromInt(model.e.cQ)),
																									rundis$elm_bootstrap$Bootstrap$Form$Input$onInput(
																									A2(
																										elm$core$Basics$composeR,
																										elm$core$String$toInt,
																										A2(
																											elm$core$Basics$composeR,
																											elm$core$Maybe$withDefault(0),
																											A2(elm$core$Basics$composeR, author$project$Main$UpdateCurrentTapePage, author$project$Main$UpdateExecutorParams))))
																								])))))))
																	]))
															]))
													]),
												rundis$elm_bootstrap$Bootstrap$Card$config(_List_Nil))))
									]))
							]))
					]))
		});
};
var elm$html$Html$h1 = _VirtualDom_node('h1');
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$node = elm$virtual_dom$VirtualDom$node;
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var rundis$elm_bootstrap$Bootstrap$CDN$stylesheet = A3(
	elm$html$Html$node,
	'link',
	_List_fromArray(
		[
			elm$html$Html$Attributes$rel('stylesheet'),
			elm$html$Html$Attributes$href('https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css')
		]),
	_List_Nil);
var rundis$elm_bootstrap$Bootstrap$Grid$containerFluid = F2(
	function (attributes, children) {
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('container-fluid')
					]),
				attributes),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Col$sm = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, 1, 0);
var rundis$elm_bootstrap$Bootstrap$Tab$Config = elm$core$Basics$identity;
var rundis$elm_bootstrap$Bootstrap$Tab$config = function (toMsg) {
	return {a0: _List_Nil, ah: false, cq: _List_Nil, ai: elm$core$Maybe$Nothing, cE: toMsg, b5: false, Z: false};
};
var rundis$elm_bootstrap$Bootstrap$Tab$items = F2(
	function (items_, _n0) {
		var configRec = _n0;
		return _Utils_update(
			configRec,
			{cq: items_});
	});
var elm$html$Html$ul = _VirtualDom_node('ul');
var rundis$elm_bootstrap$Bootstrap$Tab$getActiveItem = F2(
	function (_n0, configRec) {
		var activeTab = _n0.aT;
		if (activeTab.$ === 1) {
			return elm$core$List$head(configRec.cq);
		} else {
			var id = activeTab.a;
			return function (found) {
				if (!found.$) {
					var f = found.a;
					return elm$core$Maybe$Just(f);
				} else {
					return elm$core$List$head(configRec.cq);
				}
			}(
				elm$core$List$head(
					A2(
						elm$core$List$filter,
						function (_n2) {
							var item_ = _n2;
							return _Utils_eq(item_.c0, id);
						},
						configRec.cq)));
		}
	});
var elm$html$Html$a = _VirtualDom_node('a');
var elm$html$Html$li = _VirtualDom_node('li');
var rundis$elm_bootstrap$Bootstrap$Tab$Hidden = 0;
var rundis$elm_bootstrap$Bootstrap$Tab$Start = 1;
var rundis$elm_bootstrap$Bootstrap$Tab$visibilityTransition = F2(
	function (withAnimation_, visibility) {
		var _n0 = _Utils_Tuple2(withAnimation_, visibility);
		_n0$2:
		while (true) {
			if (_n0.a) {
				switch (_n0.b) {
					case 0:
						var _n1 = _n0.b;
						return 1;
					case 1:
						var _n2 = _n0.b;
						return 2;
					default:
						break _n0$2;
				}
			} else {
				break _n0$2;
			}
		}
		return 2;
	});
var rundis$elm_bootstrap$Bootstrap$Tab$renderLink = F4(
	function (id, active, _n0, configRec) {
		var attributes = _n0.a0;
		var children = _n0.cN;
		var commonClasses = _List_fromArray(
			[
				_Utils_Tuple2('nav-link', true),
				_Utils_Tuple2('active', active)
			]);
		var clickHandler = elm$html$Html$Events$onClick(
			configRec.cE(
				{
					aT: elm$core$Maybe$Just(id),
					j: A2(rundis$elm_bootstrap$Bootstrap$Tab$visibilityTransition, configRec.Z && (!active), 0)
				}));
		var linkItem = configRec.b5 ? A2(
			elm$html$Html$a,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$classList(commonClasses),
						clickHandler,
						elm$html$Html$Attributes$href('#' + id)
					]),
				attributes),
			children) : A2(
			elm$html$Html$button,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$classList(
						_Utils_ap(
							commonClasses,
							_List_fromArray(
								[
									_Utils_Tuple2('btn', true),
									_Utils_Tuple2('btn-link', true)
								]))),
						clickHandler
					]),
				attributes),
			children);
		return A2(
			elm$html$Html$li,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('nav-item')
				]),
			_List_fromArray(
				[linkItem]));
	});
var rundis$elm_bootstrap$Bootstrap$Tab$transitionStyles = function (opacity) {
	return _List_fromArray(
		[
			A2(
			elm$html$Html$Attributes$style,
			'opacity',
			elm$core$String$fromInt(opacity)),
			A2(elm$html$Html$Attributes$style, '-webkit-transition', 'opacity 0.15s linear'),
			A2(elm$html$Html$Attributes$style, '-o-transition', 'opacity 0.15s linear'),
			A2(elm$html$Html$Attributes$style, 'transition', 'opacity 0.15s linear')
		]);
};
var rundis$elm_bootstrap$Bootstrap$Tab$activeTabAttributes = F2(
	function (_n0, configRec) {
		var visibility = _n0.j;
		switch (visibility) {
			case 0:
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'none')
					]);
			case 1:
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'block'),
						A2(elm$html$Html$Attributes$style, 'opacity', '0')
					]);
			default:
				return _Utils_ap(
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'display', 'block')
						]),
					rundis$elm_bootstrap$Bootstrap$Tab$transitionStyles(1));
		}
	});
var rundis$elm_bootstrap$Bootstrap$Tab$renderTabPane = F5(
	function (id, active, _n0, state, configRec) {
		var attributes = _n0.a0;
		var children = _n0.cN;
		var displayAttrs = active ? A2(rundis$elm_bootstrap$Bootstrap$Tab$activeTabAttributes, state, configRec) : _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'display', 'none')
			]);
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$id(id),
						elm$html$Html$Attributes$class('tab-pane')
					]),
				_Utils_ap(displayAttrs, attributes)),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes = function (configRec) {
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('nav', true),
						_Utils_Tuple2('nav-tabs', !configRec.ah),
						_Utils_Tuple2('nav-pills', configRec.ah)
					]))
			]),
		_Utils_ap(
			function () {
				var _n0 = configRec.ai;
				if (!_n0.$) {
					switch (_n0.a) {
						case 3:
							var _n1 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('nav-justified')
								]);
						case 2:
							var _n2 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('nav-fill')
								]);
						case 0:
							var _n3 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('justify-content-center')
								]);
						default:
							var _n4 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('justify-content-end')
								]);
					}
				} else {
					return _List_Nil;
				}
			}(),
			configRec.a0));
};
var rundis$elm_bootstrap$Bootstrap$Tab$view = F2(
	function (state, _n0) {
		var configRec = _n0;
		var _n1 = A2(rundis$elm_bootstrap$Bootstrap$Tab$getActiveItem, state, configRec);
		if (_n1.$ === 1) {
			return A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$ul,
						rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes(configRec),
						_List_Nil),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('tab-content')
							]),
						_List_Nil)
					]));
		} else {
			var currentItem = _n1.a;
			return A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$ul,
						rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes(configRec),
						A2(
							elm$core$List$map,
							function (_n2) {
								var item_ = _n2;
								return A4(
									rundis$elm_bootstrap$Bootstrap$Tab$renderLink,
									item_.c0,
									_Utils_eq(item_.c0, currentItem.c0),
									item_.c9,
									configRec);
							},
							configRec.cq)),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('tab-content')
							]),
						A2(
							elm$core$List$map,
							function (_n3) {
								var item_ = _n3;
								return A5(
									rundis$elm_bootstrap$Bootstrap$Tab$renderTabPane,
									item_.c0,
									_Utils_eq(item_.c0, currentItem.c0),
									item_.df,
									state,
									configRec);
							},
							configRec.cq))
					]));
		}
	});
var author$project$Main$view = function (model) {
	return A2(
		rundis$elm_bootstrap$Bootstrap$Grid$containerFluid,
		_List_Nil,
		_List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$CDN$stylesheet,
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Grid$Col$sm]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('BF/Ook! like language interpreter/transpiler')
									]))
							]))
					])),
				A2(
				rundis$elm_bootstrap$Bootstrap$Tab$view,
				model.b1,
				A2(
					rundis$elm_bootstrap$Bootstrap$Tab$items,
					_List_fromArray(
						[
							author$project$Main$viewOfMainTabItem(model),
							author$project$Main$viewOfDebugTabItem(model)
						]),
					rundis$elm_bootstrap$Bootstrap$Tab$config(author$project$Main$UpdateTabState)))
			]));
};
var elm$browser$Browser$element = _Browser_element;
var elm$json$Json$Decode$value = _Json_decodeValue;
var author$project$Main$main = elm$browser$Browser$element(
	{dT: author$project$Main$init, d$: author$project$Main$subscriptions, d3: author$project$Main$update, d6: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(elm$json$Json$Decode$value)(0)}});}(this));