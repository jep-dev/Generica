#ifndef EXPRESSION_HPP
#define EXPRESSION_HPP

#include <memory>
#include <iosfwd>

#include "math.hpp"
#include "quaternion.tpp"
#include "dual.tpp"

#include "tag.hpp"

namespace Expressions {

typedef enum {
	e_undefined = 0,
	e_value, e_variable,
	e_negative, e_conjugate, e_exp, e_log, e_inverse,
	e_sum, e_difference, e_product, //e_quotient,
	// e_power,
	// e_cosine, e_sine, e_tangent,
	// e_cosh, e_sinh, e_tanh
} EType;

template<class T> struct Symbol;
template<class S> struct NullarySymbol;
template<class S> struct UnarySymbol;
template<class S> struct BinarySymbol;
template<class T> using SymbolPtr = std::shared_ptr<Symbol<T>>;
template<class S> using Shareable = std::enable_shared_from_this<S>;

template<class S>
struct Symbol: std::enable_shared_from_this<Symbol<S>> {
	typedef S value_type;
	typedef SymbolPtr<S> ptr_type;
	virtual unsigned nary(void) const = 0;
	virtual EType type(void) const = 0;
	virtual bool varies(char c) const = 0;
	virtual SymbolPtr<S> derive(char c) const = 0;
	friend std::ostream& operator<<(std::ostream &lhs, Symbol const& rhs) {
		return lhs << to_string(rhs, 2, false), lhs;
	}
	virtual ~Symbol(void) {}
};
template<class S, unsigned N> struct NarySymbol: Symbol<S> {
	constexpr unsigned nary(void) const { return N; }
};
template<class S> struct NullarySymbol: NarySymbol<S, 0> {
	S value;
	//S value = {1};

	/*template<class T0, class... T, class SV = Detail::InnerValue_t<S>,
		class TN = std::enable_if_t<std::is_arithmetic<T0>::value
			&& std::is_convertible<T0, SV>::value, T0>>
	NullarySymbol(T0 t0, T ... t): value{SV(t0), SV(t)...} {}
	template<class T0, //class... T,
		class TN = std::enable_if_t<!std::is_arithmetic<T0>::value
			&& std::is_convertible<T0, S>::value, T0>>
	NullarySymbol(T0 const& t): value((S) t) {}
	NullarySymbol(S && value = {1}): value(std::move(value)) {}
	NullarySymbol(S const& value): value(value) {}*/
	template<class T>
	NullarySymbol(T && t): value{S(std::forward<T>(t))} {}
	template<class T0, class T1, class... T>
	NullarySymbol(T0 && t0, T1 && t1, T &&... t):
		value(std::forward<T0>(t0), std::forward<T1>(t1), std::forward<T>(t)...) {}
	virtual ~NullarySymbol(void) {}
};
template<class S> struct UnarySymbol: NarySymbol<S, 1> {
	SymbolPtr<S> first;
	virtual bool varies(char c) const { return first -> varies(c); }
	virtual const char *prefix(void) const = 0;
	UnarySymbol(SymbolPtr<S> const& first): first(first) {}
	virtual ~UnarySymbol(void) {}
};

template<class S> struct BinarySymbol: NarySymbol<S, 2> {
	SymbolPtr<S> first, second;
	virtual const char *infix(void) const = 0;
	bool varies(char c) const { return (first -> varies(c)) || (second -> varies(c)); }
	BinarySymbol(SymbolPtr<S> const& first, SymbolPtr<S> const& second):
			first(first), second(second) {}
	virtual ~BinarySymbol(void) {}
};

template<class S> SymbolPtr<S> reduce(SymbolPtr<S> const& p);

template<class> struct Value;
template<class S> SymbolPtr<S> reduce(Value<S> const& v);
template<class S> using ValuePtr = std::shared_ptr<Value<S>>;
template<class S, class... T> SymbolPtr<S> mkVal(T &&... t)
	{ return (SymbolPtr<S>) std::make_shared<Value<S>>(std::forward<T>(t)...); }
template<class S, class T, class E = Detail::ConvertibleFrom_t<S,T>>
SymbolPtr<S> mkVal(E const& e)
	{ return (SymbolPtr<S>) std::make_shared<Value<S>>((S) e); }

template<class S> SymbolPtr<S> mkZero(void)
	{ return (SymbolPtr<S>) std::make_shared<Value<S>>(S{0}); }
template<class S> SymbolPtr<S> mkOne(void)
	{ return (SymbolPtr<S>) std::make_shared<Value<S>>(S{1}); }

template<class> struct Variable;
template<class S> SymbolPtr<S> reduce(Variable<S> const& v);
template<class S> using VariablePtr = std::shared_ptr<Variable<S>>;
template<class S, class... T> SymbolPtr<S> mkVar(T &&... t)
	{ return (SymbolPtr<S>) std::make_shared<Variable<S>>(std::forward<T>(t)...); }
template<class S, class T, class E = Detail::ConvertibleFrom_t<S,T>>
SymbolPtr<S> mkVar(char name, E const& e)
	{ return (SymbolPtr<S>) std::make_shared<Variable<S>>(name, (S) e); }

template<class> struct UnarySymbol;

template<class> struct Negative;
template<class S> SymbolPtr<S> reduce(Negative<S> const& n);
template<class S> using NegativePtr = std::shared_ptr<Negative<S>>;
template<class S> SymbolPtr<S> mkNeg(SymbolPtr<S> const& p)
	{ return (SymbolPtr<S>) std::make_shared<Negative<S>>(p); }

template<class> struct Conjugate;
template<class S> SymbolPtr<S> reduce(Conjugate<S> const& n);
template<class S> using ConjugatePtr = std::shared_ptr<Conjugate<S>>;
template<class S> SymbolPtr<S> mkConj(SymbolPtr<S> const& p)
	{ return (SymbolPtr<S>) std::make_shared<Conjugate<S>>(p); }

template<class> struct Exp;
template<class S> SymbolPtr<S> reduce(Exp<S> const& n);
template<class S> using ExpPtr = std::shared_ptr<Exp<S>>;
template<class S> SymbolPtr<S> mkExp(SymbolPtr<S> const& p)
	{ return (SymbolPtr<S>) std::make_shared<Exp<S>>(p); }

template<class> struct Log;
template<class S> SymbolPtr<S> reduce(Log<S> const& n);
template<class S> using LogPtr = std::shared_ptr<Log<S>>;
template<class S> SymbolPtr<S> mkLog(SymbolPtr<S> const& p)
	{ return (SymbolPtr<S>) std::make_shared<Log<S>>(p); }

template<class> struct Inverse;
template<class S> SymbolPtr<S> reduce(Inverse<S> const& n);
template<class S> using InversePtr = std::shared_ptr<Inverse<S>>;
template<class S> SymbolPtr<S> mkInverse(SymbolPtr<S> const& p)
	{ return (SymbolPtr<S>) std::make_shared<Inverse<S>>(p); }


template<class> struct Sum;
template<class S> SymbolPtr<S> reduce(Sum<S> const& s);
template<class S>
SymbolPtr<S> mkSum(SymbolPtr<S> const& lhs, SymbolPtr<S> const& rhs) {
	auto p = reduce(lhs), q = reduce(rhs);
	auto pt = p -> type(), qt = q -> type();
	if(pt == e_value) {
		auto vp = getValue(p);
		if(vp == vp*0)
			return q;
	}
	if(qt == e_value) {
		auto vq = getValue(q);
		if(vq == vq*0)
			return p;
	}
	if(pt == e_value && pt == qt)
		return mkVal<S>(getValue(p) + getValue(q));
	return (SymbolPtr<S>) std::make_shared<Sum<S>>(p, q);
}
	//{ return (SymbolPtr<S>) std::make_shared<Sum<S>>(p, q); }

template<class> struct Difference;
template<class S> SymbolPtr<S> reduce(Difference<S> const& s);
template<class S>
SymbolPtr<S> mkDifference(SymbolPtr<S> const& lhs, SymbolPtr<S> const& rhs) {
	auto p = reduce(lhs), q = reduce(rhs);
	auto pt = p -> type(), qt = q -> type();
	if(pt == e_value) {
		auto vp = getValue(p);
		if(vp == vp*0)
			return q;
	}
	if(qt == e_value) {
		auto vq = getValue(q);
		if(vq == vq*0)
			return p;
	}
	if(pt == e_value && pt == qt)
		return mkVal<S>(getValue(p) + getValue(q));
	return (SymbolPtr<S>) std::make_shared<Difference<S>>(p, q);
}

template<class> struct Product;
template<class S> SymbolPtr<S> reduce(Product<S> const& p);
template<class S>
SymbolPtr<S> mkProduct(SymbolPtr<S> const& p, SymbolPtr<S> const& q)
	{ return (SymbolPtr<S>) std::make_shared<Product<S>>(p, q); }

template<class S> struct Value: NullarySymbol<S>, Shareable<Value<S>> {
	using NullarySymbol<S>::NullarySymbol;
	virtual EType type(void) const { return e_value; }
	virtual bool varies(char c) const { return false; }
	virtual SymbolPtr<S> derive(char c) const { return mkVal<S>(S{0}); }
	virtual ~Value(void) {}
};

template<class S> struct Variable: NullarySymbol<S>, Shareable<Variable<S>> {
	char name = '\0';
	virtual EType type(void) const { return e_variable; }
	virtual bool varies(char c) const { return c == name; }
	virtual SymbolPtr<S> derive(char c) const {
		if(varies(c)) return mkVal<S>(getValue(*this));
		return mkZero<S>();
	}
	template<class... T>
	Variable(char name, T &&... t): name(name), NullarySymbol<S>(std::forward<T>(t)...) {}
	virtual ~Variable(void) {}
};

template<class S> struct Negative: UnarySymbol<S>, Shareable<Negative<S>> {
	using UnarySymbol<S>::UnarySymbol;
	virtual EType type(void) const { return e_negative; }
	virtual const char *prefix(void) const { return "-"; }
	virtual SymbolPtr<S> derive(char c) const {
		return reduce(mkNeg<S>(getFirst(*this) -> derive(c)));
	}
	virtual ~Negative(void) {}
};

template<class S> struct Conjugate: UnarySymbol<S>, Shareable<Conjugate<S>> {
	using UnarySymbol<S>::UnarySymbol;
	virtual EType type(void) const { return e_conjugate; }
	virtual const char *prefix(void) const { return "*"; }
	virtual SymbolPtr<S> derive(char c) const {
		return reduce(mkConj<S>(getFirst(*this) -> derive(c)));
	}
	virtual ~Conjugate(void) {}
};

template<class S> struct Exp: UnarySymbol<S>, Shareable<Exp<S>> {
	using UnarySymbol<S>::UnarySymbol;
	virtual EType type(void) const { return e_exp; }
	virtual const char *prefix(void) const { return "exp"; }
	virtual SymbolPtr<S> derive(char c) const {
		auto v = getFirst(*this), dvdc = v -> derive(c);
		return reduce(mkProduct<S>(mkExp<S>(v), dvdc));
	}
	virtual ~Exp(void) {}
};
template<class S> struct Log: UnarySymbol<S>, Shareable<Log<S>> {
	using UnarySymbol<S>::UnarySymbol;
	virtual EType type(void) const { return e_log; }
	virtual const char *prefix(void) const { return "log"; }
	virtual SymbolPtr<S> derive(char c) const {
		auto v = reduce(getFirst(*this)), dvdc = reduce(v -> derive(c));
		return reduce(mkProduct(mkInverse<S>(v), dvdc));
	}
	Log(SymbolPtr<S> const& first): UnarySymbol<S>(first) {}
	virtual ~Log(void) {}
};
template<class S> struct Inverse: UnarySymbol<S>, Shareable<Inverse<S>> {
	using UnarySymbol<S>::UnarySymbol;
	virtual EType type(void) const { return e_inverse; }
	virtual const char *prefix(void) const { return "1/"; }
	virtual SymbolPtr<S> derive(char c) const {
		auto const& v = reduce(getFirst(*this)), dvdc = reduce(v -> derive(c));
		return mkNeg(mkProduct(mkInverse(mkProduct(v, v)), dvdc));
	}
	Inverse(SymbolPtr<S> const& first): UnarySymbol<S>(first) {}
	virtual ~Inverse(void) {}
};

template<class S> struct Sum: BinarySymbol<S>, Shareable<Sum<S>> {
	using BinarySymbol<S>::BinarySymbol;
	virtual EType type(void) const { return e_sum; }
	virtual const char *infix(void) const { return "+"; }
	virtual SymbolPtr<S> derive(char c) const {
		return reduce(mkSum(getFirst(*this) -> derive(c),
				getSecond(*this) -> derive(c)));
	}
	virtual ~Sum(void) {}
};

template<class S> struct Difference: BinarySymbol<S>, Shareable<Difference<S>> {
	using BinarySymbol<S>::BinarySymbol;
	virtual EType type(void) const { return e_difference; }
	virtual const char *infix(void) const { return "-"; }
	virtual SymbolPtr<S> derive(char c) const {
		return reduce(mkDifference(getFirst(*this) -> derive(c),
				getSecond(*this) -> derive(c)));
	}
	virtual ~Difference(void) {}
};

template<class S> struct Product: BinarySymbol<S>, Shareable<Product<S>> {
	using BinarySymbol<S>::BinarySymbol;
	virtual EType type(void) const { return e_product; }
	virtual const char *infix(void) const { return "+"; }
	virtual SymbolPtr<S> derive(char c) const {
		auto f = reduce(getFirst(*this)), s = reduce(getSecond(*this)),
			dfdc = f -> derive(c), dsdc = s -> derive(c);
		return reduce(mkSum(mkProduct(f, dsdc), mkProduct(dfdc, s)));
	}
	virtual ~Product(void) {}
};

template<class S> auto getValue(NullarySymbol<S> const& n)
	-> S const& { return n.value; }
template<class S> auto getValue(NullarySymbol<S> & n)
	-> S& { return n.value; }
template<class S>
S getValue(SymbolPtr<S> const& p) {
	if(p -> nary() > 0) return {0};
	return ((NullarySymbol<S> const&) *p).value;
}
template<class S>
char getName(SymbolPtr<S> const& p) {
	if(p -> type() == e_variable)
		return ((Variable<S> const&) *p).name;
	// TODO other named symbols, e.g. 'pi'?
	return '\0';
}
template<class S> auto getName(UnarySymbol<S> const& u)
	-> decltype(u.name) { return u.name; }

template<class S> auto getFirst(UnarySymbol<S> const& u)
	-> SymbolPtr<S> { return u.first; }
template<class S> auto getFirst(BinarySymbol<S> const& b)
	-> SymbolPtr<S> { return b.first; }
template<class S> auto getSecond(BinarySymbol<S> const& b)
	-> SymbolPtr<S> { return b.second; }

template<class S> auto getPrefix(UnarySymbol<S> const& u)
	-> const char * { return u.prefix(); }
template<class S> auto getInfix(BinarySymbol<S> const& b)
	-> const char * { return b.infix(); }

template<class S>
const char *getInfix(SymbolPtr<S> const& p) {
	if(p -> nary() != 2) return "";
	return ((BinarySymbol<S> const&) *p).infix();
}
template<class S>
const char *getPrefix(SymbolPtr<S> const& p) {
	if(p -> nary() != 1) return "";
	return ((UnarySymbol<S> const&) *p).prefix();
}
template<class S>
SymbolPtr<S> getFirst(SymbolPtr<S> const& p) {
	auto n = p -> nary();
	switch(p -> nary()) {
		// Template metaprogramming may have spoiled me, but it seems like only
		// poor design could have led here:
		case 1: return ((UnarySymbol<S> const&) *p).first;
		case 2: return ((BinarySymbol<S> const&) *p).first;
		default: return mkZero<S>();
		// To introduce (N+1)ary I need to update the existing {getFirst, ..., get(N)th}
		// and define get(N+1)th. TODO Deprecate all of this if you plan for more than Ternary.
	}
}
template<class S>
SymbolPtr<S> getSecond(SymbolPtr<S> const& p) {
	if(p -> nary() == 2) return ((BinarySymbol<S> const&) *p).second;
	return mkZero<S>();
}

template<class S, class... T>
std::string to_paren_string(S const& value, T &&... t) {
	using namespace std;
	auto strValue = to_string(value, forward<T>(t)...),
		parenValue = strValue;

	if(strValue.find('+') != string::npos
			|| strValue.find('-', 1) != string::npos) {
		parenValue = "(" + parenValue + ")";
	}
	return parenValue;
}

template<class S, class... T>
std::string to_string(SymbolPtr<S> const& p, T &&... t) {
	using namespace std;
	auto value = getValue(p);
	S fVal = getValue(p), sVal = fVal;
	if(p -> nary() > 0) {
		fVal = getValue(getFirst(p));
		if(p -> nary() > 1) sVal = getValue(getSecond(p));
		else sVal = fVal * 0;
	} else fVal = getValue(p), sVal = fVal;

	string fStr = to_paren_string(fVal, forward<T>(t)...),
			sStr = to_paren_string(fVal, forward<T>(t)...);


	switch(p -> type()) {
		//case e_value: return to_string(((Value<S> const&) *p).value, forward<T>(t)...);
		case e_value: return fStr;
		case e_variable: return fStr + getName(p);
		case e_negative: case e_conjugate:
		case e_exp: case e_log: case e_inverse:
			return getPrefix(p) + string((fStr.find("(") != string::npos)
					? "" : " ") + fStr;
		case e_sum: case e_difference:
		case e_product: // case e_quotient:
			return fStr + getInfix(p) + sStr; // TODO review parenthesis strat
		default: return "?";
	}
}


}

#endif
