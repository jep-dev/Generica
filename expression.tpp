#ifndef EXPRESSION_TPP
#define EXPRESSION_TPP

#include "expression.hpp"

namespace Expressions {

template<class S>
SymbolPtr<S> reduce(Value<S> const& v) {
	return mkVar<S>(v.name, v.value);
}
template<class S>
SymbolPtr<S> reduce(Variable<S> const& v) {
	return mkVar<S>(v.name, v.value);
}
template<class S>
SymbolPtr<S> reduce(Negative<S> const& n) {
	switch(n.first -> type()) {
		case e_negative: return getFirst(n.first);
		case e_value: {
			auto v = getValue(n.first);
			if(v == v*0) return mkZero<S>();
			return mkVal<S>(-v);
		}
		case e_variable: return mkVar<S>(getName(n.first), -getValue(n.first));
		default: return mkNeg(n.first);
	}
}
template<class S>
SymbolPtr<S> reduce(Conjugate<S> const& n) {
	switch(n.first -> type()) {
		case e_conjugate: return getFirst(n.first);
		case e_value: return mkVal<S>(conjugate(getValue(n.first)));
		case e_variable: {
			auto name = getName(n.first);
			auto val = getValue(n.first);
			// TODO return mkProduct(mkConj(mkVar(name, mkOne())), mkVal(conjugate(val)))
			return mkVar<S>(name, conjugate(val));
		}
		default: return mkConj(n.first);
	}
}
template<class S>
SymbolPtr<S> reduce(Exp<S> const& n) {
	switch(n.first -> type()) {
		case e_log: return getFirst(n.first);
		case e_value: return mkVal<S>(exp(getValue(n.first)));
		default: return mkExp(n.first);
	}
}
template<class S>
SymbolPtr<S> reduce(Log<S> const& n) {
	if(n.first -> type() == e_exp)
		return getFirst(n.first);
	return mkLog<S>(n.first);
}
template<class S>
SymbolPtr<S> reduce(Inverse<S> const& i) {
	if(i.first -> type() == e_inverse)
		return getFirst(i.first);
	return mkInverse<S>(i.first);
}
template<class S>
SymbolPtr<S> reduce(Sum<S> const& s) {
	SymbolPtr<S> first = getFirst(s), second = getSecond(s);
	auto tfirst = first -> type(), tsecond = second -> type();

	bool neg = false, lconj = false, rconj = false;
	while(tfirst == e_negative || tfirst == e_conjugate) {
		if(tfirst == e_negative)
			neg = !neg, first = getFirst(first), tfirst = first -> type();
		if(tfirst == e_conjugate)
			lconj = !lconj, first = getFirst(first), tfirst = first -> type();
	}
	while(tfirst == e_negative || tfirst == e_conjugate) {
		if(tsecond == e_negative)
			neg = !neg, second = getFirst(second), tsecond = second -> type();
		if(tsecond == e_conjugate)
			rconj = !rconj, second = getFirst(second), tsecond = second -> type();
	}

	if(tfirst == e_value && tsecond == e_value) {
		S vf = getValue(first), vs = getValue(second);
		if(lconj) vf = conjugate(vf);
		if(rconj) vs = conjugate(vs);
		if(neg) vf = -vf, vs = -vs;
		return mkVal<S>(vf + vs);
	}
	if(tfirst == e_value) {
		auto vfirst = getValue(first);
		if(vfirst == vfirst*0)
			return second;
		if(tsecond == e_variable) {
			auto vsecond = getValue(second);
			if(vfirst == vsecond) {
				//return mkProduct<S>(first, mkSum<S>(mkOne<S>(),
				return mkProduct(first, mkSum(mkOne<S>(),
						mkVar<S>(getName(second))));
			}
		}
	}
	if(tsecond == e_value) {
		auto vsecond = getValue(second);
		if(vsecond == vsecond*0)
			return second;
		if(tfirst == e_variable) {
			auto vfirst = getValue(first);
			if(vsecond == vfirst) {
				//return mkProduct<S>(second, mkSum<S>(mkOne<S>(),
				return mkProduct(second, mkSum(mkOne<S>(),
						mkVar<S>(getName(first))));
			}
		}
	}
	if(tfirst == e_variable && tsecond == e_variable) {
		auto nfirst = getName(first), nsecond = getName(second);
		if(nfirst == nsecond) {
			auto lval = getValue(first), rval = getValue(second);
			if(lconj) lval = conjugate(lval);
			if(rconj) rval = conjugate(rval);
			auto val = lval + rval;
			if(val == val*0) return mkZero<S>();
			if(neg) val = -val;
			return mkVar<S>(nfirst, val);
		}
		if(nfirst > nsecond)
			return mkSum(second, first);
		return mkSum(first, second);
	}
	if(lconj && rconj) {
		auto out = mkConj(mkSum(first, second));
		return neg ? mkNeg(out) : out;
	}
	if(lconj) first = mkConj(first);
	if(rconj) second = mkConj(second);
	auto out = mkSum(first, second);
	return neg ? mkNeg(out) : out;
}

template<class S> // TODO
SymbolPtr<S> reduce(Difference<S> const& s) {
	SymbolPtr<S> first = getFirst(s), second = getSecond(s);
	auto tfirst = first -> type(), tsecond = second -> type();

	/*bool neg = false, lconj = false, rconj = false;
	while(tfirst == e_negative || tfirst == e_conjugate) {
		if(tfirst == e_negative)
			neg = !neg, first = getFirst(first), tfirst = first -> type();
		if(tfirst == e_conjugate)
			lconj = !lconj, first = getFirst(first), tfirst = first -> type();
	}
	while(tfirst == e_negative || tfirst == e_conjugate) {
		if(tsecond == e_negative)
			neg = !neg, second = getFirst(second), tsecond = second -> type();
		if(tsecond == e_conjugate)
			rconj = !rconj, second = getFirst(second), tsecond = second -> type();
	}*/
	if(tsecond == e_negative)
		return reduce(mkSum(getFirst(s), getSecond(s)));

	if(tfirst == e_value && tsecond == e_value) {
		S vf = getValue(first), vs = getValue(second);
		//if(lconj) vf = conjugate(vf);
		//if(rconj) vs = conjugate(vs);
		//if(neg) vf = -vf, vs = -vs;
		return mkVal<S>(vf - vs);
	}
	if(tfirst == e_value) {
		auto vfirst = getValue(first);
		if(vfirst == vfirst*0)
			return second;
		if(tsecond == e_variable) {
			auto vsecond = getValue(second);
			if(vfirst == vsecond) {
				//return mkProduct<S>(first, mkDifference<S>(mkOne<S>(),
				return mkProduct(first, mkDifference(mkOne<S>(),
						mkVar<S>(getName(second))));
			}
		}
	}
	if(tsecond == e_value) {
		auto vsecond = getValue(second);
		if(vsecond == vsecond*0)
			return second;
		if(tfirst == e_variable) {
			auto vfirst = getValue(first);
			if(vsecond == vfirst) {
				//return mkProduct<S>(second, mkDifference<S>(mkOne<S>(),
				return mkProduct(second, mkDifference(mkOne<S>(),
						mkVar<S>(getName(first))));
			}
		}
	}
	if(tfirst == e_variable && tsecond == e_variable) {
		auto nfirst = getName(first), nsecond = getName(second);
		if(nfirst == nsecond) {
			auto lval = getValue(first), rval = getValue(second);
			//if(lconj) lval = conjugate(lval);
			//if(rconj) rval = conjugate(rval);
			auto val = lval - rval;
			if(val == val*0) return mkZero<S>();
			//if(neg) val = -val;
			return mkVar<S>(nfirst, val);
		}
		if(nfirst > nsecond)
			return mkDifference(second, first);
		return mkDifference(first, second);
	}
	/*if(lconj && rconj) {
		auto out = mkConj(mkDifference(first, second));
		return neg ? mkNeg(out) : out;
	}
	if(lconj) first = mkConj(first);
	if(rconj) second = mkConj(second);
	auto out = mkDifference(first, second);
	return neg ? mkNeg(out) : out;*/
	return mkDifference(first, second);
}

template<class S>
SymbolPtr<S> reduce(Product<S> const& p) {
	auto first = getFirst(p), second = getSecond(p);
	auto tfirst = first -> type(), tsecond = second -> type();

	bool neg = false, lconj = false, rconj = false;
	while(tfirst == e_negative || tfirst == e_conjugate) {
		if(tfirst == e_negative)
			neg = !neg, first = getFirst(first), tfirst = first -> type();
		if(tfirst == e_conjugate)
			lconj = !lconj, first = getFirst(first), tfirst = first -> type();
	}
	while(tfirst == e_negative || tfirst == e_conjugate) {
		if(tsecond == e_negative)
			neg = !neg, second = getFirst(second), tsecond = second -> type();
		if(tsecond == e_conjugate)
			rconj = !rconj, second = getFirst(second), tsecond = second -> type();
	}
	if(tfirst == e_value && tsecond == e_value) {
		auto vf = getValue(first), vs = getValue(second);
		if(lconj) vf = conjugate(vf);
		if(rconj) vs = conjugate(vs);
		if(neg) vf = -vf;
		return mkVal<S>(vf*vs);
	}
	if((tfirst == e_variable && tsecond == e_value)
			|| (tfirst == e_value && tsecond == e_variable)) {
		char name = (tfirst == e_variable) ? getName(first) : getName(second);
		auto vf = getValue(first), vs = getValue(second);
		if((vf == vf * 0) || (vs == vs * 0)) return mkZero<S>();
		if(lconj) vf = conjugate(vf);
		if(rconj) vs = conjugate(vs);
		if(neg) vf = -vf;
		return mkVar<S>(name, vf * vs);
	}
	return mkProduct(first, second);
}

template<class S>
SymbolPtr<S> reduce(SymbolPtr<S> const& p) {
	switch(p -> type()) {
		case e_value: return p;
		case e_variable: return p;
		case e_negative: return reduce((Negative<S> const&) *p);
		case e_conjugate: return reduce((Conjugate<S> const&) *p);
		case e_exp: return reduce((Exp<S> const&) *p);
		case e_log: return reduce((Log<S> const&) *p);
		case e_inverse: return reduce((Inverse<S> const&) *p);
		case e_sum: return reduce((Sum<S> const&) *p);
		case e_difference: return reduce((Difference<S> const&) *p);
		case e_product: return reduce((Product<S> const&) *p);
		//case e_quotient: return reduce((Quotient<S> const&) *p);
		default: return p;
	}
}

}

#endif
