#ifndef DUAL_TPP
#define DUAL_TPP

#include "dual.hpp"
#include "quaternion.hpp"
#include "quaternion.tpp"
#include "streams.hpp"

template<class L, class S, class DELIM = const char*, class... T>
L& print(L &lhs, DualQuaternion<S> const& rhs, T &&... t) {
	return lhs << to_string(rhs, std::forward<T>(t)...), lhs;
}

template<class L, class S>
L& operator<<(L &lhs, DualQuaternion<S> const& rhs) {
	return lhs << to_string(rhs, 1, true), lhs;
}

template<class S, class... T>
std::string to_string(DualQuaternion<S> const& s, T &&... t) {
	bool nz = false;
	std::string out;
	for(auto i : {'e', 'i', 'j', 'k', 'E', 'I', 'J', 'K'}) {
		auto const& v = s[i];
		auto stringified = to_unit_string(v, i, std::forward<T>(t)...);
		if(!stringified.length()) continue;
		if(v > 0 && nz) out += '+';
		out += stringified;
		nz = true;
	}
	if(!nz) out += '0';
	return out;
}
template<class S>
DualQuaternion<S>::operator std::string(void) const
	{ return to_string(*this, 3); }

#endif
