#ifndef QUATERNION_TPP
#define QUATERNION_TPP

#include "streams.hpp"
#include "quaternion.hpp"

template<class S, class... T>
std::string to_string(Quaternion<S> const& qin, T &&... t) {
	bool nz = false;
	std::string out;

	for(auto i : {'e', 'i', 'j', 'k'}) {
		S const& v = qin[i];
		auto stringified = to_string((S) v, std::forward<T>(t)...);
		if(stringified == "0") continue;
		if(v > 0 && nz) out += '+';
		if(out == "1") {}
		else if(out == "-1") out += '-';
		else out += stringified;
		out += i;
		nz = true;
	}
	if(!nz) out += '0';
	return out;
}

template<class L, class S>
L& operator<<(L &lhs, Quaternion<S> const& rhs) {
	return lhs << std::string(rhs), lhs;
}
template<class S>
Quaternion<S>::operator std::string(void) const {
	return to_string(*this, 3);
}

#endif
