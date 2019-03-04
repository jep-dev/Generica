#ifndef MATH_TPP
#define MATH_TPP

#include "math.hpp"
#include "dual.hpp"

template<class S, unsigned N, class DELIM, class... T>
std::string to_string(const S (&sn)[N], unsigned prec, DELIM delim, T &&... t) {
	using namespace std;
	bool met = false;
	std::string out;
	for(auto const& s : sn) {
		if(met) out += delim;
		out += to_string(s, prec, delim, std::forward<T>(t)...);
		met = true;
	}
	return out;
}
template<class S, class T, class ST>
DualQuaternion<ST> operator*(Point<S> const& p, DualQuaternion<T> const& d) {
	return {d.s, d.t, d.u, d.v,
			d.w - p.x*d.t - p.y*d.u - p.z*d.v, d.x + p.x*d.s + p.y*d.v - p.z*d.u,
			d.y - p.x*d.v + p.y*d.s + p.z*d.t, d.z + p.x*d.u - p.y*d.t + p.z*d.s};
}
template<class S, class T, class ST>
DualQuaternion<ST> operator*(DualQuaternion<S> const& d, Point<S> p) {
	return {d.s, d.t, d.u, d.v,
			d.w - p.x*d.t - p.y*d.u - p.z*d.v, d.x + p.x*d.s - p.y*d.v + p.z*d.u,
			d.y + p.x*d.v + p.y*d.s - p.z*d.t, d.z - p.x*d.u + p.y*d.t + p.z*d.s};
}

#endif
