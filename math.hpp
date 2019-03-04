#ifndef MATH_HPP
#define MATH_HPP

#include <math.h>

#include "numeric.hpp"

template<class> struct Point;
template<class> struct Quaternion;
template<class> struct DualQuaternion;

/** Stream insertion operator; left generic to support ostringstream, etc. exactly. */
template<class L, class S> L& operator<<(L&, Point<S> const&);
template<class L, class S> L& operator<<(L&, Quaternion<S> const&);
template<class L, class S> L& operator<<(L&, DualQuaternion<S> const&);

template<class S, class DELIM = const char*>
std::enable_if_t<std::is_scalar<S>::value, std::string>
to_string(S s, unsigned prec = 0, bool show_zero = true, bool fill_zeroes = false) {
	using namespace std;

	// TODO isnan, isinf fallback for non-floats
	// if(isnan(s)) return "NaN";
	// if(isinf(s)) return "Inf";

	bool neg = false;
	if(s == -0.f) s = 0;
	else if(s < 0) neg = true, s = -s;

	unsigned p10 = pow(10, prec);
	s = round(s*p10) / p10;

	if(!prec) {
		unsigned u = s;
		auto ustr = std::to_string(u);
		return ((neg && u) ? "-" : "") + ustr;
	}

	auto tostr = std::to_string(s);
	auto len = tostr.length();
	if(!len) tostr = "0", len = 1;

	auto decimal = tostr.find('.');
	string major = tostr.substr(0, decimal), point = "", minor;
	if(decimal+1 < len) minor = tostr.substr(decimal+1);
	auto z0 = major.find('0'), nz0 = major.find_first_not_of('0'),
		z1 = minor.find_last_of('0'), nz1 = minor.find_last_not_of('0');
	if(nz0 == string::npos) {
		if(show_zero) major = "0";
		if(show_zero && (nz0 == string::npos) && (nz1 == string::npos)) major = "0";
		//if(show_zero || (nz1 == string::npos)) major = "0";
		else major = "";
	}
	if(!fill_zeroes && (nz0 == string::npos)) major = "0";
	if(show_zero && (nz0 == string::npos)) major = "0";

	if(fill_zeroes || (nz1 != string::npos))
		point = ".";
	if(nz1 == string::npos) {
		if(fill_zeroes) point = ".", minor = string(prec, '0');
		else point = minor = "";
	} else {
		if(z1 != string::npos) {
			if(!fill_zeroes && (z1 > nz1))
				minor = minor.substr(0, nz1+1);
		}
	}

	len = minor.length();
	int dlen = prec - len;
	if(dlen < 0) minor = minor.substr(0, prec);
	else if(fill_zeroes && (dlen > 0))
		minor += string(dlen, '0');
	if(!minor.length()) point = "";

	auto out = major + point + minor;
	if(neg && (out.find_first_not_of("0.") != string::npos))
		return "-" + out;
	return out;
}

template<class S, class... T>
std::string to_string(Point<S> const& d, T &&... t);
template<class S, class... T>
std::string to_string(Quaternion<S> const& d, T &&... t);
template<class S, class... T>
std::string to_string(DualQuaternion<S> const& d, T &&... t);

template<class S, class... T>
std::enable_if_t<std::is_arithmetic<S>::value, std::string>
to_unit_string(S s, char i, T &&... t) {
	auto out = to_string(s, std::forward<T>(t)...);
	if(out == "0") return "";
	if(out == "1") return std::string("") + i;
	if(out == "-1") return std::string("-") + i;
	return out + i;
}

/** @brief Named conjugate (for types/contexts where unary * may not be the conjugate) */
template<class S> S conjugate(S const& s);
/** @brief Expected specialization of named conjugate (equivalent to unary *) */
template<class S> DualQuaternion<S> conjugate(DualQuaternion<S> const& d) { return *d; }
/** @brief Expected specialization of named conjugate (equivalent to unary *) */
template<class S> DualQuaternion<S> conjugate(Quaternion<S> const& q) { return *q; }

template<class S, class T, class ST = std::common_type_t<S,T>>
DualQuaternion<ST> operator*(Point<S> const& p, DualQuaternion<T> const& d);
template<class S, class T, class ST = std::common_type_t<S,T>>
DualQuaternion<ST> operator*(DualQuaternion<S> const& l, Point<T> const& p);

/** @brief Abstract real; specialize as necessary */
template<class S>
std::enable_if_t<std::is_arithmetic<S>::value, S>
getReal(S const& s) { return s; }
template<class S>
S getReal(Quaternion<S> const& q) { return q.w; }
template<class S>
S getReal(DualQuaternion<S> const& d) { return d.s; }
/*template<class S>
S getNonreal(S const& s) { return s - getReal(s); }*/

/** @brief Abstract linear interpolation; see bezier, etc. */
template<class S, class T>
S lerp(S const& u, S const& v, T const& t)
	{ return u + (v-u)*t; }
/** @brief Abstract bilinear interpolation; see bezier, etc. */
template<class S, class T>
S lerp(S const& u0, S const& u1, S const& v0, S const& v1, T t0, T t1)
	{ return (u0*(1-t0)+u1*t0)*(1-t1) + (v0*(1-t0)+v1*t0)*t1; }

/** Abstract comparison to zero (mainly intended for floating point types) */
template<class L, class R = L>
bool near_zero(L, R = std::numeric_limits<std::remove_reference_t<L>>::epsilon());

/** Abstract comparison (mainly intended for floating point types) */
template<class L, class R = L, class T = R>
bool near(L, R, T = std::numeric_limits<
		std::remove_reference_t<std::common_type_t<L,R>>>::epsilon());

/** Abstract number of digits in decimal */
template<class T> long numDigits(T const& t);


/* Credit to floating-point-gui.de for the combination of relative and absolute comparisons;
 * I read enough of Bruce Dawson's articles to decide that a "decent" solution will have to do
 * without specific constraints. */
template<class L, class R, class T>
bool near(L lhs, R rhs, T least) {
	using LRT = std::common_type_t<L, R, T>;
	using NL = std::numeric_limits<LRT>;
	auto epsilon = NL::epsilon(), minnorm = NL::min();
	if(lhs == rhs) return true;
	L labs = abs(lhs), rabs = abs(rhs), dabs = abs(lhs - rhs);
	if((labs == 0) || (rabs == 0) || (dabs < minnorm))
		return dabs < (least * minnorm);
	return dabs / std::min(labs + rabs, NL::max()) < least;
}
template<class L, class T>
bool near_zero(L lhs, T least) {
	return near(lhs, 0, least);
}

template<class T>
long numDigits(T const& t) {
	long out = ceil(log10(abs(long(t))+1));
	return out ? out : 1;
}
template<class T>
long numDigits(T const& t, unsigned radix) {
	long out = ceil(log(abs(long(t))+1)/log(radix));
	return out ? out : 1;
}

#endif
