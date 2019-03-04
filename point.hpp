#ifndef POINT_HPP
#define POINT_HPP

#include "quaternion.hpp"

template<class S>
struct Point {
	S x = 0, y = 0, z = 0;
	const S& operator[](char c) const {
		switch(c) {
			case 'i': case 'x': return x;
			case 'j': case 'y': return y;
			case 'k': case 'z': return z;
			default: return x;
		}
	}
	S& operator[](char c) {
		switch(c) {
			case 'i': case 'x': return x;
			case 'j': case 'y': return y;
			case 'k': case 'z': return z;
			default: return x;
		}
	}
	/*const S& operator[](unsigned u) const {
		switch(u) {
			case 0: return x;
			case 1: return y;
			case 2: return z;
			default: return x;
		}
	}
	S& operator[](unsigned u) {
		switch(u) {
			case 0: return x;
			case 1: return y;
			case 2: return z;
			default: return x;
		}
	}*/
	template<class T = S>
	bool operator==(Point<T> const& p) const {
		return x == p.x && y == p.y && z == p.z;
	}
	template<class T, class ST = std::common_type_t<S,T>>
	Point<ST> operator+(Point<T> const& p) const { return {x+p.x, y+p.y, z+p.z}; }
	template<class T>
	Point<S>& operator+=(Point<T> const& p)
		{ return x += S(p.x), y += S(p.y), z += S(p.z), *this; }
	Point<S> operator-(void) const { return {-x, -y, -z}; }
	template<class T, class ST = std::enable_if_t<std::is_arithmetic<T>::value, std::common_type_t<S,T>>>
	friend Point<ST> operator*(T const& t, Point<S> const& p)
		{ return {t*p.x, t*p.y, t*p.z}; }
	template<class T, class ST = std::common_type_t<S,T>>
	Point<ST> operator*(T const& t) const { return {x*t, y*t, z*t}; }

	template<class T, class ST = std::common_type_t<S,T>>
	Point<ST> operator-(Point<T> const& p) const { return {x-p.x, y-p.y, z-p.z}; }
	template<class T>
	Point<S>& operator-=(Point<T> const& p)
		{ return x -= S(p.x), y -= S(p.y), z -= S(p.z), *this; }

	S lengthSquared(void) const { return x*x + y*y + z*z; }
	S length(void) const { return sqrt(lengthSquared()); }

	Point normalize(void) const {
		auto len = length();
		if(len == 0) return {0}; // TODO
		return *this / len;
		//return *this * (1 / len);
	}

	template<class T, class ST = std::common_type_t<S,T>>
	Point<ST> operator^(Quaternion<T> const& t) const {
		ST p = -t.x*x - t.y*y - t.z*z, q = t.w*x + t.y*z - t.z*y,
			r = t.w*y - t.x*z + t.z*x, s = t.w*z + t.x*y - t.y*x;
		return {
			-p*t.x + q*t.w - r*t.z + s*t.y,
			-p*t.y + q*t.z + r*t.w - s*t.x,
			-p*t.z - q*t.y + r*t.x + s*t.w
		};
	}
	template<class T, class ST = std::common_type_t<S,T>>
	Point<ST> operator^(DualQuaternion<T> const& t) const {
		ST p = -t.t*x - t.u*y - t.v*z, q = t.s*x + t.u*z - t.v*y,
			r = t.s*y - t.t*z + t.v*x, s = t.s*z + t.t*y - t.u*x;
		return {
			-p*t.t + q*t.s - r*t.v + s*t.u
				+ 2*(-t.w*x + t.x*t.s),
			-p*t.u + q*t.v + r*t.s - s*t.t
				+ 2*(-t.w*y + t.y*t.s),
			-p*t.v - q*t.u + r*t.t + s*t.s
				+ 2*(-t.w*z + t.z*t.s)
		};
	}
	template<class T, class ST = std::common_type_t<S,T>>
	Point<ST> operator/(T const& t) const {
		return {x/t, y/t, z/t};
	}

	/**
	 * @brief Converts a point to a half-translation to be used in a sandwich product.
	 * If the result were to be used as the object in a sandwich product and not the subject,
	 * it would be half-scale.
	 * TODO make conversion explicit or introduce intermediates like 'translation'/'position'?
	 */
	template<class T>
	//operator DualQuaternion<T>(void) const { return {1, 0, 0, 0, 0, T(x/2), T(y/2), T(z/2)}; }
	operator DualQuaternion<T>(void) const { return {1, 0, 0, 0, 0, x, y, z}; }
	template<class T>
	friend T& operator<<(T &t, Point const& p) { return t << to_string(p), t; }
	operator std::string(void) const { return to_string((DualQuaternion<S>) *this, 2, false); }
};
template<class S, class T, class ST = std::common_type_t<S,T>>
ST dot(Point<S> const& s, Point<T> const& t)
	{ return ST(s.x*t.x + s.y*t.y + s.z*t.z); }
template<class S, class T, class ST = std::common_type_t<S,T>>
Point<ST> cross(Point<S> const& s, Point<T> const& t)
	{ return {s.y*t.z-s.z*t.y, s.z*t.x-s.x*t.z, s.x*t.y-s.y*t.x}; }
/*template<class S, class T, class ST = std::common_type_t<S,T>>
Point<ST> operator*(Point<S> const& s, Point<T> const& t) {
	return cross(s, t);
}*/

template<class U>
std::pair<Point<U>, Point<U>> perpendicular(Point<U> const& p, bool norm = false) {
	typedef Point<U> Pt;
	Pt i = {1, 0, 0}, j = {0, 1, 0}, k = {0, 0, 1},
		u = i, w = norm ? p.normalize() : p, v = j;
		//u = i, w = p, v = j;
	auto wi = dot(w, i), wi2 = wi*wi, // wu = wi,
		wj = dot(w, j), wj2 = wj*wj,
		wk = dot(w, k), wk2 = wk*wk;
	if(wi2 > wj2) {
		if(wj2 > wk2) u = k; // wu = wk;
		else u = j; // wu = wj;
	} else if(wi2 > wk2) u = k; // wu = wk;
	//v = normalize(cross(w, u));
	//u = normalize(cross(v, w));
	v = cross(w, u);
	u = cross(v, w);
	if(norm) u = u.normalize(), v = v.normalize();
	return std::make_pair(u, v);

	/*auto uvw = dot(cross(u, v), w);
	return std::make_pair(u/uvw, v/uvw);*/
}

template<class S>
Point<S> normalize(Point<S> const& p) { return p.normalize(); }

template<class S, class... T>
std::string to_string(Point<S> const& p, T &&... t) {
	return "(" + to_string(p.x, std::forward<T>(t)...) + ", "
			+ to_string(p.y, std::forward<T>(t)...) + ", "
			+ to_string(p.z, std::forward<T>(t)...) + ")";
}

#define USERDEF_TEMPLATE(CLASS,PARAM,UD,X,Y,Z) \
CLASS<PARAM> operator"" UD(long long unsigned d) \
	{ return {PARAM(X), PARAM(Y), PARAM(Z)}; } \
CLASS<PARAM> operator"" UD(long double d) \
	{ return {PARAM(X), PARAM(Y), PARAM(Z)}; }
#define USERDEF_TEMPLATES(CLASS, P1, UD1, P2, UD2, X, Y, Z) \
	USERDEF_TEMPLATE(CLASS, P1, UD1, X, Y, Z) \
	USERDEF_TEMPLATE(CLASS, P2, UD2, X, Y, Z)

USERDEF_TEMPLATES(Point, float, _x, double, _dx, d, 0, 0)
USERDEF_TEMPLATES(Point, float, _y, double, _dy, 0, d, 0)
USERDEF_TEMPLATES(Point, float, _z, double, _dz, 0, 0, d)

#undef USERDEF_TEMPLATE
#undef USERDEF_TEMPLATES

#endif
