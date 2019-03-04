#ifndef DUAL_HPP
// Note: __FILE__ expands to the caller's file, not this file; TODO
#define DUAL_HPP "dual.hpp"
#define DUAL_IMPL DUAL_HPP

///@cond
#include <algorithm>
///@endcond

#include "quaternion.hpp"

/** A dual quaternion with a flat structure. */
template<class S> struct DualQuaternion {
	using value_type = S;
	S s, t, u, v, w, x, y, z;
	/** Const member access by index and unit. */
	const S& operator[](unsigned i) const {
		switch(i) {
			case 1: case 'i': return t;
			case 2: case 'j': return u;
			case 3: case 'k': return v;
			case 4: case 'E': return w;
			case 5: case 'I': return x;
			case 6: case 'J': return y;
			case 7: case 'K': return z;
			default: return s;
		}
	}
	/** Member access by index and unit. */
	S& operator[](unsigned i) {
		switch(i) {
			case 1: case 'i': return t;
			case 2: case 'j': return u;
			case 3: case 'k': return v;
			case 4: case 'E': return w;
			case 5: case 'I': return x;
			case 6: case 'J': return y;
			case 7: case 'K': return z;
			default: return s;
		}
	}
	operator Point<S>(void) const { return {x, y, z}; }

	/** Square, as in product with itself. */
	value_type square(void) const { return *this * *this; }
	/** Square of the norm, as in sum of squared elements. */
	value_type lengthSquared(void) const { return s*s + t*t + u*u + v*v + w*w + x*x + y*y + z*z; }
	/** Euclidean norm. */
	value_type length(void) const { return sqrt(lengthSquared()); }
	/** Conjugate; composed by the complex and dual conjugates. */
	template<class T> DualQuaternion& operator=(DualQuaternion<T> const& d) {
		s = d.s, t = d.t, u = d.u, v = d.v;
		w = d.w, x = d.w, y = d.y, z = d.z;
		return *this;
	}

	DualQuaternion operator*(void) const { return {s, -t, -u, -v, -w, x, y, z}; }
	DualQuaternion operator~(void) const { return {s, t, u, v, w, -x, -y, -z}; }
	DualQuaternion operator-(void) const { return {-s, -t, -u, -v, -w, -x, -y, -z}; }

	/* If q is a rotation, does adding a rotation make sense?
	 * Adding to the dual components might, if only non-duals could represent
	 * translation on their own.
	 * Even adding dual quaternions to one another is only intended for construction.
	 */
	template<class T>
	DualQuaternion& operator+=(T const& t) { return *this = *this + t; }
	template<class T>
	DualQuaternion& operator-=(T const& t) { return *this = *this - t; }

	template<class C, class SC = std::common_type_t<S,C>>
	DualQuaternion<SC> operator+(DualQuaternion<C> const& r) const
		{ return {s + r.s, t + r.t, u + r.u, v + r.v, w + r.w, x + r.x, y + r.y, z + r.z}; }

	template<class C, class SC = std::common_type_t<S,C>>
	DualQuaternion<SC> operator-(DualQuaternion<C> const& r) const
		{ return {s - r.s, t - r.t, u - r.u, v - r.v, w - r.w, x - r.x, y - r.y, z - r.z}; }

	template<class T>
	operator DualQuaternion<T>(void) {
		return {T(s), T(t), T(u), T(v), T(w), T(x), T(y), T(z)};
	}
	template<class C, class SC = std::common_type_t<S,C>>
	std::enable_if_t<!std::is_same<DualQuaternion<S>, C>::value, DualQuaternion<SC>>
	operator*=(C const& c) { return *this = *this * c; }
	template<class C>
	DualQuaternion& operator*=(Quaternion<C> const& c) {
		return *this = *this * c;
	}
	template<class C>
	DualQuaternion& operator*=(DualQuaternion<C> const& c) {
		return *this = *this * c;
	}

	template<class T>
	DualQuaternion& operator+=(DualQuaternion<T> const& t)
		{ return *this = *this + DualQuaternion<S>{t.s, t.t, t.u, t.v, t.w, t.x, t.y, t.z}; }
	template<class T, class ST = std::common_type_t<S,T>>
	DualQuaternion<ST> operator+(Point<T> const& t) {
		return {ST(s), ST(t), ST(u), ST(v),
			ST(w), ST(x+t.x), ST(y+t.y), ST(z+t.z)};
	}
	template<class T>
	DualQuaternion& operator+=(Point<T> const& t) {
		x = S(x+t.x);
		y = S(y+t.y);
		z = S(z+t.z);
		return *this;
	}
	template<class T>
	DualQuaternion& operator-=(DualQuaternion<T> const& t)
		{ return *this = *this - DualQuaternion<S>{t.s, t.t, t.u, t.v, t.w, t.x, t.y, t.z}; }

	template<class T, class V = typename T::value_type, class SV = std::common_type_t<S,V>>
	DualQuaternion<SV> operator^(T const& r) const { return r * *this * *r; }

	/** Equality operator; true if and only if all members are identical. */
	bool operator==(DualQuaternion const& d) const
		{ return s == d.s && t == d.t && u == d.u && v == d.v
				&& w == d.w && x == d.x && y == d.y && z == d.z; }
	operator std::string(void) const;

	DualQuaternion(S s = 0, S t = 0, S u = 0, S v = 0, S w = 0, S x = 0, S y = 0, S z = 0):
		s(s), t(t), u(u), v(v), w(w), x(x), y(y), z(z) {}
	template<class T>
	DualQuaternion(DualQuaternion<T> && d):
		s(std::move(d.s)), t(std::move(d.t)), u(std::move(d.u)), v(std::move(d.v)),
		w(std::move(d.w)), x(std::move(d.x)), y(std::move(d.y)), z(std::move(d.z)) {}
	template<class T>
	DualQuaternion(Quaternion<T> && q):
		s(std::move(q.w)), t(std::move(q.x)), u(std::move(q.y)), v(std::move(q.z)),
		w(0), x(0), y(0), z(0) {}
	template<class T>
	DualQuaternion(DualQuaternion<T> const& d):
		s(d.s), t(d.t), u(d.u), v(d.v), w(d.w), x(d.x), y(d.y), z(d.z) {}
	template<class T>
	DualQuaternion(Quaternion<T> const& q):
		s(q.w), t(q.x), u(q.y), v(q.z), w(0), x(0), y(0), z(0) {}

	virtual ~DualQuaternion(void) {}
};

template<class S, class T = S>
DualQuaternion<T> exp(DualQuaternion<S> const& d) {
	Quaternion<T> q = {d.s, d.t, d.u, d.v}, eq = exp(q);
	return {eq.w, eq.x, eq.y, eq.z, d.w, d.x, d.y, d.z};
}
template<class S, class T = S>
DualQuaternion<T> log(DualQuaternion<S> const& d) {
	Quaternion<T> lq = log(Quaternion<T>{d.s, d.t, d.u, d.v});
	return {lq.w, lq.x, lq.y, lq.z, d.w, d.x, d.y, d.z};
}

template<class S>
DualQuaternion<S> inverse(DualQuaternion<S> const& d) {
	Quaternion<S> p = {d.s, d.t, d.u, d.v}, q = {d.w, d.x, d.y, d.z},
		pi = *p / p.lengthSquared(), pq = pi * q * pi;
		return {pi.w, pi.x, pi.y, pi.z, -pq.w, -pq.x, -pq.y, -pq.z};
}

template<class S, class T, class ST = std::common_type_t<S,T>>
DualQuaternion<ST> to_dual(Quaternion<S> const& s, Quaternion<T> const& t)
	{ return {s.w, s.x, s.y, s.z, t.w, t.x, t.y, t.z}; }

template<class S, unsigned SN, class T = S, unsigned TN = SN,
		class ST = std::common_type_t<S,T>>
DualQuaternion<ST> to_dual(const S (&s)[SN], const T (&t)[TN]) {
	static_assert(SN >= 4 && TN >= 4, "Components must contain (at least) 4 elements");
	return {s[0], s[1], s[2], s[3], t[0], t[1], t[2], t[3]};
}
template<class R, class X = R, class Y = X, class Z = X>
DualQuaternion<R> point(X x = 0, Y y = 0, Z z = 0)
	{ return {1, 0, 0, 0, 0, x, y, z}; }
template<class R, class X = R, class Y = X, class Z = X>
DualQuaternion<R> translation(X x = 0, Y y = 0, Z z = 0)
	{ return point<R>(R(x)/2, R(y)/2, R(z)/2); }

template<class R, class X = R, class Y = X, class Z = X,
		class S = X, class T = X, class U = X, class V = X>
DualQuaternion<R> trans_rotation(X x = 0, Y y = 0, Z z = 0, S s = 0, T t = 1, U u = 0, V v = 0) {
	// (1+Et)(r)=r+Etr
	// (r)(1+Et)=r+Ert
	auto rot = rotation<R>(s, t, u, v);
	//auto pt = Point<Y>(x/2, y/2, z/2) * rot;
	auto pt = Quaternion<R>{R(0), R(x)/2, R(y)/2, R(z)/2} * rot;
	//auto pt = Quaternion<Y>{0, x/2, y/2, z/2} * rot;
	return {rot.w, rot.x, rot.y, rot.z, pt.w, pt.x, pt.y, pt.z};
}
template<class R, class S = R, class T = S, class U = S, class V = S,
		class X = S, class Y = S, class Z = S>
DualQuaternion<R> rot_translation(S s = 0, T t = 0, U u = 0, V v = 0, X x = 0, Y y = 0, Z z = 0) {
	auto rot = rotation<R>(R(s), R(t), R(u), R(v));
	auto pt = rot * Point<R>{R(x)/2, R(y)/2, R(z)/2};
	return {rot.w, rot.x, rot.y, rot.z, pt.w, pt.x, pt.y, pt.z};
}

template<class X, class Y = X, class Z = std::common_type_t<X,Y>>
DualQuaternion<Z> operator*(Point<X> const& l, Quaternion<Y> const& r) {
	return {r.w, r.x, r.y, r.z,
			-l.x*r.x-l.y*r.y-l.z*r.z, l.x*r.w+l.y*r.z-l.z*r.y,
			-l.x*r.z+l.y*r.w+l.z*r.x, l.x*r.y-l.y*r.x+l.z*r.w};
}
template<class X, class Y = X, class Z = std::common_type_t<X,Y>>
DualQuaternion<Z> operator*(Quaternion<X> const& l, Point<Y> const& r) {
	return {l.w, l.x, l.y, l.z,
		-l.x*r.x-l.y*r.y-l.z*r.z, l.w*r.x+l.y*r.z-l.z*r.y,
		l.w*r.y-l.x*r.z+l.z*r.x, l.w*r.z+l.x*r.y-l.y*r.z};
}
template<class X, class Y = X, class Z = std::common_type_t<X,Y>>
DualQuaternion<Z> operator/(DualQuaternion<X> const& l, Y const& r) {
	return {l.s/r, l.t/r, l.u/r, l.v/r, l.w/r, l.x/r, l.y/r, l.z/r};
}
template<class X, class Y = X, class Z = std::common_type_t<X,Y>>
DualQuaternion<Z> operator/(X const& l, DualQuaternion<Y> const& r) {
	return l * inverse(r);
}
template<class X, class Y = X, class Z = std::common_type_t<X,Y>>
DualQuaternion<Z> operator/(Quaternion<X> const& l, DualQuaternion<Y> const& r) {
	return l * inverse(r);
}
template<class X, class Y = X, class Z = std::common_type_t<X,Y>>
DualQuaternion<Z> operator/(DualQuaternion<X> const& l, Quaternion<Y> const& r) {
	return l * inverse(r);
}
template<class X, class Y = X, class Z = std::common_type_t<X,Y>>
DualQuaternion<Z> operator/(DualQuaternion<X> const& l, DualQuaternion<Y> const& r) {
	return l * inverse(r);
}


template<class X, class Y = X>
DualQuaternion<Y> pivot(X && t, X && u, X && v, X && w, X && x, X && y, X && z) {
	auto rot = rotation<Y>(t, u, v, w);
	auto pt = Point<Y>{x/2, y/2, z/2};
	auto rpt = rot * pt, ptr = pt * rot, rptr = rpt - ptr;
	return {rot.w, rot.x, rot.y, rot.z, rptr.w, rptr.x, rptr.y, rptr.z};
}

// // TODO Should dot be non-dual dot (rotations), dual dot (translations), or both?
template<class S, class T>
std::common_type_t<S,T> dot(DualQuaternion<S> const& l, DualQuaternion<T> const& r) {
	/*return l.s*r.s + l.t*r.t + l.u*r.u + l.v*r.v
			+ l.w*r.w + l.x*r.x + l.y*r.y + l.z*r.z;*/
	return l.s*r.s + l.t*r.t + l.u*r.u + l.v*r.v;
}

template<class L, class R, class T, class LRT = std::common_type_t<L,R,T>>
DualQuaternion<LRT> lerp(DualQuaternion<L> const& lhs, DualQuaternion<R> const& rhs, T const& t)
	{ return lhs*(1-t) + (rhs-lhs)*t; }
template<class L, class R, class T, class LRT = std::common_type_t<L,R,T>>
DualQuaternion<LRT> sclerp(DualQuaternion<L> const& lhs, DualQuaternion<R> const& rhs,
		T && t, bool normalize = false) {
	auto ou = slerp(Quaternion<L>(lhs.s, lhs.t, lhs.u, lhs.v),
			Quaternion<R>(rhs.s, rhs.t, rhs.u, rhs.v), t, normalize),
		ov = lerp(Quaternion<L>(lhs.w, lhs.x, lhs.y, lhs.z),
			Quaternion<R>(rhs.w, rhs.x, rhs.y, rhs.z), t);
	return {ou.w, ou.x, ou.y, ou.z, ov.w, ov.x, ov.y, ov.z};
}
template<class L1, class R1 = L1, class L2 = L1, class R2 = R1, class S = L1, class T = S,
		class LRST = std::common_type_t<L1, R1, L2, R2, S, T>>
DualQuaternion<LRST> sclerp(DualQuaternion<L1> const& l1, DualQuaternion<R1> const& r1,
		DualQuaternion<L2> const& l2, DualQuaternion<R2> const& r2,
			S && s, T && t, bool normalize = false)
	{ return sclerp(sclerp(l1, r1, s, normalize), sclerp(l2, r2, s, normalize), t, normalize); }


template<class S, class C, class SC = std::common_type_t<S,C>>
DualQuaternion<SC> operator*(DualQuaternion<S> const& l, Quaternion<C> const& r) {
	return {
		l.s*r.w - l.t*r.x - l.u*r.y - l.v*r.z, l.s*r.x + l.t*r.w + l.u*r.z - l.v*r.y,
		l.s*r.y - l.t*r.z + l.u*r.w + l.v*r.x, l.s*r.z + l.t*r.y - l.u*r.x + l.v*r.w,
		l.w*r.w - l.x*r.x - l.y*r.y - l.z*r.z, l.w*r.x + l.x*r.w + l.y*r.z - l.z*r.y,
		l.w*r.y - l.x*r.z + l.y*r.w + l.z*r.x, l.w*r.z + l.x*r.y - l.y*r.x + l.z*r.w
	};
}

template<class S, class C, class SC = std::common_type_t<S,C>>
DualQuaternion<SC> operator*(Quaternion<S> const& l, DualQuaternion<C> const& r) {
	return {
		l.w*r.s - l.x*r.t - l.y*r.u - l.z*r.v, l.w*r.t + l.x*r.s + l.y*r.v - l.z*r.u,
		l.w*r.u + l.y*r.s - l.x*r.v + l.z*r.t, l.w*r.v + l.z*r.s + l.x*r.u - l.y*r.t,
		l.w*r.w - l.x*r.x - l.y*r.y - l.z*r.z, l.w*r.x + l.x*r.w + l.y*r.z - l.z*r.y,
		l.w*r.y - l.x*r.z + l.y*r.w + l.z*r.x, l.w*r.z + l.x*r.y - l.y*r.x + l.z*r.w
	};
}

template<class S, class C, class SC = std::common_type_t<S,C>>
DualQuaternion<SC> operator*(DualQuaternion<S> const& l, DualQuaternion<C> const& r) {
	return {
		l.s*r.s - l.t*r.t - l.u*r.u - l.v*r.v,  l.s*r.t + l.t*r.s + l.u*r.v - l.v*r.u,
		l.s*r.u + l.u*r.s - l.t*r.v + l.v*r.t,  l.s*r.v + l.v*r.s + l.t*r.u - l.u*r.t,
		l.s*r.w - l.t*r.x - l.u*r.y - l.v*r.z + l.w*r.s - l.x*r.t - l.y*r.u - l.z*r.v,
		l.s*r.x + l.t*r.w + l.u*r.z - l.v*r.y + l.w*r.t + l.x*r.s + l.y*r.v - l.z*r.u,
		l.s*r.y - l.t*r.z + l.u*r.w + l.v*r.x + l.w*r.u - l.x*r.v + l.y*r.s + l.z*r.t,
		l.s*r.z + l.t*r.y - l.u*r.x + l.v*r.w + l.w*r.v + l.x*r.u - l.y*r.t + l.z*r.s
	};
}
template<class S, class C, class SC = std::common_type_t<S,C>>
DualQuaternion<SC> operator*(DualQuaternion<S> const& l, C const& r) {
	return {l.s*r, l.t*r, l.u*r, l.v*r, l.w*r, l.x*r, l.y*r, l.z*r};
}
template<class S, class C, class SC = std::common_type_t<S,C>>
DualQuaternion<SC> operator*(C const& l, DualQuaternion<S> const& r) {
	return {l*r.s, l*r.t, l*r.u, l*r.v, l*r.w, l*r.x, l*r.y, l*r.z};
}

template<class S, class T, class ST = std::common_type_t<S,T>>
DualQuaternion<ST> operator+(Quaternion<S> const& q, DualQuaternion<T> const& r)
	{ return {q.w+r.s, q.x+r.t, q.y+r.u, q.z+r.v, r.w, r.x, r.y, r.z}; }
template<class S, class T, class ST = std::common_type_t<S,T>>
DualQuaternion<ST> operator+(DualQuaternion<S> const& q, Quaternion<T> const& r)
	{ return r + q; }

template<class S, class C, class SC = std::common_type_t<S,C>>
std::enable_if_t<!std::is_same<S,C>::value, DualQuaternion<SC>>
operator+(DualQuaternion<S> const& l, DualQuaternion<C> const& r) {
	return {l.s+r.s, l.t+r.t, l.u+r.u, l.v+r.v, l.w+r.w, l.x+r.x, l.y+r.y, l.z+r.z};
}

// Gosh, it sure would be nice to have templates right about now.
// Template user-defined literals require that the template argument is char...,
// which would force parsing to play a role in literals.
#define USERDEF_TEMPLATE(CLASS,PARAM,UD,W,X,Y,Z) \
CLASS<PARAM> operator"" UD(long long unsigned d) \
	{ return {0, 0, 0, 0, PARAM(W), PARAM(X), PARAM(Y), PARAM(Z)}; } \
CLASS<PARAM> operator"" UD(long double d) \
	{ return {0, 0, 0, 0, PARAM(W), PARAM(X), PARAM(Y), PARAM(Z)}; }
#define USERDEF_TEMPLATES(CLASS, P1, UD1, P2, UD2, W, X, Y, Z) \
	USERDEF_TEMPLATE(CLASS, P1, UD1, W, X, Y, Z) \
	USERDEF_TEMPLATE(CLASS, P2, UD2, W, X, Y, Z)

USERDEF_TEMPLATES(DualQuaternion, float, _E, double, _dE, d,0,0,0)
USERDEF_TEMPLATES(DualQuaternion, float, _I, double, _dI, 0,d,0,0)
USERDEF_TEMPLATES(DualQuaternion, float, _J, double, _dJ, 0,0,d,0)
USERDEF_TEMPLATES(DualQuaternion, float, _K, double, _dK, 0,0,0,d)

#undef USERDEF_TEMPLATE
#undef USERDEF_TEMPLATES

#endif
