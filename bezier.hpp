#ifndef BEZIER_HPP
#define BEZIER_HPP

#include <functional> // TODO remove, used in poly-looping
#include <vector>

#include "binomial.hpp"

template<class T, class U>
T linear(T x, T y, U u) {
	return x*(1-u) + y*u;
}
template<class T, class U>
T linear(T x1, T x2, T y1, T y2, U s, U t) {
	return linear(linear(x1, x2, s), linear(y1, y2, s), t);
}
template<class T, class U>
T linear(const T (&x) [2], const T (&y) [2], U u, U v) {
	return linear(linear(x[0], x[1], u), linear(y[0], y[1], u), v);
}
template<class T, class U>
T linear(const T (&x) [2][2], const T (&y) [2][2], U u, U v, U w) {
	return linear(linear(linear(x[0][0], y[0][0], u), linear(x[0][1], y[0][1], u), v),
		linear(linear(x[1][0], y[1][0], u), linear(x[1][1], y[1][1], u), v), w);
}

template<class T, class U>
T quadratic(T x, T y, T z, U u) {
	return linear(linear(x, y, u), linear(y, z, u), u);
}
template<class T, class U>
T quadratic(T nw, T n, T ne, T w, T c, T e, T sw, T s, T se, U const& u, U const& v) {
	auto nnw = linear(nw, n, u), nne = linear(n, ne, u), north = linear(nnw, nne, u),
		wc = linear(w, c, u), ce = linear(c, e, u), center = linear(wc, ce, u),
		ssw = linear(sw, s, u), sse = linear(s, se, u), south = linear(ssw, sse, u),
		north_center = linear(north, center, v), center_south = linear(center, south, v);
	return linear(north_center, center_south, v);
}
template<class T, class U>
T quadratic(const T (&x)[3], const T (&y)[3], const T (&z)[3], U const& u, U const& v) {
	return quadratic(quadratic(x[0], x[1], x[2], u),
			quadratic(y[0], y[1], y[2], u),
			quadratic(z[0], z[1], z[2], u), v);
}

template<class T, class U>
T cubic(T w, T x, T y, T z, U u) {
	return linear(quadratic(w, x, y, u), quadratic(x, y, z, u), u);
}
template<class T, class U>
T cubic(T (&w) [4], T (&x) [4], T (&y) [4], T (&z) [4], U u, U v) {
	return cubic(cubic(w[0], w[1], w[2], w[3], u),
			cubic(x[0], x[1], x[2], x[3], u),
			cubic(y[0], y[1], y[2], y[3], u),
			cubic(z[0], z[1], z[2], z[3], u), v);
}

/*
template<class S, class T, unsigned N>
S getNormalized(S const& s, T const (&bounds) [N]) {
	static_assert(N > 0, "Cannot normalize without at least an upper bound!");
	if(N > 1) return s >= bounds[0] && s < bounds[1];
	return s < bounds[0];
}
// In-place array normalization
template<class S, class T, unsigned N>
auto getNormalized(S (&params) [N], T const (&minima) [N], T const (&maxima) [N])
	-> decltype(params) { return getNormalized(params, minima, maxima, params); }

// Array normalization (not in-place)
template<class S, class T, class U, unsigned N, class R = S>
void getNormalized(R (&out) [N], S const (&in) [N], T const (&minima) [N], U const (&maxima) [N]) {
	typedef std::common_type_t<T, U> TU;
	typedef std::common_type_t<S, T> ST;
	typedef std::common_type_t<S, TU> STU;

	TU widths[N] = {0};
	std::transform(minima, minima+N, maxima, widths, std::minus<TU>());
	std::transform(params, params+N, minima, out, std::minus<ST>());
	std::transform(out, out+N, widths, out, std::divides<T>());
}

template<class S, class MAXIMA, class MINIMA>
struct PolyForEach;

template<class S, class T, T... MINIMA, T... MAXIMA>
struct PolyForEach<S, Detail::Seq<T, MINIMA...>, Detail::Seq<T, MAXIMA...>> {
	static constexpr unsigned NMAXIMA = sizeof...(MAXIMA), NMINIMA = sizeof...(MINIM),
			RANK = NMAXIMA;
	static_assert(NMAXIMA >= NMINIMA, "Only minima can be implicit (default to 0)");
	static constexpr unsigned maxima[RANK] = {MAXIMA...}, minima[RANK] = {MINIMA...};

	// Requires NMAXIMA == NMINIMA:
	//   static constexpr unsigned bounds[RANK][2] = {{MINIMA, MAXIMA}...};

	typedef unsigned indices_type [RANK];
	typedef S params_type [RANK];

	typedef void (*fnAct_type) (params_type & params);
	template<class R> struct Accumulate {
		typedef R (*fnAccumType) (R const&, R const&);
	};
};*/

/*template<class S, class U = unsigned, unsigned BOUNDS,
		class VS = std::vector<S>, class VU = std::vector<U>, class... TN>
VS polyLoop(S (*fn)(VS const&), VS (*norm) (VU const&, VU const&, TN &&...),
		VU const (&bounds) [BOUNDS], TN &&... tn) {
	VU params = {0};
	VS out;
	for(unsigned cursor = 0, N = bounds.size(); cursor < N;) {
		params[cursor]++;
		if(params[cursor] < bounds[cursor]) {
			cursor = 0;
		} else {
			for(unsigned i = 0; i < cursor; i++) params[i] = 0;
			for(; cursor < N; cursor++) {
				params[cursor]++;
				if(params[cursor] < bounds[cursor]) {
					cursor = 0;
					break;
				}
				params[cursor] = 0;
			}
			if(cursor) break;
			//out.emplace_back(fn(norm(params, bounds)));
			// auto normed = normalizeAll<S>(params, bounds);
			out.emplace_back(fn(params));
		}
	}
	return out;
}*/

template<class S, unsigned MD, unsigned ND>
void binomialBasis(S (&basis) [MD] [ND]) {
	static_assert(MD * ND > 0, "Must have nonzero dimensions!");
	using namespace Detail;
	typedef SeqArray<Value_t<Binomial<MD-1>>> mb_type;
	typedef SeqArray<Value_t<Binomial<ND-1>>> nb_type;
	static constexpr auto ub = nb_type::value;
	static constexpr auto vb = mb_type::value;
	/*typedef Value_t<Binomial<MD-1>> mb_type;
	typedef Value_t<Binomial<ND-1>> nb_type;
	static constexpr auto ub = nb_type::value;
	static constexpr auto vb = mb_type::value;*/
	for(unsigned i = 0; i < MD; i++) {
		for(unsigned j = 0; j < ND; j++) {
			basis[i][j] = vb[i] * ub[j];
		}
	}
}

template<class S, unsigned MD, unsigned ND>
void bezierBasis(S (&basis) [MD] [ND], S u, S v) {
	binomialBasis(basis);
	S s = 1-u, t = 1-v;
	S curs = 1, curt = 1, curu = 1, curv = 1,
		ss[MD] = {0}, us[MD] = {0},
		ts[ND] = {0}, vs[ND] = {0};
	for(unsigned i = 0; i < MD; i++) {
		curt *= t;
		curv *= v;
		ts[i] = curt;
		vs[i] = curv;
	}
	for(unsigned j = 0; j < ND; j++) {
		curs *= s;
		curu *= u;
		ss[j] = curs;
		us[j] = curu;
	}
	for(unsigned i = 0; i < MD; i++) {
		for(unsigned j = 0; j < ND; j++) {
			//basis[i][j] *= us[ND-j-1] * ss[j] * vs[MD-i-1] * ts[i];
			basis[i][j] *= pow(s, MD-j-1) * pow(u, j) * pow(t, ND-i-1) * pow(v, i);
			//basis[i][j] *= pow(s, MD-j-1) * pow(t, ND-i-1);
		}
	}
}

template<unsigned MD, unsigned ND, unsigned M, unsigned N, class S, class T>
T bezier(T const (&points) [M] [N], S u, S v) {
	static_assert(M >= MD && N >= ND, "There are not enough points!");
	T out = {0};
	S basis[MD][ND], uj = u * (N-1), vi = v * (M-1);
	// Find the range closest to centered around the point inside the bounds
	unsigned i0 = std::max<int>(0, -M/2 + vi), j0 = std::max<int>(0, -N/2 + uj),
			i1 = std::min<int>(-1 + MD + i0, -1 + M), j1 = std::min<int>(-1 + ND + j0, -1 + N);
	// Remap the point to the interior of the range
	uj = (uj - j0) / (j1 - j0);
	vi = (vi - i0) / (i1 - i0);
	// Generate the basis at the new point
	bezierBasis(basis, uj, vi);
	for(unsigned i = 0, k = i0; i < MD; i++, k++) {
		for(unsigned j = 0, l = j0; j < ND; j++, l++) {
			out += basis[i][j] * points[k][l];
		}
	}
	return out;
}

#endif
