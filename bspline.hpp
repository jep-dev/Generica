#ifndef BSPLINE_HPP
#define BSPLINE_HPP

#include "bezier.hpp"
#include "size.hpp"
#include "utility.hpp"
#include <vector>

// knots = { 0, ..., 0, 0, 1, ..., M-1, M, M, ..., M }
//          |----D----||-------M+1-------||----D----|

namespace Detail {

	/** @brief One-dimensional unit step function */
	template<class S>
	bool step(S const& sMin, S const& s, S const& sMax) {
		return s >= sMin && s < sMax;
	}

	/** @brief N-dimensional unit step function */
	template<class S, unsigned DIM>
	//bool step(S const (&domain)[DIM][2], S const (&s) [DIM]) {
	bool step(S const (&mins) [DIM], S const (&s) [DIM], S const (&maxes) [DIM]) {
		for(unsigned i = 0; i < DIM; i++)
			if(!step(mins[i], s[i], maxes[i])) return false;
		return true;
	}

	/*template<class T, unsigned N>
	std::size_t get_size(T const (&t) [N]) { return N; }
	template<class T>
	std::size_t get_size(T const& t) { return t.size(); }*/

	template<unsigned D, class K, class S, class V = std::vector<S>>
	V basis(K const& knots, S const& u) {
		// d = m - n - 1
		// |knots|=m+1
		//   m = |knots|-1
		// |controls|=n+1
		// d = |knots|-1 - (|controls|-1) - 1
		// d = |knots| - |controls| - 1
		// |controls| = |knots| - d - 1
		auto M = -1 + ::getSize(knots), N = M - D;
		// TODO Only D+1 will be used, improve here
		V out(N+1, 0);
		// Special cases
		if(u <= knots[0]) return out[0] = 1, out;
		if(u >= knots[N]) return out[N] = 1, out;

		// Find the current knot span
		unsigned k;
		for(k = 0; k <= M; k++)
			if(step(knots[k], u, knots[k+1]))
				break;
		out[k] = 1;
		for(unsigned d = 0; d < D; d++) {
			out[k-d] = (knots[k+1]-u)/(knots[k+1]-knots[k-d+1]) * out[k-d+1];
			for(unsigned i = k - d + 1; i < k; i++)
				out[i] = (u-knots[i])/(knots[i+d]-knots[i]) * out[i]
						+ (knots[i+d+1]-u)/(knots[i+d+1]-knots[i+1]) * out[i+1];
			out[k] = (u-knots[k])/(knots[k+d]-knots[k]) * out[k];
		}
		return out;
	}

}

template<unsigned UD, unsigned VD, unsigned UM, unsigned VM,
		class C, class UK, class VK, class S>
C bSpline(C const (&controls) [UM][VM], UK const& uKnots, VK const& vKnots, S u, S v) {
	using namespace Detail;

	// Each control has a basis function
	auto uBasis = basis<UD>(uKnots, u);
	auto vBasis = basis<VD>(vKnots, v);
	C out = {0};

	for(unsigned i = 0; i < UM; i++) {
		auto const& ub = uBasis[i];
		auto const& row = controls[i];
		for(unsigned j = 0; j < VM; j++) {
			out += ub * vBasis[j] * row[j];
		}
	}
	return out;
}

#endif
