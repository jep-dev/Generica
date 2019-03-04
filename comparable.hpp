#ifndef COMPARABLE_HPP
#define COMPARABLE_HPP

#include "tag.hpp"

template<class C, class S, S U> struct Comparable;
template<class C, class S, S U> struct Comparable {
		//Detail::Tag<C, Detail::Seq<S, U>> {
	typedef S key_type;
	static constexpr S key_value = U;

	template<class D, S V> using lesser =
		std::conditional_t<(U < V), C, D>;
	template<class D, S V> using lesser_comparable =
		Comparable<lesser<D, V>, S, (U < V ? U : V)>;

	template<class D, S V> using greater =
		std::conditional_t<(U > V), C, D>;
	template<class D, S V> using greater_comparable =
		Comparable<greater<D, V>, S, (U > V ? U : V)>;


	// Comparison of comparables is comparison of keys
	template<class D, S V> constexpr bool
	operator<(Comparable<D, S, V> const&) const { return U < V; }
	template<class D, S V> constexpr bool
	operator==(Comparable<D, S, V> const&) const { return U == V; }
	template<class D, S V> constexpr bool
	operator>(Comparable<D, S, V> const&) const { return U > V; }
	template<class D, S V> constexpr bool
	operator!=(Comparable<D, S, V> const&) const { return U != V; }

	constexpr Comparable(void) {}
};

#endif
