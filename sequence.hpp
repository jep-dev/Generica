#ifndef SEQUENCE_HPP
#define SEQUENCE_HPP

#include "tag.hpp"
#include <iosfwd>

namespace Detail {

/** A variadic unsigned sequence, useful for indexing Tag/Val. */
template<class T, T... I> struct Seq;

/*template<class...> struct PolySeq;
template<class... S, S... V> struct PolySeq<Seq<S,V>...>: Tag<Seq<S,V>...> {
	typedef std::common_type_t<S...> common;
	typedef Tag<Seq<S,V>...> type;
	typedef Seq<common, common(V)...> homogeneous;
	static constexpr unsigned size = sizeof...(V);
	using type::contains;
	using type::contains_base;
	using type::contains_derived;
};*/

template<class S, class... T>
constexpr bool contains(S const& s, T &&... t);
template<class S, S I, S... J>
constexpr bool contains(Seq<S, J...> = {}, Seq<S, I> = {});


template<class T, T... I> struct Seq {
	typedef T type;
	static constexpr std::size_t size = 0;

	template<class S, S... J>
	static constexpr Seq<S, J...> prepend(Seq<S, J...> = {}) { return {}; }
	template<class S, S... J>
	static constexpr Seq<S, J...> append(Seq<S, J...> = {}) { return {}; }

	template<T J>
	static constexpr bool contains(void) { return false; }

};
template<class T, T I0, T... IN> struct Seq<T, I0, IN...> {
	typedef T type;
	static constexpr std::size_t size = 1 + sizeof...(IN);
	static constexpr T value = I0;

	constexpr operator T (void) const { return value; }

	template<std::size_t N>
	static constexpr T get(std::integral_constant<std::size_t, N> = {})
		{ return Seq<T, IN...>::get(std::integral_constant<std::size_t, N-1>{}); }
	static constexpr T get(std::integral_constant<std::size_t, 0>) { return I0; }
	template<std::size_t N>
	static constexpr T get(Seq<std::size_t, N>)
		{ return Seq<T, IN...>::get(std::integral_constant<std::size_t, N>{}); }

	template<class S, S... J, class ST = std::common_type_t<S, T>>
	static constexpr auto prepend(Seq<S, J...> = {})
			-> Seq<ST, (ST)J..., (ST)I0, (ST)IN...> { return {}; }
	template<class S, S... J, class ST = std::common_type_t<S, T>>
	static constexpr auto append(Seq<S, J...> = {})
			-> Seq<ST, (ST)I0, (ST)IN..., (ST)J...> { return {}; }

	template<T J>
	static constexpr bool contains(void)
		{ return Detail::contains(Seq<T, I0, IN...>{}, Seq<T, J>{}); }

	template<std::size_t N>
	using nth_type = Seq<T, get<N>()>;
};

template<class... S, S... V>
constexpr auto to_seq(Seq<S,V>...) -> Seq<std::common_type_t<S...>,
	static_cast<std::common_type_t<S...>>(V)...> { return {}; }
template<template<class...> class C, class... S, S... V>
constexpr auto to_seq(C<Seq<S,V>...> const&)
	-> decltype(to_seq(Seq<S,V>{}...)) { return {}; }
/*template<class... S, S... V>
constexpr auto to_seq(Tag<Seq<S,V>...>)
-> decltype(to_seq(Seq<S,V>{}...)) { return {}; }
template<class... S, S... V>
constexpr auto to_seq(std::tuple<Seq<S,V>...>)
-> decltype(to_seq(Seq<S,V>{}...)) { return {}; }
template<class... S, S... V>
constexpr auto to_seq(std::integral_constant<S,V>...>)
-> decltype(to_seq(Seq<S,V>{}...)) { return {}; }*/

template<class... S, S... V>
constexpr auto to_tag_seq(Seq<S,V>...)
	-> Tag<Seq<S, V>...> { return {}; }
template<template<class...> class C, class... S, S... V>
constexpr auto to_tag_seq(C<Seq<S,V>...> const& c)
	-> decltype(to_tag_seq(to_seq(c))) { return {}; }

template<class S, S U, S... V>
constexpr auto pop_front(Seq<S, U, V...> const&)
-> Seq<S, V...> { return {}; }
template<class S, S... V>
constexpr auto pop_back(Seq<S, V...> const& s)
-> decltype(reverse(pop_front(reverse(s)))) { return {}; }

/*template<std::size_t I, class S, S... V, S... W, class = std::enable_if_t<I, void>>
constexpr auto head(Seq<S, V...> const& v, Seq<S, W...> const&, SeqSz<I>)
-> decltype(head(pop_front(v), Seq<S, W..., Seq<S, V...>{}>{},
	SeqSz<I-1>{})) { return {}; }*/


/*template<std::size_t I, class S, S U, S... V, S... W,
		class = std::enable_if_t<I == 1, void>>
constexpr auto head(Seq<S, U, V...> const&, Seq<S, W...> const&, SeqSz<I>)
-> decltype(head(Seq<S, V...>{}, Seq<S, W..., U>{}, SeqSz<I-1>{})) { return {}; }

template<std::size_t I, class S, S U, S V, S... W, S... X,
		class = std::enable_if_t<(I >= 2 && I < 4), void>>
constexpr auto head(Seq<S, U, V, W...> const&, Seq<S, X...> const&, SeqSz<I>)
-> decltype(head(Seq<S, W...>{}, Seq<S, W..., U, V>{}, SeqSz<I-2>{})) { return {}; }
template<std::size_t I, class S, S U, S V, S W, S X, S... Y, S...Z,
		class = std::enable_if_t<(I >= 4), void>>
constexpr auto head(Seq<S, U, V, W, X, Y...> const&, Seq<S, Z...> const&, SeqSz<I>)
-> decltype(head(Seq<S, Y...>{}, Seq<S, Z..., U, V, W, X>{}, SeqSz<I-4>{})) { return {}; }

template<class S, S... V, S... W>
constexpr auto head(Seq<S, V...> const& v, Seq<S, W...> const&, SeqSz<0>)
-> Seq<S, W...> { return {}; }

template<std::size_t I, class S, S... V>
constexpr auto head(Seq<S, V...> const& v)
-> decltype(head(v, Seq<S>{}, SeqSz<I>{})) { return {}; }*/


/** @brief Even base case */
template<std::size_t I, class S, S... U, S... V,
	class = std::enable_if_t<(I == sizeof...(V)), void>>
constexpr auto head(Seq<S, U...> const&, Seq<S, V...> const&)
	-> Seq<S, V...> { return {}; }
/** @brief Odd base case */
template<std::size_t I, class S, S U, S... V, S... W,
	class = std::enable_if_t<(I == sizeof...(W)+1), void>>
constexpr auto head(Seq<S, U, V...> const&, Seq<S, W...> const&)
	-> Seq<S, W..., U> { return {}; }
/** @brief Inductive case */
template<std::size_t I, class S, S U, S V, S... W, S... X,
	class = std::enable_if_t<(I >= sizeof...(X)+2), void>>
constexpr auto head(Seq<S, U, V, W...> const&, Seq<S, X...> const&)
	-> decltype(head<I>(Seq<S, W...>{}, Seq<S, X..., U, V>{})) { return {}; }
/** @brief Inductive case, 2x (decreases recursion depth) */
template<std::size_t I, class S, S U, S V, S W, S X, S... Y, S... Z,
	class = std::enable_if_t<(I >= sizeof...(Z)+4), void>>
constexpr auto head(Seq<S, U, V, W, X, Y...> const& u, Seq<S, Z...> const&)
	-> decltype(head<I>(Seq<S, Y...>{}, Seq<S, Z..., U, V, W, X>{})) { return {}; }
template<std::size_t I, class S, S... U>
constexpr auto head(Seq<S, U...> const& u)
	-> decltype(head<I>(u, Seq<S>{})) { return {}; }


/** @brief Even base case, the tail is the remainder */
template<std::size_t I, class S, S... U,
	class = std::enable_if_t<(I == sizeof...(U)), void>>
constexpr auto tail(Seq<S, U...> const&) -> Seq<S, U...> { return {}; }
/** @brief Odd base case, the tail is the remainder */
template<std::size_t I, class S, S U, S... V,
	class = std::enable_if_t<(I == sizeof...(V))>>
constexpr auto tail(Seq<S, U, V...> const&) -> Seq<S, V...> { return {}; }
/** @brief Inductive case */
template<std::size_t I, class S, S U, S V, S... W,
	class = std::enable_if_t<(I <= sizeof...(W)), void>>
constexpr auto tail(Seq<S, U, V, W...> const& u)
	-> decltype(tail<I>(Seq<S, W...>{})) { return {}; }
/** @brief Inductive case (two steps in one call) */
template<std::size_t I, class S, S U, S V, S W, S X, S... Y,
	class = std::enable_if_t<(I <= sizeof...(Y)), void>>
constexpr auto tail(Seq<S, U, V, W, X, Y...> const& u)
	-> decltype(tail<I>(Seq<S, Y...>{})) { return {}; }

/*template<std::size_t I, class S, S... U,
	class = std::enable_if_t<(I == sizeof...(U))>
constexpr auto slice(Seq<S, U...> const& u)
-> Tag_t<decltype(head<I>(u), Seq<S>{})> { return {}; }
template<std::size_t I, class S, S... U,
	class = std::enable_if_t<(I < sizeof...(U))>
constexpr auto slice(Seq<S, U...> const& u)
-> Tag_t<decltype(head<I>(u), tail<I>(u))> { return {}; }*/
template<std::size_t I, class S, S... U>
constexpr auto slice(Seq<S, U...> const& u)
-> Tag<decltype(head<I>(u)), decltype(tail<I>(u))> { return {}; }

//-> decltype(tail<I>(pop_front(u))) { return {}; }


template<class S, class... T>
constexpr bool contains(S const& s, T &&... t) {
	return s.contains(std::forward<T>(t)...);
}

template<std::size_t N, class T, T... I>
constexpr auto get(Seq<T, I...>)
		{ return Seq<T, I...>::get(std::integral_constant<std::size_t, N>{}); }

template<class S, S... M, class T, T N>
struct Rotate<Seq<S, M...>, T, N> {
	typedef decltype(tag_seq_to_seq(Tag<Seq<S, M>...>{} << Seq<T, N>{})) type;
};

template<class T> struct SeqArray;
template<class T, T... I> struct SeqArray<Seq<T, I...>> {
	static constexpr T value[sizeof...(I)] = {I...};
};
template<class T, T... I>
constexpr T SeqArray<Seq<T, I...>>::value [sizeof...(I)];

/* Removed make_seq(), especially make_seq(void) for now:
 * make_seq determines the T in Seq<T,...> from its arguments, so without arguments,
 * the type should be undefined.
 * Given T, e.g.
 *     makeSeqU(...) -> SeqU<...>; // OK even if length is zero
 * behavior around the empty set is poorly defined, e.g.
 *     SeqI<> + SeqC<...> = SeqI<...> // SeqI<> carries type int; int + char = int; or
 *     SeqI<> + SeqC<...> = SeqC<...> // SeqI<> is memberless only int-related in name; or
 *     undefined in general, which might suggest Seq*<> should be undefined after all.
 */

template<class T, T... IN>
constexpr std::size_t getSize(Detail::Seq<T, IN...> = {}) { return sizeof...(IN); }

template<class T, T... V>
constexpr auto operator-(Seq<T, V...> const&) -> Seq<T, -V...> { return {}; }

/* To make sequences more expressive, operators between sequences are pairwise operators on
 * their elements. PAIRWISE is a macro function of a binary operator symbol used to generate
 * the corresponding operator overload. Its lifespan and all of its uses are inside this
 * if block. */
#ifndef PAIRWISE
#define PAIRWISE(OP) \
template<class TI, TI... I, class TJ, TJ... J, \
	class T = decltype(declval<TI>() OP declval<TJ>())> \
constexpr auto operator OP(Seq<TI, I...>, Seq<TJ, J...>) \
	-> Seq<T, (I OP J)...> { return {}; }

PAIRWISE(+) PAIRWISE(-) ///< Additive
PAIRWISE(*) PAIRWISE(/) PAIRWISE(%) ///< Multiplicative
PAIRWISE(<<) PAIRWISE(>>) ///< Bit shifts
PAIRWISE(^) PAIRWISE(&) PAIRWISE(|) ///< Bitwise logic
PAIRWISE(&&) PAIRWISE(||) ///< Bool logic

#undef PAIRWISE
#endif

template<bool I = true, bool... J> struct BoolAndSeq: std::integral_constant<bool, I> {};
template<bool I, bool J, bool... K> struct BoolAndSeq<I, J, K...>: BoolAndSeq<I && J, K...> {};

template<class S, S I = true, S... J, class BAS = BoolAndSeq<bool(I), bool(J)...>>
constexpr auto boolAndSeq(void) -> Value_t<BAS> { return BAS::value; }
template<class S, S I = true, S... J, class BAS = BoolAndSeq<bool(I), bool(J)...>>
constexpr auto boolAndSeq(Seq<S, I, J...>) -> Value_t<BAS> { return BAS::value; }

template<class S, S I, S... J> struct BitAndSeq: std::integral_constant<S, I> {};
template<class S, S I, S J, S... K> struct BitAndSeq<S, I, J, K...>: BitAndSeq<S, I & J, K...> {};

template<class S, S I = true, S... J, class BAS = BitAndSeq<S, I, J...>>
constexpr auto bitAndSeq(void) -> Value_t<BAS> { return BAS::value; }
template<class S, S I = true, S... J, class BAS = BitAndSeq<S, I, J...>>
constexpr auto bitAndSeq(Seq<S, I, J...>) -> Value_t<BAS> { return BAS::value; }

template<bool I = false, bool... J> struct BoolOrSeq: std::integral_constant<bool, I> {};
template<bool I, bool J, bool... K> struct BoolOrSeq<I, J, K...>: BoolOrSeq<I || J, K...> {};
template<class S, S I = false, S... J, class BOS = BoolOrSeq<bool(I), bool(J)...>>
constexpr auto boolOrSeq(Seq<S, I, J...> = {}) -> bool { return BOS::value; }
//constexpr auto boolOrSeq(Seq<S, I, J...> = {}) -> Value_t<BOS> { return BOS::value; }

template<class S, S I = 0, S... J> struct BitOrSeq: std::integral_constant<S, I> {};
template<class S, S I, S J, S... K> struct BitOrSeq<S, I, J, K...>: BitOrSeq<S, I | J, K...> {};
template<class S, S I = 0, S... J, class BOS = BitOrSeq<S, I, J...>>
//constexpr auto bitOrSeq(Seq<S, I, J...> = {}) -> Value_t<BOS> { return BOS::value; }
constexpr auto bitOrSeq(Seq<S, I, J...> = {}) -> bool { return BOS::value; }

/** @brief Defines value as the sum of all {I0, IN...}.
 * Not to be confused with pairwise sum, etc. */
template<class S> struct SumSeq;
template<class S, S I0>
struct SumSeq<Seq<S, I0>>: std::integral_constant<S, I0> {};
template<class S, S I0, S I1, S... IN>
struct SumSeq<Seq<S, I0, I1, IN...>>: SumSeq<Seq<S, I0+I1, IN...>> {};

/** @brief Represents N copies of I as a Seq. */
template<unsigned N, class T, T I>
struct RepeatSeq: RemoveCVRef_t<decltype(Seq<T,I>{}.append(RepeatSeq<N-1,T,I>{}))> {
	typedef typename RepeatSeq<N-1, T, I>::value_type prev_type;
	typedef decltype(Seq<T, I>{}.append(prev_type{})) value_type;
	constexpr operator value_type(void) const { return {}; }
};
template<class T, T I>
struct RepeatSeq<0, T, I> {};
template<class T, T I>
struct RepeatSeq<1, T, I>: Seq<T, I> {
	typedef Seq<T, I> value_type;
	constexpr operator value_type(void) const { return {}; }
};

/** @brief Represents the partial sum of a Seq. */
template<class T> struct PartialSumSeq;
template<class T, T I0>
struct PartialSumSeq<Seq<T, I0>>:
	Seq<T, I0> { typedef Seq<T, I0> value_type; };
template<class T>
struct PartialSumSeq: PartialSumSeq<InnerValue_t<T>>
	{ using PartialSumSeq<InnerValue_t<T>>::value_type; };
template<class T, T I0, T I1, T... IN>
struct PartialSumSeq<Seq<T, I0, I1, IN...>>:
	RemoveCVRef_t<decltype(Seq<T,I0>{}
			.append(PartialSumSeq<Seq<T, I0+I1, IN...>>{}))> {
	using value_type = RemoveCVRef_t<decltype(Seq<T,I0>{}
			.append(PartialSumSeq<Seq<T, I0+I1, IN...>>{}))>;
};

/** @brief Defines value as the product of all of {I0, IN...}.
 * Not to be confused with inner product, etc. */
template<class S> struct ProductSeq;
template<class S, S I0>
struct ProductSeq<Seq<S, I0>>: std::integral_constant<S, I0> {};
template<class S, S I0, S I1, S... IN>
struct ProductSeq<Seq<S, I0, I1, IN...>>: ProductSeq<Seq<S, I0*I1, IN...>> {};
template<class T> struct PartialProductSeq;
template<class T, T I0>
struct PartialProductSeq<Seq<T, I0>>:
	Seq<T, I0> { typedef Seq<T, I0> value_type; };
template<class T>
struct PartialProductSeq: PartialProductSeq<InnerValue_t<T>>
	{ using PartialProductSeq<InnerValue_t<T>>::value_type; };
template<class T, T I0, T I1, T... IN>
struct PartialProductSeq<Seq<T, I0, I1, IN...>>:
	RemoveCVRef_t<decltype(Seq<T,I0>{}
			.append(PartialProductSeq<Seq<T, I0*I1, IN...>>{}))> {
	using value_type = RemoveCVRef_t<decltype(Seq<T,I0>{}
			.append(PartialProductSeq<Seq<T, I0*I1, IN...>>{}))>;
};


template<unsigned N, class S = unsigned, S A = 1, S B = 1, S... C>
struct Fibonacci: Fibonacci<N-1, S, A+B, A, B, C...> {};
template<class S, S A, S B, S... C>
struct Fibonacci<0, S, A, B, C...>: std::integral_constant<S, A> {};

template<class S, class T, std::size_t N = 0> struct Find;

template<class S, std::size_t N> struct Find<S, Tag<>, N> {};
template<class S, class... TN, std::size_t N>
struct Find<S, Tag<S, TN...>, N>: std::integral_constant<std::size_t, N> {};
template<class S, std::size_t N, class T0, class... TN>
struct Find<S, Tag<T0, TN...>, N>: std::conditional_t<std::is_same<S, T0>::value,
		std::integral_constant<std::size_t, N>, Find<S, Tag<TN...>, N+1>> {};

template<class S, S V, std::size_t N>
constexpr auto decompose(Seq<S, V> = {}, SeqSz<N> = {}) {
	/* static_assert((N > 0) && (V == (((S(1) << N) - 1) & V)),
		"V contains more than N binary diigts");*/
	//typedef Value_t<RepeatSeq<N-1, S, 1>> N0;        // N0  = {1, 1, ...}
	typedef RepeatSeq<N-1, S, 1> N0;        // N0  = {1, 1, ...}
	typedef PartialSumSeq<N0> P;            // P   = {1, 2, ...}
	typedef decltype(N0{}.prepend(Seq<S,1>{})) N1; // N1  = {1, 1, 1, ...}
	typedef decltype(P{}.prepend(Seq<S,0>{})) P1;  // P1  = {0, 1, 2, ...}
	typedef decltype(N1{} << P1{}) S1;      // S1  = {1, 2, 4, ...}
	typedef RepeatSeq<N, S, V> NV;          // NV  = {V, V, V, ...}
	return NV{} & S1{};                     //    -> {V&1, V&2, V&4, ...}
}

template<class S, S... I, S... J>
auto reverse(Seq<S, I...>, Seq<S, J...> = {}) -> Seq<S, J...> { return {}; }
template<class S, S I0, S... I, S... J>
auto reverse(Seq<S, I0, I...>, Seq<S, J...> = {})
	-> decltype(reverse(Seq<S, I...>{}, Seq<S, I0, J...>{})) { return {}; }

template<unsigned N, class T = unsigned>
using OnesSeq = RepeatSeq<N, T, 1>;

template<unsigned N, bool zero_indexed = true, class T = unsigned>
struct IncSeq: decltype(PartialSumSeq<OnesSeq<N, T>>{}
	- std::conditional_t<zero_indexed, OnesSeq<N, T>, RepeatSeq<N, T, 0>>{}) {
	typedef decltype(PartialSumSeq<OnesSeq<N, T>>{}
		- std::conditional_t<zero_indexed, OnesSeq<N, T>, RepeatSeq<N, T, 0>>{}) value_type;
};
template<unsigned N, bool zero_indexed = true, class T = unsigned>
struct DecSeq: decltype(reverse(IncSeq<N, zero_indexed, T>{})) {
	typedef decltype(reverse(IncSeq<N, zero_indexed, T>{})) value_type;
};

template<class... T, long N>
constexpr auto operator<<(Tag<T...> const& t, SeqL<N> const&)
-> decltype(t << std::integral_constant<long, N>{}) { return {}; }
template<class... T, long N>
constexpr auto operator>>(Tag<T...> const& t, SeqL<N> const&)
-> decltype(t >> std::integral_constant<long, N>{}) { return {}; }

}

#endif
