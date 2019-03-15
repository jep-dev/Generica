#ifndef SEQUENCE_HPP
#define SEQUENCE_HPP

#include "tag.hpp"
#include <iosfwd>
//#include <string>

namespace Detail {

/** A variadic unsigned sequence, useful for indexing Tag/tuple/etc.. */
template<class T, T... I> struct Seq;

/** Simple nontype storage used exclusively as a scalar (not derived/convertible) */
template<class T, T V> struct Scalar {
	typedef T value_type;
	static constexpr T value = V;
};

template<int I> struct Scalar_i: Scalar<int, I> {};
template<long I> struct Scalar_l: Scalar<long, I> {};
template<short I> struct Scalar_s: Scalar<short, I> {};
template<unsigned I> struct Scalar_u: Scalar<unsigned, I> {};
template<unsigned long I> struct Scalar_ul: Scalar<unsigned long, I> {};
template<unsigned short I> struct Scalar_us: Scalar<unsigned short, I> {};

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

	template<class S, S N, S M = ((N % size + size) % size)>
	static constexpr T get(Seq<S, N>, std::enable_if_t<M == 0, std::true_type> = {}) { return I0; }
	template<class S, S N, S M = ((N % size + size) % size)>
	static constexpr T get(Seq<S, N>, std::enable_if_t<M != 0, std::false_type> = {})
		{ return Seq<T, IN...>::get(Seq<S, M-1>{}); }

	/*template<class S>
	static constexpr T get(Seq<S, 0>) { return I0; }*/
	template<class S, S N>
	static constexpr T get(std::integral_constant<S, N>)
		{ return Seq<T, IN...>::get(Seq<S, N>{}); }
	template<std::size_t N>
	static constexpr T get(void) { return get(SeqSz<N>{}); }

	template<std::size_t N>
	static constexpr auto nth_type(Seq<std::size_t, N>)
		-> Seq<T, get(Seq<std::size_t, N>{})> { return {}; }
	template<class S, S... J>
	auto constexpr operator[](Seq<S, J...> const&)
		-> Seq<T, get<J>()...> { return {}; }

	template<class S, S... J, class ST = std::common_type_t<S, T>>
	static constexpr auto prepend(Seq<S, J...> = {})
			-> Seq<ST, (ST)J..., (ST)I0, (ST)IN...> { return {}; }
	template<class S, S... J, class ST = std::common_type_t<S, T>>
	static constexpr auto append(Seq<S, J...> = {})
			-> Seq<ST, (ST)I0, (ST)IN..., (ST)J...> { return {}; }

	template<T J>
	static constexpr bool contains(void)
		{ return Detail::contains(Seq<T, I0, IN...>{}, Seq<T, J>{}); }

	template<T J>
	constexpr std::size_t find(Seq<T, J> j) const { return find(j, Seq<std::size_t, 0>{}); }
	template<T J, std::size_t N>
	constexpr std::size_t find(Seq<T, J> j, Seq<std::size_t, N> n) const {
		return Seq<T, IN...>{}.find(j, Seq<std::size_t, N+1>{});
	}
	template<std::size_t N>
	constexpr std::size_t find(Seq<T, I0>, Seq<std::size_t, N>) const { return N; }

	/*template<std::size_t N>
	using nth_type = Seq<T, get<N>()>;*/
};

template<class... S, S... V>
constexpr auto to_seq(Seq<S,V>...) -> Seq<std::common_type_t<S...>,
	static_cast<std::common_type_t<S...>>(V)...> { return {}; }
template<class... S, S... V>
constexpr auto to_seq(std::integral_constant<S,V>...)
	-> Seq<std::common_type_t<S...>,
		static_cast<std::common_type_t<S...>>(V)...> { return {}; }
template<template<class...> class C, class... S, S... V>
constexpr auto to_seq(C<Seq<S,V>...> const&)
	-> decltype(to_seq(Seq<S,V>{}...)) { return {}; }
template<template<class...> class C, class... S, S... V>
constexpr auto to_seq(C<std::integral_constant<S,V>...> const&)
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

template<class T, T... U, T... V>
constexpr auto zip(Seq<T, U...> const&, Seq<T, V...> const&)
	-> Tag<Seq<T, U, V>...> { return {}; }
template<class T, T... U, T... V>
constexpr auto unzip(Tag<Seq<T, U, V>...> const&)
	-> Tag<Seq<T, U...>, Seq<T, V...>> { return {}; }

/** Maps a sequence of values to a sequence of individual value types */
template<class S, S... I>
constexpr auto seq_to_tag_seq(Seq<S, I...> const&) -> Tag<Seq<S,I>...> { return {}; }
template<class S, S... I>
constexpr auto tag_seq_to_seq(Tag<Seq<S, I>...> const&) -> Seq<S, I...> { return {}; }

template<class... S, class C = std::common_type_t<decltype(S::value)...>>
constexpr auto tag_value_to_seq(Tag<S...> const&) -> Seq<C, S::value...> { return {}; }

template<class... S, S... V>
constexpr auto tuple_seq_to_seq(std::tuple<Seq<S,V>...> const& v)
-> decltype(tag_seq_to_seq(tuple_to_tag(v))) { return {}; }

/** @brief Converts the sequence to tuple sequence segments */
template<class S, S... I>
auto seq_to_tuple_seq(Seq<S, I...> const&)
	-> std::tuple<Seq<S,I>...> { return std::make_tuple(Seq<S,I>{}...); }

/** @brief Transforms sequence elements to tuple elements */
template<class S, S... I, class F, class... T>
auto seq_to_tuple(Seq<S, I...> const&, F const& f, T &&... t)
	-> std::tuple<decltype(f(I, std::forward<T>(t)...))...>
		{ return std::make_tuple(f(I, std::forward<T>(t)...)...); }

/** @brief Transforms sequence elements to array elements */
template<class S, S I0, S... I, class F, class... T>
auto seq_to_array(Seq<S, I0, I...> const&, F const& f, T &&... t)
	-> std::array<decltype(f(I0, std::forward<T>(t)...)), 1 + sizeof...(I)>
		{ return {f(I0, std::forward<T>(t)...), f(I, std::forward<T>(t)...)...}; }

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


/*template<class S, class... T>
constexpr bool contains(S const& s, T &&... t) {
	return s.contains(std::forward<T>(t)...);
}*/

template<std::size_t N, class T, T... I>
constexpr auto get(Seq<T, I...> i) {
	return i.get(SeqSz<N>{});
}

/*template<class S, S... M, class T, T N>
struct Rotate<Seq<S, M...>, T, N> {
	typedef decltype(tag_seq_to_seq(Tag<Seq<S, M>...>{} << Seq<T, N>{})) type;
};*/
template<class S, S... M, class T, T N>
constexpr auto rotate(Seq<S, M...> m, Seq<T, N> n = {})
-> decltype(to_seq(Rotate_t<decltype(seq_to_tag_seq(m)), T, N>{}))
	{ return to_seq(Rotate_t<decltype(seq_to_tag_seq(m)), T, N>{}); }

template<class T> struct SeqArray;
template<class T, T... I> struct SeqArray<Seq<T, I...>> {
	static constexpr T value[sizeof...(I)] = {I...};
};
template<class T, T... I>
constexpr T SeqArray<Seq<T, I...>>::value [sizeof...(I)];

template<class T, T... IN>
constexpr std::size_t getSize(Detail::Seq<T, IN...> = {}) { return sizeof...(IN); }

template<class T, T... V>
constexpr auto operator-(Seq<T, V...> const&) -> Seq<T, -V...> { return {}; }


/* To make sequences more expressive, operators between sequences are pairwise operators on
 * their elements, and operators between scalars and sequences are distributed.
 * PAIRWISE is a macro function of a binary operator symbol used to generate the corresponding
 * operator overloads. Its lifespan and all of its uses are inside this if block. */
#ifndef PAIRWISE
#define PAIRWISE(OP) \
template<class TI, TI... I, class TJ, TJ... J, \
	class T = decltype(declval<TI>() OP declval<TJ>())> \
constexpr auto operator OP(Seq<TI, I...>, Seq<TJ, J...>) \
	-> Seq<T, (I OP J)...> { return {}; } \
template<class S, S... V, class T, T I> \
constexpr auto operator OP(Seq<S, V...> const&, Scalar<T, I> const&) \
	-> Seq<decltype(declval<S>() OP I), (V OP I)...> { return {}; } \
template<class S, S I, class T, T... V> \
constexpr auto operator OP(Scalar<S, I> const&, Seq<T, V...> const&) \
	-> Seq<decltype(I OP declval<S>()), (I OP V)...> { return {}; }

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
constexpr auto bitOrSeq(Seq<S, I, J...> = {}) -> S { return BOS::value; }

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

template<class S, S... V, class F>
auto remove_if(Seq<S, V...> v, F && f)
	-> decltype(remove_if(v, std::forward<F>(f), Seq<S>{})) { return {}; }
template<class S, class F, S... W>
auto remove_if(Seq<S> v, F && f, Seq<S, W...> w)
	-> Seq<S, W...> { return {}; }
template<class S, S V0, S... V, class F, S... W>
auto remove_if(Seq<S, V0, V...> v, F && f, Seq<S, W...> w)
	-> decltype(remove_if(Seq<S, V...>{}, std::forward<F>(f),
		std::conditional_t<decltype(std::declval<F>()(Seq<S, V0>{}))::value,
		Seq<S, W...>, Seq<S, W..., V0>>{})) { return {}; }

template<class S, S... V, S W>
constexpr auto remove_all(Seq<S, V...> v, Seq<S, W> w)
-> decltype(remove_all(v, w, Seq<S>{})) { return {}; }
template<class S, S U0, S... U, S V, S... W>
constexpr auto remove_all(Seq<S, U0, U...> u, Seq<S, V> v, Seq<S, W...> w)
-> decltype(remove_all(Seq<S, U...>{}, v,
	std::conditional_t<U0 == V, Seq<S, W...>, Seq<S, W..., U0>>{}))
		{ return {}; }
template<class S, S V, S... W>
constexpr Seq<S, W...> remove_all(Seq<S>, Seq<S, V>, Seq<S, W...>)
	{ return {}; }

template<class S, S... V>
auto nonzero(Seq<S, V...> v) /*{
	return remove_if(v, [](auto && x) {
		return std::integral_constant<bool, !x.value>{};
			//!RemoveCVRef_t<decltype(x)>::value> {};
	});
}*/
-> decltype(remove_all(v, Seq<S, 0>{})) { return {}; }

/*template<class S, S... V, S W>
auto remove_all(Seq<S, V...> v, Seq<S, W> w) {
	return remove_if(v, [](auto && x) {
		return SeqB<x.value != W>{};
		//return BoolOrSeq<(V != x.value)...> {};
}*/

template<class S, S... W>
constexpr Seq<S, W...> merge_sort(Seq<S>, Seq<S>, Seq<S, W...> = {}) { return {}; }
template<class S, S U0, S... U, S... W>
constexpr Seq<S, W..., U0, U...> merge_sort(Seq<S, U0, U...>,
		Seq<S>, Seq<S, W...> = {}) { return {}; }
template<class S, S V0, S... V, S... W>
constexpr Seq<S, W..., V0, V...> merge_sort(Seq<S>,
		Seq<S, V0, V...>, Seq<S, W...> = {}) { return {}; }
template<class S, S U0, S... U, S V0, S... V, S... W>
constexpr auto merge_sort(Seq<S, U0, U...> u,
		Seq<S, V0, V...> v, Seq<S, W...> w) -> std::conditional_t<(U0 < V0),
	decltype(merge_sort(Seq<S, U...>{}, v, Seq<S, W..., U0>{})),
	decltype(merge_sort(u, Seq<S, V...>{}, Seq<S, W..., V0>{}))> { return {}; }

template<class S, S... V, S... W>
constexpr Seq<S, W..., V...> insert(Seq<S>, Seq<S, V...>, Seq<S, W...> = {}) { return {}; }
template<class S, S U0, S... U, S V, S... W>
constexpr auto insert(Seq<S, U0, U...> u, Seq<S, V> v, Seq<S, W...> w = {})
-> std::conditional_t<(U0 < V),
	decltype(insert(Seq<S, U...>{}, v, Seq<S, W..., U0>{})),
	decltype(insert(Seq<S, U...>{}, Seq<S, U0>{}, Seq<S, W..., V>{}))> { return {}; }
/*template<class S, S... U, S V>
constexpr auto insert(Seq<S, U...> u, Seq<S, V> v)
-> decltype(insert(u, v, Seq<S>{})) { return {}; }*/

template<class S, S... U, S... V>
constexpr Seq<S, V...> sort(Seq<S, U...> u, Seq<S, V...> = {}) { return {}; }
template<class S, S U0, S... U, S... V>
constexpr auto sort(Seq<S, U0, U...> u, Seq<S, V...> v = {})
-> decltype(sort(Seq<S, U...>{}, insert(v, Seq<S, U0>{}))) { return {}; }

template<class S, S... U, S V>
constexpr auto insert_unique(Seq<S, U...> u, Seq<S, V> v)
-> std::conditional_t<boolOrSeq((U == V)...), Seq<S, U...>,
		decltype(insert(u, v))> { return {}; }

template<class T, T... V, class S = unsigned>
constexpr auto seq_cast(Seq<T, V...> = {}, Tag<S> = {})
// V, S(V), or static_cast<S>(V)? TODO declare or parametrize
-> Seq<S, static_cast<S>(V)...> { return {}; }

template<class S = std::string, class T, T... U>
S to_string(Seq<T, U...> const&) {
	S cur = "{", pfx = "";
	for(auto str : {std::to_string(U)...})
		cur += pfx + str, pfx = ", ";
	return cur + "}";
}

}

#endif
