#ifndef TRANSFORM_HPP
#define TRANSFORM_HPP

#include "sequence.hpp"
#include <tuple>

namespace Detail {

template<class S, std::size_t N = std::tuple_size<RemoveCVRef_t<S>>::value>
auto reverse_tuple(S && s)
-> decltype(reverse_tuple(std::forward<S>(s), IncSeq<N, true, std::size_t>{}))
	{ return reverse_tuple(std::forward<S>(s), IncSeq<N, true, std::size_t>{}); }
template<class S, class T, T... I, std::size_t N = std::tuple_size<RemoveCVRef_t<S>>::value>
auto reverse_tuple(S && s, Seq<T, I...> = {})
-> std::tuple<decltype(get<N-I-1>(std::forward<S>(s)))...>
	{ return {get<N-I-1>(std::forward<S>(s))...}; }
template<class... S>
auto make_reverse_tuple(S &&... s) {
	return reverse_tuple(make_tuple(std::forward<S>(s)...));
}

/** @brief Transforms the Ith element of s with the given function and arguments */
template<unsigned... I, class... S, class F, class... T>
auto transform(SeqU<I...>, std::tuple<S...> const& s, F const& f, T &&... t)
	-> decltype(std::make_tuple(f(std::get<I>(s), std::forward<T>(t)...)...))
		{ return std::make_tuple(f(std::get<I>(s), std::forward<T>(t)...)...); }
	// TODO side effects are in reverse order; execute in reverse order, then reverse results
	// The index below is wrong (order of I... should be reversed, not S...)
	/*-> decltype(make_reverse_tuple(f(std::get<sizeof...(S)-I-1>(s), std::forward<T>(t)...)...))
		{ return make_reverse_tuple(f(std::get<sizeof...(S)-I-1>(s), std::forward<T>(t)...)...); }*/
/** @brief Transform with a default sequence, [0, sizeof...(S)-1] */
template<class... S, class F, class... T, class INC = IncSeq<sizeof...(S)>>
auto transform(std::tuple<S...> const& s, F const& f, T &&... t)
	-> decltype(transform(INC{}, s, f, std::forward<T>(t)...))
		{ return transform(INC{}, s, f, std::forward<T>(t)...); }

/** @brief Transforms the Ith element of s with the given function and arguments */
template<unsigned... I, class... S, class F, class... T>
auto transform(SeqU<I...>, std::tuple<S...> &s, F const& f, T &&... t)
	-> decltype(std::make_tuple(f(std::get<I>(s), std::forward<T>(t)...)...))
		{ return std::make_tuple(f(std::get<I>(s), std::forward<T>(t)...)...); }
	// TODO see previous
	/*-> decltype(make_reverse_tuple(f(std::get<sizeof...(S)-I-1>(s), std::forward<T>(t)...)...))
		{ return make_reverse_tuple(f(std::get<sizeof...(S)-I-1>(s), std::forward<T>(t)...)...); }*/
/** @brief Transform with a default sequence, [0, sizeof...(S)-1] */
template<class... S, class F, class... T, class INC = IncSeq<sizeof...(S)>>
auto transform(std::tuple<S...> &s, F const& f, T &&... t)
	-> decltype(transform(INC{}, s, f, std::forward<T>(t)...))
		{ return transform(INC{}, s, f, std::forward<T>(t)...); }

template<class S, S... I, class F, class... T>
auto transform(Seq<S, I...> i, F const& f, T &&... t)
-> decltype(std::make_tuple(f(Seq<S, I>{}, std::forward<T>(t)...)...))
{ return std::make_tuple(f(Seq<S, I>{}, std::forward<T>(t)...)...); }
// TODO see previous
/*-> decltype(transform(seq_to_tuple_seq(i), f, std::forward<T>(t)...))
	{ return transform(seq_to_tuple_seq(i), f, std::forward<T>(t)...) }*/

/** @brief Transform a sequence with explicit indices */
template<class R, R... I, class S, S... J, class F, class... T>
auto transform(Seq<R, I...> i, Seq<S, J...> j, F && f, T &&... t)
	{ return transform(i, seq_to_tuple_seq(j), std::forward<F>(f), std::forward<T>(t)...); }
/** @brief Transform a sequence with implicit indices
 * Transform with the same arguments would interpret the first sequence as indices */
template<class S, S... J, class F, class... T>
auto transform_seq(Seq<S, J...> j, F && f, T &&... t)
	{ return transform(seq_to_tuple_seq(j), std::forward<F>(f), std::forward<T>(t)...); }
/** @brief Transform a sequence with explicit indices */
template<class R, R... I, class S, S... J, class F, class... T>
auto transform_seq(Seq<R, I...> i, Seq<S, J...> j, F && f, T &&... t)
	{ return transform(i, j, std::forward<F>(f), std::forward<T>(t)...); }


/*template<unsigned... I, class... S, class F, class... T>
auto transform(SeqU<I...>, Tag<S...> s, F const& f, T &&... t)
-> std::tuple<decltype(f(std::declval<typename
		decltype(get<I>(s))::type...>(), std::forward<T>(t)...))>
	{ return std::make_tuple(f(typename
		decltype(std::get<I>(s))::type {}, std::forward<T>(t)...)...); }
template<class... S, class F, class... T, class INC = IncSeq<sizeof...(S)>>
auto transform(Tag<S...> s, F const& f, T &&... t)
-> decltype(transform(INC{}, s, f, std::forward<T>(t)...))
	{ return transform(INC{}, s, f, std::forward<T>(t)...); }*/

template<unsigned... I, class... S, class... T, class F, class... U>
auto transform_pairwise(SeqU<I...>, std::tuple<S...> const& s, std::tuple<T...> const& t,
		F const& f, U &&... u)
-> std::tuple<decltype(f(std::get<I>(s), std::get<I>(t), std::forward<T>(t)...))...>
	{ return std::make_tuple(f(std::get<I>(s), std::get<I>(t), std::forward<T>(t)...)...); }
// TODO see previous
/*-> std::tuple<decltype(f(std::get<I>(s), std::get<I>(t), std::forward<T>(t)...))...>
	{ return std::make_tuple(f(std::get<I>(s), std::get<I>(t), std::forward<T>(t)...)...); }*/
template<class... S, class... T, class F, class... U,
	class INC = std::enable_if_t<sizeof...(S) == sizeof...(T), IncSeq<sizeof...(S)>>>
auto transform_pairwise(std::tuple<S...> const& s, std::tuple<T...> const& t,
		F const& f, U &&... u)
-> decltype(transform_pairwise(INC{}, s, t, f, std::forward<T>(t)...)) {
	return transform_pairwise(INC{}, s, t, f, std::forward<T>(t)...);
}

/*
//template<class F, class... S, class... T, unsigned... I>
template<unsigned... I, class... S, class F, class... T>
auto transform2(SeqU<I...>, std::tuple<S...> const& s, F const& f, T &&... t)
	-> std::tuple<> { return {}; }
template<unsigned I, unsigned... J, class... S, class F, class... T>
auto transform2(SeqU<I, J...>, std::tuple<S...> const& s, F const& f, T &&... t) {
	return std::tuple_cat(transform(std::get<I>(s), f, t...),
		transform2(SeqU<J...>{}, s, f, std::forward<T>(t)...));
}
*/
template<class S, S... U, class T, T... V, class F, class... W>
auto transform2(Seq<S, U...> u, Seq<T, V...> t, F const& f, W &&... w) {
	return std::make_tuple(transform(u, f, Seq<S, V>{}, w...)...);
}
template<class... S>
constexpr auto tuple2_to_tag2(std::tuple<S...> const&)
-> Tag<decltype(tuple_to_tag(declval<S>()))...> { return {}; }

template<class... S>
constexpr auto tag2_to_sizes(Tag<S...> const&)
-> SeqSz<S::size...> { return {}; }
template<class S>
constexpr auto tuple2_to_sizes(S const& s)
-> decltype(tag2_to_sizes(tuple2_to_tag2(s))) { return {}; }
template<class... S>
constexpr auto tag2_to_divisors(Tag<S...> const& s) {
	typedef decltype(reverse(tag2_to_sizes(s))) Bwd;
	typedef decltype(Bwd::prepend(SeqSz<1>{})) Bwd1;
	typedef typename PartialProductSeq<Bwd1>::value_type ProBwd1;
	return pop_head(reverse(ProBwd1{}));
}
template<class... S>
constexpr auto tuple2_to_divisors(std::tuple<S...> const& s)
-> decltype(tuple2_to_tag2(s)) { return {}; }

/*template<std::size_t I = 0, class... S, std::size_t... J>
auto index_seq(Tag<> const&, SeqSz<J...> j = {}) -> SeqSz<J...> { return j; }

template<std::size_t I = 0, class... S, class... V, std::size_t... J>
auto index_seq(Tag<Tag<S...>, V...> const&, SeqSz<J...> = {})
-> decltype(index_seq<I / sizeof...(S)>(Tag<V...>{},
		SeqSz<J..., I % sizeof...(S)>{})) { return {}; }

template<std::size_t I = 0, class... S>
auto index_seq(std::tuple<S...> const& s)
-> decltype(index_seq(tuple2_to_tag2<true>(s))) { return {}; }*/

/*template<std::size_t I = 0, class... S, class... T, class F, class... U,
		class = std::enable_if_t<(I < sizeof...(S)*sizeof...(T)), void>>
auto nth_cartesian(std::tuple<S...> const& s, std::tuple<T...> const& t,
		F const& f, U &&... u) {
	return f(std::get<I / sizeof...(T)>(s), std::get<I % sizeof...(T)>(t),
			std::forward<U>(u)...);
}
template<std::size_t I = 0, class... S, class... T, class... U, class F, class... V,
		class = std::enable_if_t<(I < sizeof...(S)*sizeof...(T)*sizeof...(U)), void>>
auto nth_cartesian(std::tuple<S...> const& s, std::tuple<T...> const& t,
		std::tuple<U...> const& u, F const& f, U &&... u) {
	return f(std::get<I / (sizeof...(T)*sizeof...(U)>(s), std::get<I % sizeof...(T)>(t),
			std::forward<U>(u)...);
}*/

// TODO working on constexpr variants, considering making qualifiers optional
// (all combinations of qualifiers would warrant either selective specialization
// or C macro firepower.)
/*template<std::size_t I = 0, class S, S... V, class F, class... T>
auto for_each(Seq<S, V...> const& v, F const& f, T &&... t)
	-> std::enable_if_t<(I == sizeof...(V), void> {}
template<std::size_t I = 0, class S, S... V, class F, class... T>
auto for_each(Seq<S, V...> const& v, F const& f, T &&... t)
	-> std::enable_if_t<(I < sizeof...(V)), void> {
	f(get<I>(v), t...);
	for_each<I+1>(v, f, std::forward<T>(t)...);
}*/

/** @brief Creates a type sequence of type pairs found in parallel U, V */
template<class... U, class... V>
constexpr auto zip(Tag<U...> const&, Tag<V...> const&) -> Tag<Tag<U,V>...> { return {}; }
/** @brief Creates two type sequences from one by splitting each element */
template<class... U, class... V>
constexpr auto unzip(Tag<Tag<U, V>...> const&) -> Tag<Tag<U...>, Tag<V...>> { return {}; }

// Lvalue TTOA
template<class K, K... V, class S, class... T, class U = S>
auto tuple_to_array(Seq<K, V...> & indices, std::tuple<S, T...> & tup, Tag<U> = {})
-> std::array<U, sizeof...(V)> { return {(U) std::get<V>(tup)...}; }
// Const lvalue TTOA
template<class K, K... V, class S, class... T, class U = S>
auto tuple_to_array(Seq<K, V...> const& indices, std::tuple<S, T...> const& tup, Tag<U> = {})
-> std::array<U, sizeof...(V)> { return {(U) std::get<V>(tup)...}; }

// Lvalue TTOA, no indices (default)
template<class S, class... T, class U = S>
auto tuple_to_array(std::tuple<S, T...> & t, Tag<U> u = {})
-> decltype(tuple_to_array(IncSeq<sizeof...(T)+1, true, std::size_t>{}, t, u))
	{ return tuple_to_array(IncSeq<sizeof...(T)+1, true, std::size_t>{}, t, u); }
// Const lvalue TTOA, no indices (default)
template<class S, class... T, class U = S>
auto tuple_to_array(std::tuple<S, T...> const& t, Tag<U> u = {})
-> decltype(tuple_to_array(IncSeq<sizeof...(T)+1, true, std::size_t>{}, t, u))
	{ return tuple_to_array(IncSeq<sizeof...(T)+1, true, std::size_t>{}, t, u); }

}

#endif
