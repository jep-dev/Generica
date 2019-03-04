#ifndef TRANSFORM_HPP
#define TRANSFORM_HPP

#include "sequence.hpp"

namespace Detail {

/** @brief Transforms the Ith element of s with the given function and arguments */
template<unsigned... I, class... S, class F, class... T>
auto transform(SeqU<I...>, std::tuple<S...> const& s, F const& f, T &&... t)
	-> std::tuple<decltype(f(std::get<I>(s), std::forward<T>(t)...))...>
		{ return std::make_tuple(f(std::get<I>(s), std::forward<T>(t)...)...); }
/** @brief Transform with a default sequence, [0, sizeof...(S)-1] */
template<class... S, class F, class... T, class INC = IncSeq<sizeof...(S)>>
auto transform(std::tuple<S...> const& s, F const& f, T &&... t)
	-> decltype(transform(INC{}, s, f, std::forward<T>(t)...))
		{ return transform(INC{}, s, f, std::forward<T>(t)...); }

/** @brief Transforms the Ith element of s with the given function and arguments */
template<unsigned... I, class... S, class F, class... T>
auto transform(SeqU<I...>, std::tuple<S...> &s, F const& f, T &&... t)
	-> std::tuple<decltype(f(std::get<I>(s), std::forward<T>(t)...))...>
		{ return std::make_tuple(f(std::get<I>(s), std::forward<T>(t)...)...); }
/** @brief Transform with a default sequence, [0, sizeof...(S)-1] */
template<class... S, class F, class... T, class INC = IncSeq<sizeof...(S)>>
auto transform(std::tuple<S...> &s, F const& f, T &&... t)
	-> decltype(transform(INC{}, s, f, std::forward<T>(t)...))
		{ return transform(INC{}, s, f, std::forward<T>(t)...); }

template<class S, S... I, class F, class... T>
auto transform(Seq<S, I...>, F const& f, T &&... t)
//-> std::tuple<decltype(f(SeqU<I>{}, std::forward<T>(t)...)...)>
	-> decltype(std::make_tuple(f(Seq<S, I>{}, std::forward<T>(t)...)...))
	{ return std::make_tuple(f(Seq<S, I>{}, std::forward<T>(t)...)...); }

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
-> std::tuple<decltype(f(std::get<I>(s), std::get<I>(t), std::forward<T>(t)...))...> {
	return std::make_tuple(f(std::get<I>(s), std::get<I>(t), std::forward<T>(t)...)...);
}
//class F, class... S, class... T, class... U,
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
// or C macro firepower.
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
