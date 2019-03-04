#ifndef TAG_HPP
#define TAG_HPP

#include "utility.hpp"

namespace Detail {

template<class...> struct Tag {
	typedef Tag<> head_type;
	typedef Tag<> tail_type;
	static constexpr unsigned size = 0;
	template<class... R>
	constexpr bool contains(void) const { return false; }
	template<class... R>
	constexpr bool contains_base(void) const { return false; }
	template<class... R>
	constexpr bool contains_derived(void) const { return false; }
	template<class R, class... S>
	constexpr bool contains(R, S...) const { return false; }
	template<class R, class... S>
	constexpr bool contains_base(R, S...) const { return false; }
	template<class R, class... S>
	constexpr bool contains_derived(R, S...) const { return false; }
};

template<class HEAD, class... REST>
struct Tag<HEAD, REST...> {
	typedef HEAD type;
	typedef Tag<HEAD> head_type;
	typedef Tag<REST...> tail_type;
	static constexpr unsigned size = 1 + sizeof...(REST);

	template<class R> constexpr bool contains(Tag<R> = {}) const {
		return std::is_same<HEAD, R>::value
				|| Tag<REST...>{}.contains(Tag<R>{});
	}
	template<class R> constexpr bool contains_base(Tag<R> = {}) const {
		return std::is_base_of<HEAD, R>::value
				|| Tag<REST...>{}.contains_base(Tag<R>{});
	}
	template<class R> constexpr bool contains_derived(Tag<R> = {}) const {
		return std::is_base_of<R, HEAD>::value
				|| Tag<REST...>{}.contains_derived(Tag<R>{});
	}
	template<long I>
	constexpr auto get(Seq<long, I> i = {}) const
	-> typename decltype(*this << i)::head_type { return {}; }
};

template<long I, class S, class... T>
constexpr auto get(Tag<S, T...> const& t)
-> typename decltype(t << Seq<long, I>{})::head_type { return {}; }

template<long L, class S, class... T>
constexpr auto get(Tag<S, T...> const& t) -> decltype(Tag<S, T...>::template get<L>()) { return {}; }

template<class S, class... T>
constexpr Tag<T...> pop_front(Tag<S, T...> const&) { return {}; }

/** @brief Even base case */
template<std::size_t I, class... U, class... V,
	class = std::enable_if_t<(I == sizeof...(V)), void>>
constexpr auto head(Tag<U...> const&, Tag<V...> const&)
	-> Tag<V...> { return {}; }
/** @brief Odd base case */
template<std::size_t I, class U, class... V, class... W,
	class = std::enable_if_t<(I == sizeof...(W)+1), void>>
constexpr auto head(Tag<U, V...> const&, Tag<W...> const&)
	-> Tag<W..., U> { return {}; }
/** @brief Inductive case */
template<std::size_t I, class U, class V, class... W, class... X,
	class = std::enable_if_t<(I >= sizeof...(X)+2), void>>
constexpr auto head(Tag<U, V, W...> const&, Tag<X...> const&)
	-> decltype(head<I>(Tag<W...>{}, Tag<X..., U, V>{})) { return {}; }
/** @brief Inductive case, 2x (decreases recursion depth) */
template<std::size_t I, class S, class U, class V,
	class W, class X, class... Y, class... Z,
	class = std::enable_if_t<(I >= sizeof...(Z)+4), void>>
constexpr auto head(Tag<U, V, W, X, Y...> const& u, Tag<Z...> const&)
	-> decltype(head<I>(Tag<Y...>{}, Tag<Z..., U, V, W, X>{})) { return {}; }
template<std::size_t I, class S, class... U>
constexpr auto head(Tag<U...> const& u)
	-> decltype(head<I>(u, Tag<>{})) { return {}; }


/** @brief Even base case, the tail is the remainder */
template<std::size_t I, class... U,
	class = std::enable_if_t<(I == sizeof...(U)), void>>
constexpr auto tail(Tag<U...> const&) -> Tag<U...> { return {}; }
/** @brief Odd base case, the tail is the remainder */
template<std::size_t I, class U, class... V,
	class = std::enable_if_t<(I == sizeof...(V))>>
constexpr auto tail(Tag<U, V...> const&) -> Tag<V...> { return {}; }
/** @brief Inductive case */
template<std::size_t I, class U, class V, class... W,
	class = std::enable_if_t<(I <= sizeof...(W)), void>>
constexpr auto tail(Tag<U, V, W...> const& u)
	-> decltype(tail<I>(Tag<W...>{})) { return {}; }
/** @brief Inductive case (two steps in one call) */
template<std::size_t I, class U, class V, class W, class X, class... Y,
	class = std::enable_if_t<(I <= sizeof...(Y)), void>>
constexpr auto tail(Tag<U, V, W, X, Y...> const& u)
	-> decltype(tail<I>(Tag<Y...>{})) { return {}; }

template<class... S, class = std::enable_if_t<sizeof...(S), void>>
constexpr auto pop_back(Tag<S...> const& s) -> PopBack_t<Tag<S...>> { return {}; }
//-> decltype(tail<sizeof...(S)-1>(s)) { return {}; }

template<template<class...> class OP, class... L, class... R>
constexpr Tag<> cartesian(Tag<L...>, Tag<R...> = {}) { return {}; }
template<template<class...> class OP, class L0, class... L, class... R>
constexpr auto cartesian(Tag<L0, L...> l, Tag<R...> r = {})
-> decltype(Tag<OP<L0, R>...>{} + cartesian<OP>(Tag<L...>{}, r)) { return {}; }
template<class... L, class... R>
constexpr auto cartesian(Tag<L...> l, Tag<R...> r)
-> decltype(cartesian<Tag>(l, r)) { return {}; }


/*template<class L, L... U, class R, R... V, class FT, FT FV>
constexpr auto cartesian(Seq<L, U...>, Seq<R, V...> = {}, Seq<FT, FV> = {})
	-> Tag<> { return {}; }
template<class L, L U0, L... U, class R, R... V, class FT, FT FV>
constexpr auto cartesian(Seq<L, U0, U...>, Seq<R, V...> = {}, Seq<FT, FV> = {})
	-> decltype(Seq<decltype(FV(U0, std::declval<R>())), FV(U0, V)...>{}
		.append(cartesian(Seq<L, U...>{}, Seq<R, V...>{}))) { return {}; }*/

/*template<class L, L U0, L... U, class R, R... V>
constexpr auto cartesian(Seq<L, U0, U...>, Seq<R, V...> = {})
-> decltype(cartesian(Tag<Seq<L, U>...>{}, Tag<Seq<R, V>...>{})) { return {}; }
template<class... U, class... V, class FT, FT FV>
constexpr auto cartesian_apply(Tag<Tag<U, V>...>, Seq<FT, FV> = {})
-> Tag<Seq<decltype(FV(U,V)), FV(U,V)>...> { return {}; }
template<class L0, class... L, class... R, template<class...> class C>
constexpr auto cartesian_type(Tag<L0, L...>, Tag<R...> = {})
-> decltype(Tag<C<L0, R>...>{} // or ::type, or ::value_type...
	+ cartesian_type<C>(Tag<L...>{}, Tag<R...>{})) { return {}; }*/

/** Direct sum (treats LHS and RHS as orthogonal) */
template<class... LHS, class... RHS>
constexpr Tag<LHS..., RHS...> operator+(Tag<LHS...>, Tag<RHS...>) { return {}; }

/** Inner product */
template<class... LN, class... RN>
constexpr Tag<Tag<LN,RN>...> operator*(Tag<LN...>, Tag<RN...>) { return {}; }

/** Empty exterior; may be necessary due to ambiguity between ext(U,Tag<>) and ext(Tag<>,X) */
constexpr auto operator^(Tag<>, Tag<>) -> Tag<> { return {}; }

/** Exterior with empty LHS */
template<class X>
constexpr auto operator^(Tag<>, X &&) -> Tag<> { return {}; }

/** Exterior with empty RHS */
template<class U>
constexpr auto operator^(U &&, Tag<>) -> Tag<> { return {}; }

/** Exterior of simple types (sizeof...(U) = sizeof...(X) = 1) */
template<class U, class X>
constexpr auto operator^(Tag<U>, Tag<X>) -> Tag<Tag<U,X>> { return {}; }

/** Exterior of variadic LHS and simple RHS (sizeof...(U) > 1 = sizeof...(X)) */
template<class U, class... V, class X>
constexpr auto operator^(Tag<U, V...>, Tag<X>) -> Tag<Tag<U, X>, Tag<V, X>...> { return {}; }

/** Exterior of simple LHS and variadic RHS (sizeof...(U) = 1 < sizeof...(X)) */
template<class U, class X, class... Y>
constexpr auto operator^(Tag<U>, Tag<X, Y...>) -> Tag<Tag<U, X>, Tag<U, Y>...> { return {}; }

/** Exterior of variadic LHS and RHS (sizeof...(U) > 1, sizeof...(X) > 1) */
template<class U, class... V, class X, class... Y>
constexpr auto operator^(Tag<U, V...>, Tag<X, Y...>) -> decltype(Tag<Tag<U, X>, Tag<U, Y>...>{}
	+ (Tag<V...>{} ^ Tag<X,Y...>{})) { return {}; }

constexpr auto operator%(Tag<>, Tag<>) -> Tag<> { return {}; }
template<class X>
constexpr auto operator%(Tag<>, X &&) -> Tag<> { return {}; }
template<class U>
constexpr auto operator%(U &&, Tag<>) -> Tag<> { return {}; }
template<class U, class X>
constexpr auto operator%(Tag<U>, Tag<X>) -> Tag<Tag<U,X>> { return {}; }
template<class U, class... V, class X>
constexpr auto operator%(Tag<U, V...>, Tag<X>) -> Tag<Tag<U, X>, Tag<V, X>...> { return {}; }
template<class U, class X, class... Y>
constexpr auto operator%(Tag<U>, Tag<X, Y...>) -> Tag<Tag<U, X>, Tag<U, Y>...> { return {}; }
template<class U, class... V, class X, class... Y>
constexpr auto operator%(Tag<U, V...>, Tag<X, Y...>) -> decltype(Tag<Tag<U, X>, Tag<V, X>...>{}
		+ (Tag<U, V...>{} % Tag<Y...>{})) { return {}; }

/** Eval's default case; identity with perfect forwarding */
template<class U>
auto eval(U && u) -> U { return std::forward<U>(u); }
/** Maps a value-free Tag to a tag-free std::tuple (tuple) */
template<class... U>
auto eval(Tag<U...>) -> std::tuple<decltype(eval(std::declval<U>()))...> { return {eval(U{})...}; }

/*template<class U>
auto deval(U && u) -> Tag<U> { return Tag<U>{}; }
//auto deval(U && u) -> U { return std::forward<U>(u); }
template<class... U>
auto deval(std::tuple<U...> const&)
	-> Tag<decltype(deval(std::declval<U>()))...> { return {}; }
template<class... U, class... V>
auto deval(std::tuple<std::tuple<U...>, V...> const&)
	-> decltype(deval(std::declval<std::tuple<U...>>()) + deval(std::declval<std::tuple<V...>>()))
	{ return {}; }
	//{ return {deval(U{})...}; }
*/

/** Access's default case; identity with perfect forwarding */
template<class T>
auto access(T && t) -> decltype(t) { return std::forward<T>(t); }

/** (Const reference) access's base case; delegates to std::get */
template<unsigned I, class... T>
auto access(std::tuple<T...> const& t) -> decltype(std::get<I>(t)) { return std::get<I>(t); }

/** (Const reference) access's inductive case; recurses to compose the tail with std::get */
template<unsigned I, unsigned J, unsigned... K, class... T>
auto access(std::tuple<T...> const& t) -> decltype(access<J, K...>(std::get<I>(t))) {
	return access<J, K...>(access<I>(t));
}

/** (Reference) access's base case; delegates to std::get */
template<unsigned I, class... T>
auto access(std::tuple<T...> & t) -> decltype(std::get<I>(t)) { return std::get<I>(t); }

/** (Reference) access's inductive case; recurses to compose the tail with std::get */
template<unsigned I, unsigned J, unsigned... K, class... T>
auto access(std::tuple<T...> & t) -> decltype(access<J, K...>(std::get<I>(t))) {
	return access<J, K...>(access<I>(t));
}

/** (Rvalue) access's base case; delegates to std::get */
template<unsigned I, class... T>
auto access(std::tuple<T...> && t) -> decltype(std::get<I>(std::move(t))) {
	return std::get<I>(std::move(t));
}

/** (Rvalue) access's inductive case; recurses to compose the tail with std::get */
template<unsigned I, unsigned J, unsigned... K, class... T>
auto access(std::tuple<T...> && t) -> decltype(access<J, K...>(access<I>(std::move(t)))) {
	return access<J, K...>(std::get<I>(std::move(t)));
}


auto collapse(void) -> Tag<> { return {}; }

auto collapse(Tag<>) -> Tag<> { return {}; }
template<class... U>
auto collapse(Tag<U...>)
-> Tag<std::common_type_t<U...>> { return {}; }
template<class U, class... V>
auto collapse(Tag<U, V...>) -> Tag<decltype(collapse(std::declval<U>())),
		decltype(collapse(std::declval<V>()))...> { return {}; }


/** Reverse's inductive case; recurses after moving the head of the LHS to the RHS */
template<class U, class... V, class X = Tag<>>
constexpr auto reverse(Tag<U, V...>, X x = {})
-> decltype(reverse(Tag<V...>{}, Tag<U>{} + x)) { return {}; }

/** Reverse's base case; simply returns the RHS */
template<class... X>
constexpr auto reverse(Tag<>, Tag<X...> x = {}) -> decltype(x) { return {}; }

/*
// Moves the head to the tail N times; defines type as the result
template<class T, long N = 0, class V = void>
struct Rotate;

template<class... T, long N>
struct Rotate<Tag<T...>, N, std::enable_if_t<!(N % sizeof...(T)), void>> {
	typedef Tag<T...> type;
};

template<class S, class... T, long N>
struct Rotate<Tag<S, T...>, N, std::enable_if_t<(N % (sizeof...(T)+1)), void>> {
	typedef typename Rotate<Tag<T..., S>, (N % (sizeof...(T)+1)
		+ (sizeof...(T)*2+1)) % (sizeof...(T)+1)>::type type;
};*/


template<class... T>
Tag<T...> tuple_to_tag(std::tuple<T...> const&) { return {}; }

template<class... S, class... T>
auto tag_to_tuple(Tag<S...> const&, T &&... t)
-> decltype(std::make_tuple(std::forward<T>(t)...))
	{ return std::make_tuple(std::forward<T>(t)...); }

template<class S, class... T, std::size_t N, class = std::enable_if_t<N % (sizeof...(T)+1), void>>
constexpr auto operator<<(Tag<S, T...> const&, Seq<std::size_t, N> const&)
-> decltype(Tag<T..., S>{} << Seq<std::size_t, N-1>{}) { return {}; }
template<class S, class... T, std::size_t N, class = std::enable_if_t<!(N % (sizeof...(T)+1)), void>>
constexpr Tag<S, T...> operator<<(Tag<S, T...> const&, Seq<std::size_t, N> const&) { return {}; }

template<class S, class... T, long N>
constexpr auto operator<<(Tag<S, T...> const& t, Seq<long, N> const&) {
	constexpr std::size_t M = sizeof...(T) + 1, I = (N < 0) ? (M - (-N % M + M) % M) : (N % M);
	return t << Seq<std::size_t, I>{};
}
template<class S, class... T, long N>
constexpr auto operator>>(Tag<S, T...> const& t, Seq<long, N> const&)
-> decltype(t << Seq<long, -N>{}) { return {}; }
template<class S, class... T, std::size_t N>
constexpr auto operator>>(Tag<S, T...> const& t, Seq<std::size_t, N> const&)
-> decltype(t << Seq<long, -long(N)>{}) { return {}; }

template<class T, T N>
constexpr Tag<> operator<<(Tag<> const&, Seq<T, N> const&) { return {}; }
template<class T, T N>
constexpr Tag<> operator>>(Tag<> const&, Seq<T, N> const&) { return {}; }

template<class, class T, T N>
struct Rotate;
template<class... S, class T, T N>
struct Rotate<Tag<S...>, T, N> {
	typedef decltype(Tag<S...>{} << Seq<T, N>{}) type;
};
template<class S, class T, T N>
using Rotate_t = typename Rotate<S, T, N>::type;

template<std::size_t N, class... L, class... R, class V = std::enable_if_t<!N, void>>
constexpr Tag<R...> head(SeqSz<N>, Tag<L...>, Tag<R...>, V = {}) { return {}; }

template<std::size_t N, class L0, class... L, class... R, class V = std::enable_if_t<N, void>>
constexpr auto head(SeqSz<N>, Tag<L0, L...>, Tag<R...>, V = {})
-> decltype(head<N>(Tag<L...>{}, Tag<R..., L0>{})) { return {}; }

template<std::size_t N, class... L, class = std::enable_if_t<(N < sizeof...(L)-1), void>>
constexpr auto tail(SeqSz<N>, Tag<L...> V = {})
-> decltype(head<N>(V >> SeqL<N>{})) { return {}; }

template<class... L, class... R>
constexpr bool match_head(Tag<L...>, Tag<R...>) { return false; }
template<class... L, class... R>
constexpr bool match_head(Tag<L...>, Tag<L..., R...>) { return true; }

template<class... L, class... R>
constexpr bool match_tail(Tag<L...> l, Tag<R...> r) { return match_head(reverse(l), reverse(r)); }

template<class... L, class... R>
constexpr Tag<R...> remove_head(Tag<L...>, Tag<R...>) { return {}; }
template<class... L, class... R>
constexpr Tag<R...> remove_head(Tag<L...>, Tag<L..., R...>) { return {}; }

template<class... L, class... R>
constexpr auto remove_tail(Tag<L...> l, Tag<R...> r)
-> decltype(reverse(remove_head(reverse(l), reverse(r)))) { return {}; }

}


#endif
