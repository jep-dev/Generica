#ifndef DEFER_HPP
#define DEFER_HPP

#include "sequence.hpp"

namespace Detail {


template<class...> struct Defer;
template<class... F> struct Defer {
	static constexpr std::size_t size = sizeof...(F);
	template<class G> Defer<F..., G&> then(G & g) { return {g}; }
	template<class G> Defer<F..., const G&> then(G const& g) { return {g}; }
	template<class G> Defer<F..., G> then(G && g) { return {std::move(g)}; }

	template<class G, class H, class... I>
	auto then(G && g, H && h, I &&... i) {
	// -> decltype(then(g).then(h, i...))
		return then(std::forward<G>(g))
				.then(std::forward<H>(h), std::forward<I>(i)...);
	}
};

template<class F, class... G> struct Defer<F, G...> {
	typedef F value_type;
	value_type value;
	static constexpr std::size_t size = 1 + sizeof...(G);

	typedef Defer<F, G...> type;
	typedef Defer<G...> next_type;
	next_type next;

	template<class H> Defer<F, G..., H&> then(H & h)
		{ return {std::forward<F>(value), next.then(h)}; }
	template<class H> Defer<F, G..., H const&> then(H const& h)
		{ return {std::forward<F>(value), next.then(h)}; }
	template<class H> Defer<F, G..., H> then(H && h)
		{ return {std::forward<F>(value), next.then(std::move(h))}; }
	template<class H, class I, class... J>
	auto then(H && h, I && i, J &&... j) -> decltype(then(std::forward<H>(h))
		.then(std::forward<I>(i), std::forward<J>(j)...)) {
		return then(std::forward<H>(h))
				.then(std::forward<I>(i), std::forward<J>(j)...);
	}

	template<class... T> next_type && operator()(T &&... t)
		{ value(std::forward<T>(t)...); return std::move(next); }
};

template<std::size_t... I, class L, class... R>
auto && move_emplace_back(SeqSz<I...>, L && l, R &&... r)
// -> std::tuple<RemoveCVRef_t<std::get<I>(std::declval<std::tuple<L...>>())>..., R...>
{ return std::make_tuple(std::move(std::get<I>(l))..., std::forward<R>(r)...); }

template<class L, class R>
auto && move_emplace_back(L && l, R && r) {
// -> decltype(move_emplace_back(IncSeq<...>{}, ...))
	return move_emplace_back(IncSeq<std::tuple_size<L>::value,
			true, std::size_t>{}, std::forward<L>(l), std::forward<R>(r));
}

template<std::size_t I, std::size_t... J, class L>
auto && move_pop_front(SeqSz<I, J...>, L && l) {
// -> decltype(std::make_tuple(std::move(std::get<J>(l))...))
	return std::make_tuple(std::move(std::get<J>(l))...);
}

template<class T>
auto && move_pop_front(T && t) {
	return move_pop_front(IncSeq<std::tuple_size<RemoveCVRef_t<T>>::value,
			true, std::size_t>{}, std::forward<T>(t));
}

/*template<class S = Tag<>, class... F>
struct DeferTransform;

template<class... S, class... F>
auto defer_transform(std::tuple<S...> && s, std::tuple<F...> && f)
	-> DeferTransform<Tag<S...>, F...> { return {std::move(s), std::move(f)}; }
template<class... F>
auto defer_transform(std::tuple<F...> && f)
	-> DeferTransform<Tag<>, F...> { return {std::tuple<>{}, std::move(f)}; }

template<class... S, class... F>
struct DeferTransform<Tag<S...>, F...> {
	std::tuple<S...> values;
	std::tuple<F...> functions;

	template<class... T>
	auto operator()(T &&... t) {
		static_assert(sizeof...(F), "");
		return defer_transform(
			move_emplace_back(std::move(values),
				std::get<0>(functions)(std::forward<T>(t)...)),
			move_pop_front(std::move(functions)));
	}
	template<class G>
	auto then(G const& g) {
		return DeferTransform<Tag<S...>, F..., G const&>{std::move(values),
			move_emplace_back(std::move(functions), g)};
};*/

}

#endif
