#ifndef FOR_EACH_HPP
#define FOR_EACH_HPP

#include "sequence.hpp"

namespace Detail {

template<std::size_t I = 0, class F, class... S, class... T>
auto for_each(std::tuple<S...> const& s, F const& f, T &&... t)
		-> std::enable_if_t<I == sizeof...(S), void> {}
template<std::size_t I = 0, class F, class... S, class... T>
auto for_each(std::tuple<S...> const& s, F const& f, T &&... t)
		-> std::enable_if_t<(I < sizeof...(S)), void> {
	f(std::get<I>(s), t...);
	for_each<I+1>(s, f, std::forward<T>(t)...);
}
template<std::size_t I = 0, class F, class... S, class... T>
auto for_each(std::tuple<S...> & s, F const& f, T &&... t)
		-> std::enable_if_t<I == sizeof...(S), void> {}
template<std::size_t I = 0, class F, class... S, class... T>
auto for_each(std::tuple<S...> & s, F const& f, T &&... t)
		-> std::enable_if_t<(I < sizeof...(S)), void> {
	f(std::get<I>(s), t...);
	for_each<I+1>(s, f, std::forward<T>(t)...);
}
template<std::size_t I = 0, class F, class... S, class... T>
auto for_each(Tag<S...> const& s, F const& f, T &&... t)
		-> std::enable_if_t<I == sizeof...(S), void> {}
template<std::size_t I = 0, class F, class... S, class... T>
auto for_each(Tag<S...> const& s, F const& f, T &&... t)
		-> std::enable_if_t<I < sizeof...(S), void> {
	f(get<I>(s), t...);
	for_each<I+1>(s, f, std::forward<T>(t)...);
}

template<std::size_t I = 0, class S, S... V, class F, class... T>
auto for_each(Seq<S, V...> const& s, F const& f, T &&... t)
		-> std::enable_if_t<I == sizeof...(V), void> {};
template<std::size_t I = 0, class S, S... V, class F, class... T>
auto for_each(Seq<S, V...> const& s, F const& f, T &&... t)
		-> std::enable_if_t<(I < sizeof...(V)), void> {
	typedef Seq<S, V..., 0> S0;
	typedef Seq<S, get<I>(S0{})> J;
	f(J{}, t...);
	Detail::for_each<I+1>(s, f, std::forward<T>(t)...);
};

template<std::size_t I = 0, class... S, class... T, class F, class... U>
auto for_each_zip(std::tuple<S...> const& s,
		std::tuple<T...> const& t, F const& f, U &&... u)
-> std::enable_if_t<(I >= sizeof...(S)) || (I >= sizeof...(T)), void> {}
//-> std::enable_if_t<(I == sizeof...(S)) && (sizeof...(S) == sizeof...(T)), void> {}

template<std::size_t I = 0, class... S, class... T, class F, class... U>
auto for_each_zip(std::tuple<S...> const& s,
		std::tuple<T...> const& t, F const& f, U &&... u)
-> std::enable_if_t<(I < sizeof...(S)) && (I < sizeof...(T)), void> {
	f(std::get<I>(s), std::get<I>(t), u...);
	for_each_zip<I+1>(s, t, f, std::forward<U>(u)...);
}

template<std::size_t I = 0, std::size_t J = 0, class... S, class F, class... T>
auto for_each2(std::tuple<S...> const& s, F const& f, T &&... t)
-> std::enable_if_t<I == sizeof...(S), void> {}

template<std::size_t I = 0, std::size_t J = 0, class... S, class F, class... T>
auto for_each2(std::tuple<S...> const& s, F const& f, T &&... t)
-> std::enable_if_t<(I < sizeof...(S)), void> {
	for_each<J>(std::get<I>(s), f, t...);
	for_each2<I+1>(s, f, std::forward<T>(t)...);
}

template<std::size_t I = 0, std::size_t J = 0, class... S, class F, class... T>
auto for_each2(std::tuple<S...> &s, F const& f, T &&... t)
-> std::enable_if_t<I == sizeof...(S), void> {}

template<std::size_t I = 0, std::size_t J = 0, class... S, class F, class... T>
auto for_each2(std::tuple<S...> &s, F const& f, T &&... t)
-> std::enable_if_t<(I < sizeof...(S)), void> {
	for_each<J>(std::get<I>(s), f, t...);
	for_each2<I+1>(s, f, std::forward<T>(t)...);
}

}

#endif
