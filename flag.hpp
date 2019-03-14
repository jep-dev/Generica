#ifndef FLAG_HPP
#define FLAG_HPP

#include "sequence.hpp"
#include "math.hpp"

namespace Detail {

template<class T, T V, class S = std::size_t>
constexpr auto contract(Seq<T, V> v, Tag<S> = {})
-> Seq<S, V> { return {}; }
template<class T, T U, T V, T... W, class S = std::size_t>
constexpr auto contract(Seq<T, U, V, W...> u, Tag<S> = {})
-> decltype(contract(Seq<S, U|V, W...>{}, Tag<S>{})) { return {}; }

template<class E, class T, std::size_t V,
		std::size_t N = 1 + log<2>(SeqSz<V>{})>
constexpr auto expand(Seq<T, V> v = {}, Tag<E> = {})
-> decltype(seq_cast(nonzero(decompose(v, SeqSz<N>{})),
		Tag<E>{})) { return {}; }

}

#endif
