#ifndef TYPESORT_HPP
#define TYPESORT_HPP

#include "functional/transform.hpp"
#include "comparable.hpp"

namespace Detail {

template<class L, class C, class R>
struct Inserting;
template<class... L, class C, class... R>
struct Inserting<Tag<L...>, C, Tag<R...>>: Tag<L..., C> {
	typedef Tag<L..., C> type;
};
template<class... L, class C, class R0, class... R>
struct Inserting<Tag<L...>, C, Tag<R0, R...>> {
	static constexpr decltype(C::key_value) ckey = C::key_value;
	static constexpr decltype(R0::key_value) rkey = R0::key_value;
	typedef typename Inserting<Tag<L..., std::conditional_t<(ckey < rkey), C, R0>>,
		std::conditional_t<(ckey < rkey), R0, C>, Tag<R...>>::type type;
};
template<class C, class GE> using Insert = Inserting<Tag<>, C, GE>;
template<class C, class GE> using Insert_t = typename Insert<C, GE>::type;

template<class L, class K, class R = Tag<>, template<class...> class CMP = std::is_same>
struct Removing: R { typedef R type; };
template<class L0, class... L, class K, class... R, template<class...> class CMP>
struct Removing<Tag<L0, L...>, K, Tag<R...>, CMP>: Removing<Tag<L...>, K,
		std::conditional_t<CMP<L0, K>::value, Tag<R...>, Tag<R..., L0>>>::type {
	typedef typename Removing<Tag<L...>, K,
		std::conditional_t<CMP<L0, K>::value, Tag<R...>, Tag<R..., L0>>>::type type;
};

template<class L, class K, template<class...> class CMP = std::is_same>
using Remove = Removing<L, K, Tag<>, CMP>;
template<class L, class K, template<class...> class CMP = std::is_same>
using Remove_t = typename Remove<L, K, CMP>::type;


/*template<std::size_t N>
constexpr auto pop_head(Tag<>) -> Tag<> { return {}; }
template<std::size_t N, class S, class... T>
constexpr auto pop_head(Tag<S, T...>)
	-> decltype(pop_head<N-1>(Tag<T...>{})) { return {}; }

template<class... L, class... R>
constexpr auto split_head(SeqSz<0>, Tag<L...>, Tag<R...>)
	-> Tag<Tag<R...>, Tag<L...>> { return {}; };
template<std::size_t N, class L0, class... L, class... R>
constexpr auto split_head(SeqSz<N>, Tag<L0, L...>, Tag<R...>)
	-> decltype(split_head(SeqSz<N-1>{}, Tag<L...>{}, Tag<R..., L0>{})) { return {}; }
template<std::size_t N, class... L>
constexpr auto split_head(SeqSz<N>, Tag<L...>)
	-> decltype(split_head(SeqSz<N>{}, Tag<L...>{}, Tag<>{})) { return {}; }

template<std::size_t N, class... L>
constexpr auto split_tail(SeqSz<N> s, Tag<L...> l)
	-> decltype(reverse(split_head(s, l >> SeqL<long(N)>{}))) { return {}; }*/


/*template<class C, class R>
struct Insert;
template<class C, class... R>
struct Insert<C, Tag<R...>>: InsertionSort<Tag<>, C, Tag<R...>>::type {
	typedef typename InsertionSort<Tag<>, C, Tag<R...>>::type type;
};
template<class C, class R>
using Insert_t = typename Insert<C, R>::type;*/

}

#endif
