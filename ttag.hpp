#ifndef TTAG_HPP
#define TTAG_HPP

#include "utility.hpp"
#include "tag.hpp"
#include "sequence.hpp"

namespace Detail {

template<class C, class T> struct ATag {
	typedef C Template;
	typedef T Arguments;
};

template<template<class...> class... C> struct TTag;
template<template<class V, V...> class... C> struct TSeq;

template<template<class...> class C, class... T>
using TTagInstance = ATag<TTag<C>, Tag<T...>>;
template<template<class S, S... V> class C, class S, S... V>
using TSeqInstance = ATag<TSeq<C>, Seq<S, V...>>;

template<template<class...> class... L, template<class...> class... R>
constexpr bool contains(TTag<L...> l, TTag<R...> r) { return l.contains(r); }
template<template<class...> class... L, template<class S, S...> class... R>
constexpr bool contains(TTag<L...> s, TSeq<R...> t) { return false; }

template<template<class S, S...> class... L, template<class...> class... R>
constexpr bool contains(TSeq<L...> l, TTag<R...> r) { return false; }
template<template<class S, S...> class... L, template<class S, S...> class... R>
constexpr bool contains(TSeq<L...> l, TSeq<R...> r) { return l.contains(r); }

template<> struct TTag<> {
	typedef TTag<> head_type;
	typedef TTag<> tail_type;
	static constexpr unsigned size = 0;
	template<class R> constexpr bool contains(R &&) const { return false; }
	template<class R> constexpr bool contains(void) const { return false; }
};

template<template<class...> class C0, template<class...> class... CN>
struct TTag<C0, CN...> {
	typedef TTag<C0> head_type;
	typedef TTag<CN...> tail_type;
	static constexpr unsigned size = 1 + sizeof...(CN);
	template<template<class...> class T>
	constexpr bool contains(TTag<T> const& t) const
		{ return std::is_same<TTag<T>, TTag<C0>>::value || TTag<CN...>{}.contains(t); }
	template<template<class...> class T>
	constexpr bool contains(void) const { return contains(TTag<T>{}); }
};


template<template<class V, V...> class... C>
struct TSeq {
	typedef TSeq<> head_type;
	typedef TSeq<> tail_type;
	static constexpr unsigned size = 0;
	template<class R> constexpr bool contains(R &&) const { return false; }
	template<class R> constexpr bool contains(void) const { return false; }
};
template<template<class V, V...> class C0,
		template<class W, W...> class... CN>
struct TSeq<C0, CN...> {
	typedef TSeq<C0> head_type;
	typedef TSeq<CN...> tail_type;
	static constexpr unsigned size = 1 + sizeof...(CN);
	template<template<class U, U...> class R>
	constexpr bool contains(TSeq<R> const&) const
		{ return Tag<TSeq<C0>, TSeq<CN>...>{}.contains(Tag<TSeq<R>>{}); }
	template<template<class U, U...> class R>
	constexpr bool contains(void) const { return contains(TSeq<R>{}); }
};

template<template<class...> class C, class... T>
constexpr ATag<TTag<C>, Tag<T...>> uninstantiate(Tag<C<T...>> const&) { return {}; }
template<template<class S, S...> class C, class S, S... V,
		class = std::enable_if_t<sizeof...(V), void>>
constexpr ATag<TSeq<C>, Seq<S,V...>> uninstantiate(Tag<C<S, V...>> const&) { return {}; }

template<template<class...> class C, class... T>
constexpr Tag<C<T...>> instantiate(ATag<TTag<C>, Tag<T...>> const&) { return {}; }
template<template<class S, S... V> class C, class S, S... V,
		class = std::enable_if_t<sizeof...(V), void>>
constexpr Tag<C<S, V...>> instantiate(ATag<TSeq<C>, Seq<S, V...>> const&) { return {}; }

template<class A> using Instantiate_t = decltype(instantiate(A{}));
template<class A> using Uninstantiate_t = decltype(uninstantiate(A{}));

}

#endif
