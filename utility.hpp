#ifndef UTILITY_HPP
#define UTILITY_HPP

#include <algorithm>
#include <tuple>
#include <utility>
#include <map>
#include <type_traits>

namespace Detail {

using std::declval;

template<class...> struct Tag;
template<class S, S...> struct Seq;
template<template<class...> class...> struct TTag;
template<template<class S, S...> class...> struct TSeq;

// Enable more if you need them.
template<bool... I> using SeqB = Seq<bool, I...>;
template<char... I> using SeqC = Seq<char, I...>;
//template<short... I> using SeqS = Seq<short, I...>;
template<unsigned... I> using SeqU = Seq<unsigned, I...>;
template<int... I> using SeqI = Seq<int, I...>;
template<long... I> using SeqL = Seq<long, I...>;
template<std::size_t... I> using SeqSz = Seq<std::size_t, I...>;
//template<unsigned char... I> using SeqUC = Seq<unsigned char, I...>;
//template<unsigned short... I> using SeqUS = Seq<unsigned short, I...>;
//template<unsigned long... I> using SeqUL = Seq<unsigned long, I...>;


/** Delimiter tag; only impacts compile-time resolution */
struct Delim {};

/** Allows SFINAE for missing members. */
template<class... T> struct Void;
/** Helper for Void's type */
template<class... T> using Void_t = typename Void<T...>::value_type;

/** Removes qualifiers correctly, even for arrays */
template<class S> struct RemoveCVRef;
/** Helper for RemoveCVRef's type */
template<class S> using RemoveCVRef_t = typename RemoveCVRef<S>::type;
template<class S> RemoveCVRef_t<S> remove_cvref(S && s)
	{ return static_cast<RemoveCVRef_t<S>>(s); }

/** A variadic tag, essentially a type sequence. */
template<class...> struct Tag;

/** Equivalent to std::tuple; see eval for conversion from Tag */
/*template<class... U>
using Val = std::tuple<U...>;*/

template<class... T> struct Void { using value_type = void; };

template<class S>
struct RemoveCVRef { typedef std::remove_cv_t<std::remove_reference_t<S>> type; };
template<class S, unsigned N>
struct RemoveCVRef<S[N]> { typedef RemoveCVRef_t<S> type[N]; };

template<class S>
using Value_t = typename RemoveCVRef_t<S>::value_type;

template<class S, class V = void> struct InnerValue { typedef S value_type; };
template<class S> struct InnerValue<S, Detail::Void_t<Value_t<S>>>
	{ typedef Value_t<InnerValue<Value_t<S>>> value_type; };
template<class S>
using InnerValue_t = Value_t<InnerValue<S>>;

template<class S, class... T>
auto get_value_type(Detail::Tag<S, T...> = {}) -> Value_t<S, T...> { return {}; }
template<class S, class... T>
auto get_value_type(S && s, T &&... t) -> Value_t<S, T...> { return {}; }
template<class S>
auto get_inner_value_type(Detail::Tag<S> = {}) -> InnerValue_t<S> { return {}; }
template<class S>
auto get_inner_value_type(S && s) -> InnerValue_t<S> { return {}; }

template<class S>
using Head_t = decltype(head(std::declval<S>()));
template<class S>
using PopFront_t = decltype(pop_front(std::declval<S>()));
template<class S>
using PopBack_t = decltype(pop_back(std::declval<S>()));

template<class... T>
using CommonValue_t = std::common_type_t<Value_t<T>...>;

template<class S, class T>
using ConvertibleFrom_t = std::enable_if_t<std::is_convertible<T,S>::value, T>;
template<class S, class T>
using ConvertibleTo_t = std::enable_if_t<std::is_convertible<T,S>::value, S>;

template<class S, class... T> struct SumType { typedef S value_type; };
template<class R, class S, class... T> struct SumType<R,S,T...>
	{ using value_type = decltype(declval<R>()
		+ declval<Value_t<SumType<S,T...>>>()); };
template<class S, class... T> struct DifferenceType { typedef S value_type; };
template<class R, class S, class... T> struct DifferenceType<R,S,T...>
	{ using value_type = decltype(declval<R>()
		* declval<Value_t<DifferenceType<S,T...>>>()); };
template<class S, class... T> struct ProductType { typedef S value_type; };
template<class R, class S, class... T> struct ProductType<R,S,T...>
	{ using value_type = decltype(declval<R>()
		* declval<Value_t<ProductType<S,T...>>>()); };
template<class S, class... T> struct QuotientType { typedef S value_type; };
template<class R, class S, class... T> struct QuotientType<R,S,T...>
	{ using value_type = decltype(declval<R>()
		/ declval<Value_t<QuotientType<S,T...>>>()); };
template<class S, class... T> struct InsertType { typedef S value_type; };
template<class R, class S, class... T> struct InsertType<R,S,T...>
	{ using value_type = decltype(declval<R>()
		<< declval<Value_t<InsertType<S, T...>>>()); };
template<class S, class... T> struct ExtractType { typedef S value_type; };
template<class R, class S, class... T> struct ExtractType<R,S,T...>
	{ using value_type = decltype(declval<R>()
		>> declval<Value_t<ExtractType<S, T...>>>()); };
template<class S, class... T> struct XorType { typedef S value_type; };
template<class R, class S, class... T> struct XorType<R,S,T...>
	{ using value_type = decltype(declval<R>()
		^ declval<Value_t<XorType<S, T...>>>()); };

template<bool R, class... S> struct SumValue
	{ using value_type = std::conditional_t<R,
		Value_t<SumType<InnerValue_t<S>...>>, Value_t<SumType<S...>>>; };
template<bool R, class... S> struct DifferenceValue
	{ using value_type = std::conditional_t<R,
		Value_t<DifferenceType<InnerValue_t<S>...>>, Value_t<DifferenceType<S...>>>; };
template<bool R, class... S> struct ProductValue
	{ using value_type = std::conditional_t<R,
		Value_t<ProductType<InnerValue_t<S>...>>, Value_t<ProductType<S...>>>; };
template<bool R, class... S> struct QuotientValue
	{ using value_type = std::conditional_t<R,
		Value_t<QuotientType<InnerValue_t<S>...>>, Value_t<QuotientType<S...>>>; };
template<bool R, class... S> struct InsertValue
	{ using value_type = std::conditional_t<R,
		Value_t<InsertType<InnerValue_t<S>...>>, Value_t<InsertType<S...>>>; };
template<bool R, class... S> struct ExtractValue
	{ using value_type = std::conditional_t<R,
		Value_t<ExtractType<InnerValue_t<S>...>>, Value_t<ExtractType<S...>>>; };
template<bool R, class... S> struct XorValue
	{ using value_type = std::conditional_t<R,
		Value_t<XorType<InnerValue_t<S>...>>, Value_t<XorType<S...>>>; };

template<class... S> using SumType_t = Value_t<SumType<S...>>;
template<class... S> using DifferenceType_t = Value_t<DifferenceType<S...>>;
template<class... S> using ProductType_t = Value_t<ProductType<S...>>;
template<class... S> using QuotientType_t = Value_t<QuotientType<S...>>;
template<class... S> using InsertType_t = Value_t<InsertType<S...>>;
template<class... S> using ExtractType_t = Value_t<ExtractType<S...>>;
template<class... S> using XorType_t = Value_t<XorType<S...>>;
template<bool R, class... S> using SumValue_t = Value_t<SumValue<R, S...>>;
template<bool R, class... S> using DifferenceValue_t = Value_t<DifferenceValue<R, S...>>;
template<bool R, class... S> using ProductValue_t = Value_t<ProductValue<R, S...>>;
template<bool R, class... S> using QuotientValue_t = Value_t<QuotientValue<R, S...>>;
template<bool R, class... S> using InsertValue_t = Value_t<InsertValue<R, S...>>;
template<bool R, class... S> using ExtractValue_t = Value_t<ExtractValue<R, S...>>;
template<bool R, class... S> using XorValue_t = Value_t<XorValue<R, S...>>;

template<class C, class K>
bool contains(C const& c, K const& k) { return c.find(k) != c.cend(); }

}

#endif
