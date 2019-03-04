#ifndef NUMERIC_HPP
#define NUMERIC_HPP

#include "tag.hpp"

template<class T>
constexpr bool has_quiet_NaN(void)
	{ return std::is_arithmetic<T>::value && std::numeric_limits<T>::has_quiet_NaN; }

template<class T>
using QuietNaN = std::enable_if_t<has_quiet_NaN<T>(),
	std::integral_constant<T, std::numeric_limits<T>::quiet_NaN()>>;

template<class T>
constexpr bool has_loud_NaN(void)
	{ return std::is_arithmetic<T>::value && std::numeric_limits<T>::has_signaling_NaN; }

template<class T>
using LoudNaN = std::enable_if<has_loud_NaN<T>(),
	std::integral_constant<T, std::numeric_limits<T>::signaling_NaN()>>;

template<class S>
std::enable_if<has_quiet_NaN<S>(),S>
make_NaN(Detail::Tag<QuietNaN<S>> = {}) { return QuietNaN<S>::value; }

template<class S>
std::enable_if<has_loud_NaN<S>() && !has_quiet_NaN<S>(),S>
make_NaN(Detail::Tag<LoudNaN<S>> = {}) { return LoudNaN<S>::value; }

#endif
