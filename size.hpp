#ifndef SIZE_HPP
#define SIZE_HPP

#include "utility.hpp"

using False = std::false_type;
using True = std::true_type;

template<class S, class = void> struct HasSize : False { };
template<class S, class = void> struct HasLength : False { };
template<class S, class = void> struct HasDimension
	: std::is_array<std::remove_const_t<std::remove_reference_t<S>>> { };
template<class S, class = void, class = void> struct HasTellpSeekp : False { };
template<class S, class = void, class = void> struct HasTellgSeekg : False { };

template<class S> struct HasSize<S, Detail::Void_t<decltype(&S::size)>> : True { };
template<class S> struct HasLength<S, Detail::Void_t<decltype(&S::length)>> : True { };

template<class S>
struct HasTellpSeekp<S, Detail::Void_t<decltype(&S::tellp)>,
		Detail::Void_t<decltype(&S::seekp)>> : True {};
template<class S>
struct HasTellgSeekg<S, Detail::Void_t<decltype(&S::tellg)>,
		Detail::Void_t<decltype(&S::seekg)>> : True {};

/////////////////////////////////// Arrays ///////////////////////////////////

template<class T, unsigned N = 0> std::size_t getSize(T const (&) [N]) { return N; }
template<class T, unsigned N = 0> std::size_t getSize(T (&) [N]) { return N; }
template<class T, unsigned N = 0> std::size_t getSize(T (&&) [N]) { return N; }
template<class T> std::size_t getSize(T const (&) [0]) { return 0; }
template<class T> std::size_t getSize(T (&&) [0]) { return 0; }
template<class T> std::size_t getSize(T (&) [0]) { return 0; }

/////////////////////////////////// Length ///////////////////////////////////
/** Proof of concept for HasLength */
template<class T> std::enable_if_t<HasLength<Detail::RemoveCVRef_t<T>>::value, std::size_t>
	getSize(T && t) { return t.length(); }
template<class T> std::enable_if_t<HasLength<T>::value, std::size_t>
	getSize(T & t) { return t.length(); }

///////////////////////////////////// Size ///////////////////////////////////
/** Proof of concept for HasSize; excludes types with length to remove ambiguity */
template<class T> std::enable_if_t<HasSize<T>::value && !HasLength<T>::value, std::size_t>
	getSize(T && t) { return t.size(); }

///////////////////////////////////// Seek* //////////////////////////////////
template<class T>
std::enable_if_t<HasTellpSeekp<T>::value, std::size_t> getSize(T & t) {
	auto pos = t.tellp(), out = (t.seekp(0, std::ios::end), t.tellp());
	return t.seekp(pos), out;
}
template<class T>
std::enable_if_t<HasTellpSeekp<T>::value, std::size_t> getSize(T && t) {
	auto pos = t.tellp(), out = (t.seekp(0, std::ios::end), t.tellp());
	return t.seekp(pos), out;
}
template<class T>
std::enable_if_t<HasTellgSeekg<T>::value, std::size_t> getSize(T & t) {
	auto pos = t.tellg(), out = (t.seekg(0, std::ios::end), t.tellg());
	return t.seekg(pos), out;
}
template<class T>
std::enable_if_t<HasTellgSeekg<T>::value, std::size_t> getSize(T && t) {
	auto pos = t.tellg(), out = (t.seekg(0, std::ios::end), t.tellg());
	return t.seekg(pos), out;
}

/////////////////////////////////// Fallbacks ////////////////////////////////
/** Fallback in case none are implemented; comment out to require one of the others */
template<class T> std::enable_if_t<!std::is_array<T>::value
		&& !HasTellpSeekp<T>::value && !HasTellgSeekg<T>::value
		&& !HasSize<T>::value && !HasLength<T>::value, std::size_t>
	getSize(T && t) { return 0; }

#endif
