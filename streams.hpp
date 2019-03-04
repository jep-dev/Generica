#ifndef STREAMS_HPP
#define STREAMS_HPP

#include <algorithm>
#include <iosfwd>
#include <string>
#include <vector>

#include "tag.hpp"

#ifndef INDENT
#define INDENT "\t"
#endif

namespace Streams {
using std::string;
using std::vector;
using std::ostream;
using std::istream;
using std::ostringstream;
using std::istringstream;

using std::declval;
using std::true_type;
using std::false_type;
using std::enable_if_t;
//using std::is_arithmetic;

template<class T>
static constexpr bool is_arithmetic_v(Detail::Tag<T> = {}) {
	return std::is_arithmetic<T>::value;
}

template<class S, class T, class = void>
struct HasInsert : false_type {};
template<class S, class T = S, class = void>
struct HasAdd : false_type {};
template<class S, class = void>
struct HasLength : false_type {};
template<class S, class = void, class = void>
struct HasTellpSeekp : false_type {};

template<class S, class T>
struct HasInsert<S, T, Detail::Void_t<decltype(declval<S>() << declval<T>())>> : true_type {};
template<class S, class T>
struct HasAdd<S, T, Detail::Void_t<decltype(declval<S>() += declval<T>())>> : true_type {};
template<class S>
struct HasLength<S, Detail::Void_t<decltype(declval<S>().length())>> : true_type {};
template<class S>
struct HasTellpSeekp<S, Detail::Void_t<decltype(declval<S>().tellp())>,
		Detail::Void_t<decltype(&S::seekp)>> : true_type {};

template<class S, class T>
constexpr bool has_insert(Detail::Tag<S> && s = {}, Detail::Tag<T> && t = {})
	{ return HasInsert<S,T>::value; }
template<class S, class T = S>
constexpr bool has_add(Detail::Tag<S> && s = {}, Detail::Tag<T> && t = {})
	{ return HasAdd<S, T>::value; }

template<class S>
constexpr bool has_length(Detail::Tag<S> && s = {}) { return HasLength<S>::value; }
template<class S>
constexpr bool has_tellp_seekp(Detail::Tag<S> && s = {}) { return HasTellpSeekp<S>::value; }

/** @brief Passthrough specialization for later abstract to_string(const T&) */
string to_string(const char *s, unsigned, bool) { return s; }
/** @brief Passthrough specialization for later abstract to_string(const T&) */
string to_string(string const& s, unsigned, bool) { return s; }

template<class S, class T>
enable_if_t<HasInsert<S, T>::value, S&>
center(S & out, T const& t, unsigned N, char fill = ' ', bool leftish = true) {
	using namespace std;
	auto str = to_string(t, 2, false);
	int len = str.length(), dlen = N - len, dlen0 = dlen/2, dlen1 = dlen - dlen0;
	string pads[] = {string(dlen0, fill), string(dlen1, fill)};
	return out << (pads[leftish] + str + pads[!leftish]), out;
}

template<class S, class T>
enable_if_t<!HasInsert<S, T>::value && HasInsert<S, string>::value, S&>
center(S & out, T const& t, unsigned N, char fill = ' ', bool leftish = true) {
	return center(out, to_string(t), N, leftish);
}

string center(string const& s, unsigned N, char fill = ' ', bool leftish = true) {
	int len = s.length(), len2 = N - len, llen = len2/2, rlen = len2 - llen;
	if(leftish) std::swap(llen, rlen);
	return string(std::max(llen, 0), fill) + s + string(std::max(rlen, 0), fill);
}

template<template<class...> class V, class... VT>
V<string, VT...> center(V<string, VT...> const& v,
		unsigned N, char fill = ' ', bool leftish = true) {
	V<string, VT...> out;
	for(auto const& s : v) {
		out.emplace_back(center(s, N, fill, leftish));
	}
}
string center(const char *s, unsigned N, char fill, bool leftish) {
	return center(string(s), N, fill, leftish);
}

string align(string const& out, unsigned N, int dir = 1,
		bool truncate = false, char fill = ' ', bool leftish = true);

string trim(string const& s, bool leading = true, bool trailing = true) {
	long size = s.length(), front = 0, back = size-1;
	if(size == 0) return "";

	auto match = [] (char c) -> bool {
		switch(c) {
			case ' ': case '\t':
			case '\r': case '\n': return false;
			default: return true;
		}
	};

	if(leading) for(front = 0; front <= back; front++)
		if(match(s[front])) break;
	if(trailing) for(back = size-1; back >= front; back--)
		if(match(s[back])) break;

	return s.substr(front, back-front+1);
}

/** @brief Abstract minimax (pair with first=min and second=max.)
 * Note that it could still be abstract with respect to size/length/tellp and to begin/end
 * with more work.
 */
template<class T> using rm_ref_t = std::remove_reference_t<T>;
template<class T> using rm_const_t = std::remove_const_t<T>;
template<class T> using rm_cv_t = std::remove_cv_t<T>;

template<class T> using First_t = rm_ref_t<decltype(*std::begin(declval<T&>()))>;
template<class T, class V = First_t<T>, class F = void>
struct MiniMax;

template<class T, class V>
struct MiniMax<T, V, Detail::Void_t<decltype(&V::length)>> {
	std::pair<long, long> operator()(T& t) const {
		std::pair<long, long> out(std::numeric_limits<long>::max(), 0);
		for(auto const& i : t) {
			auto len = i.length();
			if(len < out.first) out.first = len;
			if(len > out.second) out.second = len;
		}
		return out;
	}
};
template<class T, class V>
struct MiniMax<T, V, Detail::Void_t<decltype(&V::tellp)>> {
	std::pair<long, long> operator()(T& t) const;
};
template<class T>
std::pair<long, long> minimax(T& t) {
	return MiniMax<T, First_t<T>>()(t);
}


template<class S, class SV = typename S::value_type>
auto level(S& s, char fill = ' ')
		-> std::enable_if_t<HasInsert<SV, string>::value && HasTellpSeekp<SV>::value, S>&;
template<class S, class SV = typename S::value_type>
auto level(S& s, char fill = ' ')
		-> std::enable_if_t<HasAdd<SV, string>::value && HasLength<SV>::value, S>& {
	auto mm = minimax(s);
	for(auto& it : s) {
		auto len = mm.second - it.length();
		if(len > 0) it += std::string(len, fill);
	}
	return s;
}

template<template<class...> class O, class S, class... SN,
		template<class...> class I, class T, class... TN,
		class E = std::enable_if_t<HasAdd<S,T>::value && HasLength<S>::value, S>>
auto level_insert_each(O<S, SN...> & ss, I<T, TN...> & ts,
		char fill = ' ', bool leveled = false) -> decltype(ss) {
	if(!leveled) level(ss, fill);
	auto beg = std::begin(ss);
	for(auto const& t : ts)
		*beg += t, beg++;
	level(ss, fill);
	return ss;
}
template<template<class...> class O, class S, class... SN, class T>
std::enable_if_t<HasAdd<S,T>::value && HasLength<S>::value, O<S,SN...>&>
level_insert(O<S, SN...> & ss, T const& t, char fill = ' ', bool leveled = false) {
	if(!leveled) level(ss, fill);
	for(auto & s : ss)
		s += t;
	return ss;
}
template<template<class...> class O, class S, class... SN, class T>
std::enable_if_t<HasInsert<S,T>::value && HasTellpSeekp<S>::value, O<S,SN...>&>
level_insert(O<S, SN...> & ss, T const& t, char fill = ' ', bool leveled = false) {
	if(!leveled) level(ss, fill);
	for(auto & s : ss)
		s += t;
	return ss;
}

template<class S, class V>
std::string filter(S const& str, V const& words) {
	std::string s = str;
	for(auto const& word : words) {
		std::string src = word[0], dest = word[1];
		auto pos = s.find(src);
		while(pos != std::string::npos) {
			s.replace(pos, src.length(), dest);
			pos = s.find(src, pos+1);
		}
	}
	return s;
}

struct OStringStream;

}

#endif
