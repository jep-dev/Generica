#ifndef PRETTY_HPP
#define PRETTY_HPP

#include <string>

// Future support for reflection may add "typename(type)" to replace all of this.

#if defined(__unix__)
#define HAS_PRETTY true
#define PRETTY_FUNCTION __PRETTY_FUNCTION__
#elif defined(_WIN32) || defined(_WIN64)
#define HAS_FUNCSIG true
#define PRETTY_FUNCTION __FUNCSIG__
#else
#define PRETTY_FUNCTION __func__
#endif

template<class T> struct Pretty {
	static constexpr const char *pretty(void) { return PRETTY_FUNCTION; }
	operator std::string(void) const {
		using namespace std;
		string unknown = "(unknown)",
				with = "with T = ",
				orig = pretty(), lhs, rhs;
#ifdef HAS_FUNCSIG
		lhs = "<", rhs = ">";
#elif defined HAS_PRETTY
		lhs = "{[", rhs = "]}";
#else
		return "";
#endif
		// Match the earliest opening and latest closing brackets
		auto p0 = orig.find_first_of(lhs), p1 = orig.find_last_of(rhs);
		if(p0 == string::npos || p1 == string::npos) return std::move(unknown);
		p0++; // Offset by the width of the opening character
		auto result = orig.substr(p0, p1-p0);
		if(orig.substr(p0, with.length()) == with)
			result = result.substr(with.length());
		return result;
	}
	template<class S>
	friend S& operator<<(S & s, Pretty const& p)
		{ return s << std::string(p), s; }
};

template<class... T>
std::string pretty(void) {
	std::string strs[] = {std::string(Pretty<T>{})...};
	std::string out, pfx = "";
	for(auto const& str : strs) {
		out += pfx + str;
		pfx = ", ";
	}
	return out;
}
template<class... T>
std::string pretty(T &&... t) {
	return pretty<T...>();
}

template<class T>
std::string pretty_abbrev(void) {
	std::string s = Pretty<T>(),
			subs[][2] {{"std::", ""}, {"< <", "<<"}, {"> >", ">>"}};
	std::size_t pos = 0;
	for(auto const& sub : subs) {
		pos = 0;
		while((pos = s.find(sub[0], pos)) != std::string::npos) {
			s.replace(pos, sub[0].length(), sub[1]);
		}
	}
	return s;
}
template<class T>
std::string pretty_abbrev(T && t) {
	return pretty_abbrev<T>();
}

#endif
