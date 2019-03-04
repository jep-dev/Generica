#ifndef STREAMS_TPP
#define STREAMS_TPP

#include "streams.hpp"
//#include <iostream>
#include <iomanip>
//#include <sstream>

namespace Streams {

template<class S, class T>
enable_if_t<HasInsert<S, string>::value, S&>
center(S & out, T const& t, unsigned N, char fill, bool leftish) {
	using namespace std;
	auto str = to_string(t);
	int len = str.length(), dlen = N - len, dlen0 = dlen/2, dlen1 = dlen - dlen0;
	string pads[] = {string(dlen0, fill), string(dlen1, fill)};
	return out << (pads[!leftish] + str + pads[leftish]), out;
}

string align(string const& out, unsigned N, int dir, bool truncate, char fill, bool leftish) {
	int dlen = N - out.length();
	if(dlen < 0) {
		dlen = -dlen;
		if(!truncate) return out;
		if(dir > 0) return out.substr(0, N);
		if(dir < 0) return out.substr(dlen);
		if(leftish) return out.substr(dlen/2, N);
		return out.substr(dlen-dlen/2, N);
	}
	if(!truncate) return out;
	if(dir > 0) return out + std::string(dlen, fill);
	if(dir < 0) return std::string(dlen, fill) + out;
	return center(out, N, fill, leftish);

}

template<class T, class V>
std::pair<long, long> MiniMax<T, V, Detail::Void_t<decltype(&V::tellp)>>::operator()(T& t) const {
	std::pair<long, long> out(std::numeric_limits<long>::max(), 0);
	for(auto& i : t) {
		auto pos = i.tellp();
		if(pos < 0) continue;
		i.seekp(0, std::ios_base::end);
		auto len = i.tellp();
		i.seekp(pos);
		if(len < 0) continue;
		if(len < out.first) out.first = len;
		if(len > out.second) out.second = len;
	}
	return out;
};

template<class S, class SV>
auto level(S& s, char fill)
		-> std::enable_if_t<HasInsert<SV, string>::value && HasTellpSeekp<SV>::value, S>& {
	auto mm = minimax(s);
	for(auto& it : s) {
		auto cur = it.tellp();
		it.seekp(0, std::ios_base::end);
		auto len = mm.second - it.tellp();
		it.seekp(cur);
		if(len > 0) it += std::string(len, fill);
	}
	return s;
}

/** Subclass of STL ostringstream that defines a const size method.
 * Note - adaptor.hpp does not use this because it relies on str(). */
struct OStringStream : std::ostringstream {
	std::size_t size(void) const { return str().length(); }
	std::size_t size(void) {
		auto pos = tellp();
		seekp(0, std::ios_base::end);
		auto out = tellp();
		seekp(pos);
		return std::max(0, int(out));
	}
	virtual ~OStringStream(void) {}
};

}

#endif
