#ifndef FORMAT_HPP
#define FORMAT_HPP

#include "math.hpp"
#include "streams.hpp"

namespace Streams {

vector<string> split(string str);

vector<string> & align(vector<string> & lines, int dir, char fill = ' ') {
	auto lens = minimax(lines);
	for(auto & line : lines) {
		auto len = lens.second - line.length();
		if(len > 0) {
			if(dir == 0) {
				auto llen = len / 2, rlen = len - llen;
				if(rlen > 0) {
					line += string(rlen, fill);
					if(llen > 0) line = string(llen, fill) + line;
				}
			} else {
				string pad(len, fill);
				if(dir < 0) line += pad;
				else line = pad + line;
			}
		}
	}
	return lines;
}

vector<string> paste(string const& lhs, string const& rhs,
		char fill = ' ', const char *border = " ") {
	auto lvec = split(lhs), rvec = split(rhs);
	auto llen = lvec.size(), rlen = rvec.size();
	for(long N = rlen - llen, i = 0; i < N; i++)
		lvec.emplace_back("");
	for(long N = llen - rlen, i = 0; i < N; i++)
		rvec.emplace_back("");
	align(lvec, 1, fill);
	align(rvec, -1, fill);
	for(long unsigned int N = lvec.size(), i = 0; i < N; i++) {
		lvec[i] += border + rvec[i];
	}
	return std::move(lvec);
}

template<class... T> string join(T &&... t);

}

#endif
