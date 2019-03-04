#ifndef FORMAT_TPP
#define FORMAT_TPP

#include <sstream>
#include "format.hpp"

namespace Streams {

vector<string> split(string str) {
	vector<string> out;
	istringstream iss(str);
	string cur;
	while(std::getline(iss, cur)) {
		out.emplace_back(cur);
	}
	return std::move(out);
}

template<class... T>
string join(T &&... t) {
	auto vec = paste(std::forward<T>(t)...);
	ostringstream oss;
	for(auto const& line : vec) {
		oss << line << '\n';
	}
	return oss.str();
}

}

#endif
