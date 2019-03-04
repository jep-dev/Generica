#include "parse.hpp"
#include <sstream>

namespace Streams {

template<class S, class T>
S parse_value(T const& word, S def) {
	S cur;
	std::istringstream iss(word);
	iss >> cur;
	if(iss.fail()) return def;
	return cur;
}

}
