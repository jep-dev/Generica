#ifndef UTILITY_TPP
#define UTILITY_TPP

//#include "tag.hpp"
#include "sequence.hpp"
//#include "functional.hpp"

//#include <iostream>
#include <iosfwd>

template<class S, S... I>
std::ostream& operator<<(std::ostream &os, Detail::Seq<S, I...> const&) {
	bool first = true;
	os << '{';
	static constexpr S V[] = {I...};
	for(S i : V) {
		if(!first) os << ", ";
		first = false;
		os << i;
	}
	os << '}';
	return os;
}

#endif
