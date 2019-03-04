#include <cmath>
#include <iostream>
#include <sstream>
#include <tuple>

#include "functional/transform.hpp"

namespace Detail {

template<class OS, class... S, unsigned N = sizeof...(S)>
auto print_tuple(OS & os, std::tuple<S...> const& s, const char *delim = "", SeqU<N> = {})
	-> std::enable_if_t<N == 0, OS&> { return os << "}", os; }
template<class OS, class... S, unsigned N = sizeof...(S)>
auto print_tuple(OS & os, std::tuple<S...> const& s, const char *delim = "", SeqU<N> = {})
-> std::enable_if_t<(N > 0), OS&> {
	if(N == sizeof...(S)) os << "{";
	os << delim << std::get<sizeof...(S)-N>(s);
	return print_tuple(os, s, ", ", SeqU<N-1>{});
};

template<class... T>
constexpr auto tuple_seq(std::tuple<T...> const&)
	-> IncSeq<sizeof...(T)> { return {}; }
template<class... T>
constexpr auto rev_tuple_seq(std::tuple<T...> const&)
	-> DecSeq<sizeof...(T)> { return {}; }
}

template<class OS, class... S>
OS& operator<<(OS & os, std::tuple<S...> const& s)
	{ return Detail::print_tuple(os, s), os; }


int main(int argc, const char *argv[]) {
	using namespace std;
	using Detail::transform;

	auto tup = make_tuple("Hello", 2, "world", M_PI, 'a');
	static constexpr auto N = tuple_size<decltype(tup)>();

	Detail::print_tuple(cout << "  From tuple: ", tup) << endl;
	cout << endl;

	auto tostr = [] (auto && v) -> string {
		ostringstream oss;
		oss << '\'' << v << '\'';
		return oss.str();
	};

	auto inc = Detail::tuple_seq(tup);
	auto xform_fwd = transform(tup, tostr);
	cout << " Forward seq: " << inc << " -> " << xform_fwd << endl;

	auto dec = Detail::rev_tuple_seq(tup); // = DecSeq<N>{}
	auto xform_bwd = transform(dec, tup, tostr);
	cout << "Backward seq: " << dec << " -> " << xform_bwd << endl;

	auto even = Detail::IncSeq<N-N/2>{}+Detail::IncSeq<N-N/2>{};
	auto xform_even = transform(even, tup, tostr);
	cout << "    Even seq: " << even << " -> " << xform_even << endl;

}
