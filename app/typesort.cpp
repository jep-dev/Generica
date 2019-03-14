#include <iostream>
#include <sstream>
#include <vector>

#include "pretty.hpp"
#include "streams.hpp"
#include "typesort.hpp"

namespace Detail {

enum EDim : unsigned char {
	e_width = 0, e_height, e_depth, en_dims
};

template<unsigned W> struct Width;
template<unsigned H> struct Height;
template<unsigned D> struct Depth;

template<unsigned W> struct Width: Comparable<Width<W>, EDim, e_width> {};
template<unsigned H> struct Height: Comparable<Height<H>, EDim, e_height> {};
template<unsigned D> struct Depth: Comparable<Depth<D>, EDim, e_depth> {};

}

/*template<class S, class V>
std::vector<std::pair<std::string, unsigned>> instances(S const& str, V const& words) {
	std::vector<std::pair<std::string, unsigned>> out;
	std::string s = str;
	if(!s.length()) std::cout << "Empty input string" << std::endl;
	for(auto const& word : words) {
		auto const& src = word.first;
		auto const& dest = word.second;
		if(!src.length()) std::cout << "Empty src string" << std::endl;
		unsigned count = 0;
		auto pos = s.find(src);
		while(pos != std::string::npos) {
			count++;
			pos = s.find(src, pos + 1);
		}
		out.emplace_back(src, count);
	}
	return out;
}*/

namespace Detail {
template<class S = void, class... T> constexpr bool all_same(Tag<S, T...> const&) { return true; }
template<class R, class S, class... T> constexpr bool all_same(Tag<R, S, T...> const&)
	{ return std::is_same<R, S>::value && all_same(Tag<S, T...>{}); }
template<class... T> constexpr bool all_same(void) { return all_same(Tag<T...>{}); }
}

template<class... T> auto pre = pretty_abbrev<T...>();

int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Detail;

	typedef Width<1024> W;
	typedef Height<768> H;
	typedef Depth<24> D;
	W w; H h; D d;

	static_assert(w < h && h < d && w < d, "e_width < e_height < e_depth");
	static_assert(d > h && h > w && d > w, "e_depth > e_height > e_width");


	//auto pre = [] (auto && x) -> string { return pretty_abbrev<decltype(x)>(); };

	ostringstream oss;
	string repls[][2] = { {pre<W>, "W"}, {pre<H>, "H"}, {pre<D>, "D"}, {"Detail::", ""} };

	typedef Tag<> T0;              oss << " 0 = " << pre<T0> << '\n';
	typedef Insert_t<W, T0> TW;    oss << "  + W = " << pre<TW> << '\n';
	typedef Insert_t<H, TW> TWH;   oss << "    + H = " << pre<TWH> << '\n';
	typedef Insert_t<D, TWH> TWHD; oss << "      + D = " << pre<TWHD> << '\n';
	typedef Insert_t<D, TW> TWD;   oss << "    + D = " << pre<TWD> << '\n';
	typedef Insert_t<H, TWD> TWDH; oss << "      + H = " << pre<TWDH> << '\n';
	typedef Insert_t<H, T0> TH;    oss << "  + H = " << pre<TH> << '\n';
	typedef Insert_t<W, TH> THW;   oss << "    + W = " << pre<THW> << '\n';
	typedef Insert_t<D, THW> THWD; oss << "      + D = " << pre<THWD> << '\n';
	typedef Insert_t<D, TH> THD;   oss << "    + D = " << pre<THD> << '\n';
	typedef Insert_t<W, THD> THDW; oss << "      + W = " << pre<THDW> << '\n';
	typedef Insert_t<D, T0> TD;    oss << "  + D = " << pre<TD> << '\n';
	typedef Insert_t<W, TD> TDW;   oss << "    + W = " << pre<TDW> << '\n';
	typedef Insert_t<H, TDW> TDWH; oss << "      + H = " << pre<TDWH> << '\n';
	typedef Insert_t<H, TD> TDH;   oss << "    + H = " << pre<TDH> << '\n';
	typedef Insert_t<W, TDH> TDHW; oss << "      + W = " << pre<TDHW> << '\n';

	/* Obviously with this success my first thought is how to translate/apply it to algebra.
	 * In addition to the quadratic form giving i^2 = j^2 = k^2 = -1, E^2 = ... = 0, etc.
	 * you will have [e_0 = 1] < [i] < [j] < [k] < ... < [E] < ... < [e_N]
	 *   - Not (i<j), but ([i]<[j]) or ([e_I]<[e_J]) via (I<J), the indices/keys.
	 * Most likely the keys will come from an enum like they do here, only something like
	 * enum EUnit { e_0 = 0, e_i, ..., e_Ej, e_Ek, en_units };
	 * The issue I noticed before is that I would need to literally enumerate the units of the
	 * binomial expansion, which is both limiting and antithetic to generic programming.
	 * I could enumerate the generators, e.g. EUnit { e_i = 0, e_j, e_E, e_F, ..., en_units }
	 * producing powers 2^e_i = 1, 2^e_j = 2, 2^e_E = 4, ... as well as sums of any of these,
	 * representing products of any number of units. Individual contributions can be recovered
	 * from the sum.
	 */

	static_assert(all_same<TWHD, TWDH, THWD, THDW, TDWH, TDHW>()
		&& is_same<TWH, THW>::value && is_same<TWD, TDW>::value && is_same<THD, TDH>::value,
		"Insertion sort (or any sort) should be commutative");

	cout << Streams::filter(oss.str(), repls) << endl;
	oss.str("");

	/*static_assert(is_same<decltype(head(SeqSz<2>{}, TDHW{})), TDH>::value, "");
	static_assert(is_same<decltype(tail(SeqSz<2>{}, TDHW{})), THW>::value, "");
	static_assert(is_same<decltype(tail(SeqSz<1>, head(SeqSz<1>{} TDHW{}))), TD>::value, "");*/
}
