#include <iostream>
//#include <iomanip>
#include <sstream>

#include "binomial.hpp"
#include "dual.hpp"
#include "functional/for_each.hpp"
#include "functional/transform.hpp"
#include "streams.hpp"
#include "pretty.hpp"
#include "tag.hpp"
#include "utility.tpp"

using namespace Detail;

template<class S, class T, T V>
S& operator<<(S& os, std::integral_constant<T, V>) {
	return os << "[T=" << to_string(V) << "]", os;
}

template<class T, T... TN>
std::string to_string(Seq<T, TN...> const& seq) {
	std::ostringstream oss;
	oss << seq;
	return "{" + oss.str() + "}";
}

template<class T, std::size_t N>
std::string substitute(std::string const& str, const T (&t) [N][2]) {
	std::string out = str;
	//for(auto const& p : {t...}) {
	for(auto const& p : t) {
		auto pos = out.find(p[0]);
		while((pos = out.find(p[0])) != std::string::npos)
			out.replace(pos++, p[0].length(), p[1]);
	}
	return out;
}

template<class... S, class... T>
std::string to_string(Tag<S...>, T &&... t) {
	std::string out, strs[] = {Pretty<S>()...}, pre = "{", post = "}";
	for(auto const& str : strs)
		out += pre + str, pre = ", ";
	out += post;
	return substitute(out, std::forward<T>(t)...);
}


template<class S>
S& testBinomial(S &s) { return s; }

template<class S, Size N, Size... I>
S& testBinomial(S &os, SeqSz<N, I...> = {}) {
	os << N << ". ";
	os << Binomial<N>() << std::endl;
	return testBinomial<S, I...>(os);
}

template<class S, S V>
struct Integral: std::integral_constant<S, V> {
	using value_type = S;
	S value = V;
	constexpr operator S(void) const { return V; }
	constexpr S operator()(void) const { return V; }
	constexpr Integral(void): value(V) {}
};

int main(int argc, const char *argv[]) {
	// The executable size is independent of N, which should mean that the sequences
	// all disappear in abstract elimination.
	static constexpr unsigned N = 10, I = 4;
	typedef RepeatSeq<N, unsigned, 1> ones_t;                 // {1, 1, ..., 1, 1}
	typedef SumSeq<Value_t<ones_t>> sum_t;         // 10
	typedef PartialSumSeq<ones_t> inc_t;                      // {1, 2, ..., 9, 10}
	typedef ProductSeq<Value_t<inc_t>> product_t;  // 3628800
	typedef PartialSumSeq<Value_t<inc_t>> inc2_t;  // {1, 3, ..., 45, 55}
	auto deduced = to_seq(SeqU<1>{}, SeqC<'2'>{}, SeqL<3l>{});
	std::string subs[][2] = {{"Detail::", ""}, {"std::", ""}};
	// noDetail[] = {"Detail::", ""}, noStd[] = {"std::", ""};
	// auto toStr = [=] (auto a) { return to_string(a, noDetail, noStd); };

	typedef Tag<int, char, int, char> tag_t;
	static constexpr std::size_t find0 = Find<char, tag_t>::value,
			find1 = Find<char, tag_t, find0+1>::value;
	std::ostringstream oss;
	oss << "Type sequence with repetition: "
			// << toStr(tag_t{}) << ":\n" // {int, char, int, char}:
			<< to_string(tag_t{}, subs) << ":\n" // {int, char, int, char}:
		"  - For Nth=" << Pretty<char>() << ", N=0, "
			"N' = " << find0 << "\n" // - For Nth=char, N=0, N' = 1
		"  - For Nth=" << Pretty<char>() << ", N=" << find0 << ", "
			"N' = " << find1 << "\n\n"; // - For Nth=char, N=1, N' = 3

	oss << "10 1's: " << to_string(ones_t{}) << "\n"
		"  - Sum: " << to_string(sum_t{}) << "\n"
	"  - Partial sum: " << to_string(inc_t{}) << "\n"
	"    - Evens: " << to_string(inc_t{}+inc_t{}) << "\n"
	"    - Product: " << to_string(product_t{}) << "\n"
	"    - Partial sum: " << to_string(inc2_t{}) << "\n"
	"    - Nth for N=" << I << ": " << get<I>(inc2_t{}) << "\n\n";

	oss << "Decompose<91> = " << decompose<int, 91, 7>() << '\n' << std::endl;

	oss << "Fibonacci<10> = " << Fibonacci<10>::value << '\n' << std::endl;

	typedef Integral<int, 5> ce_t;
	typedef std::integral_constant<int, ce_t{}.value> cei_t; // or...
	// typedef std::integral_constant<int, ce_t{}()> cei_t; // or...
	// typedef Integral<int, ce_t{}()> cei_t; // etc.

	oss << Pretty<ce_t>() << ": " << Pretty<cei_t>() << "\n\n";
	// This is just an example of constexpr 'members' - save for later


	oss << "Binomial coefficients: " << std::endl;
	testBinomial<std::ostream&, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9>(oss) << std::endl;

	auto printer = [](auto && v, auto & os, auto && delim) { os << v << delim; };
	auto vprinter = [](auto && v, auto & os, auto && delim) { os << v.value << delim; };
	auto tpretty = [](auto && v) -> std::string { return pretty<RemoveCVRef_t<decltype(v)>>(); };
	auto tprinter = [&](auto && v, auto & os) { oss << tpretty(v); };

	auto to_for_each = [] (auto && v, auto && f, auto &&... t)
		{ for_each(v, f, t...); };
	auto to_transform = [] (auto && v, auto && f, auto &&... t)
		{ return transform(v, f, t...); };


	IncSeq<10> inc;
	IncSeq<5, true, std::size_t> keys;

	for_each(seq_to_tuple_seq(inc), vprinter, oss << "Inc = ", ", ");
	oss << "\n  Pop front = " << pop_front(inc)
		<< "\n  Pop back = " << pop_back(inc);

	auto get_head = [] (auto && k, auto && v) { return head<k>(v); };
	auto get_tail = [] (auto && k, auto && v) { return tail<k>(v); };
	auto heads = transform(keys, get_head, inc);
	auto tails = transform(keys, get_tail, inc);

	/*auto to_tuple_seq = [] (auto && v) { return seq_to_tuple_seq(v); };
	auto heads_tup_tup_seq = transform(heads_tup_seq, to_tuple_seq);*/

	for_each(heads, printer, oss << "\n  Heads = ", "; ");
	for_each(tails, printer, oss << "\n  Tails = ", "; ");

	std::cout << Streams::filter(oss.str(), subs) << std::endl;

	typedef decltype(seq_to_tag_seq(keys)) A;
	typedef decltype(A{} << SeqL<1>{}) B;
	typedef decltype(reverse(A{})) C;
	typedef Tag<A, B, C> ABC;
	auto TABC = transform2(keys, reverse(keys), [] (auto && a, auto && c) {
		return Seq<std::common_type_t<decltype(a.value), decltype(c.value)>,
			a.value * c.value>{};
	});
	/*typedef decltype(tag_to_tuple(A{})) TA;
	typedef decltype(tag_to_tuple(A{})) TB;
	typedef decltype(tag_to_tuple(A{})) TC;
	typedef std::tuple<TA, TB, TC> TABC;*/


	auto get_break_and = [] (auto && f, auto & os) {
		return [&] (auto &&... t) { os << "\n  "; f(t...); };
	};
	for_each(ABC{}, get_break_and(tprinter, oss), oss << "\nPrint T in ABC:");

	endl(std::cout);

	std::cout << Streams::filter(oss.str(), subs) << std::endl;
}
