#include <iostream>
#include <sstream>
#include <vector>

#include "functional/for_each.hpp"
#include "functional/transform.hpp"
#include "pretty.hpp"
#include "streams.hpp"
#include "ttag.hpp"

template<class> struct Tag1 {};
template<class, class> struct Tag2 {};
template<class...> struct TagN {};
template<class S> struct Seq0 {}; // Same signature as Tag<S>, not usable in un-/instantiate
template<class S, S V> struct Seq1 {};
template<class S, S U, S V> struct Seq2 {};
template<class S, S U, S... V> struct SeqN {};

template<class... L, class... R, template<class...> class OP = std::common_type_t>
constexpr auto cartesian_to_tuple(Detail::Tag<Detail::Tag<L,R>...> const&, Detail::TTag<OP> = {})
	-> std::tuple<Detail::Tag<OP<L,R>>...> { return {}; }

int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Detail;

	vector<vector<string>> repls {
		{"Detail::", ""}, {"std::", ""}, {"__cxx11::", ""},
		{"unsigned char", "uchar"}, {"unsigned int", "uint"}, {"unsigned long", "ulong"},
		{"long int", "long"}
	};

	typedef tuple<Tag1<int>, Tag2<char, int>, TagN<char, int, long>,
			Seq1<int, 0>, Seq2<int, 1, 2>, SeqN<int, 3, 4, 5>> Tup;
	auto make_tag = [] (auto && t) { return Tag<RemoveCVRef_t<decltype(t)>>{}; };
	typedef decltype(transform(declval<Tup>(), make_tag)) TupTag;
	auto make_atag = [] (auto && t) { return uninstantiate(t); };
	typedef decltype(transform(declval<TupTag>(), make_atag)) TupAtag;
	auto remake_tag = [] (auto && t) { return instantiate(t); };
	typedef decltype(transform(declval<TupAtag>(), remake_tag)) TupTag2;
	static_assert(is_same<TupTag, TupTag2>::value, "Instantiate and uninstantiate must cancel");

	ostringstream oss;
	auto printer = [] (auto && s, auto & os) {
		typedef RemoveCVRef_t<decltype(s)> S;
		typedef decltype(uninstantiate(s)) As;
		os << pretty_abbrev(S{}) << "  <-->  "
			<< pretty_abbrev(As{}) << endl;
	};
	for_each(TupTag{}, printer, oss);
	cout << Streams::filter(oss.str(), repls), oss.str("");


	// TODO Tests from here forward mostly belong in tag.cpp
	typedef Tag<char, int, long> CIL;
	typedef decltype(cartesian(CIL{}, CIL{})) CIL2;
	static constexpr long nCIL2 = CIL2::size;
	static_assert(nCIL2 == 9, "CIL2 should have size |CIL|^2");
	static_assert(is_same<decltype(CIL2{}.get<1>()), Tag<Tag<char, int>>>::value
		&& is_same<decltype(CIL2{}.get<7>()), Tag<Tag<long, int>>>::value, "");

	typedef decltype(cartesian<common_type_t>(CIL{}, CIL{})) CommonCIL2;
	static constexpr size_t nCommonCIL2 = CommonCIL2::size;
	static_assert(nCommonCIL2 == 9, "CommonCIL2 should have size |CIL|^2");
	static_assert(is_same<decltype(CommonCIL2{}.get<3>()), Tag<int>>::value,
			"The common type of char and int should be int");
	static_assert(is_same<decltype(CommonCIL2{}.get<5>()), Tag<long>>::value,
			"The common type of int and long should be long");
	cout << Streams::filter(oss.str(), repls), oss.str("");

	repls.emplace_back(vector<string>{"char", "c"});
	repls.emplace_back(vector<string>{"int", "i"});
	repls.emplace_back(vector<string>{"long", "l"});
	repls.emplace_back(vector<string>{"c, ", "c,"});
	repls.emplace_back(vector<string>{"i, ", "i,"});
	repls.emplace_back(vector<string>{"l, ", "l,"});
	repls.emplace_back(vector<string>{"Tag<", "{"});
	repls.emplace_back(vector<string>{">", "}"});

	//static_assert(is_same<decltype(get<0>(CIL{})), char>::value, "");
	oss << "CIL = {" << pretty_abbrev(CIL{}) << "}" << endl;
	oss << "      [0] = {" << pretty_abbrev(get<0>(CIL{})) << "}" << endl;
	oss << "CIL x CIL = {" << pretty_abbrev(CIL2{}) << "}" << endl;
	oss << "      [0] = {" << pretty_abbrev(get<0>(CIL2{})) << "}" << endl;
	oss << "CIL x2 CIL = {" << pretty_abbrev(CommonCIL2{}) << "}" << endl;
	oss << "      [0] = {" << pretty_abbrev(get<0>(CommonCIL2{})) << "}" << endl;
	cout << Streams::filter(oss.str(), repls), oss.str("");
	/*static_assert(is_same<decltype(Detail::get<0>(CommonCIL2{})), Tag<char, int>>::value, "");
	static_assert(is_same<decltype(Detail::get<0>(CommonCIL2{})), Tag<long, int>>::value, "");*/
	//typedef decltype(cartesian_to_tuple(CIL2{})) TupleCIL2;
}
