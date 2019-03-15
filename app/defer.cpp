
//#include <utility>
#include <type_traits>
#include <string>
#include "functional/defer.hpp"
#include "functional/for_each.hpp"
#include "functional/transform.hpp"

#include <iostream>
#include <sstream>
#include "pretty.hpp"
#include "streams.hpp"


namespace Detail {

template<class S> std::string nocv_pretty(S && s)
{ return pretty_abbrev<RemoveCVRef_t<decltype(s)>>(); }

template<class OS, class... T>
OS& operator<<(OS &os, std::tuple<T...> const& t) {
	auto pfx = "(";
	Detail::for_each(t, [] (auto && x, auto & os, auto & pfx)
		{ os << pfx << x; pfx = ", "; }, os, pfx);
	return os << ')', os;
}

template<class C>
struct Counts {
	static unsigned ctors, dtors, instances;
	//static unsigned ctors = 0, dtors = 0, instances = 0;
	const unsigned instance = 0;
	const std::string label = pretty_abbrev<C>();
	template<class OS>
	friend OS& operator<<(OS &os, Counts const& c) {
		return os << c.label << '_' << c.instance, os;
	}
	Counts(void): instance(ctors++) { instances++; }
	~Counts(void) { dtors++, instances--; }
};
//template<class C> unsigned Counts<C>::ctors;
//template<class C> unsigned Counts<C>::dtors;
//template<class C> unsigned Counts<C>::instances;
template<class C> unsigned Counts<C>::ctors = 0;
template<class C> unsigned Counts<C>::dtors = 0;
template<class C> unsigned Counts<C>::instances = 0;

struct A: Counts<A> {};
struct B: Counts<B> {};
struct C: Counts<C> {};

template<class OS>
int test_tuples(OS & os) {
	using namespace std;
	//os << make_tuple(0) << endl;
	auto t0 = make_tuple(A());
	os << t0 << endl;
	//auto t = move_pop_front(t0);
	//auto t = move_pop_front(move(t0));
	/*auto && t01 = move_emplace_back(move(t0), '1');
	os << t01 << endl;
	auto t1 = move_pop_front(move(t01));
	os << t1 << endl;*/
	return 0;
}

template<class OS>
int test_defer(OS & os) {
	using namespace std;

	auto f_argc = [] (auto & s, auto &&... x) { s << sizeof...(x); };
	auto f_argt = [=] (auto & s, auto &&... x) {
		auto pfx = "{";
		for(auto str : {nocv_pretty(x)...}) s << pfx << str, pfx = ", ";
		s << '}';
	};
	auto f_argv = [] (auto & s, auto &&... x) {
		auto pfx = "{";
		for(auto str : {std::to_string(x)...}) s << pfx << str, pfx = ", ";
		s << '}';
	};


	auto arg0 = -1;
	auto arg1 = '1';
	auto arg2 = 2.0;

	// Defer composes heterogeneous callables without resorting to a cast to a function
	// pointer (prevents captures, objects with overloaded operator()) or std::function
	// (bulky due to virtualization)
	auto d = Defer<>{};

	// Lambdas given by reference; the caller must maintain referenced values
	auto d5 = d.then(f_argc, f_argt).then(f_argv) // Unary or heterogeneous variadic comp.
		// Capturing lambda given by value
		.then([&] (auto & os, auto &&...) {
			/*
			auto pre = arg0++;
			os << "arg0: " << pre << " -> " << arg0;
			return os;
			*/
			os << "arg0: " << arg0++ << " -> ", os << arg0;
		})
		.then(f_argv, f_argv);

	auto d4 = d5(os << "f_argc: #(A...) = ", arg0, arg1, arg2);
	auto d3 = d4(os << "\nf_argt: {decltype(A)...} = ", arg0, arg1, arg2);
	auto d2 = d3(os << "\nf_argv: {A...} = ", arg0, arg1, arg2);
	auto d1 = d2(os << '\n', arg0, arg1, arg2);
	auto d0 = d1(os << "\nf_argv: {A...} = ", arg0, arg1, arg2);
	auto d_0 = d0(os << "\nf_argc: #(A...) = ", arg0, arg1, arg2);

	static_assert(is_same<decltype(d_0), decltype(d)>::value, "");

	os << '\n' << pretty_abbrev<RemoveCVRef_t<decltype(d1)>>() << endl;
	return 0;
}

}

int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Detail;

	ostringstream oss;
	auto test = [&oss] (auto && label, auto && cb) {
		constexpr const char *words[][2] = {
			{"Detail::", ""}, {"basic_ostream<char>", "ostream"}
		};
		auto out = cb(oss << label);
		cout << Streams::filter(oss.str(), words);
		oss.str("");
		return out;
	};
	auto tests = make_tuple(
		make_pair("Function compositions:\n", [] (auto & os) { return test_defer(os); }),
		make_pair("Tuple operations:\n", [] (auto & os) { return test_tuples(os); }));

	int ret = 0;
	Detail::for_each(tests, [=,&ret] (auto && p) {
		if(ret) return;
		if(ret = test(p.first, p.second))
			cout << "Failed test:\n  \"" << p.first << '"' << endl;
		else cout << "Success" << endl;
	});

}
