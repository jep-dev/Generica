#include <iostream>
#include <iomanip>
#include <sstream>
#include <map>

#include "pretty.hpp"
#include "streams.tpp"
#include "utility.hpp"

template<class S, class T>
S& print_perfect(S& s, T && t) {
	using namespace Detail;
	using namespace std;
	using U = RemoveCVRef_t<T>;

	s << "Perfect: " << pretty_abbrev<T>() << "\n"
		"decltype(t): " << pretty_abbrev<decltype(t)>() << "\n"
		"ADL of t: " << pretty_abbrev(t) << "\n"
		"Value_t of t: " << pretty(Value_t<T>{}) << "\n"
		"Value_t of unqualified t: " << pretty(Value_t<U>{}) << endl;
		//"Potential Value_t: " << pretty_abbrev(Void_t<typename U::value_type>{}) << endl;

	/*s << "Perfect (" << pretty_abbrev<T>() << ")\n";
	s << "decltype(t): " << pretty_abbrev<decltype(t)>() << "\n";
	s << "ADL of t: " << pretty_abbrev(t) << "\n";
	s << "Value_t<T>: " << pretty_abbrev<Value_t<T>>() << "\n";*/
	unsigned i = 0, j = 0;
	for(auto && kv : t) {
		s << "\tdecltype(auto &&) in t: " << pretty_abbrev<decltype(kv)>() << "\n";
		s << "\tADL, fwd auto && in t: " << pretty_abbrev(forward<decltype(kv)>(kv)) << "\n";
		s << "\tADL, fwd auto && in t: " << pretty_abbrev(forward<decltype(kv)>(kv)) << "\n";
		j++;
		break;
	}
	i = 0;
	for(auto && kv : forward<T>(t)) {
		if(i++ < j) continue;
		s << "\tADL, auto && in fwd t: " << pretty_abbrev(kv) << "\n";
		s << "\tADL, fwd auto && in fwd t: " << pretty_abbrev(forward<Value_t<T>>(kv)) << "\n";
		j++;
		break;
	}
	s << "ADL of fwd t: " << pretty_abbrev(forward<T>(t)) << "\n";
	return s;
}
template<class S, class T>
S& print_concrete(S& s, std::map<T, T> && t) {
	using namespace Detail;
	using namespace std;

	s << "Concrete:\n";
	s << "ADL of t: " << pretty_abbrev(t) << "\n";
	s << "decltype(t): " << pretty_abbrev<decltype(t)>() << "\n";
	s << "decltype(fwd t): " << pretty_abbrev<decltype(forward<std::map<T,T>>(t))>() << "\n";
	unsigned i = 0, j = 0;
	for(auto && kv : t) {
		s << "\tValue_t<T>: " << pretty_abbrev<Value_t<std::map<T,T>>>() << "\n";
		s << "\tdecltype(auto &&): " << pretty_abbrev<decltype(kv)>() << "\n";
		s << "\tADL, auto &&: " << pretty_abbrev(kv) << "\n";
		j++;
		break;
	}
	return s;
}

template<class S, class... T>
S& print_types(S& s) {
	for(std::string p : {pretty_abbrev<T>()...})
		s << p << "; ";
	return s;
}
template<class S, class... T>
S& print_types(S& s, T &&... t) { return print_types<S, T...>(s); }

template<class S, class T>
S& print_valueTypes(S& s, T const& rhs) {
	using namespace Detail;
	using namespace std;
	s << INDENT << pretty_abbrev<T>();
	typedef Value_t<T> VT_T;
	typedef InnerValue_t<T> VTR_T;
	if(!is_same<T, VTR_T>::value) {
		s << ", contains ";
		if(!is_same<T, VT_T>::value) {
			s << "shallow ";
			if(!is_same<VT_T, VTR_T>::value) {
				s << pretty_abbrev<VT_T>() << " ";
			}
			s << "and deep " << pretty_abbrev<VTR_T>();
		} else {
			s << "deep " << pretty_abbrev<VTR_T>();
		}
	}
	return endl(s), s;
}
template<class S, class T, class U>
std::ostream& print_sumValueTypes(S &s, T const& t, U const& u) {
	using namespace Detail;
	typedef SumType_t<T, U> TU;
	typedef Value_t<T> VT_T;
	typedef Value_t<U> VT_U;
	typedef Value_t<TU> VT_TU;
	typedef InnerValue_t<T> VTR_T;
	typedef InnerValue_t<U> VTR_U;
	typedef InnerValue_t<VT_TU> VTR_TU;
	s << "Sums of types...\n";
	print_types<S, T, VT_T, VTR_T>(s << INDENT << "T: ") << "\n";
	print_types<S, U, VT_U, VTR_U>(s << INDENT << "T: ") << "\n";
	s << " Direct sum: " << pretty_abbrev<T>() << " + " << pretty_abbrev<U>()
			<< " = " << pretty_abbrev<TU>() << " (contains " << pretty_abbrev<VT_TU>() << ")\n"
		"Shallow sum: " << pretty_abbrev<VT_T>() << " + " << pretty_abbrev<VT_U>()
			<< " = " << pretty_abbrev<VT_TU>() << " (contains " << pretty_abbrev<VTR_TU>() << ")\n"
		"   Deep sum: " << pretty_abbrev<VTR_T>() << " + " << pretty_abbrev<VTR_U>()
				<< " = " << pretty_abbrev<VTR_TU>() << " (no deeper value_type)\n";
	return s;
}

template<class T> struct T1 { typedef T value_type; };
template<class T> using T2 = T1<T1<T>>;

//template<class T> using VAL = Detail::Value_t<T>;
// TODO ^ internal compiler error (segmentation fault)?
#define VAL(X) Detail::Value_t<X>
template<class T> using VALR = Detail::InnerValue_t<T>;
template<class S, class T> using ADD = Detail::SumType_t<S,T>;
template<class S, class T> using ADDR = Detail::SumValue_t<true, S, T>;
template<class S, class T> using SUB = Detail::DifferenceType_t<S,T>;
template<class S, class T> using SUBR = Detail::DifferenceValue_t<true, S,T>;
template<class S, class T> using MUL = Detail::ProductType_t<S,T>;
template<class S, class T> using MULR = Detail::ProductValue_t<true, S,T>;
template<class S, class T> using DIV = Detail::QuotientType_t<S,T>;
template<class S, class T> using DIVR = Detail::QuotientValue_t<true, S,T>;

template<class S, class T>
auto operator+(T1<S> const&, T1<T> const&)
		-> T1<ADD<S,T>> { return {}; }
template<class S, class T>
auto operator-(T1<S> const&, T1<T> const&)
		-> T1<SUB<S,T>> { return {}; }
template<class S, class T>
auto operator*(T1<S> const&, T1<T> const&)
		-> T1<MUL<S,T>> { return {}; }
template<class S, class T>
auto operator/(T1<S> const&, T1<T> const&)
		-> T1<DIV<S,T>> { return {}; }


int main(int argc, const char *argv[]) {
	using namespace Detail;
	using namespace std;


	typedef int I; typedef T1<I> I1; typedef T2<I> I2;
	typedef float F; typedef T1<F> F1; typedef T2<F> F2;

	static_assert(is_same<VAL(I2), I1>::value && is_same<VAL(I1), I>::value,
		"Value_t is the outermost value_type");
	static_assert(is_same<VALR<I1>, I>::value && is_same<VALR<I2>, I>::value,
		"Recursive Value_t is the innermost value_type");

	static_assert(is_same<ADD<I,F>,F>::value,
		"SumType_t is the sum of outermost value_types");
	static_assert(is_same<decltype(declval<I1>()+declval<F1>()), F1>::value,
		"T1<S> + T1<T> = T1<S+T>");
	static_assert(is_same<decltype(declval<I2>()+declval<F2>()), T1<F1>>::value,
		"T1<T1...<S>> + T1<T1...<T>> = T1<T1...<S+T>>");
		// = T1<T1...<S> + T1...<T>> = T1<T1...<S+T>>
	// T1...<S> + T1<T1...<T>> is only defined if S = T1<S'>
	static_assert(is_same<ADDR<I1,F1>, ADDR<I2,F2>>::value,
		"Recursive SumType_t is the sum of innermost value_types");

	static_assert(is_same<SUB<I,F>,F>::value,
		"DifferenceType_t is the sum of outermost value_types");
	static_assert(is_same<decltype(declval<I1>()-declval<F1>()), F1>::value,
		"T1<S> - T1<T> = T1<S-T>");
	static_assert(is_same<decltype(declval<I2>()-declval<F2>()), T1<F1>>::value,
		"T1<T1...<S>> - T1<T1...<T>> = T1<T1...<S-T>>");
	static_assert(is_same<SUBR<I1,F1>, SUBR<I2,F2>>::value,
		"Recursive DifferenceType_t is the sum of innermost value_types");

	static_assert(is_same<MUL<I,F>,F>::value,
		"ProductType_t is the sum of outermost value_types");
	static_assert(is_same<decltype(declval<I1>()*declval<F1>()), F1>::value,
		"T1<S> * T1<T> = T1<S*T>");
	static_assert(is_same<decltype(declval<I2>()*declval<F2>()), T1<F1>>::value,
		"T1<T1...<S>> * T1<T1...<T>> = T1<T1...<S*T>>");
	static_assert(is_same<MULR<I1,F1>, MULR<I2,F2>>::value,
		"Recursive ProductType_t is the sum of innermost value_types");

	static_assert(is_same<DIV<I,F>,F>::value,
		"QuotientType_t is the sum of outermost value_types");
	static_assert(is_same<decltype(declval<I1>()/declval<F1>()), F1>::value,
		"T1<S> / T1<T> = T1<S/T>");
	static_assert(is_same<decltype(declval<I2>()/declval<F2>()), T1<F1>>::value,
		"T1<T1...<S>> / T1<T1...<T>> = T1<T1...<S/T>>");
	static_assert(is_same<DIVR<I1,F1>, DIVR<I2,F2>>::value,
		"Recursive QuotientType_t is the sum of innermost value_types");


	print_sumValueTypes(cout, I1{}, F1{}) << endl;
	print_sumValueTypes(cout, I2{}, F2{}) << endl;

	typedef vector<string> Vec;
	Vec vec { "A", "B", "C" };
	print_perfect(cout, vec) << endl;

	/*typedef map<string, string> Map;
	Map ref {{"A", "a"}, {"B", "b"}, {"C", "c"}};
	//print_perfect(cout, ref) << endl;
	//auto const& cref = ref;
	//print_perfect(cout, cref) << endl;
	print_perfect(cout, Map {{"A", "a"}, {"B", "b"}, {"C", "c"}}) << endl;
	print_concrete(cout, Map {{"A", "a"}, {"B", "b"}, {"C", "c"}}) << endl;*/

}
