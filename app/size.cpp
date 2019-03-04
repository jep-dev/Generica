#include <fstream>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <vector>

#include "pretty.hpp"
#include "utility.hpp"
#include "size.hpp"

/////////////////////////////////// Arrays ///////////////////////////////////
template<class T, unsigned N = 0> const char *whichSize(T const (&) [N]) { return "deduced const&"; }
template<class T, unsigned N = 0> const char *whichSize(T (&) [N]) { return "deduced &"; }
template<class T, unsigned N = 0> const char *whichSize(T (&&) [N]) { return "deduced"; }
// TODO Why are these specs necessary? When N = 0, fallbacks are used otherwise.
template<class T> const char* whichSize(T const (&) [0]) { return "deduced const&"; }
template<class T> const char* whichSize(T (&&) [0]) { return "deduced &&"; }
template<class T> const char* whichSize(T (&) [0]) { return "deduced &"; }

/////////////////////////////////// Length ///////////////////////////////////
/** Proof of concept for HasLength */
template<class T> std::enable_if_t<HasLength<Detail::RemoveCVRef_t<T>>::value, const char*>
	whichSize(T && t) { return "length"; }
template<class T> std::enable_if_t<HasLength<T>::value, const char*>
	whichSize(T & t) { return "length"; }

///////////////////////////////////// Size ///////////////////////////////////
/** Proof of concept for HasSize; excludes types with length to remove ambiguity */
template<class T> std::enable_if_t<HasSize<T>::value && !HasLength<T>::value, const char*>
	whichSize(T && t) { return "size"; }

///////////////////////////////////// Seek* //////////////////////////////////
template<class T> std::enable_if_t<HasTellpSeekp<T>::value, const char*>
	whichSize(T & t) { return t.tellp(), t.seekp(0, std::ios::end), "seekp"; }
template<class T> std::enable_if_t<HasTellgSeekg<T>::value, const char*>
	whichSize(T & t) { return t.tellg(), t.seekg(0, std::ios::end), "seekg"; }
template<class T> std::enable_if_t<HasTellpSeekp<T>::value, const char*>
	whichSize(T && t) { return t.tellp(), t.seekp(0, std::ios::end), "seekp"; }
template<class T> std::enable_if_t<HasTellgSeekg<T>::value, const char*>
	whichSize(T && t) { return t.tellg(), t.seekg(0, std::ios::end), "seekg"; }

/////////////////////////////////// Fallbacks ////////////////////////////////
/** Fallback in case none are implemented; comment out to require one of the others */
template<class T> std::enable_if_t<!std::is_array<T>::value
		&& !HasTellpSeekp<T>::value && !HasTellgSeekg<T>::value
		&& !HasSize<T>::value && !HasLength<T>::value, const char*>
	whichSize(T & t) { return "fallback"; }
template<class T> std::enable_if_t<!std::is_array<T>::value
		&& !HasTellpSeekp<T>::value && !HasTellgSeekg<T>::value
		&& !HasSize<T>::value && !HasLength<T>::value, const char*>
	whichSize(T const& t) { return "fallback"; }
template<class T> std::enable_if_t<!std::is_array<T>::value
		&& !HasTellpSeekp<T>::value && !HasTellgSeekg<T>::value
		&& !HasSize<T>::value && !HasLength<T>::value, const char*>
	whichSize(T && t) { return "fallback"; }

struct A { constexpr std::size_t length(void) const { return 0; } };
struct B {
	constexpr std::size_t tellp(void) const { return 0; }
	constexpr std::size_t seekp(int, const std::ios::seek_dir & = std::ios::end) const { return 0; }
};
struct C {
	constexpr std::size_t tellg(void) const { return 0; }
	constexpr std::size_t seekg(int, const std::ios::seekdir& = std::ios::end) const { return 0; }
};
struct D { constexpr std::size_t size(void) const { return 0; } };
struct AB : A, B {};
struct CD : C, D {};
struct BC : B, C {};
struct AD : A, D {};
struct Z {};

struct Printer {
	unsigned nameCol = 16, whichCol = 16, sizeCol = 4;
	const char *pad = "  ";
	template<class S>
	S& header(S &s) {
		return s << std::setw(nameCol) << "Description" << pad
				<< std::setw(whichCol) << "Specialization" << pad
				<< std::setw(sizeCol) << "Size" << pad
				<< "(Real type)" << std::endl, s;
	}
	template<class S, class T>
	S& operator()(S& s, unsigned line, std::string const& name, T && t) {
		std::string realName = pretty_abbrev<T>();
		return s << std::setw(nameCol) << ((name.length() && name != realName) ? name : "") << pad
			<< std::setw(whichCol) << whichSize(std::forward<T>(t)) << pad
			<< std::setw(sizeCol) << getSize(std::forward<T>(t)) << pad
			<< "(" << realName << ")" << std::endl, s;
	}
};


template<class S, class T, unsigned N>
S& dispatch(S& s, T const (&) [N]) { return s << __LINE__ << std::endl, s; }
template<class S, class T>
S& dispatch(S& s, T && t) { auto const& ct = t; return dispatch(s, ct); }
int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Detail;

	string str = "Content in string";

	cout << "In file " << __FILE__ << "...\n";

	ostringstream out;

	// TODO determine why using 'arr[] = {}' (or E = int[0]) eliminates the template
	// specialization for arrays
	static constexpr unsigned N = 5;
	typedef int X;
	typedef const int Y;
	typedef X E[N];
	typedef add_const_t<E> EC;
	//typedef const int EC[N];

	E arr = {};
	EC carr = {};
	vector<int> vec = {0, 1, 2};
	ostringstream oss("Hello world");
	istringstream iss("Goodnight moon");
	ostream &os = oss;
	istream &is = iss;

	typedef add_lvalue_reference_t<E> EL;
	typedef add_rvalue_reference_t<E> ER;
	typedef remove_reference_t<EL> NOREF_EL;
	typedef remove_reference_t<ER> NOREF_ER;

	typedef add_lvalue_reference_t<add_const_t<E>> ECL;
	typedef add_const_t<add_rvalue_reference_t<ER>> ECR;
	// same as add_rvalue_reference_t<add_const_t<ER>>

	/*typedef add_const_t<remove_reference_t<ECL>> NOREF_ECL;
	typedef add_const_t<remove_reference_t<ECR>> NOREF_ECR;*/
	typedef remove_reference_t<ECL> NOREF_ECL;
	typedef remove_reference_t<ECR> NOREF_ECR;
	typedef remove_const_t<EC> NOC_EC;
	typedef remove_const_t<ECL> NOC_ECL;
	typedef remove_const_t<ECR> NOC_ECR;

	typedef remove_reference_t<NOC_ECL> NOCREF_ECL;
	typedef remove_reference_t<NOC_ECR> NOCREF_ECR;


	cout << "Legend: C = const, L = lvalue reference, R = rvalue reference\n" << endl;
	Printer printSize;
	printSize.header(cout);
	printSize(cout, __LINE__, "A (string-like)", A{});           // length
	printSize(cout, __LINE__, "string", str);
	printSize(cout, __LINE__, "string", std::move(str));
	printSize(cout, __LINE__, "B (ostream-like)", B{});          // seekp/tellp
	printSize(cout, __LINE__, "ostringstream", oss);             // fallback (!) TODO
	printSize(cout, __LINE__, "ostream&", os);
	printSize(cout, __LINE__, "C (istream-like)", C{});          // seekg/tellg
	printSize(cout, __LINE__, "istringstream", iss);             // fallback (!) TODO
	printSize(cout, __LINE__, "istream&", is);
	//printSize(cout, __LINE__, "istringstream", std::move(iss));  // fallback (!) TODO

	printSize(cout, __LINE__, "D (vector-like)", D{});           // size
	printSize(cout, __LINE__, "vector", vec);                    // size
	printSize(cout, __LINE__, "vector", std::move(vec));         // size

	printSize(cout, __LINE__, "E", E{});                         // deduced &&
	printSize(cout, __LINE__, "EC = E+C", EC{});                 // deduced const&
	printSize(cout, __LINE__, "NOC_EC = EC-C", NOC_EC{});        // deduced
	//                                                           // deduced &
	printSize(cout, __LINE__, "EL = E+L", (EL) arr);
	printSize(cout, __LINE__, "EL - L", NOREF_EL{});
	printSize(cout, __LINE__, "ECL = EL+C", (ECL) arr);
	printSize(cout, __LINE__, "ECL-C", (NOC_ECL)arr);
	//                                                           // deduced &&
	printSize(cout, __LINE__, "ECL-L", NOREF_ECL{});
	printSize(cout, __LINE__, "ECL-C-L", NOCREF_ECL{});

	//                                                           // deduced &&
	printSize(cout, __LINE__, "ER = E+R", ER{});
	printSize(cout, __LINE__, "ER - R", NOREF_ER{});
	printSize(cout, __LINE__, "ECR = ER+C", ECR{});
	printSize(cout, __LINE__, "ECR-C", NOC_ECR{});
	printSize(cout, __LINE__, "ECR-R", NOREF_ECR{});
	printSize(cout, __LINE__, "ECR-C-R", NOCREF_ECR{});
	cout << endl;
	cout << Pretty<E>() << " -> " << Pretty<Detail::RemoveCVRef_t<E>>() << endl;
	cout << Pretty<EC>() << " -> " << Pretty<Detail::RemoveCVRef_t<EC>>() << endl;
	cout << Pretty<EL>() << " -> " << Pretty<Detail::RemoveCVRef_t<EL>>() << endl;
	cout << Pretty<ER>() << " -> " << Pretty<Detail::RemoveCVRef_t<ER>>() << endl;
	cout << Pretty<ECL>() << " -> " << Pretty<Detail::RemoveCVRef_t<ECL>>() << endl;
	cout << Pretty<ECR>() << " -> " << Pretty<Detail::RemoveCVRef_t<ECR>>() << endl;
	/*cout << dispatch(cout, E{}) << endl;
	cout << dispatch(cout, (EC) arr) << endl;
	cout << dispatch(cout, (EL) arr) << endl;
	cout << dispatch(cout, ER{}) << endl;
	cout << dispatch(cout, (ECL) arr) << endl;*/

	dispatch(cout, E{});
	dispatch(cout, EC{});
	dispatch(cout, (EL) arr);
	dispatch(cout, ER{});
	dispatch(cout, (ECL) arr);
	//cout << dispatch(cout, ECR{}) << endl;
	/*cout << "E + C = " << Pretty<EC>()
			<< " or " << Pretty<add_const_t<E>>() << endl;
	cout << "E + L = " << Pretty<E&>()
			<< " or " << Pretty<add_lvalue_reference_t<E>>() << endl;
	cout << "E + R = " << Pretty<E&&>()
			<< " or " << Pretty<add_rvalue_reference_t<E>>() << endl;
	cout << "E + C + L = " << Pretty<E const&>()
			<< " or " << Pretty<add_lvalue_reference_t<add_const_t<E>>>() << endl;
	cout << "E + C + R = " << Pretty<E const &&>()
			<< " or " << Pretty<add_rvalue_reference_t<add_const_t<E>>>() << endl;
	cout << "E + L + C = " << Pretty<E const&>()
			// add_const_t<EL> = EL
			<< " or " << Pretty<add_lvalue_reference_t<add_const_t<remove_reference_t<EL>>>>()
			<< endl;
	cout << "E + R + C = " << Pretty<E const &&>()
			<< " or " << Pretty<add_const_t<add_rvalue_reference_t<E>>>() << endl;
	cout << Pretty<add_lvalue_reference_t<add_rvalue_reference_t<E>>>() << " / "
			<< Pretty<add_rvalue_reference_t<add_lvalue_reference_t<E>>>() << endl;
	cout << Pretty<add_lvalue_reference_t<add_rvalue_reference_t<EC>>>() << " / "
			<< Pretty<add_rvalue_reference_t<add_lvalue_reference_t<EC>>>() << endl;
	cout << Pretty<add_const_t<add_lvalue_reference_t<add_rvalue_reference_t<E>>>>() << " / "
			<< Pretty<add_const_t<add_rvalue_reference_t<add_lvalue_reference_t<E>>>>() << endl;*/
	/*cout << boolalpha;
	cout << Pretty<int[]>() << " : " << HasDimension<int[]>::value << endl;
	cout << Pretty<int (&) []>() << " : " << HasDimension<int (&) []>::value << endl;
	cout << Pretty<int const (&) []>() << " : " << HasDimension<int const (&) []>::value << endl;
	cout << Pretty<int (&&) []>() << " : " << HasDimension<int (&&) []>::value << endl;

	static_assert(std::is_same<const XA, YA>::value, "Inner/outer const");
	static_assert(std::is_same<std::remove_reference_t<std::add_const_t<XA>>, YA>::value, "");
	static_assert(std::is_same<std::remove_const_t<YA>, XA>::value, "");
	//static_assert(std::is_same<YA&, std::add_const_t<XA>>::value, "");
	cout << Pretty<std::add_const_t<XA>>() << " vs " << Pretty<YA&>() << endl;*/


	printSize(cout, __LINE__, "Z (empty)", Z());                 // fallback (ok)
	printSize(cout, __LINE__, "", pair<int,int>());              // fallback (ok)
	cout << out.str();

}
