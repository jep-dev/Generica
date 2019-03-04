#include <array>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

#include "functional/for_each.hpp"
#include "functional/transform.hpp"
#include "pretty.hpp"
#include "tag.hpp"

template<class CRTP>
struct Base {
	int value = 0;
	template<class T>
	CRTP& operator=(T && t) {
		value = std::forward<T>(t);
		return static_cast<CRTP&>(*this);
	}
};

struct u; struct u : Base<u> { using Base::operator=; };
struct u2; struct u2 : Base<u2> { using Base::operator=; };
struct v; struct v : Base<v> { using Base::operator=; };
struct w; struct w : Base<w> { using Base::operator=; };
struct x; struct x : Base<x> { using Base::operator=; };
struct y; struct y : Base<y> { using Base::operator=; };
struct z; struct z : Base<z> { using Base::operator=; };


template<class S, class T>
S& operator<<(S &s, Base<T> const& b) {
	return s << Pretty<T>() << '(' << b.value << ')';
}
template<class S, class T, class QUAL = std::true_type>
constexpr bool isSame(S && s, T && t, QUAL = {}) {
	return std::is_same<S, T>::value;
}
template<class S, class T>
constexpr bool isSame(S && s, T && t, std::false_type) {
	return std::is_same<Detail::RemoveCVRef_t<S>, Detail::RemoveCVRef_t<T>>::value;
}
template<class... C>
constexpr bool all_same(C &&... c) {
	using namespace Detail;
	typedef decltype(cartesian<std::is_same>(Tag<C...>{}, Tag<C...>{})) sames_t;
	typedef decltype(tag_value_to_seq(sames_t{})) sames_v;
	return boolAndSeq(sames_v{});
}

template<class L, class R> using Shift_t = decltype(L{} << R{});

/*struct PrintSimple {
	template<class OS>
	OS& operator()(OS & os
};*/
struct PrintShift {
	template<class I, class U, class OS>
	void operator()(I const& l, U const& r, OS &os) const {
		os << "  " << pretty<U>() << " << " << std::showpos << I::value
			<< " = " << pretty(r << l) << ';' << std::endl;
	}
};

namespace Detail {
	using E = Tag<>;
	using U = Tag<u>;
	using V = Tag<v>;
	using W = Tag<w>;
		using UV = Tag<u, v>;
		using VW = Tag<v, w>;
		using UVW = Tag<u, v, w>;
	using X = Tag<x>;
	using Y = Tag<y>;
	using Z = Tag<z>;
		using XY = Tag<x, y>;
		using YZ = Tag<y, z>;
		using XYZ = Tag<x, y, z>;
	using F = Tag<float>;
	using D = Tag<double>;
}

int main(int argc, const char *argv[]) {
	using std::cout;
	using std::endl;
	using std::pair;
	using std::string;
	using std::array;
	using std::vector;
	using std::tuple;
	using std::make_tuple;
	using namespace Detail;

	string indent = "  ", para = "\n" + indent;

	std::ostringstream oss;
	tuple<E, U, V, W, X, Y, Z> types;
	auto type_names = make_tuple("  0", "\n  U", "V", "W", "\n  X", "Y", "Z");
	auto type_strs = transform(types, [] (auto && t) {
		return pretty<RemoveCVRef_t<decltype(t)>>(); });

	oss << "Basic types:" << endl;
	for_each_zip(type_names, type_strs, [] (auto && l, auto && r, auto && os)
			{ os << l << " = " << r << "; "; }, oss);
	oss << endl;

	static_assert(isSame(E{}+E{}, E{}) && isSame(E{}+X{}, X{}) && isSame(U{}+E{}, U{}),
		"The empty tag must be the additive identity");
	oss << "Direct sums (operator+):" << para
			<< "0+0 = " << pretty(E{}+E{}) << "; "
			<< "U+0 = " << pretty(U{}+E{}) << "; "
			<< "0+X = " << pretty(E{}+X{}) << ";" << para;

	static_assert(isSame(UV{}+W{}, U{}+VW{}) && isSame(XY{}+Z{}, X{}+YZ{}),
		"Operator+ must be associative");
	oss << "(U+V)+W = U+(V+W) = " << pretty((U{}+V{})+W{}) << " (associative.)" << para;

	static_assert(isSame(V{}+U{}, reverse(UV{})),
		"Operator+ must preserve order (non-commutative)");
	oss << "U+V = " << pretty(U{}+V{}) << " != "
			"V+U = " << pretty(V{}+U{}) << " (non-commutative!)\n";

	static_assert(isSame(UV{}, U{}+V{}) && isSame(VW{}, V{}+W{})
			&& isSame(UVW{}, U{}+V{}+W{}) && isSame(XY{}, X{}+Y{})
			&& isSame(YZ{}, Y{}+Z{}) && isSame(XYZ{}, X{}+Y{}+Z{}),
		"Operator+ must be direct sum of types (treated as orthogonal)");
	oss << "Inner product (operator*):" << para
			<< "0*0 = " << pretty(E{}*E{}) << ";" << para
			<< "U*X = " << pretty(U{}*X{}) << ";" << para
			<< "(U+V) * (X+Y) = " << pretty(UV{}) << " * " << pretty(XY{})
				<< " = " << pretty(UV{}*XY{}) << ";" << para;
	static_assert(isSame(U{}*X{}+V{}*Y{}, UV{}*XY{}),
		"Operator* must be distributive over operator+");
	oss << "U*X+V*Y = (U+V)*(X+Y) = " << pretty(U{}*X{}+V{}*Y{}) << ".\n";

	static_assert(isSame(E{}^E{}, E{}) && isSame(E{}^X{}, E{}) && isSame(U{}^E{}, E{}),
		"The empty tag must be the multiplicative zero");

	auto outers = make_tuple(
		make_tuple("0 ^ X", E{}, X{}), make_tuple("U ^ 0", U{}, E{}),
		make_tuple("U ^ X", U{}, X{}), make_tuple("UV ^ X", UV{}, X{}),
		make_tuple("U ^ XY", U{}, XY{}), make_tuple("UV ^ XYZ", UV{}, XYZ{}),
		make_tuple("UVW ^ XYZ", UV{}, XYZ{}));
	auto print_outer = [] (auto && f, auto & os) {
		os << "  " << get<0>(f) << " = " << pretty(get<1>(f) ^ get<2>(f)) << ";\n"; };

	oss << "Outer product (operator^):" << endl;
	for_each(outers, print_outer, oss);
	/*oss << "Outer product (operator^, operator%):" << para
			<< "0 ^ X = " << pretty(E{}^X{}) << ";" << para
			<< "U ^ 0 = " << pretty(U{}^E{}) << ";" << para
			<< "U ^ X = " << pretty(U{}^X{}) << ";" << para
			<< "(U+V) ^ X = " << pretty(UV{} ^ X{}) << ";" << para
			<< "U ^ (X+Y) = " << pretty(U{} ^ XY{}) << ";" << para
			<< "(U+V) ^ (X+Y) = " << pretty(UV{} ^ XY{}) << "." << para
			<< "(U+V+W) ^ (X+Y) = " << pretty(UVW{} ^ XY{}) << "." << para
			<< "(U+V) ^ (X+Y+Z) = " << pretty(UV{} ^ XYZ{}) << "." << para
			<< "(U+V+W) ^ (X+Y+Z) = " << pretty(UVW{} ^ XYZ{}) << ".\n"*/
	oss << "Transposed outer product:" << para
			<< "0 % X = " << pretty(E{} % X{}) << ";" << para
			<< "U % 0 = " << pretty(U{} % E{}) << ";" << para
			<< "U % X = " << pretty(U{} % X{}) << ";" << para
			<< "(U+V) % X = " << pretty((UV{}) % X{}) << ";" << para
			<< "U % (X+Y) = " << pretty(U{} % XY{}) << ";" << para
			<< "(U+V) % (X+Y) = " << pretty(UV{} % XY{}) << ";" << para
			<< "(U+V+W) % (X+Y+Z) = " << pretty(UVW{} % XYZ{}) << ".\n"
		"Evaluation:" << para
			<< "[0] = " << pretty(eval(E{})) << ";" << para
			<< "[U] = " << pretty(eval(U{})) << ";" << para
			<< "[U+V] = " << pretty(eval(UV{})) << ";" << para
			<< "[(U+V)^(X+Y)] = " << pretty(eval(UV{}^XY{})) << ".\n";
	auto uv = eval(UV{});
	static_assert(std::is_same<decltype(access<0>(uv)), u&>::value
			&& std::is_same<decltype(access<1>(uv)), v&>::value,
		"Access<I> must emulate std::get<I>");
	oss << "Access:" << para
			<< "[U+V]_0 = " << pretty(access<0>(eval(UV{}))) << ";" << para
			<< "[U+V]_1 = " << pretty(access<1>(eval(UV{}))) << ";" << para
			<< "[(U+V)^(X+Y)]_1 = " << pretty(access<1>(eval(UV{}^XY{}))) << ";" << para
			<< "[(U+V)^(X+Y)]_1_1 = " << pretty(access<1,1>(eval(UV{}^XY{}))) << ";" << para
			<< "[U+V]_0 . 1 = " << (access<0>(uv) = 1) << ";" << para
			<< "[U+V]_1 . 2 = " << (access<1>(uv) = 2) << ".\n";

	static_assert(isSame(reverse(UVW{}), W{}+V{}+U{}),
		"Reverse must distribute over addition");
	static_assert(isSame(reverse(UV{}^XY{}), reverse(UV{})^reverse(XY{}))
			&& isSame(reverse(UV{}*XY{}), reverse(UV{})*reverse(XY{})),
		"Reverse must distribute over multiplication");
	oss << "Reversal:" << para
			<< "rev(U+V) = " << pretty(reverse(UV{})) << ";" << para
			<< "rev(U+V+W) = " << pretty(reverse(UVW{})) << ";" << para
			<< "rev((U+V)^(X+Y)) = " << pretty(reverse(UV{}^XY{})) << ".\n";
	oss << "Collapse:" << para
			<< "collapse(" << pretty(E{}) << ") = "
				<< pretty(collapse(E{})) << ";" << para
			<< "collapse(" << pretty(F{}) << ") = "
				<< pretty(collapse(F{})) << ";" << para
			<< "collapse(" << pretty(F{}+D{}) << ") = "
				<< pretty(collapse(F{}+D{})) << ";" << para
			<< "collapse(" << pretty((F{}+D{})^(D{}+F{})) << ") = "
				<< pretty(collapse((F{}+D{})^(D{}+F{}))) << ";" << para
			<< "collapse(" << pretty(Tag<decltype((F{}+D{})^(D{}+F{}))>{}) << ") = "
				<< pretty(collapse(Tag<decltype((F{}+D{})^(D{}+F{}))>{})) << ".\n";
	/*oss << "Deval:" << para
			<< "deval(" << pretty(eval(F{})) << ") = "
				<< pretty(deval(eval(F{}))) << ";" << para
			<< "deval(" << pretty(F{}) << ") = "
				<< pretty(deval(F{})) << ";" << para
			<< "deval(" << pretty(eval(U{})) << ") = "
				<< pretty(deval(eval(U{}))) << ".\n";*/
	/*static_assert(std::is_same<decltype(reverse(UV{})), decltype(rotate(UV{}))>::value,
		"Rotate must move the head type to the end of the tail");
	static_assert(std::is_same<UV, decltype(rotate(rotate(UV{})))>::value,
		"N rotations of N elements must be the identity");
	static_assert(std::is_same<decltype(rotate(rotate(UVW{}))),
			decltype(UVW{} << SeqSz<2>{})>::value,
			"N single rotations must be equivalent to one of size N");*/

	/* There are false positives to this test, but extreme template arguments like this could be
	 * a preliminary test of recursion limits - TODO create an implementation that fails. */
	static_assert(isSame(UVW{} << SeqL<9000>{}, UVW{}) &&
				isSame(UVW{} >> SeqL<9000>{}, UVW{}),
			"Forward and backward rotations must be cyclic");
	static_assert(isSame(UVW{} << SeqL<-5>{}, UVW{} >> SeqL<5>{}),
			"Negative and inverse rotation must be identical");

	auto inc_uvw = IncSeq<UVW::size, true, long>{};
	auto rep_uvw = RepeatSeq<UVW::size, long, -UVW::size/2>{};
	auto diff_uvw = inc_uvw - rep_uvw;

	oss << "Rotation:" << endl;
	Detail::for_each(IncSeq<10, true, long>{} - RepeatSeq<10, long, 5>{},
			PrintShift{}, UVW{}, oss);

	auto res = oss.str();

	string repls[][2] = {
		{" << ", " ## "}, {" >> ", " @@ "}, {"->", "-%"},
		{"Detail::", ""}, {"Tag", ""},
		{"std::tuple", "Val"}, {"float", "f"}, {"double", "d"},
		{"std::integral_constant<unsigned int, ", "Const<"},
		{"> >", ">>"}, {"<", "{"}, {">", "}"}, {"-%", "->"},
		{"##", "<<"}, {"@@", ">>"},
		{"}, ", "},_"}, {", ", ","}, {"},_","}, "}
	};
	for(auto const& it : repls) {
		std::size_t pos;
		while((pos = res.find(it[0])) != std::string::npos)
			res.replace(pos, it[0].length(), it[1]);
	}
	cout << res << endl;

}
