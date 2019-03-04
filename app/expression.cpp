#include <map>
#include <iostream>
#include <sstream>

#include "expression.tpp"
#include "pretty.hpp"
#include "pretty.hpp"
#include "math.tpp"
#include "streams.hpp"
#include "streams.tpp"
#include "utility.hpp"

template<class S, class... T>
std::ostream& print_derivatives(std::ostream &os, Expressions::SymbolPtr<S> const& s, T &&... t) {
	using namespace std;
	using namespace Expressions;
	for(auto it : {t...})
		os << "d/d" << it << " " << *s << " = "
			<< *(s -> derive(it)) << " = "
			<< *(simplify(s -> derive(it))) << endl;
			// << *(s -> derive(it) -> simplify()) << endl;
	return os;
}

int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Detail;
	using namespace Expressions;

	using T = float;
	using DQ = DualQuaternion<T>;
	DQ a{1}, b{1_e}, c{1_E};
	//DQ const_dual = trans_rotation<T>(1, 0, 0, M_PI/2, 0, 1, 0);
	DQ const_dual = rotation<T>(M_PI/2, 0, 1, 0);
	SymbolPtr<DQ> u = mkVar<DQ>('u', 1_E),
		v = mkVar<DQ>('v', 1_e),
		w = mkVar<DQ>('w', 1, 0, 0, 0, 0, 1, 0, 0);

	/*
	// Variable/value construction is almost entirely type agnostic:
	// 1. Arithmetic vargs that convert to dest::value_type (if it exists)
	SymbolPtr<DQ> u = mkVar<DQ>('u', 1), // -> {1, 0, ..., 0}
	// 2. 1 arg (non-arithmetic, so no ambiguity) that converts to dest
		v = mkVar<DQ>('v', 1_i),         // -> (DQ) (1_i) ~= {0, 1, 0, ..., 0}
	// 3. 1 (const) lvalue of dest's type, so copy-construct
		w = mkVal<DQ>(const_dual),
	// 4. 1 rvalue of dest's type, so move-construct
		x = mkVal<DQ>(1_e + 1_I);
	vector<SymbolPtr<DQ>> symbols = {
		u, mkNeg(u), mkConj(u), mkExp(u), mkLog(u), //1/u,
		v, mkNeg(v), mkConj(v), mkExp(v), mkLog(v), //1/u,
		w, mkNeg(w), mkConj(w), mkExp(w), mkLog(w), //1/u,
		// x, mkNeg(x), mkConj(x), mkExp(x), mkLog(x), //1/x
		mkSum(x, x),
		mkSum(u, u), mkSum(u, x), mkSum(x, u),
		mkSum(u, v),
		mkDifference(x, x),
		mkDifference(u, u), mkDifference(u, x), mkDifference(x, u),
		mkDifference(u, v),
	};
	unsigned prec = 2, N = symbols.size();
	vector<string> lines(N+1, "");
	lines[0] = "Expression";
	unsigned col0 = lines[0].length();
	ostringstream oss;
	for(unsigned i = 0; i < N; i++) {
		oss << to_string(symbols[i], prec, false);
		lines[i+1] += oss.str();
		col0 = std::max<unsigned>(lines[i+1].length(), col0);
		oss.str("");
	}
	for(auto & line : lines) {
		auto dlen = std::max<unsigned>(0, col0 - line.length());
		line = string(dlen, ' ') + line + "  ";
	}

	lines[0] += "Reduced";
	for(unsigned i = 0; i < N; i++) {
		oss << to_string(reduce(symbols[i]), prec, false);
		lines[i+1] += oss.str();
		oss.str("");
	}
	Streams::level_insert(lines, "  ");
	for(char ch : {'u', 'v'}) {
		string label = string("f(") + ch + ")";
		unsigned len = label.length();
		string check(len, ' ');
		check[len-2] = '*';
		auto labelLen = label.length();
		lines[0] += label;
		for(unsigned i = 0; i < N; i++) {
			if(symbols[i] -> varies(ch))
				lines[i+1] += check;
		}
		Streams::level_insert(lines, "  ");
		lines[0] += string("d/d") + ch;
		for(unsigned i = 0; i < N; i++) {
			lines[i+1] += to_string(reduce(reduce(symbols[i]) -> derive(ch)), prec);
		}
		Streams::level_insert(lines, "  ");
	}

	cout << "Functions, reductions, dependence on "
			"and derivation with respect to variables" << endl;
	cout << "  Note that exponent is written exp "
			"and that e is an operator like i.\n" << endl;
	for(auto const& line : lines)
		cout << line << endl;
	*/


}
