#include <limits>

#include <iostream>
#include <iomanip>
#include <sstream>
#include <map>
#include <vector>
#include <set>

#include "math.tpp"
#include "parse.hpp"
#include "streams.tpp"
#include "dual.tpp"

using namespace Streams;

template<class T>
void test_near(void) {
	using namespace std;

	static constexpr const char *epsilon = "\u03B5";
	T nl = .0625f, dnl = nl, nls[] = {nl, nl+dnl, nl+dnl*2, nl+dnl*3, nl+dnl*4};

	static constexpr unsigned n_nls = sizeof(nls)/sizeof(nls[0]);
	unsigned M = 16, N = 24,
			nDigitsM = numDigits(M), nDigitsN = numDigits(N),
			nTotal = N*(nDigitsN+1)+nDigitsM+1;
	auto setM = setw(nDigitsM), setN = setw(nDigitsN),
			setT = setw(nTotal), setShortT = setw(nTotal - nDigitsM - 1);

	string shades[n_nls+1] = {"", "`", "'", "\"", "*", "#"};
	auto interp = [=] (auto && i, auto && j) -> pair<T,T> {
		// return {log(T(i)/N+1), T(j)/N}; // Log-linear scale
		return {T(i)/M, T(j)/N}; // Linear scale
	};
	auto xformat = [=] (auto &s, auto x, bool lhs = false)
		-> decltype(s) { return s << (lhs ? setN : setM) << x, s; };
	auto hrule = [=] (auto &s) -> decltype(s) {
		auto f = cout.fill();
		s << setfill(' ') << setM << "" << '+'
				<< setfill('-') << setShortT << "" << '+';
		return s << setfill(f), s;
	};

	hrule(cout) << endl;
	xformat(cout, "", true) << '|';
	for(unsigned j = 0; j < N; j++)
		xformat(cout, j) << ' ';
	hrule(cout << '|' << endl) << endl;
	for(unsigned i = 0; i < M; i++) {
		xformat(cout, i, true) << '|';
		for(unsigned j = 0; j < N; j++) {
			auto uv = interp(i, j);
			cout << right;
			unsigned hits = 0;
			for(auto const& lim : nls)
				if(near(uv.first, uv.second, lim)) hits++;
			if(hits) xformat(cout, shades[hits]) << ' ';
			else if(near_zero(uv.first, nl) || near_zero(uv.second, nl))
				xformat(cout, '0') << ' ';
			else xformat(cout, shades[0]) << ' ';
		}
		endl(cout << '|');
	}
	hrule(cout) << endl;
}

template<class T>
void test_to_string(std::vector<T> vals, unsigned minPrec = 0,
		unsigned maxPrec = 3, unsigned cols = 80) {
	using std::string;
	using std::cout;
	using std::endl;
	using std::setw;

	string sep = " ";
	unsigned n = vals.size(), w = (cols - 8 - sep.length()*(n-1)) / (maxPrec + 2);
	std::string titles[] = {"Zeroes", "Digits"},
			labels[] = {"none", "post", "pre", "both"};
	std::size_t titleLens[] = {titles[0].length(), titles[1].length()};
	std::cout << titles[0] << "  " << titles[1] << std::endl;
	for(unsigned zeroes = 0; zeroes < 4; zeroes++) {
		bool fill_zeroes = zeroes & 1, show_zero = zeroes & 2;
		for(unsigned prec = minPrec; prec <= maxPrec; prec++) {
			cout << std::right << setw(titleLens[0]) << labels[zeroes] << "  "
				<< std::left << setw(titleLens[1]) << prec << "  ";
			std::string prefix;
			for(auto val : vals) {
				std::cout << prefix << std::setw(maxPrec+4) << std::right
						<< to_string(-val, prec, show_zero, fill_zeroes)
						<< ' ' << sep << ' ' << setw(maxPrec+4) << std::left
						<< to_string(val, prec, show_zero, fill_zeroes);
				prefix = sep;
			}
			endl(std::cout);
		}
		//endl(std::cout);
	}
}

int main(int argc, const char *argv[]) {
	//test_near<float>();
	using namespace std;

	test_to_string<float>({0.f, 2.f/3, 1000.f/81});
	/*cout << "No leading/trailing zeroes" << endl;
	test_to_string(false, false);
	cout << "\nOnly leading zero" << endl;
	test_to_string(true, false);
	cout << "\nOnly trailing zeroes" << endl;
	test_to_string(false, true);
	cout << "\nLeading and trailing zeroes" << endl;
	test_to_string(true, true);*/
}
