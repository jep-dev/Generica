#include "dual.tpp"
#include "point.hpp"
#include "bezier.hpp"
#include "math.tpp"
#include "streams.tpp"
#include "size.hpp"
#include <iostream>

template<class T>
T linearTransfer(T f){ return f; };

// log(1)=0, log(e)=1
template<class T>
T logTransfer(T f) { return log(1 + M_E * f); };

std::string replace_all(std::string const& s, char from, char to) {
	std::string out;
	for(auto const& c : s)
		if(c == from)
			out += to;
		else out += c;
	return out;
}
std::string replace_all(std::string const& s,
		std::string const& from, std::string const& to,
		unsigned lenFrom = 0, unsigned lenTo = 0) {
	using namespace std;
	string out = s;
	if(!lenFrom) lenFrom = from.length();
	if(!lenTo) lenTo = to.length();
	for(auto pos = out.find(from); pos != string::npos;) {
		out.replace(pos, lenFrom, to, lenTo);
		pos = out.find(from, pos+1);
	}
	return out;
}

int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Streams;
	typedef float T;
	typedef DualQuaternion<T> Dq;
	typedef Point<T> Pt;

	static constexpr unsigned NJ = 5, NI = 5, BR = NJ, prec = 2, valLen = prec + 1;

	unsigned cols = 80;
	if(argc > 1) {
		istringstream iss(argv[1]);
		unsigned colsTemp = 0;
		iss >> colsTemp;
		if(!iss.fail() && colsTemp) {
			cols = colsTemp;
		}
	}


	T p22[][2] = {
		{1, 0},
		{0, 1}
	}, p33[][3] = {
		{1, 0, 0},
		{0, 1, 0},
		{0, 0, 1}
	}, p44[][4] = {
		{1, 0, 0, 0},
		{0, 1, 0, 0},
		{0, 0, 1, 0},
		{0, 0, 0, 1},
	}, p55[][5] = {
		{1, 0, 0, 0, 0},
		{0, 1, 0, 0, 0},
		{0, 0, 1, 0, 0},
		{0, 0, 0, 1, 0},
		{0, 0, 0, 0, 1},
	};

	/*auto abbrev = [] (T fVal, T fMin = 0, T fMax = 1,
			T (*transfer)(T  = linearTransfer,
			std::string chars = "0123456789") -> std::string {
		auto ft = fminf(1, fmaxf(0, transfer((fVal - fMin) / (fMax - fMin))));
		unsigned uf = (chars.length() - 1)*ft;
		return {chars[uf]};
		// uf = (len^2 - 1)*ft, uMaj = uf / len, uMin = uf % len
	};

	unsigned W = 10, H = 10;
	T divisors[] = {1,1,1};
	pair<string, vector<string>> rows[] = {
		{"Linear", vector<string>(H)},
		{"Quadratic", vector<string>(H)},
		{"Cubic", vector<string>(H)}
		//{"Quartic", vector<string>(H)}
	};
	for(unsigned i = 0; i < W; i++) {
		T s = T(i)/(H-1);
		for(unsigned j = 0; j < H; j++) {
			T t = T(j)/(W-1);
			rows[0].second[j] += abbrev(linear(p22[0], p22[1],
					s/divisors[0], t/divisors[0]));
			//rows[0].second[j] += abbrev(linear(p22[0], p22[1],
			//		s/divisors[0], t/divisors[0]));
			rows[1].second[j] += abbrev(quadratic(p33[0], p33[1], p33[2],
					s/divisors[1], t/divisors[1]));
			rows[2].second[j] += abbrev(cubic(p44[0], p44[1], p44[2], p44[3],
					s/divisors[2], t/divisors[2]));
		}
		for(auto & r : rows)
			level_insert(r.second, " ");
	}
	for(auto const& r : rows) {
		cout << r.first << endl;
		for(auto const& row : r.second)
			cout << row << endl;
		cout << endl;
	}*/

	//cout << bezier<2,2>(p33, 0, 0) << endl;
	static constexpr unsigned MD = 4, ND = MD;
	vector<string> rows(MD, "");
	std::string border = "|", padding = " ", margin = "  ";

	T bases[NI][NJ][MD][ND];
	auto & basis0 = bases[0][0];

	cout << "Binomial basis: " << endl;
	binomialBasis(basis0);
	for(unsigned j = 0; j < ND; j++) {
		for(unsigned i = 0; i < MD; i++) {
			rows[i] += to_string(basis0[i][j], prec, false);
		}
		level_insert(rows, padding);
	}
	for(auto & row : rows)
		cout << row << endl;
	cout << endl;

	rows = vector<string>((MD+1)*NI+1, "");
	for(unsigned i = 0; i < NI; i++) {
		T v = T(i)/(NI-1);
		for(unsigned j = 0; j < NJ; j++) {
			T u = T(j)/(NJ-1);
			bezierBasis(bases[i][j], u, v);
		}
	}
	for(unsigned i = 0, index = 2; i < NI; i++, index += MD+1) {
		auto sv = "v=" + to_string(T(i)/(NI-1), valLen, false);
		rows[index] += sv;
	}
	level_insert(rows, border+padding);
	cout << "NI = " << NI << "; NJ = " << NJ << "; MD = " << MD << "; ND = " << ND << endl;
	for(unsigned j = 0; j < NJ; j++) {
		for(unsigned l = 0; l < ND; l++) {
			unsigned row = 1;
			for(unsigned i = 0; i < NI; i++) {
				for(unsigned k = 0; k < MD; k++) {
					/*oss << bases[i][j][k][l];
					rows[i*MD+l] += oss.str();
					oss.str("");*/
					//rows[i*MD+k] += to_string(bases[i][j][k][l], prec, false);
					auto str = to_string(bases[i][j][k][l], prec, false);
					auto dLen = std::max<int>(0, valLen - str.length());
					rows[row++] += string(dLen, ' ') + str;
				}
				row++;
			}
			level_insert(rows, padding);
		}
		level_insert(rows, border+padding);
	}
	for(unsigned i = 0; i < NI+1; i++) {
		auto index = i*(MD+1);
		rows[index] = replace_all(rows[index], '|', '+');
		rows[index] = replace_all(rows[index], ' ', '-');
	}
	{
		auto pos = rows[0].find("+");
		for(unsigned j = 0; j < NJ; j++) {
			T u = T(j)/(NJ-1);
			auto su = " u=" + to_string(u, prec, false) + " ";
			rows[0].replace(pos+2, su.length(), su);
			pos = rows[0].find("+", pos+su.length());
		}
	}
	for(auto & row : rows) {
		cout << row << endl;
	}

}
