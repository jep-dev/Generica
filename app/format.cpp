#include <iostream>
#include <array>
#include <vector>
#include <limits>
#include <functional>

#include "pretty.hpp"
#include "format.tpp"
#include "math.tpp"
#include "streams.tpp"

#include "quaternion.tpp"

#include "utility.hpp"

int main(int argc, const char *argv[]) {
	using namespace std;

	Quaternion<float> LHS[] = {{1}, {3,4}, {8,15}, {7,24}};
	using OSS = ostringstream;

{
	using namespace Streams;

	string astrs[] = {"1", "23"};
	auto mm = minimax(astrs);
	cout << mm.first << ", " << mm.second << endl;
	vector<string> vstrs = {"45", "678"};
	mm = minimax(vstrs);
	cout << mm.first << ", " << mm.second << endl;
	vector<OSS> voss(3);
	voss[0] << "";
	voss[1] << "81";
	voss[2] << "729";
	mm = minimax(voss);
	cout << mm.first << ", " << mm.second << endl;

	level_insert(vstrs, " | ");
	level_insert_each(vstrs, vstrs);
	for(auto const& vstr : vstrs) {
		cout << vstr << endl;
	}
}

	static constexpr auto N = 4;
	vector<string> rows = {
		" q  =",
		" q* =",
		"-q  =",
		"|q| ="
	};
	for(auto const& quat : LHS) {
		vector<ostringstream> oss(N);
		vector<string> v(N);
			v[0] = (oss[0] << quat, oss[0]).str();
			v[1] = (oss[1] << *quat, oss[1]).str();
			v[2] = (oss[2] << -quat, oss[2]).str();
			v[3] = (oss[3] << quat, oss[3]).str();
		auto len = Streams::minimax(v).second;
		for(auto i = 0; i < N; i++) {
			OSS result;
			result << setw(len) << right << v[i];
			rows[i] += " " /*+ column[i] + " "*/ + result.str() + " |";
		}
	}

	for(auto const& row : rows)
		cout << row << endl;

	cout << Streams::join("Line 1\nLine 2, longer", "Line 1, longer\nLine 2", ' ', " = ") << endl;
}
