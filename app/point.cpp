#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

#include "dual.tpp"
#include "math.tpp"
#include "point.hpp"
#include "pretty.hpp"
#include "quaternion.tpp"
#include "streams.tpp"
#include "tag.hpp"

void test_perpendicular(unsigned p = 2, bool norm = true) {
	using namespace std;
	using namespace Streams;
	typedef Point<float> Pt;
	vector<Pt> points = {1_x, 1_y, 1_z, -1_x, -1_y, -1_z, 1_x + 1_y, 1_x+1_y+1_z};
	auto N = points.size();
	vector<string> rows(N+1);

	auto pad = "  ";
	rows[0] += "X";
	for(unsigned i = 0; i < N; i++)
		rows[i+1] += string(points[i]);
	level_insert(rows, pad);
	rows[0] += "|X|";
	for(unsigned i = 0; i < N; i++)
		rows[i+1] += to_string(points[i].length(), p, false);
	level_insert(rows, pad);
	rows[0] += "X / |X|";
	for(unsigned i = 0; i < N; i++)
		rows[i+1] += to_string(points[i].normalize(), p, false);
	level_insert(rows, pad);
	rows[0] += "Perp. U";
	vector<pair<Pt, Pt>> perps(N);
	for(unsigned i = 0; i < N; i++) {
		perps[i] = perpendicular(points[i], norm);
		rows[i+1] += to_string(perps[i].first, p, false);
	}
	level_insert(rows, pad);
	rows[0] += "Perp. V";
	for(unsigned i = 0; i < N; i++) {
		rows[i+1] += to_string(perps[i].second, p, false);
	}
	level_insert(rows, pad);
	/*rows[0] += " U x V";
	for(unsigned i = 0; i < N; i++) {
		rows[i+1] += to_string(cross(perps[i].first, perps[i].second), p, false);
	}
	level_insert(rows, pad);*/
	for(auto const& row : rows)
		cout << row << endl;
}
int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Streams;

	using T = float;
	using PT = Point<T>;
	using Q = Quaternion<T>;
	using DQ = DualQuaternion<T>;

	unsigned prec = 2;

	cout << "Inverse cross product (arbitrarily oriented):" << endl;
	test_perpendicular();
	cout << endl;

	vector<Q> qs = {
		rotation(float(M_PI/2), 1, 0, 0),
		rotation(float(M_PI/2), 0, 1, 0),
		rotation(float(M_PI/2), 0, 0, 1),
	};
	//vector<DQ> dqs = {
	vector<pair<pair<float, PT>, PT>> dqs = {
		/*rot_translation<T>(T(M_PI/2), 1, 0, 0, 0, 1, 0),
		rot_translation<T>(T(M_PI/2), 0, 1, 0, 0, 0, 1),
		rot_translation<T>(T(M_PI/2), 0, 0, 1, 1, 0, 0),*/
		{{T(M_PI/2), PT{1, 0, 0}}, PT{0, 1, 0}},
		{{T(M_PI/2), PT{0, 1, 0}}, PT{0, 0, 1}},
		{{T(M_PI/2), PT{0, 0, 1}}, PT{1, 0, 0}},
		//1_j + 1_I, 1_j + 1_J, 1_j + 1_K
	};
	vector<PT> pts = {
		{0,0,0}, {1,0,0}, {0,1,0}, {0,0,1}
	};

	string bord = " | ", delim = "  ";
	vector<string> qRows(pts.size()+1, ""), dqRows(pts.size()+2,"");
	unsigned ptPrec = prec, qPrec = prec, dqPrec = prec;

	transform(pts.begin(), pts.end(), qRows.begin(),
			[=](PT const& pt) { return to_string(pt, qPrec) + bord; });
	level(qRows);
	qRows.insert(qRows.begin(), string(minimax(qRows).second, ' '));
	transform(pts.begin(), pts.end(), dqRows.begin(),
			[=](PT const& pt) { return to_string(pt, dqPrec) + bord; });
	level(dqRows);
	dqRows.insert(dqRows.begin(), string(minimax(dqRows).second, ' '));
	dqRows.insert(dqRows.begin(), string(minimax(dqRows).second, ' '));

	for(long i = 0, M = qs.size(); i < M; i++) {
		for(long j = 0, N = pts.size(); j < N; j++) {
			auto &row = qRows[j+1];
			row += delim + to_string(pts[j]^qs[i], qPrec);
		}
		auto dl = minimax(qRows).second - qRows[0].length();
		qRows[0] += center(to_string(qs[i], qPrec), dl) + delim;
		level(qRows);
	}
	cout << "Quaternion rotations by pi/2:" << endl;
	for(auto const& row : qRows)
		cout << row << endl;

	for(long i = 0, M = dqs.size(); i < M; i++) {
		auto theta = dqs[i].first.first;
		auto norm = dqs[i].first.second;
		auto trans = dqs[i].second;
		auto dq = rot_translation<T>(theta, norm.x, norm.y, norm.z,
				trans.x, trans.y, trans.z);
		for(long j = 0, N = pts.size(); j < N; j++) {
			auto &row = dqRows[j+2];
			row += delim + to_string(pts[j]^dq, dqPrec);
		}
		auto dl = minimax(dqRows).second - dqRows[0].length();
		//dqRows[0] += center(to_string(dq, dqPrec), dl) + delim;
		dqRows[0] += center(to_string(norm, dqPrec)+",", dl) + delim;
		dqRows[1] += center(to_string(trans, dqPrec)+" ", dl) + delim;
		level(dqRows);
	}
	cout << "Dual quaternion rotations (pi/2) after translations:" << endl;
	for(auto const& row : dqRows)
		cout << row << endl;
}
