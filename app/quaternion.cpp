#include <iostream>
#include <iomanip>
#include <vector>
#include "math.tpp"

#include "quaternion.tpp"

int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Streams;

	using T = float;
	using Q = Quaternion<T>;
	unsigned N = 2, prec = 2, hiPrec = 3, wprec = prec + 3;
	if(argc == 1) {
		T angle = M_PI / 2, scale1 = 2, scale2 = sqrt(2);
		string bord(N, ' '), sep(1, ' '), pad(N, ' '),
				tbord(N, ' '), tsep(sep.length(), '_'), tpad(N, ' ');
		tpad[1] = 'x';
		bord[1] = '|';

		vector<Q> basis = {{1}, {0,1}, {0,0,1}, {0,0,0,1}}, xforms;
		vector<pair<pair<T,T>, Q>> axes = {
				{{1, 0}, 1_i},
				{{1, M_PI/4}, 1_i},
				{{1, M_PI/3}, 1_i},
				{{1, M_PI/2}, 1_i},
				{{1, M_PI}, 1_i},
				{{2, 0}, 1_i},
				{{2, M_PI/4}, 1_i},
				{{2, M_PI/3}, 1_i},
				{{2, M_PI/2}, 1_i},
				{{2, M_PI}, 1_i}
		};
		for(auto const& axis : axes) {
			auto const& r = axis.first.first, t = axis.first.second/2;
			xforms.emplace_back(r*(cos(t)*1_e + sin(t)*axis.second));
		}
			/*xforms = {rotation(angle, 1, 0, 0), rotation(angle, 1, 0, 0)*2,
					rotation(angle, 0, 1, 0), rotation(angle, 0, 0, 1)};*/
		unsigned nBases = basis.size(), nXforms = xforms.size();
		vector<string> rows(nXforms+1, "");

		vector<pair<string, Q (*)(Q const&)>> cols = {
			{"1 / u", [] (Q const& q) -> Q { return 1/q; }},
			{"u / |u|", [] (Q const& q) -> Q { return q.normalize(); }},
			{"e ^ u", [] (Q const& q) -> Q { return exp(q); }},
			{"ln u", [] (Q const& q) -> Q { return log(q); }},
			{"e ^ ln u", [] (Q const& q) -> Q { return exp(log(q)); }},
		};
		rows[0] += "r";
		for(unsigned i = 0; i < nXforms; i++)
			rows[i+1] += to_string(axes[i].first.first, prec, false);
		level_insert(rows, sep);
		rows[0] += "t";
		for(unsigned i = 0; i < nXforms; i++)
			rows[i+1] += to_string(axes[i].first.second/M_PI, prec, false) + "pi";
		level_insert(rows, sep);
		rows[0] += "n";
		for(unsigned i = 0; i < nXforms; i++)
			rows[i+1] += to_string(axes[i].second, prec, false);
		level_insert(rows, pad);
		rows[0] += "u = re^nt";
		for(unsigned i = 0; i < nXforms; i++)
			rows[i+1] += to_string(xforms[i], prec, false);
		level_insert(rows, pad);

		for(auto const& col : cols) {
			unsigned i = 0;
			rows[i++] += col.first;
			for(unsigned i = 0; i < nXforms; i++) {
				rows[i+1] += to_string(col.second(xforms[i]), prec, false);
			}
			level_insert(rows, sep);
		}
		for(auto & row : rows) {
			cout << row << endl;
			row = "";
		}


		unsigned i = 0;
		rows[i++] += tpad;
		for(auto const& rhs : xforms)
			rows[i++] += to_string(rhs, prec, false);
		level_insert(rows, bord + sep);
		for(auto const& lhs : basis) {
			i = 0;
			rows[i++] += to_string(lhs, prec, false);
			for(auto const& rhs : xforms)
				rows[i++] += to_string(lhs ^ rhs, prec, false);
			level_insert(rows, sep);
		}
		for(auto const& row : rows)
			cout << row << endl;
		return 0;
	}


	enum Erp {
		e_lerp = 0, e_nlerp, e_slerp, e_nslerp, e_erps
	};
	bool enabled[e_erps] = {0};
	vector<string> keywords[e_erps] = {
			{"lerp", "linear", "all"},
			{"nlerp", "normalized-linear", "normalized", "all"},
			{"slerp", "spherical-linear", "spherical", "all"},
			{"nslerp", "normalized-spherical-linear", "normalized-spherical", "all"}
	};
	/*if(argc == 1) enabled[e_slerp] = true;
	else {*/
		bool hit = false;
		for(int i = 1; i < argc; i++) {
			for(int j = 0; j < e_erps; j++) {
				for(auto const& kw : keywords[j]) {
					if(argv[i] == kw) {
						enabled[j] = hit = true;
						break;
					}
				}
			}
		}
		if(!hit) enabled[e_slerp] = true;
	//}
	cout << fixed << setprecision(prec);
	auto printq = [=] (ostream& os, Q const& q) -> ostream& {
		auto f = os.flags();
		for(auto i : {'1', 'i', 'j', 'k'}) {
			if(i != 1) os << ' ';
			os << setw(wprec) << q[i];
		}
		os.flags(f);
		return os;
	};
	T ut = M_PI/4, vt = -M_PI/4;
	int scale = 2;
	auto u = scale*rotation(ut, 1, 0, 0), v = scale*rotation(vt, 1, 0, 0);
	auto lim = std::numeric_limits<T>::epsilon(), dt = T(.25);
	printq(cout << "For u = " << scale
			<< "exp(" << setw(6) << ut/2/M_PI << "\u03c0i) = ", u) << ",\n";
	printq(cout << "    v = " << scale
			<< "exp(" << setw(6) << vt/2/M_PI << "\u03c0i) = ", v) << "...\n";
	auto print_ss = [] (ostream &dest, ostringstream &l, ostringstream &r) -> ostream& {
		dest << setw(20) << l.str() << " = " << r.str();
		l.str(""), r.str("");
		return dest;
	};
	auto stringq = [=] (Q const& q) -> string {
		ostringstream oss;
		oss << setprecision(prec) << right
			<< setw(wprec) << q.w << ' ' << setw(wprec) << q.x << ' '
			<< setw(wprec) << q.y << ' ' << setw(wprec) << q.z;
		return oss.str();
	};

	ostringstream loss, ross;
	for(T t = 0; t < 1 + dt - lim; t += dt) {
		string strings[e_erps] = {stringq(lerp(u,v,t)), stringq(nlerp(u,v,t)),
				stringq(slerp(u,v,t)), stringq(slerp(u,v,t,true))};
		auto i = 0;
		for(auto j = 0; j < e_erps; j++) {
			if(enabled[j]) {
				if(!i) cout << "t = " << setw(4) << t << ": ";
				else if(i & 1) cout << "; ";
				else cout << setw(10) << "";
				cout << setw(6) << right << keywords[j][0] << " = "
						<< setw(20) << strings[j];
				if(i & 1) cout << "; " << endl;
				i++;
			}
		}
		if((i & 1)) cout << "; " << endl;
	}
}
