#include <iostream>
#include <vector>
#include <map>

#include "dual.tpp"
#include "point.hpp"

#include "pretty.hpp"

#include "math.tpp"

#ifndef DUAL_IMPL
#define DUAL_IMPL "unknown"
#endif

#include "streams.tpp"
#include "utility.hpp"

template<class T, unsigned N, unsigned M>
std::vector<std::string> get_table(std::string const& title,
		const DualQuaternion<T> (&LHS)[N], const DualQuaternion<T> (&RHS)[M],
		DualQuaternion<T> (*fn)(DualQuaternion<T> const&, DualQuaternion<T> const&)) {
	using namespace std;
	using Streams::center;

	vector<string> out;
	ostringstream os;
	char b_n = '-', b_ne = '.', b_e = '|', b_se = '\'',
			b_s = '-', b_sw = '\'', b_w = '|', b_nw = '.',
			b_c = '+', b_none = ' ';
	string utitle = "(u)", vtitle = "(v)", pad = " ";
	long wu = utitle.length(), wv = vtitle.length(), wtitle = title.length(),
			rwv = (wv-1)/2, lwv = wv-1-rwv, wcell = max(3l, wu),
			wpad = pad.length(), wpcell = wcell + wpad * 2, wrow = wpcell * M,
			dtitle = wtitle + 2;
	string lv = string(lwv, ' '), rv = string(rwv, ' '),
			north = b_none + string(wpcell + lwv, ' ') + b_nw
				+ center(center(title, dtitle, ' '), wrow + rwv, '-') + b_ne,
		middle = b_nw + center(utitle, wpcell+lwv, '-', false) + b_c + string(wrow+rwv, '-') + b_c,
		bottom = b_sw + string(wpcell+lwv, '-') + b_c + string(wrow+rwv, '-') + b_se;
	out.emplace_back(north);
	string header = b_none + string(wpcell, ' ') + vtitle;
	for(auto const& rhs : RHS)
		header += pad + center(to_string(rhs, 2, false), wcell, ' ', false) + pad;
	header += b_e;
	out.emplace_back(header);
	out.emplace_back(middle);
	for(auto const& lhs : LHS) {
		string str = b_w + center(to_string(lhs,2), wpcell+lwv, ' ', false) + b_w + string(rwv, ' ');
		for(auto const& rhs : RHS) {
			str += pad + center(to_string(fn(lhs, rhs),2), wcell, ' ', false) + pad;
		}
		str += b_e;
		out.emplace_back(str);
	}
	out.emplace_back(bottom);
	return out;
}

void test_transformations(void) {
	using namespace std;
	using namespace Streams;
	typedef float T;
	typedef DualQuaternion<T> DQ;
	vector<DQ> xs = {1_e, 1_e + 1_I, 1_e + 1_J, 1_e - 1_I, 1_e - 1_J};
	vector<pair<string,DQ>> ts = {
		{"pi k @ 0", pivot<T>(M_PI,0,0,1, 0,0,0)},
		{"pi k @ I", pivot<T>(M_PI,0,0,1, 1,0,0)},
		{"pi k @ -I", pivot<T>(M_PI,0,0,1, -1,0,0)}};
	auto xlen = xs.size(), tlen = ts.size();
	vector<string> lines(xlen+2);
	// e+X.XXi+Y.YYj+Z.ZZk
	// units in {1..4}
	//  (units-1) operators
	//  (units-1)*{0,1,4} digits/decimals
	//  max = 4*3+3+4=19
	unsigned w1 = 8, w2 = 19;
	vector<string> col1 = {"", align("Point", w1, -1, true)};
	for(auto const& x : xs)
		col1.emplace_back(align(to_string(x, 2), w1, -1, true));
	for(auto const& t : ts) {
		vector<string> col2 = {align(t.first, w2, 1, true),
			align(to_string(t.second, 2), w2, 1, true)};
		for(auto const& x : xs)
			col2.emplace_back(align(to_string(x ^ t.second, 2), w2, 1, true));
		level_insert_each(lines, col1);
		level_insert(lines, "   ");
		level_insert_each(lines, col2);
		level_insert(lines, " | ");
	}
	for(auto const& line : lines)
		cout << line << endl;
}

/* Testing the role of the non-dual component in sandwich products - e.g. position and normal
 * stored as (n + E p) as the object of the sandwich product. Oddly enough, while the traditional
 * cos(theta)+isin(theta) format did not work, 1+cos(theta)+isin(theta) appeared to.
 * That is, 1_e + n + Ex = 1_e + n' + Ex' (the 1_e is preserved and would have to be removed.)
 */
void test_binary(void) {
	using namespace std;
	typedef float T;
	typedef DualQuaternion<T> DQ;
	DQ a1 = 1_e + 1_E,
		A[] = {1_i, 1_j, 1_k},
		B[]{1_e + .5_I, 10_E, 1_e + .5_I + 10_E};
	unsigned prec = 2;
	for(auto const& rhs : A) {
		for(auto const& lhs : B) {
			cout << "(" << to_string(lhs, prec) << ")"
				"^(" << to_string(rhs, prec) << ") "
				"= " << to_string(lhs ^ rhs, prec) << endl;
		}
		cout << endl;
	}
}
void test_points(void) {
	using namespace std;
	DualQuaternion<float> transforms[] = {
			rotation(float(M_PI/2), 1, 0, 0),
			rotation(float(M_PI/2), 0, 1, 0),
			rotation(float(M_PI/2), 0, 0, 1),
			1_e + 0.5_I, 1_e + 0.5_J, 1_e + 0.5_K
	};
	Point<float> pts[] = {{}, {1}, 1_e + 1_J, 1_e + 1_K};
	for(auto const& pt : pts) {
		cout << "Point: " << pt << endl;
		for(auto const& t : transforms) {
			cout << "  ^(" << string(t) << ") = " << to_string(pt ^ t, 1) << endl;
		}
	}
}

void test_heterogeneous(void) {
	using namespace std;
	typedef float T;
	typedef Quaternion<T> Q;
	typedef DualQuaternion<T> D;
	vector<Q> qs = {1_e, 1_i, 1_j, 1_k};
	vector<D> ds = {1_e, 1_i, 1_j, 1_k, 1_E, 1_I, 1_J, 1_K};
	vector<string> dqRows(ds.size()), qdRows(qs.size());

	for(auto const& d : ds) {
		auto beg = qdRows.begin();
		for(auto const& q : qs) {
			// Use an odd width so evens (positives) are padded where signs would be
			*beg += Streams::center(to_string(q*d), 3, ' ', false);
			beg++;
		}
	}
	cout << "Quaternion * dual:\n";
	for(auto const& row : qdRows)
		cout << row << "\n";
	cout << endl;

	for(auto const& q : qs) {
		auto beg = dqRows.begin();
		for(auto const& d : ds) {
			*beg += Streams::center(to_string(d*q), 3, ' ', false);
			beg++;
		}
	}
	cout << "Dual * quaternion:\n";
	for(auto const& row : dqRows)
		cout << row << "\n";
	cout << endl;
}

int main(int argc, const char *argv[]) {
	using namespace std;
	using Streams::center;

	typedef float T;
	typedef DualQuaternion<T> DQ;
	typedef Point<T> PT;
	typedef DQ (binary) (DQ const&, DQ const&);

	//scratch();
	//test_binary();
	//test_heterogeneous();
	test_transformations();

	DQ ds[] = {1_e, 1_i, 1_j, 1_k, 1_E, 1_I, 1_J, 1_K};
	vector<DQ> dsMixed = {1_e, 1_i, 1_j, 1_k,
			1_e + 1_I, 1_e + 1_J, 1_e + 1_K,
			1_i + 1_I, 1_i + 1_J, 1_i + 1_K};

	vector<pair<string, binary*>> functions = {
		{"W(u, v) = u v *u", [] (DQ const& l, DQ const& r) -> DQ { return l ^ r; }},
		{"W(u, v) = v u *v", [] (DQ const& l, DQ const& r) -> DQ { return r ^ l; }},
		{"W(u, v) = uv", [] (DQ const& l, DQ const& r) -> DQ { return l * r; }}
	};

	{
		cout << "Operators: " << endl;
		vector<vector<std::string>> lines;
		for(auto const& fn : functions)
			lines.emplace_back(get_table(fn.first, ds, ds, fn.second));
		for(long k = 0, dk = 2, n = lines.size(); k < n; k += dk) {
			for(long j = 0, m = lines[0].size(); j < m; j++) {
				for(long i = k; i < k+dk && i < n; i++)
					cout << lines[i][j] << "  ";
				cout << endl;
			}
		}
	}

	cout << "\nDivision:\n";
	{
		unsigned n = dsMixed.size();
		vector<string> lines(n+1, "");

		for(unsigned i = 0; i < n; i++)
			lines[i+1] += to_string(dsMixed[i], 2, false);
		Streams::level_insert(lines, " | ");
		lines[0].replace(std::max<unsigned>(0, lines[0].length()-4), 1, "/");
		for(unsigned j = 0; j < n; j++) {
			auto const& rhs = dsMixed[j];
			lines[0] += to_string(rhs, 2, false);
			for(unsigned i = 0; i < n; i++) {
				auto const& lhs = dsMixed[i];
				auto quot = lhs / rhs;
				lines[i+1] += to_string(quot, 2, false);
			}
			Streams::level_insert(lines, "  ");
		}
		lines.insert(lines.begin()+1, string(lines[0].length(), '-'));
		for(auto const& line : lines) cout << line << endl;
		/*vector<vector<string>> lines(3, vector<string>(n, ""));
		unsigned numLens = 0, denLens = 0, quotLens = 0;
		for(unsigned i = 0; i < n; i++) {
			auto const& d = dsMixed[i];
			lines[0][i] += to_string(d, 2, false);
			lines[1][i] += to_string(1/d, 2, false);
			//lines[2][i] += to_string( d * (1/d), 2, false);
			//lines[2][i] += to_string( (1/d) * d, 2, false);
			lines[2][i] += to_string( d/d, 2, false);
			numLens = std::max<unsigned>(numLens, lines[0][i].length());
			denLens = std::max<unsigned>(denLens, lines[1][i].length());
			quotLens = std::max<unsigned>(quotLens, lines[2][i].length());
		}
		for(vector<string> & v : lines) {
			unsigned len = 0;
			for(auto const& i : v)
				len = std::max<unsigned>(len, i.length());
			for(string & i : v) {
				auto dlen = len - i.length();
				i = string(dlen/2, ' ') + i + string(dlen - dlen/2, ' ');
			}
		}
		for(unsigned i = 0; i < n; i++) {
			cout << lines[0][i] << " / " << lines[0][i] << " = "
					<< lines[0][i] << " * " << lines[1][i] << " = " << lines[2][i] << endl;
		}*/
	}

	cout << "\nSclerp:\n";
	{
		DualQuaternion<T> u = 1_i, v = 1_j + 10 * 1_J;

		for(auto k = 0, N = 10; k < N; k++) {
			T i = T(k)/N, dj = T(1)/N;
			auto s = sclerp(u, v, i);
			for(auto j = 0; j < 8; j++)
				if(s[j]*s[j] < dj*dj) s[j] = 0;
			cout << "  sclerp(" << u << ", " << v << ", " << i << ") = "
					<< s << ";\n";
		}
	}

	cout << "\nBi-sclerp:\n";
	{
		unsigned M = 8, N = 4;
		auto get_uv = [=] (unsigned i, unsigned j) -> std::pair<T, T> {
			float u1 = i/float(M-1),
					v1 = j/float(N-1),
				u2 = 1 - (M-i-1)/float(M-1),
				v2 = 1 - (N-j-1)/float(N-1),
				u = (u1 + u2)/2, v = (v1 + v2)/2;
			return {u, v};
		};
		DQ l1 = {1},
				r1 = {0,1,0,0,0,0,0,0},
				l2 = {0,0,0,0,0,0,0,1},
				r2 = {1};
		vector<vector<string>> cols;
		vector<unsigned> widths;
		for(auto j = 0; j < N; j++) {
			widths.emplace_back();
		}
		ostringstream cell;
		for(auto i = 0; i < M; i++) {
			cols.emplace_back();
			auto & bk = cols.back();
			for(auto j = 0; j < N; j++) {
				bk.emplace_back();
				auto uv = get_uv(i, j);
				auto const& u = uv.first, v = uv.second;
				std::string str = sclerp(l1, r1, l2, r2, u, v);
				widths[j] = std::max(std::size_t(widths[j]), str.length());
				bk[j] = std::move(str);
			}
		}
		for(auto i = 0; i < M; i++) {
			for(auto j = 0; j < N; j++) {
				auto & col = cols[i][j];
				cout << col << std::string(widths[j] - col.length() + 2, ' ');
			}
			cout << endl;
		}
	}

}
