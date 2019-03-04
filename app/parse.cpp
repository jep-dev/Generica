#include <limits>

#include <iostream>
#include <iomanip>
#include <map>
#include <vector>
#include <set>

#include "dual.tpp"
#include "math.tpp"
#include "parse.tpp"
#include "streams.tpp"
#include "tag.hpp"
#include "utility.hpp"

template<class I>
bool isvar(I const& beg, I const& end, bool trimmed = false) {
	unsigned alpha = 0;
	for(auto i = beg; i < end; i++) {
		auto c = *i;
		if(c == '_') continue;
		if(std::isalpha(c)) alpha++;
		if(!alpha && std::isdigit(c)) return false;
	}
	return alpha;
}
bool isvar(std::string const& line, bool trimmed = false) {
	return isvar(line.cbegin(), line.cend(), trimmed);
}

template<class T = float>
std::pair<T, std::string> parse_coefficient(std::string const& word) {
	std::pair<T, std::string> out;
	out.first = Streams::parse_value(word);
	// Should this function participate in name validation?
	bool start = false;
	for(auto const& c : word)
		if(start || c == '(' || std::isalpha(c))
			start = true, out.second += c;
	return out;
}

/*
// Same note as coefficient: a0b.12cd3ef is parsed as abcdef
// Skips digits, punctuation, and spaces explicitly; accepts printable
// e.g. unicode pi, epsilon, etc.
std::string parse_units(std::string const& word) {
	std::string units;
	for(auto c : word) {
		// if(std::isalpha(c)) units += c;
		if(std::isdigit(c) || std::ispunct(c) || std::isspace(c))
			continue;
		if(std::isprint(c))
			units += c;
	}
	return units;
}

// TODO figure out how to parse UTF8, etc.
template<class T>
std::pair<bool, DualQuaternion<T>> parse_dual(std::string const& word) {
	std::pair<bool, DualQuaternion<T>> out = { false, {1} };
	if(!word.length()) return out;
	auto & d = out.second;

	T coefficient = parse_value<T>(word);
	d *= coefficient;

	auto units = parse_units(word);
	static const DualQuaternion<T> Z = {0};
	bool p = false;
	char prev = '\0';
	for(auto const& c : units) {
		if(p) {
			p = false;
			// Accept PI/pi
			if(((prev == 'p') && (c == 'i')) || ((prev == 'P') && (c == 'I'))) {
				d *= T(M_PI);
				prev = c;
				// Prevent parsing as I/i
				continue;
			}
			return out; // Reject P/p otherwise
		}
		switch(c) {
			case 'e': break;
			case 'I': d *= 1_E;
			case 'i': d *= 1_i; break;
			case 'J': d *= 1_E;
			case 'j': d *= 1_j; break;
			case 'K': d *= 1_E;
			case 'k': d *= 1_k; break;
			case 'E': d *= 1_E; break;
			// Accept 'p' pending 'i' (as in pi)
			case 'P': case 'p': p = true; break;
			default: return out;
		}
		if(d == Z) return out.first = true, out;
		prev = c;
	}
	// Reject trailing 'p'
	if(p) return out;
	// Reject incomplete Latin/Greek
	// if(latin || greek) return out;
	return out.first = true, out;
}
template<class T>
std::pair<bool, DualQuaternion<T>> parse_dual(std::vector<std::string> const& words) {
	if(!words.size()) return {false, {}};
	bool valid = true;
	DualQuaternion<T> out = {0};
	for(auto const& word : words) {
		auto parsed = parse_dual<T>(word);
		if(!parsed.first) {
			valid = false;
			break;
		}
		out += parsed.second;
	}
	return {valid, out};
}

template<class S>
S& help_dual(S& s) {
	return s << "Dual quaternions can be parsed from [-]au [{+|-}bv [...]], where\n"
		"\ta,b,... are {0..9}*[.{0..9}*] (any numeric literal) or pi;\n"
		"\tu,v,... are units in {e|i|j|k|E|I=Ei|J=Ej|K=Ek}; and\n"
		"\teach term is non-empty.\n", s;
}
template<class S>
S& help_assign(S& s) {
	return s << "Assignments must be given in the form <variable> = <expression>, where\n"
		"\t<variable> is a sequence of letters and not a sequence of {e|i|j|k|E|I|J|K|pi}, and\n"
		"\t<expression> is a sum of dual quaternion terms.\n", s;
}
template<class S>
S& help(S& s) {
	return s << "Inputs may be assignments (<variable> = <expression>) "
			"or free (<expression>).\n", s;
}*/

/*template<class T>
void test_interactive(std::map<std::string, DualQuaternion<T>> &symbols) {
	std::cout << "Enter an assignment or expression: ";

	bool silenced = false;
	std::string line;
	std::getline(std::cin, line);
	if(line == "") return;

	//auto split = split_sum(line);
	//if(split.size()) {
		//std::cout << "{";
		//for(long n = split.size(), i = 0; i < n; i++) {
			//auto const& word = split[i];
			//if(i) std::cout << ", ";
			//std::cout << word << " = " << parse_value(word)
					//<< " * " << parse_units(word);
		//}
		//std::cout << "}" << std::endl;
	//}

	do {
		auto assigned = Streams::split_assign(line);
		auto & symbol = assigned.first, value = assigned.second;
		auto symbol_alpha = parse_units(symbol);
		auto symbol_dual = parse_dual<T>(symbol_alpha);
		auto value_dual = parse_dual<T>(split_sum(value));

		if((symbol.length() && !symbol_alpha.length()) || symbol_dual.first) {
			help_assign(std::cout);
			break;
		}
		if(!value.length() || !value_dual.first) {
			help_dual(std::cout);
			break;
		}
		if(symbol_alpha.length()) {
			std::cout << symbol_alpha << " = ";
			if(symbols.find(symbol_alpha) != symbols.end())
				std::cout << symbols[symbol_alpha] << " -> ";
			symbols[symbol_alpha] = value_dual.second;
		}
		std::cout << value_dual.second << std::endl;
	} while(0);

	//auto len0 = assigned.first.length(),
			//len1 = assigned.second.length();
	//if(len0)
		//std::cout << '[' << assigned.first << ']';
	//if(len0 && len1)
		//std::cout << " = ";
	//if(len1) {
		//std::cout << '`';
		//print_split(split_outer(assigned.second));
		//std::cout << '`';
	//}
	//test_interactive();

	test_interactive(symbols);
}*/

std::vector<std::string>
parse_assign(std::string const& line, bool trimmed = false) {
	using namespace std;
	if(!trimmed) return parse_assign(Streams::trim(line), true);
	vector<string> out;
	vector<char> levels;
	auto p0 = line.cbegin(), p1 = line.cend();
	for(auto i = p0; i < p1; i++) {
		char c = *i, b = 0;
		if(levels.size()) b = levels.size();
		switch(c) {
			case '(': case '{': case '[': case '<':
				levels.emplace_back(c);
				continue;
			case '=':
				if(levels.size()) continue;
				if(*p0 == '=') {
					p0++;
					// == is not assignment
					if(p0 == i) return {};
				}
				out.emplace_back(Streams::trim(string(p0, i)));
				p0 = i;
				continue;
			case ')': if(b != '(') return {}; break;
			case '}': if(b != '{') return {}; break;
			case ']': if(b != '[') return {}; break;
			case '>': if(b != '<') return {}; break;
			default: continue;
		}
		levels.pop_back();
	}
	if(*p0 == '=') {
		p0++;
		if(p0 == p1) return {};
		out.emplace_back(Streams::trim(string(p0, p1)));
	} else {
		out.emplace_back(Streams::trim(string(p0, p1)));
	}
	return out;
}

/** @brief A map from keys (variable names) to values */
template<class T = float>
using Vars = std::map<std::string, T>;
/** @brief An entry in Vars; maps one variable name to its value */
template<class T = float>
using Var = Detail::Value_t<Vars<T>>;

///** @brief An expression consisting of Var, values, and operators */
// using Expr = std::string;
///** @brief A map from expressions to subexpressions */
//using Exprs = std::map<Expr, Expr>;
// b := a + 1
// a := 5
// eval(b) = eval(a)+1 = 6

template<class C, class... T>
struct PolyVars {
	//PolyVars& last(void) { return *this; }
	//const PolyVars& last(void) { return *this; }
	C& crtp(void) { return static_cast<C&>(*this); }
	const C& crtp(void) const { return static_cast<const C&>(*this); }

	std::set<std::string> m_keys, m_reserved;

	bool contains(std::string const& k) const { return m_keys.find(k) != m_keys.cend(); }
	bool reserved(std::string const& k) const { return m_reserved.find(k) != m_reserved.cend(); }
	bool available(std::string const& k) const { return !contains(k) && !reserved(k); }
	bool add_key(std::string const& k) { return !reserved(k) && m_keys.emplace(k).second; }
	bool reserve(std::string const& k) { return m_reserved.emplace(k).second; }

	PolyVars(void) {}
	PolyVars(std::set<std::string> const& r): m_reserved(r) {}
	template<class V0, class... KV>
	PolyVars(std::string const& k, V0 && v, KV &&... kv):
			PolyVars(std::forward<KV>(kv)...) {
		crtp().emplace(k, v);
	}
	virtual ~PolyVars(void) {}
};
template<class C, class S, class... T>
struct PolyVars<C, S, T...>: PolyVars<C, T...> {
	typedef PolyVars<C, S, T...> cur_type;
	typedef PolyVars<C, T...> next_type;
	typedef PolyVars<C> last_type;

	using next_type::crtp;
	//using next_type::last;
	using next_type::add_key;
	using next_type::reserve;
	using next_type::reserved;
	using next_type::contains;

	Vars<S> vars;

	next_type& next(void) { return static_cast<next_type&>(*this); }
	const next_type& next(void) const { return static_cast<const next_type&>(*this); }

	/*void insert(Vars<S> && v) {
		for(auto && kv : v)
			add_key(kv.first);
		for(auto && kv : v)
			vars.emplace(std::move(kv.first), std::move(kv.second));
	}*/
	void emplace(Var<S> const& kv) { emplace(kv.first, kv.second); }
	void emplace(Var<S> && kv) { emplace(std::move(kv.first), std::move(kv.second)); }
	template<class... V>
	void emplace(std::string const& k, V &&... v) {
		add_key(k);
		vars.emplace(k, std::forward<V>(v)...);
	}
	template<class... V>
	void emplace(std::string && k, V &&... v) {
		add_key(k);
		vars.emplace(std::move(k), std::forward<V>(v)...);
	}
	template<class M>
	void insert(M && m) { for(auto && kv : m) emplace(std::forward<decltype(kv)>(kv)); }

	PolyVars(void): next_type() {}
	template<class K, class... V>
	PolyVars(K && k, V &&... v): PolyVars(std::forward<V>(v)...) { emplace(std::forward<K>(k)); }
	template<class... V>
	PolyVars(std::string const& k, S const& s, V &&... v):
		PolyVars(std::forward<V>(v)...) { emplace(k, std::forward<S>(s)); }
	template<class... V>
	PolyVars(std::string && k, S && s, V &&... v):
		PolyVars(std::forward<V>(v)...) { emplace(std::move(k), std::move(s)); }
	template<class... V>
	PolyVars(Vars<S> const& m, V &&... v):
		next_type(std::forward<V>(v)...) { insert(m); }
	template<class... V>
	PolyVars(Vars<S> && m, V &&... v):
		next_type(std::forward<V>(v)...) { insert(std::forward<Vars<S>>(m)); }
	virtual ~PolyVars(void) {}
};

//
///** @brief The type of an unevaluated expression */
//using Expr = std::string;
///** @brief A map from unevaluated expressions to subexpressions */
//using Exprs = std::map<Expr, Expr>;

int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace Detail;
	typedef float T;
	typedef DualQuaternion<T> DQ;

	//PolyVars<T, DQ> vars(std::make_pair(std::string("pi"), M_PI), std::make_pair(std::string("E"), 1_E));
	//PolyVars<T, DQ> vars(Var<T>("pi", T(M_PI)));
	struct Poly;
	struct Poly: PolyVars<Poly, T, DQ> { using PolyVars<Poly, T, DQ>::PolyVars; };
	Poly vars(Vars<T>{{"pi", T(M_PI)}}, Vars<DQ>{{"E", 1_E}});
	vars = Poly(Var<T>{"pi", T(M_PI)});
	//vars = Poly("pi", T(M_PI));
	/*VarMap<T> tmap ( "pi", M_PI );
	VarMap<DQ> dmap (
		"e", 1_e, "i", 1_i, "j", 1_j, "k", 1_k,
		"E", 1_E, "I", 1_I, "J", 1_J, "K", 1_K,
		"A", 1_i, 1_j, 1_k
	);


	string line;
	auto prefix = "";
	unsigned prec = 2;
	while(getline(cin, line)) {
		if(!line.length()) break;
		if(line == "dump") {
			for(auto const& v : tmap.vars) {
				prefix = "";
				cout << v.first << " = {";
				for(auto const& vv : tmap[v.first]) {
					cout << prefix << to_string(vv, prec);
					prefix = ", ";
				}
				cout << "}" << endl;
			}
			for(auto const& v : dmap.vars) {
				prefix = "";
				cout << v.first << " = {";
				for(auto const& vv : dmap[v.first]) {
					cout << prefix << to_string(vv, prec);
					prefix = ", ";
				}
				cout << "}" << endl;
			}
			continue;
		}
		auto assign = parse_assign(line);
		auto na = assign.size();
		if(!na) {
			cout << "Parse error" << endl;
			continue;
		}
		cout << "Parsed: ";
		if(na > 1) cout << "{";
		prefix = "'";
		for(auto const& a : assign) {
			cout << prefix << a;
			prefix = "', '";
		}
		cout << "'";
		if(na > 1) cout << "}";
		cout << endl;

		prefix = "";

		if(na == 1) {
			auto a = assign[0];
			bool isa = contains(tsys.vars, a), isd = contains(dsys.vars, a);
			if(!isa && !isd) {
				cout << a << " is not defined" << endl;
				continue;
			}
			cout << " = ";
			if(isa)
				for(auto v : tsys[a]) {
					cout << prefix << to_string(v, prec);
					prefix = ", ";
				}
			else if(isd)
				for(auto v : dsys[a]) {
					cout << prefix << to_string(v, prec);
					prefix = ", ";
				}
			cout << endl;
		}
	}*/

}
