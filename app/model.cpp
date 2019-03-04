#include <iostream>

#include "model.hpp"
#include "dual.tpp"
#include "math.tpp"
#include "streams.tpp"

template<class S, class T, class... I>
S& print_structure(S &s, Model<T, I...> const& m = 0) {
	using namespace Streams;
	s << " { ";
	for(auto const& c : m.children)
		print_structure(s, c);
	return s << " }", s;
}

template<class S, class T, class... I>
S& print_accum(S &s, Model<T, I...> const& m, unsigned level = 0) {
	using namespace Streams;
	s << string(level*2, ' ') << to_string(m.accumulated, 2) << "\n";
	for(auto & c : m.children)
		print_accum(s, c, level+1);
	return s;
}
template<class S, class T, class... I>
S& print_model(S &s, Model<T, I...> const& m, unsigned level = 0) {
	using namespace Streams;
	s << string(level*2, ' ') << to_string(m.accumulated, 2) << "\n";
	for(auto & c : m.children)
		print_model(s, c, level+1);
	return s;
}

int main(int argc, const char *argv[]) {
	using namespace Streams;
	using std::cout;
	using std::endl;
	typedef float T;
	typedef DualQuaternion<T> DQ;
	DQ ne = 1_e + 1_I + 1_J, nw = ne - 2_I,
		sw = nw - 2_J, se = ne - 2_J;

	Model<T> root;
	// This creates two nodes, but replaces the first with the second
	// auto lhs = &root.emplace_back(...);
	//      lhs = &lhs -> emplace_back(...);
	auto rhs = &root.emplace_back(rotation<float>(M_PI/2, 0, 0, 1));
	auto lhs = &root.emplace_back(rotation<float>(-M_PI/2, 0, 0, 1));
	//lhs = &lhs -> emplace_back(translation<float>(2, 0, 0));
	auto & sclerper = root.back();
	sclerper.emplace_back(nw);
	sclerper.emplace_back(ne);
	sclerper.emplace_back(sw);
	sclerper.emplace_back(se);

	cout << "Model:" << endl;
	print_model(cout, root);
	auto accum = translation<float>(0, 0, 1);
	root.accumulate(accum);
	cout << "Accumulate(" << accum << "):" << endl;
	print_accum(cout, root);
	accum = translation<float>(0, 0, 2);
	root.accumulate(accum);
	cout << "Accumulate(" << accum << "):" << endl;
	print_model(cout, root);
}
