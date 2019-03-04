#include <iostream>
#include <iomanip>
#include <sstream>

#include "streams.tpp"
#include "functional/transform.hpp"
#include "functional/for_each.hpp"
#include "typesort.hpp"

enum EGen : unsigned {
	g_i = 0, g_j, g_E, g_F, n_gens
};
enum EUnit : unsigned {
	e_0 = 0, e_i, e_j, e_k,
	e_E, e_Ei, e_Ej, e_Ek,
	e_F, e_Fi, e_Fj, e_Fk,
	e_G, e_Gi, e_Gj, e_Gk, n_units
};

template<EGen> struct Generator;
template<EGen E> struct Generator: Comparable<Generator<E>, EGen, E> {};

template<EUnit> struct Unit;
template<EUnit E> struct Unit: Comparable<Unit<E>, EUnit, E> {};

int main(int argc, const char *argv[]) {
	static constexpr EGen i = g_i, j = static_cast<EGen>(i+1);
}
