#ifndef BINOMIAL_HPP
#define BINOMIAL_HPP

#include "sequence.hpp"

namespace Detail {

using Size = std::size_t;

template<Size N> struct Binomial;
template<Size N> struct Binomial: Binomial<N-1> {
	typedef Binomial<N-1> base_type;
	//typedef typename base_type::value_type next_type;
	typedef Value_t<base_type> next_type;
	typedef decltype(next_type{}.prepend(SeqSz<0>{})
			+ next_type{}.append(SeqSz<0>{})) value_type;
	/*typedef decltype(prepend<Size, 0>(next_type{})
			+ append<Size, 0>(next_type{})) value_type;*/
};

template<>
struct Binomial<0> {
	typedef Binomial<0> base_type;
	typedef SeqSz<1> value_type;
};

/*template<class C, class T, class S = Value_t<C>, class U = void>
struct SeqInit;

// template<class C, class S, class T, T... I>
// struct SeqInit<C, S, Seq<T, I...>>
template<class C, class S, class T, T... I>
struct SeqInit<C, Seq<T, I...>, S, Void_t<decltype(C{I...})>> {
private:
	C m_value = {I...};
public:
	// This will not work for array types! Should I care?
	operator C && (void) { return std::move(m_value); }
	static C && get(void) { return std::move(SeqInit()); }
	virtual ~SeqInit(void) {}
};
*/

template<class OS, std::size_t N>
OS& operator<<(OS &os, Binomial<N> const&) {
	typedef Value_t<Binomial<N>> value_type;
	return os << value_type{}, os;
}

}

#endif
