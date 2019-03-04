#ifndef TIMING_HPP
#define TIMING_HPP

///@cond
#include <chrono>
#include <thread>
#include <vector>
///@endcond

#include "math.hpp"
#include "tag.hpp"

namespace Timing {

using namespace std::chrono;
using std::ratio;
using clock = high_resolution_clock;

/** Gets the time elapsed between two measurements */
template<class S, class T>
S elapsed(T && t0, T && t1) {
	using namespace std::chrono;
	return duration_cast<duration<S>>(t1-t0);
}
/** Gets the time according to the high resolution clock */
template<class T = double, class... R>
T now(Detail::Tag<T, R...> = {}) {
	using namespace std::chrono;
	auto cur = high_resolution_clock::now();
	return duration_cast<duration<T, R...>>(cur).count();
}
/** Gets the time in seconds */
template<class T = double>
T now_seconds(void) { return now<T, ratio<1>>(); }
/** Gets the time in milliseconds */
template<class T = double>
T now_milliseconds(void) { return now<T, ratio<1, 1000>>(); }
/** Gets the time in microseconds */
template<class T = double>
T now_microseconds(void) { return now<T, ratio<1, 1000000>>(); }

/** Pair of time points; easily converted to a duration without losing reference point */
template<class CLOCK = high_resolution_clock, class DUR = typename CLOCK::duration>
using Diff = std::pair<time_point<CLOCK, DUR>, time_point<CLOCK, DUR>>;

/** Pair of time points with methods supporting elapsed calculation, update, etc. */
template<class CLOCK = high_resolution_clock, class DUR = typename CLOCK::duration>
struct Stopwatch: Diff<CLOCK, DUR> {
	template<class S = std::chrono::seconds>
	S elapsed(Detail::Tag<S> = {}) const {
		return duration_cast<S>(this -> second - this -> first);
	}
	template<class S = double, class... T>
	S count(void) const {
		return elapsed<duration<S>>().count();
	}
	Stopwatch& advance(void) { return this -> first = this -> second, *this; }
	Stopwatch& update(void) { return this -> second = CLOCK::now(), *this; }
	Stopwatch& reset(void) { return this -> first = this -> second = CLOCK::now(); }
	Stopwatch(time_point<CLOCK, DUR> && p0 = CLOCK::now(),
			time_point<CLOCK, DUR> && p1 = CLOCK::now()):
		Diff<CLOCK, DUR>(p0, p1) { }
};
/** Utility returning a Stopwatch (clock specialization) */
template<class CLOCK = high_resolution_clock, class DUR = typename CLOCK::duration>
Stopwatch<CLOCK, DUR> make_stopwatch(CLOCK = {}) { return {}; }

/** Utility returning a Stopwatch (time point specialization) */
template<class CLOCK, class DUR = typename CLOCK::duration>
Stopwatch<CLOCK, DUR> make_stopwatch(time_point<CLOCK, DUR> && p0,
		time_point<CLOCK, DUR> && p1 = CLOCK::now()) { return {}; }

/** Measures the time elapsed during a given action/operation */
template<class T>
struct Delta {
	typedef std::pair<double, double> Measure;
	typedef std::pair<unsigned, Measure> Iteration;
	typedef std::vector<Iteration> Iterations;
	typedef T& (Action)(unsigned, T&);
	static double elapsed(Delta<T> const& d, unsigned N, T& x) {
		using namespace std::chrono;
		using clock = high_resolution_clock;
		auto t0 = clock::now();
		d.action(N, x);
		auto t1 = clock::now();
		return Timing::elapsed<double>(t0, t1);
	}
protected:
	Iterations iterations;
	Iteration maxes = {0, {0, 0}};
public:
	/** Decides how to spend N iterations altering x. */
	Action *action;
	/** Decides how to scale measurements. */
	unsigned uScale;
	/** Decides how to label scaled measurements. */
	const char *szScale;
	static T& multiply(unsigned N, T &x)
		{ for(auto l = 0; l < N; l++) x = x * x; return x; }
	static T& add(unsigned N, T &x)
		{ for(auto l = 0; l < N; l++) x = x + x; return x; }
	static T& conjugate(unsigned N, T &x)
		{ for(auto l = 0; l < N; l++) x = *x; return x; }
	Delta(T& t, Action *action = multiply, unsigned uScale = 100000, const char *szScale = "ns"):
		action(action), uScale(uScale), szScale(szScale) {}

	template<class... R>
	Delta(T& x, int M, R &&... r): Delta(x, std::forward<R>(r)...) {
		using namespace std::chrono;

		unsigned N = std::max(0, M);
		maxes.first = std::max(maxes.first, N);

		auto t0 = high_resolution_clock::now();
		action(N, x);
		auto t1 = high_resolution_clock::now();

		double dt = duration_cast<duration<double>>(t1 - t0).count() * uScale, dt_dl = dt / N;
		auto& s = maxes.second;
		s.first = std::max(s.first, dt_dl);
		s.second = std::max(s.second, dt);
		iterations.emplace_back(N, std::make_pair(dt_dl, dt));
	}
	template<class S>
	friend S& operator<<(S &s, Delta const& d) {
		using namespace std;
		auto f = s.flags();
		string labelLeft = "  Loop ", labelRight = ", ";
		unsigned precision = 4,
			loop = 1, loops = d.iterations.size(),
			loopDigits = numDigits(loops), nDigits, singleDigits, totalDigits,
			labelWidth = labelLeft.length() + loopDigits + labelRight.length(),
			singleWidth, totalWidth;
		{
			nDigits = numDigits(d.maxes.first);
			auto const& sec = d.maxes.second;
			unsigned single = sec.first, total = sec.second;
			singleDigits = numDigits(single);
			totalDigits = numDigits(total);
		}
		singleWidth = precision + 1 + singleDigits;
		totalWidth = precision + 1 + totalDigits;
		// Iterate in reverse to restore original argument order
		for(auto it = d.iterations.crbegin(); it < d.iterations.crend(); loop++) {
			auto const& sec = it -> second;
			s << labelLeft << setw(loopDigits) << loop << labelRight
				<< setw(nDigits) << it -> first << " ops: ";
			s << fixed << setprecision(precision);
			{
				auto fill = s.fill();
				s << setw(singleWidth) << setfill(fill)
					<< sec.first << ' ' << d.szScale << "/op ("
					<< setw(totalWidth) << setfill(fill)
					<< sec.second << ' ' << d.szScale << " total)";
			}
			s.setf(f);
			it++;
			if(it != d.iterations.crend()) cout << "\n";
		}
		s << flush;
		return s;
	}
};
template<class P, class... Q>
Delta<P> delta(P &p, Q &&... q)
	{ return {p, std::forward<Q>(q)...}; }
template<class P, class... Q>
Delta<P> delta(P && p, Q &&... q)
	{ return {p, std::forward<Q>(q)...}; }

}

#endif
