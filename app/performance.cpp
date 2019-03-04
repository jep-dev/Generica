#include <iostream>
#include <iomanip>

#include "dual.tpp"
#include "quaternion.tpp"
#include "streams.tpp"

#include "timing.hpp"
#include "utility.hpp"


int main(int argc, const char *argv[]) {
	using namespace std;
	using namespace chrono;
	using namespace Timing;

	int M = 2048, N = 2, temp;

	if(argc > 1) {
		istringstream ss;
		ss.str(argv[1]);
		ss >> temp;
		if(!ss.fail()) M = temp;
		ss.clear();
		if(argc > 2) {
			ss.str(argv[2]);
			ss >> temp;
			if(!ss.fail()) N = temp;
		}
	}

	using T = double;
	int n_digits = max(numDigits(M), numDigits(N));

	auto stopwatch = make_stopwatch(high_resolution_clock());
	auto flags = cout.flags();
	cout << "Quaternion<T>:\n" << delta(Quaternion<T>{1}, M, M*N, M*N*N, M*N*N*N) << endl;
	cout << "  Elapsed (total): " << stopwatch.update().count() << " seconds" << endl;
	stopwatch.advance();
	cout << "DualQuaternion<T>:\n" << delta(DualQuaternion<T>{1}, M, M*N, M*N*N, M*N*N*N) << endl;
	cout << "  Elapsed (total): " << stopwatch.update().count() << " seconds" << endl;
	cout.setf(flags);
}
