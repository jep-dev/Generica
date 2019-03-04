#include <functional>
#include <iostream>
#include <iomanip>

#include "pretty.hpp"

#define LINE() " [" << __LINE__ << "]  "

auto lambda = [](char) { return ""; };
auto make_lambda(void) { return [](char) { return ""; }; }
int main(int argc, const char *argv[]) {
	using std::cout;
	using std::endl;
	using std::flush;
	using std::setw;
	using std::string;

	string sp = "  ";

	short u = 0; int v = 1; long w = 2;
	cout << "Explicit: pretty<T...>() -> \"<signature>\"\n"
			<< LINE() << pretty<>() << "\n"
			<< LINE() << pretty<short>() << "\n"
			<< LINE() << pretty<int, long>() << "\n"
		"ADL: pretty(T &&...) -> \"<signature>\"\n"
			/* TODO determine why pretty() actually resolves as pretty<>()!
			 * I just read that when a template and a non-template candidate are available
			 * the non-template candidate is chosen. Is the ADL deferred from candidacy? */
			<< LINE() << '{' << pretty() << "}\n"
			<< LINE() << '{' << pretty(u) << "}\n"
			<< LINE() << '{' << pretty(short(u)) << "} = {" << u << "}\n"
			<< LINE() << '{' << pretty(int(v), long(w)) << "} = {" << v << ", " << w << "}\n"
			<< LINE() << '{' << pretty(lambda) << "}\n"
			<< LINE() << '{' << pretty(make_lambda()) << "}\n"
			<< LINE() << '{' << pretty(make_lambda) << "}\n"
		"Explicit: Pretty<T...>{} -> \"args...\"\n"
			<< LINE() << '{' << Pretty<short>() << "}\n"
			<< LINE() << '{' << Pretty<decltype(lambda)>() << "}\n"
			<< LINE() << '{' << Pretty<decltype(make_lambda)>() << "}\n"
		<< flush;
}
