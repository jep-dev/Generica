#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>

#include "math.hpp"
#include "reader.hpp"
#include "streams.hpp"
#include "utility.hpp"

int main(int argc, const char *argv[]) {
	using std::string;
	using std::cout;
	using std::endl;
	using std::flush;
	using std::setw;
	using std::left;
	using std::right;
	using std::vector;

	using Streams::Reader;
	using Streams::readLines;
	string self = argv[0], share, vert, frag;
	string delim = "/", sep = " | ";
	auto pos = self.find_last_of(delim);
	if(pos == string::npos)
		delim = "\\", pos = self.find_last_of(delim);
	if(pos == string::npos)
		return cout << "Could not detect path" << endl, 1;

	if(argc < 3) share = self.substr(0, pos+1) + ".." + delim + "share" + delim;
	vert = (argc < 2) ? share + "vert.glsl" : argv[1];
	frag = (argc < 3) ? share + "frag.glsl" : argv[2];
	for(auto const& p : {vert}) {
		cout << "Reading " << p << " line by line; ";

		Reader reader(p.c_str());
		auto &dest = reader.get();
		if(!reader) {
			cout << "failed!" << endl;
		} else {
			cout << "done (" << dest.size() << " lines)";
			auto flags = cout.flags();
			auto i = 0;
			auto mm = Streams::minimax(dest);
			long lw = numDigits(dest.size()), rw = numDigits(mm.second), bw = sep.length(),
				cw = 80 - lw - rw - 2*bw;
			for(auto const& line : dest) {
				cout << '\n' << setw(lw) << i << sep
					<< left << setw(cw) << line << sep
					<< right << setw(rw) << line.length();
				i++;
			}
			cout.setf(flags);
			cout << endl;
		}
	}
	for(auto const& p : {frag}) {
		cout << "Reading " << p << " as a whole; ";
		bool dest;
		//std::string res = readLines(p.c_str(), &dest);
		std::string res;
		if(readLines(p.c_str(), res)) {
		//if(dest) {
			cout << "done (" << std::count(res.cbegin(), res.cend(), '\n')
					<< " lines)\n" << res << flush;
		} else {
			cout << "Failed!\n" << endl;
		}
	}
}
