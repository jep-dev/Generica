#ifndef READER_HPP
#define READER_HPP

///@cond
#include <fstream>
#include <string>
#include <vector>
#include <numeric>
///@endcond

namespace Streams {

bool readLines(const char *fname, std::vector<std::string> &dest, bool *status = nullptr) {
	bool result;
	if(!status) status = &result;
	std::ifstream ifs;
	ifs.open(fname, std::fstream::in);
	if(!ifs.good()) return *status = false;
	for(std::string line; std::getline(ifs, line);)
		dest.emplace_back(line);
	*status = ifs.eof();
	ifs.close();
	return *status;
}
bool readLines(const char *fname, std::string &dest) {
	std::ifstream ifs(fname);
	if(!ifs.good()) return false;

	ifs.seekg(0, std::ios::end);
	auto len = ifs.tellg();
	ifs.seekg(0, std::ios::beg);

	std::string buf(len, '\0');
	ifs.read(&buf[0], len);
	if(ifs.is_open())
		ifs.close();
	return dest = buf, true;
}
bool readLines(std::string const& fname, std::string &dest) {
	return readLines(fname.c_str(), dest);
}

struct Reader {
	typedef std::vector<std::string> type;
	const char *filename = nullptr;
	bool success = true;
	operator bool(void) const { return success; }
	type lines;
	type& get(bool reread = false, bool clear = false) {
		if(reread) source(filename, clear);
		return lines;
	}
	bool source(const char *fname, bool immediate = true, bool clear = false) {
		filename = fname;
		if(clear) lines.clear();
		if(immediate) readLines(filename, lines, &success);
		return *this;
	}
	Reader& operator+=(Reader && r) {
		for(auto && line : r.lines)
			lines.emplace_back(line);
		return *this;
	}
	Reader& operator+=(Reader const& r) {
		for(auto const& line : r.lines)
			lines.push_back(line);
		return *this;
	}
	std::size_t size(void) const {
		return std::accumulate(lines.begin(), lines.end(), std::string(),
			[](std::string const& str, std::string const& s) { return str + "\n" + s; }).length();
	}
	operator std::string(void) const;
	template<class S>
	friend S& operator<<(S& s, Reader const& r) {
		return s << std::string(r), s;
	}
	Reader(void): filename(nullptr) {}
	Reader(const char *fname, bool immediate = true) {
		source(fname, immediate);
	}
};

}

#endif
