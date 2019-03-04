#include <iostream>
#include <sstream>

#include "bspline.hpp"
#include "point.hpp"

int main(int argc, const char *argv[]) {
	typedef float S;
	typedef Point<S> C;
	static constexpr unsigned
			UM = 4, UD = 2, UK = UM + UD + 1,
			VM = UM, VD = UD, VK = VM + VD + 1;

	C controls[UM+1][VM+1] = {0};
	for(unsigned i = 0; i <= UM; i++)
		for(unsigned j = 0; j <= VM; j++)
			controls[i][j] += -1_x + 2_x*j/(VM+1) - 1_y + 2_y*i/(UM+1);
	controls[(UM+1)/2][(VM+1)/2] += 4_z;

	S uKnots[] = {0, 0, 0, .2, .4, .6, .8, 1, 1, 1},
		vKnots[] = {0, 0, 0, .2, .4, .6, .8, 1, 1, 1};
	typedef decltype(uKnots) UKnots;
	typedef decltype(vKnots) VKnots;

	std::string line;
	S u = 0, v = 0;
	while(std::getline(std::cin, line)) {
		std::istringstream iss(line);
		S ui = u, vi = v;
		iss >> ui;
		if(!iss.fail()) {
			u = ui;
			iss >> vi;
			if(!iss.fail()) {
				v = vi;
			}
		}
		std::cout << bSpline<UD, VD, UM+1, VM+1, C, UKnots, VKnots, S>(controls,
				uKnots, vKnots, u, v);
	}


}
