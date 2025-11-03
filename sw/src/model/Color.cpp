#include "Color.hpp"
#include <vector>


int Color::iterator = 0;


std::vector<std::vector<double>> Color::map = {
  {0.15, 0.64, 0.41, 1.00},
  {0.10, 0.37, 0.71, 1.00},
  {0.90, 0.65, 0.04, 1.00},
  {0.78, 0.27, 0.00, 1.00},
  {0.65, 0.11, 0.18, 1.00},
  {0.38, 0.21, 0.51, 1.00}
};


Color::Color() {
  red   = map[iterator][0];
  green = map[iterator][1];
  blue  = map[iterator][2];
  alpha = map[iterator][3];
  iterator = (iterator+1)%map.size();
}


Color::Color(double red, double green, double blue, double alpha): red(red), blue(blue), green(green), alpha(alpha) {}


Color::~Color() {}
