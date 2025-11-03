#pragma once


#include <vector>


class Color {

  private:
    static int iterator;
    static std::vector<std::vector<double>> map;

  public:
    double red;
    double blue;
    double green;
    double alpha;

    Color();
    Color(double red, double green, double blue, double alpha);
    ~Color();
};
