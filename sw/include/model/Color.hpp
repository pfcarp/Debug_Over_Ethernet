#pragma once


#include <vector>


class Color {

  private:
    double f(const double t) const;
    double gamma(const double u) const;
    void generate(const double h, const double c, const double l);

  public:
    double red;
    double blue;
    double green;
    double alpha;

    Color();
    Color(double red, double green, double blue, double alpha);
    ~Color();
};
