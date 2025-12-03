#pragma once


#include <cmath>
#include <cstddef>
#include <string>
#include <vector>


#include "TimedData.hpp"
#include "Buffer.hpp"


class DataBuffer: public Buffer {

  protected:
    struct {
      std::vector<double> x;
      std::vector<double> yacc;
      std::vector<double> y;
    } data;
    struct {
      double min =  INFINITY;
      double max = -INFINITY;
    } y;
    struct {
      double min =  INFINITY;
      double max = -INFINITY;
    } yacc;

  public:

    DataBuffer(Event* event, std::string style = "-");
    virtual TimedData at(size_t index) override;
    virtual void add(TimedData item) override;
    virtual double ymin() override;
    virtual double ymax() override;
    double xmin() override;
    double xmax() override;
    size_t size() override;
    void clear() override;
};

