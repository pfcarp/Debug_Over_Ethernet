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
      std::vector<double> y;
    } data;
    struct {
      double min =  INFINITY;
      double max = -INFINITY;
    } y;

  public:

    DataBuffer(Event* event, std::string style = "-");
    virtual TimedData at(size_t index) const override;
    virtual void add(TimedData item) override;
    virtual double ymin() const override;
    virtual double ymax() const override;
    double xmin() const override;
    double xmax() const override;
    size_t size() const override;
    void clear() override;
};

