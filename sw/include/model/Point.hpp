#pragma once


#include <utility>


#include "Buffer.hpp"
#include "TimedData.hpp"


class Point: public Buffer {

  private:
    // pass   
  
  protected:
    std::pair<double, double> data;

  public:
    Point(Event* event);
    TimedData at(size_t index) override;
    void add(TimedData item) override;
    virtual double ymin() override;
    virtual double ymax() override;
    double xmin() override;
    double xmax() override;
    size_t size() override;
    void clear() override;
};
