#pragma once


#include <map>

#include "Buffer.hpp"
#include "TimedData.hpp"


class HistogramBuffer: public Buffer {

  private:
    // pass

  protected:
    std::map<double, double> data;

  public:
    HistogramBuffer(Event* event);
    TimedData at(size_t index) override;
    virtual void add(TimedData item) override;
    virtual double ymin() override;
    virtual double ymax() override;
    double xmin() override;
    double xmax() override;
    size_t size() override;
    void clear() override;
};

