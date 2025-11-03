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
    TimedData at(size_t index) const override;
    virtual void add(TimedData item) override;
    virtual double ymin() const override;
    virtual double ymax() const override;
    double xmin() const override;
    double xmax() const override;
    size_t size() const override;
    void clear() override;
};

