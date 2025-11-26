#include "DataBuffer.hpp"
#include "TimedData.hpp"


#include <algorithm>
#include <cmath>


DataBuffer::DataBuffer(Event* event, std::string style): Buffer(event, style) {}

TimedData DataBuffer::at(size_t index) const {
  TimedData res = {.time = data.x[index], .value = data.y[index]};
  return res;
}

void DataBuffer::add(TimedData item) {
  data.x.push_back(item.time);
  data.y.push_back(item.value);
  // TODO: wrap in a mutex
  y.min = std::min(y.min, item.value);
  y.max = std::max(y.max, item.value);
}

double DataBuffer::ymin() const {
  if (data.y.empty())
    return 0;
  return y.min;
}

double DataBuffer::ymax() const {
  if (data.y.empty())
    return 1;
  return y.max;
}

double DataBuffer::xmin() const {
  if (data.x.empty())
    return 0;
  return *data.x.begin();
}

double DataBuffer::xmax() const {
  if (data.x.empty())
    return 1;
  return *data.x.rbegin();
}

size_t DataBuffer::size() const {
  return data.x.size();
}

void DataBuffer::clear() {
  data.x.clear();
  data.y.clear();
  y.min = INFINITY;
  y.max = -INFINITY;
}
