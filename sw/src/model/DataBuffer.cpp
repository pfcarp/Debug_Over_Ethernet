#include "DataBuffer.hpp"
#include "TimedData.hpp"


#include <algorithm>
#include <cmath>
#include <numeric>


DataBuffer::DataBuffer(Event* event, std::string style): Buffer(event, style) {}

TimedData DataBuffer::at(size_t index) const {
  auto it = data.y.begin();
  std::advance(it, index);
  TimedData res = {.time = data.x[index], .value = 0};
  if (cumulative)
    res.value = std::accumulate(data.y.begin(), it, 0, [](uint32_t sum, const auto& item) { return sum + item; });
  else
    res.value = data.y[index];
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
  double res = 1;
  if (cumulative) {
    res = std::accumulate(data.y.begin(), data.y.end(), 0, [](double sum, const auto& item) { return sum + item; });
  }
  else if (!data.y.empty()) {
    res = y.max;
  }
  return res;
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
