#include "DataBuffer.hpp"
#include "TimedData.hpp"


#include <algorithm>
#include <cmath>
#include <numeric>
#include <iostream>


DataBuffer::DataBuffer(Event* event, std::string style): Buffer(event, style) {}

TimedData DataBuffer::at(size_t index) {
  std::lock_guard<std::mutex> lock(m);
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
  std::lock_guard<std::mutex> lock(m);
  data.x.push_back(item.time);
  data.y.push_back(item.value);
  if (data.yacc.empty())
    data.yacc.push_back(item.value);
  else
    data.yacc.push_back(data.yacc.back()+item.value);
  // TODO: wrap in a mutex
  y.min = std::min(y.min, item.value);
  y.max = std::max(y.max, item.value);
  yacc.min = y.min;
  yacc.max = std::max(yacc.max, data.yacc.back());
}

double DataBuffer::ymin() {
  std::lock_guard<std::mutex> lock(m);
  if (data.y.empty())
    return 0;
  return y.min;
}

double DataBuffer::ymax() {
  std::lock_guard<std::mutex> lock(m);
  double res = 1;
  if (cumulative) {
    res = yacc.max;
  }
  else if (!data.y.empty()) {
    res = y.max;
  }
  return res;
}

double DataBuffer::xmin() {
  std::lock_guard<std::mutex> lock(m);
  if (data.x.empty())
    return 0;
  return *data.x.begin();
}

double DataBuffer::xmax() {
  std::lock_guard<std::mutex> lock(m);
  if (data.x.empty())
    return 1;
  return *data.x.rbegin();
}

size_t DataBuffer::size() {
  std::lock_guard<std::mutex> lock(m);
  return data.x.size();
}

void DataBuffer::clear() {
  std::lock_guard<std::mutex> lock(m);
  data.x.clear();
  data.y.clear();
  data.yacc.clear();
  y.min = INFINITY;
  y.max = -INFINITY;
  yacc.min = INFINITY;
  yacc.max = -INFINITY;
}
