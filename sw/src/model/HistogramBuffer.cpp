#include "HistogramBuffer.hpp"
#include "TimedData.hpp"


#include <algorithm>
#include <cstdint>
#include <numeric>


HistogramBuffer::HistogramBuffer(Event* event): Buffer(event) {}


TimedData HistogramBuffer::at(size_t index) const {
  auto it = data.begin();
  std::advance(it, index);
  TimedData res = {.time = it->first, .value = 0};
  if (cumulative)
    res.value = std::accumulate(data.begin(), it, 0, [](uint32_t sum, const auto& item) { return sum + item.second; });
  else
    res.value = it->second;
  return res;
}

void HistogramBuffer::add(TimedData item) {
  data[item.time]++;
}

double HistogramBuffer::ymin() const {
  return 0;
}

double HistogramBuffer::ymax() const {
  double res = 1;
  if (cumulative) {
    res = std::accumulate(data.begin(), data.end(), 0, [](double sum, const auto& item) { return sum + item.second; });
  }
  else if (!data.empty()) {
    res = std::max_element(data.begin(), data.end(), [](const auto& a, const auto& b) { return a.second < b.second; })->second;
  }
  return res;
}

double HistogramBuffer::xmin() const {
  if (data.empty())
    return 0;
  return data.begin()->first;
}

double HistogramBuffer::xmax() const {
  if (data.empty())
    return 1;
  return data.rbegin()->first;
}

size_t HistogramBuffer::size() const {
  return data.size();
}

void HistogramBuffer::clear() {
  data.clear();
}
