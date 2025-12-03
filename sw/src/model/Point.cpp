#include "Point.hpp"


Point::Point(Event* event): Buffer(event, ".") {
  data = std::make_pair(0.0, 0.0);
}

TimedData Point::at(size_t index) {
  std::lock_guard<std::mutex> lock(m);
  TimedData res {.time = data.first, .value = data.second};
  return res;
}

void Point::add(TimedData item) {
  std::lock_guard<std::mutex> lock(m);
  data.first  = item.time;
  data.second = item.value;
}

double Point::ymin() {
  std::lock_guard<std::mutex> lock(m);
  return data.second*0.9;
}

double Point::ymax() {
  std::lock_guard<std::mutex> lock(m);
  return data.second*1.1;
}

double Point::xmin() {
  std::lock_guard<std::mutex> lock(m);
  return data.first*0.9;
}

double Point::xmax() {
  std::lock_guard<std::mutex> lock(m);
  return data.first*1.1;
}

size_t Point::size() {
  return 1;
}

void Point::clear() {
  std::lock_guard<std::mutex> lock(m);
  data.first  = 0;
  data.second = 0;
}
