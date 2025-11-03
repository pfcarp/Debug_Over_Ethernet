#include "Point.hpp"


Point::Point(Event* event): Buffer(event, ".") {
  data = std::make_pair(0.0, 0.0);
}

TimedData Point::at(size_t index) const {
  TimedData res {.time = data.first, .value = data.second};
  return res;
}

void Point::add(TimedData item) {
  data.first  = item.time;
  data.second = item.value;
}

double Point::ymin() const {
  return -1.1;
  return data.second*0.9;
}

double Point::ymax() const {
  return 1.1;
  return data.second*1.1;
}

double Point::xmin() const {
  return 0.0;
  return data.first*0.9;
}

double Point::xmax() const {
  return 50.0;
  return data.first*1.1;
}

size_t Point::size() const {
  return 1;
}

void Point::clear() {
  data.first  = 0;
  data.second = 0;
}
