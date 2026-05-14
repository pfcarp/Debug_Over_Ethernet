#include "TimemarkerCollection.hpp"


#include "Color.hpp"


TimemarkerCollection::TimemarkerCollection() {
  range.begin = collection.begin();
  range.end = collection.end();
}


TimemarkerCollection& TimemarkerCollection::instance() {
  static TimemarkerCollection instance;
  return instance;
}


void TimemarkerCollection::insert(Timemarker marker) {
  collection.insert(marker);
  resetScope();
}


void TimemarkerCollection::setScope(uint64_t lower, uint64_t upper) {
  Timemarker begin(lower, Color(), "");
  Timemarker end(upper, Color(), "");
  range.begin = collection.lower_bound(begin);
  range.end = collection.upper_bound(end);
}

void TimemarkerCollection::resetScope() {
  range.begin = collection.begin();
  range.end = collection.end();
}


size_t TimemarkerCollection::size() const {
  return std::distance(range.begin, range.end);
}


bool TimemarkerCollection::empty() const {
  return size() == 0;
}


std::set<Timemarker>::iterator TimemarkerCollection::begin() {
  return range.begin;
}


std::set<Timemarker>::iterator TimemarkerCollection::end() {
  return range.end;
}


std::set<Timemarker>::const_iterator TimemarkerCollection::begin() const {
  return range.begin;
}


std::set<Timemarker>::const_iterator TimemarkerCollection::end() const {
  return range.end;
}


std::set<Timemarker>::const_iterator TimemarkerCollection::cbegin() const {
  return range.begin;
}


std::set<Timemarker>::const_iterator TimemarkerCollection::cend() const {
  return range.end;
}
