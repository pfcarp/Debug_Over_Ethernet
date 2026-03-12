#include "TimemarkerCollection.hpp"


TimemarkerCollection& TimemarkerCollection::instance() {
  static TimemarkerCollection instance;
  return instance;
}


void TimemarkerCollection::add(Timemarker marker) {
  collection.push_back(marker);
}


size_t TimemarkerCollection::size() const {
  return collection.size();
}


bool TimemarkerCollection::empty() const {
  return collection.empty();
}


std::vector<Timemarker>::iterator TimemarkerCollection::begin() {
  return collection.begin();
}


std::vector<Timemarker>::iterator TimemarkerCollection::end() {
  return collection.end();
}


std::vector<Timemarker>::const_iterator TimemarkerCollection::begin() const {
  return collection.begin();
}


std::vector<Timemarker>::const_iterator TimemarkerCollection::end() const {
  return collection.end();
}


std::vector<Timemarker>::const_iterator TimemarkerCollection::cbegin() const {
  return collection.cbegin();
}


std::vector<Timemarker>::const_iterator TimemarkerCollection::cend() const {
  return collection.cend();
}


Timemarker& TimemarkerCollection::operator[](size_t i) {
  return collection[i];
}


const Timemarker& TimemarkerCollection::operator[](size_t i) const {
  return collection[i];
}
