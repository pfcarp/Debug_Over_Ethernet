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


auto TimemarkerCollection::begin() {
  return collection.begin();
}


auto TimemarkerCollection::end() {
  return collection.end();
}


auto TimemarkerCollection::begin() const {
  return collection.begin();
}


auto TimemarkerCollection::end() const {
  return collection.end();
}


auto TimemarkerCollection::cbegin() const {
  return collection.cbegin();
}


auto TimemarkerCollection::cend() const {
  return collection.cend();
}


auto& TimemarkerCollection::operator[](size_t i) {
  return collection[i];
}


const auto& TimemarkerCollection::operator[](size_t i) const {
  return collection[i];
}
