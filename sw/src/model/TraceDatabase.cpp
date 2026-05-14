#include "TraceDatabase.hpp"
#include <iostream>


TraceDatabase::TraceDatabase() {
  collection.resize(4);
}


TraceDatabase& TraceDatabase::instance() {
  static TraceDatabase instance;
  return instance;
}


size_t TraceDatabase::size() const {
  return collection.size();
}


bool TraceDatabase::empty() const {
  return collection.empty();
}

bool TraceDatabase::isEmpty(const std::string name) {
  bool empty = true;
  for (auto& c : collection)
    empty &= c.isEmpty(name);
  return empty;
}


std::vector<TraceCollection>::iterator TraceDatabase::begin() {
  return collection.begin();
}


std::vector<TraceCollection>::iterator TraceDatabase::end() {
  return collection.end();
}


std::vector<TraceCollection>::const_iterator TraceDatabase::begin() const {
  return collection.begin();
}


std::vector<TraceCollection>::const_iterator TraceDatabase::end() const {
  return collection.end();
}


std::vector<TraceCollection>::const_iterator TraceDatabase::cbegin() const {
  return collection.cbegin();
}


std::vector<TraceCollection>::const_iterator TraceDatabase::cend() const {
  return collection.cend();
}


TraceCollection& TraceDatabase::operator[](size_t i) {
  return collection[i];
}


const TraceCollection& TraceDatabase::operator[](size_t i) const {
  return collection[i];
}


uint64_t TraceDatabase::minTimestamp() const {
  uint64_t res = UINT64_MAX;
  for (const auto& traces : collection) {
    res = std::min(res, traces.minTimestamp());
  }
  return (res == UINT64_MAX)? 0 : res;
}


uint64_t TraceDatabase::maxTimestamp() const {
  uint64_t res = 0;
  for (const auto& traces : collection) {
    res = std::max(res, traces.maxTimestamp());
  }
  return res;
}


uint32_t TraceDatabase::minCount() const {
  uint32_t res = UINT32_MAX;
  for (const auto& traces : collection) {
    res = std::min(res, traces.minCount());
  }
  return (res == UINT32_MAX)? 0 : res;
}


uint32_t TraceDatabase::maxCount() const {
  uint32_t res = 0;
  for (const auto& traces : collection) {
    res = std::max(res, traces.maxCount());
  }
  return res;
}
