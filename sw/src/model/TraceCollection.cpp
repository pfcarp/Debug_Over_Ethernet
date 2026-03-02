#include "TraceCollection.hpp"


#include <algorithm>


Packet::Variant* TraceCollection::add(std::string name, uint64_t ts, Packet::Variant pkt) {
  return map[name].add(ts, pkt);
}

uint64_t TraceCollection::minTimestamp() const {
  uint64_t res = UINT64_MAX;
  for (const auto& pair : map) {
    res = std::min(res, pair.second.minTimestamp());
  }
  return res;
}

uint64_t TraceCollection::maxTimestamp() const {
  uint64_t res = 0;
  for (const auto& pair : map) {
    res = std::max(res, pair.second.maxTimestamp());
  }
  return res;
}

uint64_t TraceCollection::minCount() const {
  uint64_t res = UINT64_MAX;
  for (const auto& pair : map) {
    res = std::min(res, pair.second.minCount());
  }
  return res;
}

uint64_t TraceCollection::maxCount() const {
  uint64_t res = 0;
  for (const auto& pair : map) {
    res = std::max(res, pair.second.maxCount(cumulative));
  }
  return res;
}

const Trace& TraceCollection::entries(std::string name) const {
  return map.at(name);
}

const std::vector<std::string> TraceCollection::getVariants() const {
  std::vector<std::string> res;
  for (const auto& entry : map) {
    res.emplace_back(entry.first);
  }
  return res;
}

void TraceCollection::setCumulative(const bool bit) {
  cumulative = bit;
}

const bool TraceCollection::isCumulative() const {
  return cumulative;
}
