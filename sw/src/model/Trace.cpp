#include "Trace.hpp"


Packet::Variant* Trace::add(uint64_t ts, Packet::Variant pkt) {
  if (data.empty() || (data.back().first != ts)) {
    data.emplace_back(ts, std::vector<Packet::Variant>{});
    flattened.emplace_back(ts, 0);
    accumulated.emplace_back(ts, (accumulated.empty())? 0 : accumulated.back().second);
  }
  auto& seq = data.back().second;
  seq.push_back(std::move(pkt));
  if (seq.size() > max) {
    max = seq.size();
  }
  flattened.back().second++;
  accumulated.back().second++;
  return &seq.back();
}

uint64_t Trace::minTimestamp() const {
  if (data.empty()) {
    return 0;
  }
  return data.front().first;
}

uint64_t Trace::maxTimestamp() const {
  if (data.empty()) {
    return 1;
  }
  return data.back().first;
}

uint64_t Trace::maxCount(bool cumulative) const {
  return (cumulative)? accumulated.back().second : max;
}

uint64_t Trace::minCount() const {
  return 0;
}

const std::vector<std::pair<uint64_t, uint32_t>>& Trace::entries(bool cumulative) const {
  if (cumulative) {
    return accumulated;
  }
  return flattened;
}
