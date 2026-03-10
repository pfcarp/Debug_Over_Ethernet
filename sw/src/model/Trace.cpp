#include "Trace.hpp"


#include <algorithm>
#include <iostream>


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

uint64_t Trace::minTimestamp(bool cumulative) const {
  if (cumulative) {
    if (accumulated.empty()) {
      return 0;
    }
    return accumulated.front().first;
  }
  else {
    if (data.empty()) {
      return 0;
    }
    return data.front().first;
  }
}

uint64_t Trace::maxTimestamp(bool cumulative) const {
  if (cumulative) {
    if (accumulated.empty()) {
      return 1;
    }
    return accumulated.back().first;
  }
  else {
    if (data.empty()) {
      return 1;
    }
    return data.back().first;
  }
}

uint64_t Trace::minCount() const {
  return 0;
}

uint64_t Trace::maxCount(bool cumulative) const {
  return (cumulative)? accumulated.back().second : max;
}

const std::vector<std::pair<uint64_t, uint32_t>>& Trace::entries(bool cumulative) const {
  if (cumulative) {
    return accumulated;
  }
  return flattened;
}

std::optional<std::vector<Packet::Variant>> Trace::find(uint64_t timestamp, uint32_t occurences, bool cumulative) const {
  if (cumulative) {
    const auto it = std::lower_bound(
      accumulated.begin(), accumulated.end(), timestamp,
      [](const auto& p, uint64_t value){ return p.first < value; }
    );
    if ((it != accumulated.end()) && (it->first == timestamp) && (it->second == occurences)) {
      size_t offset = std::distance(accumulated.begin(), it);
      return data.at(offset).second;
    }
  }
  else {
    const auto it = std::lower_bound(
      flattened.begin(), flattened.end(), timestamp,
      [](const auto& p, uint64_t value){ return p.first < value; }
    );
    if ((it != flattened.end()) && (it->first == timestamp) && (it->second == occurences)) {
      size_t offset = std::distance(flattened.begin(), it);
      return data.at(offset).second;
    }
  }
  return std::nullopt;
}
