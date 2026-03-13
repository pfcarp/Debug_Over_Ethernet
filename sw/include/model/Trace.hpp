#pragma once

#include <cstdint>
#include <vector>
#include <optional>


#include "Packet.hpp"


class Trace {
  
  private:
    std::vector<std::pair<uint64_t, std::vector<Packet::Variant>>> data;
    std::vector<std::pair<uint64_t, uint32_t>> flattened;
    std::vector<std::pair<uint64_t, uint32_t>> accumulated;
    uint64_t max = 0;

  public:
    Packet::Variant* add(uint64_t ts, Packet::Variant pkt);
    uint64_t minTimestamp(bool cumulative) const;
    uint64_t maxTimestamp(bool cumulative) const;
    uint32_t maxCount(bool cumulative) const;
    uint32_t minCount() const;
    const std::vector<std::pair<uint64_t, uint32_t>>& entries(bool cumulative) const;
    std::optional<std::vector<Packet::Variant>> find(uint64_t timestamp, uint32_t occurences, bool cumulative) const;
};
