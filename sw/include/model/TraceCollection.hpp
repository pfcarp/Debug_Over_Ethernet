#pragma once


#include <string>
#include <cstdint>
#include <vector>


#include "Packet.hpp"
#include "Trace.hpp"


class TraceCollection {

  private:
    bool cumulative = false;

  public:
    // Attributes
    std::map<std::string, Trace> map;
    // Methods
    Packet::Variant* add(std::string name, uint64_t ts, Packet::Variant pkt);
    bool isEmpty(const std::string name);
    uint64_t minTimestamp() const;
    uint64_t maxTimestamp() const;
    uint32_t maxCount() const;
    uint32_t minCount() const;
    const std::vector<std::pair<uint64_t, uint32_t>>& entries(std::string name) const;
    const std::vector<std::string> getVariants() const;
    void setCumulative(const bool bit);
    const bool isCumulative() const;
    std::string find(uint64_t timestamp, uint32_t occurences);

};
