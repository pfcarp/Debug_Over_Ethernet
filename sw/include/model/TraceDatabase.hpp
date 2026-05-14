#pragma once


#include <cstddef>
#include <cstdint>
#include <vector>


#include "TraceCollection.hpp"


class TraceDatabase {
  
  private:
    // Attributes
    std::vector<TraceCollection> collection;
    // Methods
    TraceDatabase();

  public:
    static TraceDatabase& instance();
    TraceDatabase(const TraceDatabase&) = delete;
    TraceDatabase& operator=(const TraceDatabase&) = delete;
    size_t size() const;
    bool empty() const;
    bool isEmpty(const std::string name);
    std::vector<TraceCollection>::iterator begin();
    std::vector<TraceCollection>::iterator end();
    std::vector<TraceCollection>::const_iterator begin() const;
    std::vector<TraceCollection>::const_iterator end() const;
    std::vector<TraceCollection>::const_iterator cbegin() const;
    std::vector<TraceCollection>::const_iterator cend() const;
    TraceCollection& operator[](size_t i);
    const TraceCollection& operator[](size_t i) const;
    uint64_t minTimestamp() const;
    uint64_t maxTimestamp() const;
    uint32_t minCount() const;
    uint32_t maxCount() const;

};
