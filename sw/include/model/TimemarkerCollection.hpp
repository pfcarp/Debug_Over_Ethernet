#pragma once


#include <vector>


#include "Timemarker.hpp"


class TimemarkerCollection {
  
  private:
    // Attributes
    std::vector<Timemarker> collection;
    // Methods
    TimemarkerCollection() = default;

  public:
    static TimemarkerCollection& instance();
    TimemarkerCollection(const TimemarkerCollection&) = delete;
    TimemarkerCollection& operator=(const TimemarkerCollection&) = delete;
    void add(Timemarker marker);
    size_t size() const;
    bool empty() const;
    auto begin();
    auto end();
    auto begin() const;
    auto end() const;
    auto cbegin() const;
    auto cend() const;
    auto& operator[](size_t i);
    const auto& operator[](size_t i) const;

};
