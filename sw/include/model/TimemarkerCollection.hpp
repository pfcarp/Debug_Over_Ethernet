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
    std::vector<Timemarker>::iterator begin();
    std::vector<Timemarker>::iterator end();
    std::vector<Timemarker>::const_iterator begin() const;
    std::vector<Timemarker>::const_iterator end() const;
    std::vector<Timemarker>::const_iterator cbegin() const;
    std::vector<Timemarker>::const_iterator cend() const;
    Timemarker& operator[](size_t i);
    const Timemarker& operator[](size_t i) const;

};
