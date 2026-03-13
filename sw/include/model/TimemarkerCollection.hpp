#pragma once


#include <cstddef>
#include <set>


#include "Timemarker.hpp"


class TimemarkerCollection {
  
  private:
    // Attributes
    std::set<Timemarker> collection;
    struct {
      std::set<Timemarker>::iterator begin;
      std::set<Timemarker>::iterator end;
    } range;
    // Methods
    TimemarkerCollection();

  public:
    static TimemarkerCollection& instance();
    TimemarkerCollection(const TimemarkerCollection&) = delete;
    TimemarkerCollection& operator=(const TimemarkerCollection&) = delete;
    void insert(Timemarker marker);
    void setScope(uint64_t lower, uint64_t upper);
    void resetScope();
    size_t size() const;
    bool empty() const;
    std::set<Timemarker>::iterator begin();
    std::set<Timemarker>::iterator end();
    std::set<Timemarker>::const_iterator begin() const;
    std::set<Timemarker>::const_iterator end() const;
    std::set<Timemarker>::const_iterator cbegin() const;
    std::set<Timemarker>::const_iterator cend() const;

};
