#pragma once

#include <cstdint>
#include <memory>
#include <vector>


#include "Packet.hpp"


class PacketFactory {
  
  private:
    // Attributes
    std::unique_ptr<Packet::Base> current = nullptr;
    uint64_t timestamp = 0;
    // Methods
    constexpr bool isInInclusiveRange(const uint8_t& a, const uint8_t& lower, const uint8_t& upper) const;
    constexpr void identify(const uint8_t& id);

  public:
    // Methods
    virtual bool insert(uint8_t byte);
    void consume();
    std::unique_ptr<Packet::Base> get();
    void setTimestamp(uint64_t t);

};
