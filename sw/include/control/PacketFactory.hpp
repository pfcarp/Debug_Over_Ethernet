#pragma once

#include <cstdint>
#include <memory>
#include <vector>


#include "Packet.hpp"


class PacketFactory {
  
  private:
    // Attributes
    std::unique_ptr<Packet::Base> current = nullptr;
    // Methods
    inline bool isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper);
    void identify(uint8_t id);

  public:
    // Methods
    virtual bool insert(uint8_t byte);
    void consume();
    std::unique_ptr<Packet::Base> get();

};
