#pragma once

#include <cstdint>
#include <memory>


#include "Packet.hpp"


class PacketFactory {
  
  private:
    std::unique_ptr<Packet::Base> current = nullptr;

    inline bool isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper);
    void identify(uint8_t id);

  public:

    bool insert(uint8_t byte);
    void consume();
    std::unique_ptr<Packet::Base> get();

};

