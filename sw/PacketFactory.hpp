#pragma once

#include <cstdint>
#include <vector>
#include <memory>


#include "Packet.hpp"


class PacketFactory {
  
  private:
    std::vector<uint8_t> buffer;
    std::unique_ptr<Packet> current;

    void identify(uint8_t id);

  public:

    bool insert(uint8_t byte);

};

