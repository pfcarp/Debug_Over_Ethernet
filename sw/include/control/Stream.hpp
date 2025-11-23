#pragma once


#include <vector>
#include <memory>
#include <cstdint>


#include "Packet.hpp"
#include "PacketFactory.hpp"


class Stream {

  public:
    // Attributes
    PacketFactory factory = PacketFactory();
    std::vector<std::unique_ptr<Packet::Base>> packets;
    // Methods
    void insert(uint8_t byte);
    ~Stream();

};
