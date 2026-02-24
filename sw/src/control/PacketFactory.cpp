#include "PacketFactory.hpp"


#include <iostream>
#include <memory>

#include "Packet.hpp"
#include "Tools.hpp"


PacketFactory::PacketFactory() {
  packets.reserve(32*1024*1024);
}

std::ostream& operator<<(std::ostream& os, const Packet::Base& e) {
  os << e.asString();
  return os;
}

bool PacketFactory::insert(const uint8_t& byte) {
  // Reserved packet means that it is not set
  if (std::holds_alternative<Packet::Reserved>(packets[current])) {
    factory[byte](*this, byte);
    Packet::setTimestamp(packets[current], timestamp);
  }
  else {
    MEASURE_TIME({
    Packet::insert(packets[current], byte);
    })
  }
  // Separate if for cases where no payload is present
  if (Packet::isDone(packets[current])) {
    current++;
    return true;
  }
  return false;
}

void PacketFactory::setTimestamp(uint64_t t) {
  timestamp = t;
}
