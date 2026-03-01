#include "PacketFactory.hpp"


#include <iostream>
#include <memory>

#include "Packet.hpp"
#include "Tools.hpp"


PacketFactory::PacketFactory() {}

std::ostream& operator<<(std::ostream& os, const Packet::Base& e) {
  os << e.asString();
  return os;
}

bool PacketFactory::insert(const uint8_t& byte) {
  // Reserved packet means that it is not set
  if (!current) {
    current = factory[byte](*this, byte);
    Packet::setTimestamp(*current, timestamp);
  }
  else {
    Packet::insert(*current, byte);
  }
  // Separate if for cases where no payload is present
  if (Packet::isDone(*current)) {
    current = nullptr;
    return true;
  }
  return false;
}

void PacketFactory::setTimestamp(uint64_t t) {
  timestamp = t;
}
