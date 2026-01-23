#include "PacketFactory.hpp"


#include <iostream>
#include <memory>

#include "Packet.hpp"


void PacketFactory::identify(const uint8_t& id) {
  current = factory[id](id);
  current->setTimestamp(timestamp);
}

std::ostream& operator<<(std::ostream& os, const Packet::Base& e) {
  os << e.asString();
  return os;
}

bool PacketFactory::insert(uint8_t byte) {
  if (!current) {
    identify(byte);
  }
  else
    current->insert(byte);
  // Separate if for cases where no payload is present
  return current->isDone();
}

void PacketFactory::consume() {
  current.reset();
}

std::unique_ptr<Packet::Base> PacketFactory::get() {
  return std::move(current);
}

void PacketFactory::setTimestamp(uint64_t t) {
  timestamp = t;
}
