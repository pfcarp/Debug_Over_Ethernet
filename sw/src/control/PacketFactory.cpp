#include "PacketFactory.hpp"


#include <cstdint>
#include <iostream>
#include <memory>
#include <mutex>
#include <utility>

#include "Packet.hpp"
#include "PacketDatabase.hpp"
#include "Tools.hpp"


PacketFactory::PacketFactory(uint32_t id): map(TraceDatabase::instance()[id]), stream(id) {
  // Every factory shares the same database, only register the packet types once
  static std::once_flag registered;
  std::call_once(registered, [] {
    constexpr auto names = []<size_t... I>(std::index_sequence<I...>) {
      return std::array<const char*, 256>{ nameOf(Tag<factory[I]>{})... };
    }(std::make_index_sequence<256>{});
    for (size_t header = 0; header < names.size(); header++)
      PacketDatabase::instance().addType(header, names[header]);
  });
}

std::ostream& operator<<(std::ostream& os, const Packet::Base& e) {
  os << e.asString();
  return os;
}

bool PacketFactory::insert(const uint8_t& byte) {
  // Reserved packet means that it is not set
  if (!current) {
    current = factory[byte](*this, byte, timestamp);
    start = timestamp;
    length = 0;
  }
  else {
    Packet::insert(*current, byte);
  }
  //keep accumulating the data
  // this logic needs to be double checked for a possible edge case 
  // Also why not combine insert and record?

  //major problem is that the (pressumably) the last whole message is lumped together with the last bytes that are the filler data of the coresight transaction
  //but this should already be dealt by the deformatter?
  if (length < bytes.size())
    bytes[length++] = byte;
  // Separate if for cases where no payload is present
  if (Packet::isDone(*current)) {
    Packet::record(bytes[0], std::span(bytes.data()+1, length-1), start, stream);
    current = nullptr;
    return true;
  }
  return false;
}

void PacketFactory::setTimestamp(uint64_t t) {
  timestamp = t;
}
