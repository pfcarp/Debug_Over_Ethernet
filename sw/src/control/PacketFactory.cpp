#include "PacketFactory.hpp"


#include <iostream>


#include "Packet.hpp"


inline bool PacketFactory::isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper) {
  return (lower <= a) && (a <= upper);
}


void PacketFactory::identify(uint8_t id) {
  if (id == 0b00000000)
    current = std::make_unique<Packet::Extension>();
  else if (id == 0b00000001)
    current = std::make_unique<Packet::TraceInfo>();
  else if (isInInclusiveRange(id, 0b00000010, 0b00000011))
    current = std::make_unique<Packet::Timestamp>(id);
  else if (id == 0b00000100)
    current = std::make_unique<Packet::TraceOn>();
  else if (id == 0b00000101)
    current = std::make_unique<Packet::FunctionReturn>();
  else if (id == 0b00000110)
    current = std::make_unique<Packet::Exception>();
  else if (id == 0b00000111)
    current = std::make_unique<Packet::ExceptionReturn>();
  else if (id == 0b00001000)
    current = std::make_unique<Packet::Resynchronization>();
  else if (isInInclusiveRange(id, 0b00001100, 0b00001101))
    current = std::make_unique<Packet::CycleCountFormat2>(id);
  else if (isInInclusiveRange(id, 0b00001110, 0b00001111))
    current = std::make_unique<Packet::CycleCountFormat1>(id);
  else if (isInInclusiveRange(id, 0b00010000, 0b00011111))
    current = std::make_unique<Packet::CycleCountFormat3>(id);
  else if (isInInclusiveRange(id, 0b00100000, 0b00100111))
    current = std::make_unique<Packet::NumberedDataSyncMark>(id);
  else if (isInInclusiveRange(id, 0b00101000, 0b00101011))
    current = std::make_unique<Packet::UnnumberedDataSyncMark>(id);
  else if (id == 0b00101101)
    current = std::make_unique<Packet::Commit>();
  else if (isInInclusiveRange(id, 0b00101110, 0b00101111))
    current = std::make_unique<Packet::CancelFormat1>(id);
  else if (isInInclusiveRange(id, 0b00110000, 0b00110011))
    current = std::make_unique<Packet::Mispredict>(id);
  else if (isInInclusiveRange(id, 0b00110100, 0b00110111))
    current = std::make_unique<Packet::CancelFormat2>(id);
  else if (isInInclusiveRange(id, 0b00111000, 0b00111111))
    current = std::make_unique<Packet::CancelFormat3>(id);
  else if (isInInclusiveRange(id, 0b01000000, 0b01000010))
    current = std::make_unique<Packet::ConditionalInstructionFormat2>(id);
  else if (id == 0b01000011)
    current = std::make_unique<Packet::ConditionalFlush>();
  else if (isInInclusiveRange(id, 0b01000100, 0b01000110))
    current = std::make_unique<Packet::ConditionalResultFormat4>(id);
  else if (isInInclusiveRange(id, 0b01001000, 0b01001010))
    current = std::make_unique<Packet::ConditionalResultFormat2>(id);
  else if (isInInclusiveRange(id, 0b01010000, 0b01011111))
    current = std::make_unique<Packet::ConditionalResultFormat3>(id);
  else if (isInInclusiveRange(id, 0b01101000, 0b01101011))
    current = std::make_unique<Packet::ConditionalResultFormat1>(id);
  else if (id == 0b01101100)
    current = std::make_unique<Packet::ConditionalInstructionFormat1>();
  else if (id == 0b01101101)
    current = std::make_unique<Packet::ConditionalInstructionFormat3>();
  else if (isInInclusiveRange(id, 0b01101110, 0b01101111))
    current = std::make_unique<Packet::ConditionalInstructionFormat3>();
  else if (id == 0b01110000)
    current = std::make_unique<Packet::Ignore>();
  else if (isInInclusiveRange(id, 0b01110001, 0b01111111))
    current = std::make_unique<Packet::Event>(id);
  else if (isInInclusiveRange(id, 0b10000000, 0b10000001))
    current = std::make_unique<Packet::Context>(id);
  else if (isInInclusiveRange(id, 0b10000010, 0b10000011))
    current = std::make_unique<Packet::AddressWithContext>(id);
  else if (isInInclusiveRange(id, 0b10000101, 0b10000110))
    current = std::make_unique<Packet::AddressWithContext>(id);
  else if (id == 0b10001000)
    current = std::make_unique<Packet::TimestampMarker>();
  else if (isInInclusiveRange(id, 0b10010000, 0b10010010))
    current = std::make_unique<Packet::ExactMatchAddress>(id);
  else if (isInInclusiveRange(id, 0b10010101, 0b10010110))
    current = std::make_unique<Packet::ShortAddress>(id);
  else if (isInInclusiveRange(id, 0b10011010, 0b10011011))
    current = std::make_unique<Packet::LongAddress>(id);
  else if (isInInclusiveRange(id, 0b10011101, 0b10011110))
    current = std::make_unique<Packet::LongAddress>(id);
  else if (isInInclusiveRange(id, 0b10100000, 0b10101111))
    current = std::make_unique<Packet::Q>(id);
  else if (isInInclusiveRange(id, 0b11000000, 0b11010100))
    current = std::make_unique<Packet::AtomFormat6>(id);
  else if (isInInclusiveRange(id, 0b11010101, 0b11010111))
    current = std::make_unique<Packet::AtomFormat5>(id);
  else if (isInInclusiveRange(id, 0b11011000, 0b11011011))
    current = std::make_unique<Packet::AtomFormat2>(id);
  else if (isInInclusiveRange(id, 0b11011100, 0b11011111))
    current = std::make_unique<Packet::AtomFormat4>(id);
  else if (isInInclusiveRange(id, 0b11100000, 0b11110100))
    current = std::make_unique<Packet::AtomFormat6>(id);
  else if (id == 0b11110101)
    current = std::make_unique<Packet::AtomFormat5>(id);
  else if (isInInclusiveRange(id, 0b11110110, 0b11110111))
    current = std::make_unique<Packet::AtomFormat1>(id);
  else if (isInInclusiveRange(id, 0b11111000, 0b11111111))
    current = std::make_unique<Packet::AtomFormat3>(id);
  else
    current = std::make_unique<Packet::Reserved>();
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
