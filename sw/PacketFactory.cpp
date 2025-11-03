#include "PacketFactory.hpp"
#include "Packet.hpp"
#include <cstdint>
#include <memory>


inline bool isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper) {
  return (lower <= a) && (a <= upper);
}


void PacketFactory::identify(uint8_t id) {
  if (id == 0b00000000)
    current = new Packet::Extension();
  if (id == 0b00000001)
    current = new Packet::Synchronization();
  else if (isInInclusiveRange(id, 0b00000010, 0b00000011))
    current = new Packet::Timestamp();
  else if (id == 0b00000100)
    current = new Packet::TraceOn();
  else if (id == 0b00000101)
    current = new Packet::FunctionReturn();
  else if (id == 0b00000110)
    current = new Packet::Exception();
  else if (id == 0b00000111)
    current = new Packet::ExceptionReturn();
  else if (id == 0b00001000)
    current = new Packet::Resynchronization();
  else if (isInInclusiveRange(id, 0b00001100, 0b00001101))
    current = new Packet::CycleCountFormat2();
  else if (isInInclusiveRange(id, 0b00001110, 0b00001111))
    current = new Packet::CycleCountFormat1();
  else if (isInInclusiveRange(id, 0b00010000, 0b00011111))
    current = new Packet::CycleCountFormat3();
  else if (isInInclusiveRange(id, 0b00100000, 0b00100111))
    current = new Packet::NumberedDataSyncMark();
  else if (isInInclusiveRange(id, 0b00101000, 0b00101011))
    current = new Packet::UnnumberedDataSyncMark();
  else if (id == 0b00101101)
    current = new Packet::Commit();
  else if (isInInclusiveRange(id, 0b00101110, 0b00101111))
    current = new Packet::CancelFormat1();
  else if (isInInclusiveRange(id, 0b00110000, 0b00110011))
    current = new Packet::Mispredict();
  else if (isInInclusiveRange(id, 0b00110100, 0b00110111))
    current = new Packet::CancelFormat2();
  else if (isInInclusiveRange(id, 0b00111000, 0b00111111))
    current = new Packet::CancelFormat3();
  else if (isInInclusiveRange(id, 0b01000000, 0b01000010))
    current = new Packet::ConditionalInstructionFormat2();
  else if (id == 0b01000011)
    current = new Packet::ConditionalFlush();
  else if (isInInclusiveRange(id, 0b01000100, 0b01000110))
    current = new Packet::ConditionalResultFormat4();
  else if (isInInclusiveRange(id, 0b01001000, 0b01001010))
    current = new Packet::ConditionalResultFormat2();
  else if (isInInclusiveRange(id, 0b01010000, 0b01011111))
    current = new Packet::ConditionalResultFormat3();
  else if (isInInclusiveRange(id, 0b01101000, 0b01101011))
    current = new Packet::ConditionalResultFormat1();
  else if (id == 0b01101100)
    current = new Packet::ConditionalInstructionFormat1();
  else if (id == 0b01101101)
    current = new Packet::ConditionalInstructionFormat3();
  else if (isInInclusiveRange(id, 0b01101110, 0b01101111))
    current = new Packet::ConditionalInstructionFormat3();
  else if (id == 0b01110000)
    current = new Packet::Ignore();
  else if (isInInclusiveRange(id, 0b01110001, 0b01111111))
    current = new Packet::Event();
  else if (isInInclusiveRange(id, 0b10000000, 0b10000001))
    current = new Packet::Context();
  else if (isInInclusiveRange(id, 0b10000010, 0b10000011))
    current = new Packet::AddressWithContext();
  else if (isInInclusiveRange(id, 0b10000101, 0b10000110))
    current = new Packet::AddressWithContext();
  else if (id == 0b10001000)
    current = new Packet::TimestampMarker();
  else if (isInInclusiveRange(id, 0b10010000, 0b10010010))
    current = new Packet::ExactMatchAddress();
  else if (isInInclusiveRange(id, 0b10010101, 0b10010110))
    current = new Packet::ShortAddress();
  else if (isInInclusiveRange(id, 0b10011010, 0b10011011))
    current = new Packet::LongAddress();
  else if (isInInclusiveRange(id, 0b10011101, 0b10011110))
    current = new Packet::LongAddress();
  else if (isInInclusiveRange(id, 0b10100000, 0b10101111))
    current = new Packet::Q();
  else if (isInInclusiveRange(id, 0b11000000, 0b11010100))
    current = new Packet::AtomFormat6();
  else if (isInInclusiveRange(id, 0b11010101, 0b11010111))
    current = new Packet::AtomFormat5();
  else if (isInInclusiveRange(id, 0b11011000, 0b11011011))
    current = new Packet::AtomFormat2();
  else if (isInInclusiveRange(id, 0b11011100, 0b11011111))
    current = new Packet::AtomFormat4();
  else if (isInInclusiveRange(id, 0b11100000, 0b11110100))
    current = new Packet::AtomFormat6();
  else if (id == 0b11110101)
    current = new Packet::AtomFormat5();
  else if (isInInclusiveRange(id, 0b11110110, 0b11110111))
    current = new Packet::AtomFormat1();
  else if (isInInclusiveRange(id, 0b11111000, 0b11111111))
    current = new Packet::AtomFormat3();
  else
    current = new Packet::Reserved();
}

bool PacketFactory::insert(uint8_t byte) {
  if (current == nullptr)
    identify(byte);
  else
    current->insert(byte);
  // Separate if for cases where no payload is present
  return current->isDone();
}
