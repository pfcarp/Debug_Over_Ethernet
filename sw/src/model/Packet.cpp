#include "Packet.hpp"
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>


void Packet::Base::insert(const uint8_t& byte) {
  if (iterator < Packet::bytesize) {
    raw[iterator] = byte;
    iterator++;
  }
}

std::string Packet::Base::asString() const {
  return std::format("[@{}] ", timestamp);
}

bool Packet::Base::isDone() const {
  return iterator == Packet::bytesize;
}

void Packet::Base::markDone() {
  iterator = Packet::bytesize;
}

uint8_t Packet::Base::getIterator() const {
  return iterator;
}

void Packet::Base::setTimestamp(uint64_t t) {
  timestamp = t;
}


Packet::Extension::Extension(const uint8_t& header) {
  Packet::Base::insert(header);
}

bool Packet::Extension::isASync() const {
  return raw[1] == 0;
}

bool Packet::Extension::isDiscard() const {
  return raw[1] == 3;
}

bool Packet::Extension::isOverflow() const {
  return raw[1] == 5;
}

bool Packet::Extension::isBranchFutureFlush() const {
  return raw[1] == 7;
}

void Packet::Extension::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  // 63 as iterator will be incremented in the Base::insert()
  if (iterator == 2)
    iterator = (byte > 0)? Packet::bytesize : Packet::bytesize-10;
}

std::string Packet::Extension::asString() const {
  if (isASync()) {
    return Packet::Base::asString()+"ASync.";
  }
  else if (isDiscard()) {
    return Packet::Base::asString()+"Discard.";
  }
  else if (isOverflow()) {
    return Packet::Base::asString()+"Overflow";
  }
  else if (isBranchFutureFlush()) {
    return Packet::Base::asString()+"BranchFutureFlush";
  }
  return Packet::Base::asString()+"No match found!";
}


Packet::TraceInfo::TraceInfo(const uint8_t& header) {
  Packet::Base::insert(header);
  counter = 0;
}

uint32_t Packet::TraceInfo::findInfoStartIndex() const {
  uint32_t index = 1; // Skip header
  while (raw[index] > 127) {
    index++;
  }
  index++; // always increment one more time to get the starting point of the Info section.
  return index;
}

bool Packet::TraceInfo::hasInfo() const {
  return (0b00000001 & raw[1]);
}

bool Packet::TraceInfo::hasKey() const {
  return (0b00000010 & raw[1]) == 0b00000010;
}

bool Packet::TraceInfo::hasSpec() const {
  return (0b00000100 & raw[1]) == 0b00000100;
}

bool Packet::TraceInfo::hasCyct() const {
  return (0b00001000 & raw[1]) == 0b00001000;
}

bool Packet::TraceInfo::isDone() const {
  return (counter == 5);
}

void Packet::TraceInfo::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  if (counter == 0) { // PLCTL
    if (byte < 128) {
      if (hasInfo())      { counter = 1; }
      else if (hasKey())  { counter = 2; }
      else if (hasSpec()) { counter = 3; }
      else if (hasCyct()) { counter = 4; }
      else                { counter = 5; }
    }
  }
  else if (counter == 1) {
    if (byte < 128) {
      if (hasKey())       { counter = 2; }
      else if (hasSpec()) { counter = 3; }
      else if (hasCyct()) { counter = 4; }
      else                { counter = 5; }
    }
  }
  else if (counter == 2) {
    if (byte < 128) {
      if (hasSpec())      { counter = 3; }
      else if (hasCyct()) { counter = 4; }
      else                { counter = 5; }
    }
  }
  else if (counter == 3) {
    if (byte < 128) {
      if (hasCyct())      { counter = 4; }
      else                { counter = 5; }
    }
  }
  else if (counter == 4) {
    if (byte < 128) {
      counter = 5;
    }
  }
}

std::string Packet::TraceInfo::asString() const {
  std::string base = Packet::Base::asString()+"Trace info";
  if (Packet::TraceInfo::hasInfo()) {
    uint32_t i = findInfoStartIndex();
    do {
      bool cc_enabled = raw[i] & 0b00000001;
      if (cc_enabled)
        base += " (cycle count enabled)";
      else
        base += " (cycle count disabled)";
      switch(raw[i] & 0b00001110) {
        case 0b00000000: base += " (Tracing of conditional non-branch instructions is disabled)"; break;
        case 0b00000010: base += " (Conditional load instructions are traced)"; break;
        case 0b00000100: base += " (Conditional store instructions are traced)"; break;
        case 0b00000110: base += " (Conditional load and store instructions are traced)"; break;
        case 0b00001110: base += " (All conditional non-branch instructions are traced)"; break;
        default   : break;
      }
    } while (raw[i] > 127);
  }
  return base;
}


Packet::Timestamp::Timestamp(const uint8_t& header) {
  Packet::Base::insert(header);
}

bool Packet::Timestamp::hasCount() const {
  return raw[0] & 0x01;
}

uint64_t Packet::Timestamp::getTS() const {
  uint64_t ts = 0;
  uint32_t i = 1;
  while ((raw[i] > 128) && (i < 10)) {
    ts |= static_cast<uint64_t>(raw[i] & 0x7f) << (i*7);
    i++;
  }
  return ts;
}

uint32_t Packet::Timestamp::getCount() const {
  uint32_t count = 0;
  uint32_t i = 10;
  while ((raw[i] > 128) && (i < 13)) {
    count |= static_cast<uint32_t>(raw[i] & 0x7f) << ((i-10)*7);
    i++;
  }
  return count;
}

void Packet::Timestamp::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  if (counter < 10) {
    Packet::Base::insert(byte);
    if ((byte < 128) || (counter == 9)) {
      if (!hasCount())
        Packet::Base::markDone();
      counter = 10;
    }
    else {
      counter++;
    }
  }
  else if (counter < 13) {
    Packet::Base::insert(byte);
    if ((byte < 128) || (counter == 12)) {
      Packet::Base::markDone();
    }
    else {
      counter++;
    }
  }
}

std::string Packet::Timestamp::asString() const {
  return Packet::Base::asString()+std::format("Timestamp (TS = {}, COUNT = {})", Packet::Timestamp::getTS(), Packet::Timestamp::getCount());
}


Packet::TraceOn::TraceOn(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::TraceOn::asString() const {
  return Packet::Base::asString()+"Trace on.";
}


Packet::FunctionReturn::FunctionReturn(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::FunctionReturn::asString() const {
  return Packet::Base::asString()+"Function return.";
}


Packet::ExceptionReturn::ExceptionReturn(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::ExceptionReturn::asString() const {
  return Packet::Base::asString()+"Exception return.";
}


Packet::Resynchronization::Resynchronization(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::Resynchronization::asString() const {
  return Packet::Base::asString()+"Resynchronization.";
}


Packet::Reserved::Reserved(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::Reserved::asString() const {
  return Packet::Base::asString()+"Reserved.";
}


Packet::CycleCountFormat2::CycleCountFormat2(const uint8_t& header) {
  Packet::Base::insert(header);
}

uint8_t Packet::CycleCountFormat2::getF() const {
  return 0x01 & raw[0];
}

uint8_t Packet::CycleCountFormat2::getA() const {
  return (0xf0 & raw[1]) >> 4;
}

uint8_t Packet::CycleCountFormat2::getB() const {
  return (0x0f & raw[1]);
}

void Packet::CycleCountFormat2::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  Packet::Base::markDone();
}

std::string Packet::CycleCountFormat2::asString() const {
  return Packet::Base::asString()+"Cycle count format 2.";
}


Packet::CycleCountFormat1::CycleCountFormat1(const uint8_t& header) {
  Packet::Base::insert(header);
}

uint8_t Packet::CycleCountFormat1::getU() const {
  return 0x01 & raw[0];
}

void Packet::CycleCountFormat1::insert(const uint8_t& byte) {
  Packet::Base::insert(byte & 0x7f);
  if (counter == 0) {
    if ((byte < 128) || (iterator == Packet::bytesize)) {
      if (!Packet::CycleCountFormat1::getU()) {
        Packet::Base::markDone();
      }
      else {
        counter = 1;
      }
    }
  }
  else if (Packet::CycleCountFormat1::getU()) {
    if (byte < 128) { // Assumes that the SBZ are held!
      Packet::Base::markDone();
    }
  }
}

std::string Packet::CycleCountFormat1::asString() const {
  return Packet::Base::asString()+"Cycle count format 1.";
}


Packet::CycleCountFormat3::CycleCountFormat3(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

uint8_t Packet::CycleCountFormat3::getAA() const {
  return (0b00001100 & raw[0]) >> 2;
}

uint8_t Packet::CycleCountFormat3::getBB() const {
  return (0b00000011 & raw[0]);
}

std::string Packet::CycleCountFormat3::asString() const {
  return Packet::Base::asString()+"Cycle count format 3.";
}


Packet::NumberedDataSyncMark::NumberedDataSyncMark(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

uint8_t Packet::NumberedDataSyncMark::getNum() const {
  return 0b00000111 & raw[0];
}

std::string Packet::NumberedDataSyncMark::asString() const {
  return Packet::Base::asString()+"Numbered data sync mark.";
}


Packet::UnnumberedDataSyncMark::UnnumberedDataSyncMark(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

uint8_t Packet::UnnumberedDataSyncMark::getA() const {
  return 0b00000111 & raw[0];
}

std::string Packet::UnnumberedDataSyncMark::asString() const {
  return Packet::Base::asString()+"Unnumbered data sync mark.";
}


Packet::Commit::Commit(const uint8_t& header) {
  Packet::Base::insert(header);
}

void Packet::Commit::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  if ((byte < 128) || (iterator == Packet::bytesize)) {
    Packet::Base::markDone();
  }
}

std::string Packet::Commit::asString() const {
  return Packet::Base::asString()+"Commit.";
}


Packet::CancelFormat1::CancelFormat1(const uint8_t& header) {
  Packet::Base::insert(header);
}

uint8_t Packet::CancelFormat1::getM() const {
  return 0b00000001 & raw[0];
}

void Packet::CancelFormat1::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  if ((byte < 128) || (iterator == Packet::bytesize)) {
    Packet::Base::markDone();
  }
}

std::string Packet::CancelFormat1::asString() const {
  return Packet::Base::asString()+"Cancel format 1.";
}


Packet::Mispredict::Mispredict(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::Mispredict::getA() const {
  return 0b00000011 & raw[0];
}

std::string Packet::Mispredict::asString() const {
  return Packet::Base::asString()+"Mispredict (A = "+std::to_string(static_cast<int>(getA()))+")";
}


Packet::CancelFormat2::CancelFormat2(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::CancelFormat2::getA() const {
  return 0b00000011 & raw[0];
}

std::string Packet::CancelFormat2::asString() const {
  return Packet::Base::asString()+"CancelFormat2 (A = "+std::to_string(static_cast<int>(getA()))+")";
}


Packet::CancelFormat3::CancelFormat3(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::CancelFormat3::getCC() const {
  return 0b00000110 & raw[0];
}

uint8_t Packet::CancelFormat3::getA() const {
  return 0b00000001 & raw[0];
}

std::string Packet::CancelFormat3::asString() const {
  return Packet::Base::asString()+"CancelFormat3 (CC = "+std::to_string(static_cast<int>(getCC()))+", A = "+std::to_string(static_cast<int>(getA()))+")";
}


Packet::ConditionalInstructionFormat2::ConditionalInstructionFormat2(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::ConditionalInstructionFormat2::getCI() const {
  return 0b00000011 & raw[0];
}

std::string Packet::ConditionalInstructionFormat2::asString() const {
  return Packet::Base::asString()+"Conditional instruction format 2 (CI = "+std::to_string(static_cast<int>(getCI()))+")";
}


Packet::ConditionalFlush::ConditionalFlush(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::ConditionalFlush::asString() const {
  return Packet::Base::asString()+"Conditional flush.";
}


Packet::ConditionalResultFormat4::ConditionalResultFormat4(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::ConditionalResultFormat4::getT() const {
  return 0b00000011 & raw[0];
}

std::string Packet::ConditionalResultFormat4::asString() const {
  return Packet::Base::asString()+std::format("Conditional result format 4 (TOKEN = {})", getT());
}


Packet::ConditionalResultFormat2::ConditionalResultFormat2(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::ConditionalResultFormat2::getT() const {
  return 0b00000011 & raw[0];
}

uint8_t Packet::ConditionalResultFormat2::getK() const {
  return (0b00000100 & raw[0]) >> 2;
}

std::string Packet::ConditionalResultFormat2::asString() const {
  return Packet::Base::asString()+"Condition result format 2.";
}


Packet::ConditionalResultFormat3::ConditionalResultFormat3(const uint8_t& header) {
  Packet::Base::insert(0b00001111 & header);
}
  
void Packet::ConditionalResultFormat3::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  Packet::Base::markDone();
}

uint16_t Packet::ConditionalResultFormat3::getToken() const {
  uint16_t token = 0;
  token |= static_cast<uint16_t>(raw[0] & 0b00001111) << 8;
  token |= static_cast<uint16_t>(raw[1]);
  return token;
}

std::string Packet::ConditionalResultFormat3::asString() const {
  return Packet::Base::asString()+"Condition result format 3.";
}


Packet::ConditionalResultFormat1::ConditionalResultFormat1(const uint8_t& header) {
  counter = ((0b00000100 & header) == 0b00000100);
}
  
bool Packet::ConditionalResultFormat1::isDone() const {
  return (counter == 2);
}

void Packet::ConditionalResultFormat1::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  counter += (byte < 128);
}

std::string Packet::ConditionalResultFormat1::asString() const {
  return Packet::Base::asString()+"Conditional result format 1.";
}


Packet::ConditionalInstructionFormat1::ConditionalInstructionFormat1(const uint8_t& header) {
  Packet::Base::insert(header);
}

void Packet::ConditionalInstructionFormat1::insert(const uint8_t& byte) {
  Packet::Base::insert(0b01111111 & byte);
  if (byte < 128) {
    Packet::Base::markDone();
  }
}

std::string Packet::ConditionalInstructionFormat1::asString() const {
  return Packet::Base::asString()+"Conditional instruction format 1.";
}


Packet::ConditionalInstructionFormat3::ConditionalInstructionFormat3(const uint8_t& header) {
  Packet::Base::insert(header);
}

uint8_t Packet::ConditionalInstructionFormat3::getZ() const {
  return 0b00000001 & raw[1];
}

uint8_t Packet::ConditionalInstructionFormat3::getNum() const {
  return (0b01111110 & raw[1]) >> 1;
}

void Packet::ConditionalInstructionFormat3::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  Packet::Base::markDone();
}

std::string Packet::ConditionalInstructionFormat3::asString() const {
  return Packet::Base::asString()+"Condition instruction format 3.";
}


Packet::Ignore::Ignore(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::Ignore::asString() const {
  return Packet::Base::asString()+"Ignore.";
}


Packet::Event::Event(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
std::string Packet::Event::asString() const {
  return Packet::Base::asString()+"Event (#0 = "+std::to_string(static_cast<int>(hasEvent(0)))+", #1 = "+std::to_string(static_cast<int>(hasEvent(1)))+", #2 = "+std::to_string(static_cast<int>(hasEvent(2)))+", #3 = "+std::to_string(static_cast<int>(hasEvent(3)))+").";
}

bool Packet::Event::hasEvent(const uint8_t& index) const {
  switch (index) {
    case 0 : return (0b00000001 & raw[0]) == 0b00000001;
    case 1 : return (0b00000010 & raw[0]) == 0b00000010;
    case 2 : return (0b00000100 & raw[0]) == 0b00000100;
    case 3 : return (0b00001000 & raw[0]) == 0b00001000;
    default: return false;
  }
}


Packet::Context::Context(const uint8_t& header) {
  Packet::Base::insert(header);
  // No payload expected
  if (!hasPayload())
    Packet::Base::markDone();
}

bool Packet::Context::hasPayload() const {
  return raw[0] & 0b00000001;
}

bool Packet::Context::hasVMID() const {
  return hasPayload() & ((0b01000000 & raw[1]) == 0b01000000);
}

bool Packet::Context::hasContextID() const {
  return hasPayload() & ((0b10000000 & raw[1]) == 0b10000000);
}

uint8_t Packet::Context::getEL() const {
  return hasPayload() & (0b00000011 & raw[1]);
}

uint8_t Packet::Context::getSF() const {
  return hasPayload() & ((0b00010000 & raw[1]) == 0b00010000);
}

uint8_t Packet::Context::getNS() const {
  return hasPayload() & ((0b00100000 & raw[1]) == 0b00100000);
}

uint32_t Packet::Context::getVMID() const {
  uint32_t vmid = 0;
  if (hasVMID()) {
    for (uint32_t i = 0; i < 4; i++) {
      vmid |= static_cast<uint32_t>(raw[i+2]) << (i*8);
    }
  }
  return vmid;
}

uint32_t Packet::Context::getContextID() const {
  uint32_t contextid = 0;
  if (hasContextID()) {
    uint32_t offset = (hasVMID())? 6 : 2;
    for (uint32_t i = 0; i < 4; i++) {
      contextid |= static_cast<uint32_t>(raw[offset+i]) << (i*8);
    }
  }
  return contextid;
}

void Packet::Context::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  if (((hasVMID()+hasContextID()) << 2) == iterator-2)
    Packet::Base::markDone();
}

std::string Packet::Context::asString() const {
  return Packet::Base::asString()+std::format("Context (EL = {}, SF = {}, NS = {}, VMID = 0x{:016X}, CONTEXTID = 0x{:016X})", getEL(), getSF(), getNS(), getVMID(), getContextID());
}


Packet::AddressWithContext::AddressWithContext(const uint8_t& header) {
  Packet::Base::insert(header);
}

uint8_t Packet::AddressWithContext::getOffset() const {
  uint8_t base = raw[0] & 0b00000111;
  return 1 << ((base == 0b00000101) || (base == 0b00000010));
}
  
uint8_t Packet::AddressWithContext::getLength() const {
  uint8_t base = raw[0] & 0b00000111;
  return 4 << ((base == 0b00000101) || (base == 0b00000110));
}
  
bool Packet::AddressWithContext::hasVMID() const {
  return (0b01000000 & raw[1+getLength()]) == 0b01000000;
}

bool Packet::AddressWithContext::hasContextID() const {
  return (0b10000000 & raw[1+getLength()]) == 0b10000000;
}

uint8_t Packet::AddressWithContext::getEL() const {
  return 0b00000011 & raw[1+getLength()];
}

uint8_t Packet::AddressWithContext::getSF() const {
  return (0b00010000 & raw[1+getLength()]) == 0b00010000;
}

uint8_t Packet::AddressWithContext::getNS() const {
  return (0b00100000 & raw[1+getLength()]) == 0b00100000;
}

uint64_t Packet::AddressWithContext::getAddress() const {
  uint64_t address = 0;
  for (uint32_t i = 0; i < getOffset(); i++) {
    address |= static_cast<uint64_t>(raw[i+1]) << ((i*8)+getOffset()-i);
  }
  for (uint32_t i = getOffset(); i < getLength(); i++) {
    address |= static_cast<uint64_t>(raw[i+1]) << (i*8);
  }
  return address;
}

uint32_t Packet::AddressWithContext::getVMID() const {
  uint32_t vmid = 0;
  if (hasVMID()) {
    uint32_t offset = 2+getLength();
    for (uint32_t i = 0; i < 4; i++) {
      vmid |= static_cast<uint32_t>(raw[offset+i]) << (i*8);
    }
  }
  return vmid;
}

uint32_t Packet::AddressWithContext::getContextID() const {
  uint32_t contextid = 0;
  if (hasContextID()) {
    uint32_t offset = 2+getLength()+(4*hasVMID());
    for (uint32_t i = 0; i < 4; i++) {
      contextid |= static_cast<uint32_t>(raw[offset+i]) << (i*8);
    }
  }
  return contextid;
}

void Packet::AddressWithContext::insert(const uint8_t& byte) {
  Packet::Base::insert(byte);
  uint8_t offset = 2+getLength();
  if (iterator >= offset)
    if (((hasVMID()+hasContextID()) << 2) == iterator-offset)
      Packet::Base::markDone();
}

std::string Packet::AddressWithContext::asString() const {
  return Packet::Base::asString()+std::format("Address with context (A = 0x{:016X}, Context ID = {})", getAddress(), getContextID());
}


Packet::TimestampMarker::TimestampMarker(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

std::string Packet::TimestampMarker::asString() const {
  return Packet::Base::asString()+"Timestamp marker.";
}


Packet::ExactMatchAddress::ExactMatchAddress(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::ExactMatchAddress::getQE() const {
  return 0b00000011 & raw[0];
}

std::string Packet::ExactMatchAddress::asString() const {
  return Packet::Base::asString()+"exact match address.";
}


Packet::ShortAddress::ShortAddress(const uint8_t& header) {
  Packet::Base::insert(header);
}
  
bool Packet::ShortAddress::isIS0() const {
  return raw[0] == 0b10010101;
}

uint32_t Packet::ShortAddress::getAddress() const {
  uint32_t address = 0;
  address |= static_cast<uint32_t>(raw[1] & 0x7f) << (1+isIS0());
  if (raw[1] & 0x80)
    address |= static_cast<uint32_t>(raw[2]) << (8+isIS0());
  return address;
}

void Packet::ShortAddress::insert(const uint8_t& byte) {
  if (iterator == 1) {
    Packet::Base::insert(byte); // Important to keep the C field for getAddress!
    if (byte < 128) {
      Packet::Base::markDone();
    }
  }
  else {
    Packet::Base::insert(byte);
    Packet::Base::markDone();
  }
}

std::string Packet::ShortAddress::asString() const {
  return Packet::Base::asString()+std::format("Short address (0x{:04X})", getAddress());
}


Packet::LongAddress::LongAddress(const uint8_t& header) {
  Packet::Base::insert(header);
}

// I.e., is IS0
uint8_t Packet::LongAddress::getOffset() const {
  return 1 << ((raw[0] == 0b10011010) || (raw[0] == 0b10011101));
}

uint8_t Packet::LongAddress::getLength() const {
  return 4 << ((raw[0] == 0b10011101) || (raw[0] == 0b10011110));
}

uint64_t Packet::LongAddress::getAddress() const {
  uint64_t address = 0;
  for (uint32_t i = 0; i < 2; i++) {
    address |= static_cast<uint64_t>(raw[1+i]) << ((i*8)+(getOffset()-i));
  }
  for (uint32_t i = 2; i < getLength(); i++) {
    address |= static_cast<uint64_t>(raw[1+i]) << (i*8);
  }
  return address;
}

void Packet::LongAddress::insert(const uint8_t& byte) {
  uint8_t mask = 0xff >> (iterator <= getOffset());
  Packet::Base::insert(byte & mask);
  if (iterator == 1+getLength())
    Packet::Base::markDone();
}

std::string Packet::LongAddress::asString() const {
  return Packet::Base::asString()+std::format("Long address (0x{:016X})", getAddress());
}


Packet::Q::Q(const uint8_t& header) {
  Packet::Base::insert(header);
}

bool Packet::Q::hasAddress() const {
  return (raw[0] == 0b10100101) || (raw[0] == 0b10100110) || (raw[0] == 0b10101010) || (raw[0] == 0b10101011);
}

bool Packet::Q::hasCount() const {
  return (raw[0] == 0b10100000) || (raw[0] == 0b10100001) || (raw[0] == 0b10100010) || (raw[0] == 0b10101100) || (raw[0] == 0b10100101) || (raw[0] == 0b10100110) || (raw[0] == 0b10101010) || (raw[0] == 0b10101011);
}

bool Packet::Q::isLong() const {
  return (raw[0] == 0b10101010) || (raw[0] == 0b10101011);
}

uint8_t Packet::Q::getOffset() const {
  return 1 << ((raw[0] == 0b10100101) || (raw[0] == 0b10101010));
}

uint8_t Packet::Q::getLength() const {
  return 2 << isLong();
}

uint64_t Packet::Q::getAddress() const {
  uint64_t address = 0;
  if (hasAddress()) {
    if (!isLong()) {
      address |= static_cast<uint64_t>(raw[1] & 0x7f) << getOffset();
      if (raw[1] & 0x80)
        address |= static_cast<uint64_t>(raw[2]) << (8+getOffset()-1);
    }
    else {
      for (uint32_t i = 0; i < 2; i++) {
        address |= static_cast<uint64_t>(raw[1+i]) << ((i*8)+(getOffset()-i));
      }
      for (uint32_t i = 2; i < 4; i++) {
        address |= static_cast<uint64_t>(raw[1+i]) << (i*8);
      }
    }
  }
  return address;
}

bool Packet::Q::isDone() const {
  return (counter == 2) || (!hasAddress() && !hasCount());
}

void Packet::Q::insert(const uint8_t& byte) {
  if ((counter == 0) && hasAddress()) {
    Packet::Base::insert(byte);
    if (((!isLong()) && (byte < 128)) || (iterator == 1+getLength())) {
      counter = 1+(!hasCount());
    }
  }
  else if (((counter == 0) && !hasAddress()) || ((counter == 1) && hasCount())) {
    Packet::Base::insert(byte & 0x7f);
    counter = 1+(byte < 128); // make sure we stay in this branch
  }
}

std::string Packet::Q::asString() const {
  return Packet::Base::asString()+std::format("Q (Address = {:016X})", getAddress());
}


Packet::AtomFormat1::AtomFormat1(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

uint8_t Packet::AtomFormat1::getA() const {
  return 0b00000001 & raw[0];
}

std::string Packet::AtomFormat1::asString() const {
  return Packet::Base::asString()+"Atom format 1.";
}


Packet::AtomFormat2::AtomFormat2(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

uint8_t Packet::AtomFormat2::getA() const {
  return 0b00000011 | raw[0];
}

std::string Packet::AtomFormat2::asString() const {
  return Packet::Base::asString()+"Atom format 2.";
}


Packet::AtomFormat3::AtomFormat3(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::AtomFormat3::getA() const {
  return 0b00000111 & raw[0];
}

std::string Packet::AtomFormat3::asString() const {
  return Packet::Base::asString()+"Atom format 3 (A = "+std::format("0x{:02X}", getA())+")";
}


Packet::AtomFormat4::AtomFormat4(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

uint8_t Packet::AtomFormat4::getA() const {
  return 0b00000011 | raw[0];
}

std::string Packet::AtomFormat4::asString() const {
  return Packet::Base::asString()+"Atom format 4.";
}


Packet::AtomFormat5::AtomFormat5(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}
  
uint8_t Packet::AtomFormat5::getABC() const {
  return ((0b00100000 & raw[0]) >> 3) | (0b00000011 & raw[0]);
}

std::string Packet::AtomFormat5::asString() const {
  return Packet::Base::asString()+"Atom format 5.";
}


Packet::AtomFormat6::AtomFormat6(const uint8_t& header) {
  Packet::Base::insert(header);
  Packet::Base::markDone();
}

uint8_t Packet::AtomFormat6::getA() const {
  return (0b00100000 & raw[0]) == 0b00100000;
}

uint8_t Packet::AtomFormat6::getCount() const {
  return 0b00011111 & raw[0];
}

std::string Packet::AtomFormat6::asString() const {
  return Packet::Base::asString()+"Atom format 6 (COUNT = "+std::format("0x{:02X}", getCount())+")";
}


Packet::Exception::Exception(const uint8_t& header) {
  Packet::Base::insert(header);
}

uint16_t Packet::Exception::getType() const {
  uint16_t type = 0;
  type |= static_cast<uint16_t>(raw[1] & 0b00111110) >> 1;
  if (raw[1] & 0b10000000) // If C is assrted
    type |= static_cast<uint16_t>(raw[2]) << 5;
  return type;
}

uint8_t Packet::Exception::getE0() const {
  return 0b00000001 & raw[1];
}

uint8_t Packet::Exception::getE1() const {
  return (0b01000000 & raw[1]) >> 6;
}

uint8_t Packet::Exception::getP() const {
  uint8_t p = 0xff; // Error
  if (0b10000000 & raw[1])
    p = (0b00100000 & raw[1]) >> 5;
  return p;
}

void Packet::Exception::insert(const uint8_t& byte) {
  Packet::Base::insert(byte); // important to keep the MSB here for the C field!
  if (byte < 128) { // Assumes that SBZ are 0s
    Packet::Base::markDone();
  }
}
 
std::string Packet::Exception::asString() const {
  return Packet::Base::asString()+std::format("Exception (TYPE = {}, P = {})", getType(), getP());
}
