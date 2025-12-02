#include "Packet.hpp"
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>

bool Packet::isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper) {
  return (lower <= a) && (a <= upper);
}

uint8_t Packet::Base::getIterator() const {
  return iterator;
}


bool Packet::Base::isDone() const {
  return false;
}

void Packet::Base::insert(uint8_t byte) {}

std::string Packet::Base::asString() const {
  return std::format("[@{}] ", timestamp);
}

void Packet::Base::setTimestamp(uint64_t t) {
  timestamp = t;
}


bool Packet::Extension::isDone() const {
  switch (type) {
    case Extension::Ext::ASync:             return iterator == 11;
    case Extension::Ext::Discard:           return iterator ==  1;
    case Extension::Ext::Overflow:          return iterator ==  1;
    case Extension::Ext::BranchFutureFlush: return iterator ==  1;
    default: return false;
  } 
}

void Packet::Extension::insert(uint8_t byte) {
  if (iterator == 0) {
    switch (byte) {
      case 0b00000000: type = Extension::Ext::ASync            ; break;
      case 0b00000011: type = Extension::Ext::Discard          ; break;
      case 0b00000101: type = Extension::Ext::Overflow         ; break;
      case 0b00000111: type = Extension::Ext::BranchFutureFlush; break;
    }
  }
  else {
    if ((iterator == 10) && (byte != 0x80)) {
      // throw;
      std::cerr << "ASync sequence should end with 0x80 but " << static_cast<int>(byte) << " gotten!" << std::endl;
    }
    else if ((iterator < 10) && (byte != 0x00)) {
      std::cerr << "ASync content should be 0x00 but " << static_cast<int>(byte) << " gotten at step #" << static_cast<int>(iterator) << "!" << std::endl;
    }
  }
  iterator++;
}

std::string Packet::Extension::asString() const {
  switch (type) {
    case Extension::Ext::ASync:             return Packet::Base::asString()+"ASync.";
    case Extension::Ext::Discard:           return Packet::Base::asString()+"Discard.";
    case Extension::Ext::Overflow:          return Packet::Base::asString()+"Overflow";
    case Extension::Ext::BranchFutureFlush: return Packet::Base::asString()+"BranchFutureFlush";
    default: return Packet::Base::asString()+"No match found!";
  }
}


bool Packet::TraceInfo::isDone() const {
  return iterator == 5;
}

// NOTE: Can be optimized by commenting the push_back calls.
void Packet::TraceInfo::insert(uint8_t byte) {
  if (iterator == 0) { // PLCTL
    hasInfo = (0b00000001 & byte);
    hasKey  = (0b00000010 & byte) == 0b00000010;
    hasSpec = (0b00000100 & byte) == 0b00000100;
    hasCyct = (0b00001000 & byte) == 0b00001000;
    if (byte < 128) {
      if (hasInfo)      { iterator = 1; }
      else if (hasKey)  { iterator = 2; }
      else if (hasSpec) { iterator = 3; }
      else if (hasCyct) { iterator = 4; }
      else              { iterator = 5; }
    }
  }
  else if (iterator == 1) {
    if (hasInfo) {
      info.push_back(0b01111111 & byte);
      if (byte < 128) {
        if      (hasKey)  { iterator = 2; }
        else if (hasSpec) { iterator = 3; }
        else if (hasCyct) { iterator = 4; }
        else              { iterator = 5; }
      }
    }
  }
  else if (iterator == 2) {
    if (hasKey) {
      key.push_back(0b01111111 & byte);
      if (byte < 128) {
        if      (hasSpec) { iterator = 3; }
        else if (hasCyct) { iterator = 4; }
        else              { iterator = 5; }
      }
    }
  }
  else if (iterator == 3) {
    if (hasSpec) {
      spec.push_back(0b01111111 & byte);
      if (byte < 128) {
        if   (hasCyct) { iterator = 4; }
        else           { iterator = 5; }
      }
    }
  }
  else if (iterator == 4) {
    if (hasCyct) {
      cyct.push_back(0b01111111 & byte);
      if (byte < 128) {
        iterator = 5;
      }
    }
  }
}

std::string Packet::TraceInfo::asString() const {
  std::string base = Packet::Base::asString()+"Trace info";
  if (hasInfo) {
    for (uint8_t inf : info) {
      bool cc_enabled = inf & 0b00000001;
      if (cc_enabled)
        base += " (cycle count enabled)";
      else
        base += " (cycle count disabled)";
      uint8_t cond_enabled = (inf & 0b00001110) >> 1;
      switch (cond_enabled) {
        case 0b000: base += " (Tracing of conditional non-branch instructions is disabled)"; break;
        case 0b001: base += " (Conditional load instructions are traced)"; break;
        case 0b010: base += " (Conditional store instructions are traced)"; break;
        case 0b011: base += " (Conditional load and store instructions are traced)"; break;
        case 0b111: base += " (All conditional non-branch instructions are traced)"; break;
        default   : break;
      }
    }
  }
  return base;
}


bool Packet::Timestamp::isDone() const {
  return !(hasCountFlag || timestampFlag);
}

Packet::Timestamp::Timestamp(uint8_t header) {
  //Page 264: N = 0 -> no count; N = 1 -> count.
  hasCountFlag = header%2;
}

void Packet::Timestamp::insert(uint8_t byte) {
  if (timestampFlag) {
    if (iterator < 7) {
      TS |= static_cast<uint64_t>(0b01111111 & byte) << (iterator*7);
      iterator++;
      if (byte < 128) {
        iterator = 0;
        timestampFlag = false;
      }
    }
    else {
      TS |= static_cast<uint64_t>(byte) << (iterator*7);
      iterator = 0;
      timestampFlag = false;
    }
  }
  else if (hasCountFlag) {
    if (iterator < 2) {
      COUNT |= static_cast<uint32_t>(0b01111111 & byte) << (iterator*7);
      iterator++;
      if (byte < 128) {
        hasCountFlag = false;
      }
    }
    else {
      COUNT |= static_cast<uint32_t>(0b00111111 & byte) << (iterator*7);
      hasCountFlag = false;
    }
  }
}

std::string Packet::Timestamp::asString() const {
  return Packet::Base::asString()+std::format("Timestamp (TS = {}, COUNT = {})", TS, COUNT);
}


bool Packet::TraceOn::isDone() const {
  return true;
}

std::string Packet::TraceOn::asString() const {
  return Packet::Base::asString()+"Trace on.";
}


bool Packet::FunctionReturn::isDone() const {
  return true;
}

std::string Packet::FunctionReturn::asString() const {
  return Packet::Base::asString()+"Function return.";
}


bool Packet::ExceptionReturn::isDone() const {
  return true;
}

std::string Packet::ExceptionReturn::asString() const {
  return Packet::Base::asString()+"Exception return.";
}


bool Packet::Resynchronization::isDone() const {
  return true;
}

std::string Packet::Resynchronization::asString() const {
  return Packet::Base::asString()+"Resynchronization.";
}


bool Packet::Reserved::isDone() const {
  return true;
}

std::string Packet::Reserved::asString() const {
  return Packet::Base::asString()+"Reserved.";
}


Packet::CycleCountFormat2::CycleCountFormat2(uint8_t header) {
  F = 0b00000001 & header;
}

bool Packet::CycleCountFormat2::isDone() const {
  return iterator == 1;
}

void Packet::CycleCountFormat2::insert(uint8_t byte) {
  aaaa = (0b11110000 && byte) >> 4;
  bbbb = (0b00001111 && byte);
  iterator++;
}

std::string Packet::CycleCountFormat2::asString() const {
  return Packet::Base::asString()+"Cycle count format 2.";
}


Packet::CycleCountFormat1::CycleCountFormat1(uint8_t header) {
  U = header & 0b00000001;
}

bool Packet::CycleCountFormat1::isDone() const {
  return iterator == 4;
}

void Packet::CycleCountFormat1::insert(uint8_t byte) {
  if (iterator == 0) {
    commit.push_back(byte & 0b01111111);
    if (byte < 128)
      iterator = (U)? 4 : 1;
  }
  else if ((0 < iterator) && (iterator < 4)) {
    if (iterator == 3) {
      count |= static_cast<uint32_t>(0b00111111 & byte) << ((iterator-1)*7);
      iterator = 4;
    }
    else {
      count |= static_cast<uint32_t>(0b01111111 & byte) << ((iterator-1)*7);
      iterator++;
      if (byte < 128)
        iterator = 4;
    }
  }
}

std::string Packet::CycleCountFormat1::asString() const {
  return Packet::Base::asString()+"Cycle count format 1.";
}


Packet::CycleCountFormat3::CycleCountFormat3(uint8_t header) {
  aa = (0b00001100 & header) >> 2;
  bb = (0b00000011 & header);
}

bool Packet::CycleCountFormat3::isDone() const {
  return true;
}

std::string Packet::CycleCountFormat3::asString() const {
  return Packet::Base::asString()+"Cycle count format 3.";
}


Packet::NumberedDataSyncMark::NumberedDataSyncMark(uint8_t header) {
  NUM = 0b00000111 & header;
}

bool Packet::NumberedDataSyncMark::isDone() const {
  return true;
}

std::string Packet::NumberedDataSyncMark::asString() const {
  return Packet::Base::asString()+"Numbered data sync mark.";
}


Packet::UnnumberedDataSyncMark::UnnumberedDataSyncMark(uint8_t header) {
  A = 0b00000111 & header;
}

bool Packet::UnnumberedDataSyncMark::isDone() const {
  return true;
}

std::string Packet::UnnumberedDataSyncMark::asString() const {
  return Packet::Base::asString()+"Unnumbered data sync mark.";
}


bool Packet::Commit::isDone() const {
  return done;
}

void Packet::Commit::insert(uint8_t byte) {
  commit.push_back(0b01111111 & byte);
  done = (byte < 128);
}

std::string Packet::Commit::asString() const {
  return Packet::Base::asString()+"Commit.";
}


Packet::CancelFormat1::CancelFormat1(uint8_t header) {
  M = 0b00000001 & header;
}

bool Packet::CancelFormat1::isDone() const {
  return done;
}

void Packet::CancelFormat1::insert(uint8_t byte) {
  cancel.push_back(0b01111111 & byte);
  done = (byte < 128);
}

std::string Packet::CancelFormat1::asString() const {
  return Packet::Base::asString()+"Cancel format 1.";
}


Packet::Mispredict::Mispredict(uint8_t header) {
  A = 0b00000011 & header;
}
  
bool Packet::Mispredict::isDone() const {
  return true;
}

std::string Packet::Mispredict::asString() const {
  return Packet::Base::asString()+"Mispredict (A = "+std::to_string(static_cast<int>(A))+")";
}


Packet::CancelFormat2::CancelFormat2(uint8_t header) {
  A = 0b00000011 & header;
}
  
bool Packet::CancelFormat2::isDone() const {
  return true;
}

std::string Packet::CancelFormat2::asString() const {
  return Packet::Base::asString()+"CancelFormat2 (A = "+std::to_string(static_cast<int>(A))+")";
}


Packet::CancelFormat3::CancelFormat3(uint8_t header) {
  CC = 0b00000110 & header;
  A  = 0b00000001 & header;
}
  
bool Packet::CancelFormat3::isDone() const {
  return true;
}

std::string Packet::CancelFormat3::asString() const {
  return Packet::Base::asString()+"CancelFormat3 (CC = "+std::to_string(static_cast<int>(CC))+", A = "+std::to_string(static_cast<int>(A))+")";
}


Packet::ConditionalInstructionFormat2::ConditionalInstructionFormat2(uint8_t header) {
  CI = 0b00000011 & header;
}
  
bool Packet::ConditionalInstructionFormat2::isDone() const {
  return true;
}

std::string Packet::ConditionalInstructionFormat2::asString() const {
  return Packet::Base::asString()+"Conditional instruction format 2 (CI = "+std::to_string(static_cast<int>(CI))+")";
}


bool Packet::ConditionalFlush::isDone() const {
  return true;
}

std::string Packet::ConditionalFlush::asString() const {
  return Packet::Base::asString()+"Conditional flush.";
}


Packet::ConditionalResultFormat4::ConditionalResultFormat4(uint8_t header) {
  T = 0b00000011 & header;
}
  
bool Packet::ConditionalResultFormat4::isDone() const {
  return true;
}

std::string Packet::ConditionalResultFormat4::asString() const {
  return Packet::Base::asString()+std::format("Conditional result format 4 (TOKEN = {})", T);
}


Packet::ConditionalResultFormat2::ConditionalResultFormat2(uint8_t header) {
  T = (0b00000011 & header);
  K = (0b00000100 & header) >> 2;
}
  
bool Packet::ConditionalResultFormat2::isDone() const {
  return true;
}

std::string Packet::ConditionalResultFormat2::asString() const {
  return Packet::Base::asString()+"Condition result format 2.";
}


Packet::ConditionalResultFormat3::ConditionalResultFormat3(uint8_t header) {
  TOKEN |= static_cast<uint16_t>(0b00001111 & header) << 8;
}
  
bool Packet::ConditionalResultFormat3::isDone() const {
  return iterator == 1;
}

void Packet::ConditionalResultFormat3::insert(uint8_t byte) {
  TOKEN |= static_cast<uint16_t>(byte);
  iterator++;
}

std::string Packet::ConditionalResultFormat3::asString() const {
  return Packet::Base::asString()+"Condition result format 3.";
}


Packet::ConditionalResultFormat1::ConditionalResultFormat1(uint8_t header) {
  single = (0b00000100 & header) >> 2;
  CI0 = 0b00000001 & header;
  if (!single)
    CI1 = (0b00000010 & header) >> 1;
}
  
bool Packet::ConditionalResultFormat1::isDone() const {
  return (single)? iterator == 1 : iterator == 2;
}

void Packet::ConditionalResultFormat1::insert(uint8_t byte) {
  if (iterator == 0) {
    if (header) {
      RESULT0 = 0b00001111 & byte;
      KEY0.push_back((0b01110000 & byte) >> 4);
      header = false;
    }
    else {
      KEY0.push_back(0b01111111 & byte);
    }
  }
  else if (iterator == 1) {
    if (header) {
      RESULT1 = 0b00001111 & byte;
      KEY1.push_back((0b01110000 & byte) >> 4);
      header = false;
    }
    else {
      KEY1.push_back(0b01111111 & byte);
    }
  }
  if (byte < 128) {
    iterator++;
    header = true;
  }
}

std::string Packet::ConditionalResultFormat1::asString() const {
  return Packet::Base::asString()+"Conditional result format 1.";
}


bool Packet::ConditionalInstructionFormat1::isDone() const {
  return done;
}

void Packet::ConditionalInstructionFormat1::insert(uint8_t byte) {
  KEY.push_back(0b01111111 & byte);
  done = (byte < 128);
}

std::string Packet::ConditionalInstructionFormat1::asString() const {
  return Packet::Base::asString()+"Conditional instruction format 1.";
}


bool Packet::ConditionalInstructionFormat3::isDone() const {
  return iterator == 1;
}

void Packet::ConditionalInstructionFormat3::insert(uint8_t byte) {
  Z = 0b00000001 & byte;
  NUM = (0b01111110 & byte) >> 1;
  iterator++;
}

std::string Packet::ConditionalInstructionFormat3::asString() const {
  return Packet::Base::asString()+"Condition instruction format 3.";
}


bool Packet::Ignore::isDone() const {
  return true;
}

std::string Packet::Ignore::asString() const {
  return Packet::Base::asString()+"Ignore.";
}


Packet::Event::Event(uint8_t header) {
  events = header & 0b00001111;
}
  
bool Packet::Event::isDone() const {
  return true;
}

std::string Packet::Event::asString() const {
  return Packet::Base::asString()+"Event (#0 = "+std::to_string(static_cast<int>(hasEvent(0)))+", #1 = "+std::to_string(static_cast<int>(hasEvent(1)))+", #2 = "+std::to_string(static_cast<int>(hasEvent(2)))+", #3 = "+std::to_string(static_cast<int>(hasEvent(3)))+").";
}

bool Packet::Event::hasEvent(uint8_t index) const {
  switch (index) {
    case 0 : return (0b00000001 & events) == 0b00000001;
    case 1 : return (0b00000010 & events) == 0b00000010;
    case 2 : return (0b00000100 & events) == 0b00000100;
    case 3 : return (0b00001000 & events) == 0b00001000;
    default: return false;
  }
}


Packet::Context::Context(uint8_t header) {
  P = header & 0b00000001;
}

bool Packet::Context::isDone() const {
  return (P)? headerDone && (!(hasVirt || hasCont)) : true;
}

void Packet::Context::insert(uint8_t byte) {
  if (!headerDone) {
    EL = 0b00000011 & byte;
    SF = (0b00010000 & byte) == 0b00010000;
    NS = (0b00100000 & byte) == 0b00100000;
    hasVirt = (0b01000000 & byte) == 0b01000000;
    hasCont = (0b10000000 & byte) == 0b10000000;
    headerDone = true;
    iterator = 0;
  }
  else if (hasVirt) {
    VMID |= static_cast<uint32_t>(byte) << (8*iterator);
    iterator++;
    if (iterator == 4) {
      iterator = 0;
      hasVirt = false;
    }
  }
  else if (hasCont) {
    CONTEXTID |= static_cast<uint32_t>(byte) << (8*iterator);
    iterator++;
    if (iterator == 4) {
      hasCont = false;
    }
  }
}

std::string Packet::Context::asString() const {
  return Packet::Base::asString()+std::format("Context (P = {}, EL = {}, SF = {}, NS = {}, VMID = 0x{:016X}, CONTEXTID = 0x{:016X})", P, EL, SF, NS, VMID, CONTEXTID);
}

uint32_t Packet::Context::getVmID() const {
  return VMID;
}

uint32_t Packet::Context::getContextID() const {
  return CONTEXTID;
}


Packet::AddressWithContext::AddressWithContext(uint8_t header) {
  switch(header & 0b00000111) {
    case 0b00000010: offset = 2; length = 4; break;
    case 0b00000011: offset = 1; length = 4; break;
    case 0b00000101: offset = 2; length = 8; break;
    case 0b00000110: offset = 1; length = 8; break;
    default        : offset = 0; length = 0; break;
  }
}
  
bool Packet::AddressWithContext::isDone() const {
  return addrDone && headerDone && !(hasVirt || hasCont);
}

void Packet::AddressWithContext::insert(uint8_t byte) {
  if (!addrDone) {
    if (iterator < offset) {
      A |= static_cast<uint64_t>(0b01111111 & byte) << (offset+(8*iterator)-iterator);
    }
    else {
      A |= static_cast<uint64_t>(byte) << (8*iterator);
    }
    iterator++;
    if (iterator == length) {
      addrDone = true;
      iterator = 0;
    }
  }
  else if (!headerDone) {
    EL = 0b00000011 & byte;
    SF = (0b00010000 & byte) == 0b00010000;
    NS = (0b00100000 & byte) == 0b00100000;
    hasVirt = (0b01000000 & byte) == 0b01000000;
    hasCont = (0b10000000 & byte) == 0b10000000;
    headerDone = true;
    iterator = 0;
  }
  else if (hasVirt) {
    VMID |= static_cast<uint32_t>(byte) << (8*iterator);
    iterator++;
    if (iterator == 4) {
      iterator = 0;
      hasVirt = false;
    }
  }
  else if (hasCont) {
    CONTEXTID |= static_cast<uint32_t>(byte) << (8*iterator);
    iterator++;
    if (iterator == 4) {
      hasCont = false;
    }
  }
}

std::string Packet::AddressWithContext::asString() const {
  return Packet::Base::asString()+std::format("Address with context (A = 0x{:016X}, Context ID = {})", A, CONTEXTID);
}

uint64_t Packet::AddressWithContext::getAddress() const {
  return A;
}

uint32_t Packet::AddressWithContext::getVmID() const {
  return VMID;
}

uint32_t Packet::AddressWithContext::getContextID() const {
  return CONTEXTID;
}


bool Packet::TimestampMarker::isDone() const {
  return true;
}

std::string Packet::TimestampMarker::asString() const {
  return Packet::Base::asString()+"Timestamp marker.";
}


Packet::ExactMatchAddress::ExactMatchAddress(uint8_t header) {
  QE = 0b00000011 && header;
}
  
bool Packet::ExactMatchAddress::isDone() const {
  return true;
}

std::string Packet::ExactMatchAddress::asString() const {
  return Packet::Base::asString()+"exact match address.";
}


Packet::ShortAddress::ShortAddress(uint8_t header) {
  switch(header & 0b00000011) {
    case 0b00000001: offset = 2; break;
    case 0b00000010: offset = 1; break;
    default        : offset = 0; break;
  }
}
  
bool Packet::ShortAddress::isDone() const {
  return done;
}

void Packet::ShortAddress::insert(uint8_t byte) {
  if (iterator == 0) {
    address = static_cast<uint32_t>(0b01111111 & byte) << offset;
    offset--;
    done = (byte < 128);
  }
  else if (iterator == 1) {
    address |= static_cast<uint32_t>(byte) << (8+offset);
    done = true;
  }
  iterator++;
}

std::string Packet::ShortAddress::asString() const {
  return Packet::Base::asString()+std::format("Short address (0x{:04X})", address);
}

uint32_t Packet::ShortAddress::getAddress() const {
  return address;
}


Packet::LongAddress::LongAddress(uint8_t header) {
  switch(header & 0b00000111) {
    case 0b00000010: offset = 2; length = 4; break;
    case 0b00000011: offset = 1; length = 4; break;
    case 0b00000101: offset = 2; length = 8; break;
    case 0b00000110: offset = 1; length = 8; break;
    default        : throw; break; // TODO: undefined header subtype
  }
}
  
bool Packet::LongAddress::isDone() const {
  return iterator == length;
}

void Packet::LongAddress::insert(uint8_t byte) {
  if (iterator < offset) {
    address |= static_cast<uint64_t>(0b01111111 & byte) << (offset-iterator+(8*iterator));
  }
  else {
    address |= static_cast<uint64_t>(byte) << (8*iterator);
  }
  iterator += iterator < length;
}

std::string Packet::LongAddress::asString() const {
  return Packet::Base::asString()+std::format("Long address (0x{:016X})", address);
}

uint64_t Packet::LongAddress::getAddress() const {
  return address;
}


Packet::Q::Q(uint8_t header) {
  TYPE = header & 0b00001111;
  switch (TYPE) {
    case 0b0000: hasAddress = false; hasCount = true ; break;
    case 0b0001: hasAddress = false; hasCount = true ; break;
    case 0b0010: hasAddress = false; hasCount = true ; break;
    case 0b1100: hasAddress = false; hasCount = true ; break;
    case 0b0101: hasAddress = true ; hasCount = true ; offset = 2; break;
    case 0b0110: hasAddress = true ; hasCount = true ; offset = 1; break;
    case 0b1010: hasAddress = true ; hasCount = true ; offset = 2; isAddrLong = true; break;
    case 0b1011: hasAddress = true ; hasCount = true ; offset = 1; isAddrLong = true; break;
    case 0b1111: hasAddress = false; hasCount = false; break;
    default: break; // throw warning
  }
}

bool Packet::Q::isDone() const {
  return !(hasAddress || hasCount);
}

void Packet::Q::insert(uint8_t byte) {
  if (hasAddress) {
    if (!isAddrLong) {
      if (iterator == 0) {
        address |= static_cast<uint64_t>(0b01111111 & byte) << offset;
        offset--;
        hasAddress = (byte >= 128);
        iterator++;
      }
      else {
        address |= static_cast<uint64_t>(byte) << (8+offset);
        hasAddress = false;
        iterator = 0;
      }
    }
    else {
      if (iterator < offset) {
        address |= static_cast<uint64_t>(0b01111111 & byte) << (offset+(8*iterator)-iterator);
      }
      else {
        address |= static_cast<uint64_t>(byte) << (8*iterator);
      }
      iterator++;
      hasAddress = (iterator != 4);
    }
  }
  else if (hasCount) {
    count.push_back(byte & 0b01111111);
    hasCount = (byte >= 128);
  }
}

std::string Packet::Q::asString() const {
  return Packet::Base::asString()+std::format("Q (Address = {:016X}, #Counts = {})", address, count.size());
}

uint64_t Packet::Q::getAddress() const {
  return address;
}


Packet::AtomFormat1::AtomFormat1(uint8_t header) {
  a = 0b00000001 | header;
}

bool Packet::AtomFormat1::isDone() const {
  return true;
}

std::string Packet::AtomFormat1::asString() const {
  return Packet::Base::asString()+"Atom format 1.";
}


Packet::AtomFormat2::AtomFormat2(uint8_t header) {
  a = 0b00000011 | header;
}

bool Packet::AtomFormat2::isDone() const {
  return true;
}

std::string Packet::AtomFormat2::asString() const {
  return Packet::Base::asString()+"Atom formt 2.";
}


Packet::AtomFormat3::AtomFormat3(uint8_t header) {
  a = 0b00000111 & header;
}
  
bool Packet::AtomFormat3::isDone() const {
  return true;
}

std::string Packet::AtomFormat3::asString() const {
  return Packet::Base::asString()+"Atom format 3 (A = "+std::format("0x{:02X}", a)+")";
}


Packet::AtomFormat4::AtomFormat4(uint8_t header) {
  a = 0b00000011 | header;
}

bool Packet::AtomFormat4::isDone() const {
  return true;
}

std::string Packet::AtomFormat4::asString() const {
  return Packet::Base::asString()+"Atom formt 4.";
}


Packet::AtomFormat5::AtomFormat5(uint8_t header) {
  abc = ((0b00100000 & header) >> 3) | (0b00000011 & header);
}
  
bool Packet::AtomFormat5::isDone() const {
  return true;
}

std::string Packet::AtomFormat5::asString() const {
  return Packet::Base::asString()+"Atom formt 5.";
}


Packet::AtomFormat6::AtomFormat6(uint8_t header) {
  A = (0b00100000 & header) == 0b00100000;
  COUNT = 0b00011111 & header;
}

bool Packet::AtomFormat6::isDone() const {
  return true;
}

std::string Packet::AtomFormat6::asString() const {
  return Packet::Base::asString()+"Atom format 6 (COUNT = "+std::format("0x{:02X}", COUNT)+")";
}


bool Packet::Exception::isDone() const {
  return headerDone && (!hasAddress);
}

void Packet::Exception::insert(uint8_t byte) {
  if (!headerDone) {
    if (iterator == 0) {
      switch (byte & 0b01000001) {
        case 0b00000001: hasAddress = true ; break;
        case 0b01000000: hasAddress = true ; break;
        default        : hasAddress = false; break;
      }
      type = static_cast<uint16_t>(byte & 0b00111110) >> 1;
      if (byte >= 128) { 
        iterator++;
      }
      else {
        iterator = 0;
        headerDone = true;
      }
    }
    else {
      type |= static_cast<uint16_t>(byte & 0b00011111) << 5;
      p = (byte & 0b00100000) == 0b00100000;
      iterator = 0;
      headerDone = true;
    }
  }
  else if (hasAddress) {
    if (address == nullptr) {
      if (isInInclusiveRange(byte, 0b10000010, 0b10000011))
        address = new Packet::AddressWithContext(byte);
      else if (isInInclusiveRange(byte, 0b10000101, 0b10000110))
        address = new Packet::AddressWithContext(byte);
      else if (isInInclusiveRange(byte, 0b10010000, 0b10010010))
        address = new Packet::ExactMatchAddress(byte);
      else if (isInInclusiveRange(byte, 0b10010101, 0b10010110))
        address = new Packet::ShortAddress(byte);
      else if (isInInclusiveRange(byte, 0b10011010, 0b10011011))
        address = new Packet::LongAddress(byte);
      else if (isInInclusiveRange(byte, 0b10011101, 0b10011110))
        address = new Packet::LongAddress(byte);
    }
    else {
      address->insert(byte);
      hasAddress = !address->isDone();
    }
  }
}
 
std::string Packet::Exception::asString() const {
  std::string base = Packet::Base::asString()+std::format("Exception (TYPE = {}, P = {})", type, p);
  if (hasAddress)
    base += " ("+address->asString()+")";
  return base;
}

Packet::Exception::~Exception() {
  if (address != nullptr) {
    delete address;
  }
}
