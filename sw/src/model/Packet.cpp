#include "Packet.hpp"
#include <cstdint>

inline bool Packet::isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper) {
  return (lower <= a) && (a <= upper);
}

inline uint8_t Packet::Base::getIterator() const {
  return iterator;
}


inline bool Packet::Extension::isDone() const {
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
      case 0b00000000:
        type = Extension::Ext::ASync;
        break;
      case 0b00000011:
        type = Extension::Ext::Discard;
        break;
      case 0b00000101:
        type = Extension::Ext::Overflow;
        break;
      case 0b00000111:
        type = Extension::Ext::BranchFutureFlush;
        break;
    }
  }
  iterator++;
}

std::string Packet::Extension::asString() const {
  switch (type) {
    case Extension::Ext::ASync:             return "ASync.";
    case Extension::Ext::Discard:           return "Discard.";
    case Extension::Ext::Overflow:          return "Overflow";
    case Extension::Ext::BranchFutureFlush: return "BranchFutureFlush";
    default: return "No match found!";
  }
}


inline bool Packet::TraceInfo::isDone() const {
  return iterator == 5;
}

void Packet::TraceInfo::insert(uint8_t byte) {
  if (iterator == 0) { // PLCTL
    hasInfo = (0b00000001 & byte);
    hasKey  = (0b00000010 & byte) >> 1;
    hasSpec = (0b00000100 & byte) >> 2;
    hasCyct = (0b00001000 & byte) >> 3;
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

inline std::string Packet::TraceInfo::asString() const {
  return "Trace info.";
}


inline bool Packet::Timestamp::isDone() const {
  return !(hasCountFlag || timestampFlag);
}

Packet::Timestamp::Timestamp(uint8_t header) {
  //Page 264: N = 0 -> no count; N = 1 -> count.
  hasCountFlag = header%2;
}

void Packet::Timestamp::insert(uint8_t byte) {
  if (timestampFlag) {
    if (iterator < 7) {
      TS |= (0b01111111 & byte) << (iterator*7);
      iterator++;
      if (byte < 128) {
        iterator = 0;
        timestampFlag = false;
      }
    }
    else {
      TS |= byte << (iterator*7);
      iterator = 0;
      timestampFlag = false;
    }
  }
  else if (hasCountFlag) {
    if (iterator < 2) {
      COUNT |= (0b01111111 & byte) << (iterator*7);
      iterator++;
      if (byte < 128) {
        hasCountFlag = false;
      }
    }
    else {
      COUNT |= (0b00111111 & byte) << (iterator*7);
      hasCountFlag = false;
    }
  }
}

inline std::string Packet::Timestamp::asString() const {
  return "Timestamp.";
}


inline bool Packet::TraceOn::isDone() const {
  return true;
}

inline void Packet::TraceOn::insert(uint8_t byte) {}

inline std::string Packet::TraceOn::asString() const {
  return "Trace on.";
}


inline bool Packet::FunctionReturn::isDone() const {
  return true;
}

inline void Packet::FunctionReturn::insert(uint8_t byte) {}

inline std::string Packet::FunctionReturn::asString() const {
  return "Function return.";
}


inline bool Packet::ExceptionReturn::isDone() const {
  return true;
}

inline void Packet::ExceptionReturn::insert(uint8_t byte) {}

inline std::string Packet::ExceptionReturn::asString() const {
  return "Exception return.";
}


inline bool Packet::Resynchronization::isDone() const {
  return true;
}

inline void Packet::Resynchronization::insert(uint8_t byte) {}

inline std::string Packet::Resynchronization::asString() const {
  return "Resynchronization.";
}


Packet::Reserved::Reserved(uint8_t header) {}

inline bool Packet::Reserved::isDone() const {
  return true;
}

inline void Packet::Reserved::insert(uint8_t byte) {}

inline std::string Packet::Reserved::asString() const {
  return "Reserved.";
}


Packet::CycleCountFormat2::CycleCountFormat2(uint8_t header) {
  F = 0b00000001 & header;
}

inline bool Packet::CycleCountFormat2::isDone() const {
  return iterator == 1;
}

void Packet::CycleCountFormat2::insert(uint8_t byte) {
  aaaa = (0b11110000 && byte) >> 4;
  bbbb = (0b00001111 && byte);
  iterator++;
}

inline std::string Packet::CycleCountFormat2::asString() const {
  return "Cycle count format 2.";
}


Packet::CycleCountFormat1::CycleCountFormat1(uint8_t header) {
  U = header & 0b00000001;
}

inline bool Packet::CycleCountFormat1::isDone() const {
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
      count |= (0b00111111 & byte) << ((iterator-1)*7);
      iterator = 4;
    }
    else {
      count |= (0b01111111 & byte) << ((iterator-1)*7);
      iterator++;
      if (byte < 128)
        iterator = 4;
    }
  }
}

inline std::string Packet::CycleCountFormat1::asString() const {
  return "Cycle count format 1.";
}


Packet::CycleCountFormat3::CycleCountFormat3(uint8_t header) {
  aa = (0b00001100 & header) >> 2;
  bb = (0b00000011 & header);
}

inline bool Packet::CycleCountFormat3::isDone() const {
  return true;
}

inline void Packet::CycleCountFormat3::insert(uint8_t byte) {}

inline std::string Packet::CycleCountFormat3::asString() const {
  return "Cycle count format 3.";
}


Packet::NumberedDataSyncMark::NumberedDataSyncMark(uint8_t header) {
  NUM = 0b00000111 & header;
}

inline bool Packet::NumberedDataSyncMark::isDone() const {
  return true;
}

inline void Packet::NumberedDataSyncMark::insert(uint8_t byte) {}

inline std::string Packet::NumberedDataSyncMark::asString() const {
  return "Numbered data sync mark.";
}


Packet::UnnumberedDataSyncMark::UnnumberedDataSyncMark(uint8_t header) {
  A = 0b00000111 & header;
}

inline bool Packet::UnnumberedDataSyncMark::isDone() const {
  return true;
}

inline void Packet::UnnumberedDataSyncMark::insert(uint8_t byte) {}

inline std::string Packet::UnnumberedDataSyncMark::asString() const {
  return "Unnumbered data sync mark.";
}


inline bool Packet::Commit::isDone() const {
  return done;
}

inline void Packet::Commit::insert(uint8_t byte) {
  commit.push_back(0b01111111 & byte);
  done = (byte < 128);
}

inline std::string Packet::Commit::asString() const {
  return "Commit.";
}


Packet::CancelFormat1::CancelFormat1(uint8_t header) {
  M = 0b00000001 & header;
}

inline bool Packet::CancelFormat1::isDone() const {
  return done;
}

inline void Packet::CancelFormat1::insert(uint8_t byte) {
  cancel.push_back(0b01111111 & byte);
  done = (byte < 128);
}

inline std::string Packet::CancelFormat1::asString() const {
  return "Cancel format 1.";
}


Packet::Mispredict::Mispredict(uint8_t header) {
  A = 0b00000011 & header;
}
  
inline bool Packet::Mispredict::isDone() const {
  return true;
}

inline void Packet::Mispredict::insert(uint8_t byte) {}

inline std::string Packet::Mispredict::asString() const {
  return "Mispredict (A = "+std::to_string(static_cast<int>(A))+")";
}


Packet::CancelFormat2::CancelFormat2(uint8_t header) {
  A = 0b00000011 & header;
}
  
inline bool Packet::CancelFormat2::isDone() const {
  return true;
}

inline void Packet::CancelFormat2::insert(uint8_t byte) {}

inline std::string Packet::CancelFormat2::asString() const {
  return "CancelFormat2 (A = "+std::to_string(static_cast<int>(A))+")";
}


Packet::CancelFormat3::CancelFormat3(uint8_t header) {
  CC = 0b00000110 & header;
  A  = 0b00000001 & header;
}
  
inline bool Packet::CancelFormat3::isDone() const {
  return true;
}

inline void Packet::CancelFormat3::insert(uint8_t byte) {}

inline std::string Packet::CancelFormat3::asString() const {
  return "CancelFormat3 (CC = "+std::to_string(static_cast<int>(CC))+", A = "+std::to_string(static_cast<int>(A))+")";
}


Packet::ConditionalInstructionFormat2::ConditionalInstructionFormat2(uint8_t header) {
  CI = 0b00000011 & header;
}
  
inline bool Packet::ConditionalInstructionFormat2::isDone() const {
  return true;
}

inline void Packet::ConditionalInstructionFormat2::insert(uint8_t byte) {}

inline std::string Packet::ConditionalInstructionFormat2::asString() const {
  return "Conditional instruction format 2 (CI = "+std::to_string(static_cast<int>(CI))+")";
}


inline bool Packet::ConditionalFlush::isDone() const {
  return true;
}

inline void Packet::ConditionalFlush::insert(uint8_t byte) {}

inline std::string Packet::ConditionalFlush::asString() const {
  return "Conditional flush.";
}


Packet::ConditionalResultFormat4::ConditionalResultFormat4(uint8_t header) {
  T = 0b00000011 & header;
}
  
inline bool Packet::ConditionalResultFormat4::isDone() const {
  return true;
}

inline void Packet::ConditionalResultFormat4::insert(uint8_t byte) {}

inline std::string Packet::ConditionalResultFormat4::asString() const {
  return "Conditional result format 4.";
}


Packet::ConditionalResultFormat2::ConditionalResultFormat2(uint8_t header) {
  T = (0b00000011 & header);
  K = (0b00000100 & header) >> 2;
}
  
inline bool Packet::ConditionalResultFormat2::isDone() const {
  return true;
}

inline void Packet::ConditionalResultFormat2::insert(uint8_t byte) {}

inline std::string Packet::ConditionalResultFormat2::asString() const {
  return "Condition result format 2.";
}


Packet::ConditionalResultFormat3::ConditionalResultFormat3(uint8_t header) {
  TOKEN |= (0b00001111 & header) << 8;
}
  
inline bool Packet::ConditionalResultFormat3::isDone() const {
  return iterator == 1;
}

inline void Packet::ConditionalResultFormat3::insert(uint8_t byte) {
  TOKEN |= byte;
  iterator++;
}

inline std::string Packet::ConditionalResultFormat3::asString() const {
  return "Condition result format 3.";
}


Packet::ConditionalResultFormat1::ConditionalResultFormat1(uint8_t header) {
  single = (0b00000100 & header) >> 2;
  CI0 = 0b00000001 & header;
  if (!single)
    CI1 = (0b00000010 & header) >> 1;
}
  
inline bool Packet::ConditionalResultFormat1::isDone() const {
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

inline std::string Packet::ConditionalResultFormat1::asString() const {
  return "Conditional result format 1.";
}


inline bool Packet::ConditionalInstructionFormat1::isDone() const {
  return done;
}

inline void Packet::ConditionalInstructionFormat1::insert(uint8_t byte) {
  KEY.push_back(0b01111111 & byte);
  done = (byte < 128);
}

inline std::string Packet::ConditionalInstructionFormat1::asString() const {
  return "Conditional instruction format 1.";
}


inline bool Packet::ConditionalInstructionFormat3::isDone() const {
  return iterator == 1;
}

inline void Packet::ConditionalInstructionFormat3::insert(uint8_t byte) {
  Z = 0b00000001 & byte;
  NUM = (0b01111110 & byte) >> 1;
  iterator++;
}

inline std::string Packet::ConditionalInstructionFormat3::asString() const {
  return "Condition instruction format 3.";
}


inline bool Packet::Ignore::isDone() const {
  return true;
}

inline void Packet::Ignore::insert(uint8_t byte) {}

inline std::string Packet::Ignore::asString() const {
  return "Ignore.";
}


Packet::Event::Event(uint8_t header) {
  for (int i = 0; i < events.size(); i++) {
    events[i] = ((0b00000001 << i) & header) >> i;
  }
}
  
inline bool Packet::Event::isDone() const {
  return true;
}

inline void Packet::Event::insert(uint8_t byte) {}

inline std::string Packet::Event::asString() const {
  return "Event (#0 = "+std::to_string(static_cast<int>(events[0]))+", #1 = "+std::to_string(static_cast<int>(events[1]))+", #2 = "+std::to_string(static_cast<int>(events[2]))+", #3 = "+std::to_string(static_cast<int>(events[3]))+").";
}


Packet::Context::Context(uint8_t header) {
  P = header & 0b00000001;
}

inline bool Packet::Context::isDone() const {
  return (P)? headerDone && (!(hasVirt || hasCont)) : true;
}

void Packet::Context::insert(uint8_t byte) {
  if (!headerDone) {
    EL = 0b00000011 & byte;
    SF = (0b00010000 & byte) >> 4;
    NS = (0b00100000 & byte) >> 5;
    hasVirt = (0b01000000 & byte) >> 6;
    hasCont = (0b10000000 & byte) >> 7;
    headerDone = true;
    iterator = 0;
  }
  else if (hasVirt) {
    VMID |= byte << (8*iterator);
    iterator++;
    if (iterator == 4) {
      iterator = 0;
      hasVirt = false;
    }
  }
  else if (hasCont) {
    CONTEXTID |= byte << (8*(iterator-4));
    iterator++;
    if (iterator == 4) {
      hasCont = false;
    }
  }
}

inline std::string Packet::Context::asString() const {
  return "Context.";
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
  
inline bool Packet::AddressWithContext::isDone() const {
  return addrDone && headerDone && !(hasVirt || hasCont);
}

void Packet::AddressWithContext::insert(uint8_t byte) {
  if (!addrDone) {
    if (iterator < offset) {
      A |= (0b01111111 & byte) << (offset+(8*iterator)-iterator);
    }
    else {
      A |= byte << (8*iterator);
    }
    iterator++;
    if (iterator == length) {
      addrDone = true;
      iterator = 0;
    }
  }
  else if (!headerDone) {
    EL = 0b00000011 & byte;
    SF = (0b00010000 & byte) >> 4;
    NS = (0b00100000 & byte) >> 5;
    hasVirt = (0b01000000 & byte) >> 6;
    hasCont = (0b10000000 & byte) >> 7;
    headerDone = true;
    iterator = 0;
  }
  else if (hasVirt) {
    VMID |= byte << (8*iterator);
    iterator++;
    if (iterator == 4) {
      iterator = 0;
      hasVirt = false;
    }
  }
  else if (hasCont) {
    CONTEXTID |= byte << (8*(iterator-4));
    iterator++;
    if (iterator == 4) {
      hasCont = false;
    }
  }
}

inline std::string Packet::AddressWithContext::asString() const {
  return "Addres with context.";
}


inline bool Packet::TimestampMarker::isDone() const {
  return true;
}

inline void Packet::TimestampMarker::insert(uint8_t byte) {}

inline std::string Packet::TimestampMarker::asString() const {
  return "Timestamp marker.";
}


Packet::ExactMatchAddress::ExactMatchAddress(uint8_t header) {
  QE = 0b00000011 && header;
}
  
inline bool Packet::ExactMatchAddress::isDone() const {
  return true;
}

inline void Packet::ExactMatchAddress::insert(uint8_t byte) {}

inline std::string Packet::ExactMatchAddress::asString() const {
  return "exact match address.";
}


Packet::ShortAddress::ShortAddress(uint8_t header) {
  switch(header & 0b00000011) {
    case 0b00000001: offset = 2; break;
    case 0b00000010: offset = 1; break;
    default        : offset = 0; break;
  }
}
  
inline bool Packet::ShortAddress::isDone() const {
  return done;
}

void Packet::ShortAddress::insert(uint8_t byte) {
  if (iterator == 0) {
    address |= (0b01111111 & byte) << offset;
    done = !((0b10000000 & byte) >> 7);
  }
  else if (!done & (iterator == 1)) {
    address |= byte << (8+offset);
    done = true;
  }
  iterator++;
}

inline std::string Packet::ShortAddress::asString() const {
  return "Short address.";
}


Packet::LongAddress::LongAddress(uint8_t header) {
  switch(header & 0b00000111) {
    case 0b00000010: offset = 2; length = 4; break;
    case 0b00000011: offset = 1; length = 4; break;
    case 0b00000101: offset = 2; length = 8; break;
    case 0b00000110: offset = 1; length = 8; break;
    default        : offset = 0; length = 0; break;
  }
}
  
inline bool Packet::LongAddress::isDone() const {
  return iterator == length;
}

void Packet::LongAddress::insert(uint8_t byte) {
  if (iterator < offset) {
    address |= (0b01111111 & byte) << (offset+(8*iterator)-iterator);
  }
  else {
    address |= byte << (8*iterator);
  }
  iterator += iterator < length;
}

inline std::string Packet::LongAddress::asString() const {
  return "Long address.";
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

inline bool Packet::Q::isDone() const {
  return !(hasAddress || hasCount);
}

inline void Packet::Q::insert(uint8_t byte) {
  if (hasAddress) {
    if (!isAddrLong) {
      if (iterator == 0) {
        address |= (0b01111111 & byte) << offset;
        hasAddress = (byte >= 128);
        iterator++;
      }
      else {
        address |= byte << (8+offset);
        hasAddress = false;
        iterator = 0;
      }
    }
    else {
      if (iterator < offset) {
        address |= (0b01111111 & byte) << (offset+(8*iterator)-iterator);
      }
      else {
        address |= byte << (8*iterator);
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

inline std::string Packet::Q::asString() const {
  return "Q.";
}


Packet::AtomFormat1::AtomFormat1(uint8_t header) {
  a = 0b00000001 | header;
}

inline bool Packet::AtomFormat1::isDone() const {
  return true;
}

inline void Packet::AtomFormat1::insert(uint8_t byte) {}

inline std::string Packet::AtomFormat1::asString() const {
  return "Atom format 1.";
}


Packet::AtomFormat2::AtomFormat2(uint8_t header) {
  a = 0b00000011 | header;
}

inline bool Packet::AtomFormat2::isDone() const {
  return true;
}

inline void Packet::AtomFormat2::insert(uint8_t byte) {}

inline std::string Packet::AtomFormat2::asString() const {
  return "Atom formt 2.";
}


Packet::AtomFormat3::AtomFormat3(uint8_t header) {
  a = 0b00000111 | header;
}
  
inline bool Packet::AtomFormat3::isDone() const {
  return true;
}

inline void Packet::AtomFormat3::insert(uint8_t byte) {}

inline std::string Packet::AtomFormat3::asString() const {
  return "Atom formt 3.";
}


Packet::AtomFormat4::AtomFormat4(uint8_t header) {
  a = 0b00000011 | header;
}

inline bool Packet::AtomFormat4::isDone() const {
  return true;
}

inline void Packet::AtomFormat4::insert(uint8_t byte) {}

inline std::string Packet::AtomFormat4::asString() const {
  return "Atom formt 4.";
}


Packet::AtomFormat5::AtomFormat5(uint8_t header) {
  abc = ((0b00100000 & header) >> 3) | (0b00000011 & header);
}
  
inline bool Packet::AtomFormat5::isDone() const {
  return true;
}

inline void Packet::AtomFormat5::insert(uint8_t byte) {}

inline std::string Packet::AtomFormat5::asString() const {
  return "Atom formt 5.";
}


Packet::AtomFormat6::AtomFormat6(uint8_t header) {
  A = (0b00100000 & header) >> 5;
  COUNT = 0b00011111 & header;
}

inline bool Packet::AtomFormat6::isDone() const {
  return true;
}

inline void Packet::AtomFormat6::insert(uint8_t byte) {}

inline std::string Packet::AtomFormat6::asString() const {
  return "Atom formt 6.";
}


inline bool Packet::Exception::isDone() const {
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
      type = (byte & 0b00111110) >> 1;
      if (byte >= 128) { 
        iterator++;
      }
      else {
        iterator = 0;
        headerDone = true;
      }
    }
    else {
      type |= (byte & 0b00011111) << 5;
      p = (byte & 0b00100000) >> 5;
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
      
inline std::string Packet::Exception::asString() const {
  return "Exception.";
}

Packet::Exception::~Exception() {
  if (address != nullptr) {
    delete address;
  }
}
