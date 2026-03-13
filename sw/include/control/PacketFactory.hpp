#pragma once

#include <array>
#include <cstdint>
#include <memory>


#include "TraceDatabase.hpp"
#include "TraceCollection.hpp"
#include "Packet.hpp"
#include "Trace.hpp"


class PacketFactory {
  
  using FactoryFunction = Packet::Variant*(*)(PacketFactory&, const uint8_t&, const uint64_t&);
  
  public:
    // Attributes
    TraceCollection& map;

  private:
    // Attibutes
    Packet::Variant* current = nullptr;
    // Methods
    #define MAKE_FACTORY(T) static Packet::Variant* make##T(PacketFactory& self, const uint8_t& id, const uint64_t& timestamp) { return self.map.add(#T, timestamp, Packet::T(id)); }
    MAKE_FACTORY(Extension)
    MAKE_FACTORY(TraceInfo)
    MAKE_FACTORY(Timestamp)
    MAKE_FACTORY(TraceOn)
    MAKE_FACTORY(FunctionReturn)
    MAKE_FACTORY(Exception)
    MAKE_FACTORY(ExceptionReturn)
    MAKE_FACTORY(Resynchronization)
    MAKE_FACTORY(Reserved)
    MAKE_FACTORY(CycleCountFormat1)
    MAKE_FACTORY(CycleCountFormat2)
    MAKE_FACTORY(CycleCountFormat3)
    MAKE_FACTORY(NumberedDataSyncMark)
    MAKE_FACTORY(UnnumberedDataSyncMark)
    MAKE_FACTORY(Commit)
    MAKE_FACTORY(Mispredict)
    MAKE_FACTORY(CancelFormat1)
    MAKE_FACTORY(CancelFormat2)
    MAKE_FACTORY(CancelFormat3)
    MAKE_FACTORY(ConditionalFlush)
    MAKE_FACTORY(ConditionalResultFormat1)
    MAKE_FACTORY(ConditionalResultFormat2)
    MAKE_FACTORY(ConditionalResultFormat3)
    MAKE_FACTORY(ConditionalResultFormat4)
    MAKE_FACTORY(ConditionalInstructionFormat1)
    MAKE_FACTORY(ConditionalInstructionFormat2)
    MAKE_FACTORY(ConditionalInstructionFormat3)
    MAKE_FACTORY(Ignore)
    MAKE_FACTORY(Event)
    MAKE_FACTORY(Context)
    MAKE_FACTORY(AddressWithContext)
    MAKE_FACTORY(TimestampMarker)
    MAKE_FACTORY(ExactMatchAddress)
    MAKE_FACTORY(ShortAddress)
    MAKE_FACTORY(LongAddress)
    MAKE_FACTORY(Q)
    MAKE_FACTORY(AtomFormat1)
    MAKE_FACTORY(AtomFormat2)
    MAKE_FACTORY(AtomFormat3)
    MAKE_FACTORY(AtomFormat4)
    MAKE_FACTORY(AtomFormat5)
    MAKE_FACTORY(AtomFormat6)
    #undef MAKE_FACTORY
    // Attributes
    uint64_t timestamp = 0;
    static constexpr std::array<FactoryFunction, 256> factory = [] {
      std::array<FactoryFunction, 256> t{};
    	t[0x00] = &PacketFactory::makeExtension;
    	t[0x01] = &PacketFactory::makeTraceInfo;
    	t[0x02] = &PacketFactory::makeTimestamp;
    	t[0x03] = &PacketFactory::makeTimestamp;
    	t[0x04] = &PacketFactory::makeTraceOn;
    	t[0x05] = &PacketFactory::makeFunctionReturn;
    	t[0x06] = &PacketFactory::makeException;
    	t[0x07] = &PacketFactory::makeExceptionReturn;
    	t[0x08] = &PacketFactory::makeResynchronization;
    	t[0x09] = &PacketFactory::makeReserved;
    	t[0x0A] = &PacketFactory::makeReserved;
    	t[0x0B] = &PacketFactory::makeReserved;
    	t[0x0C] = &PacketFactory::makeCycleCountFormat2;
    	t[0x0D] = &PacketFactory::makeCycleCountFormat2;
    	t[0x0E] = &PacketFactory::makeCycleCountFormat1;
    	t[0x0F] = &PacketFactory::makeCycleCountFormat1;
    	t[0x10] = &PacketFactory::makeCycleCountFormat3;
    	t[0x11] = &PacketFactory::makeCycleCountFormat3;
    	t[0x12] = &PacketFactory::makeCycleCountFormat3;
    	t[0x13] = &PacketFactory::makeCycleCountFormat3;
    	t[0x14] = &PacketFactory::makeCycleCountFormat3;
    	t[0x15] = &PacketFactory::makeCycleCountFormat3;
    	t[0x16] = &PacketFactory::makeCycleCountFormat3;
    	t[0x17] = &PacketFactory::makeCycleCountFormat3;
    	t[0x18] = &PacketFactory::makeCycleCountFormat3;
    	t[0x19] = &PacketFactory::makeCycleCountFormat3;
    	t[0x1A] = &PacketFactory::makeCycleCountFormat3;
    	t[0x1B] = &PacketFactory::makeCycleCountFormat3;
    	t[0x1C] = &PacketFactory::makeCycleCountFormat3;
    	t[0x1D] = &PacketFactory::makeCycleCountFormat3;
    	t[0x1E] = &PacketFactory::makeCycleCountFormat3;
    	t[0x1F] = &PacketFactory::makeCycleCountFormat3;
    	t[0X20] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0X21] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0X22] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0X23] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0X24] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0X25] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0X26] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0X27] = &PacketFactory::makeNumberedDataSyncMark;
    	t[0x28] = &PacketFactory::makeUnnumberedDataSyncMark;
    	t[0x29] = &PacketFactory::makeUnnumberedDataSyncMark;
    	t[0x2A] = &PacketFactory::makeUnnumberedDataSyncMark;
    	t[0x2B] = &PacketFactory::makeUnnumberedDataSyncMark;
    	t[0x2C] = &PacketFactory::makeUnnumberedDataSyncMark;
    	t[0x2D] = &PacketFactory::makeCommit;
    	t[0x2E] = &PacketFactory::makeCancelFormat1;
    	t[0x2F] = &PacketFactory::makeCancelFormat1;
    	t[0x30] = &PacketFactory::makeMispredict;
    	t[0x31] = &PacketFactory::makeMispredict;
    	t[0x32] = &PacketFactory::makeMispredict;
    	t[0x33] = &PacketFactory::makeMispredict;
    	t[0x34] = &PacketFactory::makeCancelFormat2;
    	t[0x35] = &PacketFactory::makeCancelFormat2;
    	t[0x36] = &PacketFactory::makeCancelFormat2;
    	t[0x37] = &PacketFactory::makeCancelFormat2;
    	t[0x38] = &PacketFactory::makeCancelFormat3;
    	t[0x39] = &PacketFactory::makeCancelFormat3;
    	t[0x3A] = &PacketFactory::makeCancelFormat3;
    	t[0x3B] = &PacketFactory::makeCancelFormat3;
    	t[0x3C] = &PacketFactory::makeCancelFormat3;
    	t[0x3D] = &PacketFactory::makeCancelFormat3;
    	t[0x3E] = &PacketFactory::makeCancelFormat3;
    	t[0x3F] = &PacketFactory::makeCancelFormat3;
    	t[0x40] = &PacketFactory::makeConditionalInstructionFormat2;
    	t[0x41] = &PacketFactory::makeConditionalInstructionFormat2;
    	t[0x42] = &PacketFactory::makeConditionalInstructionFormat2;
    	t[0x43] = &PacketFactory::makeConditionalFlush;
    	t[0x44] = &PacketFactory::makeConditionalResultFormat4;
    	t[0x45] = &PacketFactory::makeConditionalResultFormat4;
    	t[0x46] = &PacketFactory::makeConditionalResultFormat4;
    	t[0x47] = &PacketFactory::makeReserved;
    	t[0x48] = &PacketFactory::makeConditionalResultFormat2;
    	t[0x49] = &PacketFactory::makeConditionalResultFormat2;
    	t[0x4A] = &PacketFactory::makeConditionalResultFormat2;
    	t[0x4B] = &PacketFactory::makeReserved;
    	t[0x4C] = &PacketFactory::makeConditionalResultFormat2;
    	t[0x4D] = &PacketFactory::makeConditionalResultFormat2;
    	t[0x4E] = &PacketFactory::makeConditionalResultFormat2;
    	t[0x4F] = &PacketFactory::makeReserved;
    	t[0x50] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x51] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x52] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x53] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x54] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x55] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x56] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x57] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x58] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x59] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x5A] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x5B] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x5C] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x5D] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x5E] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x5F] = &PacketFactory::makeConditionalResultFormat3;
    	t[0x60] = &PacketFactory::makeReserved;
    	t[0x61] = &PacketFactory::makeReserved;
    	t[0x62] = &PacketFactory::makeReserved;
    	t[0x63] = &PacketFactory::makeReserved;
    	t[0x64] = &PacketFactory::makeReserved;
    	t[0x65] = &PacketFactory::makeReserved;
    	t[0x66] = &PacketFactory::makeReserved;
    	t[0x67] = &PacketFactory::makeReserved;
    	t[0x68] = &PacketFactory::makeConditionalResultFormat1;
    	t[0x69] = &PacketFactory::makeConditionalResultFormat1;
    	t[0x6A] = &PacketFactory::makeConditionalResultFormat1;
    	t[0x6B] = &PacketFactory::makeConditionalResultFormat1;
    	t[0x6C] = &PacketFactory::makeConditionalInstructionFormat1;
    	t[0x6D] = &PacketFactory::makeConditionalInstructionFormat3;
    	t[0x6E] = &PacketFactory::makeConditionalResultFormat1;
    	t[0x6F] = &PacketFactory::makeConditionalResultFormat1;
    	t[0x70] = &PacketFactory::makeIgnore;
    	t[0x71] = &PacketFactory::makeEvent;
    	t[0x72] = &PacketFactory::makeEvent;
    	t[0x73] = &PacketFactory::makeEvent;
    	t[0x74] = &PacketFactory::makeEvent;
    	t[0x75] = &PacketFactory::makeEvent;
    	t[0x76] = &PacketFactory::makeEvent;
    	t[0x77] = &PacketFactory::makeEvent;
    	t[0x78] = &PacketFactory::makeEvent;
    	t[0x79] = &PacketFactory::makeEvent;
    	t[0x7A] = &PacketFactory::makeEvent;
    	t[0x7B] = &PacketFactory::makeEvent;
    	t[0x7C] = &PacketFactory::makeEvent;
    	t[0x7D] = &PacketFactory::makeEvent;
    	t[0x7E] = &PacketFactory::makeEvent;
    	t[0x7F] = &PacketFactory::makeEvent;
    	t[0x80] = &PacketFactory::makeContext;
    	t[0x81] = &PacketFactory::makeContext;
    	t[0x82] = &PacketFactory::makeAddressWithContext;
    	t[0x83] = &PacketFactory::makeAddressWithContext;
    	t[0x84] = &PacketFactory::makeReserved;
    	t[0x85] = &PacketFactory::makeAddressWithContext;
    	t[0x86] = &PacketFactory::makeAddressWithContext;
    	t[0x87] = &PacketFactory::makeReserved;
    	t[0x88] = &PacketFactory::makeTimestampMarker;
    	t[0x89] = &PacketFactory::makeReserved;
    	t[0x8A] = &PacketFactory::makeReserved;
    	t[0x8B] = &PacketFactory::makeReserved;
    	t[0x8C] = &PacketFactory::makeReserved;
    	t[0x8D] = &PacketFactory::makeReserved;
    	t[0x8E] = &PacketFactory::makeReserved;
    	t[0x8F] = &PacketFactory::makeReserved;
    	t[0x90] = &PacketFactory::makeExactMatchAddress;
    	t[0x91] = &PacketFactory::makeExactMatchAddress;
    	t[0x92] = &PacketFactory::makeExactMatchAddress;
    	t[0x93] = &PacketFactory::makeReserved;
    	t[0x94] = &PacketFactory::makeReserved;
    	t[0x95] = &PacketFactory::makeShortAddress;
    	t[0x96] = &PacketFactory::makeShortAddress;
    	t[0x97] = &PacketFactory::makeReserved;
    	t[0x98] = &PacketFactory::makeReserved;
    	t[0x99] = &PacketFactory::makeReserved;
    	t[0x9A] = &PacketFactory::makeLongAddress;
    	t[0x9B] = &PacketFactory::makeLongAddress;
    	t[0x9C] = &PacketFactory::makeReserved;
    	t[0x9D] = &PacketFactory::makeLongAddress;
    	t[0x9E] = &PacketFactory::makeLongAddress;
    	t[0x9F] = &PacketFactory::makeReserved;
    	t[0xA0] = &PacketFactory::makeQ;
    	t[0xA1] = &PacketFactory::makeQ;
    	t[0xA2] = &PacketFactory::makeQ;
    	t[0xA3] = &PacketFactory::makeQ;
    	t[0xA4] = &PacketFactory::makeQ;
    	t[0xA5] = &PacketFactory::makeQ;
    	t[0xA6] = &PacketFactory::makeQ;
    	t[0xA7] = &PacketFactory::makeQ;
    	t[0xA8] = &PacketFactory::makeQ;
    	t[0xA9] = &PacketFactory::makeQ;
    	t[0xAA] = &PacketFactory::makeQ;
    	t[0xAB] = &PacketFactory::makeQ;
    	t[0xAC] = &PacketFactory::makeQ;
    	t[0xAD] = &PacketFactory::makeQ;
    	t[0xAE] = &PacketFactory::makeQ;
    	t[0xAF] = &PacketFactory::makeQ;
    	t[0xB0] = &PacketFactory::makeReserved;
    	t[0xB1] = &PacketFactory::makeReserved;
    	t[0xB2] = &PacketFactory::makeReserved;
    	t[0xB3] = &PacketFactory::makeReserved;
    	t[0xB4] = &PacketFactory::makeReserved;
    	t[0xB5] = &PacketFactory::makeReserved;
    	t[0xB6] = &PacketFactory::makeReserved;
    	t[0xB7] = &PacketFactory::makeReserved;
    	t[0xB8] = &PacketFactory::makeReserved;
    	t[0xB9] = &PacketFactory::makeReserved;
    	t[0xBA] = &PacketFactory::makeReserved;
    	t[0xBB] = &PacketFactory::makeReserved;
    	t[0xBC] = &PacketFactory::makeReserved;
    	t[0xBD] = &PacketFactory::makeReserved;
    	t[0xBE] = &PacketFactory::makeReserved;
    	t[0xBF] = &PacketFactory::makeReserved;
    	t[0xC0] = &PacketFactory::makeAtomFormat6;
    	t[0xC1] = &PacketFactory::makeAtomFormat6;
    	t[0xC2] = &PacketFactory::makeAtomFormat6;
    	t[0xC3] = &PacketFactory::makeAtomFormat6;
    	t[0xC4] = &PacketFactory::makeAtomFormat6;
    	t[0xC5] = &PacketFactory::makeAtomFormat6;
    	t[0xC6] = &PacketFactory::makeAtomFormat6;
    	t[0xC7] = &PacketFactory::makeAtomFormat6;
    	t[0xC8] = &PacketFactory::makeAtomFormat6;
    	t[0xC9] = &PacketFactory::makeAtomFormat6;
    	t[0xCA] = &PacketFactory::makeAtomFormat6;
    	t[0xCB] = &PacketFactory::makeAtomFormat6;
    	t[0xCC] = &PacketFactory::makeAtomFormat6;
    	t[0xCD] = &PacketFactory::makeAtomFormat6;
    	t[0xCE] = &PacketFactory::makeAtomFormat6;
    	t[0xCF] = &PacketFactory::makeAtomFormat6;
    	t[0xD0] = &PacketFactory::makeAtomFormat6;
    	t[0xD1] = &PacketFactory::makeAtomFormat6;
    	t[0xD2] = &PacketFactory::makeAtomFormat6;
    	t[0xD3] = &PacketFactory::makeAtomFormat6;
    	t[0xD4] = &PacketFactory::makeAtomFormat6;
    	t[0xD5] = &PacketFactory::makeAtomFormat5;
    	t[0xD6] = &PacketFactory::makeAtomFormat5;
    	t[0xD7] = &PacketFactory::makeAtomFormat5;
    	t[0xD8] = &PacketFactory::makeAtomFormat2;
    	t[0xD9] = &PacketFactory::makeAtomFormat2;
    	t[0xDA] = &PacketFactory::makeAtomFormat2;
    	t[0xDB] = &PacketFactory::makeAtomFormat2;
    	t[0xDC] = &PacketFactory::makeAtomFormat4;
    	t[0xDD] = &PacketFactory::makeAtomFormat4;
    	t[0xDE] = &PacketFactory::makeAtomFormat4;
    	t[0xDF] = &PacketFactory::makeAtomFormat4;
    	t[0xE0] = &PacketFactory::makeAtomFormat6;
    	t[0xE1] = &PacketFactory::makeAtomFormat6;
    	t[0xE2] = &PacketFactory::makeAtomFormat6;
    	t[0xE3] = &PacketFactory::makeAtomFormat6;
    	t[0xE4] = &PacketFactory::makeAtomFormat6;
    	t[0xE5] = &PacketFactory::makeAtomFormat6;
    	t[0xE6] = &PacketFactory::makeAtomFormat6;
    	t[0xE7] = &PacketFactory::makeAtomFormat6;
    	t[0xE8] = &PacketFactory::makeAtomFormat6;
    	t[0xE9] = &PacketFactory::makeAtomFormat6;
    	t[0xEA] = &PacketFactory::makeAtomFormat6;
    	t[0xEB] = &PacketFactory::makeAtomFormat6;
    	t[0xEC] = &PacketFactory::makeAtomFormat6;
    	t[0xED] = &PacketFactory::makeAtomFormat6;
    	t[0xEE] = &PacketFactory::makeAtomFormat6;
    	t[0xEF] = &PacketFactory::makeAtomFormat6;
    	t[0xF0] = &PacketFactory::makeAtomFormat6;
    	t[0xF1] = &PacketFactory::makeAtomFormat6;
    	t[0xF2] = &PacketFactory::makeAtomFormat6;
    	t[0xF3] = &PacketFactory::makeAtomFormat6;
    	t[0xF4] = &PacketFactory::makeAtomFormat6;
    	t[0xF5] = &PacketFactory::makeAtomFormat5;
    	t[0xF6] = &PacketFactory::makeAtomFormat1;
    	t[0xF7] = &PacketFactory::makeAtomFormat1;
    	t[0xF8] = &PacketFactory::makeAtomFormat3;
    	t[0xF9] = &PacketFactory::makeAtomFormat3;
    	t[0xFA] = &PacketFactory::makeAtomFormat3;
    	t[0xFB] = &PacketFactory::makeAtomFormat3;
    	t[0xFC] = &PacketFactory::makeAtomFormat3;
    	t[0xFD] = &PacketFactory::makeAtomFormat3;
    	t[0xFE] = &PacketFactory::makeAtomFormat3;
    	t[0xFF] = &PacketFactory::makeAtomFormat3;
      return t;
    }();

  public:
    // Attributes
    // Methods
    PacketFactory(uint32_t id);
    bool insert(const uint8_t& byte);
    void setTimestamp(uint64_t t);

};
