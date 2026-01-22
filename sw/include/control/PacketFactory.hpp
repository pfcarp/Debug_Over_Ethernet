#pragma once

#include <array>
#include <cstdint>
#include <memory>
#include <vector>


#include "Packet.hpp"


class PacketFactory {
  
  using FactoryFunction = std::unique_ptr<Packet::Base>(*)(const uint8_t&);
  
  private:
    // Methods
    void identify(const uint8_t& id);
    #define MAKE_FACTORY(T) static std::unique_ptr<Packet::Base> make##T(const uint8_t& id) { return std::make_unique<Packet::T>(id); }
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
    std::unique_ptr<Packet::Base> current = nullptr;
    uint64_t timestamp = 0;
    static constexpr std::array<FactoryFunction, 256> factory = [] {
      std::array<FactoryFunction, 256> t{};
    	t[0x00] = makeExtension;
    	t[0x01] = makeTraceInfo;
    	t[0x02] = makeTimestamp;
    	t[0x03] = makeTimestamp;
    	t[0x04] = makeTraceOn;
    	t[0x05] = makeFunctionReturn;
    	t[0x06] = makeException;
    	t[0x07] = makeExceptionReturn;
    	t[0x08] = makeResynchronization;
    	t[0x09] = makeReserved;
    	t[0x0A] = makeReserved;
    	t[0x0B] = makeReserved;
    	t[0x0C] = makeCycleCountFormat2;
    	t[0x0D] = makeCycleCountFormat2;
    	t[0x0E] = makeCycleCountFormat1;
    	t[0x0F] = makeCycleCountFormat1;
    	t[0x10] = makeCycleCountFormat3;
    	t[0x11] = makeCycleCountFormat3;
    	t[0x12] = makeCycleCountFormat3;
    	t[0x13] = makeCycleCountFormat3;
    	t[0x14] = makeCycleCountFormat3;
    	t[0x15] = makeCycleCountFormat3;
    	t[0x16] = makeCycleCountFormat3;
    	t[0x17] = makeCycleCountFormat3;
    	t[0x18] = makeCycleCountFormat3;
    	t[0x19] = makeCycleCountFormat3;
    	t[0x1A] = makeCycleCountFormat3;
    	t[0x1B] = makeCycleCountFormat3;
    	t[0x1C] = makeCycleCountFormat3;
    	t[0x1D] = makeCycleCountFormat3;
    	t[0x1E] = makeCycleCountFormat3;
    	t[0x1F] = makeCycleCountFormat3;
    	t[0X20] = makeNumberedDataSyncMark;
    	t[0X21] = makeNumberedDataSyncMark;
    	t[0X22] = makeNumberedDataSyncMark;
    	t[0X23] = makeNumberedDataSyncMark;
    	t[0X24] = makeNumberedDataSyncMark;
    	t[0X25] = makeNumberedDataSyncMark;
    	t[0X26] = makeNumberedDataSyncMark;
    	t[0X27] = makeNumberedDataSyncMark;
    	t[0x28] = makeUnnumberedDataSyncMark;
    	t[0x29] = makeUnnumberedDataSyncMark;
    	t[0x2A] = makeUnnumberedDataSyncMark;
    	t[0x2B] = makeUnnumberedDataSyncMark;
    	t[0x2C] = makeUnnumberedDataSyncMark;
    	t[0x2D] = makeCommit;
    	t[0x2E] = makeCancelFormat1;
    	t[0x2F] = makeCancelFormat1;
    	t[0x30] = makeMispredict;
    	t[0x31] = makeMispredict;
    	t[0x32] = makeMispredict;
    	t[0x33] = makeMispredict;
    	t[0x34] = makeCancelFormat2;
    	t[0x35] = makeCancelFormat2;
    	t[0x36] = makeCancelFormat2;
    	t[0x37] = makeCancelFormat2;
    	t[0x38] = makeCancelFormat3;
    	t[0x39] = makeCancelFormat3;
    	t[0x3A] = makeCancelFormat3;
    	t[0x3B] = makeCancelFormat3;
    	t[0x3C] = makeCancelFormat3;
    	t[0x3D] = makeCancelFormat3;
    	t[0x3E] = makeCancelFormat3;
    	t[0x3F] = makeCancelFormat3;
    	t[0x40] = makeConditionalInstructionFormat2;
    	t[0x41] = makeConditionalInstructionFormat2;
    	t[0x42] = makeConditionalInstructionFormat2;
    	t[0x43] = makeConditionalFlush;
    	t[0x44] = makeConditionalResultFormat4;
    	t[0x45] = makeConditionalResultFormat4;
    	t[0x46] = makeConditionalResultFormat4;
    	t[0x47] = makeReserved;
    	t[0x48] = makeConditionalResultFormat2;
    	t[0x49] = makeConditionalResultFormat2;
    	t[0x4A] = makeConditionalResultFormat2;
    	t[0x4B] = makeReserved;
    	t[0x4C] = makeConditionalResultFormat2;
    	t[0x4D] = makeConditionalResultFormat2;
    	t[0x4E] = makeConditionalResultFormat2;
    	t[0x4F] = makeReserved;
    	t[0x50] = makeConditionalResultFormat3;
    	t[0x51] = makeConditionalResultFormat3;
    	t[0x52] = makeConditionalResultFormat3;
    	t[0x53] = makeConditionalResultFormat3;
    	t[0x54] = makeConditionalResultFormat3;
    	t[0x55] = makeConditionalResultFormat3;
    	t[0x56] = makeConditionalResultFormat3;
    	t[0x57] = makeConditionalResultFormat3;
    	t[0x58] = makeConditionalResultFormat3;
    	t[0x59] = makeConditionalResultFormat3;
    	t[0x5A] = makeConditionalResultFormat3;
    	t[0x5B] = makeConditionalResultFormat3;
    	t[0x5C] = makeConditionalResultFormat3;
    	t[0x5D] = makeConditionalResultFormat3;
    	t[0x5E] = makeConditionalResultFormat3;
    	t[0x5F] = makeConditionalResultFormat3;
    	t[0x60] = makeReserved;
    	t[0x61] = makeReserved;
    	t[0x62] = makeReserved;
    	t[0x63] = makeReserved;
    	t[0x64] = makeReserved;
    	t[0x65] = makeReserved;
    	t[0x66] = makeReserved;
    	t[0x67] = makeReserved;
    	t[0x68] = makeConditionalResultFormat1;
    	t[0x69] = makeConditionalResultFormat1;
    	t[0x6A] = makeConditionalResultFormat1;
    	t[0x6B] = makeConditionalResultFormat1;
    	t[0x6C] = makeConditionalInstructionFormat1;
    	t[0x6D] = makeConditionalInstructionFormat3;
    	t[0x6E] = makeConditionalResultFormat1;
    	t[0x6F] = makeIgnore;
    	t[0x70] = makeEvent;
    	t[0x71] = makeEvent;
    	t[0x72] = makeEvent;
    	t[0x73] = makeEvent;
    	t[0x74] = makeEvent;
    	t[0x75] = makeEvent;
    	t[0x76] = makeEvent;
    	t[0x77] = makeEvent;
    	t[0x78] = makeEvent;
    	t[0x79] = makeEvent;
    	t[0x7A] = makeEvent;
    	t[0x7B] = makeEvent;
    	t[0x7C] = makeEvent;
    	t[0x7D] = makeEvent;
    	t[0x7E] = makeEvent;
    	t[0x7F] = makeEvent;
    	t[0x80] = makeContext;
    	t[0x81] = makeContext;
    	t[0x82] = makeAddressWithContext;
    	t[0x83] = makeAddressWithContext;
    	t[0x84] = makeReserved;
    	t[0x85] = makeAddressWithContext;
    	t[0x86] = makeAddressWithContext;
    	t[0x87] = makeReserved;
    	t[0x88] = makeTimestampMarker;
    	t[0x89] = makeReserved;
    	t[0x8A] = makeReserved;
    	t[0x8B] = makeReserved;
    	t[0x8C] = makeReserved;
    	t[0x8D] = makeReserved;
    	t[0x8E] = makeReserved;
    	t[0x8F] = makeReserved;
    	t[0x90] = makeExactMatchAddress;
    	t[0x91] = makeExactMatchAddress;
    	t[0x92] = makeExactMatchAddress;
    	t[0x93] = makeReserved;
    	t[0x94] = makeReserved;
    	t[0x95] = makeShortAddress;
    	t[0x96] = makeShortAddress;
    	t[0x97] = makeReserved;
    	t[0x98] = makeReserved;
    	t[0x99] = makeReserved;
    	t[0x9A] = makeLongAddress;
    	t[0x9B] = makeLongAddress;
    	t[0x9C] = makeReserved;
    	t[0x9D] = makeLongAddress;
    	t[0x9E] = makeLongAddress;
    	t[0x9F] = makeReserved;
    	t[0xA0] = makeQ;
    	t[0xA1] = makeQ;
    	t[0xA2] = makeQ;
    	t[0xA3] = makeQ;
    	t[0xA4] = makeQ;
    	t[0xA5] = makeQ;
    	t[0xA6] = makeQ;
    	t[0xA7] = makeQ;
    	t[0xA8] = makeQ;
    	t[0xA9] = makeQ;
    	t[0xAA] = makeQ;
    	t[0xAB] = makeQ;
    	t[0xAC] = makeQ;
    	t[0xAD] = makeQ;
    	t[0xAE] = makeQ;
    	t[0xAF] = makeQ;
    	t[0xB0] = makeReserved;
    	t[0xB1] = makeReserved;
    	t[0xB2] = makeReserved;
    	t[0xB3] = makeReserved;
    	t[0xB4] = makeReserved;
    	t[0xB5] = makeReserved;
    	t[0xB6] = makeReserved;
    	t[0xB7] = makeReserved;
    	t[0xB8] = makeReserved;
    	t[0xB9] = makeReserved;
    	t[0xBA] = makeReserved;
    	t[0xBB] = makeReserved;
    	t[0xBC] = makeReserved;
    	t[0xBD] = makeReserved;
    	t[0xBE] = makeReserved;
    	t[0xBF] = makeReserved;
    	t[0xC0] = makeAtomFormat6;
    	t[0xC1] = makeAtomFormat6;
    	t[0xC2] = makeAtomFormat6;
    	t[0xC3] = makeAtomFormat6;
    	t[0xC4] = makeAtomFormat6;
    	t[0xC5] = makeAtomFormat6;
    	t[0xC6] = makeAtomFormat6;
    	t[0xC7] = makeAtomFormat6;
    	t[0xC8] = makeAtomFormat6;
    	t[0xC9] = makeAtomFormat6;
    	t[0xCA] = makeAtomFormat6;
    	t[0xCB] = makeAtomFormat6;
    	t[0xCC] = makeAtomFormat6;
    	t[0xCD] = makeAtomFormat6;
    	t[0xCE] = makeAtomFormat6;
    	t[0xCF] = makeAtomFormat6;
    	t[0xD0] = makeAtomFormat6;
    	t[0xD1] = makeAtomFormat6;
    	t[0xD2] = makeAtomFormat6;
    	t[0xD3] = makeAtomFormat6;
    	t[0xD4] = makeAtomFormat6;
    	t[0xD5] = makeAtomFormat5;
    	t[0xD6] = makeAtomFormat5;
    	t[0xD7] = makeAtomFormat5;
    	t[0xD8] = makeAtomFormat2;
    	t[0xD9] = makeAtomFormat2;
    	t[0xDA] = makeAtomFormat2;
    	t[0xDB] = makeAtomFormat2;
    	t[0xDC] = makeAtomFormat4;
    	t[0xDD] = makeAtomFormat4;
    	t[0xDE] = makeAtomFormat4;
    	t[0xDF] = makeAtomFormat4;
    	t[0xE0] = makeAtomFormat6;
    	t[0xE1] = makeAtomFormat6;
    	t[0xE2] = makeAtomFormat6;
    	t[0xE3] = makeAtomFormat6;
    	t[0xE4] = makeAtomFormat6;
    	t[0xE5] = makeAtomFormat6;
    	t[0xE6] = makeAtomFormat6;
    	t[0xE7] = makeAtomFormat6;
    	t[0xE8] = makeAtomFormat6;
    	t[0xE9] = makeAtomFormat6;
    	t[0xEA] = makeAtomFormat6;
    	t[0xEB] = makeAtomFormat6;
    	t[0xEC] = makeAtomFormat6;
    	t[0xED] = makeAtomFormat6;
    	t[0xEE] = makeAtomFormat6;
    	t[0xEF] = makeAtomFormat6;
    	t[0xF0] = makeAtomFormat6;
    	t[0xF1] = makeAtomFormat6;
    	t[0xF2] = makeAtomFormat6;
    	t[0xF3] = makeAtomFormat6;
    	t[0xF4] = makeAtomFormat6;
    	t[0xF5] = makeAtomFormat5;
    	t[0xF6] = makeAtomFormat1;
    	t[0xF7] = makeAtomFormat1;
    	t[0xF8] = makeAtomFormat3;
    	t[0xF9] = makeAtomFormat3;
    	t[0xFA] = makeAtomFormat3;
    	t[0xFB] = makeAtomFormat3;
    	t[0xFC] = makeAtomFormat3;
    	t[0xFD] = makeAtomFormat3;
    	t[0xFE] = makeAtomFormat3;
    	t[0xFF] = makeAtomFormat3;
      return t;
    }();

  public:
    // Methods
    bool insert(uint8_t byte);
    void consume();
    std::unique_ptr<Packet::Base> get();
    void setTimestamp(uint64_t t);

};
