#pragma once


#include <cstdint>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include <map>


#include "Color.hpp"


namespace Packet {

  constexpr uint32_t bytesize = 64;

  class Base {
    
    protected:
      // Attributes
      uint8_t counter = 0;
      uint8_t iterator = 0;
      uint8_t  raw[Packet::bytesize];

    public:
      // Attributes
      uint64_t timestamp = 0;
      // Methods
      virtual void insert(const uint8_t& byte);
      virtual std::string asString() const;
      virtual bool isDone() const;
      virtual void markDone();
      virtual uint8_t getIterator() const;
      virtual ~Base() = default;

  };

  
  class Extension: public Base {

    private:
      bool isASync() const;
      bool isDiscard() const;
      bool isOverflow() const;
      bool isBranchFutureFlush() const;

    public:
      // Methods
      Extension(const uint8_t& header);
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class TraceInfo: public Base {

    private:
      uint32_t findInfoStartIndex() const;

    public:
      // Methods
      TraceInfo(const uint8_t& header);
      bool hasInfo() const;
      bool hasKey() const;
      bool hasSpec() const;
      bool hasCyct() const;
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class Timestamp: public Base {

    private:
      bool hasCount() const;

    public:
      // Methods
      Timestamp(const uint8_t& header);
      uint64_t getTS() const;
      uint32_t getCount() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class TraceOn: public Base {

    public:
      // Methods
      TraceOn(const uint8_t& header);
      std::string asString() const override;

  };

  class FunctionReturn: public Base {

    public:
      // Methods
      FunctionReturn(const uint8_t& header);
      std::string asString() const override;

  };

  class ExceptionReturn: public Base {

    public:
      // Methods
      ExceptionReturn(const uint8_t& header);
      std::string asString() const override;

  };

  class Resynchronization: public Base {

    public:
      // Methods
      Resynchronization(const uint8_t& header);
      std::string asString() const override;

  };

  class Reserved: public Base {

    public:
      // Methods
      Reserved(const uint8_t& header);
      std::string asString() const override;

  };

  class CycleCountFormat2: public Base {

    private:

    public:
      // Methods
      CycleCountFormat2(const uint8_t& header);
      uint8_t getF() const;
      uint8_t getA() const;
      uint8_t getB() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class CycleCountFormat1: public Base {

    private:

    public:
      // Methods
      CycleCountFormat1(const uint8_t& header);
      uint8_t getU() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class CycleCountFormat3: public Base {

    private:

    public:
      // Methods
      CycleCountFormat3(const uint8_t& header);
      uint8_t getAA() const;
      uint8_t getBB() const;
      std::string asString() const override;

  };

  class NumberedDataSyncMark: public Base {

    private:

    public:
      // Methods
      NumberedDataSyncMark(const uint8_t& header);
      uint8_t getNum() const;
      std::string asString() const override;

  };

  class UnnumberedDataSyncMark: public Base {
  
    private:

    public:
      // Methods
      UnnumberedDataSyncMark(const uint8_t& header);
      uint8_t getA() const;
      std::string asString() const override;

  };

  class Commit: public Base {

    private:
  
    public:
      // Methods
      Commit(const uint8_t& header);
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class CancelFormat1: public Base {

    private:
  
    public:
      // Methods
      CancelFormat1(const uint8_t& header);
      uint8_t getM() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class Mispredict: public Base {
  
    private:

    public:
      // Methods
      Mispredict(const uint8_t& header);
      uint8_t getA() const;
      std::string asString() const override;

  };

  class CancelFormat2: public Base {
  
    private:

    public:
      // Methods
      CancelFormat2(const uint8_t& header);
      uint8_t getA() const;
      std::string asString() const override;

  };

  class CancelFormat3: public Base {

    private:
  
    public:
      // Methods
      CancelFormat3(const uint8_t& header);
      uint8_t getCC() const;
      uint8_t getA() const;
      std::string asString() const override;

  };

  class ConditionalInstructionFormat2: public Base {

    private:
  
    public:
      // Methods
      ConditionalInstructionFormat2(const uint8_t& header);
      uint8_t getCI() const;
      std::string asString() const override;

  };

  class ConditionalFlush: public Base {
  
    public: 
      // Methods
      ConditionalFlush(const uint8_t& header);
      std::string asString() const override;

  };

  class ConditionalResultFormat4: public Base {

    private:
  
    public:
      // Methods
      ConditionalResultFormat4(const uint8_t& header);
      uint8_t getT() const;
      std::string asString() const override;

  };

  class ConditionalResultFormat2: public Base {

    private:
  
    public:
      // Methods
      ConditionalResultFormat2(const uint8_t& header);
      uint8_t getT() const;
      uint8_t getK() const;
      std::string asString() const override;

  };

  class ConditionalResultFormat3: public Base {

    private:
  
    public:
      // Methods
      ConditionalResultFormat3(const uint8_t& header);
      void insert(const uint8_t& byte) override;
      uint16_t getToken() const;
      std::string asString() const override;

  };

  class ConditionalResultFormat1: public Base {
    
    private:
  
    public:
      // Methods
      ConditionalResultFormat1(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class ConditionalInstructionFormat1: public Base {
  
    private:
  
    public: 
      // Methods
      ConditionalInstructionFormat1(const uint8_t& header);
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class ConditionalInstructionFormat3: public Base {

    private:
  
    public: 
      // Methods
      ConditionalInstructionFormat3(const uint8_t& header);
      uint8_t getZ() const;
      uint8_t getNum() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class Ignore: public Base {
  
    public:
      // Methods
      Ignore(const uint8_t& header);
      std::string asString() const override;

  };

  class Event: public Base {

    private:
  
    public:
      // Methods
      Event(const uint8_t& header);
      std::string asString() const override;
      bool hasEvent(const uint8_t& index) const;

  };

  class Context: public Base {

    private:
  
    public:
      // Methods
      Context(const uint8_t& header);
      bool hasPayload() const;
      bool hasVMID() const;
      bool hasContextID() const;
      uint8_t getEL() const;
      uint8_t getSF() const;
      uint8_t getNS() const;
      uint32_t getVMID() const;
      uint32_t getContextID() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class AddressWithContext: public Base {
  
    private:

    public:
      // Methods
      AddressWithContext(const uint8_t& header);
      bool hasVMID() const;
      bool hasContextID() const;
      uint8_t getEL() const;
      uint8_t getSF() const;
      uint8_t getNS() const;
      uint8_t getOffset() const;
      uint8_t getLength() const;
      uint64_t getAddress() const;
      uint32_t getVMID() const;
      uint32_t getContextID() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class TimestampMarker: public Base {
  
    public:
      // Methods
      TimestampMarker(const uint8_t& header);
      std::string asString() const override;

  };

  class ExactMatchAddress: public Base {

    private:
  
    public:
      // Methods
      ExactMatchAddress(const uint8_t& header);
      uint8_t getQE() const;
      std::string asString() const override;

  };

  class ShortAddress: public Base {
  
    private:

    public:
      // Methods
      ShortAddress(const uint8_t& header);
      bool isIS0() const;
      uint32_t getAddress() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class LongAddress: public Base {
  
    private:

    public:
      // Methods
      LongAddress(const uint8_t& header);
      uint8_t getOffset() const;
      uint8_t getLength() const;
      uint64_t getAddress() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class Q: public Base {

    private:
  
    public:
      // Methods 
      Q(const uint8_t& header);
      bool hasAddress() const;
      bool hasCount() const;
      bool isLong() const;
      uint8_t getOffset() const;
      uint8_t getLength() const;
      uint64_t getAddress() const;
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class AtomFormat1: public Base {
  
    private:
  
    public:
      // Methods
      AtomFormat1(const uint8_t& header);
      uint8_t getA() const;
      std::string asString() const override;

  };

  class AtomFormat2: public Base {
  
    private:
  
    public:
      // Methods
      AtomFormat2(const uint8_t& header);
      uint8_t getA() const;
      std::string asString() const override;

  };

  class AtomFormat3: public Base {

    private:
  
    public:
      // Methods
      AtomFormat3(const uint8_t& header);
      uint8_t getA() const;
      std::string asString() const override;

  };

  class AtomFormat4: public Base {
  
    private:

    public:
      // Methods
      AtomFormat4(const uint8_t& header);
      uint8_t getA() const;
      std::string asString() const override;

  };

  class AtomFormat5: public Base {

    private:
  
    public:
      // Methods
      AtomFormat5(const uint8_t& header);
      uint8_t getABC() const;
      std::string asString() const override;

  };

  class AtomFormat6: public Base {

    private:
  
    public:
      // Methods 
      AtomFormat6(const uint8_t& header);
      uint8_t getA() const;
      uint8_t getCount() const;
      std::string asString() const override;

  };

  class Exception: public Base {

    private:

    public:
      // Methods
      Exception(const uint8_t& header);
      uint16_t getType() const;
      uint8_t getE0() const;
      uint8_t getE1() const;
      uint8_t getP() const;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  using Variant = std::variant<Reserved, Extension, TraceInfo, Timestamp, TraceOn, FunctionReturn, ExceptionReturn, Resynchronization, CycleCountFormat1, CycleCountFormat2, CycleCountFormat3, NumberedDataSyncMark, UnnumberedDataSyncMark, Commit, CancelFormat1, Mispredict, CancelFormat2, CancelFormat3, ConditionalInstructionFormat1, ConditionalInstructionFormat2, ConditionalFlush, ConditionalResultFormat4, ConditionalResultFormat2, ConditionalResultFormat3, ConditionalResultFormat1, ConditionalInstructionFormat3, Ignore, Event, Context, AddressWithContext, TimestampMarker, ExactMatchAddress, ShortAddress, LongAddress, Q, AtomFormat1, AtomFormat2, AtomFormat3, AtomFormat4, AtomFormat5, AtomFormat6, Exception>;
  
  inline std::map<std::string, Color> ColorMap = {
    {"Extension"                    , Color()},
    {"TraceInfo"                    , Color()},
    {"Timestamp"                    , Color()},
    {"TraceOn"                      , Color()},
    {"FunctionReturn"               , Color()},
    {"Exception"                    , Color()},
    {"ExceptionReturn"              , Color()},
    {"Resynchronization"            , Color()},
    {"Reserved"                     , Color()},
    {"CycleCountFormat1"            , Color()},
    {"CycleCountFormat2"            , Color()},
    {"CycleCountFormat3"            , Color()},
    {"NumberedDataSyncMark"         , Color()},
    {"UnumberedDataSyncMark"        , Color()},
    {"Commit"                       , Color()},
    {"Mispredict"                   , Color()},
    {"CancelFormat1"                , Color()},
    {"CancelFormat2"                , Color()},
    {"CancelFormat3"                , Color()},
    {"ConditionalFlush"             , Color()},
    {"ConditionalResultFormat1"     , Color()},
    {"ConditionalResultFormat2"     , Color()},
    {"ConditionalResultFormat3"     , Color()},
    {"ConditionalResultFormat4"     , Color()},
    {"ConditionalInstructionFormat1", Color()},
    {"ConditionalInstructionFormat2", Color()},
    {"ConditionalInstructionFormat3", Color()},
    {"Ignore"                       , Color()},
    {"Event"                        , Color()},
    {"Context"                      , Color()},
    {"AddressWithContext"           , Color()},
    {"TimestampMarker"              , Color()},
    {"ExactMatchAddress"            , Color()},
    {"ShortAddress"                 , Color()},
    {"LongAddress"                  , Color()},
    {"Q"                            , Color()},
    {"AtomFormat1"                  , Color()},
    {"AtomFormat2"                  , Color()},
    {"AtomFormat3"                  , Color()},
    {"AtomFormat4"                  , Color()},
    {"AtomFormat5"                  , Color()},
    {"AtomFormat6"                  , Color()},
  };

  bool isDone(const Variant& packet);
  void insert(Variant& packet, const uint8_t& byte);
  std::string asString(const Variant& packet);

}
