#pragma once


#include <cstdint>
#include <string>
#include <vector>


namespace Packet {

  bool isInInclusiveRange(const uint8_t& a, const uint8_t& lower, const uint8_t& upper);

  class Base {
    
    protected:
      uint8_t iterator = 0;

    public:
      // Attributes
      uint64_t timestamp = 0;
      // Methods
      virtual bool isDone() const;
      virtual void insert(const uint8_t& byte);
      virtual std::string asString() const;
      virtual ~Base() = default;
      virtual uint8_t getIterator() const;
      virtual void setTimestamp(uint64_t t);

  };
  
  class Extension: public Base {

    private:
      enum class Ext {
        ASync,
        Discard,
        Overflow,
        BranchFutureFlush
      };

      Ext type;

    public:
      // Methods
      Extension(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class TraceInfo: public Base {

    private:
      bool hasInfo = false;
      std::vector<uint8_t> info;
      bool hasKey  = false;
      std::vector<uint8_t> key;
      bool hasSpec = false;
      std::vector<uint8_t> spec;
      bool hasCyct = false;
      std::vector<uint8_t> cyct;

    public:
      // Methods
      TraceInfo(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class Timestamp: public Base {

    private:
      bool timestampFlag = true;
      bool hasCountFlag = false;
      uint64_t TS = 0;
      uint32_t COUNT = 0;

    public:
      // Methods
      Timestamp(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class TraceOn: public Base {

    public:
      // Methods
      TraceOn(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;
  };

  class FunctionReturn: public Base {

    public:
      // Methods
      FunctionReturn(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ExceptionReturn: public Base {

    public:
      // Methods
      ExceptionReturn(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class Resynchronization: public Base {

    public:
      // Methods
      Resynchronization(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class Reserved: public Base {

    public:
      // Methods
      Reserved(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class CycleCountFormat2: public Base {

    private:
      bool F = false;
      uint8_t aaaa = 0;
      uint8_t bbbb = 0;

    public:
      // Methods
      CycleCountFormat2(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class CycleCountFormat1: public Base {

    private:
      bool U = false;
      std::vector<uint8_t> commit = std::vector<uint8_t>();
      uint32_t count = 0;

    public:
      // Methods
      CycleCountFormat1(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class CycleCountFormat3: public Base {

    private:
      uint8_t aa = 0;
      uint8_t bb = 0;

    public:
      // Methods
      CycleCountFormat3(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class NumberedDataSyncMark: public Base {

    private:
      uint8_t NUM = 0;

    public:
      // Methods
      NumberedDataSyncMark(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class UnnumberedDataSyncMark: public Base {
  
    private:
      uint8_t A = 0;

    public:
      // Methods
      UnnumberedDataSyncMark(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class Commit: public Base {

    private:
      bool done = false;
      std::vector<uint8_t> commit = std::vector<uint8_t>();
  
    public:
      // Methods
      Commit(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class CancelFormat1: public Base {

    private:
      bool M = false;
      bool done = false;
      std::vector<uint8_t> cancel = std::vector<uint8_t>();
  
    public:
      // Methods
      CancelFormat1(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class Mispredict: public Base {
  
    private:
      uint8_t A = 0;

    public:
      // Methods
      Mispredict(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class CancelFormat2: public Base {
  
    private:
      uint8_t A = 0;

    public:
      // Methods
      CancelFormat2(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class CancelFormat3: public Base {

    private:
      uint8_t CC = 0;
      bool    A  = 0;
  
    public:
      // Methods
      CancelFormat3(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ConditionalInstructionFormat2: public Base {

    private:
      uint8_t CI = 0;
  
    public:
      // Methods
      ConditionalInstructionFormat2(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ConditionalFlush: public Base {
  
    public: 
      // Methods
      ConditionalFlush(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ConditionalResultFormat4: public Base {

    private:
      uint8_t T = 0;
  
    public:
      // Methods
      ConditionalResultFormat4(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ConditionalResultFormat2: public Base {

    private:
      bool    K = false;
      uint8_t T = 0;
  
    public:
      // Methods
      ConditionalResultFormat2(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ConditionalResultFormat3: public Base {

    private:
      uint16_t TOKEN = 0;
  
    public:
      // Methods
      ConditionalResultFormat3(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class ConditionalResultFormat1: public Base {
    
    private:
      bool single = true;
      bool header = true;
      bool CI0 = false;
      uint8_t RESULT0 = 0;
      std::vector<uint8_t> KEY0 = std::vector<uint8_t>();
      bool CI1 = false;
      uint8_t RESULT1 = 0;
      std::vector<uint8_t> KEY1 = std::vector<uint8_t>();
  
    public:
      // Methods
      ConditionalResultFormat1(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class ConditionalInstructionFormat1: public Base {
  
    private:
      bool done = false;
      std::vector<uint8_t> KEY = std::vector<uint8_t>();
  
    public: 
      // Methods
      ConditionalInstructionFormat1(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class ConditionalInstructionFormat3: public Base {

    private:
      bool Z = false;
      uint8_t NUM = 0;
  
    public: 
      // Methods
      ConditionalInstructionFormat3(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;

  };

  class Ignore: public Base {
  
    public:
      // Methods
      Ignore(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class Event: public Base {

    private:
      // Attributes
      uint8_t events = 0;
  
    public:
      // Methods
      Event(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;
      bool hasEvent(const uint8_t& index) const;

  };

  class Context: public Base {

    private:
      bool     P = false;
      uint8_t  EL = 0;
      bool     SF = false;
      bool     NS = false;
      bool     hasVirt = false;
      bool     hasCont = false;
      bool     headerDone = false;
      uint32_t VMID = 0;
      uint32_t CONTEXTID = 0;
  
    public:
      // Methods
      Context(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;
      uint32_t getVmID() const;
      uint32_t getContextID() const;

  };

  class AddressWithContext: public Base {
  
    private:
      // Controll variable
      uint8_t offset = 0;
      uint8_t length = 0;
      // Attributes
      uint64_t A = 0;
      uint8_t  EL = 0;
      bool     SF = false;
      bool     NS = false;
      bool     hasVirt = false;
      bool     hasCont = false;
      bool     addrDone = false;
      bool     headerDone = false;
      uint32_t VMID = 0;
      uint32_t CONTEXTID = 0;

    public:
      // Methods
      AddressWithContext(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;
      uint64_t getAddress() const;
      uint32_t getVmID() const;
      uint32_t getContextID() const;

  };

  class TimestampMarker: public Base {
  
    public:
      // Methods
      TimestampMarker(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ExactMatchAddress: public Base {

    private:
      uint8_t QE = 0;
  
    public:
      // Methods
      ExactMatchAddress(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class ShortAddress: public Base {
  
    private:
      // Attributes
      bool done = false;
      uint8_t offset = 0;
      uint32_t address = 0;

    public:
      // Methods
      ShortAddress(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;
      uint32_t getAddress() const;

  };

  class LongAddress: public Base {
  
    private:
      // Attributes
      uint8_t offset = 0;
      uint8_t length = 4;
      uint64_t address = 0;

    public:
      // Methods
      LongAddress(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;
      uint64_t getAddress() const;

  };

  class Q: public Base {

    private:
      uint8_t TYPE = 0;
      uint8_t offset = 0;
      uint64_t address = 0;
      bool    hasAddress = false;
      bool    isAddrLong = false;
      bool    hasCount   = false;
      std::vector<uint8_t> count;
  
    public:
      // Methods 
      Q(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;
      uint64_t getAddress() const;

  };

  class AtomFormat1: public Base {
  
    private:
      bool a = false;
  
    public:
      // Methods
      AtomFormat1(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class AtomFormat2: public Base {
  
    private:
      uint8_t a = 0;
  
    public:
      // Methods
      AtomFormat2(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class AtomFormat3: public Base {

    private:
      uint8_t a = 0;
  
    public:
      // Methods
      AtomFormat3(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class AtomFormat4: public Base {
  
    private:
      uint8_t a = 0;

    public:
      // Methods
      AtomFormat4(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class AtomFormat5: public Base {

    private:
      uint8_t abc = 0;
  
    public:
      // Methods
      AtomFormat5(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class AtomFormat6: public Base {

    private:
      bool A = false;
      uint8_t COUNT = 0;
  
    public:
      // Methods 
      AtomFormat6(const uint8_t& header);
      bool isDone() const override;
      std::string asString() const override;

  };

  class Exception: public Base {

    private:
      uint16_t         type       = 0;
      bool             p          = false;
      bool             headerDone = false;
      bool             hasAddress = false;
      Exception::Base* address    = nullptr;

    public:
      // Methods
      Exception(const uint8_t& header);
      bool isDone() const override;
      void insert(const uint8_t& byte) override;
      std::string asString() const override;
      ~Exception();

  };

}
