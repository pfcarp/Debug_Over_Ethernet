#pragma once


#include <cstdint>
#include <string>
#include <vector>


namespace Packet {

  bool isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper);

  class Base {
    
    protected:
      uint8_t iterator = 0;

    public:

      virtual inline bool isDone() const = 0;
      virtual void insert(uint8_t byte) = 0;
      virtual std::string asString() const = 0;
      virtual ~Base() = default;
      inline uint8_t getIterator() const;

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
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

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
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Timestamp: public Base {

    private:
      bool timestampFlag = true;
      bool hasCountFlag = false;
      uint64_t TS = 0;
      uint32_t COUNT = 0;

    public:

      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class TraceOn: public Base {

    public:

      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

      // For test only
      friend struct TraceOnTestAccess;
  };

  class FunctionReturn: public Base {

    public:

      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ExceptionReturn: public Base {

    public:

      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Resynchronization: public Base {

    public:

      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Reserved: public Base {

    public:

      Reserved(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class CycleCountFormat2: public Base {

    private:
      bool F = false;
      uint8_t aaaa = 0;
      uint8_t bbbb = 0;

    public:

      CycleCountFormat2(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class CycleCountFormat1: public Base {

    private:
      bool U = false;
      std::vector<uint8_t> commit = std::vector<uint8_t>();
      uint32_t count = 0;

    public:

      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class CycleCountFormat3: public Base {

    private:
      uint8_t aa = 0;
      uint8_t bb = 0;

    public:

      CycleCountFormat3(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class NumberedDataSyncMark: public Base {

    private:
      uint8_t NUM = 0;

    public:

      NumberedDataSyncMark(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class UnnumberedDataSyncMark: public Base {
  
    private:
      uint8_t A = 0;

    public:
  
      UnnumberedDataSyncMark(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Commit: public Base {

    private:
      bool done = false;
      std::vector<uint8_t> commit = std::vector<uint8_t>();
  
    public:
  
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class CancelFormat1: public Base {

    private:
      bool M = false;
      bool done = false;
      std::vector<uint8_t> cancel = std::vector<uint8_t>();
  
    public:
  
      CancelFormat1(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Mispredict: public Base {
  
    private:
      uint8_t A = 0;

    public:

      Mispredict(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class CancelFormat2: public Base {
  
    private:
      uint8_t A = 0;

    public:
  
      CancelFormat2(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class CancelFormat3: public Base {

    private:
      uint8_t CC = 0;
      bool    A  = 0;
  
    public:

      CancelFormat3(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ConditionalInstructionFormat2: public Base {

    private:
      uint8_t CI = 0;
  
    public:

      ConditionalInstructionFormat2(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ConditionalFlush: public Base {
  
    public:
  
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ConditionalResultFormat4: public Base {

    private:
      uint8_t T = 0;
  
    public:

      ConditionalResultFormat4(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ConditionalResultFormat2: public Base {

    private:
      bool    K = false;
      uint8_t T = 0;
  
    public:

      ConditionalResultFormat2(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ConditionalResultFormat3: public Base {

    private:
      uint16_t TOKEN = 0;
  
    public:

      ConditionalResultFormat3(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

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

      ConditionalResultFormat1(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ConditionalInstructionFormat1: public Base {
  
    private:
      bool done = false;
      std::vector<uint8_t> KEY = std::vector<uint8_t>();
  
    public:
  
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ConditionalInstructionFormat3: public Base {

    private:
      bool Z = false;
      uint8_t NUM = 0;
  
    public:
  
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Ignore: public Base {
  
    public:
  
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Event: public Base {

    private:
      std::vector<bool> events = std::vector<bool>(4);
  
    public:

      Event(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Context: public Base {

    private:
      bool     P = false;
      uint8_t  EL = 0;
      bool     SF = false;
      bool     NS = false;
      bool     hasVirt = false;
      bool     hasCont = false;
      uint32_t VMID = 0;
      uint32_t CONTEXTID = 0;
  
    public:

      Context(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

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
      uint32_t VMID = 0;
      uint32_t CONTEXTID = 0;

    public:
  
      AddressWithContext(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class TimestampMarker: public Base {
  
    public:
  
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ExactMatchAddress: public Base {

    private:
      uint8_t QE = 0;
  
    public:

      ExactMatchAddress(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class ShortAddress: public Base {
  
    private:
      bool done = false;
      uint8_t offset = 0;
      uint32_t address = 0;

    public:

      ShortAddress(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class LongAddress: public Base {
  
    private:
      uint8_t offset = 0;
      uint8_t length = 4;
      uint64_t address = 0;

    public:
  
      LongAddress(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Q: public Base {
  
    public:
  
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class AtomFormat1: public Base {
  
    private:
      bool a = false;
  
    public:
  
      AtomFormat1(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class AtomFormat2: public Base {
  
    private:
      uint8_t a = 0;
  
    public:
  
      AtomFormat2(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class AtomFormat3: public Base {

    private:
      uint8_t a = 0;
  
    public:

      AtomFormat3(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class AtomFormat4: public Base {
  
    private:
      uint8_t a = 0;

    public:
  
      AtomFormat4(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class AtomFormat5: public Base {

    private:
      uint8_t abc = 0;
  
    public:

      AtomFormat5(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class AtomFormat6: public Base {

    private:
      bool A = false;
      uint8_t COUNT = 0;
  
    public:
  
      AtomFormat6(uint8_t header);
      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;

  };

  class Exception: public Base {

    private:
      uint8_t          e1e0    = 0;
      uint16_t         type    = 0;
      bool             p       = false;
      Exception::Base* address = nullptr;

    public:

      virtual inline bool isDone() const override;
      virtual void insert(uint8_t byte) override;
      virtual std::string asString() const override;
      ~Exception();

  };

}
