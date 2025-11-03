#pragma once


#include <cstdint>


namespace Packet {

  class Base {
    
    protected:
      uint8_t iterator = 0;

    public:

      virtual inline bool isDone() const;
      virtual void insert(uint8_t byte) = 0;
      virtual ~Base() = default;

  };

  class Extension: public Base {

    public:

      enum class Ext {
        ASync,
        Discard,
        Overflow,
        BranchFutureFlush
      };

      virtual inline bool isDone() const override {
        return 1; // TODO: at least one
      }

      virtual void insert(uint8_t byte) override {
        if (iterator == 0) {
          switch (byte) {
            case 0b00000000: 
          }
        }
      }

  };

  class Synchronization: public Base {

    public:

      virtual inline bool isDone() const override {
        return 1; // TODO: at least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Timestamp: public Base {

    public:

      virtual inline bool isDone() const override {
        return 11; // TODO: at least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class TraceOn: public Base {

    public:

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class FunctionReturn: public Base {

    public:

      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Exception: public Base {

    public:

      virtual inline bool isDone() const override {
        return 12; // From 3 to 12
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ExceptionReturn: public Base {

    public:

      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Resynchronization: public Base {

    public:

      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Reserved: public Base {

    public:

      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class CycleCountFormat2: public Base {

    public:

      virtual inline bool isDone() const override {
        return 1;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class CycleCountFormat1: public Base {

    public:

      virtual inline bool isDone() const override {
        return 1; // at least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class CycleCountFormat3: public Base {

    public:

      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class NumberedDataSyncMark: public Base {

    public:

      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class UnnumberedDataSyncMark: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Commit: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1; // At least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class CancelFormat1: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1; // At least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Mispredict: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class CancelFormat2: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class CancelFormat3: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalInstructionFormat2: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalFlush: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalResultFormat4: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalResultFormat2: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalResultFormat3: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalResultFormat1: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1; // At least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalInstructionFormat1: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1; // At least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ConditionalInstructionFormat3: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Ignore: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Event: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Context: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 9; // 0 to 9
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class AddressWithContext: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 8+6; // 4 addresses or 8 addresses + 1 to 6 contexts
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class TimestampMarker: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ExactMatchAddress: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class ShortAddress: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1; // 1 or 2
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class LongAddress: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 8; // 4 or 8
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class Q: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 1; // At least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class AtomFormat1: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class AtomFormat2: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class AtomFormat3: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class AtomFormat4: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class AtomFormat5: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };

  class AtomFormat6: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return 0;
      }

      virtual void insert(uint8_t byte) override {
        //
      }

  };


}
