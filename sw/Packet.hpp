#pragma once


#include <cstdint>
#include <string>
#include <vector>


namespace Packet {

  inline bool isInInclusiveRange(uint8_t a, uint8_t lower, uint8_t upper) {
    return (lower <= a) && (a <= upper);
  }

  class Base {
    
    protected:
      uint8_t iterator = 0;

    public:

      virtual inline bool isDone() const = 0;
      virtual void insert(uint8_t byte) = 0;
      virtual std::string asString() const = 0;
      virtual ~Base() = default;

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
      
      virtual inline bool isDone() const override {
        switch (type) {
          case Extension::Ext::ASync:             return iterator == 12; // 11+1
          case Extension::Ext::Discard:           return iterator ==  2; //  1+1
          case Extension::Ext::Overflow:          return iterator ==  2; //  1+1
          case Extension::Ext::BranchFutureFlush: return iterator ==  2; //  1+1
          default: return false;
        } 
      }

      virtual void insert(uint8_t byte) override {
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

      virtual std::string asString() const override {
        switch (type) {
          case Extension::Ext::ASync:                  return "ASync.";
          case Extension::Ext::Discard:           return "Discard.";
          case Extension::Ext::Overflow:          return "Overflow";
          case Extension::Ext::BranchFutureFlush: return "BranchFutureFlush";
          default: return "No match found!";
        }
      }

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

      virtual inline bool isDone() const override {
        return iterator == 5;
      }

      virtual void insert(uint8_t byte) override {
        if (iterator == 0) { // PLCTL
          hasInfo = (0b00000001 & byte);
          hasKey  = (0b00000010 & byte) >> 1;
          hasSpec = (0b00000100 & byte) >> 2;
          hasCyct = (0b00001000 & byte) >> 3;
          iterator += (byte < 128);
        }
        else if (iterator == 1) {
          if (hasInfo) {
            info.push_back(0b01111111 & byte);
            iterator += (byte < 128);
          }
          else {
            iterator++;
          }
        }
        else if (iterator == 2) {
          if (hasKey) {
            key.push_back(0b01111111 & byte);
            iterator += (byte < 128);
          }
          else {
            iterator++;
          }
        }
        else if (iterator == 3) {
          if (hasSpec) {
            spec.push_back(0b01111111 & byte);
            iterator += (byte < 128);
          }
          else {
            iterator++;
          }
        }
        else if (iterator == 4) {
          if (hasCyct) {
            cyct.push_back(0b01111111 & byte);
            iterator += (byte < 128);
          }
          else {
            iterator++;
          }
        }
      }
      
      virtual std::string asString() const override {
        return "Trace info.";
      }

  };

  class Timestamp: public Base {

    private:
      bool timestampFlag = true;
      bool hasCountFlag = false;
      uint64_t TS = 0;
      uint32_t COUNT = 0;

    public:

      virtual inline bool isDone() const override {
        return (!hasCountFlag) && (!timestampFlag);
      }

      virtual void insert(uint8_t byte) override {
        if (iterator == 0) {
          //Page 264: N = 0 -> no count; N = 1 -> count.
          hasCountFlag = byte%2;
        }
        else if (timestampFlag && (iterator <= 1) && (iterator < 9)) {
          timestampFlag = byte >= (1 << 7);
          TS |= byte << ((iterator-1)*7);
        }
        else if (timestampFlag && (iterator == 9)) {
          timestampFlag = false;
          TS |= byte << ((iterator-1)*7);
        }
        else if (hasCountFlag && (iterator < 12)) {
          hasCountFlag = byte >= (1 << 7);
          COUNT |= byte << ((iterator-10)*7);
        }
        else if (hasCountFlag && (iterator == 12)) {
          hasCountFlag = false;
          COUNT |= byte << ((iterator-10)*7);
        }
        iterator++;
      }

      virtual std::string asString() const override {
        return "Timestamp.";
      }

  };

  class TraceOn: public Base {

    public:

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Trace on.";
      }

  };

  class FunctionReturn: public Base {

    public:

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Function return.";
      }

  };

  class ExceptionReturn: public Base {

    public:

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Exception return.";
      }

  };

  class Resynchronization: public Base {

    public:

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Resynchronization.";
      }

  };

  class Reserved: public Base {

    public:

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Reserved.";
      }

  };

  // Constructor must take extra bit from header!
  class CycleCountFormat2: public Base {

    private:
      bool F = false;
      uint8_t aaaa = 0;
      uint8_t bbbb = 0;

    public:

      CycleCountFormat2(uint8_t header) {
        F = 0b00000001 & header;
      }

      virtual inline bool isDone() const override {
        return iterator == 1;
      }

      virtual void insert(uint8_t byte) override {
        aaaa = (0b11110000 && byte) >> 4;
        bbbb = (0b00001111 && byte);
        iterator++;
      }

      virtual std::string asString() const override {
        return "Cycle count format 2.";
      }

  };

  class CycleCountFormat1: public Base {

    private:
      bool U = false;
      std::vector<uint8_t> commit = std::vector<uint8_t>();
      uint32_t count = 0;

    public:

      virtual inline bool isDone() const override {
        return iterator == 3;
      }

      virtual void insert(uint8_t byte) override {
        if (iterator == 0) {
          commit.push_back(byte & 0b01111111);
          iterator += (byte < 128);
        }
        else if (iterator == 1) {
          count |= (0b01111111 & byte) << 7;
          if (byte < 128)
            iterator += 2;
        }
        else if (iterator == 2) {
          count |= (0b00111111 & byte) << 14;
          iterator++;
        }
      }

      virtual std::string asString() const override {
        return "Cycle count format 1.";
      }

  };

  class CycleCountFormat3: public Base {

    private:
      uint8_t aa = 0;
      uint8_t bb = 0;

    public:

      CycleCountFormat3(uint8_t header) {
        aa = (0b00001100 & header) >> 2;
        bb = (0b00000011 & header);
      }

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Cycle count format 3.";
      }

  };

  // Constructor must take extra bit from header!
  class NumberedDataSyncMark: public Base {

    private:
      uint8_t NUM = 0;

    public:

      NumberedDataSyncMark(uint8_t header) {
        NUM = 0b00000111 & header;
      }

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Numbered data sync mark.";
      }

  };

  // Constructor must take extra bit from header!
  class UnnumberedDataSyncMark: public Base {
  
    private:
      uint8_t A = 0;

    public:
  
      UnnumberedDataSyncMark(uint8_t header) {
        A = 0b00000111 & header;
      }

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Unnumbered data sync mark.";
      }

  };

  class Commit: public Base {

    private:
      bool done = false;
      std::vector<uint8_t> commit = std::vector<uint8_t>();
  
    public:
  
      virtual inline bool isDone() const override {
        return done;
      }

      virtual void insert(uint8_t byte) override {
        commit.push_back(0b01111111 & byte);
        done = (byte < 128);
      }

      virtual std::string asString() const override {
        return "Commit.";
      }

  };

  class CancelFormat1: public Base {

    private:
      bool M = false;
      bool done = false;
      std::vector<uint8_t> cancel = std::vector<uint8_t>();
  
    public:
  
      CancelFormat1(uint8_t header) {
        M = 0b00000001 & header;
      }

      virtual inline bool isDone() const override {
        return done;
      }

      virtual void insert(uint8_t byte) override {
        cancel.push_back(0b01111111 & byte);
        done = (byte < 128);
      }

      virtual std::string asString() const override {
        return "Cancel format 1.";
      }

  };

  class Mispredict: public Base {
  
    private:
      uint8_t A = 0;

    public:

      Mispredict(uint8_t header) {
        A = 0b00000011 & header;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Mispredict (A = "+std::to_string(static_cast<int>(A))+")";
      }

  };

  class CancelFormat2: public Base {
  
    private:
      uint8_t A = 0;

    public:
  
      CancelFormat2(uint8_t header) {
        A = 0b00000011 & header;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "CancelFormat2 (A = "+std::to_string(static_cast<int>(A))+")";
      }

  };

  class CancelFormat3: public Base {

    private:
      uint8_t CC = 0;
      bool    A  = 0;
  
    public:

      CancelFormat3(uint8_t header) {
        CC = 0b00000110 & header;
        A  = 0b00000001 & header;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "CancelFormat3 (CC = "+std::to_string(static_cast<int>(CC))+", A = "+std::to_string(static_cast<int>(A))+")";
      }

  };

  class ConditionalInstructionFormat2: public Base {

    private:
      uint8_t CI = 0;
  
    public:

      ConditionalInstructionFormat2(uint8_t header) {
        CI = 0b00000011 & header;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Conditional instruction format 2 (CI = "+std::to_string(static_cast<int>(CI))+")";
      }

  };

  class ConditionalFlush: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Conditional flush.";
      }

  };

  class ConditionalResultFormat4: public Base {

    private:
      uint8_t T = 0;
  
    public:

      ConditionalResultFormat4(uint8_t header) {
        T = 0b00000011 & header;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Conditional result format 4.";
      }

  };

  class ConditionalResultFormat2: public Base {

    private:
      bool    K = false;
      uint8_t T = 0;
  
    public:

      ConditionalResultFormat2(uint8_t header) {
        T = (0b00000011 & header);
        K = (0b00000100 & header) >> 2;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Condition result format 2.";
      }

  };

  class ConditionalResultFormat3: public Base {

    private:
      uint16_t TOKEN = 0;
  
    public:

      ConditionalResultFormat3(uint8_t header) {
        TOKEN |= (0b00001111 & header) << 8;
  }
  
      virtual inline bool isDone() const override {
        return iterator == 1;
      }

      virtual void insert(uint8_t byte) override {
        TOKEN |= byte;
        iterator++;
      }

      virtual std::string asString() const override {
        return "Condition result format 3.";
      }

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

      ConditionalResultFormat1(uint8_t header) {
        single = (0b00000100 & header) >> 2;
        CI0 = 0b00000001 & header;
        if (!single)
          CI1 = (0b00000010 & header) >> 1;
      }
  
      virtual inline bool isDone() const override {
        return (single)? iterator == 1 : iterator == 2;
      }

      virtual void insert(uint8_t byte) override {
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

      virtual std::string asString() const override {
        return "Conditional result format 1.";
      }

  };

  class ConditionalInstructionFormat1: public Base {
  
    private:
      bool done = false;
      std::vector<uint8_t> KEY = std::vector<uint8_t>();
  
    public:
  
      virtual inline bool isDone() const override {
        return done;
      }

      virtual void insert(uint8_t byte) override {
        KEY.push_back(0b01111111 & byte);
        done = (byte < 128);
      }

      virtual std::string asString() const override {
        return "Conditional instruction format 1.";
      }

  };

  class ConditionalInstructionFormat3: public Base {

    private:
      bool Z = false;
      uint8_t NUM = 0;
  
    public:
  
      virtual inline bool isDone() const override {
        return iterator == 1;
      }

      virtual void insert(uint8_t byte) override {
        Z = 0b00000001 & byte;
        NUM = (0b01111110 & byte) >> 1;
        iterator++;
      }

      virtual std::string asString() const override {
        return "Condition instruction format 3.";
      }

  };

  class Ignore: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Ignore.";
      }

  };

  class Event: public Base {

    private:
      std::vector<bool> events = std::vector<bool>(4);
  
    public:

      Event(uint8_t header) {
        for (int i = 0; i < events.size(); i++) {
          events[i] = ((0b00000001 << i) & header) >> i;
        }
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Event (#0 = "+std::to_string(static_cast<int>(events[0]))+", #1 = "+std::to_string(static_cast<int>(events[1]))+", #2 = "+std::to_string(static_cast<int>(events[2]))+", #3 = "+std::to_string(static_cast<int>(events[3]))+").";
      }

  };

  class Context: public Base {

    private:
      uint8_t  EL = 0;
      bool     SF = false;
      bool     NS = false;
      bool     hasVirt = false;
      bool     hasCont = false;
      uint32_t VMID = 0;
      uint32_t CONTEXTID = 0;
  
    public:

      Context(uint8_t header) {
      }

      virtual inline bool isDone() const override {
        return iterator == 7;
      }

      virtual void insert(uint8_t byte) override {
        if (iterator == 0) {
          EL = 0b00000011 & byte;
          SF = (0b00010000 & byte) >> 4;
          NS = (0b00100000 & byte) >> 5;
          hasVirt = (0b01000000 & byte) >> 6;
          hasCont = (0b10000000 & byte) >> 7;
          iterator ++;
        }
        else if (hasVirt && (1 <= iterator) && (iterator < 4)) {
          VMID |= byte << (8*(iterator-1));
          iterator ++;
        }
        else if (!hasVirt) {
          iterator = 4;
        }
        else if (hasCont && (4 <= iterator) && (iterator < 7)) {
          CONTEXTID |= byte << (8*(iterator-4));
          iterator ++;
        }
        else if (!hasCont) {
          iterator = 7;
        }
      }

      virtual std::string asString() const override {
        return "Context.";
      }

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
  
      AddressWithContext(uint8_t header) {
        switch(header & 0b00000111) {
          case 0b00000010: offset = 2; length = 4; break;
          case 0b00000011: offset = 1; length = 4; break;
          case 0b00000101: offset = 2; length = 8; break;
          case 0b00000110: offset = 1; length = 8; break;
          default        : offset = 0; length = 0; break;
        }
      }
  
      virtual inline bool isDone() const override {
        return iterator == length+9;
      }

      virtual void insert(uint8_t byte) override {
        if (iterator < length) {
          if (iterator < offset) {
            A |= (0b01111111 & byte) << (offset+(8*iterator)-iterator);
          }
          else {
            A |= byte << (8*iterator);
          }
          iterator += iterator < length;
        }
        else if (iterator == length) {
          EL = 0b00000011 & byte;
          SF = (0b00010000 & byte) >> 4;
          NS = (0b00100000 & byte) >> 5;
          hasVirt = (0b01000000 & byte) >> 6;
          hasCont = (0b10000000 & byte) >> 7;
          iterator ++;
        }
        else if (hasVirt && (length+1 <= iterator) && (iterator < length+5)) {
          VMID |= byte << (8*(iterator-1));
          iterator ++;
        }
        else if (!hasVirt) {
          iterator = length+4;
        }
        else if (hasCont && (length+5 <= iterator) && (iterator < length+9)) {
          CONTEXTID |= byte << (8*(iterator-length-4));
          iterator++;
        }
        else if (!hasCont) {
          iterator = length+9;
        }
      }

      virtual std::string asString() const override {
        return "Addres with context.";
      }

  };

  class TimestampMarker: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Timestamp marker.";
      }

  };

  class ExactMatchAddress: public Base {

    private:
      uint8_t QE = 0;
  
    public:

      ExactMatchAddress(uint8_t header) {
        QE = 0b00000011 && header;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "exact match address.";
      }

  };

  class ShortAddress: public Base {
  
    private:
      bool done = false;
      uint8_t offset = 0;
      uint32_t address = 0;

    public:

      ShortAddress(uint8_t header) {
        switch(header & 0b00000011) {
          case 0b00000001: offset = 2; break;
          case 0b00000010: offset = 1; break;
          default        : offset = 0; break;
        }
      }
  
      virtual inline bool isDone() const override {
        return done;
      }

      virtual void insert(uint8_t byte) override {
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

      virtual std::string asString() const override {
        return "Short address.";
      }

  };

  class LongAddress: public Base {
  
    private:
      uint8_t offset = 0;
      uint8_t length = 4;
      uint64_t address = 0;

    public:
  
      LongAddress(uint8_t header) {
        switch(header & 0b00000111) {
          case 0b00000010: offset = 2; length = 4; break;
          case 0b00000011: offset = 1; length = 4; break;
          case 0b00000101: offset = 2; length = 8; break;
          case 0b00000110: offset = 1; length = 8; break;
          default        : offset = 0; length = 0; break;
        }
      }
  
      virtual inline bool isDone() const override {
        return iterator == length;
      }

      virtual void insert(uint8_t byte) override {
        if (iterator < offset) {
          address |= (0b01111111 & byte) << (offset+(8*iterator)-iterator);
        }
        else {
          address |= byte << (8*iterator);
        }
        iterator += iterator < length;
      }

      virtual std::string asString() const override {
        return "Long address.";
      }

  };

  class Q: public Base {
  
    public:
  
      virtual inline bool isDone() const override {
        return true; // At least one
      }

      virtual void insert(uint8_t byte) override {
        //
      }

      virtual std::string asString() const override {
        return "Q.";
      }

  };

  class AtomFormat1: public Base {
  
    private:
      bool a = false;
  
    public:
  
      AtomFormat1(uint8_t header) {
        a = 0b00000001 | header;
      }

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Atom format 1.";
      }

  };

  class AtomFormat2: public Base {
  
    private:
      uint8_t a = 0;
  
    public:
  
      AtomFormat2(uint8_t header) {
        a = 0b00000011 | header;
      }

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Atom formt 2.";
      }

  };

  class AtomFormat3: public Base {

    private:
      uint8_t a = 0;
  
    public:

      AtomFormat3(uint8_t header) {
        a = 0b00000111 | header;
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Atom formt 3.";
      }

  };

  class AtomFormat4: public Base {
  
    private:
      uint8_t a = 0;

    public:
  
      AtomFormat4(uint8_t header) {
        a = 0b00000011 | header;
      }

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Atom formt 4.";
      }

  };

  class AtomFormat5: public Base {

    private:
      uint8_t abc = 0;
  
    public:

      AtomFormat5(uint8_t header) {
        abc = ((0b00100000 & header) >> 3) | (0b00000011 & header);
      }
  
      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Atom formt 5.";
      }

  };

  class AtomFormat6: public Base {

    private:
      bool A = false;
      uint8_t COUNT = 0;
  
    public:
  
      AtomFormat6(uint8_t header) {
        A = (0b00100000 & header) >> 5;
        COUNT = 0b00011111 & header;
      }

      virtual inline bool isDone() const override {
        return true;
      }

      virtual void insert(uint8_t byte) override {}

      virtual std::string asString() const override {
        return "Atom formt 6.";
      }

  };

  class Exception: public Base {

    private:
      uint8_t          e1e0    = 0;
      uint16_t         type    = 0;
      bool             p       = false;
      Exception::Base* address = nullptr;

    public:

      virtual inline bool isDone() const override {
        return (e1e0 == 1) || ((e1e0 == 2) && (address != nullptr) && address->isDone());
      }

      virtual void insert(uint8_t byte) override {
        if (iterator == 0) {
          switch (byte & 0b01000001) {
            case 0b00000001: e1e0 = 1; break;
            case 0b01000000: e1e0 = 2; break;
            default        : e1e0 = 0; break;
          }
          type = (byte & 0b00111110) >> 1;
        }
        else if (iterator == 1) {
          type |= (byte & 0b00011111) << 5;
          p = (byte & 0b00100000) >> 5;
        }
        else if ((iterator == 3) && (e1e0 == 2)) {
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
        else if ((iterator > 3) && !address->isDone()) {
          address->insert(byte);
        }
        iterator++;
      }

      ~Exception() {
        if (address != nullptr) {
          delete address;
        }
      }

      virtual std::string asString() const override {
        return "Exception.";
      }

  };

}
