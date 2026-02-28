#pragma once


#include <cstdint>


#include "Dispatcher.hpp"
#include "PacketFactory.hpp"


class Deformatter {

  private:
    // Attributes
    bool insertInPrevious = false;
    uint8_t current = 0;
    uint8_t previous = 0;
    uint8_t counter = 0;
    uint64_t timestamp = 0;
    // Methods
    void format();
    
  public:
    // Attributes
    uint8_t frame[16]; // Needed for tests...
    PacketFactory factories[4]; // Needed for tests...
    // Methods
    bool insert(const uint8_t& byte);
    bool insert_bytes(const uint8_t * chunk, size_t chunk_len);
    bool toInsertInPrevious(const uint8_t& aux, const uint8_t& offset) const;
    void setTimestamp(uint64_t t);
    virtual ~Deformatter();

};


class DeformatterVector: public Deformatter {

  public:
    DeformatterVector();
    ~DeformatterVector();

};


class DeformatterDispatcher: public Deformatter {

  private:
    // Attributes
    Dispatcher& dispatcher;

  public:
    DeformatterDispatcher(Dispatcher& dispatcher);
    ~DeformatterDispatcher();

};
