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
    uint64_t timestamp = 0; // Absolute value
    // Methods
    void deformat();
    
  public:
    constexpr static const uint32_t frameWidth = 16; // TPIU trace width
    constexpr static const uint32_t timestampWidth = 4; // timestamp width
    constexpr static const uint32_t workFrameWidth = frameWidth+timestampWidth;
    constexpr static const uint32_t factoriesNumber = 4;
    // Attributes
    uint8_t frame[workFrameWidth]; // Needed for tests...
    PacketFactory factories[factoriesNumber]; // Needed for tests...
    // Methods
    bool insert(const uint8_t& byte);
    bool insert_bytes(const uint8_t * chunk, size_t chunk_len);
    bool toInsertInPrevious(const uint8_t& aux, const uint8_t& offset) const;
    void setTimestamp();
    uint64_t getTimestamp();
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
