#include "Deformatter.hpp"


#include <cstdint>
#include <cstring>
#include <iostream>


#include "Tools.hpp"


bool Deformatter::toInsertInPrevious(const uint8_t& aux, const uint8_t& offset) const {
  return (aux >> offset) & 0x01;
}

bool Deformatter::insert(const uint8_t& byte) {
  // Insert new byte
  frame[counter] = byte;
  counter++;
  // If frame size reached: Format frame
  if (counter == workFrameWidth) [[unlikely]] {
    setTimestamp();
    deformat();
    counter = 0;
  }
  // Counter is 0 iff it was `frameWidth` when entering the function
  return (counter == 0);
}

bool Deformatter::insert_bytes(const uint8_t * chunk, size_t chunk_len) {
    // Insert new byte
    memcpy(frame, chunk, chunk_len);
    deformat();
    return 0;
}

/**
 * Followed format presented in DDI0314H page 220 (Sec. 8.12.1)
 */
#if 0
void Deformatter::deformat() {
    register uint8_t disc = frame[15];

#define PROCESS_16BITS(index)						\
    do {								\
	if (frame[index] & 0x01) {					\
	    previous = current;						\
	    current = frame[index] >> 1;				\
	    factories[(disc & (1 << index)?previous:current)].insert(frame[index+1]); \
	} else {							\
	    factories[current].insert((frame[index] & 0xfe) |		\
				      ((disc & (1<<index))>>index));	\
	    factories[current].insert(frame[index+1]);			\
	}								\
    } while(0)

    PROCESS_16BITS(0);
    PROCESS_16BITS(2);
    PROCESS_16BITS(4);
    PROCESS_16BITS(6);
    PROCESS_16BITS(8);
    PROCESS_16BITS(10);
    PROCESS_16BITS(12);
    PROCESS_16BITS(14);
    
}	
#endif


void Deformatter::deformat() {
  for (uint32_t i = timestampWidth; i < workFrameWidth-1; i++) { // `frameWidth-1` becasue last byte contains carried over bits
    // Inspect if odd indexed byte and check if it is an ID
    if (!(i & 0x01)) {
      // Check AUX to detect whether the next (data) byte belong to the current or previous stream source
      if (frame[i] & 0x01) { // (frame[i]%2) {
        previous = current;
        current = frame[i] >> 1;
        insertInPrevious = frame[workFrameWidth-1] & 0x01;
        //insertInPrevious = toInsertInPrevious(frame[15], i>>1);
      }
      else {
        factories[current].insert((frame[i] & 0xfe) | (frame[workFrameWidth-1] & 0x01));
      }
      // Update AUX
      frame[workFrameWidth-1] >>= 1;
    }
    else { // (i%2 == 1) {
      if (insertInPrevious) {
        factories[previous].insert(frame[i]);
        insertInPrevious = false;
      }
      else {
        factories[current].insert(frame[i]);
      }
    }
  }
}

void Deformatter::setTimestamp() {
  uint32_t relative; // relative timestamp
  std::memcpy(&relative, frame, timestampWidth);
  timestamp += relative;
  for (size_t i = 0; i < factoriesNumber; i++) {
    factories[i].setTimestamp(timestamp);
  }
}

uint64_t Deformatter::getTimestamp() {
  return timestamp;
}


DeformatterVector::DeformatterVector() {}

DeformatterVector::~DeformatterVector() {}


DeformatterDispatcher::DeformatterDispatcher(Dispatcher& dispatcher): dispatcher(dispatcher) {}

DeformatterDispatcher::~DeformatterDispatcher() {}

Deformatter::~Deformatter() {}
