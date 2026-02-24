#include "Deformatter.hpp"


#include <cstdint>
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
  if (counter == 16) [[unlikely]] {
    format();
    counter = 0;
  }
  // Counter is 0 iff it was 15 when entering the function
  return (counter == 0);
}

/**
 * Followed format presented in DDI0314H page 220 (Sec. 8.12.1)
 */
void Deformatter::format() {
  for (uint8_t i = 0; i < 15; i++) { // 15 becasue last byte contains carried over bits
    // Inspect if odd indexed byte and check if it is an ID
    if (!(i & 0x01)) {
      // Check AUX to detect whether the next (data) byte belong to the current or previous stream source
      if (frame[i] & 0x01) { // (frame[i]%2) {
        previous = current;
        current = frame[i] >> 1;
        insertInPrevious = frame[15] & 0x01;
        //insertInPrevious = toInsertInPrevious(frame[15], i>>1);
      }
      else {
        factories[current].insert((frame[i] & 0xfe) | (frame[15] & 0x01));
      }
      // Update AUX
      frame[15] >>= 1;
    }
    else { // (i%2 == 1) {
      /*
      if (insertInPrevious) {
        factories[previous].insert(frame[i]);
        insertInPrevious = false;
      }
      else {
        factories[current].insert(frame[i]);
      }
      */
      factories[(insertInPrevious)? previous : current].insert(frame[i]);
    }
  }
}

void Deformatter::setTimestamp(uint64_t t) {
  for (size_t i = 0; i < 4; i++) {
    factories[i].setTimestamp(t);
  }
}


DeformatterVector::DeformatterVector() {}

DeformatterVector::~DeformatterVector() {}


DeformatterDispatcher::DeformatterDispatcher(Dispatcher& dispatcher): dispatcher(dispatcher) {}

DeformatterDispatcher::~DeformatterDispatcher() {}

Deformatter::~Deformatter() {}
