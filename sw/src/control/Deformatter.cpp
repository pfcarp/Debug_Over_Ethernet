#include "Deformatter.hpp"


#include <cstdint>
#include <iostream>
#include <format>


inline bool Deformatter::toInsertInPrevious(const uint8_t aux, const uint8_t offset) const {
  return (aux >> offset)%2;
}

bool Deformatter::insert(uint8_t byte) {
  // Insert new byte
  frame[counter] = byte;
  // If frame size reached
  if (counter == 15) {
    // Format frame
    format();
    clean();
    return true;
  }
  else {
    counter++;
    return false;
  }
}

/**
 * Followed format presented in DDI0314H page 220 (Sec. 8.12.1)
 */
void Deformatter::format() {
  for (uint8_t i = 0; i < 15; i++) { // 15 becasue last byte contains carried over bits
    // Inspect if odd indexed byte and check if it is an ID
    if (i%2 == 0) {
      // Check AUX to detect whether the next (data) byte belong to the current or previous stream source
      if (frame[i]%2) {
        previous = current;
        current = frame[i] >> 1;
        insertInPrevious = toInsertInPrevious(frame[15], i/2);
      }
      else {
        streams[current]->insert((frame[i] & 0xfe) | ((frame[15] >> (i/2)) & 0x01));
      }
    }
    else {
      if (insertInPrevious) {
        streams[previous]->insert(frame[i]);
        insertInPrevious = false;
      }
      else
        streams[current]->insert(frame[i]);
    }
  }
}

void Deformatter::clean() {
  counter = 0;
  frame.resize(16);
  frame.assign(frame.size(), 0);
}


DeformatterVector::DeformatterVector() {
  for (size_t i = 0; i < 4; i++) {
    streams.push_back(new StreamVector());
  }
}

DeformatterVector::~DeformatterVector() {
  for (size_t i = 0; i < streams.size(); i++) {
    delete streams[i];
  }
}


DeformatterDispatcher::DeformatterDispatcher(Dispatcher& dispatcher): dispatcher(dispatcher) {
  for (size_t i = 0; i < 4; i++) {
    streams.push_back(new StreamDispatcher(dispatcher));
  }
}

DeformatterDispatcher::~DeformatterDispatcher() {
  for (size_t i = 0; i < streams.size(); i++) {
    delete streams[i];
  }
}

