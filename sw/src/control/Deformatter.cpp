#include "Deformatter.hpp"


#include <cstdint>
#include <iostream>
#include <format>
#include <chrono>


inline bool Deformatter::toInsertInPrevious(const uint8_t& aux, const uint8_t& offset) const {
  return (aux >> offset) & 0x01;
}

bool Deformatter::insert(const uint8_t& byte) {
  // Insert new byte
  frame[counter] = byte;
  counter++;
  // If frame size reached
  if (counter == 16) [[unlikely]] {
    // Format frame
    //auto start = std::chrono::steady_clock::now();
    format();
    //auto end = std::chrono::steady_clock::now();
    //std::chrono::duration<double> elapsed_seconds = end-start;
    //std::cout << elapsed_seconds.count() << std::endl;
    counter = 0;
  }
  // Counter is 0 iff it was 15 when entering the function
  return (counter == 0);
}

/**
 * Followed format presented in DDI0314H page 220 (Sec. 8.12.1)
 */
void Deformatter::format() {
  for (uint32_t i = 0; i < 15; i++) { // 15 becasue last byte contains carried over bits
    // Inspect if odd indexed byte and check if it is an ID
    if (!(i & 0x01)) { // i%2 == 0
      // Check AUX to detect whether the next (data) byte belong to the current or previous stream source
      if (frame[i] & 0x01) {
        previous = current;
        current = frame[i] >> 1;
        insertInPrevious = frame[15] & 0x01;
      }
      else {
        streams[current]->insert((frame[i] & 0xfe) | (frame[15] & 0x01));
      }
      frame[15] >>= 1;
    }
    else {
      streams[(insertInPrevious)? previous : current]->insert(frame[i]);
    }
  }
}

void Deformatter::setTimestamp(uint64_t t) {
  for (size_t i = 0; i < 4; i++) {
    streams[i]->factory.setTimestamp(t);
  }
}


DeformatterVector::DeformatterVector() {
  for (size_t i = 0; i < 4; i++) {
    streams[i] = new StreamVector();
  }
}

DeformatterVector::~DeformatterVector() {
  for (size_t i = 0; i < 4; i++) {
    delete streams[i];
  }
}


DeformatterDispatcher::DeformatterDispatcher(Dispatcher& dispatcher): dispatcher(dispatcher) {
  for (size_t i = 0; i < 4; i++) {
    streams[i] = new StreamDispatcher(dispatcher);
  }
}

DeformatterDispatcher::~DeformatterDispatcher() {
  for (size_t i = 0; i < 4; i++) {
    delete streams[i];
  }
}

Deformatter::~Deformatter() {
  for (size_t i = 0; i < 4; i++) {
    delete streams[i];
  }
}
