#include "Deformatter.hpp"


#include <iostream>
#include <format>


void Deformatter::insert(uint8_t byte) {
  // Insert new byte
  frame[counter] = byte;
  // If frame size reached
  if (counter == 15) {
    // Format frame
    std::cout << "Formating" << std::endl;
    format();
    clean();
  }
  else {
    counter++;
  }
}

/**
 * Followed format presented in DDI0314H page 220 (Sec. 8.12.1)
 */
void Deformatter::format() {
  for (uint8_t i = 0; i < 15; i++) { // 15 becasue last byte contains carried over bits
    std::cout << std::format("0x{:02X}", frame[i]) << std::endl;
    // Inspect if odd indexed byte and check if it is an ID
    if (i%2 == 0) {
      if (frame[i]%2) {
        previous = current;
        current = frame[i] >> 1;
        // Check AUX to detect whether the next (data) byte belong to the current or previous stream source
        insertInPrevious = (frame[15] >> i)%2;
        std::cout << "(New ID! " << static_cast<int>(current) << " @" << static_cast<int>(i) << ")" << std::endl;
      }
      else {
        streams[current].insert((frame[i] & 0xfe) | ((frame[15] >> (i/2)) & 0x01));
      }
    }
    else {
      if (insertInPrevious)
        streams[previous].insert(frame[i]);
      else
        streams[current].insert(frame[i]);
    }
  }
  std::cout << std::format("0x{:02X}", frame[15]) << std::endl;
}

void Deformatter::clean() {
  counter = 0;
  frame.resize(16);
  frame.assign(frame.size(), 0);
}
