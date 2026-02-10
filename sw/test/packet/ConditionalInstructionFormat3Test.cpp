#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("ConditionalInstructionFormat3 packet") {
  // Encoding
  std::vector<uint8_t> encoding = {0xDA};
  // Packet under construction
  Packet::ConditionalInstructionFormat3 obj(0b01101101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
