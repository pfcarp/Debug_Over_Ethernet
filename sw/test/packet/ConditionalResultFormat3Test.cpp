#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ConditionalResultFormat3 packets") {
  Packet::ConditionalResultFormat3 obj(0b01011111);

  // Right after creation
  CHECK(!obj.isDone());
  obj.insert(0xAA);

  // Done
  CHECK(obj.isDone());
}
