#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Ignore packets are 'done' right after construction") {
  Packet::Ignore obj;

  // Right after creaion
  CHECK(obj.isDone());
}
