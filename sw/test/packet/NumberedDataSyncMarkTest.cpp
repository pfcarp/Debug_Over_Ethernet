#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("NumberedDataSyncMark packets are 'done' right after construction") {
  Packet::NumberedDataSyncMark obj(0b00100111);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("NumberedDataSyncMark packets are always 'done'") {
  Packet::NumberedDataSyncMark obj(0b00100110);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
