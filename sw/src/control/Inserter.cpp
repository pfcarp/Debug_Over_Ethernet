#include "Inserter.hpp"


#include <cmath>
#include <iostream>


#include "TimedData.hpp"


Inserter::Inserter(Buffer* buffer, double offset): buffer(buffer), offset(offset), interval(1000/30), running(false) {}

Inserter::~Inserter() {
  stop();
}


void Inserter::start() {
  running = true;
  worker = std::thread(&Inserter::run, this);
}

void Inserter::stop() {
  running = false;
  if (worker.joinable()) {
    worker.join();
  }
}

void Inserter::run() {
  while (running) {
    buffer->add(generate());
    std::this_thread::sleep_for(std::chrono::milliseconds(interval));
  }
}

InserterLinear::InserterLinear(Buffer* buffer, double offset): Inserter(buffer, offset) {}

TimedData InserterLinear::generate() {
  // Pack value
  TimedData data;
  data.time = timestamp++;
  data.value = sin(offset+(timestamp)*0.1);
  return data;
}

InserterNormal::InserterNormal(Buffer* buffer, double offset): Inserter(buffer, offset) {}

TimedData InserterNormal::generate() {
  // Generate value
  double u1 = (rand()+1.0)/(RAND_MAX+2.0);
  double u2 = (rand()+1.0)/(RAND_MAX+2.0);
  double z0 = std::sqrt(-2.0*std::log(u1))*std::cos(2.0*M_PI*u2);
  // Pack value
  TimedData data;
  data.time = std::round(offset+z0*5);
  data.value = 1;
  return data;
}

InserterStep::InserterStep(Buffer* buffer, double offset): Inserter(buffer, offset) {}

TimedData InserterStep::generate() {
  timestamp++;
  // Pack value
  TimedData data;
  data.time  = 25+cos(offset+(timestamp)*0.1);
  data.value = sin(offset+(timestamp)*0.1);
  return data;
}
