#pragma once


#include <atomic>
#include <cstdint>
#include <thread>


#include "Buffer.hpp"
#include "Point.hpp"
#include "TimedData.hpp"


class Inserter {

  protected:
    int interval;
    std::atomic<bool> running;
    std::thread worker;
    uint32_t offset;
    Buffer* buffer;
    
    virtual TimedData generate() = 0;
    void run();

  public:
    Inserter(Buffer* buffer, double offset = 0);
    ~Inserter();
    void start();
    void stop();

};


class InserterLinear: public Inserter {

  protected:
    uint32_t timestamp = 0;
    TimedData generate() override;

  public:
    InserterLinear(Buffer* buffer, double offset = 0);
};


class InserterNormal: public Inserter {

  protected:
    TimedData generate() override;

  public:
    InserterNormal(Buffer* buffer, double offset = 0);
};


class InserterStep: public Inserter {

  protected:
    uint32_t timestamp = 0;
    TimedData generate() override;

  public:
    InserterStep(Buffer* buffer, double offset = 0);
};
