#pragma once


#include <cstddef>
#include <vector>
#include <memory>
#include <cstdint>


#include "Dispatcher.hpp"
#include "Packet.hpp"
#include "PacketFactory.hpp"


class Stream {

  public:
    // Attributes
    PacketFactory factory = PacketFactory();
    std::vector<std::unique_ptr<Packet::Base>> packets;
    // Methods
    virtual void insert(uint8_t byte) = 0;
    virtual size_t size() = 0;
    ~Stream();

};


class StreamVector: public Stream {

  private:
    // Attributes

  public:
    void insert(uint8_t byte) override;
    size_t size() override;
    ~StreamVector();
};


class StreamDispatcher: public Stream {

  private:
    // Attributes
    Dispatcher& dispatcher;

  public:
    // Methods
    StreamDispatcher(Dispatcher& dispatcher);
    void insert(uint8_t byte) override;
    size_t size() override;
    ~StreamDispatcher();

};
