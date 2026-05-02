#include "Sniffer.hpp"


#include <cstddef>
#include <cstdint>
#include <iostream>
#include <fstream>
#include <format>
#include <cstring>


Sniffer::Sniffer(std::vector<uint8_t>* buffer): buffer(buffer) {
  buffer->reserve(100*1024*1024);
}

inline bool Sniffer::hasHeader(const u_char* packet) const {
  return (packet[0] == 0xab) && (packet[1] == 0xba);
}

inline bool Sniffer::hasFooter(const u_char* packet) const {
  return (packet[0] == 0xeb) && (packet[1] == 0xbe) &&
    (packet[2] == 0x00) && (packet[3] == 0x00) &&
    (packet[4] == 0x00) && (packet[5] == 0x00) &&
    (packet[6] == 0x00) && (packet[7] == 0x00);
}

inline bool Sniffer::areNext4BytesAllSet(const u_char* packet) const {
  return (packet[0] == 0xff) && (packet[1] == 0xff) && (packet[2] == 0xff) && (packet[3] == 0xff);
}

void Sniffer::onPacket(const pcap_pkthdr* header, const u_char* packet) {
  // Must be larger than 2+8 (i.e., 0xABBA+0xEBBE000000000000)
  if ((header->len > 10+headerOffset) && hasHeader(&packet[headerOffset]) && hasFooter(&packet[header->len-8])) {
    buffer->insert(buffer->end(), packet+2+headerOffset, packet+header->len-8);
  }
  //else {
  //  std::cerr << "Expected header and/or footer (0xABBA, 0xEBBE) not found..." << std::endl;
  //}
}

std::vector<std::string> Sniffer::getDevices() {
  // Interfaces
  std::vector<std::string> interfaces;
  // PCAP
  pcap_if_t *alldevs, *dev;

  if (pcap_findalldevs(&alldevs, errbuf) == -1) {
    std::cerr << "Error: " << errbuf << std::endl;
  }

  for (dev = alldevs; dev != NULL; dev = dev->next) {
    interfaces.push_back(std::string(dev->name));
  }

  pcap_freealldevs(alldevs);

  return interfaces;
}

void Sniffer::dispatch(u_char* user, const pcap_pkthdr* header, const u_char* packet) {
  Sniffer* self = reinterpret_cast<Sniffer*>(user);
  self->onPacket(header, packet);
}

void Sniffer::pickDevice(std::string newInterfaceName) {
  // Close current interface is one is already opened
  if (interface != nullptr) {
     pcap_close(interface);
  }
  // Update name
  name = newInterfaceName;
  // Init. interface
  interface = pcap_create(name.c_str(), errbuf);
  if (interface == nullptr) {
    std::cerr << "pcap_open_live failed: " << errbuf << std::endl;
  }
  pcap_set_snaplen(interface, 65535);
  pcap_set_promisc(interface, 1);
  pcap_set_timeout(interface, 10);
  pcap_set_buffer_size(interface, 1024*1024*1024);
  pcap_activate(interface);

  // Start sniffing
  captureThread = std::thread(&Sniffer::captureLoop, this);
}

void Sniffer::captureLoop() {
  pcap_loop(interface, 0, Sniffer::dispatch, (u_char*)this);
}

void Sniffer::unpickDevice() {
  if (interface != nullptr) {
    pcap_breakloop(interface);
    if (captureThread.joinable())
      captureThread.join();
    printStats();
    pcap_close(interface);
    interface = nullptr;
  }
}

void Sniffer::printStats() {
    if (interface != nullptr) {
        struct pcap_stat stats;
        
        if (pcap_stats(interface, &stats) == 0) {
            std::cout << "Packet Capture Statistics:" << std::endl;
            std::cout << "Packets received: " << stats.ps_recv << std::endl;
            std::cout << "Packets dropped by kernel: " << stats.ps_drop << std::endl;
            std::cout << "Packets dropped by interface: " << stats.ps_ifdrop << std::endl;
            // Calculate drop percentage if any packets were received
            if (stats.ps_recv > 0) {
                double dropRate = (static_cast<double>(stats.ps_drop) / stats.ps_recv) * 100.0;
                std::cout << "Drop rate: " << std::format("{:.2f}%", dropRate) << std::endl;
            }
        } else {
            std::cerr << "Error getting statistics: " << pcap_geterr(interface) << std::endl;
        }
    } else {
        std::cerr << "No active interface to get statistics from" << std::endl;
    }
}

Sniffer::~Sniffer() {
  // Close pcap
  if (interface != nullptr) {
    pcap_breakloop(interface);
    pcap_close(interface);
    interface = nullptr;
  }
}
