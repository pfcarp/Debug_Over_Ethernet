#include "Sniffer.hpp"


#include <iostream>


void Sniffer::onPacket(const pcap_pkthdr* header, const u_char* packet) {
  std::cout << "Packet: " << header->len << " bytes\n";
  for (u_int i = 0; i < header->len; i++) {
    printf("%02X ", packet[i]);
  }
  printf("\n");
}

void Sniffer::dispatch(u_char* user, const pcap_pkthdr* header, const u_char* packet) {
  Sniffer* self = reinterpret_cast<Sniffer*>(user);
  self->onPacket(header, packet);
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

void Sniffer::pickDevice(std::string newInterfaceName) {
  // Close current interface is one is already opened
  if (interface != nullptr) {
     pcap_close(interface);
  }
  // Update name
  name = newInterfaceName;
  // Init. interface
  interface = pcap_open_live(name.c_str(), 65535, 1, 10, errbuf);
  if (interface == nullptr) {
    std::cerr << "pcap_open_live failed: " << errbuf << std::endl;
  }
  // Start sniffing
  pcap_loop(interface, 0, Sniffer::dispatch, (u_char*)this);
}

void Sniffer::unpickDevice() {
  if (interface != nullptr) {
     pcap_close(interface);
  }
}

Sniffer::~Sniffer() {
  if (interface != nullptr) {
     pcap_close(interface);
  }
}
