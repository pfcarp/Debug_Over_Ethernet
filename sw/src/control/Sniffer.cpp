#include "Sniffer.hpp"


#include <pcap.h>
#include <iostream>


std::vector<std::string> Sniffer::getDevices() {
  // Interfaces
  std::vector<std::string> interfaces;
  // PCAP
  char errbuf[PCAP_ERRBUF_SIZE];
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

void Sniffer::setDevice(std::string interface) {
  interfaceName = interface;
}
