#include <iostream>
#include <string>
#include <vector>


#include "Sniffer.hpp"


int main(int argc, char* argv[]) {

  Sniffer sniffer;
  
  if (argc != 2) {
    std::cerr << "Error: exactly one argument expected." << std::endl;
    std::cerr << "Usage: " << argv[0] << " <argument>" << std::endl;
    std::cerr << "Interfaces available include:" << std::endl;
    for (std::string interface: sniffer.getDevices()) {
      std::cerr << "\t" << interface << std::endl;
    }
    std::cerr << "NOTE: do not forget to allow access via:" << std::endl;
    std::cerr << "\tsudo setcap cap_net_raw,cap_net_admin=eip ./online.out" << std::endl;
    return 1;
  }
  sniffer.pickDevice(argv[1]);

  return 0;
}

