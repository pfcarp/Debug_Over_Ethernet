#include <iostream>
#include <string>
#include <vector>
#include <csignal>


#include "Sniffer.hpp"
#include "Deformatter.hpp"



static Sniffer* sniffer;
static DeformatterVector* deformatter;


static void handle_sigint(int) {
  sniffer->unpickDevice();
}


int main(int argc, char* argv[]) {
  // CTRL+C handler
  std::signal(SIGINT, handle_sigint);

  deformatter = new DeformatterVector();
  sniffer = new Sniffer(*deformatter);

  if (argc != 2) {
    std::cerr << "Error: exactly one argument expected." << std::endl;
    std::cerr << "Usage: " << argv[0] << " <argument>" << std::endl;
    std::cerr << "Interfaces available include:" << std::endl;
    for (std::string interface: sniffer->getDevices()) {
      std::cerr << "\t" << interface << std::endl;
    }
    std::cerr << "NOTE: do not forget to allow access via:" << std::endl;
    std::cerr << "\tsudo setcap cap_net_raw,cap_net_admin=eip ./online.out" << std::endl;
    return 1;
  }
  sniffer->pickDevice(argv[1]);

  delete deformatter;
  delete sniffer;

  return 0;
}

