#pragma once


#include <gtk/gtk.h>
#include <cstdint>
#include <vector>


#include "PacketFactory.hpp"
#include "SourcePanel.hpp"


class SourcePanels {

  private:
    // Attributes
    static const uint32_t panelNumber = 1;
    SourcePanel* panel[panelNumber];
    GtkWidget* panels;
    // Methods

  public:
    // Attributes
    GtkWidget* parent;
    // Methods
    SourcePanels(std::vector<PacketFactory>& factories);
    void update();

};
