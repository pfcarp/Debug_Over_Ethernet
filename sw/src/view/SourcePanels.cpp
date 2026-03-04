#include "SourcePanels.hpp"


SourcePanels::SourcePanels(std::vector<PacketFactory>& factories) {
  parent = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  // Panels
  panels = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_box_append(GTK_BOX(parent), panels);
  for (uint32_t i = 0; i < panelNumber; i++) {
    panel[i] = new SourcePanel(i, factories[i]);
    gtk_box_append(GTK_BOX(panels), panel[i]->parent);
  }
}

void SourcePanels::update() {
  for (uint32_t i = 0; i < panelNumber; i++) {
    panel[i]->update();
  }
}
