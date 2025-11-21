#include "InterfaceSelector.hpp"


InterfaceSelector::InterfaceSelector(Sniffer* sniffer): sniffer(sniffer) {
  parent            = gtk_combo_box_text_new();
  // Populate
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(parent), nullptr, "Select interface");
  for (std::string interface : sniffer->getDevices())
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(parent), interface.c_str(), interface.c_str());
  gtk_combo_box_set_active(GTK_COMBO_BOX(parent), 0);
  // Action
  g_signal_connect(parent, "changed", G_CALLBACK(InterfaceSelector::c_on_combo_changed), this);
}


void InterfaceSelector::on_combo_changed(GtkComboBox* box) {
  const char* id = gtk_combo_box_get_active_id(box);
  const char* text = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(box));
  if (!id || !text || strlen(text) == 0) {
    sniffer->unpickDevice();
  }
  else {
    g_print("Combo selected: %s (id=%s)\n", text, id);
    sniffer->pickDevice(std::string(id));
  }
}

void InterfaceSelector::c_on_combo_changed(GObject* object, gpointer user_data) {
  InterfaceSelector* self = static_cast<InterfaceSelector*>(user_data);
  self->on_combo_changed(GTK_COMBO_BOX(object));
}

